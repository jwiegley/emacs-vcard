# vCard.el Architecture and API Analysis

**Version:** 1.0.0
**Analyzed:** 2025-11-16
**Purpose:** CardDAV integration planning

---

## 1. EIEIO Class Structure

### 1.1 Core Classes

The library uses two EIEIO classes representing the vCard object model:

#### `vcard-property` (vcard.el:62-83)

Represents a single property with all its metadata:

```elisp
(defclass vcard-property ()
  ((group                              ; Optional group prefix (e.g., "item1")
    :initarg :group
    :initform nil
    :type (or null string))
   (name                               ; Property name (uppercase, e.g., "TEL")
    :initarg :name
    :initform ""
    :type string)
   (parameters                         ; Alist of parameters
    :initarg :parameters
    :initform nil
    :type list                         ; ((PARAM-NAME . param-value) ...)
   (value                              ; Property value
    :initarg :value
    :initform ""
    :type (or string list)))           ; List for structured properties
```

**Key Characteristics:**
- Immutable after creation (no setters provided)
- Parameters stored as alist: `(("TYPE" . "work") ("PREF" . "1"))`
- Value can be string or list (for N, ADR, ORG, GENDER, CATEGORIES, NICKNAME)

#### `vcard` (vcard.el:85-271)

Represents a complete vCard 4.0 object with 30+ property slots:

```elisp
(defclass vcard ()
  ((version :initarg :version :initform nil :type list)
   (source :initarg :source :initform nil :type list)
   (kind :initarg :kind :initform nil :type list)
   (fn :initarg :fn :initform nil :type list)          ; REQUIRED
   (n :initarg :n :initform nil :type list)
   (email :initarg :email :initform nil :type list)
   (tel :initarg :tel :initform nil :type list)
   ;; ... 20+ more standard properties
   (extended :initarg :extended :initform nil :type list)))  ; X-* properties
```

**Key Characteristics:**
- Each slot holds a **list of `vcard-property` objects** (enables multiple emails, phones, etc.)
- `:extended` slot uses alist structure: `((x-name . (list of vcard-property)) ...)`
- Cardinality constraints enforced for *1 properties (N, BDAY, ANNIVERSARY, GENDER, REV, PRODID, UID, KIND)
- VERSION and FN are **required** and validated

### 1.2 Class Design Patterns

**Slot-based Property Storage:**
```elisp
;; Each property type has dedicated slot holding list of property objects
(oref vcard email)  ; => List of vcard-property objects
;; Example: (#<vcard-property "EMAIL" "john@work.com" (("TYPE" . "work"))>
;;           #<vcard-property "EMAIL" "john@home.com" (("TYPE" . "home"))>)
```

**Extended Properties Alist:**
```elisp
;; X-* properties stored separately
(oref vcard extended)  ; => ((X-MANAGER . (#<vcard-property...>))
                        ;;     (X-DEPARTMENT . (#<vcard-property...>)))
```

---

## 2. API Overview

### 2.1 Parsing API

All parsing functions support both single and multiple vCard handling:

| Function | Returns | Use Case |
|----------|---------|----------|
| `vcard-parse` | vcard object **or** list | Auto-detects single vs multiple |
| `vcard-parse-multiple` | **Always** list | Explicit multi-vCard handling |
| `vcard-parse-file` | vcard object or list | Parse from file |
| `vcard-parse-file-multiple` | Always list | Multi-vCard files |
| `vcard-parse-buffer` | vcard object or list | Parse current buffer |
| `vcard-parse-buffer-multiple` | Always list | Multiple vCards in buffer |

**Critical Design Decision:**
The `-multiple` variants **always** return a list, even for single vCards. This is important for CardDAV where you'll frequently process multiple contacts.

**Example Usage:**
```elisp
;; Single vCard file
(let ((vc (vcard-parse-file "contact.vcf")))
  (if (listp vc)
      (message "Multiple: %d" (length vc))
    (message "Single: %s" (vcard-get-property-value vc 'fn))))

;; Multiple vCards guaranteed
(let ((vcs (vcard-parse-file-multiple "contacts.vcf")))
  (dolist (vc vcs)
    (process-contact vc)))
```

### 2.2 Creation API

#### `vcard-create (&rest args)` (vcard.el:746-881)

Keyword-based vCard creation with automatic VERSION:4.0 injection:

```elisp
(vcard-create :fn "John Doe"                    ; Required
              :n '("Doe" "John" "Q." "Mr." "Jr.")
              :email '("john@work.com" "john@home.com")
              :tel "+1-555-1234"
              :org '("Acme Corp" "Engineering")  ; Structured
              :categories '("work" "friend")     ; Text-list
              :uid "unique-identifier")
```

**Supported Keywords:**
- `:fn` (required), `:n`, `:email`, `:tel`, `:adr`, `:org`, `:title`, `:role`
- `:url`, `:note`, `:uid`, `:bday`, `:anniversary`, `:gender`, `:nickname`
- `:categories`, `:geo`, `:tz`, `:photo`, `:logo`, `:sound`, `:key`
- `:rev`, `:prodid`, `:kind`, `:source`

**Automatic Conversions:**
- Strings converted to lists for ORG, GENDER
- Single values wrapped in lists for multi-value properties
- VERSION:4.0 automatically added

### 2.3 Property Access API

#### Read Operations

```elisp
;; Get first property value
(vcard-get-property-value vc 'email)
;; => "john@example.com"

;; Get all property values
(vcard-get-property-values vc 'email)
;; => ("john@work.com" "john@home.com" "john@personal.com")

;; Access structured property
(vcard-get-property-value vc 'n)
;; => ("Doe" "John" "Q." "Mr." "Jr.")
```

#### Write Operations

```elisp
;; Replace all values for property (removes existing)
(vcard-set-property vc 'email "new@example.com")
(vcard-set-property vc 'email "new@example.com" '(("TYPE" . "work")))

;; Add additional value (appends)
(vcard-add-property vc 'tel "+1-555-9999" '(("TYPE" . "cell") ("PREF" . "1")))

;; With group
(vcard-add-property vc 'email "alt@example.com" '(("TYPE" . "home")) "item1")
```

**Important for CardDAV:**
- Use `vcard-add-property` to preserve existing values
- Use `vcard-set-property` to replace (e.g., when syncing)
- Parameters enable TYPE discrimination (work/home/cell)

### 2.4 Serialization API

```elisp
;; Serialize single vCard
(vcard-serialize vc)
;; => "BEGIN:VCARD\r\nVERSION:4.0\r\nFN:John Doe\r\n...END:VCARD"

;; Serialize multiple vCards
(vcard-serialize-multiple (list vc1 vc2 vc3))
;; => "BEGIN:VCARD\r\n...END:VCARD\r\nBEGIN:VCARD\r\n...END:VCARD"

;; Write to file
(vcard-write-file vc "/path/to/contact.vcf")
```

**Line Folding:**
- Automatic at 75 **octets** (not characters)
- UTF-8 safe (doesn't break multi-byte characters)
- RFC 6350 compliant

---

## 3. Serialization/Deserialization Architecture

### 3.1 Parsing Pipeline

```
Input Text
    ↓
vcard--unfold-lines (vcard.el:275-300)
    ↓ [List of logical lines]
vcard--parse-property-line (vcard.el:400-427)
    ↓ [Plist: :group :name :parameters :value]
vcard--unescape-value (vcard.el:302-322)
    ↓ [Unescaped values]
vcard--add-property-to-vcard (vcard.el:441-471)
    ↓ [vcard-property objects added to slots]
vcard--validate-vcard (vcard.el:562-600)
    ↓ [Validation checks]
vcard object
```

### 3.2 Key Parsing Functions

#### Line Unfolding (vcard.el:275-300)

Handles RFC 6350 line continuation (CRLF + SPACE):

```elisp
;; Input: "TEL:+1-555-\r\n 1234"
;; Output: "TEL:+1-555-1234"
```

**Performance:** O(n) using list accumulation instead of string concatenation

#### Property Parsing (vcard.el:400-427)

Regex-based property line parsing:

```
^                                    # Start of line
(?:([a-zA-Z0-9_-]+)\.)?              # Optional group (item1.)
([^;:]+)                             # Property name
(?:;([^:]*))?                        # Optional parameters
:                                    # Colon separator
(.*)$                                # Value
```

**Critical Fix:** Group regex only matches valid identifiers, preventing dots in parameters (e.g., `PID=1.1`, `geo:37.386`) from being misinterpreted.

#### Value Unescaping (vcard.el:302-322)

Handles RFC 6350 escape sequences:
- `\n` → newline
- `\\` → backslash
- `\,` → comma
- `\;` → semicolon

#### Structured Property Detection

```elisp
;; Semicolon-separated components (vcard.el:416-418)
(member name '("N" "ADR" "ORG" "GENDER"))
;; => List: ("Doe" "John" "Q." "Mr." "Jr.")

;; Comma-separated text-list (vcard.el:420-422)
(member name '("CATEGORIES" "NICKNAME"))
;; => List: ("work" "friends" "family")
```

### 3.3 Serialization Pipeline

```
vcard object
    ↓
vcard--serialize-properties (vcard.el:525-532)
    ↓ [Iterate slots in defined order]
vcard--format-property (vcard.el:503-523)
    ↓ [Format single property]
vcard--escape-value (vcard.el:353-361)
    ↓ [Escape special characters]
vcard--fold-line (vcard.el:473-501)
    ↓ [Fold at 75 octets, UTF-8 safe]
Output Text
```

### 3.4 Serialization Features

#### Property Ordering

Properties serialized in RFC 6350 recommended order (vcard.el:710-716):
```
VERSION → SOURCE → KIND → XML → FN → N → NICKNAME → PHOTO → ...
```

#### Value Escaping

```elisp
;; Structured properties (semicolon separator)
(member name '("N" "ADR" "ORG" "GENDER"))
;; => "Doe;John;Q.;Mr.;Jr."

;; Text-list properties (comma separator)
(member name '("CATEGORIES" "NICKNAME"))
;; => "work,friends,family"

;; All values escaped: \ → \\, newline → \n, comma → \,, semicolon → \;
```

#### Line Folding (vcard.el:473-501)

**Algorithm:**
1. Count UTF-8 octets (not characters)
2. If ≤75 octets, return as-is
3. Otherwise, fold at character boundaries
4. Continuation lines start with SPACE

**Example:**
```
Input:  "TEL;TYPE=work,voice:+1-555-123-4567-890-1234-5678"
Output: "TEL;TYPE=work,voice:+1-555-123-4567-890-1234-567\r\n 8"
```

---

## 4. Network and Protocol Support

### 4.1 Current State: **NO** Network Support

The library is **purely local**:
- No HTTP/HTTPS client
- No WebDAV support
- No CardDAV protocol implementation
- No authentication mechanisms
- No URL parsing/construction

### 4.2 Existing File I/O

```elisp
;; Read operations
(vcard-parse-file filename)           ; Uses insert-file-contents
(vcard-parse-buffer)                  ; Uses buffer-string

;; Write operations
(vcard-write-file vc filename)        ; Uses write-region
```

**All I/O is synchronous and blocking.**

### 4.3 Extension Points for CardDAV

To add CardDAV support, you would need:

1. **HTTP Client Layer**
   - Use `url-retrieve` or `request.el` for HTTP
   - WebDAV method support (PROPFIND, REPORT, GET, PUT, DELETE)
   - XML parsing for multistatus responses

2. **CardDAV Protocol**
   - Address book discovery (PROPFIND on principal)
   - Contact listing (addressbook-query REPORT)
   - Contact retrieval (GET on vCard URLs)
   - Contact creation/update (PUT)
   - Contact deletion (DELETE)
   - ETag support for conflict detection

3. **Authentication**
   - Basic Auth (username/password)
   - OAuth2 (for Google, iCloud)
   - Bearer tokens

4. **Synchronization**
   - Sync-token support (RFC 6578)
   - Delta synchronization
   - Conflict resolution (ETags)

---

## 5. Architecture and Design Patterns

### 5.1 Object-Oriented Design

**EIEIO Class Hierarchy:**
```
eieio-default-superclass
    ↓
vcard-property          (Single property with metadata)
vcard                   (Complete vCard object)
```

**Why EIEIO?**
- Type safety via `:type` declarations
- Slot-based property access (`oref`, `setf`)
- Clear separation of concerns
- Extensibility via method dispatch

### 5.2 Data Structure Patterns

#### Lists Over Arrays

All multi-value properties use lists:
```elisp
;; Pros: Easy append (cons), natural Lisp idiom
;; Cons: O(n) access, but n typically small (< 10 emails)
(oref vc email)  ; => List of vcard-property objects
```

#### Alist for Parameters

```elisp
;; Parameters: ((KEY . value) ...)
'(("TYPE" . "work") ("PREF" . "1") ("ALTID" . "alt1"))

;; Pros: Simple lookup with assoc
;; Cons: Duplicate keys allowed (not an issue for vCard)
```

#### Structured Values as Lists

```elisp
;; N property: Family, Given, Additional, Prefix, Suffix
'("Doe" "John" "Q." "Mr." "Jr.")

;; Pros: Direct mapping to vCard spec
;; Cons: Positional (error-prone if indices wrong)
```

### 5.3 Functional Patterns

#### Pure Functions

Parsing and serialization are pure (no side effects):
```elisp
(vcard-parse text)      ; text → vcard object
(vcard-serialize vc)    ; vcard object → text
```

#### Validation as Separate Phase

```elisp
;; Parse creates object
(setq vc (vcard))
;; ... populate ...
;; Validate at end
(vcard--validate-vcard vc)  ; Signals errors
```

**Implication for CardDAV:** You can construct partial vCards during sync, then validate before serialization.

### 5.4 Error Handling Strategy

Two custom error conditions:
```elisp
(define-error 'vcard-parse-error "vCard parse error")
(define-error 'vcard-validation-error "vCard validation error")
```

**Usage Pattern:**
```elisp
(condition-case err
    (vcard-parse text)
  (vcard-parse-error
   (message "Parse failed: %s" (error-message-string err)))
  (vcard-validation-error
   (message "Invalid vCard: %s" (error-message-string err))))
```

### 5.5 Performance Optimizations

#### List Accumulation (O(n) not O(n²))

```elisp
;; GOOD: Push + nreverse
(let ((result nil))
  (dolist (item items)
    (push item result))
  (nreverse result))

;; BAD: Append (O(n²) for n items)
(let ((result nil))
  (dolist (item items)
    (setq result (append result (list item)))))
```

**Used throughout:** Line unfolding, property parsing, serialization

#### Character-boundary-aware Folding

Avoids breaking UTF-8 multi-byte sequences:
```elisp
;; Check octet count, but fold at character boundaries
(let ((char-octets (length (encode-coding-string char-str 'utf-8))))
  (if (> (+ current-octets char-octets) max-len)
      (fold-here)))
```

---

## 6. Integration Patterns (vcard-compat.el, vcard-org.el)

### 6.1 vCard 2.1/3.0 Compatibility Layer

**vcard-compat.el** provides backward compatibility:

```elisp
;; Auto-detect version and convert to 4.0
(vcard-compat-parse text)  ; Handles 2.1, 3.0, 4.0

;; Explicit version parsers
(vcard-compat-parse-21 text)
(vcard-compat-parse-30 text)
```

**Features:**
- BASE64 and QUOTED-PRINTABLE decoding
- Character set conversion (ISO-8859-1 → UTF-8)
- Type parameter mapping (HOME/WORK/PREF → work/home/pref)
- Dropped property handling (LABEL, MAILER, CLASS, AGENT)
- Data URI creation for binary properties

**Architecture Pattern:**
```
Legacy vCard Text
    ↓
vcard-compat--parse-legacy-property
    ↓
vcard-compat--convert-params-21/30
    ↓
vcard-compat--process-property-value (decode, charset convert)
    ↓
vcard-compat--build-vcard-40
    ↓
vcard 4.0 object
```

### 6.2 Org-mode Integration Layer

**vcard-org.el** provides bidirectional Org ↔ vCard conversion:

#### Property Mapping Configuration

```elisp
(defcustom vcard-org-property-mappings
  '(("EMAIL" email nil)
    ("EMAIL_HOME" email (("TYPE" . "home")))
    ("EMAIL_WORK" email (("TYPE" . "work")))
    ("MOBILE" tel (("TYPE" . "cell")))
    ("PHONE_WORK" tel (("TYPE" . "work,voice")))
    ;; ...
    )
```

**Pattern:** `(ORG-PROPERTY VCARD-SLOT PARAMETERS)`

#### Org → vCard Conversion

```elisp
;; Convert single entry at point
(vcard-org-entry-to-vcard)

;; Convert all contacts in buffer/region/subtree
(vcard-org-buffer-to-vcards)
(vcard-org-region-to-vcards)
(vcard-org-subtree-to-vcards)

;; Export to .vcf file
(vcard-org-export-buffer "contacts.vcf")
```

#### vCard → Org Conversion

```elisp
;; Convert vCard object to Org entry string
(vcard-org-vcard-to-entry vc 1)  ; level 1 heading

;; Import from .vcf file
(vcard-org-import-file "contacts.vcf")
```

**Org Entry Format:**
```org
* John Doe
:PROPERTIES:
:VCARD: t
:EMAIL: john@example.com
:EMAIL_WORK: john.doe@company.com
:MOBILE: +1-555-123-4567
:ORG: Example Corporation;Engineering Department
:TITLE: Senior Software Engineer
:END:
```

### 6.3 Design Pattern: Adapter Layers

Both integration modules follow the **Adapter Pattern**:

```
External Format (2.1/3.0 vCard or Org) → Adapter Layer → vCard 4.0 EIEIO objects
```

**Benefits for CardDAV:**
- CardDAV module can follow same pattern
- HTTP/WebDAV responses → CardDAV adapter → vCard 4.0 objects
- Keeps core vcard.el clean and focused

---

## 7. CardDAV Integration Recommendations

### 7.1 Architecture

```
CardDAV Client Layer
    ↓ (WebDAV HTTP requests)
CardDAV Protocol Layer
    ↓ (XML parsing, addressbook-query)
vCard Storage Layer
    ↓ (vcard.el objects)
vCard Serialization
    ↓ (vcard-serialize, vcard-parse)
Network I/O
```

### 7.2 Module Structure

Recommended file organization:

```
vcard-carddav.el          Core CardDAV protocol
vcard-carddav-sync.el     Synchronization engine
vcard-carddav-auth.el     Authentication (OAuth2, Basic)
vcard-carddav-cache.el    Local cache management
vcard-carddav-ui.el       User interface (optional)
```

### 7.3 Integration Points with vcard.el

#### 1. Parsing Server Responses

```elisp
;; CardDAV GET response
(let ((vcard-text (vcard-carddav--http-get url)))
  (vcard-parse vcard-text))  ; Returns vcard object
```

#### 2. Creating Contacts for Upload

```elisp
;; Create new contact
(let ((vc (vcard-create :fn name :email email :tel phone)))
  (vcard-carddav--http-put url (vcard-serialize vc)))
```

#### 3. Updating Existing Contacts

```elisp
;; Fetch, modify, upload
(let ((vc (vcard-parse (vcard-carddav--http-get url))))
  (vcard-add-property vc 'email "new@example.com")
  (vcard-carddav--http-put url (vcard-serialize vc) etag))
```

#### 4. UID Management

```elisp
;; Always preserve UIDs for CardDAV
(vcard-get-property-value vc 'uid)  ; Get UID
(vcard-set-property vc 'uid (generate-uid))  ; Set if missing
```

#### 5. REV (Revision) Tracking

```elisp
;; Update REV on modification
(vcard-set-property vc 'rev (format-time-string "%Y%m%dT%H%M%SZ" nil t))
```

### 7.4 Recommended Dependencies

```elisp
;; Package-Requires:
((emacs "27.1")
 (request "0.3.0")    ; HTTP client
 (xml "1.0")          ; XML parsing (built-in)
 (vcard "1.0"))       ; This library
```

### 7.5 Sync Strategy

**Recommended Approach: Sync-token based**

```elisp
(defun vcard-carddav-sync (addressbook-url)
  "Synchronize addressbook using sync-token."
  (let* ((sync-token (vcard-carddav-cache-get-sync-token addressbook-url))
         (changes (vcard-carddav--sync-collection addressbook-url sync-token)))
    ;; Process changes
    (dolist (change changes)
      (pcase (plist-get change :status)
        ('new (vcard-carddav-cache-add (plist-get change :vcard)))
        ('modified (vcard-carddav-cache-update (plist-get change :vcard)))
        ('deleted (vcard-carddav-cache-delete (plist-get change :href)))))
    ;; Update sync token
    (vcard-carddav-cache-set-sync-token addressbook-url
                                         (plist-get changes :sync-token))))
```

---

## 8. Key Takeaways for CardDAV Development

### 8.1 Strengths to Leverage

1. **Complete RFC 6350 support** - No need to implement vCard parsing
2. **EIEIO objects** - Type-safe, easy to inspect and manipulate
3. **Multiple vCard handling** - Built-in multi-contact support
4. **Property access API** - Clean get/set/add operations
5. **Validation** - Automatic validation on parse/serialize
6. **UTF-8 safe** - Proper Unicode handling throughout

### 8.2 Gaps to Address

1. **No network layer** - Need HTTP client with WebDAV support
2. **No XML handling** - Need multistatus response parsing
3. **No authentication** - Need OAuth2 and Basic Auth
4. **No caching** - Need local storage with ETag tracking
5. **No conflict resolution** - Need merge strategies

### 8.3 Design Principles to Follow

1. **Separation of concerns**
   - Keep protocol logic separate from vCard handling
   - Use adapter pattern like vcard-compat.el and vcard-org.el

2. **Async-first**
   - Use `url-retrieve` or `request` with callbacks
   - Don't block Emacs during sync operations

3. **Incremental sync**
   - Support sync-token (RFC 6578)
   - Only fetch changed contacts

4. **Local-first**
   - Cache all contacts locally
   - Allow offline access
   - Sync in background

5. **Error resilience**
   - Handle network failures gracefully
   - Retry with backoff
   - Preserve local changes on conflict

### 8.4 Testing Strategy

Follow vcard.el's comprehensive testing approach:
- Unit tests for protocol functions
- Integration tests with mock CardDAV server
- Real-world tests against known servers (Google, iCloud, NextCloud)

---

## 9. Example CardDAV Workflow

### Conceptual Implementation

```elisp
;; 1. Discover addressbooks
(vcard-carddav-discover-addressbooks "https://dav.example.com/principals/user@example.com/")
;; => (("Work" . "https://dav.example.com/addressbooks/user@example.com/work/")
;;     ("Personal" . "https://dav.example.com/addressbooks/user@example.com/personal/"))

;; 2. Initial sync
(vcard-carddav-sync "https://dav.example.com/addressbooks/user@example.com/work/")
;; Fetches all contacts, stores locally with ETags

;; 3. Access local cache (vcard.el objects)
(let ((contacts (vcard-carddav-cache-list)))
  (dolist (vc contacts)
    (message "%s: %s"
             (vcard-get-property-value vc 'fn)
             (vcard-get-property-value vc 'email))))

;; 4. Create new contact
(let ((vc (vcard-create :fn "New Contact"
                        :email "new@example.com"
                        :tel "+1-555-9999")))
  (vcard-carddav-create addressbook-url vc))

;; 5. Update existing contact
(let ((vc (vcard-carddav-cache-get-by-uid "uid-12345")))
  (vcard-add-property vc 'tel "+1-555-8888" '(("TYPE" . "cell")))
  (vcard-carddav-update addressbook-url vc))

;; 6. Incremental sync (only changed contacts)
(vcard-carddav-sync addressbook-url)  ; Uses sync-token
```

---

## 10. Conclusion

The vcard.el library provides a **solid foundation** for CardDAV integration:

**Ready to use:**
- Complete vCard 4.0 parsing and serialization
- EIEIO-based object model
- Property manipulation API
- Validation and error handling

**Need to build:**
- HTTP/WebDAV client layer
- CardDAV protocol implementation
- XML request/response handling
- Authentication mechanisms
- Synchronization engine
- Local caching with ETag support

**Recommended approach:**
- Create separate `vcard-carddav.el` module
- Follow adapter pattern from vcard-compat.el
- Use request.el for HTTP
- Implement sync-token based synchronization
- Test against multiple CardDAV servers

The architecture is clean, well-documented, and extensible. The vCard object model maps naturally to CardDAV operations, making integration straightforward.
