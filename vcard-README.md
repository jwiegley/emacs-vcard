# vCard 4.0 Implementation for Emacs

Complete vCard 4.0 (RFC 6350) parser and serializer using EIEIO.

## Implementation Summary

### What Was Implemented

1. **EIEIO Class Design**
   - `vcard-property` class representing individual properties with:
     - `group` - Property group prefix (e.g., "item1")
     - `name` - Property name in uppercase (e.g., "TEL", "EMAIL")
     - `parameters` - Alist of property parameters
     - `value` - Property value (string or list for structured properties)

   - `vcard` class representing complete vCard with slots for all RFC 6350 section 6 properties:
     - **General**: version, source, kind, xml
     - **Identification**: fn, n, nickname, photo, bday, anniversary, gender
     - **Delivery**: adr
     - **Communications**: tel, email
     - **Geographical**: geo, tz
     - **Organizational**: title, role, logo, org, member, related
     - **Explanatory**: categories, note, prodid, rev, sound, uid, clientpidmap, url
     - **Security**: key
     - **Calendar**: fburl, caladruri, caluri
     - **Extended**: extended (alist for X-* properties)

2. **Parser Implementation**
   - `vcard-parse` - Parse vCard text into object
   - `vcard-parse-file` - Parse from file
   - `vcard-parse-buffer` - Parse from current buffer
   - Line unfolding (handles CRLF + space/tab continuations per RFC 6350)
   - Property line parsing with group.name;parameters:value format
   - Parameter parsing into alist with uppercase keys
   - Value unescaping (\\n, \\\\, \\,, \\;)
   - Structured value parsing for N and ADR properties
   - Validation of required properties (VERSION:4.0, FN)

3. **Serializer Implementation**
   - `vcard-serialize` - Serialize vCard object to text
   - `vcard-write-file` - Write vCard to file
   - Line folding at 75 octets with proper UTF-8 handling
   - Property formatting with groups and parameters
   - Value escaping (newlines, backslashes, commas, semicolons)
   - Structured value formatting (join with semicolons)
   - Always outputs BEGIN:VCARD, VERSION:4.0, FN, END:VCARD

4. **Convenience Functions**
   - `vcard-create` - Create vCard with keyword arguments
   - `vcard-get-property-value` - Get first value for property
   - `vcard-get-property-values` - Get all values for property
   - `vcard-set-property` - Set property (replaces existing)
   - `vcard-add-property` - Add property (appends to existing)

### Key Design Decisions

1. **EIEIO Over Structs**
   - Used EIEIO for proper object-oriented design
   - Enables future extensibility via inheritance
   - Provides type checking for slots
   - Better introspection capabilities

2. **Slot-Based Property Storage**
   - Each RFC 6350 property type has dedicated slot
   - Properties stored as lists of `vcard-property` objects
   - Supports multiple instances of same property type
   - Extended properties (X-*) use separate alist

3. **Lenient Parsing, Strict Generation**
   - Parser accepts various formats
   - Generator produces RFC 6350 compliant output
   - Always generates VERSION:4.0
   - Proper CRLF line endings in output

4. **UTF-8 Support**
   - Line folding respects UTF-8 character boundaries
   - Counts octets not characters for 75-byte limit
   - Preserves international characters correctly

5. **Parameter Handling**
   - Parameters stored as alist with uppercase keys
   - Supports quoted values containing special characters
   - Preserves parameter order from parsing

6. **Error Handling**
   - Custom error conditions: `vcard-parse-error`, `vcard-validation-error`
   - Descriptive error messages
   - Validates required properties

### Limitations and Future Enhancements

#### Current Limitations

1. **Single vCard Parsing**
   - Parser handles one vCard per call
   - Does not parse multiple vCards in single text
   - Enhancement: Add `vcard-parse-multiple` function

2. **No VERSION 3.0 Support**
   - Only handles vCard 4.0
   - Could add backward compatibility parser
   - Could add version converter (3.0 <-> 4.0)

3. **Basic Value Type Validation**
   - Parser doesn't validate value formats (dates, URIs, etc.)
   - Enhancement: Add optional strict validation mode
   - Could validate against VALUE parameter

4. **No MIME Type Handling**
   - Photo, Logo, Sound properties accept strings
   - No automatic base64 encoding/decoding
   - Enhancement: Add binary data helpers

5. **Property-Specific Methods**
   - Could add convenience methods like:
     - `vcard-add-email`, `vcard-add-phone`
     - `vcard-primary-email`, `vcard-work-phone`
     - Type-specific parameter helpers

#### Future Enhancements

1. **Query Interface**
   ```elisp
   (vcard-find-property vc 'email
                        :type "work"
                        :pref "1")
   ```

2. **Validation Framework**
   ```elisp
   (vcard-validate vc :strict t)
   ;; Returns list of validation issues
   ```

3. **Diff and Merge**
   ```elisp
   (vcard-diff vc1 vc2)
   (vcard-merge vc1 vc2)
   ```

4. **Import/Export**
   ```elisp
   (vcard-from-bbdb contact)
   (vcard-to-org vc)
   ```

5. **Batch Operations**
   ```elisp
   (vcard-parse-directory "~/contacts/")
   (vcard-export-all contacts "contacts.vcf")
   ```

6. **Property Groups**
   - Better support for property grouping
   - Helper to create grouped properties
   - Query by group

7. **Performance Optimizations**
   - Hash table for property lookup
   - Lazy parsing for large files
   - Incremental serialization for streams

8. **Interactive Commands**
   ```elisp
   M-x vcard-edit-file
   M-x vcard-display
   M-x vcard-insert-contact
   ```

### Testing

Comprehensive test suite in `vcard-test.el`:
- Simple and complex parsing
- Line folding/unfolding
- Extended properties and groups
- Serialization and escaping
- Round-trip fidelity
- Programmatic creation
- Property access helpers
- Validation
- File I/O
- UTF-8 support

All tests pass (0.004s execution time).

### Example Usage

#### Creating a vCard

```elisp
(require 'vcard)

;; Simple creation
(setq card (vcard-create
            :fn "John Doe"
            :email "john@example.com"
            :tel "+1-555-1234"
            :org "Example Corp"))

;; With structured name
(setq card (vcard-create
            :fn "Alice Johnson"
            :n '("Johnson" "Alice" "Marie" "" "")
            :email '("alice@work.com" "alice@home.com")
            :adr '("" "" "123 Main St" "Springfield" "IL" "62701" "USA")))
```

#### Parsing a vCard

```elisp
;; From string
(setq card (vcard-parse "BEGIN:VCARD\nVERSION:4.0\nFN:John Doe\n..."))

;; From file
(setq card (vcard-parse-file "~/contact.vcf"))

;; From buffer
(with-current-buffer "contact.vcf"
  (setq card (vcard-parse-buffer)))
```

#### Accessing Properties

```elisp
;; Get single value
(vcard-get-property-value card 'fn)
;; => "John Doe"

;; Get all values (for multi-value properties)
(vcard-get-property-values card 'email)
;; => ("john@work.com" "john@home.com")

;; Get structured value
(vcard-get-property-value card 'n)
;; => ("Doe" "John" "Robert" "Mr." "Jr.")
```

#### Modifying Properties

```elisp
;; Set property (replaces existing)
(vcard-set-property card 'tel "+1-555-9999")

;; Add property (appends)
(vcard-add-property card 'email "new@example.com"
                    '(("TYPE" . "work")))

;; Add with group
(vcard-add-property card 'tel "+1-555-1111"
                    '(("TYPE" . "work"))
                    "item1")
```

#### Serializing a vCard

```elisp
;; To string
(setq text (vcard-serialize card))

;; To file
(vcard-write-file card "~/output.vcf")
```

#### Advanced: Property Access

```elisp
;; Get property objects for detailed inspection
(let ((email-props (slot-value card 'email)))
  (dolist (prop email-props)
    (message "Email: %s (Type: %s)"
             (oref prop value)
             (alist-get "TYPE" (oref prop parameters) nil nil #'string=))))
```

### Code Quality

- **Lexical binding** enabled for performance
- **Comprehensive docstrings** for all public functions
- **Error conditions** with descriptive messages
- **Follows Emacs Lisp conventions**:
  - Namespace prefix (`vcard-`)
  - Private functions marked (`vcard--`)
  - Autoload cookies for public API
- **Package metadata** complete (Author, Version, Keywords, etc.)
- **Clean separation** of concerns (parsing, serialization, access)

### Files

- `vcard.el` - Main implementation (738 lines)
- `vcard-test.el` - Comprehensive test suite (402 lines)
- `vcard-README.md` - This documentation

### Performance

- Parsing is efficient with single-pass line processing
- Serialization uses string concatenation (could be optimized with buffers)
- Line folding handles UTF-8 character boundaries correctly
- Property lookup is O(1) via EIEIO slots
- Test suite runs in ~4ms

### Dependencies

- Emacs 25.1+ (for modern EIEIO)
- `cl-lib` (Common Lisp extensions)
- `eieio` (Enhanced Implementation of Emacs Interpreted Objects)

### License

Part of the user's Emacs configuration. Follow the same license as the rest of the dot-emacs repository.

## Conclusion

This implementation provides a complete, robust, and well-tested vCard 4.0 library for Emacs. It follows RFC 6350 specifications while providing an ergonomic API for Emacs Lisp programmers. The EIEIO-based design enables future extensibility and provides excellent introspection capabilities.
