;;; ecard.el --- Complete vCard 4.0 (RFC 6350) parser and serializer -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: comm, data, ecard
;; URL: https://github.com/jwiegley/dot-emacs

;;; Commentary:

;; This package provides complete vCard 4.0 (RFC 6350) support for Emacs
;; using EIEIO object-oriented programming.
;;
;; Features:
;; - Full RFC 6350 section 6 property support
;; - EIEIO-based object model
;; - Robust parsing with line unfolding and value unescaping
;; - Proper serialization with line folding and value escaping
;; - UTF-8 support
;; - Validation of required properties
;;
;; Example usage:
;;
;;   ;; Parse from string
;;   (setq card (ecard-parse "BEGIN:VCARD\nVERSION:4.0\nFN:John Doe\nEND:VCARD"))
;;
;;   ;; Parse from file
;;   (setq card (ecard-parse-file "~/contact.vcf"))
;;
;;   ;; Create programmatically
;;   (setq card (ecard-create :fn "John Doe"
;;                            :email "john@example.com"
;;                            :tel "+1-555-1234"))
;;
;;   ;; Serialize to string
;;   (ecard-serialize card)
;;
;;   ;; Write to file
;;   (ecard-write-file card "~/output.vcf")

;;; Code:

(require 'cl-lib)
(require 'eieio)

;;; Custom group

(defgroup ecard nil
  "Library for vCard 4.0 (RFC 6350) support."
  :group 'comm
  :prefix "ecard-")

;;; Error conditions

(define-error 'ecard-parse-error "vCard parse error")
(define-error 'ecard-validation-error "vCard validation error")

;;; EIEIO Classes

;; MIGRATION NOTE: Converted from EIEIO defclass to cl-defstruct for performance
;; Expected improvements: 2-3x faster instantiation, 2-5x faster slot access
(cl-defstruct (ecard-property
               (:constructor ecard-property-create
                             (&key (group nil) (name "") (parameters nil) (value "")))
               (:copier nil)
               (:predicate ecard-property-p))
  "Represents a single vCard property with optional group, parameters, and value."
  (group nil :type (or null string)
   :documentation "Property group prefix (e.g., \"item1\" in \"item1.TEL\").")
  (name "" :type string
   :documentation "Property name in uppercase (e.g., \"TEL\", \"EMAIL\").")
  (parameters nil :type list
   :documentation "Property parameters as alist ((PARAM-NAME . param-value) ...).")
  (value "" :type (or string list)
   :documentation "Property value; list for structured properties (N, ADR)."))

;; Backward compatibility wrapper for old EIEIO-style constructor
(defun ecard-property (&rest args)
  "Create ecard-property struct.
Accepts keyword arguments :group, :name, :parameters, :value for compatibility."
  (apply #'ecard-property-create args))

;; MIGRATION NOTE: Converted from EIEIO defclass to cl-defstruct for performance
;; Expected improvements: 2-3x faster instantiation, 2-5x faster slot access, 20-40% memory reduction
;; All 36 vCard 4.0 properties stored as lists of ecard-property structs
(cl-defstruct (ecard
               (:constructor ecard--create-internal)
               (:copier nil)
               (:predicate ecard-p))
  "Represents a complete vCard 4.0 object with all RFC 6350 section 6 properties.
Each slot stores a list of ecard-property structs for that property type.
The :extended slot uses an alist structure: ((x-name . (list of ecard-property)) ...)."
  (version nil :type list
           :documentation "VERSION property (always \"4.0\").")
  (source nil :type list
          :documentation "SOURCE property.")
  (kind nil :type list
        :documentation "KIND property (individual, group, org, location).")
  (xml nil :type list
       :documentation "XML property.")
  (fn nil :type list
      :documentation "FN (formatted name) property - REQUIRED, can be multiple.")
  (n nil :type list
     :documentation "N (structured name) property.")
  (nickname nil :type list
            :documentation "NICKNAME property.")
  (photo nil :type list
         :documentation "PHOTO property.")
  (bday nil :type list
        :documentation "BDAY (birthday) property.")
  (anniversary nil :type list
               :documentation "ANNIVERSARY property.")
  (gender nil :type list
          :documentation "GENDER property.")
  (adr nil :type list
       :documentation "ADR (structured address) property.")
  (tel nil :type list
       :documentation "TEL (telephone) property.")
  (email nil :type list
         :documentation "EMAIL property.")
  (impp nil :type list
        :documentation "IMPP (instant messaging and presence protocol) property.")
  (lang nil :type list
        :documentation "LANG (language) property.")
  (geo nil :type list
       :documentation "GEO (geographical position) property.")
  (tz nil :type list
      :documentation "TZ (time zone) property.")
  (title nil :type list
         :documentation "TITLE property.")
  (role nil :type list
        :documentation "ROLE property.")
  (logo nil :type list
        :documentation "LOGO property.")
  (org nil :type list
       :documentation "ORG (organization) property.")
  (member nil :type list
          :documentation "MEMBER property.")
  (related nil :type list
           :documentation "RELATED property.")
  (categories nil :type list
              :documentation "CATEGORIES property.")
  (note nil :type list
        :documentation "NOTE property.")
  (prodid nil :type list
          :documentation "PRODID (product identifier) property.")
  (rev nil :type list
       :documentation "REV (revision) property.")
  (sound nil :type list
         :documentation "SOUND property.")
  (uid nil :type list
       :documentation "UID (unique identifier) property.")
  (clientpidmap nil :type list
                :documentation "CLIENTPIDMAP property.")
  (url nil :type list
       :documentation "URL property.")
  (key nil :type list
       :documentation "KEY (public key) property.")
  (fburl nil :type list
         :documentation "FBURL (free/busy URL) property.")
  (caladruri nil :type list
             :documentation "CALADRURI (calendar address URI) property.")
  (caluri nil :type list
          :documentation "CALURI (calendar URI) property.")
  (extended nil :type list
            :documentation "Alist for X-* properties: ((x-name . (list of ecard-property)) ...)."))

;; Smart constructor for ecard struct
(defun ecard-create-struct (&rest args)
  "Create ecard struct with keyword arguments.
ARGS is a plist of slot names and values.
Handles list initialization for multi-value properties.
All slots default to nil if not provided.

Example:
  (ecard-create-struct :fn (list prop1) :email (list prop2 prop3))"
  (apply #'ecard--create-internal args))

;; Backward compatibility wrapper for old EIEIO-style constructor
(defun ecard (&rest args)
  "Create ecard struct.
Accepts keyword arguments for all slots for backward compatibility.
If called with no args, creates empty ecard with all slots nil."
  (if args
      (apply #'ecard-create-struct args)
    (ecard--create-internal)))

;; Dynamic slot access helpers for migration from EIEIO
;; These replace slot-value and slot-exists-p for dynamic slot names
(defun ecard--slot-value (vc slot-name)
  "Get value of SLOT-NAME from ecard struct VC.
SLOT-NAME can be a symbol like \\='fn or \\='email, or a keyword like \\=:fn.
Returns nil if slot doesn't exist."
  ;; Normalize keyword to symbol (e.g., :note -> note)
  (when (keywordp slot-name)
    (setq slot-name (intern (substring (symbol-name slot-name) 1))))
  (pcase slot-name
    ('version (ecard-version vc))
    ('source (ecard-source vc))
    ('kind (ecard-kind vc))
    ('xml (ecard-xml vc))
    ('fn (ecard-fn vc))
    ('n (ecard-n vc))
    ('nickname (ecard-nickname vc))
    ('photo (ecard-photo vc))
    ('bday (ecard-bday vc))
    ('anniversary (ecard-anniversary vc))
    ('gender (ecard-gender vc))
    ('adr (ecard-adr vc))
    ('tel (ecard-tel vc))
    ('email (ecard-email vc))
    ('impp (ecard-impp vc))
    ('lang (ecard-lang vc))
    ('geo (ecard-geo vc))
    ('tz (ecard-tz vc))
    ('title (ecard-title vc))
    ('role (ecard-role vc))
    ('logo (ecard-logo vc))
    ('org (ecard-org vc))
    ('member (ecard-member vc))
    ('related (ecard-related vc))
    ('categories (ecard-categories vc))
    ('note (ecard-note vc))
    ('prodid (ecard-prodid vc))
    ('rev (ecard-rev vc))
    ('sound (ecard-sound vc))
    ('uid (ecard-uid vc))
    ('clientpidmap (ecard-clientpidmap vc))
    ('url (ecard-url vc))
    ('key (ecard-key vc))
    ('fburl (ecard-fburl vc))
    ('caladruri (ecard-caladruri vc))
    ('caluri (ecard-caluri vc))
    ('extended (ecard-extended vc))
    (_ nil)))

(defun ecard--set-slot-value (vc slot-name value)
  "Set SLOT-NAME in ecard struct VC to VALUE.
SLOT-NAME can be a symbol like \\='fn or \\='email, or a keyword like \\=:fn.
Signals \\='invalid-slot-name error if SLOT-NAME is not a valid ecard slot."
  ;; Normalize keyword to symbol (e.g., :note -> note)
  (when (keywordp slot-name)
    (setq slot-name (intern (substring (symbol-name slot-name) 1))))
  (pcase slot-name
    ('version (setf (ecard-version vc) value))
    ('source (setf (ecard-source vc) value))
    ('kind (setf (ecard-kind vc) value))
    ('xml (setf (ecard-xml vc) value))
    ('fn (setf (ecard-fn vc) value))
    ('n (setf (ecard-n vc) value))
    ('nickname (setf (ecard-nickname vc) value))
    ('photo (setf (ecard-photo vc) value))
    ('bday (setf (ecard-bday vc) value))
    ('anniversary (setf (ecard-anniversary vc) value))
    ('gender (setf (ecard-gender vc) value))
    ('adr (setf (ecard-adr vc) value))
    ('tel (setf (ecard-tel vc) value))
    ('email (setf (ecard-email vc) value))
    ('impp (setf (ecard-impp vc) value))
    ('lang (setf (ecard-lang vc) value))
    ('geo (setf (ecard-geo vc) value))
    ('tz (setf (ecard-tz vc) value))
    ('title (setf (ecard-title vc) value))
    ('role (setf (ecard-role vc) value))
    ('logo (setf (ecard-logo vc) value))
    ('org (setf (ecard-org vc) value))
    ('member (setf (ecard-member vc) value))
    ('related (setf (ecard-related vc) value))
    ('categories (setf (ecard-categories vc) value))
    ('note (setf (ecard-note vc) value))
    ('prodid (setf (ecard-prodid vc) value))
    ('rev (setf (ecard-rev vc) value))
    ('sound (setf (ecard-sound vc) value))
    ('uid (setf (ecard-uid vc) value))
    ('clientpidmap (setf (ecard-clientpidmap vc) value))
    ('url (setf (ecard-url vc) value))
    ('key (setf (ecard-key vc) value))
    ('fburl (setf (ecard-fburl vc) value))
    ('caladruri (setf (ecard-caladruri vc) value))
    ('caluri (setf (ecard-caluri vc) value))
    ('extended (setf (ecard-extended vc) value))
    (_ (signal 'invalid-slot-name (list slot-name 'ecard)))))

(defun ecard--slot-exists-p (vc slot-name)
  "Check if SLOT-NAME exists in ecard struct VC.
SLOT-NAME can be a symbol like \\='fn or \\='email, or a keyword like \\=:fn.
Always returns t for valid slot names, nil otherwise."
  ;; Normalize keyword to symbol (e.g., :note -> note)
  (when (keywordp slot-name)
    (setq slot-name (intern (substring (symbol-name slot-name) 1))))
  (memq slot-name '(version source kind xml fn n nickname photo bday anniversary
                    gender adr tel email impp lang geo tz title role logo org
                    member related categories note prodid rev sound uid
                    clientpidmap url key fburl caladruri caluri extended)))

;;; Internal utility functions

(defun ecard--unfold-lines (text)
  "Unfold vCard TEXT by removing CRLF followed by space or tab.
Returns a list of unfolded lines.
Per RFC 6350, the CRLF is removed but the space/tab is kept.

Performance: Uses list accumulation for O(n) string building
instead of O(n²) concatenation."
  (let ((lines (split-string text "[\r\n]+" t)))
    (cl-loop with result = nil
             with current-parts = nil  ; Accumulate line parts in reverse
             for line in lines
             do (if (string-match-p "^[ \t]" line)
                    ;; Continuation line - accumulate the continuation (without leading space/tab)
                    (push (substring line 1) current-parts)
                  ;; New property line - complete previous line if any
                  (progn
                    (when current-parts
                      ;; Build completed line from accumulated parts - O(n)
                      (push (mapconcat #'identity (nreverse current-parts) "") result)
                      (setq current-parts nil))
                    ;; Start new line
                    (setq current-parts (list line))))
             finally (when current-parts
                       ;; Complete final line
                       (push (mapconcat #'identity (nreverse current-parts) "") result))
             finally return (nreverse result))))

(defun ecard--unescape-value (value)
  "Unescape vCard VALUE according to RFC 6350.
Handles \\n and \\N (newline), \\\\ (backslash), \\, (comma), \\; (semicolon).
RFC 6350 Section 3.4 allows both lowercase and uppercase N for newlines."
  (let ((result "")
        (i 0)
        (len (length value)))
    (while (< i len)
      (let ((char (aref value i)))
        (if (and (= char ?\\) (< (1+ i) len))
            (let ((next (aref value (1+ i))))
              (setq result (concat result
                                   (pcase next
                                     (?n "\n")
                                     (?N "\n")  ; RFC 6350 allows capital N
                                     (?\\ "\\")
                                     (?, ",")
                                     (?\; ";")
                                     (_ (string next)))))
              (setq i (+ i 2)))
          (setq result (concat result (string char)))
          (setq i (1+ i)))))
    result))

(defun ecard--split-text-list (value-string)
  "Split text-list VALUE-STRING on unescaped commas.
Returns list of unescaped component values.
Used for CATEGORIES and NICKNAME properties."
  (let ((result nil)
        (current "")
        (i 0)
        (len (length value-string)))
    (while (< i len)
      (let ((char (aref value-string i)))
        (cond
         ;; Escaped character
         ((and (= char ?\\) (< (1+ i) len))
          (setq current (concat current (substring value-string i (+ i 2))))
          (setq i (+ i 2)))
         ;; Unescaped comma - split here
         ((= char ?,)
          (push (ecard--unescape-value current) result)
          (setq current "")
          (setq i (1+ i)))
         ;; Regular character
         (t
          (setq current (concat current (string char)))
          (setq i (1+ i))))))
    ;; Add final component
    (when (> (length current) 0)
      (push (ecard--unescape-value current) result))
    (nreverse result)))

(defun ecard--split-on-unescaped-semicolon (value-string)
  "Split VALUE-STRING on unescaped semicolons, preserving escape sequences.
Returns list of unescaped component values.
Used for structured properties like N, ADR, ORG, GENDER."
  (let ((result nil)
        (current "")
        (i 0)
        (len (length value-string))
        (escaped nil))  ; Is the NEXT character escaped?
    (while (< i len)
      (let ((char (aref value-string i)))
        (cond
         ;; Previous char was backslash, so this char is escaped
         (escaped
          (setq current (concat current "\\" (char-to-string char)))
          (setq escaped nil)
          (setq i (1+ i)))
         ;; This is a backslash - next char will be escaped
         ((eq char ?\\)
          (setq escaped t)
          (setq i (1+ i)))
         ;; Unescaped semicolon - split here
         ((eq char ?\;)
          (push (ecard--unescape-value current) result)
          (setq current "")
          (setq i (1+ i)))
         ;; Regular character
         (t
          (setq current (concat current (char-to-string char)))
          (setq i (1+ i))))))
    ;; Add final component
    (push (ecard--unescape-value current) result)
    (nreverse result)))

(defun ecard--escape-value (value)
  "Escape vCard VALUE according to RFC 6350.
Escapes newlines (\\n), backslashes (\\\\), commas (\\,), semicolons (\\;)."
  (when value
    (setq value (replace-regexp-in-string "\\\\" "\\\\\\\\" value))
    (setq value (replace-regexp-in-string "\n" "\\\\n" value))
    (setq value (replace-regexp-in-string "," "\\\\," value))
    (setq value (replace-regexp-in-string ";" "\\\\;" value))
    value))

(defun ecard--split-parameters (param-string)
  "Split PARAM-STRING on semicolons, respecting quoted values.
Returns list of parameter strings. Quoted values may contain semicolons."
  (let ((result nil)
        (current "")
        (i 0)
        (len (length param-string))
        (in-quotes nil))
    (while (< i len)
      (let ((char (aref param-string i)))
        (cond
         ;; Toggle quote state
         ((eq char ?\")
          (setq current (concat current "\""))
          (setq in-quotes (not in-quotes))
          (setq i (1+ i)))
         ;; Semicolon outside quotes - split here
         ((and (eq char ?\;) (not in-quotes))
          (when (> (length (string-trim current)) 0)
            (push (string-trim current) result))
          (setq current "")
          (setq i (1+ i)))
         ;; Any other character - add to current
         (t
          (setq current (concat current (char-to-string char)))
          (setq i (1+ i))))))
    ;; Add final parameter
    (when (> (length (string-trim current)) 0)
      (push (string-trim current) result))
    (nreverse result)))

(defun ecard--parse-parameters (param-string)
  "Parse vCard parameter string PARAM-STRING into alist.
Returns ((PARAM-NAME . param-value) ...).
Properly handles quoted parameter values that may contain semicolons."
  (when (and param-string (not (string-empty-p param-string)))
    (let ((params (ecard--split-parameters param-string))
          (result nil))
      (dolist (param params)
        (if (string-match "^\\([^=]+\\)=\\(.+\\)$" param)
            (let ((key (upcase (match-string 1 param)))
                  (val (match-string 2 param)))
              ;; Remove quotes if present
              (when (and (>= (length val) 2)
                         (= (aref val 0) ?\")
                         (= (aref val (1- (length val))) ?\"))
                (setq val (substring val 1 -1)))
              (push (cons key val) result))
          ;; Parameter without value
          (push (cons (upcase param) t) result)))
      (nreverse result))))

(defun ecard--format-parameters (parameters)
  "Format PARAMETERS alist into vCard parameter string.
Returns string like \"PARAM1=val1;PARAM2=val2\" or empty string."
  (if (null parameters)
      ""
    (mapconcat (lambda (param)
                 (let ((key (car param))
                       (val (cdr param)))
                   (if (eq val t)
                       key
                     (format "%s=%s" key
                             (if (string-match-p "[;:,]" val)
                                 (format "\"%s\"" val)
                               val)))))
               parameters
               ";")))

(defun ecard--find-value-separator (str)
  "Find position of value separator colon in STR.
Returns the index of the colon, or nil if not found.
Handles both RFC-compliant and lenient parsing:
- Colons inside quoted strings are ignored
- For malformed params like X-AVAILABLE=09:00-17:00, finds the last colon
- For values with colons like urn:uuid:foo, finds the first colon after params

Strategy: Collect all unquoted colon positions, then determine which is the separator.
For parameter values with colons (malformed), the separator is after all param colons."
  (let ((i 0)
        (len (length str))
        (in-quotes nil)
        (colon-positions nil)
        (last-equals -1))

    ;; First pass: collect all unquoted colon positions and track last equals
    (while (< i len)
      (let ((char (aref str i)))
        (cond
         ((eq char ?\")
          (setq in-quotes (not in-quotes)))
         ((and (eq char ?=) (not in-quotes))
          (setq last-equals i))
         ((and (eq char ?:) (not in-quotes))
          (push i colon-positions))))
      (setq i (1+ i)))

    (setq colon-positions (nreverse colon-positions))

    ;; If no colons found, return nil
    (if (null colon-positions)
        nil
      ;; Strategy: Determine if we have evenly-spaced colons (malformed param value)
    ;; or a clear separator pattern (normal case with URI value)
    ;; Malformed: X-AVAILABLE=09:00-17:00 has ALL gaps small (< 10 chars)
    ;; Normal: TYPE=spouse:urn:uuid:foo has large gap before first colon
    (let ((result nil))
      (if (< last-equals 0)
          ;; No equals sign - use first colon
          (setq result (car colon-positions))
        ;; Check if we have multiple colons after the equals sign
        (let ((colons-after-equals (seq-filter (lambda (pos) (> pos last-equals))
                                               colon-positions)))
          (if (< (length colons-after-equals) 2)
              ;; Only one colon after equals - use it
              (setq result (car colons-after-equals))
            ;; Multiple colons - check if they're all closely spaced (malformed)
            ;; or if there's a clear separator (normal)
            (let* ((gaps nil)
                   (prev last-equals))
              ;; Calculate gaps between consecutive colons
              (dolist (colon-pos colons-after-equals)
                (push (- colon-pos prev) gaps)
                (setq prev colon-pos))
              (setq gaps (nreverse gaps))
              ;; If first gap is notably larger than average of remaining gaps,
              ;; it's a normal separator (use first colon)
              ;; Otherwise, malformed parameter value (use last colon)
              (let* ((first-gap (car gaps))
                     (remaining-gaps (cdr gaps))
                     (avg-remaining (if remaining-gaps
                                        (/ (apply #'+ remaining-gaps)
                                           (float (length remaining-gaps)))
                                      0)))
                ;; Check if value after first colon looks like a URI
                (let ((first-colon (car colons-after-equals))
                      (after-first (substring str (1+ (car colons-after-equals)))))
                  (if (string-match-p "^\\(https?\\|urn\\|ftp\\|file\\):" after-first)
                      ;; Value starts with URI scheme - use first colon
                      (setq result first-colon)
                    ;; Not a URI - use gap analysis
                    (if (> first-gap (* 1.5 avg-remaining))
                        ;; First gap is significantly larger - normal case
                        (setq result (car colons-after-equals))
                      ;; Gaps are similar - malformed case
                      (setq result (car (last colons-after-equals)))))))))))
      ;; Fallback: if still no result, use first colon
      (or result (car colon-positions))))))

(defun ecard--parse-property-line (line)
  "Parse a single vCard property LINE.
Returns a plist (:group GROUP :name NAME :parameters PARAMS :value VALUE)."
  ;; First, extract optional group and property name using simple regex
  ;; Pattern: optional group (alphanumeric, underscore, hyphen), then property name (until ; or :)
  (unless (string-match "^\\(?:\\([a-zA-Z0-9_-]+\\)\\.\\)?\\([^;:]+\\)\\(.*\\)$" line)
    (signal 'ecard-parse-error (list "Invalid property line" line)))

  (let* ((group (when-let ((g (match-string 1 line))) (downcase g)))  ; RFC 6350: groups are case-insensitive
         (name (upcase (match-string 2 line)))
         (remainder (match-string 3 line))  ; Everything after property name
         (param-string nil)
         (value-string nil)
         (parameters nil)
         (value nil))

    ;; Parse the remainder to find where parameters end and value begins
    ;; The value separator is the first unquoted colon
    (if (string-prefix-p ":" remainder)
        ;; No parameters, just :value
        (setq value-string (substring remainder 1))
      ;; Has parameters: ;params:value
      (if (string-prefix-p ";" remainder)
          (let* ((params-and-value (substring remainder 1))  ; Remove leading semicolon
                 (colon-pos (ecard--find-value-separator params-and-value)))
            (unless colon-pos
              (signal 'ecard-parse-error (list "No value separator colon found" line)))
            (setq param-string (substring params-and-value 0 colon-pos))
            (setq value-string (substring params-and-value (1+ colon-pos))))
        (signal 'ecard-parse-error (list "Invalid property line format" line))))

    ;; Parse parameters and unescape value
    (setq parameters (ecard--parse-parameters param-string))
    (setq value (ecard--unescape-value value-string))

    ;; Parse structured values for N, ADR, ORG, and GENDER (semicolon-separated components)
    (when (member name '("N" "ADR" "ORG" "GENDER"))
      (setq value (ecard--split-on-unescaped-semicolon value-string)))

    ;; Parse text-list values for CATEGORIES and NICKNAME (comma-separated components)
    (when (member name '("CATEGORIES" "NICKNAME"))
      (setq value (ecard--split-text-list value-string)))

    (list :group group
          :name name
          :parameters parameters
          :value value)))

(defun ecard--property-slot-name (prop-name)
  "Convert property name PROP-NAME to slot symbol.
E.g., \"TEL\" -> tel, \"CALADRURI\" -> caladruri."
  (intern (downcase prop-name)))

(defun ecard--is-cardinality-one-property-p (prop-name)
  "Return non-nil if PROP-NAME has cardinality *1 (at most one).
Per RFC 6350, these properties can appear at most once:
VERSION (exactly 1), N, BDAY, ANNIVERSARY, GENDER, REV, PRODID, UID, KIND."
  (member prop-name '("VERSION" "N" "BDAY" "ANNIVERSARY" "GENDER" "REV"
                      "PRODID" "UID" "KIND")))

(defun ecard--add-property-to-ecard (vc prop-plist)
  "Add property PROP-PLIST to ecard object VC.
PROP-PLIST is a plist from `ecard--parse-property-line'.
Enforces cardinality constraints for *1 properties."
  (let* ((name (plist-get prop-plist :name))
         (prop (ecard-property
                :group (plist-get prop-plist :group)
                :name name
                :parameters (plist-get prop-plist :parameters)
                :value (plist-get prop-plist :value))))

    (cond
     ;; Extended properties (X-*)
     ((string-prefix-p "X-" name)
      (let* ((extended (ecard-extended vc))
             (existing (assoc name extended)))
        (if existing
            (setcdr existing (append (cdr existing) (list prop)))
          (setf (ecard-extended vc) (append extended (list (cons name (list prop))))))))

     ;; Standard properties
     (t
      (let ((slot (ecard--property-slot-name name)))
        (when (ecard--slot-exists-p vc slot)
          (let ((current (ecard--slot-value vc slot)))
            ;; Enforce cardinality *1 constraint
            (when (and (ecard--is-cardinality-one-property-p name)
                       current)
              (signal 'ecard-validation-error
                      (list (format "Property %s can appear at most once (cardinality *1)" name))))
            (ecard--set-slot-value vc slot (append current (list prop))))))))))

(defun ecard--fold-line (line)
  "Fold LINE at 75 octets using space continuation.
Returns list of folded lines."
  (let ((max-len 75)
        (lines nil)
        (current-line "")
        (octets (encode-coding-string line 'utf-8)))

    (if (<= (length octets) max-len)
        (list line)
      ;; Need to fold - be conservative and fold at character boundaries
      (let ((chars (string-to-list line))
            (current-octets 0))
        (dolist (char chars)
          (let* ((char-str (string char))
                 (char-octets (length (encode-coding-string char-str 'utf-8))))
            (if (and (> current-octets 0)
                     (> (+ current-octets char-octets) max-len))
                ;; Need to fold here
                (progn
                  (push current-line lines)
                  (setq current-line (concat " " char-str))
                  (setq current-octets (1+ char-octets)))
              ;; Add to current line
              (setq current-line (concat current-line char-str))
              (setq current-octets (+ current-octets char-octets)))))
        (when (not (string-empty-p current-line))
          (push current-line lines))
        (nreverse lines)))))

(defun ecard--format-property (prop)
  "Format ecard property PROP into vCard line (without folding).
Returns formatted string."
  (let* ((group (ecard-property-group prop))
         (name (ecard-property-name prop))
         (parameters (ecard-property-parameters prop))
         (value (ecard-property-value prop))
         (param-str (ecard--format-parameters parameters))
         (value-str (if (listp value)
                        ;; Text-list properties (CATEGORIES, NICKNAME) use comma separator
                        (if (member name '("CATEGORIES" "NICKNAME"))
                            (mapconcat #'ecard--escape-value value ",")
                          ;; Structured properties (N, ADR, ORG, GENDER) use semicolon separator
                          (mapconcat #'ecard--escape-value value ";"))
                      (ecard--escape-value value))))

    (concat (if group (concat group ".") "")
            name
            (if (not (string-empty-p param-str)) (concat ";" param-str) "")
            ":"
            value-str)))

(defun ecard--serialize-properties (props)
  "Serialize list of ecard properties PROPS into vCard lines.
Returns list of folded lines."
  (let ((lines nil))
    (dolist (prop props)
      (let ((line (ecard--format-property prop)))
        (setq lines (append lines (ecard--fold-line line)))))
    lines))

;;; Public API

(defun ecard--validate-pref-parameters (vc)
  "Validate PREF parameter values are integers 1-100.
VC is the ecard object to validate.
Signals `ecard-validation-error' if any PREF value is out of range."
  (dolist (slot '(fn n nickname photo bday anniversary gender adr tel email
                  impp lang geo tz title role logo org member related
                  categories note prodid rev sound uid clientpidmap url
                  key fburl caladruri caluri source kind xml))
    (let ((props (ecard--slot-value vc slot)))
      (dolist (prop props)
        (let ((params (ecard-property-parameters prop)))
          (when params
            (let ((pref-param (assoc "PREF" params)))
              (when pref-param
                (let* ((pref-value (cdr pref-param))
                       ;; Parse as number and check if it's a valid integer in range
                       (pref-num (ignore-errors (string-to-number pref-value)))
                       (pref-int (and pref-num (truncate pref-num))))
                  (unless (and pref-int
                               (= pref-num pref-int)  ; Ensure it's actually an integer, not a float
                               (>= pref-int 1)
                               (<= pref-int 100))
                    (signal 'ecard-validation-error
                            (list (format "PREF parameter must be integer 1-100, got: %s"
                                          pref-value)))))))))))))

(defun ecard--validate-ecard (vc)
  "Validate required properties for ecard object VC.
Signals `ecard-validation-error' if validation fails."
  ;; Validate required properties
  (unless (ecard-version vc)
    (signal 'ecard-validation-error '("Missing VERSION property")))

  (let ((fn-props (ecard-fn vc)))
    (unless fn-props
      (signal 'ecard-validation-error '("Missing FN (formatted name) property")))
    ;; Validate FN is not empty
    (let* ((fn-prop (car fn-props))
           (fn-value (when fn-prop (ecard-property-value fn-prop))))
      (when (or (null fn-value)
                (and (stringp fn-value) (string-empty-p fn-value)))
        (signal 'ecard-validation-error '("FN (formatted name) property cannot be empty")))))

  ;; Validate VERSION is 4.0
  (let ((version-prop (car (ecard-version vc))))
    (unless (and version-prop
                 (string= (ecard-property-value version-prop) "4.0"))
      (signal 'ecard-validation-error
              (list "Unsupported VERSION"
                    (if version-prop (ecard-property-value version-prop) "nil")))))

  ;; Validate KIND value if present
  (let ((kind-props (ecard-kind vc)))
    (when kind-props
      (let* ((kind-prop (car kind-props))
             (kind-value (downcase (ecard-property-value kind-prop))))
        (unless (member kind-value '("individual" "group" "org" "location"))
          (signal 'ecard-validation-error
                  (list (format "Invalid KIND value: %s (must be individual, group, org, or location)"
                                (ecard-property-value kind-prop))))))))

  ;; Validate MEMBER/KIND relationship
  (let ((member-props (ecard-member vc))
        (kind-props (ecard-kind vc)))
    (when (and member-props
               (or (null kind-props)
                   (not (string= (downcase (ecard-property-value (car kind-props))) "group"))))
      (signal 'ecard-validation-error
              '("MEMBER property requires KIND to be 'group'"))))

  ;; Validate PREF parameter ranges across all properties
  (ecard--validate-pref-parameters vc))

;;;###autoload
(defun ecard-parse-multiple (text)
  "Parse vCard TEXT into a list of ecard objects.
TEXT can contain one or more vCard 4.0 records.
Returns a list of ecard objects, even if only one vCard is present.
Signals `ecard-parse-error' if parsing fails.
Signals `ecard-validation-error' if required properties are missing.

DoS Protection:
- Maximum property value length: 10000 characters
- Maximum properties per vCard: 1000"
  (let* ((lines (ecard--unfold-lines text))
         (vcards nil)
         (current-vc nil)
         (in-ecard nil)
         (property-count 0)
         (first-property-seen nil)
         (max-property-count 1000)
         (max-value-length 10000))

    (dolist (line lines)
      (cond
       ((string-match-p "^BEGIN:VCARD" line)
        (when in-ecard
          (signal 'ecard-parse-error '("Nested BEGIN:VCARD not allowed")))
        (setq in-ecard t)
        (setq current-vc (ecard))
        (setq property-count 0)
        (setq first-property-seen nil))

       ((string-match-p "^END:VCARD" line)
        (unless in-ecard
          (signal 'ecard-parse-error '("END:VCARD without BEGIN:VCARD")))
        (setq in-ecard nil)
        ;; Validate and add completed vCard
        (ecard--validate-ecard current-vc)
        (push current-vc vcards)
        (setq current-vc nil))

       (in-ecard
        ;; Skip empty or whitespace-only lines
        (unless (string-match-p "^[ \t]*$" line)
          ;; Skip lines with binary data (non-printable control characters)
          ;; These could be from corrupted data or improperly encoded PHOTO/LOGO values
          (unless (string-match-p "[\x00-\x08\x0b\x0c\x0e-\x1f\x7f-\x9f]" line)
            ;; DoS protection: limit property count
            (when (>= property-count max-property-count)
              (signal 'ecard-validation-error
                      (list (format "Too many properties (max %d allowed for DoS protection)"
                                    max-property-count))))
            (setq property-count (1+ property-count))

            (let* ((prop-plist (ecard--parse-property-line line))
                   (prop-name (plist-get prop-plist :name))
                   (prop-value (plist-get prop-plist :value)))

              ;; DoS protection: limit property value length
              (let ((value-length (if (listp prop-value)
                                      (apply #'+ (mapcar #'length prop-value))
                                    (length prop-value))))
                (when (> value-length max-value-length)
                  (signal 'ecard-validation-error
                          (list (format "Property value too long (%d chars, max %d allowed for DoS protection)"
                                        value-length max-value-length)))))

              ;; Validate VERSION is the first property after BEGIN
              (unless first-property-seen
                (setq first-property-seen t)
                (unless (string= prop-name "VERSION")
                  (signal 'ecard-validation-error
                          (list (format "VERSION must be the first property after BEGIN:VCARD, got %s"
                                        prop-name)))))

              (ecard--add-property-to-ecard current-vc prop-plist)))))))

    (when in-ecard
      (signal 'ecard-parse-error '("Missing END:VCARD")))

    (nreverse vcards)))

;;;###autoload
(defun ecard-parse (text)
  "Parse vCard TEXT into a ecard object or list of ecard objects.
TEXT should contain one or more complete vCard 4.0 records.

If TEXT contains a single vCard, returns a single ecard object.
If TEXT contains multiple vCards, returns a list of ecard objects.
If TEXT contains no vCards, signals an error.

Signals `ecard-parse-error' if parsing fails.
Signals `ecard-validation-error' if required properties are missing.

For explicit control over return type, use `ecard-parse-multiple'
which always returns a list."
  (let ((vcards (ecard-parse-multiple text)))
    (cond
     ((= (length vcards) 0)
      (signal 'ecard-parse-error '("No vCards found in input")))
     ((= (length vcards) 1)
      (car vcards))
     (t
      vcards))))

;;;###autoload
(defun ecard-parse-file-multiple (filename)
  "Parse vCard from FILENAME and return list of ecard objects.
Always returns a list, even if only one vCard is present.
Signals `ecard-parse-error' if parsing fails."
  (with-temp-buffer
    (insert-file-contents filename)
    (ecard-parse-multiple (buffer-string))))

;;;###autoload
(defun ecard-parse-file (filename)
  "Parse vCard from FILENAME and return ecard object or list.
If FILENAME contains a single vCard, returns a single ecard object.
If FILENAME contains multiple vCards, returns a list of ecard objects.
Signals `ecard-parse-error' if parsing fails.

For explicit control over return type, use `ecard-parse-file-multiple'
which always returns a list."
  (with-temp-buffer
    (insert-file-contents filename)
    (ecard-parse (buffer-string))))

;;;###autoload
(defun ecard-parse-buffer-multiple ()
  "Parse vCard from current buffer and return list of ecard objects.
Always returns a list, even if only one vCard is present.
Signals `ecard-parse-error' if parsing fails."
  (ecard-parse-multiple (buffer-string)))

;;;###autoload
(defun ecard-parse-buffer ()
  "Parse vCard from current buffer and return ecard object or list.
If buffer contains a single vCard, returns a single ecard object.
If buffer contains multiple vCards, returns a list of ecard objects.
Signals `ecard-parse-error' if parsing fails.

For explicit control over return type, use `ecard-parse-buffer-multiple'
which always returns a list."
  (ecard-parse (buffer-string)))

;;;###autoload
(defun ecard-serialize (vc)
  "Serialize ecard object VC to vCard 4.0 text string.
Returns a properly formatted and folded vCard string."
  (let ((lines '("BEGIN:VCARD" "VERSION:4.0")))

    ;; Serialize all properties in defined order
    (dolist (slot '(source kind xml fn n nickname photo bday anniversary gender
                    adr tel email impp lang geo tz title role logo org member related
                    categories note prodid rev sound uid clientpidmap url
                    key fburl caladruri caluri))
      (let ((props (ecard--slot-value vc slot)))
        (when props
          (setq lines (append lines (ecard--serialize-properties props))))))

    ;; Serialize extended properties
    (let ((extended (ecard-extended vc)))
      (dolist (entry extended)
        (let ((props (cdr entry)))
          (setq lines (append lines (ecard--serialize-properties props))))))

    ;; Add END:VCARD
    (setq lines (append lines '("END:VCARD")))

    ;; Join with CRLF as per RFC 6350
    (mapconcat #'identity lines "\r\n")))

;;;###autoload
(defun ecard-serialize-multiple (vcards)
  "Serialize list of ecard objects VCARDS to vCard 4.0 text string.
Each vCard is separated by a newline.
Returns a properly formatted and folded vCard string."
  (mapconcat #'ecard-serialize vcards "\r\n"))

;;;###autoload
(defun ecard-write-file (vc filename)
  "Write ecard object VC to FILENAME.
Creates or overwrites FILENAME with serialized vCard."
  (with-temp-buffer
    (insert (ecard-serialize vc))
    (write-region (point-min) (point-max) filename)))

;;;###autoload
(defun ecard-create (&rest args)
  "Create a new ecard object with properties from ARGS.
ARGS is a plist of property keywords and values.

Supported keywords:
  :fn STRING - Formatted name (required)
  :n LIST - Structured name (family given additional prefix suffix)
  :email STRING-OR-LIST - Email address(es)
  :tel STRING-OR-LIST - Telephone number(s)
  :adr LIST-OR-LISTS - Address(es)
  :org STRING - Organization
  :title STRING - Job title
  :role STRING - Role
  :url STRING - URL
  :note STRING - Note
  :uid STRING - Unique identifier
  :bday STRING - Birthday
  :anniversary STRING - Anniversary
  :gender STRING - Gender
  :nickname STRING - Nickname
  :categories STRING-OR-LIST - Categories
  :geo STRING - Geographical position
  :tz STRING - Time zone
  :photo STRING - Photo data
  :logo STRING - Logo data
  :sound STRING - Sound data
  :key STRING - Public key
  :rev STRING - Revision timestamp
  :prodid STRING - Product ID
  :kind STRING - Kind (individual, group, org, location)
  :source STRING - Source

Example:
  (ecard-create :fn \"John Doe\"
                :email \"john@example.com\"
                :tel \"+1-555-1234\"
                :org \"Example Corp\")"

  (let ((vc (ecard))
        (fn (plist-get args :fn))
        (n (plist-get args :n))
        (email (plist-get args :email))
        (tel (plist-get args :tel))
        (adr (plist-get args :adr))
        (org (plist-get args :org))
        (title (plist-get args :title))
        (role (plist-get args :role))
        (url (plist-get args :url))
        (note (plist-get args :note))
        (uid (plist-get args :uid))
        (bday (plist-get args :bday))
        (anniversary (plist-get args :anniversary))
        (gender (plist-get args :gender))
        (nickname (plist-get args :nickname))
        (categories (plist-get args :categories))
        (geo (plist-get args :geo))
        (tz (plist-get args :tz))
        (photo (plist-get args :photo))
        (logo (plist-get args :logo))
        (sound (plist-get args :sound))
        (key (plist-get args :key))
        (rev (plist-get args :rev))
        (prodid (plist-get args :prodid))
        (kind (plist-get args :kind))
        (source (plist-get args :source)))

    ;; Set VERSION:4.0
    (setf (ecard-version vc) (list (ecard-property :name "VERSION" :value "4.0")))

    ;; Helper macro to add simple properties
    (cl-macrolet ((add-prop (slot-name value)
                    `(when ,value
                       (setf (,(intern (concat "ecard-" (symbol-name slot-name))) vc)
                             (if (listp ,value)
                                 (mapcar (lambda (v)
                                           (ecard-property
                                            :name (upcase (symbol-name ',slot-name))
                                            :value v))
                                         ,value)
                               (list (ecard-property
                                      :name (upcase (symbol-name ',slot-name))
                                      :value ,value)))))))

      ;; Add all properties
      (add-prop fn fn)
      (add-prop email email)
      (add-prop tel tel)
      (add-prop title title)
      (add-prop role role)
      (add-prop url url)
      (add-prop note note)
      (add-prop uid uid)
      (add-prop bday bday)
      (add-prop anniversary anniversary)
      (add-prop gender gender)
      (add-prop nickname nickname)
      (add-prop categories categories)
      (add-prop geo geo)
      (add-prop tz tz)
      (add-prop photo photo)
      (add-prop logo logo)
      (add-prop sound sound)
      (add-prop key key)
      (add-prop rev rev)
      (add-prop prodid prodid)
      (add-prop kind kind)
      (add-prop source source))

    ;; Handle N (structured name) - can be a list
    (when n
      (setf (ecard-n vc) (list (ecard-property :name "N" :value n))))

    ;; Handle ORG (structured organization) - convert string to list
    (when org
      (setf (ecard-org vc) (list (ecard-property :name "ORG"
                                                  :value (if (listp org) org (list org))))))

    ;; Handle GENDER (structured) - convert string to list
    (when gender
      (setf (ecard-gender vc) (list (ecard-property :name "GENDER"
                                                     :value (if (listp gender) gender (list gender))))))

    ;; Handle ADR (structured address) - can be list of lists
    (when adr
      (setf (ecard-adr vc)
            (if (and (listp adr) (listp (car adr)) (stringp (car (car adr))))
                ;; List of addresses
                (mapcar (lambda (a) (ecard-property :name "ADR" :value a)) adr)
              ;; Single address
              (list (ecard-property :name "ADR" :value adr)))))

    ;; Validate FN is present
    (unless (ecard-fn vc)
      (signal 'ecard-validation-error '("FN (formatted name) is required")))

    vc))

;;; Helper functions for property access

(defun ecard-get-property-values (vc property-name)
  "Get all values for PROPERTY-NAME from ecard object VC.
Returns list of values (strings or lists for structured properties).
PROPERTY-NAME should be a lowercase symbol (e.g., \\='fn, \\='email)."
  (let ((props (ecard--slot-value vc property-name)))
    (mapcar (lambda (prop) (ecard-property-value prop)) props)))

(defun ecard-get-property-value (vc property-name)
  "Get first value for PROPERTY-NAME from ecard object VC.
Returns string or list for structured properties, or nil if not found.
PROPERTY-NAME should be a lowercase symbol (e.g., \\='fn, \\='email)."
  (let ((props (ecard--slot-value vc property-name)))
    (when props
      (ecard-property-value (car props)))))

(defun ecard-set-property (vc property-name value &optional parameters group)
  "Set PROPERTY-NAME in ecard object VC to VALUE.
Replaces all existing values for this property.
PROPERTY-NAME should be a lowercase symbol (e.g., \\='fn, \\='email).
VALUE is a string or list (for structured properties).
Optional PARAMETERS is an alist of property parameters.
Optional GROUP is the property group string."
  (let ((prop (ecard-property
               :name (upcase (symbol-name property-name))
               :value value
               :parameters parameters
               :group group)))
    (ecard--set-slot-value vc property-name (list prop))))

(defun ecard-add-property (vc property-name value &optional parameters group)
  "Add PROPERTY-NAME to ecard object VC with VALUE.
Appends to existing values for this property.
PROPERTY-NAME should be a lowercase symbol (e.g., \\='fn, \\='email).
VALUE is a string or list (for structured properties).
Optional PARAMETERS is an alist of property parameters.
Optional GROUP is the property group string."
  (let ((prop (ecard-property
               :name (upcase (symbol-name property-name))
               :value value
               :parameters parameters
               :group group))
        (existing (ecard--slot-value vc property-name)))
    (ecard--set-slot-value vc property-name (append existing (list prop)))))

(provide 'ecard)
;;; ecard.el ends here
