;;; ecard-compat.el --- vCard 2.1/3.0 to 4.0 converter -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: comm, data, ecard
;; URL: https://github.com/jwiegley/dot-emacs

;;; Commentary:

;; This package provides compatibility support for reading vCard 2.1 and 3.0
;; formats and converting them to vCard 4.0 EIEIO objects.
;;
;; Features:
;; - Automatic version detection (2.1, 3.0, 4.0)
;; - BASE64 and QUOTED-PRINTABLE encoding support
;; - Character set conversion to UTF-8
;; - Property name and parameter mapping
;; - Multi-vCard file parsing
;; - Clean integration with ecard.el
;;
;; Example usage:
;;
;;   ;; Parse legacy vCard (auto-detects version)
;;   (setq card (ecard-compat-parse "BEGIN:VCARD\nVERSION:3.0\n..."))
;;
;;   ;; Parse file with multiple vCards
;;   (setq cards (ecard-compat-parse-file "~/contacts.vcf"))
;;
;;   ;; Parse specific version
;;   (setq card (ecard-compat-parse-21 "BEGIN:VCARD\nVERSION:2.1\n..."))
;;   (setq card (ecard-compat-parse-30 "BEGIN:VCARD\nVERSION:3.0\n..."))

;;; Code:

(require 'ecard)
(require 'cl-lib)
(require 'seq)

;;; Custom group

(defgroup ecard-compat nil
  "Provide vCard 2.1/3.0 compatibility layer for vCard 4.0."
  :group 'ecard
  :prefix "ecard-compat-")

;;; Error conditions

(define-error 'ecard-compat-error "vCard compatibility error")
(define-error 'ecard-compat-encoding-error "vCard encoding error" 'ecard-compat-error)
(define-error 'ecard-compat-version-error "vCard version error" 'ecard-compat-error)

;;; Constants

(defconst ecard-compat--dropped-properties
  '("LABEL" "MAILER" "CLASS" "AGENT" "NAME")
  "Properties in vCard 2.1/3.0 that don't exist in 4.0.
These properties are silently dropped during conversion:
- LABEL: Address labels (merged into ADR in 4.0)
- MAILER: Email program name (obsolete)
- CLASS: Access classification (obsolete)
- AGENT: Assistant representation (deprecated)
- NAME: Display name for SOURCE (obsolete)")

(defconst ecard-compat--type-value-mapping
  '(("HOME" . "home")
    ("WORK" . "work")
    ("PREF" . "pref")
    ("VOICE" . "voice")
    ("FAX" . "fax")
    ("CELL" . "cell")
    ("PAGER" . "pager")
    ("VIDEO" . "video")
    ("MSG" . "text")
    ("INTERNET" . nil)  ; Default for EMAIL in 4.0
    ("DOM" . nil)       ; Geographic hint, not used in 4.0
    ("INTL" . nil)      ; Geographic hint, not used in 4.0
    ("POSTAL" . nil)    ; Default for ADR in 4.0
    ("PARCEL" . nil))   ; Default for ADR in 4.0
  "Mapping of vCard 2.1/3.0 type values to vCard 4.0.
Nil values indicate the type should be dropped.")

;;; Version detection

(defun ecard-compat--detect-version (text)
  "Detect vCard version from TEXT.
Returns symbol: \\='v21, \\='v30, \\='v40, or nil if unknown."
  (when (string-match "VERSION:\\s-*\\([0-9.]+\\)" text)
    (let ((version (match-string 1 text)))
      (cond
       ((string= version "2.1") 'v21)
       ((string= version "3.0") 'v30)
       ((string= version "4.0") 'v40)
       (t nil)))))

;;; Encoding functions

(defun ecard-compat--decode-base64 (encoded-value)
  "Decode BASE64 ENCODED-VALUE.
Returns decoded string."
  (condition-case err
      (base64-decode-string encoded-value)
    (error
     (signal 'ecard-compat-encoding-error
             (list "BASE64 decode failed" (error-message-string err))))))

(defun ecard-compat--decode-quoted-printable (encoded-value &optional charset)
  "Decode QUOTED-PRINTABLE ENCODED-VALUE with optional CHARSET.
Returns decoded string.
Handles soft line breaks (=\\r\\n or =\\n) and =XX hex sequences.

Performance: Uses list accumulation instead of O(n²) string concatenation."
  (condition-case err
      (let ((chars nil)  ; Accumulate characters in reverse order - O(1) per char
            (i 0)
            (len (length encoded-value)))
        ;; First, remove soft line breaks
        (setq encoded-value (replace-regexp-in-string "=[\r\n]+" "" encoded-value))
        (setq len (length encoded-value))

        ;; Decode =XX sequences - accumulate chars in list
        (while (< i len)
          (let ((char (aref encoded-value i)))
            (if (and (= char ?=) (< (+ i 2) len))
                (let ((hex (substring encoded-value (1+ i) (+ i 3))))
                  (push (string-to-number hex 16) chars)
                  (setq i (+ i 3)))
              (push char chars)
              (setq i (1+ i)))))

        ;; Build result string from accumulated characters - O(n)
        (let ((result (concat (nreverse chars))))
          ;; Convert from charset if specified
          (if charset
              (ecard-compat--convert-charset result charset)
            result)))
    (error
     (signal 'ecard-compat-encoding-error
             (list "QUOTED-PRINTABLE decode failed" (error-message-string err))))))

(defun ecard-compat--decode-value (value encoding &optional charset)
  "Decode VALUE with ENCODING and optional CHARSET.
ENCODING can be \"BASE64\", \"QUOTED-PRINTABLE\", \"b\", \"B\", etc.
Returns decoded string."
  (when (and value encoding)
    (let ((enc (upcase encoding)))
      (cond
       ((or (string= enc "BASE64") (string= enc "B"))
        ;; Remove whitespace from base64 data
        (setq value (replace-regexp-in-string "[ \t\r\n]" "" value))
        (ecard-compat--decode-base64 value))
       ((or (string= enc "QUOTED-PRINTABLE") (string= enc "Q"))
        (ecard-compat--decode-quoted-printable value charset))
       (t value)))))

(defun ecard-compat--convert-charset (value charset)
  "Convert VALUE from CHARSET to UTF-8.
CHARSET is string like \"ISO-8859-1\" or \"UTF-8\".
Returns UTF-8 encoded string."
  (if (or (null charset)
          (string-match-p "UTF-?8" (upcase charset)))
      value
    (condition-case err
        (let ((coding-system (intern (downcase charset))))
          (if (coding-system-p coding-system)
              (decode-coding-string value coding-system)
            value))
      (error
       (message "vCard charset conversion failed for %s: %s"
                charset (error-message-string err))
       value))))

(defun ecard-compat--create-data-uri (base64-data media-type)
  "Create data: URI from BASE64-DATA with MEDIA-TYPE.
Returns string like \"data:image/jpeg;base64,<data>\"."
  (format "data:%s;base64,%s" media-type base64-data))

;;; Property filtering

(defun ecard-compat--should-include-property-p (prop-name)
  "Return t if PROP-NAME should be included in conversion.
Returns nil for properties that don't exist in vCard 4.0."
  (not (member (upcase prop-name) ecard-compat--dropped-properties)))

;;; Parameter conversion

(defun ecard-compat--map-type-value (type-value)
  "Map vCard 2.1/3.0 TYPE-VALUE to vCard 4.0.
Returns mapped value or nil if it should be dropped."
  (let ((mapping (assoc (upcase type-value) ecard-compat--type-value-mapping)))
    (if mapping
        (cdr mapping)
      ;; Unknown type values pass through as lowercase
      (downcase type-value))))

(defun ecard-compat--convert-params-21 (params _prop-name)
  "Convert vCard 2.1 PARAMS to vCard 4.0 format.
PARAMS is alist like ((\"HOME\" . t) (\"VOICE\" . t)).
Returns alist like ((\"TYPE\" . \"home,voice\"))."
  (let ((types '())
        (other-params '())
        (encoding nil)
        (charset nil))

    (dolist (param params)
      (let ((name (upcase (car param)))
            (value (cdr param)))
        (cond
         ;; Type indicators (no value in 2.1)
         ((member name '("HOME" "WORK" "PREF" "VOICE" "FAX" "CELL"
                         "PAGER" "VIDEO" "MSG" "INTERNET" "DOM" "INTL"
                         "POSTAL" "PARCEL"))
          (let ((mapped (ecard-compat--map-type-value name)))
            (when mapped
              (push mapped types))))

         ;; Encoding parameter (handle separately)
         ((string= name "ENCODING")
          (setq encoding value))

         ;; Charset parameter (handle separately)
         ((string= name "CHARSET")
          (setq charset value))

         ;; Value type parameter
         ((string= name "VALUE")
          (push (cons "VALUE" (downcase value)) other-params))

         ;; Other parameters with values
         (t
          (push (cons name value) other-params)))))

    ;; Build result
    (when types
      (push (cons "TYPE" (mapconcat #'identity (nreverse types) ","))
            other-params))

    (list :params (nreverse other-params)
          :encoding encoding
          :charset charset)))

(defun ecard-compat--convert-params-30 (params _prop-name)
  "Convert vCard 3.0 PARAMS to vCard 4.0 format.
PARAMS is alist like ((\"TYPE\" . \"HOME,WORK\")).
Returns alist like ((\"TYPE\" . \"home,work\"))."
  (let ((converted '())
        (encoding nil)
        (charset nil))

    (dolist (param params)
      (let ((name (upcase (car param)))
            (value (cdr param)))
        (cond
         ;; Encoding parameter (handle separately)
         ((string= name "ENCODING")
          (setq encoding value))

         ;; Charset parameter (handle separately)
         ((string= name "CHARSET")
          (setq charset value))

         ;; Type parameter - lowercase all values
         ((string= name "TYPE")
          (let* ((type-values (split-string value "," t "[ \t]*"))
                 (mapped-values (delq nil (mapcar #'ecard-compat--map-type-value
                                                  type-values))))
            (when mapped-values
              (push (cons "TYPE" (mapconcat #'identity mapped-values ","))
                    converted))))

         ;; Value type parameter
         ((string= name "VALUE")
          (push (cons "VALUE" (downcase value)) converted))

         ;; Other parameters
         (t
          (push param converted)))))

    (list :params (nreverse converted)
          :encoding encoding
          :charset charset)))

;;; Legacy property parsing

(defun ecard-compat--parse-legacy-params (param-string _version)
  "Parse legacy parameter string PARAM-STRING.
Returns alist of parameters."
  (when (and param-string (not (string-empty-p param-string)))
    (let ((params (split-string param-string ";" t "[ \t]*"))
          (result nil))
      (dolist (param params)
        (if (string-match "^\\([^=]+\\)=\\(.+\\)$" param)
            ;; PARAM=value format (3.0 style, but also in 2.1 for some params)
            (let ((key (upcase (match-string 1 param)))
                  (val (match-string 2 param)))
              ;; Remove quotes if present
              (when (and (>= (length val) 2)
                         (= (aref val 0) ?\")
                         (= (aref val (1- (length val))) ?\"))
                (setq val (substring val 1 -1)))
              (push (cons key val) result))
          ;; Parameter without value (2.1 style type indicators)
          (push (cons (upcase param) t) result)))
      (nreverse result))))

(defun ecard-compat--parse-legacy-property (line version)
  "Parse a legacy vCard property LINE for VERSION.
VERSION is \\='v21 or \\='v30.
Returns plist with :name, :params, :value, :encoding, :charset."
  (when (string-match "^\\(?:\\([a-zA-Z0-9_-]+\\)\\.\\)?\\([^:;]+\\)\\(?:;\\([^:]*\\)\\)?:\\(.*\\)$" line)
    (let* ((group (match-string 1 line))
           (prop-name (upcase (match-string 2 line)))
           (params-string (match-string 3 line))
           (value (match-string 4 line))
           (params (ecard-compat--parse-legacy-params params-string version))
           (converted (if (eq version 'v21)
                         (ecard-compat--convert-params-21 params prop-name)
                       (ecard-compat--convert-params-30 params prop-name))))

      (list :group group
            :name prop-name
            :params (plist-get converted :params)
            :value value
            :encoding (plist-get converted :encoding)
            :charset (plist-get converted :charset)))))

;;; Media type detection

(defun ecard-compat--detect-media-type (prop-name params)
  "Detect media type for PROP-NAME with PARAMS.
Returns string like \"image/jpeg\" or nil."
  (let ((type-param (cdr (assoc "TYPE" params)))
        (value-param (cdr (assoc "VALUE" params))))
    (cond
     ;; If VALUE=uri, it's already a URI
     ((and value-param (string= (downcase value-param) "uri"))
      nil)

     ;; If TYPE parameter specifies media type
     ((and type-param (string-match-p "/" type-param))
      type-param)

     ;; Default media types by property
     ((string= prop-name "PHOTO") "image/jpeg")
     ((string= prop-name "LOGO") "image/png")
     ((string= prop-name "SOUND") "audio/basic")
     (t nil))))

;;; vCard 4.0 object construction

(defun ecard-compat--add-property-to-ecard (vc name value params group)
  "Add property to ecard object VC.
NAME is property name (uppercase string).
VALUE is property value (string or list).
PARAMS is alist of parameters.
GROUP is optional group string.

Performance: Uses O(1) cons instead of O(n) append.
Note: Properties are stored in reverse order of appearance, but RFC 6350
does not mandate property order, and serialization maintains data fidelity."
  (let ((slot (intern (downcase name))))
    (when (and (ecard--slot-exists-p vc slot)
               (not (eq slot 'version)))  ; Don't override VERSION
      (let* ((prop (ecard-property
                   :name name
                   :value value
                   :parameters params
                   :group group))
             (existing (ecard--slot-value vc slot)))

        ;; Check cardinality constraints
        (when (and (ecard--is-cardinality-one-property-p name)
                   existing)
          ;; For *1 properties, replace rather than append
          (ecard--set-slot-value vc slot (list prop)))

        ;; For other properties, prepend using cons - O(1) instead of O(n)
        (unless (and (ecard--is-cardinality-one-property-p name)
                     existing)
          (ecard--set-slot-value vc slot (cons prop existing)))))))

(defun ecard-compat--build-ecard-40 (fn properties)
  "Build vCard 4.0 object from FN and PROPERTIES list.
FN is the formatted name string (required).
PROPERTIES is list of plists with :name, :value, :params, :group.
Returns ecard object."
  (let ((vc (ecard-create :fn (or fn "Unknown"))))

    ;; Add all properties
    (dolist (prop properties)
      (let ((name (plist-get prop :name))
            (value (plist-get prop :value))
            (params (plist-get prop :params))
            (group (plist-get prop :group)))

        ;; Skip VERSION and FN (already set by ecard-create)
        (unless (member name '("VERSION" "FN"))
          (ecard-compat--add-property-to-ecard vc name value params group))))

    vc))

;;; Property value processing

(defun ecard-compat--process-property-value (name value params encoding charset _version)
  "Process property VALUE for NAME with PARAMS, ENCODING, and CHARSET.
Returns processed value (string or list)."
  (let ((processed-value value))

    ;; First decode if encoded
    (when encoding
      (setq processed-value
            (or (ecard-compat--decode-value processed-value encoding charset)
                processed-value)))

    ;; Then convert charset if specified and not encoded
    (when (and charset (not encoding))
      (setq processed-value
            (ecard-compat--convert-charset processed-value charset)))

    ;; For binary properties with base64 encoding, create data URI
    (when (and encoding
               (or (string= (upcase encoding) "BASE64")
                   (string= (upcase encoding) "B"))
               (member name '("PHOTO" "LOGO" "SOUND" "KEY")))
      (let ((media-type (ecard-compat--detect-media-type name params)))
        (when media-type
          (setq processed-value
                (ecard-compat--create-data-uri processed-value media-type)))))

    ;; Parse structured values for N, ADR, ORG, GENDER (semicolon-separated)
    (when (member name '("N" "ADR" "ORG" "GENDER"))
      (setq processed-value
            (mapcar (lambda (s)
                     (ecard--unescape-value s))
                   (split-string processed-value ";" nil))))

    ;; Parse text-list values for CATEGORIES, NICKNAME (comma-separated)
    (when (member name '("CATEGORIES" "NICKNAME"))
      (setq processed-value
            (ecard--split-text-list processed-value)))

    ;; For other properties, unescape the value
    (unless (or (listp processed-value)
                (member name '("PHOTO" "LOGO" "SOUND" "KEY")))
      (setq processed-value (ecard--unescape-value processed-value)))

    processed-value))

;;; Main conversion functions

(defun ecard-compat--properties-to-ecard-40 (properties version)
  "Convert PROPERTIES list from VERSION to vCard 4.0 object.
PROPERTIES is list of plists from ecard-compat--parse-legacy-property.
VERSION is \\='v21 or \\='v30.
Returns ecard object."
  (let ((fn nil)
        (converted-props '()))

    ;; Process each property
    (dolist (prop properties)
      (let* ((name (plist-get prop :name))
             (value (plist-get prop :value))
             (params (plist-get prop :params))
             (group (plist-get prop :group))
             (encoding (plist-get prop :encoding))
             (charset (plist-get prop :charset)))

        ;; Skip dropped properties
        (when (ecard-compat--should-include-property-p name)
          ;; Process value (decode, convert charset, etc.)
          (setq value (ecard-compat--process-property-value
                      name value params encoding charset version))

          ;; Store FN for later
          (when (string= name "FN")
            (setq fn value))

          ;; Add to converted properties
          (push (list :name name
                     :value value
                     :params params
                     :group group)
                converted-props))))

    ;; Create vCard 4.0 object
    (ecard-compat--build-ecard-40 fn (nreverse converted-props))))

(defun ecard-compat--parse-legacy-ecard (text version)
  "Parse legacy vCard TEXT for VERSION.
VERSION is \\='v21 or \\='v30.
Returns ecard object."
  (let* ((lines (ecard--unfold-lines text))
         (properties '())
         (in-ecard nil))

    ;; Parse all properties
    (dolist (line lines)
      (cond
       ((string-match-p "^BEGIN:VCARD" line)
        (setq in-ecard t))

       ((string-match-p "^END:VCARD" line)
        (setq in-ecard nil))

       ((and in-ecard (not (string-match-p "^\\s-*$" line)))
        (when-let ((parsed (ecard-compat--parse-legacy-property line version)))
          (push parsed properties)))))

    ;; Convert to vCard 4.0
    (ecard-compat--properties-to-ecard-40 (nreverse properties) version)))

;;;###autoload
(defun ecard-compat-parse-21 (text)
  "Parse vCard 2.1 TEXT and convert to vCard 4.0 object.
Handles BASE64 and QUOTED-PRINTABLE encodings.
Converts character sets to UTF-8.
Returns ecard object."
  (ecard-compat--parse-legacy-ecard text 'v21))

;;;###autoload
(defun ecard-compat-parse-30 (text)
  "Parse vCard 3.0 TEXT and convert to vCard 4.0 object.
Handles BASE64 encoding.
Converts character sets to UTF-8.
Returns ecard object."
  (ecard-compat--parse-legacy-ecard text 'v30))

;;;###autoload
(defun ecard-compat-parse (text)
  "Parse vCard TEXT of any version (2.1, 3.0, 4.0) and convert to vCard 4.0.
Auto-detects version and applies appropriate conversion.
Returns ecard object.
Signals `ecard-compat-version-error' if version is unknown or missing."
  (let ((version (ecard-compat--detect-version text)))
    (cond
     ((eq version 'v21) (ecard-compat-parse-21 text))
     ((eq version 'v30) (ecard-compat-parse-30 text))
     ((eq version 'v40) (ecard-parse text))
     (t (signal 'ecard-compat-version-error
               '("Unknown or missing vCard VERSION"))))))

;;;###autoload
(defun ecard-compat-parse-multiple (text)
  "Parse TEXT containing one or more vCards of any version.
Auto-detects each vCard's version and converts to vCard 4.0.
Returns list of vCard 4.0 objects.

Performance: Uses list accumulation for O(n) line building
instead of O(n²) concatenation."
  (let ((vcards '())
        (current-lines nil)  ; Accumulate lines in reverse order - O(1) per line
        (in-ecard nil))

    (dolist (line (split-string text "[\r\n]+" t))
      (cond
       ((string-match-p "^BEGIN:VCARD" line)
        (setq in-ecard t)
        (setq current-lines (list line)))  ; Start new vCard

       ((string-match-p "^END:VCARD" line)
        (push line current-lines)
        ;; Build vCard text from accumulated lines - O(n)
        (let ((current-ecard (mapconcat #'identity (nreverse current-lines) "\n")))
          (condition-case err
              (push (ecard-compat-parse current-ecard) vcards)
            (error
             (message "Failed to parse vCard: %s" (error-message-string err)))))
        (setq in-ecard nil)
        (setq current-lines nil))

       (in-ecard
        (push line current-lines))))  ; Accumulate line - O(1)

    (nreverse vcards)))

;;;###autoload
(defun ecard-compat-parse-buffer (&optional buffer)
  "Parse vCard data from BUFFER and convert to vCard 4.0.
BUFFER defaults to current buffer if not specified.
Auto-detects each vCard's version (2.1, 3.0, 4.0) and converts to vCard 4.0.

Returns a single ecard object if only one vCard is found.
Returns a list of ecard objects if multiple vCards are found.
Returns nil if no vCards are found.

Example:
  (with-current-buffer \"contacts.vcf\"
    (ecard-compat-parse-buffer))
  => ecard object or list of ecard objects"
  (with-current-buffer (or buffer (current-buffer))
    (let ((vcards (ecard-compat-parse-multiple (buffer-string))))
      (cond
       ((null vcards) nil)
       ((= (length vcards) 1) (car vcards))
       (t vcards)))))

;;;###autoload
(defun ecard-compat-parse-file (filename)
  "Parse vCard file FILENAME of any version and convert to vCard 4.0.
Returns ecard object or list of ecard objects.
If FILENAME contains a single vCard, returns a single ecard object.
If FILENAME contains multiple vCards, returns a list of ecard objects."
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((vcards (ecard-compat-parse-multiple (buffer-string))))
      (if (= (length vcards) 1)
          (car vcards)
        vcards))))

;;; vCard 3.0 Serialization
;;
;; Functions for converting vCard 4.0 objects to vCard 3.0 format.
;; This is the reverse of the parsing functions above.

(defun ecard-compat--extract-data-uri (data-uri)
  "Extract media type and base64 data from DATA-URI.
Returns cons cell (MEDIA-TYPE . BASE64-DATA) or nil if not a data URI."
  (when (and data-uri (string-prefix-p "data:" data-uri))
    (if (string-match "^data:\\([^;,]+\\);base64,\\(.*\\)$" data-uri)
        (cons (match-string 1 data-uri)
              (match-string 2 data-uri))
      nil)))

(defun ecard-compat--needs-quoted-printable-p (value)
  "Return t if VALUE contains non-ASCII characters requiring QUOTED-PRINTABLE.
vCard 3.0 requires CHARSET and optionally ENCODING for non-ASCII text."
  (and value
       (stringp value)
       (not (string-match-p "\\`[[:ascii:]]*\\'" value))))

(defun ecard-compat--encode-quoted-printable (value)
  "Encode VALUE using QUOTED-PRINTABLE encoding.
Encodes bytes that are not printable ASCII (33-126 except =) as =XX."
  (let ((encoded "")
        (bytes (encode-coding-string value 'utf-8)))
    (dotimes (i (length bytes))
      (let ((byte (aref bytes i)))
        (if (or (< byte 33) (> byte 126) (= byte ?=))
            (setq encoded (concat encoded (format "=%02X" byte)))
          (setq encoded (concat encoded (char-to-string byte))))))
    encoded))

(defun ecard-compat--format-parameters-30 (parameters)
  "Format PARAMETERS alist into vCard 3.0 parameter string.
Unlike vCard 4.0, vCard 3.0 doesn't quote comma-separated TYPE values.
Returns string like \"PARAM1=val1;PARAM2=val2\" or empty string."
  (if (null parameters)
      ""
    (mapconcat (lambda (param)
                 (let ((key (car param))
                       (val (cdr param)))
                   (if (eq val t)
                       key
                     (format "%s=%s" key val))))
               parameters
               ";")))

(defun ecard-compat--convert-params-to-30 (params prop-name value)
  "Convert vCard 4.0 PARAMS to vCard 3.0 format for PROP-NAME.
VALUE is the property value, used to determine encoding needs.
Returns alist of parameters suitable for vCard 3.0."
  (let ((converted '())
        (has-encoding nil))

    (dolist (param params)
      (let ((name (upcase (car param)))
            (val (cdr param)))
        (cond
         ;; TYPE parameter - uppercase values for 3.0
         ((string= name "TYPE")
          (let ((type-values (split-string val "," t "[ \t]*")))
            (push (cons "TYPE" (mapconcat #'upcase type-values ","))
                  converted)))

         ;; VALUE parameter - keep as-is
         ((string= name "VALUE")
          (push (cons "VALUE" (upcase val)) converted))

         ;; ENCODING parameter - already present
         ((string= name "ENCODING")
          (setq has-encoding t)
          (push param converted))

         ;; CHARSET parameter - already present
         ((string= name "CHARSET")
          (push param converted))

         ;; Other parameters - keep as-is
         (t
          (push param converted)))))

    ;; Add CHARSET=UTF-8 if value has non-ASCII and no encoding specified
    (when (and (ecard-compat--needs-quoted-printable-p value)
               (not has-encoding)
               (not (member prop-name '("PHOTO" "LOGO" "SOUND" "KEY"))))
      (push (cons "CHARSET" "UTF-8") converted))

    (nreverse converted)))

(defun ecard-compat--format-property-30 (prop)
  "Format ecard property PROP into vCard 3.0 line (without folding).
Returns formatted string."
  (let* ((group (ecard-property-group prop))
         (name (ecard-property-name prop))
         (parameters (ecard-property-parameters prop))
         (value (ecard-property-value prop))
         (is-binary (member name '("PHOTO" "LOGO" "SOUND" "KEY")))
         (actual-value value)
         (extra-params '()))

    ;; Handle binary properties with data URIs
    (when (and is-binary (stringp value))
      (let ((extracted (ecard-compat--extract-data-uri value)))
        (if extracted
            (progn
              ;; Extract media type and base64 data
              (setq actual-value (cdr extracted))
              (push (cons "ENCODING" "BASE64") extra-params)
              (push (cons "TYPE" (car extracted)) extra-params))
          ;; Not a data URI, use as-is
          (setq actual-value value))))

    ;; Convert parameters to 3.0 format
    (let* ((all-params (append extra-params parameters))
           (params-30 (ecard-compat--convert-params-to-30
                       all-params name actual-value))
           (param-str (ecard-compat--format-parameters-30 params-30))
           (value-str (if (listp actual-value)
                          ;; Text-list properties use comma separator
                          (if (member name '("CATEGORIES" "NICKNAME"))
                              (mapconcat #'ecard--escape-value actual-value ",")
                            ;; Structured properties use semicolon separator
                            (mapconcat #'ecard--escape-value actual-value ";"))
                        (ecard--escape-value actual-value))))

      (concat (if group (concat group ".") "")
              name
              (if (not (string-empty-p param-str)) (concat ";" param-str) "")
              ":"
              value-str))))

(defun ecard-compat--serialize-properties-30 (props)
  "Serialize list of ecard properties PROPS into vCard 3.0 lines.
Returns list of folded lines.

Special handling: For text-list properties (CATEGORIES, NICKNAME),
if there are multiple properties with single values, they are combined
into a single property with comma-separated values."
  (let ((lines nil)
        (text-list-props '("CATEGORIES" "NICKNAME"))
        (pending-text-lists (make-hash-table :test 'equal)))

    ;; First pass: collect text-list properties
    (dolist (prop props)
      (let ((name (ecard-property-name prop)))
        (if (member name text-list-props)
            ;; Accumulate text-list values
            (let ((existing (gethash name pending-text-lists)))
              (puthash name (append existing (list prop)) pending-text-lists))
          ;; Regular property - serialize immediately
          (let ((line (ecard-compat--format-property-30 prop)))
            (setq lines (append lines (ecard--fold-line line)))))))

    ;; Second pass: serialize accumulated text-list properties
    (maphash
     (lambda (name prop-list)
       (if (and (= (length prop-list) 1)
                (listp (ecard-property-value (car prop-list))))
           ;; Single property with list value - serialize as-is
           (let ((line (ecard-compat--format-property-30 (car prop-list))))
             (setq lines (append lines (ecard--fold-line line))))
         ;; Multiple properties with single values - combine them
         (let* ((first-prop (car prop-list))
                (combined-values (mapcar (lambda (p) (ecard-property-value p)) prop-list))
                (combined-prop (ecard-property
                                :name name
                                :value combined-values
                                :parameters (ecard-property-parameters first-prop)
                                :group (ecard-property-group first-prop))))
           (let ((line (ecard-compat--format-property-30 combined-prop)))
             (setq lines (append lines (ecard--fold-line line)))))))
     pending-text-lists)

    lines))

;;;###autoload
(defun ecard-compat-serialize (vc)
  "Serialize ecard object VC to vCard 3.0 text string.
Converts vCard 4.0 properties to vCard 3.0 format.
Returns a properly formatted and folded vCard 3.0 string.

Key conversions:
- VERSION:4.0 → VERSION:3.0
- Data URIs → ENCODING=BASE64 for binary properties
- Adds CHARSET=UTF-8 for non-ASCII text
- TYPE parameter values uppercased
- UID urn:uuid: prefix preserved (optional in 3.0)

Example:
  (setq card (ecard-create :fn \"John Doe\" :email \"john@example.com\"))
  (ecard-compat-serialize card)
  => \"BEGIN:VCARD\\r\\nVERSION:3.0\\r\\n...\""
  (let ((lines '("BEGIN:VCARD" "VERSION:3.0")))

    ;; Serialize all properties in defined order (same as 4.0)
    (dolist (slot '(source kind xml fn n nickname photo bday anniversary gender
                    adr tel email impp lang geo tz title role logo org member related
                    categories note prodid rev sound uid clientpidmap url
                    key fburl caladruri caluri))
      (let ((props (ecard--slot-value vc slot)))
        (when props
          (setq lines (append lines (ecard-compat--serialize-properties-30 props))))))

    ;; Serialize extended properties (X-*)
    (let ((extended (ecard-extended vc)))
      (dolist (entry extended)
        (let ((props (cdr entry)))
          (setq lines (append lines (ecard-compat--serialize-properties-30 props))))))

    ;; Add END:VCARD
    (setq lines (append lines '("END:VCARD")))

    ;; Join with CRLF as per RFC 2425
    (mapconcat #'identity lines "\r\n")))

;;;###autoload
(defun ecard-compat-serialize-multiple (vcards)
  "Serialize list of ecard objects VCARDS to vCard 3.0 text string.
Each vCard is separated by a newline.
Returns a properly formatted and folded vCard 3.0 string."
  (mapconcat #'ecard-compat-serialize vcards "\r\n"))

(provide 'ecard-compat)
;;; ecard-compat.el ends here
