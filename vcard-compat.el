;;; vcard-compat.el --- vCard 2.1/3.0 to 4.0 converter -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: comm, data, vcard
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
;; - Clean integration with vcard.el
;;
;; Example usage:
;;
;;   ;; Parse legacy vCard (auto-detects version)
;;   (setq card (vcard-compat-parse "BEGIN:VCARD\nVERSION:3.0\n..."))
;;
;;   ;; Parse file with multiple vCards
;;   (setq cards (vcard-compat-parse-file "~/contacts.vcf"))
;;
;;   ;; Parse specific version
;;   (setq card (vcard-compat-parse-21 "BEGIN:VCARD\nVERSION:2.1\n..."))
;;   (setq card (vcard-compat-parse-30 "BEGIN:VCARD\nVERSION:3.0\n..."))

;;; Code:

(require 'vcard)
(require 'cl-lib)
(require 'seq)

;;; Custom group

(defgroup vcard-compat nil
  "Provide vCard 2.1/3.0 compatibility layer for vCard 4.0."
  :group 'vcard
  :prefix "vcard-compat-")

;;; Error conditions

(define-error 'vcard-compat-error "vCard compatibility error")
(define-error 'vcard-compat-encoding-error "vCard encoding error" 'vcard-compat-error)
(define-error 'vcard-compat-version-error "vCard version error" 'vcard-compat-error)

;;; Constants

(defconst vcard-compat--dropped-properties
  '("LABEL" "MAILER" "CLASS" "AGENT" "NAME")
  "Properties in vCard 2.1/3.0 that don't exist in 4.0.
These properties are silently dropped during conversion:
- LABEL: Address labels (merged into ADR in 4.0)
- MAILER: Email program name (obsolete)
- CLASS: Access classification (obsolete)
- AGENT: Assistant representation (deprecated)
- NAME: Display name for SOURCE (obsolete)")

(defconst vcard-compat--type-value-mapping
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

(defun vcard-compat--detect-version (text)
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

(defun vcard-compat--decode-base64 (encoded-value)
  "Decode BASE64 ENCODED-VALUE.
Returns decoded string."
  (condition-case err
      (base64-decode-string encoded-value)
    (error
     (signal 'vcard-compat-encoding-error
             (list "BASE64 decode failed" (error-message-string err))))))

(defun vcard-compat--decode-quoted-printable (encoded-value &optional charset)
  "Decode QUOTED-PRINTABLE ENCODED-VALUE with optional CHARSET.
Returns decoded string.
Handles soft line breaks (=\\r\\n or =\\n) and =XX hex sequences."
  (condition-case err
      (let ((result "")
            (i 0)
            (len (length encoded-value)))
        ;; First, remove soft line breaks
        (setq encoded-value (replace-regexp-in-string "=[\r\n]+" "" encoded-value))
        (setq len (length encoded-value))

        ;; Decode =XX sequences
        (while (< i len)
          (let ((char (aref encoded-value i)))
            (if (and (= char ?=) (< (+ i 2) len))
                (let ((hex (substring encoded-value (1+ i) (+ i 3))))
                  (setq result (concat result (char-to-string (string-to-number hex 16))))
                  (setq i (+ i 3)))
              (setq result (concat result (char-to-string char)))
              (setq i (1+ i)))))

        ;; Convert from charset if specified
        (if charset
            (vcard-compat--convert-charset result charset)
          result))
    (error
     (signal 'vcard-compat-encoding-error
             (list "QUOTED-PRINTABLE decode failed" (error-message-string err))))))

(defun vcard-compat--decode-value (value encoding &optional charset)
  "Decode VALUE with ENCODING and optional CHARSET.
ENCODING can be \"BASE64\", \"QUOTED-PRINTABLE\", \"b\", \"B\", etc.
Returns decoded string."
  (when (and value encoding)
    (let ((enc (upcase encoding)))
      (cond
       ((or (string= enc "BASE64") (string= enc "B"))
        ;; Remove whitespace from base64 data
        (setq value (replace-regexp-in-string "[ \t\r\n]" "" value))
        (vcard-compat--decode-base64 value))
       ((or (string= enc "QUOTED-PRINTABLE") (string= enc "Q"))
        (vcard-compat--decode-quoted-printable value charset))
       (t value)))))

(defun vcard-compat--convert-charset (value charset)
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

(defun vcard-compat--create-data-uri (base64-data media-type)
  "Create data: URI from BASE64-DATA with MEDIA-TYPE.
Returns string like \"data:image/jpeg;base64,<data>\"."
  (format "data:%s;base64,%s" media-type base64-data))

;;; Property filtering

(defun vcard-compat--should-include-property-p (prop-name)
  "Return t if PROP-NAME should be included in conversion.
Returns nil for properties that don't exist in vCard 4.0."
  (not (member (upcase prop-name) vcard-compat--dropped-properties)))

;;; Parameter conversion

(defun vcard-compat--map-type-value (type-value)
  "Map vCard 2.1/3.0 TYPE-VALUE to vCard 4.0.
Returns mapped value or nil if it should be dropped."
  (let ((mapping (assoc (upcase type-value) vcard-compat--type-value-mapping)))
    (if mapping
        (cdr mapping)
      ;; Unknown type values pass through as lowercase
      (downcase type-value))))

(defun vcard-compat--convert-params-21 (params _prop-name)
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
          (let ((mapped (vcard-compat--map-type-value name)))
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

(defun vcard-compat--convert-params-30 (params _prop-name)
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
                 (mapped-values (delq nil (mapcar #'vcard-compat--map-type-value
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

(defun vcard-compat--parse-legacy-params (param-string _version)
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

(defun vcard-compat--parse-legacy-property (line version)
  "Parse a legacy vCard property LINE for VERSION.
VERSION is \\='v21 or \\='v30.
Returns plist with :name, :params, :value, :encoding, :charset."
  (when (string-match "^\\(?:\\([a-zA-Z0-9_-]+\\)\\.\\)?\\([^:;]+\\)\\(?:;\\([^:]*\\)\\)?:\\(.*\\)$" line)
    (let* ((group (match-string 1 line))
           (prop-name (upcase (match-string 2 line)))
           (params-string (match-string 3 line))
           (value (match-string 4 line))
           (params (vcard-compat--parse-legacy-params params-string version))
           (converted (if (eq version 'v21)
                         (vcard-compat--convert-params-21 params prop-name)
                       (vcard-compat--convert-params-30 params prop-name))))

      (list :group group
            :name prop-name
            :params (plist-get converted :params)
            :value value
            :encoding (plist-get converted :encoding)
            :charset (plist-get converted :charset)))))

;;; Media type detection

(defun vcard-compat--detect-media-type (prop-name params)
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

(defun vcard-compat--add-property-to-vcard (vc name value params group)
  "Add property to vcard object VC.
NAME is property name (uppercase string).
VALUE is property value (string or list).
PARAMS is alist of parameters.
GROUP is optional group string."
  (let ((slot (intern (downcase name))))
    (when (and (slot-exists-p vc slot)
               (not (eq slot 'version)))  ; Don't override VERSION
      (let* ((prop (vcard-property
                   :name name
                   :value value
                   :parameters params
                   :group group))
             (existing (slot-value vc slot)))

        ;; Check cardinality constraints
        (when (and (vcard--is-cardinality-one-property-p name)
                   existing)
          ;; For *1 properties, replace rather than append
          (setf (slot-value vc slot) (list prop)))

        ;; For other properties, append
        (unless (and (vcard--is-cardinality-one-property-p name)
                     existing)
          (setf (slot-value vc slot) (append existing (list prop))))))))

(defun vcard-compat--build-vcard-40 (fn properties)
  "Build vCard 4.0 object from FN and PROPERTIES list.
FN is the formatted name string (required).
PROPERTIES is list of plists with :name, :value, :params, :group.
Returns vcard object."
  (let ((vc (vcard-create :fn (or fn "Unknown"))))

    ;; Add all properties
    (dolist (prop properties)
      (let ((name (plist-get prop :name))
            (value (plist-get prop :value))
            (params (plist-get prop :params))
            (group (plist-get prop :group)))

        ;; Skip VERSION and FN (already set by vcard-create)
        (unless (member name '("VERSION" "FN"))
          (vcard-compat--add-property-to-vcard vc name value params group))))

    vc))

;;; Property value processing

(defun vcard-compat--process-property-value (name value params encoding charset _version)
  "Process property VALUE for NAME with PARAMS, ENCODING, and CHARSET.
Returns processed value (string or list)."
  (let ((processed-value value))

    ;; First decode if encoded
    (when encoding
      (setq processed-value
            (or (vcard-compat--decode-value processed-value encoding charset)
                processed-value)))

    ;; Then convert charset if specified and not encoded
    (when (and charset (not encoding))
      (setq processed-value
            (vcard-compat--convert-charset processed-value charset)))

    ;; For binary properties with base64 encoding, create data URI
    (when (and encoding
               (or (string= (upcase encoding) "BASE64")
                   (string= (upcase encoding) "B"))
               (member name '("PHOTO" "LOGO" "SOUND" "KEY")))
      (let ((media-type (vcard-compat--detect-media-type name params)))
        (when media-type
          (setq processed-value
                (vcard-compat--create-data-uri processed-value media-type)))))

    ;; Parse structured values for N, ADR, ORG, GENDER (semicolon-separated)
    (when (member name '("N" "ADR" "ORG" "GENDER"))
      (setq processed-value
            (mapcar (lambda (s)
                     (vcard--unescape-value s))
                   (split-string processed-value ";" nil))))

    ;; Parse text-list values for CATEGORIES, NICKNAME (comma-separated)
    (when (member name '("CATEGORIES" "NICKNAME"))
      (setq processed-value
            (vcard--split-text-list processed-value)))

    ;; For other properties, unescape the value
    (unless (or (listp processed-value)
                (member name '("PHOTO" "LOGO" "SOUND" "KEY")))
      (setq processed-value (vcard--unescape-value processed-value)))

    processed-value))

;;; Main conversion functions

(defun vcard-compat--properties-to-vcard-40 (properties version)
  "Convert PROPERTIES list from VERSION to vCard 4.0 object.
PROPERTIES is list of plists from vcard-compat--parse-legacy-property.
VERSION is \\='v21 or \\='v30.
Returns vcard object."
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
        (when (vcard-compat--should-include-property-p name)
          ;; Process value (decode, convert charset, etc.)
          (setq value (vcard-compat--process-property-value
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
    (vcard-compat--build-vcard-40 fn (nreverse converted-props))))

(defun vcard-compat--parse-legacy-vcard (text version)
  "Parse legacy vCard TEXT for VERSION.
VERSION is \\='v21 or \\='v30.
Returns vcard object."
  (let* ((lines (vcard--unfold-lines text))
         (properties '())
         (in-vcard nil))

    ;; Parse all properties
    (dolist (line lines)
      (cond
       ((string-match-p "^BEGIN:VCARD" line)
        (setq in-vcard t))

       ((string-match-p "^END:VCARD" line)
        (setq in-vcard nil))

       ((and in-vcard (not (string-match-p "^\\s-*$" line)))
        (when-let ((parsed (vcard-compat--parse-legacy-property line version)))
          (push parsed properties)))))

    ;; Convert to vCard 4.0
    (vcard-compat--properties-to-vcard-40 (nreverse properties) version)))

;;;###autoload
(defun vcard-compat-parse-21 (text)
  "Parse vCard 2.1 TEXT and convert to vCard 4.0 object.
Handles BASE64 and QUOTED-PRINTABLE encodings.
Converts character sets to UTF-8.
Returns vcard object."
  (vcard-compat--parse-legacy-vcard text 'v21))

;;;###autoload
(defun vcard-compat-parse-30 (text)
  "Parse vCard 3.0 TEXT and convert to vCard 4.0 object.
Handles BASE64 encoding.
Converts character sets to UTF-8.
Returns vcard object."
  (vcard-compat--parse-legacy-vcard text 'v30))

;;;###autoload
(defun vcard-compat-parse (text)
  "Parse vCard TEXT of any version (2.1, 3.0, 4.0) and convert to vCard 4.0.
Auto-detects version and applies appropriate conversion.
Returns vcard object.
Signals `vcard-compat-version-error' if version is unknown or missing."
  (let ((version (vcard-compat--detect-version text)))
    (cond
     ((eq version 'v21) (vcard-compat-parse-21 text))
     ((eq version 'v30) (vcard-compat-parse-30 text))
     ((eq version 'v40) (vcard-parse text))
     (t (signal 'vcard-compat-version-error
               '("Unknown or missing vCard VERSION"))))))

;;;###autoload
(defun vcard-compat-parse-multiple (text)
  "Parse TEXT containing one or more vCards of any version.
Auto-detects each vCard's version and converts to vCard 4.0.
Returns list of vCard 4.0 objects."
  (let ((vcards '())
        (current-vcard "")
        (in-vcard nil))

    (dolist (line (split-string text "[\r\n]+" t))
      (cond
       ((string-match-p "^BEGIN:VCARD" line)
        (setq in-vcard t)
        (setq current-vcard (concat line "\n")))

       ((string-match-p "^END:VCARD" line)
        (setq current-vcard (concat current-vcard line "\n"))
        (condition-case err
            (push (vcard-compat-parse current-vcard) vcards)
          (error
           (message "Failed to parse vCard: %s" (error-message-string err))))
        (setq in-vcard nil)
        (setq current-vcard ""))

       (in-vcard
        (setq current-vcard (concat current-vcard line "\n")))))

    (nreverse vcards)))

;;;###autoload
(defun vcard-compat-parse-buffer (&optional buffer)
  "Parse vCard data from BUFFER and convert to vCard 4.0.
BUFFER defaults to current buffer if not specified.
Auto-detects each vCard's version (2.1, 3.0, 4.0) and converts to vCard 4.0.

Returns a single vcard object if only one vCard is found.
Returns a list of vcard objects if multiple vCards are found.
Returns nil if no vCards are found.

Example:
  (with-current-buffer \"contacts.vcf\"
    (vcard-compat-parse-buffer))
  => vcard object or list of vcard objects"
  (with-current-buffer (or buffer (current-buffer))
    (let ((vcards (vcard-compat-parse-multiple (buffer-string))))
      (cond
       ((null vcards) nil)
       ((= (length vcards) 1) (car vcards))
       (t vcards)))))

;;;###autoload
(defun vcard-compat-parse-file (filename)
  "Parse vCard file FILENAME of any version and convert to vCard 4.0.
Returns vcard object or list of vcard objects.
If FILENAME contains a single vCard, returns a single vcard object.
If FILENAME contains multiple vCards, returns a list of vcard objects."
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((vcards (vcard-compat-parse-multiple (buffer-string))))
      (if (= (length vcards) 1)
          (car vcards)
        vcards))))

(provide 'vcard-compat)
;;; vcard-compat.el ends here
