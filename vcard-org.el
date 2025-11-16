;;; vcard-org.el --- Bidirectional conversion between Org-mode and vCard -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Claude Code
;; Keywords: contact, vcard, org
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (org "9.0"))

;; This file is part of emacs-vcard.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides bidirectional conversion between Org-mode contact
;; entries and vCard objects, compatible with org-contacts format.
;;
;; Features:
;; - Convert Org entries to vCard objects and vice versa
;; - Export contacts to .vcf files (buffer, region, subtree)
;; - Import vCards from files into Org buffers
;; - Configurable property mappings
;; - Support for structured properties (ORG, ADR, N)
;; - Support for parameterized properties (EMAIL_HOME, PHONE_WORK)
;; - org-contacts compatible property naming
;;
;; Usage:
;;
;; Export contacts from Org to vCard:
;;   M-x vcard-org-export-buffer  ; Export all contacts
;;   M-x vcard-org-export-region  ; Export region
;;   M-x vcard-org-export-subtree ; Export subtree
;;
;; Import vCards to Org:
;;   M-x vcard-org-import-file    ; Import from .vcf file
;;
;; Programmatic usage:
;;   (vcard-org-entry-to-vcard)         ; Convert current entry
;;   (vcard-org-buffer-to-vcards)       ; Get all contacts as vcards
;;   (vcard-org-vcard-to-entry vc)      ; Convert vcard to Org string
;;
;; Org Entry Format (org-contacts compatible):
;;
;;   * John Doe
;;   :PROPERTIES:
;;   :VCARD: t
;;   :EMAIL: john@example.com
;;   :EMAIL_WORK: john.doe@company.com
;;   :MOBILE: +1-555-123-4567
;;   :PHONE_WORK: +1-555-987-6543
;;   :ORG: Example Corporation;Engineering Department
;;   :TITLE: Senior Software Engineer
;;   :BDAY: 1990-05-15
;;   :URL: https://johndoe.example.com
;;   :NOTE: Met at conference 2024
;;   :CATEGORIES: colleague,tech,friend
;;   :END:

;;; Code:

(require 'org)
(require 'vcard)
(require 'vcard-compat)
(require 'cl-lib)
(require 'seq)

;;; Customization

(defgroup vcard-org nil
  "Integration between vCard and Org-mode."
  :group 'org
  :group 'vcard
  :prefix "vcard-org-")

(defcustom vcard-org-property-mappings
  '(("EMAIL" email nil)
    ("EMAIL_HOME" email (("TYPE" . "home")))
    ("EMAIL_WORK" email (("TYPE" . "work")))
    ("PHONE" tel (("TYPE" . "voice")))
    ("MOBILE" tel (("TYPE" . "cell")))
    ("PHONE_WORK" tel (("TYPE" . "work,voice")))
    ("PHONE_HOME" tel (("TYPE" . "home,voice")))
    ("FAX" tel (("TYPE" . "fax")))
    ("FAX_WORK" tel (("TYPE" . "work,fax")))
    ("ADDRESS_HOME" adr (("TYPE" . "home")))
    ("ADDRESS_WORK" adr (("TYPE" . "work")))
    ("ORG" org nil)
    ("TITLE" title nil)
    ("ROLE" role nil)
    ("URL" url nil)
    ("NOTE" note nil)
    ("BDAY" bday nil)
    ("NICKNAME" nickname nil)
    ("ANNIVERSARY" anniversary nil)
    ("CATEGORIES" categories nil))
  "Mapping between Org properties and vCard properties.

Each entry is a list: (ORG-PROPERTY VCARD-SLOT PARAMETERS)

ORG-PROPERTY: String name of the Org property (e.g., \"EMAIL_HOME\")
VCARD-SLOT: Symbol for the vCard property slot (e.g., email)
PARAMETERS: Alist of vCard parameters to match/set
            (e.g., ((\"TYPE\" . \"home\"))) or nil for no parameters.

Properties are processed in order, allowing multiple mappings
for the same vCard slot with different parameters."
  :type '(repeat (list (string :tag "Org Property")
                       (symbol :tag "vCard Slot")
                       (choice (const :tag "No Parameters" nil)
                               (alist :tag "Parameters"
                                      :key-type string
                                      :value-type string))))
  :group 'vcard-org)

(defcustom vcard-org-auto-mark-contacts t
  "Automatically add VCARD property when importing contacts.
When non-nil, imported contacts get :VCARD: t property to mark
them as contacts for easy identification."
  :type 'boolean
  :group 'vcard-org)

(defcustom vcard-org-export-unknown-properties nil
  "Export Org properties without vCard mappings as X-* properties.
When non-nil, unmapped Org properties become X-ORG-PROPERTY in
the exported vCard."
  :type 'boolean
  :group 'vcard-org)

(defcustom vcard-org-import-unmapped-properties t
  "Import vCard properties without Org mappings.
When non-nil, unmapped vCard properties are stored as uppercase
Org properties."
  :type 'boolean
  :group 'vcard-org)

(defcustom vcard-org-require-vcard-property t
  "Require VCARD property to identify contacts.
When non-nil, only entries with :VCARD: t are considered contacts.
When nil, entries with contact-like properties are auto-detected."
  :type 'boolean
  :group 'vcard-org)

;;; Internal Helper Functions

(defun vcard-org--is-contact-p ()
  "Return t if current Org entry is a contact.
Checks for explicit VCARD property or uses auto-detection based
on `vcard-org-require-vcard-property'."
  (if vcard-org-require-vcard-property
      (org-entry-get nil "VCARD")
    (or (org-entry-get nil "VCARD")
        (vcard-org--looks-like-contact-p))))

(defun vcard-org--looks-like-contact-p ()
  "Return t if entry has contact-like properties.
Used for auto-detection when `vcard-org-require-vcard-property' is nil."
  (let ((properties (org-entry-properties)))
    (seq-some (lambda (mapping)
                (assoc (car mapping) properties))
              vcard-org-property-mappings)))

(defun vcard-org--params-match-p (params target-params)
  "Return t if PARAMS match TARGET-PARAMS.
Both are alists of parameter name to value.
TARGET-PARAMS may be nil to match any parameters."
  (if (null target-params)
      t
    (cl-every (lambda (target-pair)
                (let ((param-name (car target-pair))
                      (target-value (cdr target-pair)))
                  (let ((actual-value (cdr (assoc param-name params))))
                    (and actual-value
                         (string= (downcase actual-value)
                                  (downcase target-value))))))
              target-params)))

(defun vcard-org--find-org-prop-for-vcard-prop (vcard-slot params)
  "Find Org property name for VCARD-SLOT with PARAMS.
Returns the first matching Org property name from mappings."
  (cl-loop for (org-prop slot target-params) in vcard-org-property-mappings
           when (and (eq slot vcard-slot)
                     (vcard-org--params-match-p params target-params))
           return org-prop))

(defun vcard-org--format-vcard-value (value vcard-slot)
  "Format VALUE for vCard property VCARD-SLOT.
Handles structured properties (lists) and text-list properties."
  (cond
   ;; List values - structured properties use semicolon
   ((and (listp value)
         (member vcard-slot '(org adr n)))
    (mapconcat #'identity value ";"))
   ;; List values - text-list properties use comma
   ((and (listp value)
         (member vcard-slot '(categories nickname)))
    (mapconcat #'identity value ","))
   ;; Simple string values
   (t (if (stringp value) value (format "%s" value)))))

(defun vcard-org--parse-org-value (value vcard-slot)
  "Parse Org property VALUE for VCARD-SLOT.
Converts strings to appropriate format (list for structured properties)."
  (cond
   ;; Structured properties (semicolon-separated)
   ((member vcard-slot '(org adr n))
    (split-string value ";" t))
   ;; Text-list properties (comma-separated)
   ((member vcard-slot '(categories nickname))
    (split-string value "," t "[ \t]+"))
   ;; Simple string
   (t value)))

(defun vcard-org--get-reverse-mappings ()
  "Return alist mapping vCard slots to lists of (org-prop params) pairs.
Used for efficient vCard to Org conversion."
  (let ((result '()))
    (dolist (mapping vcard-org-property-mappings)
      (let* ((org-prop (nth 0 mapping))
             (vcard-slot (nth 1 mapping))
             (params (nth 2 mapping))
             (existing (assq vcard-slot result)))
        (if existing
            (push (cons org-prop params) (cdr existing))
          (push (cons vcard-slot (list (cons org-prop params))) result))))
    result))

;;; Org to vCard Conversion

(defun vcard-org-entry-to-vcard ()
  "Convert current Org entry at point to vcard object.
Returns nil if entry is not a contact (no VCARD property or
contact-like properties).

The entry heading becomes the FN (formatted name) property.
Properties are mapped according to `vcard-org-property-mappings'.

Example:
  * John Doe
  :PROPERTIES:
  :VCARD: t
  :EMAIL: john@example.com
  :MOBILE: +1-555-1234
  :ORG: Example Corp;Engineering
  :END:

Returns a vcard object with FN, EMAIL, TEL, and ORG properties."
  (save-excursion
    (org-back-to-heading t)
    (when (vcard-org--is-contact-p)
      (let* ((properties (org-entry-properties))
             (heading (org-get-heading t t t t))
             (vc (vcard-create :fn heading)))

        ;; Handle N (structured name) if present
        (when-let ((n-value (cdr (assoc "N" properties))))
          (vcard-set-property vc 'n (split-string n-value ";" t)))

        ;; Map all properties according to mappings
        (dolist (mapping vcard-org-property-mappings)
          (let* ((org-prop (nth 0 mapping))
                 (vcard-slot (nth 1 mapping))
                 (params (nth 2 mapping))
                 (org-value (cdr (assoc org-prop properties))))
            (when org-value
              (let ((parsed-value (vcard-org--parse-org-value org-value vcard-slot)))
                (vcard-add-property vc vcard-slot parsed-value params)))))

        ;; Optionally export unknown properties as X-ORG-*
        (when vcard-org-export-unknown-properties
          (let ((mapped-props (mapcar #'car vcard-org-property-mappings)))
            (dolist (prop properties)
              (let ((prop-name (car prop))
                    (prop-value (cdr prop)))
                (unless (or (member prop-name mapped-props)
                           (member prop-name '("CATEGORY" "ITEM" "VCARD" "N")))
                  (vcard-add-property vc
                                     (intern (concat "x-org-" (downcase prop-name)))
                                     prop-value
                                     nil))))))

        vc))))

(defun vcard-org-buffer-to-vcards ()
  "Export all contacts in current buffer to list of vcard objects.
Only processes entries marked as contacts (see `vcard-org--is-contact-p').

Returns a list of vcard objects, one per contact entry.
Returns nil if no contacts found.

Example:
  (let ((vcards (vcard-org-buffer-to-vcards)))
    (message \"Found %d contacts\" (length vcards)))"
  (let ((vcards '()))
    (org-map-entries
     (lambda ()
       (when-let ((vc (vcard-org-entry-to-vcard)))
         (push vc vcards)))
     nil nil)
    (nreverse vcards)))

(defun vcard-org-region-to-vcards ()
  "Export contacts in active region to list of vcard objects.
Only processes entries within the region that are marked as contacts.

Signals an error if no region is active.
Returns a list of vcard objects, one per contact entry.
Returns nil if no contacts found in region."
  (unless (use-region-p)
    (user-error "No active region"))
  (let ((vcards '()))
    (org-map-entries
     (lambda ()
       (when-let ((vc (vcard-org-entry-to-vcard)))
         (push vc vcards)))
     nil 'region)
    (nreverse vcards)))

(defun vcard-org-subtree-to-vcards ()
  "Export contacts in current subtree to list of vcard objects.
Processes the current entry and all its children.

Returns a list of vcard objects, one per contact entry.
Returns nil if no contacts found in subtree."
  (let ((vcards '()))
    (org-map-entries
     (lambda ()
       (when-let ((vc (vcard-org-entry-to-vcard)))
         (push vc vcards)))
     nil 'tree)
    (nreverse vcards)))

;;; vCard to Org Conversion

(defun vcard-org-vcard-to-entry (vc &optional level)
  "Convert vcard object VC to Org entry string.
LEVEL specifies heading level (defaults to 1).

Returns a string containing a complete Org entry with properties.
The FN property becomes the heading.
All vCard properties are mapped to Org properties according to
`vcard-org-property-mappings'.

If `vcard-org-auto-mark-contacts' is non-nil, adds :VCARD: t property.

Example:
  (vcard-org-vcard-to-entry my-vcard 1)
  => \"* John Doe
  :PROPERTIES:
  :VCARD: t
  :EMAIL: john@example.com
  ...
  :END:
  \""
  (let* ((level (or level 1))
         (stars (make-string level ?*))
         (fn (or (vcard-get-property-value vc 'fn) "Unknown"))
         (properties '()))

    ;; Add VCARD marker if configured
    (when vcard-org-auto-mark-contacts
      (push (cons "VCARD" "t") properties))

    ;; Handle N (structured name) if present and different from FN
    (when-let ((n (vcard-get-property-value vc 'n)))
      (push (cons "N" (mapconcat #'identity n ";")) properties))

    ;; Build reverse mapping for efficiency
    (let ((reverse-mappings (vcard-org--get-reverse-mappings)))

      ;; Process each vCard slot
      (dolist (slot-mappings reverse-mappings)
        (let* ((vcard-slot (car slot-mappings))
               (_org-mappings (cdr slot-mappings))
               (vcard-props (ignore-errors (slot-value vc vcard-slot))))

          ;; Process each property value for this slot
          (dolist (prop vcard-props)
            (let* ((value (oref prop value))
                   (params (oref prop parameters))
                   (org-prop (vcard-org--find-org-prop-for-vcard-prop vcard-slot params)))

              (if org-prop
                  ;; Mapped property
                  (let ((formatted-value (vcard-org--format-vcard-value value vcard-slot)))
                    (push (cons org-prop formatted-value) properties))
                ;; Unmapped property
                (when vcard-org-import-unmapped-properties
                  (let* ((prop-name (upcase (symbol-name vcard-slot)))
                         (formatted-value (vcard-org--format-vcard-value value vcard-slot)))
                    (push (cons prop-name formatted-value) properties)))))))))

    ;; Format as Org entry
    (concat
     (format "%s %s\n" stars fn)
     ":PROPERTIES:\n"
     (mapconcat (lambda (p)
                  (format ":%s: %s" (car p) (cdr p)))
                (nreverse properties)
                "\n")
     "\n:END:\n")))

;;; Interactive Export Commands

;;;###autoload
(defun vcard-org-export-buffer (file)
  "Export all contacts in current buffer to FILE (.vcf).
Prompts for filename if called interactively.

Only exports entries marked as contacts (see `vcard-org--is-contact-p').
The file is created or overwritten with vCard 3.0 format data.

Returns the number of contacts exported, or nil if none found."
  (interactive "FExport contacts to vCard file: ")
  (let* ((vcards (vcard-org-buffer-to-vcards))
         (count (length vcards)))
    (if (zerop count)
        (progn
          (message "No contacts found to export")
          nil)
      (with-temp-file file
        (dolist (vc vcards)
          (insert (vcard-serialize vc))
          (insert "\n")))
      (message "Exported %d contact%s to %s"
               count (if (= count 1) "" "s") file)
      count)))

;;;###autoload
(defun vcard-org-export-region (file)
  "Export contacts in active region to FILE (.vcf).
Prompts for filename if called interactively.

Signals an error if no region is active.
Only exports entries within the region marked as contacts.

Returns the number of contacts exported, or nil if none found."
  (interactive "FExport region to vCard file: ")
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((vcards (vcard-org-region-to-vcards))
         (count (length vcards)))
    (if (zerop count)
        (progn
          (message "No contacts found in region")
          nil)
      (with-temp-file file
        (dolist (vc vcards)
          (insert (vcard-serialize vc))
          (insert "\n")))
      (message "Exported %d contact%s from region to %s"
               count (if (= count 1) "" "s") file)
      count)))

;;;###autoload
(defun vcard-org-export-subtree (file)
  "Export contacts in current subtree to FILE (.vcf).
Prompts for filename if called interactively.

Exports the current entry and all its children that are marked as contacts.

Returns the number of contacts exported, or nil if none found."
  (interactive "FExport subtree to vCard file: ")
  (let* ((vcards (vcard-org-subtree-to-vcards))
         (count (length vcards)))
    (if (zerop count)
        (progn
          (message "No contacts found in subtree")
          nil)
      (with-temp-file file
        (dolist (vc vcards)
          (insert (vcard-serialize vc))
          (insert "\n")))
      (message "Exported %d contact%s from subtree to %s"
               count (if (= count 1) "" "s") file)
      count)))

;;; Interactive Import Commands

;;;###autoload
(defun vcard-org-import-file (file &optional level)
  "Import vCards from FILE into current buffer at point.
LEVEL specifies heading level for imported entries (default 1).
Prompts for filename if called interactively.

Inserts Org entries for all vCards found in FILE.
Each vCard becomes a separate heading with properties.

If `vcard-org-auto-mark-contacts' is non-nil, imported entries
are marked with :VCARD: t property.

Returns the number of contacts imported, or nil if none found."
  (interactive "fImport vCard file: \nP")
  (let* ((level (if (numberp level) level 1))
         (vcards (vcard-compat-parse-file file))
         (count (length vcards)))
    (if (zerop count)
        (progn
          (message "No vCards found in %s" file)
          nil)
      (save-excursion
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (dolist (vc vcards)
          (insert (vcard-org-vcard-to-entry vc level))
          (insert "\n")))
      (message "Imported %d contact%s from %s"
               count (if (= count 1) "" "s") file)
      count)))

;;;###autoload
(defun vcard-org-import-buffer (&optional buffer level)
  "Import vCards from BUFFER into current buffer at point.
BUFFER defaults to the current buffer.
LEVEL specifies heading level for imported entries (default 1).

Parses vCard data from BUFFER and inserts Org entries.
Supports vCard 2.1, 3.0, and 4.0 formats via compatibility layer.
Returns the number of contacts imported, or nil if none found."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (level (or level 1))
         (vcards-raw (with-current-buffer buffer
                       (vcard-compat-parse-buffer)))
         ;; Ensure vcards is always a list
         (vcards (if (listp vcards-raw) vcards-raw (list vcards-raw)))
         (count (length vcards)))
    (if (zerop count)
        (progn
          (message "No vCards found in buffer")
          nil)
      (save-excursion
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (dolist (vc vcards)
          (insert (vcard-org-vcard-to-entry vc level))
          (insert "\n")))
      (message "Imported %d contact%s from buffer"
               count (if (= count 1) "" "s"))
      count)))

;;;###autoload
(defun vcard-org-import-region (start end &optional level)
  "Import vCards from region between START and END.
LEVEL specifies heading level for imported entries (default 1).

Parses vCard data from the region and inserts Org entries at point.
Supports vCard 2.1, 3.0, and 4.0 formats via compatibility layer.
Returns the number of contacts imported, or nil if none found."
  (interactive "r\nP")
  (let* ((level (if (numberp level) level 1))
         (vcard-text (buffer-substring-no-properties start end))
         (vcards-raw (with-temp-buffer
                       (insert vcard-text)
                       (goto-char (point-min))
                       (vcard-compat-parse-buffer)))
         ;; Ensure vcards is always a list
         (vcards (if (listp vcards-raw) vcards-raw (list vcards-raw)))
         (count (length vcards)))
    (if (zerop count)
        (progn
          (message "No vCards found in region")
          nil)
      (save-excursion
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (dolist (vc vcards)
          (insert (vcard-org-vcard-to-entry vc level))
          (insert "\n")))
      (message "Imported %d contact%s from region"
               count (if (= count 1) "" "s"))
      count)))

;;; Utility Functions

(defun vcard-org-validate-entry ()
  "Validate current Org entry as a vCard contact.
Checks that required properties are present and well-formed.
Returns t if valid, nil otherwise.
Displays warnings for any issues found."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((issues '())
          (fn (org-get-heading t t t t)))

      ;; Check if marked as contact
      (unless (vcard-org--is-contact-p)
        (push "Entry not marked as contact (no VCARD property)" issues))

      ;; FN is required
      (when (or (null fn) (string-empty-p fn))
        (push "Missing or empty heading (FN required)" issues))

      ;; Check structured properties format
      (when-let ((org-val (org-entry-get nil "ORG")))
        (unless (string-match-p ";" org-val)
          (push "ORG property should contain semicolon-separated components" issues)))

      (when-let ((adr-home (org-entry-get nil "ADDRESS_HOME")))
        (unless (string-match-p ";" adr-home)
          (push "ADDRESS_HOME should contain semicolon-separated components" issues)))

      (when-let ((adr-work (org-entry-get nil "ADDRESS_WORK")))
        (unless (string-match-p ";" adr-work)
          (push "ADDRESS_WORK should contain semicolon-separated components" issues)))

      ;; Report issues
      (if issues
          (progn
            (message "Validation issues:\n%s"
                     (mapconcat #'identity issues "\n"))
            nil)
        (message "Entry is valid")
        t))))

(defun vcard-org-count-contacts ()
  "Count number of contacts in current buffer.
Returns the count and displays it in the message area."
  (interactive)
  (let ((count 0))
    (org-map-entries
     (lambda ()
       (when (vcard-org--is-contact-p)
         (setq count (1+ count))))
     nil nil)
    (message "Found %d contact%s" count (if (= count 1) "" "s"))
    count))

(provide 'vcard-org)
;;; vcard-org.el ends here
