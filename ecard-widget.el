;;; ecard-widget.el --- Widget-based editing for vCards -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: comm, carddav, contacts
;; URL: https://github.com/jwiegley/dot-emacs

;;; Commentary:

;; Provides a widget-based interface for viewing and editing vCard (ecard)
;; data in Emacs.  The interface is designed to closely match the Emacs
;; customize interface.
;;
;; Main entry points:
;;   `ecard-widget-create' - Create widget form for an ecard in current buffer
;;   `ecard-widget-get-value' - Extract modified ecard from widget form

;;; Code:

(require 'widget)
(require 'wid-edit)
(require 'cus-edit)
(require 'ecard)
(require 'ecard-compat)
(require 'cl-lib)

;;; Custom group

(defgroup ecard-widget nil
  "Widget-based editing for vCards."
  :group 'ecard
  :prefix "ecard-widget-")

;;; Buffer-local variables

(defvar-local ecard-widget--widgets nil
  "Alist mapping property names to their widget(s).")

(defvar-local ecard-widget--ecard nil
  "The ecard object being edited.")

(defvar-local ecard-widget--notify-fn nil
  "Function to call when widget values change.")

(defvar-local ecard-widget--section-visibility nil
  "Alist mapping section names to visibility state.")

;;; Helper functions

(defun ecard-widget--parse-type-value (type-val valid-types)
  "Parse TYPE-VAL and return first matching type from VALID-TYPES.
TYPE-VAL can be a single type, comma-separated types (\"home,pref\"),
or a list of types.  Returns the first type that matches VALID-TYPES,
or \"other\" if none match."
  (let ((types (cond
                ((null type-val) nil)
                ((listp type-val) type-val)
                ((stringp type-val)
                 ;; Split on comma and normalize
                 (mapcar #'string-trim (split-string type-val ",")))
                (t nil))))
    ;; Find first matching valid type (case-insensitive)
    (or (cl-find-if (lambda (t)
                      (cl-member t valid-types :test #'string-equal-ignore-case))
                    types)
        "other")))

(defun ecard-widget--get-type-param (prop valid-types)
  "Get the TYPE parameter value from ecard-property PROP.
VALID-TYPES is a list of valid type strings for this property kind.
Returns a type from VALID-TYPES, or \"other\" if no match."
  (if prop
      (let* ((params (ecard-property-parameters prop))
             (type-val (or (cdr (assoc "TYPE" params))
                           (cdr (assoc "type" params)))))
        (ecard-widget--parse-type-value type-val valid-types))
    "other"))

(defun ecard-widget--make-type-param (type)
  "Create a parameters alist with TYPE."
  (when (and type (not (string-empty-p type)))
    (list (cons "TYPE" type))))

(defun ecard-widget--safe-value (val)
  "Safely extract a string value from VAL."
  (cond
   ((null val) "")
   ((stringp val) val)
   ((and (listp val) (stringp (car val))) (car val))
   ((listp val) "")
   (t (format "%s" val))))

(defun ecard-widget--notify-change (&rest _)
  "Called when any widget value changes."
  (when ecard-widget--notify-fn
    (funcall ecard-widget--notify-fn)))

;;; Customize-style rendering helpers

(defun ecard-widget--indent (level)
  "Insert indentation for nesting LEVEL."
  (widget-insert (make-string (* level 2) ?\s)))

(defun ecard-widget--insert-group-header (name &optional level)
  "Insert a group header for NAME at indent LEVEL."
  (let ((lvl (or level 0)))
    (ecard-widget--indent lvl)
    (widget-insert (propertize name 'face 'custom-group-tag))
    (widget-insert ":\n")))

(defun ecard-widget--insert-field-label (name &optional level)
  "Insert a field label for NAME at indent LEVEL."
  (let ((lvl (or level 1)))
    (ecard-widget--indent lvl)
    (widget-insert (propertize name 'face 'custom-variable-tag))
    (widget-insert ": ")))

;;; Widget creation functions

(defun ecard-widget--create-field (label value &optional level size)
  "Create an editable field with LABEL and VALUE at indent LEVEL.
Optional SIZE specifies field width."
  (ecard-widget--insert-field-label label level)
  (let ((w (widget-create 'editable-field
                          :size (or size 40)
                          :notify #'ecard-widget--notify-change
                          :value (or value ""))))
    (widget-insert "\n")
    w))

(defun ecard-widget--create-text-area (label value &optional level)
  "Create a multiline text area with LABEL and VALUE at indent LEVEL."
  (ecard-widget--insert-field-label label level)
  (widget-insert "\n")
  (ecard-widget--indent (1+ (or level 1)))
  (let ((w (widget-create 'text
                          :format "%v"
                          :notify #'ecard-widget--notify-change
                          :value (or value ""))))
    (widget-insert "\n")
    w))

(defun ecard-widget--create-readonly (label value &optional level)
  "Create a read-only display for LABEL and VALUE at indent LEVEL."
  (ecard-widget--insert-field-label label level)
  (widget-insert (propertize (or value "") 'face 'custom-state))
  (widget-insert "\n"))

(defun ecard-widget--create-choice (label value choices &optional level)
  "Create a choice menu for LABEL with VALUE from CHOICES at indent LEVEL."
  (ecard-widget--insert-field-label label level)
  (let* ((choice-args (mapcar (lambda (c)
                                `(const :tag ,c ,c))
                              choices))
         (w (apply #'widget-create 'menu-choice
                   :tag label
                   :format "%[%v%]"
                   :notify #'ecard-widget--notify-change
                   :value (or value (car choices))
                   choice-args)))
    w))

;;; Section: Name

(defun ecard-widget--create-name-section (n-prop)
  "Create widgets for structured name from N-PROP.
Returns alist of widgets."
  (let* ((n-val (when n-prop (ecard-property-value n-prop)))
         (family (ecard-widget--safe-value (nth 0 n-val)))
         (given (ecard-widget--safe-value (nth 1 n-val)))
         (additional (ecard-widget--safe-value (nth 2 n-val)))
         (prefix (ecard-widget--safe-value (nth 3 n-val)))
         (suffix (ecard-widget--safe-value (nth 4 n-val)))
         widgets)
    (ecard-widget--insert-group-header "Name Components" 0)
    (push (cons 'n-given (ecard-widget--create-field "Given Name" given 1 30))
          widgets)
    (push (cons 'n-family (ecard-widget--create-field "Family Name" family 1 30))
          widgets)
    (push (cons 'n-additional (ecard-widget--create-field "Additional" additional 1 30))
          widgets)
    (push (cons 'n-prefix (ecard-widget--create-field "Prefix" prefix 1 15))
          widgets)
    (push (cons 'n-suffix (ecard-widget--create-field "Suffix" suffix 1 15))
          widgets)
    widgets))

;;; Section: Multi-value entries (Email, Tel, Address)

(defun ecard-widget--create-multi-entry (idx type-choices type-val value-val value-size prefix)
  "Create a single multi-value entry.
IDX is the entry index, TYPE-CHOICES are type options,
TYPE-VAL and VALUE-VAL are current values, VALUE-SIZE is field width,
PREFIX is the widget name prefix (e.g., \"email\")."
  (let (widgets)
    (ecard-widget--indent 1)
    ;; Entry number indicator
    (widget-insert (propertize (format "%d. " (1+ idx)) 'face 'custom-state))
    ;; Type choice
    (let* ((choice-args (mapcar (lambda (c) `(const :tag ,c ,c)) type-choices))
           (type-w (apply #'widget-create 'menu-choice
                          :tag "Type"
                          :format "%[%v%]"
                          :notify #'ecard-widget--notify-change
                          :value (or type-val (car type-choices))
                          choice-args)))
      (push (cons (intern (format "%s-%d-type" prefix idx)) type-w) widgets))
    (widget-insert " ")
    ;; Value field
    (let ((val-w (widget-create 'editable-field
                                :size value-size
                                :notify #'ecard-widget--notify-change
                                :value (or value-val ""))))
      (push (cons (intern (format "%s-%d-value" prefix idx)) val-w) widgets))
    (widget-insert "\n")
    widgets))

(defun ecard-widget--create-email-section (email-props)
  "Create widgets for email list from EMAIL-PROPS."
  (ecard-widget--insert-group-header "Email Addresses" 0)
  (let ((widgets nil)
        (idx 0)
        (types '("work" "home" "other")))
    (if email-props
        (dolist (prop email-props)
          (let* ((value (ecard-widget--safe-value (ecard-property-value prop)))
                 (type (ecard-widget--get-type-param prop types)))
            (setq widgets (append (ecard-widget--create-multi-entry
                                   idx types type value 35 "email")
                                  widgets)))
          (setq idx (1+ idx)))
      ;; One empty entry
      (setq widgets (ecard-widget--create-multi-entry 0 types "work" "" 35 "email"))
      (setq idx 1))
    (push (cons 'email-count idx) widgets)
    widgets))

(defun ecard-widget--create-tel-section (tel-props)
  "Create widgets for telephone list from TEL-PROPS."
  (ecard-widget--insert-group-header "Phone Numbers" 0)
  (let ((widgets nil)
        (idx 0)
        (types '("work" "home" "cell" "fax" "other")))
    (if tel-props
        (dolist (prop tel-props)
          (let* ((value (ecard-widget--safe-value (ecard-property-value prop)))
                 (type (ecard-widget--get-type-param prop types)))
            (setq widgets (append (ecard-widget--create-multi-entry
                                   idx types type value 20 "tel")
                                  widgets)))
          (setq idx (1+ idx)))
      ;; One empty entry
      (setq widgets (ecard-widget--create-multi-entry 0 types "work" "" 20 "tel"))
      (setq idx 1))
    (push (cons 'tel-count idx) widgets)
    widgets))

;;; Section: Addresses

(defun ecard-widget--create-address-entry (adr-prop idx)
  "Create widgets for a single address from ADR-PROP at index IDX."
  (let* ((adr-val (when adr-prop (ecard-property-value adr-prop)))
         (adr-types '("work" "home" "other"))
         (type (ecard-widget--get-type-param adr-prop adr-types))
         (street (ecard-widget--safe-value (nth 2 adr-val)))
         (city (ecard-widget--safe-value (nth 3 adr-val)))
         (region (ecard-widget--safe-value (nth 4 adr-val)))
         (postal (ecard-widget--safe-value (nth 5 adr-val)))
         (country (ecard-widget--safe-value (nth 6 adr-val)))
         widgets)
    (ecard-widget--indent 1)
    (widget-insert (propertize (format "Address %d" (1+ idx)) 'face 'custom-variable-tag))
    (widget-insert " ")
    ;; Type
    (let ((type-w (widget-create 'menu-choice
                                 :format "%[%v%]"
                                 :notify #'ecard-widget--notify-change
                                 :value type
                                 '(const :tag "work" "work")
                                 '(const :tag "home" "home")
                                 '(const :tag "other" "other"))))
      (push (cons (intern (format "adr-%d-type" idx)) type-w) widgets))
    (widget-insert "\n")
    ;; Address fields
    (push (cons (intern (format "adr-%d-street" idx))
                (ecard-widget--create-field "Street" street 2 40))
          widgets)
    (push (cons (intern (format "adr-%d-city" idx))
                (ecard-widget--create-field "City" city 2 25))
          widgets)
    (push (cons (intern (format "adr-%d-region" idx))
                (ecard-widget--create-field "State/Region" region 2 20))
          widgets)
    (push (cons (intern (format "adr-%d-postal" idx))
                (ecard-widget--create-field "Postal Code" postal 2 15))
          widgets)
    (push (cons (intern (format "adr-%d-country" idx))
                (ecard-widget--create-field "Country" country 2 25))
          widgets)
    widgets))

(defun ecard-widget--create-adr-section (adr-props)
  "Create widgets for address list from ADR-PROPS."
  (ecard-widget--insert-group-header "Addresses" 0)
  (let ((widgets nil)
        (idx 0))
    (if adr-props
        (dolist (prop adr-props)
          (setq widgets (append (ecard-widget--create-address-entry prop idx) widgets))
          (setq idx (1+ idx)))
      ;; One empty entry
      (setq widgets (ecard-widget--create-address-entry nil 0))
      (setq idx 1))
    (push (cons 'adr-count idx) widgets)
    widgets))

;;; Section: Organization

(defun ecard-widget--create-org-section (ecard-obj)
  "Create widgets for organization info from ECARD-OBJ."
  (let ((org-props (ecard-org ecard-obj))
        (title-props (ecard-title ecard-obj))
        (role-props (ecard-role ecard-obj))
        widgets)
    (ecard-widget--insert-group-header "Organization" 0)
    (let* ((org-val (when org-props
                      (ecard-widget--safe-value (ecard-property-value (car org-props)))))
           (title-val (when title-props
                        (ecard-widget--safe-value (ecard-property-value (car title-props)))))
           (role-val (when role-props
                       (ecard-widget--safe-value (ecard-property-value (car role-props))))))
      (push (cons 'org (ecard-widget--create-field "Organization" org-val 1 40))
            widgets)
      (push (cons 'title (ecard-widget--create-field "Title" title-val 1 35))
            widgets)
      (push (cons 'role (ecard-widget--create-field "Role" role-val 1 35))
            widgets))
    widgets))

;;; Section: URLs

(defun ecard-widget--create-url-section (ecard-obj)
  "Create widgets for URLs from ECARD-OBJ."
  (let ((url-props (ecard-url ecard-obj))
        widgets)
    (ecard-widget--insert-group-header "URLs" 0)
    (let ((idx 0))
      (if url-props
          (dolist (prop url-props)
            (let ((val (ecard-widget--safe-value (ecard-property-value prop))))
              (push (cons (intern (format "url-%d" idx))
                          (ecard-widget--create-field (format "URL %d" (1+ idx)) val 1 50))
                    widgets))
            (setq idx (1+ idx)))
        ;; One empty entry
        (push (cons 'url-0 (ecard-widget--create-field "URL" "" 1 50))
              widgets)
        (setq idx 1))
      (push (cons 'url-count idx) widgets))
    widgets))

;;; Section: Notes

(defun ecard-widget--create-notes-section (ecard-obj)
  "Create widgets for notes from ECARD-OBJ."
  (let ((note-props (ecard-note ecard-obj))
        widgets)
    (ecard-widget--insert-group-header "Notes" 0)
    (let ((note-val (when note-props
                      (ecard-widget--safe-value (ecard-property-value (car note-props))))))
      (push (cons 'note (ecard-widget--create-text-area "Note" note-val 1))
            widgets))
    widgets))

;;; Main entry point

;;;###autoload
(defun ecard-widget-create (ecard-obj &optional notify-fn)
  "Create widget form for ECARD-OBJ in current buffer.
Optional NOTIFY-FN is called when any widget value changes.
Returns alist of widgets created."
  (setq ecard-widget--ecard ecard-obj
        ecard-widget--notify-fn notify-fn
        ecard-widget--widgets nil)
  (let ((inhibit-read-only t))
    ;; Disable after-change-functions during cleanup to prevent "Overlapping fields"
    (let ((after-change-functions nil))
      (erase-buffer)
      (remove-overlays))

    ;; Header
    (widget-insert (propertize "Contact" 'face 'custom-group-tag))
    (widget-insert ":\n\n")

    ;; UID (read-only if present)
    (let ((uids (ecard-uid ecard-obj)))
      (when uids
        (ecard-widget--create-readonly
         "UID" (ecard-widget--safe-value (ecard-property-value (car uids))) 0)))

    ;; Full Name (FN) - required
    (let* ((fns (ecard-fn ecard-obj))
           (fn-val (when fns (ecard-property-value (car fns))))
           (fn-str (ecard-widget--safe-value fn-val)))
      (push (cons 'fn (ecard-widget--create-field "Full Name" fn-str 0 45))
            ecard-widget--widgets))

    (widget-insert "\n")

    ;; Structured Name (N)
    (let ((name-widgets (ecard-widget--create-name-section (car (ecard-n ecard-obj)))))
      (setq ecard-widget--widgets (append name-widgets ecard-widget--widgets)))

    (widget-insert "\n")

    ;; Email
    (let ((email-widgets (ecard-widget--create-email-section (ecard-email ecard-obj))))
      (setq ecard-widget--widgets (append email-widgets ecard-widget--widgets)))

    (widget-insert "\n")

    ;; Telephone
    (let ((tel-widgets (ecard-widget--create-tel-section (ecard-tel ecard-obj))))
      (setq ecard-widget--widgets (append tel-widgets ecard-widget--widgets)))

    (widget-insert "\n")

    ;; Addresses
    (let ((adr-widgets (ecard-widget--create-adr-section (ecard-adr ecard-obj))))
      (setq ecard-widget--widgets (append adr-widgets ecard-widget--widgets)))

    (widget-insert "\n")

    ;; Organization
    (let ((org-widgets (ecard-widget--create-org-section ecard-obj)))
      (setq ecard-widget--widgets (append org-widgets ecard-widget--widgets)))

    (widget-insert "\n")

    ;; URLs
    (let ((url-widgets (ecard-widget--create-url-section ecard-obj)))
      (setq ecard-widget--widgets (append url-widgets ecard-widget--widgets)))

    (widget-insert "\n")

    ;; Notes
    (let ((notes-widgets (ecard-widget--create-notes-section ecard-obj)))
      (setq ecard-widget--widgets (append notes-widgets ecard-widget--widgets)))

    (widget-insert "\n")

    ;; Action buttons in customize style
    (widget-insert "\n")
    (widget-create 'push-button
                   :tag " Save "
                   :help-echo "Save changes to server"
                   :action (lambda (&rest _)
                             (call-interactively #'ecard-display-contact-save)))
    (widget-insert " ")
    (widget-create 'push-button
                   :tag " Revert "
                   :help-echo "Discard changes"
                   :action (lambda (&rest _)
                             (call-interactively #'ecard-display-contact-revert)))
    (widget-insert " ")
    (widget-create 'push-button
                   :tag " Close "
                   :help-echo "Close this buffer"
                   :action (lambda (&rest _)
                             (call-interactively #'ecard-display-contact-quit)))
    (widget-insert "\n")

    (widget-setup)
    (goto-char (point-min)))
  ecard-widget--widgets)

;;; Value extraction

(defun ecard-widget--get-widget-value (key)
  "Get the value of widget identified by KEY."
  (let ((widget (cdr (assq key ecard-widget--widgets))))
    (when (and widget (widgetp widget))
      (widget-value widget))))

(defun ecard-widget--extract-emails ()
  "Extract email properties from widgets."
  (let ((count (or (cdr (assq 'email-count ecard-widget--widgets)) 0))
        (emails nil))
    (dotimes (i count)
      (let ((type (ecard-widget--get-widget-value (intern (format "email-%d-type" i))))
            (value (ecard-widget--get-widget-value (intern (format "email-%d-value" i)))))
        (when (and value (not (string-empty-p value)))
          (push (ecard-property :name "EMAIL"
                                :value value
                                :parameters (ecard-widget--make-type-param type))
                emails))))
    (nreverse emails)))

(defun ecard-widget--extract-tels ()
  "Extract telephone properties from widgets."
  (let ((count (or (cdr (assq 'tel-count ecard-widget--widgets)) 0))
        (tels nil))
    (dotimes (i count)
      (let ((type (ecard-widget--get-widget-value (intern (format "tel-%d-type" i))))
            (value (ecard-widget--get-widget-value (intern (format "tel-%d-value" i)))))
        (when (and value (not (string-empty-p value)))
          (push (ecard-property :name "TEL"
                                :value value
                                :parameters (ecard-widget--make-type-param type))
                tels))))
    (nreverse tels)))

(defun ecard-widget--extract-addresses ()
  "Extract address properties from widgets."
  (let ((count (or (cdr (assq 'adr-count ecard-widget--widgets)) 0))
        (addresses nil))
    (dotimes (i count)
      (let ((type (ecard-widget--get-widget-value (intern (format "adr-%d-type" i))))
            (street (or (ecard-widget--get-widget-value (intern (format "adr-%d-street" i))) ""))
            (city (or (ecard-widget--get-widget-value (intern (format "adr-%d-city" i))) ""))
            (region (or (ecard-widget--get-widget-value (intern (format "adr-%d-region" i))) ""))
            (postal (or (ecard-widget--get-widget-value (intern (format "adr-%d-postal" i))) ""))
            (country (or (ecard-widget--get-widget-value (intern (format "adr-%d-country" i))) "")))
        ;; Only add if at least one field is non-empty
        (when (cl-some (lambda (s) (not (string-empty-p s)))
                       (list street city region postal country))
          (push (ecard-property :name "ADR"
                                :value (list "" "" street city region postal country)
                                :parameters (ecard-widget--make-type-param type))
                addresses))))
    (nreverse addresses)))

(defun ecard-widget--extract-urls ()
  "Extract URL properties from widgets."
  (let ((count (or (cdr (assq 'url-count ecard-widget--widgets)) 0))
        (urls nil))
    (dotimes (i count)
      (let ((value (ecard-widget--get-widget-value (intern (format "url-%d" i)))))
        (when (and value (not (string-empty-p value)))
          (push (ecard-property :name "URL" :value value) urls))))
    (nreverse urls)))

;;;###autoload
(defun ecard-widget-get-value ()
  "Extract ecard object from current widget form.
Returns a new ecard object with values from the widgets."
  (unless ecard-widget--ecard
    (error "No ecard associated with this buffer"))

  ;; Start with a copy of the original ecard to preserve properties we don't edit
  ;; Try strict parser first, fall back to lenient parser with suppressed warnings
  (let* ((text (ecard-serialize ecard-widget--ecard))
         (new-ecard (condition-case nil
                        (ecard-parse text)
                      (ecard-parse-error
                       (let ((inhibit-message t))
                         (ecard-compat-parse text))))))

    ;; FN (required)
    (let ((fn-val (ecard-widget--get-widget-value 'fn)))
      (when (and fn-val (not (string-empty-p fn-val)))
        (setf (ecard-fn new-ecard)
              (list (ecard-property :name "FN" :value fn-val)))))

    ;; Structured name (N) - only set if at least one component is non-empty
    (let ((given (or (ecard-widget--get-widget-value 'n-given) ""))
          (family (or (ecard-widget--get-widget-value 'n-family) ""))
          (additional (or (ecard-widget--get-widget-value 'n-additional) ""))
          (prefix (or (ecard-widget--get-widget-value 'n-prefix) ""))
          (suffix (or (ecard-widget--get-widget-value 'n-suffix) "")))
      (if (cl-some (lambda (s) (not (string-empty-p s)))
                   (list family given additional prefix suffix))
          (setf (ecard-n new-ecard)
                (list (ecard-property :name "N"
                                      :value (list family given additional prefix suffix))))
        ;; Keep original N if all widget fields are empty
        nil))

    ;; Emails
    (setf (ecard-email new-ecard) (ecard-widget--extract-emails))

    ;; Telephones
    (setf (ecard-tel new-ecard) (ecard-widget--extract-tels))

    ;; Addresses
    (setf (ecard-adr new-ecard) (ecard-widget--extract-addresses))

    ;; Organization
    (let ((org-val (ecard-widget--get-widget-value 'org)))
      (if (and org-val (not (string-empty-p org-val)))
          (setf (ecard-org new-ecard)
                (list (ecard-property :name "ORG" :value org-val)))
        (setf (ecard-org new-ecard) nil)))

    (let ((title-val (ecard-widget--get-widget-value 'title)))
      (if (and title-val (not (string-empty-p title-val)))
          (setf (ecard-title new-ecard)
                (list (ecard-property :name "TITLE" :value title-val)))
        (setf (ecard-title new-ecard) nil)))

    (let ((role-val (ecard-widget--get-widget-value 'role)))
      (if (and role-val (not (string-empty-p role-val)))
          (setf (ecard-role new-ecard)
                (list (ecard-property :name "ROLE" :value role-val)))
        (setf (ecard-role new-ecard) nil)))

    ;; URLs
    (setf (ecard-url new-ecard) (ecard-widget--extract-urls))

    ;; Notes
    (let ((note-val (ecard-widget--get-widget-value 'note)))
      (if (and note-val (not (string-empty-p note-val)))
          (setf (ecard-note new-ecard)
                (list (ecard-property :name "NOTE" :value note-val)))
        (setf (ecard-note new-ecard) nil)))

    new-ecard))

;;;###autoload
(defun ecard-widget-modified-p ()
  "Return non-nil if the widget form has been modified from the original."
  (when ecard-widget--ecard
    (let ((current (ecard-widget-get-value))
          (original ecard-widget--ecard))
      (not (equal (ecard-serialize current)
                  (ecard-serialize original))))))

(provide 'ecard-widget)
;;; ecard-widget.el ends here
