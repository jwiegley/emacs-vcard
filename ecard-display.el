;;; ecard-display.el --- CardDAV browsing interface -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: comm, carddav, contacts
;; URL: https://github.com/jwiegley/dot-emacs

;;; Commentary:

;; Interactive interface for browsing and managing CardDAV servers and contacts.
;;
;; This module provides a complete hierarchical UI for working with CardDAV:
;; - Server list buffer (*CardDAV Servers*)
;; - Addressbook list buffer (*CardDAV: <server>*)
;; - Contact list buffer (*CardDAV Contacts: <addressbook>*)
;; - Contact detail buffer (*CardDAV Contact: <name>*)
;;
;; Entry point:
;;   M-x ecard-display
;;
;; Server configuration:
;;   (setq ecard-display-servers
;;         '((:name "FastMail"
;;            :url "https://carddav.fastmail.com"
;;            :username "user@fastmail.com"
;;            :password "secret")))

;;; Code:

(require 'ecard)
(require 'ecard-carddav)
(require 'ecard-carddav-auth)
(require 'tabulated-list)
(require 'widget)
(require 'wid-edit)

;;; Custom group

(defgroup ecard-display nil
  "Interactive CardDAV browsing interface."
  :group 'ecard
  :prefix "ecard-display-")

(defcustom ecard-display-servers nil
  "List of CardDAV server configurations.

Each element can be either:

1. A plist with keys:
   :name STRING - Display name for the server
   :url STRING - Base URL of CardDAV server
   :username STRING - Username for authentication
   :password STRING - Password for authentication

2. An ecard-carddav-server object (created with ecard-carddav-server-create)

Both formats are supported and can be mixed in the same list.

Examples:

  ;; Plist format (simple)
  (setq ecard-display-servers
        \\='((:name \"FastMail\"
           :url \"https://carddav.fastmail.com\"
           :username \"user@fastmail.com\"
           :password \"secret\")))

  ;; Server object format (flexible)
  (setq ecard-display-servers
        (list (ecard-carddav-server-create
               :url \"https://carddav.fastmail.com\"
               :auth (ecard-carddav-auth-basic-create
                      :username \"user@fastmail.com\"
                      :password \"secret\"))))

  ;; Mixed format
  (setq ecard-display-servers
        (list \\='(:name \"FastMail\" :url \"...\" :username \"...\" :password \"...\")
              (ecard-carddav-server-create :url \"...\" :auth ...)))"
  :type '(repeat (choice
                  (plist :tag "Plist Configuration"
                         :key-type (choice (const :name)
                                           (const :url)
                                           (const :username)
                                           (const :password))
                         :value-type string)
                  (object :tag "Server Object" ecard-carddav-server)))
  :group 'ecard-display)

(defcustom ecard-display-confirm-delete t
  "Whether to confirm before deleting servers, addressbooks, or contacts."
  :type 'boolean
  :group 'ecard-display)

(defcustom ecard-display-contacts-page-size 100
  "Number of contacts to fetch and display per page.
Smaller values provide faster initial display for large addressbooks,
while larger values reduce the number of server requests needed.
Setting to nil disables pagination and loads all contacts at once."
  :type '(choice (integer :tag "Contacts per page")
                 (const :tag "Load all contacts at once" nil))
  :group 'ecard-display)

(defcustom ecard-display-fetch-names-immediately t
  "Whether to fetch real contact names immediately when opening contacts buffer.

If t (default), fetches real names using multiget before displaying, showing
accurate names immediately (may be slow for large addressbooks).

If nil, displays filename-based names instantly for immediate navigation,
allowing users to browse the contact list without delay. Real names can be
fetched later with `ecard-display-contacts-refresh-names'."
  :type 'boolean
  :group 'ecard-display)

;;; Buffer-local state variables

(defvar-local ecard-display--server nil
  "Current CardDAV server object.")

(defvar-local ecard-display--addressbook nil
  "Current CardDAV addressbook object.")

(defvar-local ecard-display--resource nil
  "Current CardDAV resource object.")

(defvar-local ecard-display--original-ecard nil
  "Original ecard object before editing.")

(defvar-local ecard-display--modified nil
  "Whether the current contact has been modified.")

(defvar-local ecard-display--current-page 0
  "Current page number for contact list pagination (0-indexed).")

(defvar-local ecard-display--total-contacts 0
  "Total number of contacts in current addressbook.")

;;; Server Buffer

(defvar ecard-display-servers-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'ecard-display-servers-open)
    (define-key map (kbd "a") #'ecard-display-servers-add)
    (define-key map (kbd "d") #'ecard-display-servers-delete)
    (define-key map (kbd "r") #'ecard-display-servers-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `ecard-display-servers-mode'.")

(define-derived-mode ecard-display-servers-mode tabulated-list-mode "CardDAV Servers"
  "Major mode for browsing CardDAV servers.

\\{ecard-display-servers-mode-map}"
  (setq tabulated-list-format [("Name" 30 t)
                                ("URL" 50 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header))

;;;###autoload
(defun ecard-display ()
  "Display CardDAV servers in a browsable interface."
  (interactive)
  (let ((buffer (get-buffer-create "*CardDAV Servers*")))
    (with-current-buffer buffer
      (ecard-display-servers-mode)
      (ecard-display-servers-refresh))
    (switch-to-buffer buffer)))

(defun ecard-display-refresh ()
  "Refresh the server list."
  (interactive)
  (let ((entries nil))
    (dolist (config ecard-display-servers)
      ;; Normalize config to handle both plists and server objects
      (let* ((normalized (ecard-display--normalize-server-config config))
             (name (plist-get normalized :name))
             (url (plist-get normalized :url)))
        (push (list config  ; Keep original config as ID
                    (vector name url))
              entries)))
    (setq tabulated-list-entries (nreverse entries))
    (tabulated-list-print t)))

(defalias 'ecard-display-servers-refresh 'ecard-display-refresh
  "Alias for `ecard-display-refresh'.")

(defun ecard-display-servers-open ()
  "Open addressbooks for the server at point."
  (interactive)
  (let ((config (tabulated-list-get-id)))
    (unless config
      (user-error "No server at point"))
    ;; Normalize config to handle both formats
    (let* ((normalized (ecard-display--normalize-server-config config))
           (name (plist-get normalized :name))
           (server (ecard-display--server-config-to-object config)))
      (message "Connecting to %s..." name)
      (condition-case err
          (progn
            (message "Discovering addressbooks...")
            (ecard-carddav-discover-addressbooks server)
            (ecard-display-addressbooks server normalized))
        (error
         (message "Failed to connect to server: %s" (error-message-string err)))))))

(defun ecard-display-servers-add ()
  "Add a new CardDAV server."
  (interactive)
  (let* ((name (read-string "Server name: "))
         (url (read-string "Server URL: "))
         (username (read-string "Username: "))
         (password (read-passwd "Password: "))
         (config (list :name name
                       :url url
                       :username username
                       :password password)))
    (customize-save-variable 'ecard-display-servers
                             (append ecard-display-servers (list config)))
    (ecard-display-servers-refresh)
    (message "Added server: %s" name)))

(defun ecard-display-servers-delete ()
  "Delete the server at point."
  (interactive)
  (let ((config (tabulated-list-get-id)))
    (unless config
      (user-error "No server at point"))
    (when (or (not ecard-display-confirm-delete)
              (yes-or-no-p (format "Delete server '%s'? "
                                   (plist-get config :name))))
      (customize-save-variable 'ecard-display-servers
                               (cl-remove config ecard-display-servers :test #'equal))
      (ecard-display-servers-refresh)
      (message "Deleted server"))))

;;; Addressbook Buffer

(defvar ecard-display-addressbooks-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'ecard-display-addressbooks-open)
    (define-key map (kbd "r") #'ecard-display-addressbooks-refresh)
    (define-key map (kbd "^") #'ecard-display-addressbooks-back)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `ecard-display-addressbooks-mode'.")

(define-derived-mode ecard-display-addressbooks-mode tabulated-list-mode "CardDAV Addressbooks"
  "Major mode for browsing addressbooks on a CardDAV server.

\\{ecard-display-addressbooks-mode-map}"
  (setq tabulated-list-format [("Name" 30 t)
                                ("Description" 40 t)
                                ("Contacts" 10 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header))

(defun ecard-display-addressbooks (server config)
  "Display addressbooks for SERVER with CONFIG."
  (let* ((name (plist-get config :name))
         (buffer (get-buffer-create (format "*CardDAV: %s*" name))))
    (with-current-buffer buffer
      (ecard-display-addressbooks-mode)
      (setq ecard-display--server server)
      (ecard-display-addressbooks--populate server))
    (switch-to-buffer buffer)))

(defun ecard-display-addressbooks--populate (server)
  "Populate addressbook list for SERVER.
Shows contact counts if resources are already loaded, otherwise shows '?'.
Resources will be loaded on-demand when opening an addressbook."
  (let ((addressbooks (oref server addressbooks))
        (entries nil))
    ;; Populate table without loading resources - much faster
    ;; Resources will be loaded when user opens the addressbook
    (dolist (addressbook addressbooks)
      (let* ((name (or (oref addressbook display-name) "Unnamed"))
             (desc (or (oref addressbook description) ""))
             (count (ecard-display--addressbook-contact-count addressbook)))
        (push (list addressbook
                    (vector name desc count))
              entries)))
    (setq tabulated-list-entries (nreverse entries))
    (tabulated-list-print t)
    (message "Loaded %d addressbook(s)" (length addressbooks))))

(defun ecard-display-addressbooks-refresh ()
  "Refresh the addressbook list."
  (interactive)
  (unless ecard-display--server
    (user-error "No server associated with this buffer"))
  (message "Refreshing addressbooks...")
  (condition-case err
      (progn
        (ecard-carddav-discover-addressbooks ecard-display--server)
        (ecard-display-addressbooks--populate ecard-display--server)
        (message "Refreshed addressbooks"))
    (error
     (message "Failed to refresh: %s" (error-message-string err)))))

(defun ecard-display-addressbooks-open ()
  "Open contacts for the addressbook at point."
  (interactive)
  (let ((addressbook (tabulated-list-get-id)))
    (unless addressbook
      (user-error "No addressbook at point"))
    (message "Loading contacts...")
    (condition-case err
        (progn
          ;; Ensure resources are loaded (paths and ETags)
          (unless (oref addressbook resources)
            (ecard-carddav-list-resources addressbook))
          (ecard-display-contacts addressbook))
      (error
       (message "Failed to load contacts: %s" (error-message-string err))))))

(defun ecard-display-addressbooks-back ()
  "Return to server list."
  (interactive)
  (ecard-display))

;;; Contacts Buffer

(defvar ecard-display-contacts-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'ecard-display-contacts-open)
    (define-key map (kbd "a") #'ecard-display-contacts-add)
    (define-key map (kbd "d") #'ecard-display-contacts-delete)
    (define-key map (kbd "r") #'ecard-display-contacts-refresh)
    (define-key map (kbd "R") #'ecard-display-contacts-refresh-names)
    (define-key map (kbd "n") #'ecard-display-contacts-next-page)
    (define-key map (kbd "p") #'ecard-display-contacts-prev-page)
    (define-key map (kbd "L") #'ecard-display-contacts-load-all)
    (define-key map (kbd "^") #'ecard-display-contacts-back)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `ecard-display-contacts-mode'.")

(define-derived-mode ecard-display-contacts-mode tabulated-list-mode "CardDAV Contacts"
  "Major mode for browsing contacts in a CardDAV addressbook.

By default, contacts are loaded in pages for better performance with large
addressbooks. Use \\[ecard-display-contacts-next-page] and \\[ecard-display-contacts-prev-page]
to navigate between pages, or \\[ecard-display-contacts-load-all] to load all contacts at once.

If `ecard-display-fetch-names-immediately' is nil, filename-based names are
shown initially for instant navigation. Use \\[ecard-display-contacts-refresh-names]
to fetch real contact names for the current page.

\\{ecard-display-contacts-mode-map}"
  (setq tabulated-list-format [("Full Name" 30 t)
                                ("Email" 35 t)
                                ("Phone" 20 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Full Name" nil))
  (tabulated-list-init-header))

(defun ecard-display-contacts (addressbook)
  "Display contacts for ADDRESSBOOK.
Always refreshes the contact list when entering the buffer, ensuring names
are fetched if `ecard-display-fetch-names-immediately' is enabled."
  (let* ((name (or (oref addressbook display-name) "Unnamed"))
         (buffer (get-buffer-create (format "*CardDAV Contacts: %s*" name))))
    (with-current-buffer buffer
      (unless (eq major-mode 'ecard-display-contacts-mode)
        (ecard-display-contacts-mode))
      (setq ecard-display--addressbook addressbook)
      ;; Always populate, even if buffer existed - this ensures names
      ;; are fetched when ecard-display-fetch-names-immediately is t
      (ecard-display-contacts--populate addressbook))
    (switch-to-buffer buffer)))

(defun ecard-display-contacts--populate (addressbook &optional page fetch-names)
  "Populate contact list for ADDRESSBOOK.
Optional PAGE specifies which page to load (default 0).
Optional FETCH-NAMES forces fetching real names via multiget.

If `ecard-display-contacts-page-size' is nil, loads all contacts at once.
Otherwise, loads contacts in pages for better performance with large
addressbooks.

If FETCH-NAMES is nil and `ecard-display-fetch-names-immediately' is nil,
displays filename-based names instantly for immediate navigation.
Real names can be fetched later with `ecard-display-contacts-refresh-names'.

Uses addressbook-multiget to efficiently fetch contact details for the
current page in a single HTTP request."
  (let* ((all-resources (oref addressbook resources))
         (page (or page ecard-display--current-page 0))
         (page-size ecard-display-contacts-page-size)
         (total-count (length all-resources)))

    (when (null all-resources)
      (user-error "No resources loaded for addressbook"))

    ;; Store total for pagination info
    (setq ecard-display--total-contacts total-count
          ecard-display--current-page page)

    ;; Calculate page boundaries
    (let* ((start (if page-size (* page page-size) 0))
           (end (if page-size
                    (min (+ start page-size) total-count)
                  total-count))
           (page-resources (if page-size
                               (cl-subseq all-resources start end)
                             all-resources))
           (page-info (if page-size
                          (format "page %d/%d, contacts %d-%d/%d"
                                  (1+ page)
                                  (ceiling (/ (float total-count) page-size))
                                  (1+ start)
                                  end
                                  total-count)
                        (format "%d contacts" total-count))))

      ;; Fetch vCard data for current page only if requested
      (when (and (> (length page-resources) 0)
                 (or fetch-names ecard-display-fetch-names-immediately))
        (message "Fetching names for %s..." page-info)
        (let ((start-time (float-time)))
          (condition-case err
              (let* ((paths (mapcar (lambda (r) (oref r path)) page-resources))
                     (fetched-resources (ecard-carddav-multiget-resources addressbook paths)))
                ;; Update resources in the ORIGINAL all-resources list with fetched ecard data
                ;; Use hash table for O(1) lookups instead of O(n) cl-find
                (let ((path-to-resource (make-hash-table :test 'equal :size (length all-resources))))
                  ;; Build hash table: path -> resource
                  (dolist (resource all-resources)
                    (puthash (oref resource path) resource path-to-resource))
                  ;; Update resources using hash table lookup
                  (dolist (fetched fetched-resources)
                    (let ((original (gethash (oref fetched path) path-to-resource)))
                      (when original
                        (oset original ecard (oref fetched ecard))
                        (oset original ecard-data (oref fetched ecard-data))
                        (oset original etag (oref fetched etag))))))
                (message "Fetched names in %.2f seconds" (- (float-time) start-time)))
            (error
             (let ((error-msg (format "Failed to fetch contact details (%s): %s"
                                      page-info
                                      (error-message-string err))))
               (message "%s" error-msg)
               (display-warning 'ecard-display error-msg :warning))))))

      ;; Populate table with ONLY current page resources to ensure consistent display
      ;; All displayed contacts will have real data (if multiget succeeded)
      (let ((entries nil)
            (start-time (float-time)))
        (dolist (resource page-resources)
          (let* ((ecard-obj (oref resource ecard))
                 (fn (if ecard-obj
                         (ecard-display--get-fn ecard-obj)
                       (ecard-display--extract-name-from-path (oref resource path))))
                 (email (if ecard-obj
                            (ecard-display--get-first-email ecard-obj)
                          ""))
                 (tel (if ecard-obj
                          (ecard-display--get-first-tel ecard-obj)
                        "")))
            (push (list resource (vector fn email tel)) entries)))
        (setq tabulated-list-entries (nreverse entries))
        (tabulated-list-print t)
        (let ((display-time (- (float-time) start-time))
              (total-displayed (length entries)))
          (message "Displayed %d contacts (%s) in %.2f seconds"
                   total-displayed page-info display-time))))))

(defun ecard-display--get-fn (ecard-obj)
  "Get formatted name from ECARD-OBJ."
  (if ecard-obj
      (let ((fn-props (oref ecard-obj fn)))
        (if fn-props
            (oref (car fn-props) value)
          "Unknown"))
    "Unknown"))

(defun ecard-display--get-first-email (ecard-obj)
  "Get first email from ECARD-OBJ."
  (if ecard-obj
      (let ((emails (oref ecard-obj email)))
        (if emails
            (oref (car emails) value)
          ""))
    ""))

(defun ecard-display--get-first-tel (ecard-obj)
  "Get first telephone from ECARD-OBJ."
  (if ecard-obj
      (let ((tels (oref ecard-obj tel)))
        (if tels
            (oref (car tels) value)
          ""))
    ""))

(defun ecard-display-contacts-refresh ()
  "Refresh the contact list."
  (interactive)
  (unless ecard-display--addressbook
    (user-error "No addressbook associated with this buffer"))
  (message "Refreshing contacts...")
  (condition-case err
      (progn
        (ecard-carddav-list-resources ecard-display--addressbook)
        ;; Reset to page 0 on refresh
        (setq ecard-display--current-page 0)
        (ecard-display-contacts--populate ecard-display--addressbook 0)
        (message "Refreshed contacts"))
    (error
     (message "Failed to refresh: %s" (error-message-string err)))))

(defun ecard-display-contacts-refresh-names ()
  "Fetch real contact names for current page.
This is useful when `ecard-display-fetch-names-immediately' is nil
and you want to see actual contact names instead of filename-based
placeholders."
  (interactive)
  (unless ecard-display--addressbook
    (user-error "No addressbook associated with this buffer"))
  (ecard-display-contacts--populate ecard-display--addressbook
                                    ecard-display--current-page
                                    t))

(defun ecard-display-contacts-next-page ()
  "Load next page of contacts."
  (interactive)
  (unless ecard-display--addressbook
    (user-error "No addressbook associated with this buffer"))
  (unless ecard-display-contacts-page-size
    (user-error "Pagination is disabled (set `ecard-display-contacts-page-size')"))
  (let* ((total ecard-display--total-contacts)
         (page-size ecard-display-contacts-page-size)
         (max-page (1- (ceiling (/ (float total) page-size))))
         (next-page (1+ ecard-display--current-page)))
    (if (> next-page max-page)
        (message "Already at last page")
      (ecard-display-contacts--populate ecard-display--addressbook next-page))))

(defun ecard-display-contacts-prev-page ()
  "Load previous page of contacts."
  (interactive)
  (unless ecard-display--addressbook
    (user-error "No addressbook associated with this buffer"))
  (unless ecard-display-contacts-page-size
    (user-error "Pagination is disabled (set `ecard-display-contacts-page-size')"))
  (let ((prev-page (1- ecard-display--current-page)))
    (if (< prev-page 0)
        (message "Already at first page")
      (ecard-display-contacts--populate ecard-display--addressbook prev-page))))

(defun ecard-display-contacts-load-all ()
  "Load all contacts at once (disable pagination for current buffer)."
  (interactive)
  (unless ecard-display--addressbook
    (user-error "No addressbook associated with this buffer"))
  (when (or (not ecard-display-contacts-page-size)
            (yes-or-no-p (format "Load all %d contacts? This may take a while. "
                                 ecard-display--total-contacts)))
    (let ((ecard-display-contacts-page-size nil))
      (ecard-display-contacts--populate ecard-display--addressbook 0))))

(defun ecard-display-contacts-open ()
  "Open contact detail view for contact at point.
Fetches full vCard data if not already loaded."
  (interactive)
  (let ((resource (tabulated-list-get-id)))
    (unless resource
      (user-error "No contact at point"))
    ;; Fetch vCard data if not already loaded
    (unless (oref resource ecard)
      (message "Loading contact...")
      (condition-case err
          (let ((fetched (ecard-carddav-get-resource ecard-display--addressbook
                                                       (oref resource url))))
            (oset resource ecard (oref fetched ecard))
            (oset resource etag (oref fetched etag))
            (message "Loaded contact"))
        (ecard-parse-error
         (let ((error-details (error-message-string err)))
           (message "Failed to parse vCard: %s" error-details)
           (user-error "Contact has invalid vCard data: %s\n\nThis contact may have been created by another application with non-standard formatting. The parser has been improved to handle whitespace issues, but this specific vCard may require manual correction" error-details)))
        (ecard-validation-error
         (let ((error-details (error-message-string err)))
           (message "Failed to validate vCard: %s" error-details)
           (user-error "Contact failed validation: %s" error-details)))
        (error
         (message "Failed to load contact: %s" (error-message-string err))
         (user-error "Failed to load contact: %s" (error-message-string err)))))
    (ecard-display-contact-detail resource)))

(defun ecard-display-contacts-add ()
  "Add a new contact."
  (interactive)
  (unless ecard-display--addressbook
    (user-error "No addressbook associated with this buffer"))
  (let* ((fn (read-string "Full name: "))
         (given (read-string "Given name: "))
         (family (read-string "Family name: "))
         (email (read-string "Email: "))
         (tel (read-string "Phone: "))
         ;; Create new ecard object
         (ecard-obj (ecard-create :fn fn))
         ;; Generate a unique filename
         (filename (format "%s.vcf" (format-time-string "%Y%m%d-%H%M%S")))
         (path (concat "/" filename)))

    ;; Add N property if we have name components
    (when (or given family)
      (let ((n-prop (ecard-property
                     :name "N"
                     :value (list family given "" "" ""))))
        (oset ecard-obj n (list n-prop))))

    ;; Add EMAIL if provided
    (when (and email (not (string-empty-p email)))
      (let ((email-prop (ecard-property
                         :name "EMAIL"
                         :parameters '(("TYPE" . "work"))
                         :value email)))
        (oset ecard-obj email (list email-prop))))

    ;; Add TEL if provided
    (when (and tel (not (string-empty-p tel)))
      (let ((tel-prop (ecard-property
                       :name "TEL"
                       :parameters '(("TYPE" . "work"))
                       :value tel)))
        (oset ecard-obj tel (list tel-prop))))

    ;; Add UID
    (let ((uid (format "urn:uuid:%s" (ecard-display--generate-uuid))))
      (oset ecard-obj uid (list (ecard-property :name "UID" :value uid))))

    (message "Creating contact...")
    (condition-case err
        (progn
          (ecard-carddav-put-ecard ecard-display--addressbook path ecard-obj)
          (ecard-display-contacts-refresh)
          (message "Created contact: %s" fn))
      (error
       (message "Failed to create contact: %s" (error-message-string err))))))

(defun ecard-display-contacts-delete ()
  "Delete the contact at point."
  (interactive)
  (let ((resource (tabulated-list-get-id)))
    (unless resource
      (user-error "No contact at point"))
    (let ((ecard-obj (oref resource ecard)))
      (when (or (not ecard-display-confirm-delete)
                (yes-or-no-p (format "Delete contact '%s'? "
                                     (ecard-display--get-fn ecard-obj))))
        (message "Deleting contact...")
        (condition-case err
            (progn
              (ecard-carddav-delete-resource ecard-display--addressbook
                                             (oref resource url)
                                             (oref resource etag))
              (ecard-display-contacts-refresh)
              (message "Deleted contact"))
          (error
           (message "Failed to delete contact: %s" (error-message-string err))))))))

(defun ecard-display-contacts-back ()
  "Return to addressbook list."
  (interactive)
  (when ecard-display--addressbook
    (let ((server (oref ecard-display--addressbook server)))
      (when server
        ;; We need the config to recreate the buffer name
        ;; For now, just go back to servers list
        (ecard-display)))))

;;; Contact Detail Buffer

(defvar ecard-display-contact-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ecard-display-contact-save)
    (define-key map (kbd "C-c C-k") #'ecard-display-contact-revert)
    (define-key map (kbd "C-c C-d") #'ecard-display-contact-delete)
    (define-key map (kbd "q") #'ecard-display-contact-quit)
    map)
  "Keymap for `ecard-display-contact-mode'.")

(define-derived-mode ecard-display-contact-mode special-mode "CardDAV Contact"
  "Major mode for viewing and editing a CardDAV contact.

\\{ecard-display-contact-mode-map}"
  (setq buffer-read-only nil))

(defun ecard-display-contact-detail (resource)
  "Display detail view for RESOURCE."
  (let* ((ecard-obj (oref resource ecard))
         (fn (ecard-display--get-fn ecard-obj))
         (buffer (get-buffer-create (format "*CardDAV Contact: %s*" fn))))
    (with-current-buffer buffer
      (ecard-display-contact-mode)
      (setq ecard-display--resource resource
            ecard-display--addressbook (oref resource addressbook)
            ecard-display--original-ecard (ecard-display--clone-ecard ecard-obj)
            ecard-display--modified nil)
      (ecard-display-contact--render resource)
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(defun ecard-display-contact--render (resource)
  "Render contact detail form for RESOURCE."
  (let ((inhibit-read-only t)
        (ecard-obj (oref resource ecard)))
    (erase-buffer)
    (remove-overlays)

    ;; Header
    (widget-insert (propertize "Contact Details\n" 'face 'bold))
    (widget-insert (propertize (make-string 60 ?=) 'face 'shadow))
    (widget-insert "\n\n")

    ;; UID
    (let ((uids (oref ecard-obj uid)))
      (when uids
        (widget-insert (propertize "UID: " 'face 'bold))
        (widget-insert (ecard-display--safe-string (oref (car uids) value)))
        (widget-insert "\n\n")))

    ;; FN
    (widget-insert (propertize "Full Name: " 'face 'bold))
    (widget-insert (ecard-display--safe-string (ecard-display--get-fn ecard-obj)))
    (widget-insert "\n\n")

    ;; N (structured name)
    (let ((n-props (oref ecard-obj n)))
      (when n-props
        (let* ((n-val (oref (car n-props) value))
               (family (ecard-display--safe-string (if (listp n-val) (nth 0 n-val) "")))
               (given (ecard-display--safe-string (if (listp n-val) (nth 1 n-val) "")))
               (additional (ecard-display--safe-string (if (listp n-val) (nth 2 n-val) "")))
               (prefix (ecard-display--safe-string (if (listp n-val) (nth 3 n-val) "")))
               (suffix (ecard-display--safe-string (if (listp n-val) (nth 4 n-val) ""))))
          (widget-insert (propertize "Given Name: " 'face 'bold))
          (widget-insert given)
          (widget-insert "\n")
          (widget-insert (propertize "Family Name: " 'face 'bold))
          (widget-insert family)
          (widget-insert "\n")
          (when (not (string-empty-p additional))
            (widget-insert (propertize "Additional: " 'face 'bold))
            (widget-insert additional)
            (widget-insert "\n"))
          (when (not (string-empty-p prefix))
            (widget-insert (propertize "Prefix: " 'face 'bold))
            (widget-insert prefix)
            (widget-insert "\n"))
          (when (not (string-empty-p suffix))
            (widget-insert (propertize "Suffix: " 'face 'bold))
            (widget-insert suffix)
            (widget-insert "\n"))
          (widget-insert "\n"))))

    ;; Emails
    (widget-insert (propertize "Emails:\n" 'face 'bold))
    (let ((emails (oref ecard-obj email)))
      (if emails
          (dolist (email-prop emails)
            (let ((type (ecard-display--safe-string (ecard-display--get-param email-prop "TYPE")))
                  (value (ecard-display--safe-string (oref email-prop value))))
              (widget-insert (format "  [%s] %s\n"
                                     (if (string-empty-p type) "other" type)
                                     value))))
        (widget-insert "  (none)\n")))
    (widget-insert "\n")

    ;; Telephones
    (widget-insert (propertize "Telephones:\n" 'face 'bold))
    (let ((tels (oref ecard-obj tel)))
      (if tels
          (dolist (tel-prop tels)
            (let ((type (ecard-display--safe-string (ecard-display--get-param tel-prop "TYPE")))
                  (value (ecard-display--safe-string (oref tel-prop value))))
              (widget-insert (format "  [%s] %s\n"
                                     (if (string-empty-p type) "other" type)
                                     value))))
        (widget-insert "  (none)\n")))
    (widget-insert "\n")

    ;; Addresses
    (let ((addrs (oref ecard-obj adr)))
      (when addrs
        (widget-insert (propertize "Addresses:\n" 'face 'bold))
        (dolist (adr-prop addrs)
          (let ((type (ecard-display--safe-string (ecard-display--get-param adr-prop "TYPE")))
                (value (oref adr-prop value)))
            (widget-insert (format "  [%s] " (if (string-empty-p type) "other" type)))
            (if (listp value)
                (let ((street (ecard-display--safe-string (nth 2 value)))
                      (city (ecard-display--safe-string (nth 3 value)))
                      (region (ecard-display--safe-string (nth 4 value)))
                      (postal (ecard-display--safe-string (nth 5 value)))
                      (country (ecard-display--safe-string (nth 6 value))))
                  (widget-insert (mapconcat #'identity
                                            (cl-remove-if #'string-empty-p
                                                          (list street city region postal country))
                                            ", ")))
              (widget-insert (ecard-display--safe-string value)))
            (widget-insert "\n")))
        (widget-insert "\n")))

    ;; Organization
    (let ((orgs (oref ecard-obj org)))
      (when orgs
        (widget-insert (propertize "Organization: " 'face 'bold))
        (widget-insert (ecard-display--safe-string (oref (car orgs) value)))
        (widget-insert "\n")))

    ;; Title
    (let ((titles (oref ecard-obj title)))
      (when titles
        (widget-insert (propertize "Title: " 'face 'bold))
        (widget-insert (ecard-display--safe-string (oref (car titles) value)))
        (widget-insert "\n")))

    ;; Note
    (let ((notes (oref ecard-obj note)))
      (when notes
        (widget-insert "\n")
        (widget-insert (propertize "Notes:\n" 'face 'bold))
        (widget-insert (ecard-display--safe-string (oref (car notes) value)))
        (widget-insert "\n")))

    ;; Footer
    (widget-insert "\n")
    (widget-insert (propertize (make-string 60 ?=) 'face 'shadow))
    (widget-insert "\n")
    (widget-insert "[C-c C-c] Save  [C-c C-k] Revert  [C-c C-d] Delete  [q] Quit\n")

    (use-local-map ecard-display-contact-mode-map)))

(defun ecard-display--get-param (prop param-name)
  "Get parameter PARAM-NAME from property PROP."
  (let ((params (oref prop parameters)))
    (cdr (assoc param-name params))))

(defun ecard-display-contact-save ()
  "Save changes to the current contact."
  (interactive)
  (unless ecard-display--resource
    (user-error "No contact associated with this buffer"))
  (message "Saving contact...")
  (condition-case err
      (let* ((resource ecard-display--resource)
             (ecard-obj (oref resource ecard))
             (url (oref resource url))
             (etag (oref resource etag)))
        (ecard-carddav-put-ecard ecard-display--addressbook url ecard-obj etag)
        (setq ecard-display--modified nil
              ecard-display--original-ecard (ecard-display--clone-ecard ecard-obj))
        (message "Saved contact"))
    (error
     (message "Failed to save contact: %s" (error-message-string err)))))

(defun ecard-display-contact-revert ()
  "Revert changes to the current contact."
  (interactive)
  (when (or (not ecard-display--modified)
            (yes-or-no-p "Discard all changes? "))
    (when ecard-display--original-ecard
      (oset ecard-display--resource ecard ecard-display--original-ecard)
      (ecard-display-contact--render ecard-display--resource)
      (setq ecard-display--modified nil)
      (message "Reverted changes"))))

(defun ecard-display-contact-delete ()
  "Delete the current contact."
  (interactive)
  (unless ecard-display--resource
    (user-error "No contact associated with this buffer"))
  (let ((ecard-obj (oref ecard-display--resource ecard)))
    (when (yes-or-no-p (format "Delete contact '%s'? "
                               (ecard-display--get-fn ecard-obj)))
      (message "Deleting contact...")
      (condition-case err
          (progn
            (ecard-carddav-delete-resource ecard-display--addressbook
                                           (oref ecard-display--resource url)
                                           (oref ecard-display--resource etag))
            (message "Deleted contact")
            (kill-buffer)
            ;; Try to refresh the contacts list if it's still open
            (dolist (buf (buffer-list))
              (with-current-buffer buf
                (when (eq major-mode 'ecard-display-contacts-mode)
                  (ecard-display-contacts-refresh)))))
        (error
         (message "Failed to delete contact: %s" (error-message-string err)))))))

(defun ecard-display-contact-quit ()
  "Quit the contact detail buffer."
  (interactive)
  (when (or (not ecard-display--modified)
            (yes-or-no-p "Discard unsaved changes? "))
    (quit-window t)))

;;; Helper functions

(defun ecard-display--addressbook-contact-count (addressbook)
  "Get contact count for ADDRESSBOOK.
Returns count as string if resources are loaded, otherwise '?'."
  (let ((resources (oref addressbook resources)))
    (if resources
        (number-to-string (length resources))
      "?")))

(defun ecard-display--safe-string (value)
  "Convert VALUE to a safe string for widget-insert.
Handles nil, empty lists, lists with empty strings, and other edge cases."
  (cond
   ;; nil -> empty string
   ((null value) "")
   ;; Already a string -> use it
   ((stringp value) value)
   ;; Empty list -> empty string
   ((and (listp value) (null value)) "")
   ;; List with one empty string -> empty string
   ((and (listp value) (equal value '(""))) "")
   ;; List with one string -> use that string
   ((and (listp value) (= (length value) 1) (stringp (car value)))
    (car value))
   ;; List of strings -> join with commas
   ((and (listp value) (cl-every #'stringp value))
    (mapconcat #'identity value ", "))
   ;; Other list -> format it
   ((listp value) (format "%S" value))
   ;; Number -> convert to string
   ((numberp value) (number-to-string value))
   ;; Symbol -> convert to string
   ((symbolp value) (symbol-name value))
   ;; Anything else -> format it
   (t (format "%s" value))))

(defun ecard-display--extract-name-from-path (path)
  "Extract a display name from vCard PATH.
Removes .vcf extension and URL-decodes the filename."
  (let* ((filename (file-name-nondirectory path))
         (name (file-name-sans-extension filename)))
    ;; URL-decode the name
    (url-unhex-string name)))

(defun ecard-display--normalize-server-config (config)
  "Normalize server CONFIG to internal format.
Accepts either:
  - Plist with :name, :url, :username, :password keys
  - ecard-carddav-server object directly

Returns a plist suitable for use in ecard-display."
  (cond
   ;; Already an ecard-carddav-server object - extract info
   ((ecard-carddav-server-p config)
    (let ((url (oref config url))
          (auth (oref config auth)))
      (list :name (or url "Unnamed Server")
            :url url
            :username (when auth (oref auth username))
            :password (when auth (oref auth password))
            :server-object config)))

   ;; Plist configuration - return as-is
   ((and (listp config) (plist-get config :url))
    config)

   (t (error "Invalid server configuration: must be plist or ecard-carddav-server object"))))

(defun ecard-display--server-config-to-object (config)
  "Convert server CONFIG (plist or object) to ecard-carddav-server object.
If CONFIG is already an ecard-carddav-server object, return it.
If CONFIG is a plist, create a new ecard-carddav-server object."
  (cond
   ;; Already an ecard-carddav-server object
   ((ecard-carddav-server-p config)
    config)

   ;; Plist configuration - create server object
   ((listp config)
    (let* ((url (plist-get config :url))
           (username (plist-get config :username))
           (password (plist-get config :password))
           (auth (when (and username password)
                   (ecard-carddav-auth-basic-create
                    :username username
                    :password password))))
      (unless url
        (error "Server configuration must include :url"))
      (ecard-carddav-server-create
       :url url
       :auth auth)))

   (t (error "Invalid server configuration: %S" config))))

(defun ecard-display--clone-ecard (ecard-obj)
  "Create a deep copy of ECARD-OBJ."
  ;; For now, serialize and re-parse to get a clean copy
  (if ecard-obj
      (ecard-parse (ecard-serialize ecard-obj))
    nil))

(defun ecard-display--generate-uuid ()
  "Generate a simple UUID-like string."
  (format "%04x%04x-%04x-%04x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536)
          (logior (logand (random 65536) 16383) 16384)
          (logior (logand (random 65536) 16383) 32768)
          (random 65536) (random 65536) (random 65536)))

(provide 'ecard-display)
;;; ecard-display.el ends here
