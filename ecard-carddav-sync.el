;;; ecard-carddav-sync.el --- CardDAV synchronization engine -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: comm, data, carddav, sync
;; URL: https://github.com/jwiegley/dot-emacs

;;; Commentary:

;; Synchronization engine for CardDAV with conflict resolution.
;;
;; This module provides:
;; - Initial full synchronization
;; - Incremental sync using sync-token or CTag
;; - Local caching with ETags
;; - Conflict detection and resolution strategies
;; - Diff detection between local and server states
;; - addressbook-multiget for efficient batch retrieval
;; - sync-collection REPORT support
;;
;; Conflict resolution strategies:
;; - :server-wins - Server version takes precedence
;; - :client-wins - Client version takes precedence
;; - :manual - Invoke user callback for manual resolution
;; - :newest - Use REV property to pick newest version
;;
;; Example usage:
;;
;;   ;; Create sync manager
;;   (setq sync (ecard-carddav-sync-create
;;               :addressbook addressbook
;;               :cache-dir "~/.emacs.d/carddav-cache"
;;               :strategy :server-wins))
;;
;;   ;; Initial full sync
;;   (ecard-carddav-sync-full sync)
;;
;;   ;; Incremental sync
;;   (ecard-carddav-sync-incremental sync)
;;
;;   ;; Get locally cached contacts
;;   (ecard-carddav-sync-get-local sync "/contacts/john.vcf")

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'ecard-compat)
(require 'ecard)
(require 'ecard-carddav)
(require 'ecard-carddav-auth)

;;; Custom group

(defgroup ecard-carddav-sync nil
  "Synchronization engine for CardDAV."
  :group 'ecard-carddav
  :prefix "ecard-carddav-sync-")

(defcustom ecard-carddav-sync-batch-size 50
  "Number of resources to fetch in a single multiget request."
  :type 'integer
  :group 'ecard-carddav-sync)

;;; Error conditions

(define-error 'ecard-carddav-sync-error "CardDAV sync error")
(define-error 'ecard-carddav-sync-conflict "CardDAV sync conflict" 'ecard-carddav-sync-error)

;;; EIEIO Classes

(defclass ecard-carddav-sync ()
  ((addressbook
    :initarg :addressbook
    :initform nil
    :type (or null ecard-carddav-addressbook)
    :documentation "Address book to synchronize.")
   (cache-dir
    :initarg :cache-dir
    :initform nil
    :type (or null string)
    :documentation "Directory for local cache storage.")
   (local-cache
    :initarg :local-cache
    :initform (make-hash-table :test 'equal)
    :type hash-table
    :documentation "Hash table: path -> (ecard etag mtime).")
   (last-sync-token
    :initarg :last-sync-token
    :initform nil
    :type (or null string)
    :documentation "Last sync-token from server.")
   (last-ctag
    :initarg :last-ctag
    :initform nil
    :type (or null string)
    :documentation "Last CTag from server.")
   (strategy
    :initarg :strategy
    :initform :server-wins
    :type symbol
    :documentation "Conflict resolution strategy.")
   (conflict-callback
    :initarg :conflict-callback
    :initform nil
    :type (or null function)
    :documentation "Callback for manual conflict resolution."))
  "Synchronization manager for CardDAV address book.")

(defclass ecard-carddav-sync-conflict ()
  ((path
    :initarg :path
    :initform nil
    :type (or null string)
    :documentation "Path to conflicting resource.")
   (local-ecard
    :initarg :local-ecard
    :initform nil
    ;; Note: :type removed because ecard is now a cl-defstruct, not an EIEIO class
    :documentation "Local version of vCard.")
   (local-etag
    :initarg :local-etag
    :initform nil
    :type (or null string)
    :documentation "Local ETag.")
   (server-ecard
    :initarg :server-ecard
    :initform nil
    ;; Note: :type removed because ecard is now a cl-defstruct, not an EIEIO class
    :documentation "Server version of vCard.")
   (server-etag
    :initarg :server-etag
    :initform nil
    :type (or null string)
    :documentation "Server ETag.")
   (resolution
    :initarg :resolution
    :initform nil
    :type symbol
    :documentation "Resolved action: :use-local, :use-server, :merge."))
  "Represents a synchronization conflict.")

;;; Cache management

(defun ecard-carddav-sync--cache-file-path (sync path)
  "Get cache file path for SYNC manager and resource PATH."
  (let* ((cache-dir (oref sync cache-dir))
         ;; Sanitize path for filename
         (safe-name (replace-regexp-in-string "[/:]" "_" path)))
    (expand-file-name (concat safe-name ".vcf") cache-dir)))

(defun ecard-carddav-sync--ensure-cache-dir (sync)
  "Ensure cache directory exists for SYNC manager."
  (let ((cache-dir (oref sync cache-dir)))
    (unless (file-directory-p cache-dir)
      (make-directory cache-dir t))))

(defun ecard-carddav-sync--save-to-cache (sync path ecard etag)
  "Save VCARD with ETAG to cache for SYNC manager at PATH."
  (ecard-carddav-sync--ensure-cache-dir sync)
  (let* ((cache-file (ecard-carddav-sync--cache-file-path sync path))
         (ecard-data (ecard-serialize ecard)))
    (with-temp-file cache-file
      (insert ecard-data))
    ;; Update in-memory cache
    (puthash path
             (list ecard etag (float-time))
             (oref sync local-cache))))

(defun ecard-carddav-sync--load-from-cache (sync path)
  "Load vCard from cache for SYNC manager at PATH.
Returns (ecard etag mtime) or nil if not in cache."
  (gethash path (oref sync local-cache)))

(defun ecard-carddav-sync--remove-from-cache (sync path)
  "Remove resource at PATH from cache for SYNC manager."
  (let ((cache-file (ecard-carddav-sync--cache-file-path sync path)))
    (when (file-exists-p cache-file)
      (delete-file cache-file)))
  (remhash path (oref sync local-cache)))

(defun ecard-carddav-sync--load-cache-index (sync)
  "Load cache index from disk for SYNC manager."
  (ecard-carddav-sync--ensure-cache-dir sync)
  (let* ((cache-dir (oref sync cache-dir))
         (index-file (expand-file-name ".index" cache-dir)))
    (when (file-exists-p index-file)
      (with-temp-buffer
        (insert-file-contents index-file)
        (let ((data (read (buffer-string))))
          (when (hash-table-p data)
            (oset sync local-cache data)))))))

(defun ecard-carddav-sync--save-cache-index (sync)
  "Save cache index to disk for SYNC manager."
  (ecard-carddav-sync--ensure-cache-dir sync)
  (let* ((cache-dir (oref sync cache-dir))
         (index-file (expand-file-name ".index" cache-dir)))
    (with-temp-file index-file
      (prin1 (oref sync local-cache) (current-buffer)))))

;;; Conflict resolution

(defun ecard-carddav-sync--resolve-conflict (sync conflict)
  "Resolve CONFLICT using SYNC manager's strategy.
Returns resolved ecard object."
  (let ((strategy (oref sync strategy))
        (local (oref conflict local-ecard))
        (server (oref conflict server-ecard)))
    (pcase strategy
      (:server-wins
       (oset conflict resolution :use-server)
       server)
      (:client-wins
       (oset conflict resolution :use-local)
       local)
      (:newest
       (ecard-carddav-sync--resolve-newest conflict))
      (:manual
       (let ((callback (oref sync conflict-callback)))
         (if callback
             (funcall callback conflict)
           (signal 'ecard-carddav-sync-conflict
                   (list "Manual conflict resolution required but no callback set"
                         (oref conflict path))))))
      (_
       (signal 'ecard-carddav-sync-error
               (list "Unknown conflict resolution strategy" strategy))))))

(defun ecard-carddav-sync--resolve-newest (conflict)
  "Resolve CONFLICT by comparing REV properties.
Returns newest ecard or server version if REV not available."
  (let* ((local (oref conflict local-ecard))
         (server (oref conflict server-ecard))
         (local-rev (ecard-get-property-value local 'rev))
         (server-rev (ecard-get-property-value server 'rev)))
    (cond
     ((and local-rev server-rev)
      (if (string> server-rev local-rev)
          (progn
            (oset conflict resolution :use-server)
            server)
        (oset conflict resolution :use-local)
        local))
     (server-rev
      (oset conflict resolution :use-server)
      server)
     (local-rev
      (oset conflict resolution :use-local)
      local)
     (t
      ;; No REV on either - default to server
      (oset conflict resolution :use-server)
      server))))

;;; Synchronization operations

(defun ecard-carddav-sync-full (sync)
  "Perform full synchronization for SYNC manager.
Fetches all resources from server and updates local cache.
Returns list of updated paths."
  (ecard-carddav-sync--load-cache-index sync)
  (let* ((addressbook (oref sync addressbook))
         (resources (ecard-carddav-list-resources addressbook))
         (updated-paths nil))
    ;; Process each resource
    (dolist (resource resources)
      (let* ((path (oref resource path))
             (server-etag (oref resource etag))
             (cached (ecard-carddav-sync--load-from-cache sync path)))
        (if (and cached (string= server-etag (nth 1 cached)))
            ;; ETag matches - no change needed
            nil
          ;; Need to fetch
          (condition-case err
              (let ((full-resource (ecard-carddav-get-resource addressbook path)))
                (ecard-carddav-sync--save-to-cache
                 sync path
                 (oref full-resource ecard)
                 (oref full-resource etag))
                (push path updated-paths))
            (error
             (message "Warning: Failed to fetch %s: %s" path err))))))

    ;; Update sync metadata
    (oset sync last-sync-token (oref addressbook sync-token))
    (oset sync last-ctag (oref addressbook ctag))
    (ecard-carddav-sync--save-cache-index sync)

    (nreverse updated-paths)))

(defun ecard-carddav-sync-incremental (sync)
  "Perform incremental synchronization for SYNC manager.
Uses sync-token if available, otherwise falls back to CTag comparison.
Returns plist with :added :modified :deleted paths."
  (ecard-carddav-sync--load-cache-index sync)
  (let* ((addressbook (oref sync addressbook))
         (last-token (oref sync last-sync-token))
         (result nil))

    (if last-token
        ;; Use sync-collection REPORT
        (setq result (ecard-carddav-sync--sync-collection sync))
      ;; Fall back to CTag comparison
      (let* ((last-ctag (oref sync last-ctag))
             (current-ctag (oref addressbook ctag)))
        (if (and last-ctag (string= last-ctag current-ctag))
            ;; No changes
            (setq result '(:added nil :modified nil :deleted nil))
          ;; CTag changed - do full sync
          (let ((updated (ecard-carddav-sync-full sync)))
            (setq result (list :added updated :modified nil :deleted nil))))))

    (ecard-carddav-sync--save-cache-index sync)
    result))

(defun ecard-carddav-sync--sync-collection (sync)
  "Perform sync-collection REPORT for SYNC manager.
Returns plist with :added :modified :deleted paths."
  (let* ((addressbook (oref sync addressbook))
         (server (oref addressbook server))
         (auth (oref server auth))
         (url (oref addressbook url))
         (sync-token (oref sync last-sync-token))
         (request-body (ecard-carddav-sync--make-sync-collection-body sync-token))
         (buffer (ecard-carddav--request-with-retry
                  "REPORT" url auth request-body
                  "application/xml; charset=utf-8"
                  '(("Depth" . "1")))))
    (unwind-protect
        (let* ((status (ecard-carddav--get-http-status buffer))
               (xml (when (and status (= status 207))
                     (ecard-carddav--parse-xml-response buffer))))
          (if xml
              (ecard-carddav-sync--process-sync-response sync xml url)
            (signal 'ecard-carddav-sync-error
                    (list "sync-collection REPORT failed" status))))
      (kill-buffer buffer))))

(defun ecard-carddav-sync--make-sync-collection-body (sync-token)
  "Create sync-collection REPORT request body with SYNC-TOKEN."
  (with-temp-buffer
    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    (insert (format "<sync-collection xmlns=\"%s\" xmlns:C=\"%s\">\n"
                   ecard-carddav-ns-dav
                   ecard-carddav-ns-carddav))
    (insert (format "  <sync-token>%s</sync-token>\n" (or sync-token "")))
    (insert "  <sync-level>1</sync-level>\n")
    (insert "  <prop>\n")
    (insert "    <getetag/>\n")
    (insert "  </prop>\n")
    (insert "</sync-collection>\n")
    (buffer-string)))

(defun ecard-carddav-sync--process-sync-response (sync xml base-url)
  "Process sync-collection response XML for SYNC manager.
BASE-URL is used to resolve relative URLs.
Returns plist with :added :modified :deleted paths."
  (let ((added nil)
        (modified nil)
        (deleted nil)
        (new-sync-token nil))

    ;; Extract new sync-token
    (let ((token-nodes (ecard-carddav--dom-by-tag-qname xml 'sync-token ecard-carddav-ns-dav)))
      (when token-nodes
        (setq new-sync-token (dom-text (car token-nodes)))))

    ;; Process responses
    (let ((responses (ecard-carddav--dom-by-tag-qname xml 'response ecard-carddav-ns-dav)))
      (dolist (response responses)
        (let* ((href-node (ecard-carddav--dom-by-tag-qname response 'href ecard-carddav-ns-dav))
               (href (when href-node (dom-text (car href-node))))
               (status-node (ecard-carddav--dom-by-tag-qname response 'status ecard-carddav-ns-dav))
               (status-text (when status-node (dom-text (car status-node))))
               (is-deleted (and status-text (string-match "404" status-text))))

          (when href
            (let* ((url (ecard-carddav--resolve-url href base-url))
                   (path (url-filename (url-generic-parse-url url))))

              (if is-deleted
                  ;; Deleted resource
                  (progn
                    (ecard-carddav-sync--remove-from-cache sync path)
                    (push path deleted))

                ;; Added or modified resource
                (let* ((propstat (ecard-carddav--dom-by-tag-qname response 'propstat ecard-carddav-ns-dav))
                       (prop (when propstat (ecard-carddav--dom-by-tag-qname (car propstat) 'prop
                                                                               ecard-carddav-ns-dav)))
                       (etag-node (when prop (ecard-carddav--dom-by-tag-qname (car prop) 'getetag
                                                                                ecard-carddav-ns-dav)))
                       (etag (when etag-node (dom-text (car etag-node))))
                       (cached (ecard-carddav-sync--load-from-cache sync path)))

                  (when etag
                    (setq etag (string-trim etag "\"" "\"")))

                  (cond
                   ((not cached)
                    ;; New resource
                    (push path added))
                   ((not (string= etag (nth 1 cached)))
                    ;; Modified resource
                    (push path modified))))))))))

    ;; Fetch content for added/modified resources
    (let ((to-fetch (append added modified)))
      (ecard-carddav-sync--multiget sync to-fetch))

    ;; Update sync token
    (when new-sync-token
      (oset sync last-sync-token new-sync-token))

    (list :added (nreverse added)
          :modified (nreverse modified)
          :deleted (nreverse deleted))))

(defun ecard-carddav-sync--multiget (sync paths)
  "Fetch multiple resources using addressbook-multiget for SYNC manager.
PATHS is list of resource paths to fetch."
  (when paths
    (let ((batches (seq-partition paths ecard-carddav-sync-batch-size)))
      (dolist (batch batches)
        (ecard-carddav-sync--multiget-batch sync batch)))))

(defun ecard-carddav-sync--multiget-batch (sync paths)
  "Fetch batch of resources using addressbook-multiget for SYNC manager.
PATHS is list of resource paths to fetch in this batch."
  (let* ((addressbook (oref sync addressbook))
         (server (oref addressbook server))
         (auth (oref server auth))
         (url (oref addressbook url))
         (request-body (ecard-carddav-sync--make-multiget-body paths))
         (buffer (ecard-carddav--request-with-retry
                  "REPORT" url auth request-body
                  "application/xml; charset=utf-8"
                  '(("Depth" . "1")))))
    (unwind-protect
        (let* ((status (ecard-carddav--get-http-status buffer))
               (xml (when (and status (= status 207))
                     (ecard-carddav--parse-xml-response buffer))))
          (when xml
            (ecard-carddav-sync--process-multiget-response sync xml url)))
      (kill-buffer buffer))))

(defun ecard-carddav-sync--make-multiget-body (paths)
  "Create addressbook-multiget REPORT request body for PATHS."
  (with-temp-buffer
    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    (insert (format "<C:addressbook-multiget xmlns=\"%s\" xmlns:C=\"%s\">\n"
                   ecard-carddav-ns-dav
                   ecard-carddav-ns-carddav))
    (insert "  <prop>\n")
    (insert "    <getetag/>\n")
    (insert "    <C:address-data/>\n")
    (insert "  </prop>\n")
    (dolist (path paths)
      (insert (format "  <href>%s</href>\n" (xml-escape-string path))))
    (insert "</C:addressbook-multiget>\n")
    (buffer-string)))

(defun ecard-carddav-sync--process-multiget-response (sync xml base-url)
  "Process addressbook-multiget response XML for SYNC manager.
BASE-URL is used to resolve relative URLs."
  (let ((responses (ecard-carddav--dom-by-tag-qname xml 'response ecard-carddav-ns-dav)))
    (dolist (response responses)
      (let* ((href-node (ecard-carddav--dom-by-tag-qname response 'href ecard-carddav-ns-dav))
             (href (when href-node (dom-text (car href-node))))
             (propstat (ecard-carddav--dom-by-tag-qname response 'propstat ecard-carddav-ns-dav))
             (prop (when propstat (ecard-carddav--dom-by-tag-qname (car propstat) 'prop
                                                                     ecard-carddav-ns-dav)))
             (etag-node (when prop (ecard-carddav--dom-by-tag-qname (car prop) 'getetag
                                                                      ecard-carddav-ns-dav)))
             (etag (when etag-node (dom-text (car etag-node))))
             (data-node (when prop (ecard-carddav--dom-by-tag-qname (car prop) 'address-data
                                                                      ecard-carddav-ns-carddav)))
             (ecard-data (when data-node (dom-text (car data-node)))))

        (when (and href ecard-data)
          (let* ((url (ecard-carddav--resolve-url href base-url))
                 (path (url-filename (url-generic-parse-url url))))

            (when etag
              (setq etag (string-trim etag "\"" "\"")))

            (condition-case err
                (let ((ecard-obj (ecard-compat-parse ecard-data)))
                  (ecard-carddav-sync--save-to-cache sync path ecard-obj etag))
              (error
               (message "Warning: Failed to parse vCard at %s: %s" path err)))))))))

;;; Public API

(defun ecard-carddav-sync-get-local (sync path)
  "Get locally cached vCard at PATH from SYNC manager.
Returns ecard object or nil if not in cache."
  (let ((cached (ecard-carddav-sync--load-from-cache sync path)))
    (when cached
      (car cached))))

(defun ecard-carddav-sync-get-all-local (sync)
  "Get all locally cached vCards from SYNC manager.
Returns list of (path . ecard) pairs."
  (let ((results nil))
    (maphash (lambda (path data)
               (push (cons path (car data)) results))
             (oref sync local-cache))
    (nreverse results)))

;;;###autoload
(defun ecard-carddav-sync-create (&rest args)
  "Create synchronization manager from ARGS.

ARGS is a plist with keys:
  :addressbook ADDRESSBOOK - Address book to sync (required)
  :cache-dir STRING - Directory for cache storage (required)
  :strategy SYMBOL - Conflict resolution strategy (default :server-wins)
  :conflict-callback FUNCTION - Callback for manual resolution (optional)

Example:
  (ecard-carddav-sync-create
   :addressbook addressbook
   :cache-dir \"~/.emacs.d/carddav-cache\"
   :strategy :server-wins)"
  (let ((addressbook (plist-get args :addressbook))
        (cache-dir (plist-get args :cache-dir))
        (strategy (or (plist-get args :strategy) :server-wins))
        (callback (plist-get args :conflict-callback)))
    (unless addressbook
      (signal 'ecard-carddav-sync-error '("Address book required")))
    (unless cache-dir
      (signal 'ecard-carddav-sync-error '("Cache directory required")))
    (ecard-carddav-sync
     :addressbook addressbook
     :cache-dir (expand-file-name cache-dir)
     :strategy strategy
     :conflict-callback callback)))

(provide 'ecard-carddav-sync)
;;; ecard-carddav-sync.el ends here
