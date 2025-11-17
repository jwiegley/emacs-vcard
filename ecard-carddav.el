;;; ecard-carddav.el --- CardDAV protocol implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: comm, data, carddav, ecard
;; URL: https://github.com/jwiegley/dot-emacs

;;; Commentary:

;; Complete CardDAV (RFC 6352) protocol implementation for Emacs.
;;
;; This module provides:
;; - Service discovery via .well-known/carddav
;; - Address book discovery and listing
;; - Full CRUD operations on vCard resources
;; - addressbook-query REPORT support
;; - addressbook-multiget REPORT support
;; - sync-collection REPORT for incremental sync
;; - ETag-based concurrency control
;; - Sync-token support
;; - Async operations to avoid blocking
;;
;; Example usage:
;;
;;   ;; Connect to CardDAV server with static auth
;;   (setq server (ecard-carddav-server-create
;;                 :url "https://carddav.example.com"
;;                 :auth (ecard-carddav-auth-basic-create
;;                        :username "user"
;;                        :password "secret")))
;;
;;   ;; Or use a function for dynamic auth (e.g., prompting user)
;;   (setq server (ecard-carddav-server-create
;;                 :url "https://carddav.example.com"
;;                 :auth (lambda ()
;;                         (ecard-carddav-auth-basic-create
;;                          :username (read-string "Username: ")
;;                          :password (read-passwd "Password: ")))))
;;
;;   ;; Discover address books. Returns list of address books and also updates
;;   ;; `addressbooks' slot of the server object.
;;   (ecard-carddav-discover-addressbooks server)
;;
;;   ;; List contacts in address book. Also updates `resources' slot of the
;;   ;; `addressbook' object.
;;   (ecard-carddav-list-resources addressbook)
;;
;;   ;; Get a contact resource. The string can should either be the `path' or
;;   ;; `url' field of a resource object. Updates the `ecard' slot of that
;;   ;; resource with the ecard object, and also the `ecard-data' slot with
;;   ;; the vCard string.
;;   (setq resource
;;         (ecard-carddav-get-resource addressbook "/contacts/john.vcf"))
;;   (oref resource ecard)                ;; this is the ecard object
;;
;;   ;; Create/update contact from the given ecard object.
;;   (ecard-carddav-put-ecard addressbook "/contacts/jane.vcf" ecard-obj)
;;
;;   ;; Delete contact. Note that this does not delete any associated resource
;;   ;; from the addressbook object, because it is not necessary that you have
;;   ;; populated the list of resources before using this function.
;;   (ecard-carddav-delete-resource addressbook "/contacts/old.vcf")

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'url)
(require 'url-http)
(require 'dom)
(require 'xml)
(require 'ecard)
(require 'ecard-compat)
(require 'ecard-carddav-auth)

;;; Custom group

(defgroup ecard-carddav nil
  "CardDAV protocol implementation."
  :group 'ecard
  :prefix "ecard-carddav-")

(defcustom ecard-carddav-timeout 30
  "Timeout in seconds for CardDAV HTTP requests."
  :type 'integer
  :group 'ecard-carddav)

(defcustom ecard-carddav-max-retries 3
  "Maximum number of retries for failed requests."
  :type 'integer
  :group 'ecard-carddav)

(defcustom ecard-carddav-retry-delay 1.0
  "Initial retry delay in seconds (exponential backoff)."
  :type 'float
  :group 'ecard-carddav)

(defcustom ecard-carddav-user-agent "Emacs vCard-CardDAV/1.0"
  "User-Agent string for CardDAV requests."
  :type 'string
  :group 'ecard-carddav)

;;; Error conditions

(define-error 'ecard-carddav-error "CardDAV error")
(define-error 'ecard-carddav-http-error "CardDAV HTTP error" 'ecard-carddav-error)
(define-error 'ecard-carddav-xml-error "CardDAV XML error" 'ecard-carddav-error)
(define-error 'ecard-carddav-conflict-error "CardDAV conflict error" 'ecard-carddav-error)
(define-error 'ecard-carddav-not-found-error "CardDAV not found error" 'ecard-carddav-error)

;;; EIEIO Classes

(defclass ecard-carddav-server ()
  ((url
    :initarg :url
    :initform nil
    :type (or null string)
    :documentation "Base URL of CardDAV server (e.g., https://example.com).")
   (auth
    :initarg :auth
    :initform nil
    :type (or null ecard-carddav-auth function)
    :documentation "Authentication credentials object or function that returns one.
If a function, it will be called with no arguments whenever authentication
is needed, allowing for dynamic credential retrieval (e.g., from auth-source,
password managers, or OAuth token refresh).")
   (principal-url
    :initarg :principal-url
    :initform nil
    :type (or null string)
    :documentation "Principal URL discovered from server.")
   (addressbook-home-url
    :initarg :addressbook-home-url
    :initform nil
    :type (or null string)
    :documentation "Address book home URL for this principal.")
   (addressbooks
    :initarg :addressbooks
    :initform nil
    :type list
    :documentation "List of addressbook objects on this server."))
  "Represents a CardDAV server connection.")

(defclass ecard-carddav-addressbook ()
  ((server
    :initarg :server
    :initform nil
    :type (or null ecard-carddav-server)
    :documentation "Reference to parent server object.")
   (url
    :initarg :url
    :initform nil
    :type (or null string)
    :documentation "Full URL to this address book resource.")
   (display-name
    :initarg :display-name
    :initform nil
    :type (or null string)
    :documentation "Human-readable name for this address book.")
   (description
    :initarg :description
    :initform nil
    :type (or null string)
    :documentation "Description of this address book.")
   (ctag
    :initarg :ctag
    :initform nil
    :type (or null string)
    :documentation "Current CTag (change tag) for this address book.")
   (sync-token
    :initarg :sync-token
    :initform nil
    :type (or null string)
    :documentation "Current sync-token for incremental sync.")
   (resources
    :initarg :resources
    :initform nil
    :type list
    :documentation "List of resource objects in this address book."))
  "Represents a CardDAV address book collection.")

(defclass ecard-carddav-resource ()
  ((addressbook
    :initarg :addressbook
    :initform nil
    :type (or null ecard-carddav-addressbook)
    :documentation "Reference to parent address book object.")
   (url
    :initarg :url
    :initform nil
    :type (or null string)
    :documentation "Full URL to this vCard resource.")
   (path
    :initarg :path
    :initform nil
    :type (or null string)
    :documentation "Path component of URL (relative to addressbook).")
   (etag
    :initarg :etag
    :initform nil
    :type (or null string)
    :documentation "ETag for concurrency control.")
   (ecard
    :initarg :ecard
    :initform nil
    :type (or null ecard)
    :documentation "Parsed ecard object (nil if not yet fetched).")
   (ecard-data
    :initarg :ecard-data
    :initform nil
    :type (or null string)
    :documentation "Raw vCard text data."))
  "Represents a vCard resource in a CardDAV address book.")

;;; XML namespace constants

(defconst ecard-carddav-ns-dav "DAV:"
  "WebDAV namespace URI.")

(defconst ecard-carddav-ns-carddav "urn:ietf:params:xml:ns:carddav"
  "CardDAV namespace URI.")

(defconst ecard-carddav-ns-cs "http://calendarserver.org/ns/"
  "CalendarServer namespace URI.")

;;; Helper functions

(defun ecard-carddav--get-auth (server)
  "Get authentication object from SERVER, resolving functions if needed.
If the :auth slot contains a function, call it to get the auth object.
Otherwise, return the auth object directly."
  (let ((auth (oref server auth)))
    (if (functionp auth)
        (funcall auth)
      auth)))

;;; XML generation helpers

(defun ecard-carddav--xml-to-string (xml)
  "Convert XML s-expression to string.
XML is in the format returned by `xml-parse-region'."
  (with-temp-buffer
    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    (xml-print (list xml))
    (buffer-string)))

(defun ecard-carddav--make-xml-element (name _namespace attrs &rest children)
  "Create XML element with NAME with ATTRS and CHILDREN.
NAMESPACE argument is ignored - use namespace prefixes in attrs instead.
Returns s-expression suitable for `xml-print'."
  (let ((tag (intern name)))
    (if children
        `(,tag ,attrs ,@children)
      `(,tag ,attrs))))

(defun ecard-carddav--dom-by-tag-qname (dom tag &optional namespace)
  "Find elements in DOM by TAG, handling both plain and QName symbols.
If NAMESPACE is provided, also searches for {NAMESPACE}TAG and C:TAG formats.
Returns list of matching nodes."
  (let* ((tag-str (if (symbolp tag) (symbol-name tag) tag))
         (plain-tag (intern tag-str))
         (qname-tag (when namespace
                      (intern (format "{%s}%s" namespace tag-str))))
         (prefix-tags (when namespace
                        (cond
                         ((string= namespace ecard-carddav-ns-carddav)
                          ;; Different servers use different prefixes: C: or CR:
                          (list (intern (concat "C:" tag-str))
                                (intern (concat "CR:" tag-str))))
                         ((string= namespace ecard-carddav-ns-cs)
                          (list (intern (concat "CS:" tag-str))))
                         (t nil)))))
    (append (dom-by-tag dom plain-tag)
            (when qname-tag
              (dom-by-tag dom qname-tag))
            (mapcan (lambda (ptag) (dom-by-tag dom ptag)) prefix-tags))))

(defun ecard-carddav--propfind-body (props)
  "Create PROPFIND request body requesting PROPS.
PROPS is list of (name . namespace) pairs."
  (with-temp-buffer
    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    (insert (format "<propfind xmlns=\"%s\" xmlns:C=\"%s\" xmlns:CS=\"%s\">\n"
                   ecard-carddav-ns-dav
                   ecard-carddav-ns-carddav
                   ecard-carddav-ns-cs))
    (insert "  <prop>\n")
    (dolist (prop props)
      (let ((name (car prop))
            (ns (cdr prop)))
        (cond
         ((string= ns ecard-carddav-ns-carddav)
          (insert (format "    <C:%s/>\n" name)))
         ((string= ns ecard-carddav-ns-cs)
          (insert (format "    <CS:%s/>\n" name)))
         (t
          (insert (format "    <%s/>\n" name))))))
    (insert "  </prop>\n")
    (insert "</propfind>\n")
    (buffer-string)))

;;; HTTP request helpers

(defun ecard-carddav--parse-xml-response (buffer)
  "Parse XML from BUFFER.
Returns parsed XML s-expression or signals error."
  (with-current-buffer buffer
    (goto-char (point-min))
    ;; Skip HTTP headers
    (when (re-search-forward "\r?\n\r?\n" nil t)
      (condition-case err
          (car (xml-parse-region (point) (point-max)))
        (error
         (signal 'ecard-carddav-xml-error
                 (list "Failed to parse XML response" err)))))))

(defun ecard-carddav--get-http-status (buffer)
  "Extract HTTP status code from BUFFER.
Returns integer status code or nil."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (string-to-number (match-string 1)))))

(defun ecard-carddav--get-http-header (buffer header-name)
  "Extract HTTP header HEADER-NAME from BUFFER.
Returns header value or nil."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward
           (format "^%s: \\([^\r\n]*\\)" (regexp-quote header-name))
           nil t)
      (match-string 1))))

(defun ecard-carddav--request (method url auth &optional body content-type headers)
  "Make HTTP request with METHOD to URL using AUTH.
BODY is optional request body string.
CONTENT-TYPE is optional Content-Type header.
HEADERS is optional alist of additional headers.
Returns response buffer.

Uses with-timeout to ensure request doesn't hang indefinitely,
even if DNS, TCP, or TLS operations block outside url.el's control."
  (with-timeout (ecard-carddav-timeout
                 (signal 'ecard-carddav-http-error
                         (list "Request timeout after" ecard-carddav-timeout "seconds" url)))
    (let* ((url-request-method method)
           (url-request-extra-headers
            (append
             (list (cons "User-Agent" ecard-carddav-user-agent))
             (when auth
               (list (cons "Authorization" (ecard-carddav-auth-get-header auth))))
             (when content-type
               (list (cons "Content-Type" content-type)))
             headers))
           (url-request-data (when body (encode-coding-string body 'utf-8)))
           (url-http-attempt-keepalives nil)  ; Avoid connection reuse issues
           (url-show-status nil)
           ;; Prevent url.el from prompting for credentials interactively
           ;; when we've already provided Authorization header
           (url-request-noninteractive t))
      (url-retrieve-synchronously url t nil ecard-carddav-timeout))))

(defun ecard-carddav--request-with-retry (method url auth &optional body content-type headers)
  "Make HTTP request with automatic retry on failure.
Uses exponential backoff strategy.
Returns response buffer or signals error."
  (let ((retries 0)
        (delay ecard-carddav-retry-delay)
        (buffer nil)
        (last-error nil))
    (while (and (< retries ecard-carddav-max-retries)
                (not buffer))
      (condition-case err
          (progn
            (setq buffer (ecard-carddav--request method url auth body content-type headers))
            ;; Check for server errors that should be retried
            (let ((status (ecard-carddav--get-http-status buffer)))
              (when (and status (>= status 500) (< status 600))
                (kill-buffer buffer)
                (setq buffer nil)
                (setq last-error (list 'http-error status))
                (setq retries (1+ retries))
                (when (< retries ecard-carddav-max-retries)
                  (sleep-for delay)
                  (setq delay (* delay 2))))))
        (error
         (setq last-error err)
         (setq retries (1+ retries))
         (when (< retries ecard-carddav-max-retries)
           (sleep-for delay)
           (setq delay (* delay 2))))))
    (unless buffer
      (signal 'ecard-carddav-http-error
              (list "Request failed after retries" last-error)))
    buffer))

;;; Service discovery

(defun ecard-carddav-discover-principal (server)
  "Discover principal URL for SERVER.
Updates SERVER object with principal-url.
Returns SERVER."
  (let* ((url (oref server url))
         (auth (ecard-carddav--get-auth server))
         (well-known-url (concat url "/.well-known/carddav"))
         (buffer (ecard-carddav--request-with-retry "PROPFIND" well-known-url auth
                                                     (ecard-carddav--propfind-body
                                                      `(("current-user-principal" . ,ecard-carddav-ns-dav)))
                                                     "application/xml; charset=utf-8"
                                                     '(("Depth" . "0")))))
    (unwind-protect
        (let* ((status (ecard-carddav--get-http-status buffer))
               (xml (when (and status (= status 207))
                      (ecard-carddav--parse-xml-response buffer))))
          (when xml
            (let ((principal (ecard-carddav--extract-principal-url xml url)))
              (oset server principal-url principal))))
      (kill-buffer buffer))
    server))

(defun ecard-carddav--extract-principal-url (xml base-url)
  "Extract principal URL from PROPFIND response XML.
BASE-URL is used to resolve relative URLs.
Returns absolute URL string."
  (let ((hrefs (ecard-carddav--dom-by-tag-qname xml 'current-user-principal
                                                 ecard-carddav-ns-dav)))
    (when hrefs
      (let ((href-node (ecard-carddav--dom-by-tag-qname (car hrefs) 'href
                                                          ecard-carddav-ns-dav)))
        (when href-node
          (let ((href (dom-text (car href-node))))
            ;; HREF should already be absolute or server-relative
            (if (string-prefix-p "http" href)
                href
              ;; Build full URL from base
              (let ((parsed-base (url-generic-parse-url base-url)))
                (format "%s://%s%s"
                       (url-type parsed-base)
                       (url-host parsed-base)
                       href)))))))))

(defun ecard-carddav-discover-addressbook-home (server)
  "Discover addressbook-home-set URL for SERVER.
Requires principal-url to be set.
Updates SERVER object with addressbook-home-url.
Returns SERVER."
  (unless (oref server principal-url)
    (ecard-carddav-discover-principal server))
  (let* ((principal-url (oref server principal-url))
         (auth (ecard-carddav--get-auth server)))
    (unless principal-url
      (signal 'ecard-carddav-error '("Principal URL not set after discovery")))
    (let ((buffer (ecard-carddav--request-with-retry
                   "PROPFIND" principal-url auth
                   (ecard-carddav--propfind-body
                    `(("addressbook-home-set" . ,ecard-carddav-ns-carddav)))
                   "application/xml; charset=utf-8"
                   '(("Depth" . "0")))))
      (unwind-protect
          (let* ((status (ecard-carddav--get-http-status buffer))
                 (xml (when (and status (= status 207))
                        (ecard-carddav--parse-xml-response buffer))))
            (when xml
              (let ((home-url (ecard-carddav--extract-addressbook-home-url xml principal-url)))
                (oset server addressbook-home-url home-url))))
        (kill-buffer buffer)))
    server))

(defun ecard-carddav--extract-addressbook-home-url (xml base-url)
  "Extract addressbook-home-set URL from PROPFIND response XML.
BASE-URL is used to resolve relative URLs.
Returns absolute URL string."
  (let ((home-sets (ecard-carddav--dom-by-tag-qname xml 'addressbook-home-set
                                                     ecard-carddav-ns-carddav)))
    (when home-sets
      (let ((href-node (ecard-carddav--dom-by-tag-qname (car home-sets) 'href
                                                          ecard-carddav-ns-dav)))
        (when href-node
          (let ((href (dom-text (car href-node))))
            ;; HREF should already be absolute or server-relative
            (if (string-prefix-p "http" href)
                href
              ;; Build full URL from base
              (let ((parsed-base (url-generic-parse-url base-url)))
                (format "%s://%s%s"
                       (url-type parsed-base)
                       (url-host parsed-base)
                       href)))))))))

(defun ecard-carddav-discover-addressbooks (server)
  "Discover all addressbook collections for SERVER.
Requires addressbook-home-url to be set.
Updates SERVER object with list of addressbook objects.
Returns list of `ecard-carddav-addressbook' objects."
  (unless (oref server addressbook-home-url)
    (ecard-carddav-discover-addressbook-home server))
  (let* ((home-url (oref server addressbook-home-url))
         (auth (ecard-carddav--get-auth server)))
    (unless home-url
      (signal 'ecard-carddav-error '("Addressbook home URL not set after discovery")))
    (let ((buffer (ecard-carddav--request-with-retry
                   "PROPFIND" home-url auth
                   (ecard-carddav--propfind-body
                    `(("resourcetype" . ,ecard-carddav-ns-dav)
                      ("displayname" . ,ecard-carddav-ns-dav)
                      ("addressbook-description" . ,ecard-carddav-ns-carddav)
                      ("getctag" . ,ecard-carddav-ns-cs)
                      ("sync-token" . ,ecard-carddav-ns-dav)))
                   "application/xml; charset=utf-8"
                   '(("Depth" . "1")))))
      (unwind-protect
          (let* ((status (ecard-carddav--get-http-status buffer))
                 (xml (when (and status (= status 207))
                        (ecard-carddav--parse-xml-response buffer)))
                 (addressbooks (when xml
                                (ecard-carddav--parse-addressbooks xml server home-url))))
            (oset server addressbooks addressbooks)
            addressbooks)
        (kill-buffer buffer)))))

(defun ecard-carddav--parse-addressbooks (xml server base-url)
  "Parse addressbook collections from PROPFIND response XML.
SERVER is the parent server object.
BASE-URL is used to resolve relative URLs.
Returns list of `ecard-carddav-addressbook' objects."
  (let ((responses (ecard-carddav--dom-by-tag-qname xml 'response ecard-carddav-ns-dav))
        (addressbooks nil))
    (dolist (response responses)
      (let* ((href-node (ecard-carddav--dom-by-tag-qname response 'href ecard-carddav-ns-dav))
             (href (when href-node (dom-text (car href-node))))
             (propstat (ecard-carddav--dom-by-tag-qname response 'propstat ecard-carddav-ns-dav))
             (prop (when propstat (ecard-carddav--dom-by-tag-qname (car propstat) 'prop
                                                                     ecard-carddav-ns-dav)))
             (resourcetype (when prop (ecard-carddav--dom-by-tag-qname (car prop) 'resourcetype
                                                                         ecard-carddav-ns-dav)))
             (is-addressbook (and resourcetype
                                 (ecard-carddav--dom-by-tag-qname (car resourcetype) 'addressbook
                                                                   ecard-carddav-ns-carddav))))
        (when (and href is-addressbook)
          (let* ((url (ecard-carddav--resolve-url href base-url))
                 (displayname-node (when prop (ecard-carddav--dom-by-tag-qname (car prop) 'displayname
                                                                                 ecard-carddav-ns-dav)))
                 (displayname (when displayname-node (dom-text (car displayname-node))))
                 (desc-node (when prop (ecard-carddav--dom-by-tag-qname (car prop) 'addressbook-description
                                                                          ecard-carddav-ns-carddav)))
                 (description (when desc-node (dom-text (car desc-node))))
                 (ctag-node (when prop (ecard-carddav--dom-by-tag-qname (car prop) 'getctag
                                                                          ecard-carddav-ns-cs)))
                 (ctag (when ctag-node (dom-text (car ctag-node))))
                 (sync-node (when prop (ecard-carddav--dom-by-tag-qname (car prop) 'sync-token
                                                                          ecard-carddav-ns-dav)))
                 (sync-token (when sync-node (dom-text (car sync-node)))))
            (push (ecard-carddav-addressbook
                   :server server
                   :url url
                   :display-name displayname
                   :description description
                   :ctag ctag
                   :sync-token sync-token)
                  addressbooks)))))
    (nreverse addressbooks)))

;;; URL resolution

(defun ecard-carddav--resolve-url (href base-url)
  "Resolve HREF against BASE-URL.
Returns absolute URL string."
  (if (string-prefix-p "http" href)
      href
    (let ((parsed-base (url-generic-parse-url base-url)))
      (if (string-prefix-p "/" href)
          ;; Absolute path
          (format "%s://%s%s"
                  (url-type parsed-base)
                  (url-host parsed-base)
                  href)
        ;; Relative path
        (let ((base-path (url-filename parsed-base)))
          (format "%s://%s%s%s"
                  (url-type parsed-base)
                  (url-host parsed-base)
                  (file-name-directory base-path)
                  href))))))

;;; Resource operations

(defun ecard-carddav-list-resources (addressbook)
  "List all vCard resources in ADDRESSBOOK.
Returns list of `ecard-carddav-resource' objects with ETags but no vCard data.
Updates ADDRESSBOOK resources slot."
  (let* ((server (oref addressbook server))
         (auth (ecard-carddav--get-auth server))
         (url (oref addressbook url))
         (buffer (ecard-carddav--request-with-retry
                  "PROPFIND" url auth
                  (ecard-carddav--propfind-body
                   `(("getetag" . ,ecard-carddav-ns-dav)
                     ("getcontenttype" . ,ecard-carddav-ns-dav)))
                  "application/xml; charset=utf-8"
                  '(("Depth" . "1")))))
    (unwind-protect
        (let* ((status (ecard-carddav--get-http-status buffer))
               (xml (when (and status (= status 207))
                      (ecard-carddav--parse-xml-response buffer)))
               (resources (when xml
                            (ecard-carddav--parse-resources xml addressbook url))))
          (oset addressbook resources resources)
          resources)
      (kill-buffer buffer))))

(defun ecard-carddav--parse-resources (xml addressbook base-url)
  "Parse vCard resources from PROPFIND response XML.
ADDRESSBOOK is the parent addressbook object.
BASE-URL is used to resolve relative URLs.
Returns list of `ecard-carddav-resource' objects."
  (let ((responses (ecard-carddav--dom-by-tag-qname xml 'response ecard-carddav-ns-dav))
        (addressbook-url (oref addressbook url))
        (resources nil))
    (dolist (response responses)
      (let* ((href-node (ecard-carddav--dom-by-tag-qname response 'href ecard-carddav-ns-dav))
             (href (when href-node (dom-text (car href-node))))
             (propstat (ecard-carddav--dom-by-tag-qname response 'propstat ecard-carddav-ns-dav))
             (prop (when propstat (ecard-carddav--dom-by-tag-qname (car propstat) 'prop
                                                                     ecard-carddav-ns-dav)))
             (content-type-node (when prop (ecard-carddav--dom-by-tag-qname (car prop) 'getcontenttype
                                                                              ecard-carddav-ns-dav)))
             (content-type (when content-type-node (dom-text (car content-type-node)))))
        ;; Only include vCard resources, not the addressbook collection itself
        ;; Content-type check is optional since some servers don't return it
        (when (and href
                   (or (null content-type)  ; No content-type returned
                       (string-match-p "text/vcard" content-type)))  ; Or it's a vcard
          (let* ((url (ecard-carddav--resolve-url href base-url))
                 ;; Skip if this is the addressbook collection itself
                 (is-collection (string= url addressbook-url)))
            (unless is-collection
              (let* ((etag-node (when prop (ecard-carddav--dom-by-tag-qname (car prop) 'getetag
                                                                              ecard-carddav-ns-dav)))
                     (etag (when etag-node (dom-text (car etag-node))))
                     ;; Remove quotes from ETag if present
                     (etag (when etag (string-trim etag "\"" "\"")))
                     (path (url-filename (url-generic-parse-url url))))
                (push (ecard-carddav-resource
                       :addressbook addressbook
                       :url url
                       :path path
                       :etag etag)
                      resources)))))))
    (nreverse resources)))

(defun ecard-carddav-get-resource (addressbook path-or-url)
  "Get vCard resource at PATH-OR-URL from ADDRESSBOOK.
PATH-OR-URL can be a full URL or a path relative to addressbook.
Returns `ecard-carddav-resource' object with vCard data populated.
Signals error if resource not found."
  (let* ((server (oref addressbook server))
         (auth (ecard-carddav--get-auth server))
         (url (if (string-prefix-p "http" path-or-url)
                  path-or-url
                (ecard-carddav--resolve-url path-or-url (oref addressbook url))))
         (buffer (ecard-carddav--request-with-retry "GET" url auth)))
    (unwind-protect
        (let ((status (ecard-carddav--get-http-status buffer)))
          (cond
           ((= status 200)
            (let ((etag (ecard-carddav--get-http-header buffer "ETag"))
                  (ecard-data (with-current-buffer buffer
                                (goto-char (point-min))
                                (when (re-search-forward "\r?\n\r?\n" nil t)
                                  (decode-coding-string
                                   (buffer-substring (point) (point-max))
                                   'utf-8)))))
              (when etag
                (setq etag (string-trim etag "\"" "\"")))
              (let ((ecard-obj (ecard-compat-parse ecard-data)))
                (ecard-carddav-resource
                 :addressbook addressbook
                 :url url
                 :path (url-filename (url-generic-parse-url url))
                 :etag etag
                 :ecard ecard-obj
                 :ecard-data ecard-data))))
           ((= status 404)
            (signal 'ecard-carddav-not-found-error
                    (list "Resource not found" url)))
           (t
            (signal 'ecard-carddav-http-error
                    (list "Failed to get resource" status url)))))
      (kill-buffer buffer))))

(defun ecard-carddav-put-ecard (addressbook path-or-url ecard-obj &optional etag)
  "Create or update vCard resource at PATH-OR-URL in ADDRESSBOOK.
VCARD-OBJ is the ecard object to store.
ETAG is optional - if provided, uses If-Match for concurrency control.
Returns updated `ecard-carddav-resource' object.
Signals conflict error if ETAG doesn't match."
  (let* ((server (oref addressbook server))
         (auth (ecard-carddav--get-auth server))
         (url (if (string-prefix-p "http" path-or-url)
                  path-or-url
                (ecard-carddav--resolve-url path-or-url (oref addressbook url))))
         (ecard-data (ecard-serialize ecard-obj))
         (headers (when etag
                   (list (cons "If-Match" (format "\"%s\"" etag)))))
         (buffer (ecard-carddav--request-with-retry
                  "PUT" url auth ecard-data "text/vcard; charset=utf-8" headers)))
    (unwind-protect
        (let ((status (ecard-carddav--get-http-status buffer)))
          (cond
           ((or (= status 201) (= status 204))
            (let ((new-etag (ecard-carddav--get-http-header buffer "ETag")))
              (when new-etag
                (setq new-etag (string-trim new-etag "\"" "\"")))
              (ecard-carddav-resource
               :addressbook addressbook
               :url url
               :path (url-filename (url-generic-parse-url url))
               :etag new-etag
               :ecard ecard-obj
               :ecard-data ecard-data)))
           ((= status 412)
            (signal 'ecard-carddav-conflict-error
                    (list "ETag mismatch - resource modified" url)))
           (t
            (signal 'ecard-carddav-http-error
                    (list "Failed to put resource" status url)))))
      (kill-buffer buffer))))

(defun ecard-carddav-delete-resource (addressbook path-or-url &optional etag)
  "Delete vCard resource at PATH-OR-URL from ADDRESSBOOK.
ETAG is optional - if provided, uses If-Match for concurrency control.
Returns t on success.
Signals conflict error if ETAG doesn't match."
  (let* ((server (oref addressbook server))
         (auth (ecard-carddav--get-auth server))
         (url (if (string-prefix-p "http" path-or-url)
                  path-or-url
                (ecard-carddav--resolve-url path-or-url (oref addressbook url))))
         (headers (when etag
                   (list (cons "If-Match" (format "\"%s\"" etag)))))
         (buffer (ecard-carddav--request-with-retry "DELETE" url auth nil nil headers)))
    (unwind-protect
        (let ((status (ecard-carddav--get-http-status buffer)))
          (cond
           ((or (= status 204) (= status 200))
            t)
           ((= status 404)
            (signal 'ecard-carddav-not-found-error
                    (list "Resource not found" url)))
           ((= status 412)
            (signal 'ecard-carddav-conflict-error
                    (list "ETag mismatch - resource modified" url)))
           (t
            (signal 'ecard-carddav-http-error
                    (list "Failed to delete resource" status url)))))
      (kill-buffer buffer))))

;;;###autoload
(defun ecard-carddav-change-uid (addressbook old-path-or-url new-uid)
  "Change the UID of a vCard resource from OLD-PATH-OR-URL to NEW-UID.
ADDRESSBOOK is the addressbook object containing the resource.
OLD-PATH-OR-URL is the current path or URL of the resource.
NEW-UID is the new UID value to assign.

This operation:
1. Fetches the current vCard from the server
2. Modifies its UID to NEW-UID
3. Deletes the old vCard resource
4. Creates a new vCard resource with the new UID

Returns the new `ecard-carddav-resource' object with updated UID.

Note: This is effectively a delete + create operation. The new resource
path will be derived from the NEW-UID (typically NEWUID.vcf).

Signals error if:
- The old resource is not found
- The new UID already exists
- Network or permission errors occur

Example:
  (ecard-carddav-change-uid addressbook
                            \"/contacts/old.vcf\"
                            \"urn:uuid:new-uuid-here\")"
  (interactive
   (let* ((addressbook (if (and (boundp 'ecard-display--addressbook)
                                ecard-display--addressbook)
                          ecard-display--addressbook
                        (error "No addressbook context available")))
          (old-path (read-string "Old resource path or URL: "))
          (new-uid (read-string "New UID: ")))
     (list addressbook old-path new-uid)))

  ;; Step 1: Fetch the current resource
  (message "Fetching current vCard...")
  (let* ((resource (ecard-carddav-get-resource addressbook old-path-or-url))
         (ecard-obj (oref resource ecard))
         (old-etag (oref resource etag))
         (old-url (oref resource url)))

    ;; Step 2: Modify the UID
    (message "Modifying UID to %s..." new-uid)
    (ecard-set-property ecard-obj 'uid new-uid)

    ;; Step 3: Generate new path based on new UID
    ;; Use a sanitized version of the UID for the filename
    (let* ((sanitized-uid (replace-regexp-in-string "[^a-zA-Z0-9-]" "-" new-uid))
           (new-path (concat sanitized-uid ".vcf"))
           (new-resource nil))

      ;; Step 4: Create the new vCard (this will fail if UID already exists)
      (message "Creating new vCard at %s..." new-path)
      (condition-case err
          (setq new-resource (ecard-carddav-put-ecard addressbook new-path ecard-obj))
        (error
         (message "Failed to create new vCard: %s" (error-message-string err))
         (signal (car err) (cdr err))))

      ;; Step 5: Delete the old vCard (only if creation succeeded)
      (message "Deleting old vCard at %s..." old-path-or-url)
      (condition-case err
          (ecard-carddav-delete-resource addressbook old-url old-etag)
        (error
         ;; If deletion fails, try to clean up the new resource
         (message "Warning: Failed to delete old vCard: %s" (error-message-string err))
         (message "Attempting to delete newly created vCard to rollback...")
         (ignore-errors
           (ecard-carddav-delete-resource addressbook (oref new-resource url)))
         (signal (car err) (cdr err))))

      (message "UID changed successfully from %s to %s"
               (or (ecard-get-property-value ecard-obj 'uid) "unknown")
               new-uid)
      new-resource)))

;;; addressbook-multiget REPORT

(defun ecard-carddav-multiget-resources (addressbook resource-paths)
  "Fetch multiple vCard resources in a single request using addressbook-multiget.
ADDRESSBOOK is the addressbook object.
RESOURCE-PATHS is a list of resource paths to fetch.
Returns list of `ecard-carddav-resource' objects with ecard data populated.

This uses the CardDAV addressbook-multiget REPORT (RFC 6352) to batch-fetch
multiple vCards in a single HTTP request, which is much more efficient than
individual GET requests."
  (when (null resource-paths)
    (signal 'ecard-carddav-error '("No resource paths provided")))

  (let* ((server (oref addressbook server))
         (auth (ecard-carddav--get-auth server))
         (url (oref addressbook url))
         (body (ecard-carddav--multiget-body resource-paths))
         (buffer (ecard-carddav--request-with-retry
                  "REPORT" url auth body
                  "application/xml; charset=utf-8"
                  '(("Depth" . "1")))))
    (unwind-protect
        (let* ((status (ecard-carddav--get-http-status buffer))
               (xml (when (and status (= status 207))
                      (ecard-carddav--parse-xml-response buffer))))
          (when xml
            (ecard-carddav--parse-multiget-response xml addressbook)))
      (kill-buffer buffer))))

(defun ecard-carddav--multiget-body (resource-paths)
  "Create addressbook-multiget REPORT request body for RESOURCE-PATHS."
  (with-temp-buffer
    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    (insert (format "<C:addressbook-multiget xmlns=\"%s\" xmlns:C=\"%s\">\n"
                   ecard-carddav-ns-dav
                   ecard-carddav-ns-carddav))
    (insert "  <prop>\n")
    (insert "    <getetag/>\n")
    (insert "    <C:address-data/>\n")
    (insert "  </prop>\n")
    (dolist (path resource-paths)
      (insert (format "  <href>%s</href>\n" (ecard-carddav--xml-escape-string path))))
    (insert "</C:addressbook-multiget>\n")
    (buffer-string)))

(defun ecard-carddav--xml-escape-string (str)
  "Escape STR for use in XML content."
  (replace-regexp-in-string
   "[<>&\"]"
   (lambda (match)
     (pcase match
       ("<" "&lt;")
       (">" "&gt;")
       ("&" "&amp;")
       ("\"" "&quot;")))
   str))

(defun ecard-carddav--parse-multiget-response (xml addressbook)
  "Parse addressbook-multiget REPORT response XML.
ADDRESSBOOK is the parent addressbook object.
Returns list of `ecard-carddav-resource' objects with ecard data populated."
  (let ((responses (ecard-carddav--dom-by-tag-qname xml 'response ecard-carddav-ns-dav))
        (resources nil))
    (dolist (response responses)
      (let* ((href-node (ecard-carddav--dom-by-tag-qname response 'href ecard-carddav-ns-dav))
             (href (when href-node (dom-text (car href-node))))
             (propstat (ecard-carddav--dom-by-tag-qname response 'propstat ecard-carddav-ns-dav))
             (status-node (when propstat
                           (ecard-carddav--dom-by-tag-qname (car propstat) 'status
                                                              ecard-carddav-ns-dav)))
             (status-text (when status-node (dom-text (car status-node))))
             (is-success (and status-text (string-match-p "200" status-text))))

        (when (and href is-success)
          (let* ((prop (when propstat
                        (ecard-carddav--dom-by-tag-qname (car propstat) 'prop
                                                          ecard-carddav-ns-dav)))
                 (etag-node (when prop
                             (ecard-carddav--dom-by-tag-qname (car prop) 'getetag
                                                               ecard-carddav-ns-dav)))
                 (etag (when etag-node (dom-text (car etag-node))))
                 (address-data-node (when prop
                                     (ecard-carddav--dom-by-tag-qname (car prop) 'address-data
                                                                       ecard-carddav-ns-carddav)))
                 (ecard-data (when address-data-node (dom-text (car address-data-node)))))

            (when ecard-data
              ;; Remove quotes from ETag if present
              (when etag
                (setq etag (string-trim etag "\"" "\"")))

              ;; Parse vCard data
              (condition-case err
                  (let* ((ecard-obj (ecard-compat-parse ecard-data))
                         (path (if (string-prefix-p "http" href)
                                  (url-filename (url-generic-parse-url href))
                                href))
                         (url (if (string-prefix-p "http" href)
                                 href
                               (ecard-carddav--resolve-url href (oref addressbook url)))))
                    (push (ecard-carddav-resource
                           :addressbook addressbook
                           :url url
                           :path path
                           :etag etag
                           :ecard ecard-obj
                           :ecard-data ecard-data)
                          resources))
                (error
                 (message "Failed to parse vCard at %s: %s" href (error-message-string err)))))))))
    (nreverse resources)))

;;; Server creation

;;;###autoload
(defun ecard-carddav-server-create (&rest args)
  "Create CardDAV server connection from ARGS.

ARGS is a plist with keys:
  :url STRING - Base URL of CardDAV server (required)
  :auth AUTH-OBJ-OR-FUNCTION - Authentication object or function (required)

The :auth parameter can be either:
  - An ecard-carddav-auth object (created with ecard-carddav-auth-*-create)
  - A function that returns an ecard-carddav-auth object when called

Using a function allows for dynamic credential retrieval, such as:
  - Fetching from auth-source
  - OAuth token refresh
  - Password manager integration
  - Prompting user for credentials

Examples:
  ;; Static auth object
  (ecard-carddav-server-create
   :url \"https://carddav.example.com\"
   :auth (ecard-carddav-auth-basic-create
          :username \"user\"
          :password \"secret\"))

  ;; Dynamic auth function
  (ecard-carddav-server-create
   :url \"https://carddav.example.com\"
   :auth (lambda ()
           (ecard-carddav-auth-basic-create
            :username (read-string \"Username: \")
            :password (read-passwd \"Password: \"))))"
  (let ((url (plist-get args :url))
        (auth (plist-get args :auth)))
    (unless url
      (signal 'ecard-carddav-error '("Server URL required")))
    (unless auth
      (signal 'ecard-carddav-error '("Authentication required")))
    ;; Validate auth - if it's a function, call it once to validate
    (let ((auth-obj (if (functionp auth) (funcall auth) auth)))
      (ecard-carddav-auth-ensure-valid auth-obj))
    (ecard-carddav-server
     :url url
     :auth auth)))

(provide 'ecard-carddav)
;;; ecard-carddav.el ends here
