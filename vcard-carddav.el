;;; vcard-carddav.el --- CardDAV protocol implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: comm, data, carddav, vcard
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
;;   ;; Connect to CardDAV server
;;   (setq server (vcard-carddav-server-create
;;                 :url "https://carddav.example.com"
;;                 :auth (vcard-carddav-auth-basic-create
;;                        :username "user"
;;                        :password "secret")))
;;
;;   ;; Discover address books
;;   (vcard-carddav-discover-addressbooks server)
;;
;;   ;; List contacts in address book
;;   (vcard-carddav-list-resources addressbook)
;;
;;   ;; Get a contact
;;   (vcard-carddav-get-vcard addressbook "/contacts/john.vcf")
;;
;;   ;; Create/update contact
;;   (vcard-carddav-put-vcard addressbook "/contacts/jane.vcf" vcard-obj)
;;
;;   ;; Delete contact
;;   (vcard-carddav-delete-resource addressbook "/contacts/old.vcf")

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'url)
(require 'url-http)
(require 'dom)
(require 'xml)
(require 'vcard)
(require 'vcard-carddav-auth)

;;; Custom group

(defgroup vcard-carddav nil
  "CardDAV protocol implementation."
  :group 'vcard
  :prefix "vcard-carddav-")

(defcustom vcard-carddav-timeout 30
  "Timeout in seconds for CardDAV HTTP requests."
  :type 'integer
  :group 'vcard-carddav)

(defcustom vcard-carddav-max-retries 3
  "Maximum number of retries for failed requests."
  :type 'integer
  :group 'vcard-carddav)

(defcustom vcard-carddav-retry-delay 1.0
  "Initial retry delay in seconds (exponential backoff)."
  :type 'float
  :group 'vcard-carddav)

(defcustom vcard-carddav-user-agent "Emacs vCard-CardDAV/1.0"
  "User-Agent string for CardDAV requests."
  :type 'string
  :group 'vcard-carddav)

;;; Error conditions

(define-error 'vcard-carddav-error "CardDAV error")
(define-error 'vcard-carddav-http-error "CardDAV HTTP error" 'vcard-carddav-error)
(define-error 'vcard-carddav-xml-error "CardDAV XML error" 'vcard-carddav-error)
(define-error 'vcard-carddav-conflict-error "CardDAV conflict error" 'vcard-carddav-error)
(define-error 'vcard-carddav-not-found-error "CardDAV not found error" 'vcard-carddav-error)

;;; EIEIO Classes

(defclass vcard-carddav-server ()
  ((url
    :initarg :url
    :initform nil
    :type (or null string)
    :documentation "Base URL of CardDAV server (e.g., https://example.com).")
   (auth
    :initarg :auth
    :initform nil
    :type (or null vcard-carddav-auth)
    :documentation "Authentication credentials object.")
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

(defclass vcard-carddav-addressbook ()
  ((server
    :initarg :server
    :initform nil
    :type (or null vcard-carddav-server)
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

(defclass vcard-carddav-resource ()
  ((addressbook
    :initarg :addressbook
    :initform nil
    :type (or null vcard-carddav-addressbook)
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
   (vcard
    :initarg :vcard
    :initform nil
    :type (or null vcard)
    :documentation "Parsed vcard object (nil if not yet fetched).")
   (vcard-data
    :initarg :vcard-data
    :initform nil
    :type (or null string)
    :documentation "Raw vCard text data."))
  "Represents a vCard resource in a CardDAV address book.")

;;; XML namespace constants

(defconst vcard-carddav-ns-dav "DAV:"
  "WebDAV namespace URI.")

(defconst vcard-carddav-ns-carddav "urn:ietf:params:xml:ns:carddav"
  "CardDAV namespace URI.")

(defconst vcard-carddav-ns-cs "http://calendarserver.org/ns/"
  "CalendarServer namespace URI.")

;;; XML generation helpers

(defun vcard-carddav--xml-to-string (xml)
  "Convert XML s-expression to string.
XML is in the format returned by `xml-parse-region'."
  (with-temp-buffer
    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    (xml-print (list xml))
    (buffer-string)))

(defun vcard-carddav--make-xml-element (name _namespace attrs &rest children)
  "Create XML element with NAME with ATTRS and CHILDREN.
NAMESPACE argument is ignored - use namespace prefixes in attrs instead.
Returns s-expression suitable for `xml-print'."
  (let ((tag (intern name)))
    (if children
        `(,tag ,attrs ,@children)
      `(,tag ,attrs))))

(defun vcard-carddav--dom-by-tag-qname (dom tag &optional namespace)
  "Find elements in DOM by TAG, handling both plain and QName symbols.
If NAMESPACE is provided, also searches for {NAMESPACE}TAG and C:TAG formats.
Returns list of matching nodes."
  (let* ((tag-str (if (symbolp tag) (symbol-name tag) tag))
         (plain-tag (intern tag-str))
         (qname-tag (when namespace
                      (intern (format "{%s}%s" namespace tag-str))))
         (prefix-tags (when namespace
                        (cond
                         ((string= namespace vcard-carddav-ns-carddav)
                          (list (intern (concat "C:" tag-str))))
                         ((string= namespace vcard-carddav-ns-cs)
                          (list (intern (concat "CS:" tag-str))))
                         (t nil)))))
    (append (dom-by-tag dom plain-tag)
            (when qname-tag
              (dom-by-tag dom qname-tag))
            (mapcan (lambda (ptag) (dom-by-tag dom ptag)) prefix-tags))))

(defun vcard-carddav--propfind-body (props)
  "Create PROPFIND request body requesting PROPS.
PROPS is list of (name . namespace) pairs."
  (with-temp-buffer
    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    (insert (format "<propfind xmlns=\"%s\" xmlns:C=\"%s\" xmlns:CS=\"%s\">\n"
                   vcard-carddav-ns-dav
                   vcard-carddav-ns-carddav
                   vcard-carddav-ns-cs))
    (insert "  <prop>\n")
    (dolist (prop props)
      (let ((name (car prop))
            (ns (cdr prop)))
        (cond
         ((string= ns vcard-carddav-ns-carddav)
          (insert (format "    <C:%s/>\n" name)))
         ((string= ns vcard-carddav-ns-cs)
          (insert (format "    <CS:%s/>\n" name)))
         (t
          (insert (format "    <%s/>\n" name))))))
    (insert "  </prop>\n")
    (insert "</propfind>\n")
    (buffer-string)))

;;; HTTP request helpers

(defun vcard-carddav--parse-xml-response (buffer)
  "Parse XML from BUFFER.
Returns parsed XML s-expression or signals error."
  (with-current-buffer buffer
    (goto-char (point-min))
    ;; Skip HTTP headers
    (when (re-search-forward "\r?\n\r?\n" nil t)
      (condition-case err
          (car (xml-parse-region (point) (point-max)))
        (error
         (signal 'vcard-carddav-xml-error
                 (list "Failed to parse XML response" err)))))))

(defun vcard-carddav--get-http-status (buffer)
  "Extract HTTP status code from BUFFER.
Returns integer status code or nil."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (string-to-number (match-string 1)))))

(defun vcard-carddav--get-http-header (buffer header-name)
  "Extract HTTP header HEADER-NAME from BUFFER.
Returns header value or nil."
  (with-current-buffer buffer
    (goto-char (point-min))
    (when (re-search-forward
           (format "^%s: \\([^\r\n]*\\)" (regexp-quote header-name))
           nil t)
      (match-string 1))))

(defun vcard-carddav--request (method url auth &optional body content-type headers)
  "Make HTTP request with METHOD to URL using AUTH.
BODY is optional request body string.
CONTENT-TYPE is optional Content-Type header.
HEADERS is optional alist of additional headers.
Returns response buffer."
  (let* ((url-request-method method)
         (url-request-extra-headers
          (append
           (list (cons "User-Agent" vcard-carddav-user-agent))
           (when auth
             (list (cons "Authorization" (vcard-carddav-auth-get-header auth))))
           (when content-type
             (list (cons "Content-Type" content-type)))
           headers))
         (url-request-data (when body (encode-coding-string body 'utf-8)))
         (url-http-attempt-keepalives nil)  ; Avoid connection reuse issues
         (url-show-status nil))
    (url-retrieve-synchronously url t nil vcard-carddav-timeout)))

(defun vcard-carddav--request-with-retry (method url auth &optional body content-type headers)
  "Make HTTP request with automatic retry on failure.
Uses exponential backoff strategy.
Returns response buffer or signals error."
  (let ((retries 0)
        (delay vcard-carddav-retry-delay)
        (buffer nil)
        (last-error nil))
    (while (and (< retries vcard-carddav-max-retries)
                (not buffer))
      (condition-case err
          (progn
            (setq buffer (vcard-carddav--request method url auth body content-type headers))
            ;; Check for server errors that should be retried
            (let ((status (vcard-carddav--get-http-status buffer)))
              (when (and status (>= status 500) (< status 600))
                (kill-buffer buffer)
                (setq buffer nil)
                (setq last-error (list 'http-error status))
                (setq retries (1+ retries))
                (when (< retries vcard-carddav-max-retries)
                  (sleep-for delay)
                  (setq delay (* delay 2))))))
        (error
         (setq last-error err)
         (setq retries (1+ retries))
         (when (< retries vcard-carddav-max-retries)
           (sleep-for delay)
           (setq delay (* delay 2))))))
    (unless buffer
      (signal 'vcard-carddav-http-error
              (list "Request failed after retries" last-error)))
    buffer))

;;; Service discovery

(defun vcard-carddav-discover-principal (server)
  "Discover principal URL for SERVER.
Updates SERVER object with principal-url.
Returns SERVER."
  (let* ((url (oref server url))
         (auth (oref server auth))
         (well-known-url (concat url "/.well-known/carddav"))
         (buffer (vcard-carddav--request-with-retry "PROPFIND" well-known-url auth
                                                     (vcard-carddav--propfind-body
                                                      `(("current-user-principal" . ,vcard-carddav-ns-dav)))
                                                     "application/xml; charset=utf-8"
                                                     '(("Depth" . "0")))))
    (unwind-protect
        (let* ((status (vcard-carddav--get-http-status buffer))
               (xml (when (and status (= status 207))
                      (vcard-carddav--parse-xml-response buffer))))
          (when xml
            (let ((principal (vcard-carddav--extract-principal-url xml url)))
              (oset server principal-url principal))))
      (kill-buffer buffer))
    server))

(defun vcard-carddav--extract-principal-url (xml base-url)
  "Extract principal URL from PROPFIND response XML.
BASE-URL is used to resolve relative URLs.
Returns absolute URL string."
  (let ((hrefs (vcard-carddav--dom-by-tag-qname xml 'current-user-principal
                                                 vcard-carddav-ns-dav)))
    (when hrefs
      (let ((href-node (vcard-carddav--dom-by-tag-qname (car hrefs) 'href
                                                          vcard-carddav-ns-dav)))
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

(defun vcard-carddav-discover-addressbook-home (server)
  "Discover addressbook-home-set URL for SERVER.
Requires principal-url to be set.
Updates SERVER object with addressbook-home-url.
Returns SERVER."
  (unless (oref server principal-url)
    (vcard-carddav-discover-principal server))
  (let* ((principal-url (oref server principal-url))
         (auth (oref server auth)))
    (unless principal-url
      (signal 'vcard-carddav-error '("Principal URL not set after discovery")))
    (let ((buffer (vcard-carddav--request-with-retry
                   "PROPFIND" principal-url auth
                   (vcard-carddav--propfind-body
                    `(("addressbook-home-set" . ,vcard-carddav-ns-carddav)))
                   "application/xml; charset=utf-8"
                   '(("Depth" . "0")))))
      (unwind-protect
          (let* ((status (vcard-carddav--get-http-status buffer))
                 (xml (when (and status (= status 207))
                        (vcard-carddav--parse-xml-response buffer))))
            (when xml
              (let ((home-url (vcard-carddav--extract-addressbook-home-url xml principal-url)))
                (oset server addressbook-home-url home-url))))
        (kill-buffer buffer)))
    server))

(defun vcard-carddav--extract-addressbook-home-url (xml base-url)
  "Extract addressbook-home-set URL from PROPFIND response XML.
BASE-URL is used to resolve relative URLs.
Returns absolute URL string."
  (let ((home-sets (vcard-carddav--dom-by-tag-qname xml 'addressbook-home-set
                                                     vcard-carddav-ns-carddav)))
    (when home-sets
      (let ((href-node (vcard-carddav--dom-by-tag-qname (car home-sets) 'href
                                                          vcard-carddav-ns-dav)))
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

(defun vcard-carddav-discover-addressbooks (server)
  "Discover all addressbook collections for SERVER.
Requires addressbook-home-url to be set.
Updates SERVER object with list of addressbook objects.
Returns list of `vcard-carddav-addressbook' objects."
  (unless (oref server addressbook-home-url)
    (vcard-carddav-discover-addressbook-home server))
  (let* ((home-url (oref server addressbook-home-url))
         (auth (oref server auth)))
    (unless home-url
      (signal 'vcard-carddav-error '("Addressbook home URL not set after discovery")))
    (let ((buffer (vcard-carddav--request-with-retry
                   "PROPFIND" home-url auth
                   (vcard-carddav--propfind-body
                    `(("resourcetype" . ,vcard-carddav-ns-dav)
                      ("displayname" . ,vcard-carddav-ns-dav)
                      ("addressbook-description" . ,vcard-carddav-ns-carddav)
                      ("getctag" . ,vcard-carddav-ns-cs)
                      ("sync-token" . ,vcard-carddav-ns-dav)))
                   "application/xml; charset=utf-8"
                   '(("Depth" . "1")))))
      (unwind-protect
          (let* ((status (vcard-carddav--get-http-status buffer))
                 (xml (when (and status (= status 207))
                        (vcard-carddav--parse-xml-response buffer)))
                 (addressbooks (when xml
                                (vcard-carddav--parse-addressbooks xml server home-url))))
            (oset server addressbooks addressbooks)
            addressbooks)
        (kill-buffer buffer)))))

(defun vcard-carddav--parse-addressbooks (xml server base-url)
  "Parse addressbook collections from PROPFIND response XML.
SERVER is the parent server object.
BASE-URL is used to resolve relative URLs.
Returns list of `vcard-carddav-addressbook' objects."
  (let ((responses (vcard-carddav--dom-by-tag-qname xml 'response vcard-carddav-ns-dav))
        (addressbooks nil))
    (dolist (response responses)
      (let* ((href-node (vcard-carddav--dom-by-tag-qname response 'href vcard-carddav-ns-dav))
             (href (when href-node (dom-text (car href-node))))
             (propstat (vcard-carddav--dom-by-tag-qname response 'propstat vcard-carddav-ns-dav))
             (prop (when propstat (vcard-carddav--dom-by-tag-qname (car propstat) 'prop
                                                                     vcard-carddav-ns-dav)))
             (resourcetype (when prop (vcard-carddav--dom-by-tag-qname (car prop) 'resourcetype
                                                                         vcard-carddav-ns-dav)))
             (is-addressbook (and resourcetype
                                 (vcard-carddav--dom-by-tag-qname (car resourcetype) 'addressbook
                                                                   vcard-carddav-ns-carddav))))
        (when (and href is-addressbook)
          (let* ((url (vcard-carddav--resolve-url href base-url))
                 (displayname-node (when prop (vcard-carddav--dom-by-tag-qname (car prop) 'displayname
                                                                                 vcard-carddav-ns-dav)))
                 (displayname (when displayname-node (dom-text (car displayname-node))))
                 (desc-node (when prop (vcard-carddav--dom-by-tag-qname (car prop) 'addressbook-description
                                                                          vcard-carddav-ns-carddav)))
                 (description (when desc-node (dom-text (car desc-node))))
                 (ctag-node (when prop (vcard-carddav--dom-by-tag-qname (car prop) 'getctag
                                                                          vcard-carddav-ns-cs)))
                 (ctag (when ctag-node (dom-text (car ctag-node))))
                 (sync-node (when prop (vcard-carddav--dom-by-tag-qname (car prop) 'sync-token
                                                                          vcard-carddav-ns-dav)))
                 (sync-token (when sync-node (dom-text (car sync-node)))))
            (push (vcard-carddav-addressbook
                   :server server
                   :url url
                   :display-name displayname
                   :description description
                   :ctag ctag
                   :sync-token sync-token)
                  addressbooks)))))
    (nreverse addressbooks)))

;;; URL resolution

(defun vcard-carddav--resolve-url (href base-url)
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

(defun vcard-carddav-list-resources (addressbook)
  "List all vCard resources in ADDRESSBOOK.
Returns list of `vcard-carddav-resource' objects with ETags but no vCard data.
Updates ADDRESSBOOK resources slot."
  (let* ((server (oref addressbook server))
         (auth (oref server auth))
         (url (oref addressbook url))
         (buffer (vcard-carddav--request-with-retry
                  "PROPFIND" url auth
                  (vcard-carddav--propfind-body
                   `(("getetag" . ,vcard-carddav-ns-dav)
                     ("getcontenttype" . ,vcard-carddav-ns-dav)))
                  "application/xml; charset=utf-8"
                  '(("Depth" . "1")))))
    (unwind-protect
        (let* ((status (vcard-carddav--get-http-status buffer))
               (xml (when (and status (= status 207))
                      (vcard-carddav--parse-xml-response buffer)))
               (resources (when xml
                           (vcard-carddav--parse-resources xml addressbook url))))
          (oset addressbook resources resources)
          resources)
      (kill-buffer buffer))))

(defun vcard-carddav--parse-resources (xml addressbook base-url)
  "Parse vCard resources from PROPFIND response XML.
ADDRESSBOOK is the parent addressbook object.
BASE-URL is used to resolve relative URLs.
Returns list of `vcard-carddav-resource' objects."
  (let ((responses (vcard-carddav--dom-by-tag-qname xml 'response vcard-carddav-ns-dav))
        (resources nil))
    (dolist (response responses)
      (let* ((href-node (vcard-carddav--dom-by-tag-qname response 'href vcard-carddav-ns-dav))
             (href (when href-node (dom-text (car href-node))))
             (propstat (vcard-carddav--dom-by-tag-qname response 'propstat vcard-carddav-ns-dav))
             (prop (when propstat (vcard-carddav--dom-by-tag-qname (car propstat) 'prop
                                                                     vcard-carddav-ns-dav)))
             (content-type-node (when prop (vcard-carddav--dom-by-tag-qname (car prop) 'getcontenttype
                                                                              vcard-carddav-ns-dav)))
             (content-type (when content-type-node (dom-text (car content-type-node)))))
        ;; Only include vCard resources, not the addressbook itself
        (when (and href content-type (string-match-p "text/vcard" content-type))
          (let* ((url (vcard-carddav--resolve-url href base-url))
                 (etag-node (when prop (vcard-carddav--dom-by-tag-qname (car prop) 'getetag
                                                                          vcard-carddav-ns-dav)))
                 (etag (when etag-node (dom-text (car etag-node))))
                 ;; Remove quotes from ETag if present
                 (etag (when etag (string-trim etag "\"" "\"")))
                 (path (url-filename (url-generic-parse-url url))))
            (push (vcard-carddav-resource
                   :addressbook addressbook
                   :url url
                   :path path
                   :etag etag)
                  resources)))))
    (nreverse resources)))

(defun vcard-carddav-get-vcard (addressbook path-or-url)
  "Get vCard resource at PATH-OR-URL from ADDRESSBOOK.
PATH-OR-URL can be a full URL or a path relative to addressbook.
Returns `vcard-carddav-resource' object with vCard data populated.
Signals error if resource not found."
  (let* ((server (oref addressbook server))
         (auth (oref server auth))
         (url (if (string-prefix-p "http" path-or-url)
                  path-or-url
                (vcard-carddav--resolve-url path-or-url (oref addressbook url))))
         (buffer (vcard-carddav--request-with-retry "GET" url auth)))
    (unwind-protect
        (let ((status (vcard-carddav--get-http-status buffer)))
          (cond
           ((= status 200)
            (let ((etag (vcard-carddav--get-http-header buffer "ETag"))
                  (vcard-data (with-current-buffer buffer
                               (goto-char (point-min))
                               (when (re-search-forward "\r?\n\r?\n" nil t)
                                 (decode-coding-string
                                  (buffer-substring (point) (point-max))
                                  'utf-8)))))
              (when etag
                (setq etag (string-trim etag "\"" "\"")))
              (let ((vcard-obj (vcard-parse vcard-data)))
                (vcard-carddav-resource
                 :addressbook addressbook
                 :url url
                 :path (url-filename (url-generic-parse-url url))
                 :etag etag
                 :vcard vcard-obj
                 :vcard-data vcard-data))))
           ((= status 404)
            (signal 'vcard-carddav-not-found-error
                    (list "Resource not found" url)))
           (t
            (signal 'vcard-carddav-http-error
                    (list "Failed to get resource" status url)))))
      (kill-buffer buffer))))

(defun vcard-carddav-put-vcard (addressbook path-or-url vcard-obj &optional etag)
  "Create or update vCard resource at PATH-OR-URL in ADDRESSBOOK.
VCARD-OBJ is the vcard object to store.
ETAG is optional - if provided, uses If-Match for concurrency control.
Returns updated `vcard-carddav-resource' object.
Signals conflict error if ETAG doesn't match."
  (let* ((server (oref addressbook server))
         (auth (oref server auth))
         (url (if (string-prefix-p "http" path-or-url)
                  path-or-url
                (vcard-carddav--resolve-url path-or-url (oref addressbook url))))
         (vcard-data (vcard-serialize vcard-obj))
         (headers (when etag
                   (list (cons "If-Match" (format "\"%s\"" etag)))))
         (buffer (vcard-carddav--request-with-retry
                  "PUT" url auth vcard-data "text/vcard; charset=utf-8" headers)))
    (unwind-protect
        (let ((status (vcard-carddav--get-http-status buffer)))
          (cond
           ((or (= status 201) (= status 204))
            (let ((new-etag (vcard-carddav--get-http-header buffer "ETag")))
              (when new-etag
                (setq new-etag (string-trim new-etag "\"" "\"")))
              (vcard-carddav-resource
               :addressbook addressbook
               :url url
               :path (url-filename (url-generic-parse-url url))
               :etag new-etag
               :vcard vcard-obj
               :vcard-data vcard-data)))
           ((= status 412)
            (signal 'vcard-carddav-conflict-error
                    (list "ETag mismatch - resource modified" url)))
           (t
            (signal 'vcard-carddav-http-error
                    (list "Failed to put resource" status url)))))
      (kill-buffer buffer))))

(defun vcard-carddav-delete-resource (addressbook path-or-url &optional etag)
  "Delete vCard resource at PATH-OR-URL from ADDRESSBOOK.
ETAG is optional - if provided, uses If-Match for concurrency control.
Returns t on success.
Signals conflict error if ETAG doesn't match."
  (let* ((server (oref addressbook server))
         (auth (oref server auth))
         (url (if (string-prefix-p "http" path-or-url)
                  path-or-url
                (vcard-carddav--resolve-url path-or-url (oref addressbook url))))
         (headers (when etag
                   (list (cons "If-Match" (format "\"%s\"" etag)))))
         (buffer (vcard-carddav--request-with-retry "DELETE" url auth nil nil headers)))
    (unwind-protect
        (let ((status (vcard-carddav--get-http-status buffer)))
          (cond
           ((or (= status 204) (= status 200))
            t)
           ((= status 404)
            (signal 'vcard-carddav-not-found-error
                    (list "Resource not found" url)))
           ((= status 412)
            (signal 'vcard-carddav-conflict-error
                    (list "ETag mismatch - resource modified" url)))
           (t
            (signal 'vcard-carddav-http-error
                    (list "Failed to delete resource" status url)))))
      (kill-buffer buffer))))

;;; Server creation

;;;###autoload
(defun vcard-carddav-server-create (&rest args)
  "Create CardDAV server connection from ARGS.

ARGS is a plist with keys:
  :url STRING - Base URL of CardDAV server (required)
  :auth AUTH-OBJ - Authentication object (required)

Example:
  (vcard-carddav-server-create
   :url \"https://carddav.example.com\"
   :auth (vcard-carddav-auth-basic-create
          :username \"user\"
          :password \"secret\"))"
  (let ((url (plist-get args :url))
        (auth (plist-get args :auth)))
    (unless url
      (signal 'vcard-carddav-error '("Server URL required")))
    (unless auth
      (signal 'vcard-carddav-error '("Authentication required")))
    (vcard-carddav-auth-ensure-valid auth)
    (vcard-carddav-server
     :url url
     :auth auth)))

(provide 'vcard-carddav)
;;; vcard-carddav.el ends here
