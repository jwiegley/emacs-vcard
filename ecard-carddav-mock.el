;;; ecard-carddav-mock.el --- Mock CardDAV server for testing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: comm, data, carddav, testing
;; URL: https://github.com/jwiegley/dot-emacs

;;; Commentary:

;; Mock CardDAV server implementation for testing purposes.
;; Simulates Radicale server behavior.
;;
;; This module provides:
;; - In-memory CardDAV server simulation
;; - Support for all CardDAV operations
;; - ETag generation and validation
;; - Sync-token support
;; - CTag tracking
;; - 207 Multi-Status responses
;; - Proper namespace handling
;;
;; The mock server intercepts url.el requests and returns
;; appropriate responses without making actual HTTP requests.
;;
;; Example usage:
;;
;;   ;; Create mock server
;;   (setq mock (ecard-carddav-mock-server-create
;;               :base-url "https://test.example.com"))
;;
;;   ;; Add an address book
;;   (ecard-carddav-mock-add-addressbook
;;    mock "/addressbooks/user/contacts/"
;;    "Contacts" "My contacts")
;;
;;   ;; Add a vCard
;;   (ecard-carddav-mock-put-ecard
;;    mock "/addressbooks/user/contacts/john.vcf"
;;    ecard-obj)
;;
;;   ;; Install mock URL handler
;;   (ecard-carddav-mock-install mock)
;;
;;   ;; Run tests...
;;
;;   ;; Uninstall mock handler
;;   (ecard-carddav-mock-uninstall)

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'url)
(require 'ecard)

;;; Custom group

(defgroup ecard-carddav-mock nil
  "Mock CardDAV server for testing."
  :group 'ecard-carddav
  :prefix "ecard-carddav-mock-")

;;; EIEIO Classes

(defclass ecard-carddav-mock-server ()
  ((base-url
    :initarg :base-url
    :initform "https://mock.example.com"
    :type string
    :documentation "Base URL for this mock server.")
   (principal-path
    :initarg :principal-path
    :initform "/principals/user/"
    :type string
    :documentation "Principal path for current user.")
   (addressbook-home-path
    :initarg :addressbook-home-path
    :initform "/addressbooks/user/"
    :type string
    :documentation "Address book home path.")
   (addressbooks
    :initarg :addressbooks
    :initform (make-hash-table :test 'equal)
    :type hash-table
    :documentation "Hash table: path -> mock-addressbook object.")
   (next-etag
    :initarg :next-etag
    :initform 1
    :type integer
    :documentation "Next ETag number to assign.")
   (next-sync-token
    :initarg :next-sync-token
    :initform 1
    :type integer
    :documentation "Next sync-token number to assign."))
  "Mock CardDAV server for testing.")

(defclass ecard-carddav-mock-addressbook ()
  ((path
    :initarg :path
    :initform nil
    :type (or null string)
    :documentation "Path to this address book.")
   (display-name
    :initarg :display-name
    :initform "Mock Address Book"
    :type string
    :documentation "Display name for this address book.")
   (description
    :initarg :description
    :initform "Mock address book for testing"
    :type string
    :documentation "Description of this address book.")
   (ctag
    :initarg :ctag
    :initform "1"
    :type string
    :documentation "Current CTag.")
   (sync-token
    :initarg :sync-token
    :initform "1"
    :type string
    :documentation "Current sync-token.")
   (resources
    :initarg :resources
    :initform (make-hash-table :test 'equal)
    :type hash-table
    :documentation "Hash table: path -> mock-resource object."))
  "Mock address book collection.")

(defclass ecard-carddav-mock-resource ()
  ((path
    :initarg :path
    :initform nil
    :type (or null string)
    :documentation "Path to this resource.")
   (etag
    :initarg :etag
    :initform nil
    :type (or null string)
    :documentation "Current ETag.")
   (ecard
    :initarg :ecard
    :initform nil
    :type (or null ecard)
    :documentation "vCard object.")
   (ecard-data
    :initarg :ecard-data
    :initform nil
    :type (or null string)
    :documentation "Serialized vCard data."))
  "Mock vCard resource.")

;;; Global state

(defvar ecard-carddav-mock--active-server nil
  "Currently active mock server, or nil if not mocking.")

(defvar ecard-carddav-mock--original-retrieve nil
  "Original `url-retrieve-synchronously' function.")

;;; XML helpers

(defun ecard-carddav-mock--xml-escape (text)
  "Escape TEXT for inclusion in XML."
  (when text
    (replace-regexp-in-string
     "[<>&\"]"
     (lambda (match)
       (pcase match
         ("<" "&lt;")
         (">" "&gt;")
         ("&" "&amp;")
         ("\"" "&quot;")))
     text)))

(defun ecard-carddav-mock--xml-to-string (xml)
  "Convert XML s-expression to string."
  (with-temp-buffer
    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    (xml-print (list xml))
    (buffer-string)))

(defun ecard-carddav-mock--make-multistatus (responses)
  "Create 207 Multi-Status response with RESPONSES list."
  (ecard-carddav-mock--xml-to-string
   `(multistatus ((xmlns . "DAV:")
                  (xmlns:C . "urn:ietf:params:xml:ns:carddav")
                  (xmlns:CS . "http://calendarserver.org/ns/"))
                 ,@responses)))

(defun ecard-carddav-mock--make-response (href &rest propstats)
  "Create response element with HREF and PROPSTATS."
  `(response nil
             (href nil ,href)
             ,@propstats))

(defun ecard-carddav-mock--make-propstat (props &optional status)
  "Create propstat element with PROPS and STATUS."
  (let ((status (or status "HTTP/1.1 200 OK")))
    `(propstat nil
               (prop nil ,@props)
               (status nil ,status))))

;;; Request parsing

(defun ecard-carddav-mock--parse-request-body (body)
  "Parse XML request BODY.
Returns parsed XML s-expression."
  (when body
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (car (xml-parse-region (point-min) (point-max))))))

(defun ecard-carddav-mock--extract-prop-names (xml)
  "Extract requested property names from PROPFIND XML."
  (let ((prop-nodes (dom-by-tag xml 'prop)))
    (when prop-nodes
      (let ((prop-node (car prop-nodes))
            (names nil))
        (dolist (child (dom-children prop-node))
          (when (listp child)
            (push (dom-tag child) names)))
        (nreverse names)))))

;;; Mock request handlers

(defun ecard-carddav-mock--handle-options (_mock _path)
  "Handle OPTIONS request for MOCK server at PATH."
  (list :status 200
        :headers '(("DAV" . "1, 2, 3, addressbook")
                   ("Allow" . "OPTIONS, GET, HEAD, POST, PUT, DELETE, PROPFIND, REPORT"))
        :body ""))

(defun ecard-carddav-mock--handle-propfind (mock path depth body)
  "Handle PROPFIND request for MOCK server at PATH with DEPTH and BODY."
  (let ((_xml (ecard-carddav-mock--parse-request-body body))
        (_prop-names (when body
                       (ecard-carddav-mock--extract-prop-names
                        (ecard-carddav-mock--parse-request-body body)))))
    (cond
     ;; Principal discovery
     ((string-match-p "/.well-known/carddav" path)
      (ecard-carddav-mock--propfind-principal mock))

     ;; Principal properties
     ((string= path (oref mock principal-path))
      (ecard-carddav-mock--propfind-principal-props mock))

     ;; Address book home
     ((string= path (oref mock addressbook-home-path))
      (ecard-carddav-mock--propfind-addressbook-home mock depth))

     ;; Specific address book
     (t
      (ecard-carddav-mock--propfind-addressbook mock path depth)))))

(defun ecard-carddav-mock--propfind-principal (mock)
  "Handle principal discovery PROPFIND for MOCK server."
  (let* ((base-url (oref mock base-url))
         (principal-path (oref mock principal-path))
         (principal-url (concat base-url principal-path))
         (response (ecard-carddav-mock--make-response
                    "/.well-known/carddav"
                    (ecard-carddav-mock--make-propstat
                     `((current-user-principal nil
                        (href nil ,principal-url)))))))
    (list :status 207
          :headers '(("Content-Type" . "application/xml; charset=utf-8"))
          :body (ecard-carddav-mock--make-multistatus (list response)))))

(defun ecard-carddav-mock--propfind-principal-props (mock)
  "Handle principal properties PROPFIND for MOCK server."
  (let* ((base-url (oref mock base-url))
         (home-path (oref mock addressbook-home-path))
         (home-url (concat base-url home-path))
         (principal-url (concat base-url (oref mock principal-path)))
         (response (ecard-carddav-mock--make-response
                    principal-url
                    (ecard-carddav-mock--make-propstat
                     `((C:addressbook-home-set nil
                        (href nil ,home-url)))))))
    (list :status 207
          :headers '(("Content-Type" . "application/xml; charset=utf-8"))
          :body (ecard-carddav-mock--make-multistatus (list response)))))

(defun ecard-carddav-mock--propfind-addressbook-home (mock depth)
  "Handle address book home PROPFIND for MOCK server with DEPTH."
  (let ((base-url (oref mock base-url))
        (home-path (oref mock addressbook-home-path)))
    (if (string= depth "0")
        ;; Just the home collection itself
        (let ((_home-url (concat base-url home-path))
              (response (ecard-carddav-mock--make-response
                         (concat base-url home-path)
                         (ecard-carddav-mock--make-propstat
                          '((resourcetype nil (collection nil)))))))
          (list :status 207
                :headers '(("Content-Type" . "application/xml; charset=utf-8"))
                :body (ecard-carddav-mock--make-multistatus response)))

      ;; List all address books
      (let ((responses nil))
        (maphash (lambda (path addressbook)
                   (push (ecard-carddav-mock--make-response
                          (concat base-url path)
                          (ecard-carddav-mock--make-propstat
                           `((resourcetype nil
                                           (collection nil)
                                           (C:addressbook nil))
                             (displayname nil ,(oref addressbook display-name))
                             (C:addressbook-description nil ,(oref addressbook description))
                             (CS:getctag nil ,(oref addressbook ctag))
                             (sync-token nil ,(format "http://mock.example.com/ns/sync/%s"
                                                      (oref addressbook sync-token))))))
                         responses))
                 (oref mock addressbooks))
        (list :status 207
              :headers '(("Content-Type" . "application/xml; charset=utf-8"))
              :body (ecard-carddav-mock--make-multistatus (nreverse responses)))))))

(defun ecard-carddav-mock--propfind-addressbook (mock path depth)
  "Handle address book PROPFIND for MOCK server at PATH with DEPTH."
  (let ((base-url (oref mock base-url))
        (addressbook (gethash path (oref mock addressbooks))))
    (unless addressbook
      (signal 'error (list "Address book not found" path)))

    (if (string= depth "0")
        ;; Just the address book itself
        (let ((response (ecard-carddav-mock--make-response
                        (concat base-url path)
                        (ecard-carddav-mock--make-propstat
                         `((resourcetype nil
                             (collection nil)
                             (C:addressbook nil))
                           (displayname nil ,(oref addressbook display-name))
                           (CS:getctag nil ,(oref addressbook ctag))
                           (sync-token nil ,(format "http://mock.example.com/ns/sync/%s"
                                                   (oref addressbook sync-token))))))))
          (list :status 207
                :headers '(("Content-Type" . "application/xml; charset=utf-8"))
                :body (ecard-carddav-mock--make-multistatus response)))

      ;; List all resources
      ;; IMPORTANT: Mimic Radicale behavior - include text/vcard content-type
      ;; for the collection itself, not just individual resources. This ensures
      ;; that ecard-carddav-list-resources properly filters out the collection.
      (let ((responses (list (ecard-carddav-mock--make-response
                             (concat base-url path)
                             (ecard-carddav-mock--make-propstat
                              `((resourcetype nil
                                  (collection nil)
                                  (C:addressbook nil))
                                (getcontenttype nil "text/vcard; charset=utf-8")))))))
        (maphash (lambda (resource-path resource)
                   (push (ecard-carddav-mock--make-response
                          (concat base-url resource-path)
                          (ecard-carddav-mock--make-propstat
                           `((getetag nil ,(format "\"%s\"" (oref resource etag)))
                             (getcontenttype nil "text/vcard; charset=utf-8"))))
                         responses))
                 (oref addressbook resources))
        (list :status 207
              :headers '(("Content-Type" . "application/xml; charset=utf-8"))
              :body (ecard-carddav-mock--make-multistatus (nreverse responses)))))))

(defun ecard-carddav-mock--handle-get (mock path)
  "Handle GET request for MOCK server at PATH."
  (let ((addressbook (ecard-carddav-mock--find-addressbook-for-path mock path)))
    (unless addressbook
      (signal 'error (list "Address book not found for path" path)))

    (let ((resource (gethash path (oref addressbook resources))))
      (if resource
          (list :status 200
                :headers `(("Content-Type" . "text/vcard; charset=utf-8")
                          ("ETag" . ,(format "\"%s\"" (oref resource etag))))
                :body (oref resource ecard-data))
        (list :status 404
              :headers '()
              :body "Not Found")))))

(defun ecard-carddav-mock--handle-put (mock path body if-match)
  "Handle PUT request for MOCK server at PATH with BODY and IF-MATCH."
  (let ((addressbook (ecard-carddav-mock--find-addressbook-for-path mock path)))
    (unless addressbook
      (signal 'error (list "Address book not found for path" path)))

    (let ((resource (gethash path (oref addressbook resources))))
      ;; Check If-Match if provided
      (when (and if-match resource)
        (let ((expected-etag (string-trim if-match "\"" "\""))
              (current-etag (oref resource etag)))
          (unless (string= expected-etag current-etag)
            (signal 'error (list "ETag mismatch" expected-etag current-etag)))))

      ;; Parse vCard
      (condition-case err
          (let* ((ecard-obj (ecard-parse body))
                 (new-etag (number-to-string (oref mock next-etag)))
                 (is-new (null resource)))

            ;; Increment etag counter
            (oset mock next-etag (1+ (oref mock next-etag)))

            ;; Create or update resource
            (puthash path
                     (ecard-carddav-mock-resource
                      :path path
                      :etag new-etag
                      :ecard ecard-obj
                      :ecard-data body)
                     (oref addressbook resources))

            ;; Update address book metadata
            (ecard-carddav-mock--increment-ctag addressbook)
            (ecard-carddav-mock--increment-sync-token addressbook)

            (list :status (if is-new 201 204)
                  :headers `(("ETag" . ,(format "\"%s\"" new-etag)))
                  :body ""))
        (error
         (list :status 400
               :headers '()
               :body (format "Invalid vCard: %s" err)))))))

(defun ecard-carddav-mock--handle-delete (mock path if-match)
  "Handle DELETE request for MOCK server at PATH with IF-MATCH."
  (let ((addressbook (ecard-carddav-mock--find-addressbook-for-path mock path)))
    (unless addressbook
      (signal 'error (list "Address book not found for path" path)))

    (let ((resource (gethash path (oref addressbook resources))))
      (unless resource
        (signal 'error (list "Resource not found" path)))

      ;; Check If-Match if provided
      (when if-match
        (let ((expected-etag (string-trim if-match "\"" "\""))
              (current-etag (oref resource etag)))
          (unless (string= expected-etag current-etag)
            (signal 'error (list "ETag mismatch" expected-etag current-etag)))))

      ;; Delete resource
      (remhash path (oref addressbook resources))

      ;; Update address book metadata
      (ecard-carddav-mock--increment-ctag addressbook)
      (ecard-carddav-mock--increment-sync-token addressbook)

      (list :status 204
            :headers '()
            :body ""))))

(defun ecard-carddav-mock--handle-report (mock path body)
  "Handle REPORT request for MOCK server at PATH with BODY."
  (let ((xml (ecard-carddav-mock--parse-request-body body)))
    (cond
     ((dom-by-tag xml 'C:addressbook-multiget)
      (ecard-carddav-mock--handle-multiget mock path xml))

     ((dom-by-tag xml 'sync-collection)
      (ecard-carddav-mock--handle-sync-collection mock path xml))

     ((dom-by-tag xml 'C:addressbook-query)
      (ecard-carddav-mock--handle-query mock path xml))

     (t
      (list :status 400
            :headers '()
            :body "Unsupported REPORT type")))))

(defun ecard-carddav-mock--handle-multiget (mock path xml)
  "Handle addressbook-multiget REPORT for MOCK server at PATH with XML."
  (let* ((_base-url (oref mock base-url))
         (addressbook (gethash path (oref mock addressbooks)))
         (href-nodes (dom-by-tag xml 'href))
         (hrefs (mapcar #'dom-text href-nodes))
         (responses nil))

    (dolist (href hrefs)
      ;; Extract path from full URL if needed
      (let* ((resource-path (if (string-prefix-p "http" href)
                                (url-filename (url-generic-parse-url href))
                              href))
             (resource (gethash resource-path (oref addressbook resources))))
        (if resource
            (push (ecard-carddav-mock--make-response
                   href
                   (ecard-carddav-mock--make-propstat
                    `((getetag nil ,(format "\"%s\"" (oref resource etag)))
                      (C:address-data nil ,(ecard-carddav-mock--xml-escape (oref resource ecard-data))))))
                  responses)
          (push (ecard-carddav-mock--make-response
                 href
                 (ecard-carddav-mock--make-propstat
                  '() "HTTP/1.1 404 Not Found"))
                responses))))

    (list :status 207
          :headers '(("Content-Type" . "application/xml; charset=utf-8"))
          :body (ecard-carddav-mock--make-multistatus (nreverse responses)))))

(defun ecard-carddav-mock--handle-sync-collection (mock path xml)
  "Handle sync-collection REPORT for MOCK server at PATH with XML."
  (let* ((base-url (oref mock base-url))
         (addressbook (gethash path (oref mock addressbooks)))
         (sync-token-node (dom-by-tag xml 'sync-token))
         (_old-token (when sync-token-node (dom-text (car sync-token-node))))
         (current-token (oref addressbook sync-token))
         (responses nil))

    ;; For simplicity, always return all current resources
    ;; A real implementation would track changes
    (maphash (lambda (resource-path resource)
               (push (ecard-carddav-mock--make-response
                      (concat base-url resource-path)
                      (ecard-carddav-mock--make-propstat
                       `((getetag nil ,(format "\"%s\"" (oref resource etag))))))
                     responses))
             (oref addressbook resources))

    ;; Add sync-token to response
    (let ((token-element `(sync-token nil
                                      ,(format "http://mock.example.com/ns/sync/%s" current-token))))
      (list :status 207
            :headers '(("Content-Type" . "application/xml; charset=utf-8"))
            :body (ecard-carddav-mock--xml-to-string
                   `(multistatus ((xmlns . "DAV:")
                                  (xmlns:C . "urn:ietf:params:xml:ns:carddav")
                                  (xmlns:CS . "http://calendarserver.org/ns/"))
                                 ,@(nreverse responses)
                                 ,token-element))))))

(defun ecard-carddav-mock--handle-query (mock path _xml)
  "Handle addressbook-query REPORT for MOCK server at PATH with XML."
  ;; Simplified - just return all resources
  (let* ((base-url (oref mock base-url))
         (addressbook (gethash path (oref mock addressbooks)))
         (responses nil))

    (maphash (lambda (resource-path resource)
               (push (ecard-carddav-mock--make-response
                      (concat base-url resource-path)
                      (ecard-carddav-mock--make-propstat
                       `((getetag nil ,(format "\"%s\"" (oref resource etag)))
                         (C:address-data nil ,(ecard-carddav-mock--xml-escape (oref resource ecard-data))))))
                     responses))
             (oref addressbook resources))

    (list :status 207
          :headers '(("Content-Type" . "application/xml; charset=utf-8"))
          :body (ecard-carddav-mock--make-multistatus (nreverse responses)))))

;;; Helper functions

(defun ecard-carddav-mock--find-addressbook-for-path (mock path)
  "Find address book containing PATH in MOCK server."
  (let ((result nil))
    (maphash (lambda (ab-path addressbook)
               (when (string-prefix-p ab-path path)
                 (setq result addressbook)))
             (oref mock addressbooks))
    result))

(defun ecard-carddav-mock--increment-ctag (addressbook)
  "Increment CTag for ADDRESSBOOK."
  (let ((current (string-to-number (oref addressbook ctag))))
    (oset addressbook ctag (number-to-string (1+ current)))))

(defun ecard-carddav-mock--increment-sync-token (addressbook)
  "Increment sync-token for ADDRESSBOOK."
  (let ((current (string-to-number (oref addressbook sync-token))))
    (oset addressbook sync-token (number-to-string (1+ current)))))

;;; Mock URL handler

(defun ecard-carddav-mock--url-retrieve-synchronously
    (url &optional silent inhibit-cookies timeout)
  "Mock replacement for `url-retrieve-synchronously'."
  (if (and ecard-carddav-mock--active-server
           (string-prefix-p (oref ecard-carddav-mock--active-server base-url) url))
      (ecard-carddav-mock--handle-request url)
    ;; Not a mock URL - use original function
    (funcall ecard-carddav-mock--original-retrieve url silent inhibit-cookies timeout)))

(defun ecard-carddav-mock--handle-request (url-string)
  "Handle mock HTTP request to URL-STRING."
  (let* ((url (url-generic-parse-url url-string))
         (mock ecard-carddav-mock--active-server)
         (method url-request-method)
         (path (url-filename url))
         (body (when url-request-data
                 (decode-coding-string url-request-data 'utf-8)))
         (headers url-request-extra-headers)
         (depth (cdr (assoc "Depth" headers)))
         (if-match (cdr (assoc "If-Match" headers)))
         (response (condition-case err
                       (pcase method
                         ("OPTIONS" (ecard-carddav-mock--handle-options mock path))
                         ("PROPFIND" (ecard-carddav-mock--handle-propfind mock path depth body))
                         ("GET" (ecard-carddav-mock--handle-get mock path))
                         ("PUT" (ecard-carddav-mock--handle-put mock path body if-match))
                         ("DELETE" (ecard-carddav-mock--handle-delete mock path if-match))
                         ("REPORT" (ecard-carddav-mock--handle-report mock path body))
                         (_ (list :status 405 :headers '() :body "Method Not Allowed")))
                     (error
                      ;; Log error for debugging
                      (message "Mock server error: %S" err)
                      (pcase (car (cdr err))
                        ("ETag mismatch" (list :status 412 :headers '() :body "Precondition Failed"))
                        ("Resource not found" (list :status 404 :headers '() :body "Not Found"))
                        ("Address book not found" (list :status 404 :headers '() :body "Not Found"))
                        (_ (list :status 500 :headers '() :body (format "Error: %s" err))))))))

    ;; Create mock response buffer
    (with-current-buffer (generate-new-buffer " *mock-carddav*")
      (let ((status (plist-get response :status))
            (headers (plist-get response :headers))
            (body (plist-get response :body)))

        ;; Insert HTTP response
        (insert (format "HTTP/1.1 %d %s\r\n"
                       status
                       (pcase status
                         (200 "OK")
                         (201 "Created")
                         (204 "No Content")
                         (207 "Multi-Status")
                         (400 "Bad Request")
                         (404 "Not Found")
                         (405 "Method Not Allowed")
                         (412 "Precondition Failed")
                         (500 "Internal Server Error")
                         (_ "Unknown"))))

        ;; Insert headers
        (dolist (header headers)
          (insert (format "%s: %s\r\n" (car header) (cdr header))))

        (insert "\r\n")

        ;; Insert body
        (when body
          (insert body))

        (goto-char (point-min))
        (current-buffer)))))

;;; Public API

;;;###autoload
(defun ecard-carddav-mock-server-create (&rest args)
  "Create mock CardDAV server from ARGS.

ARGS is a plist with keys:
  :base-url STRING - Base URL for mock server
    (default: https://mock.example.com)
  :principal-path STRING - Principal path
    (default: /principals/user/)
  :addressbook-home-path STRING - Address book home
    (default: /addressbooks/user/)

Example:
  (ecard-carddav-mock-server-create
   :base-url \"https://test.example.com\")"
  (let ((base-url (or (plist-get args :base-url) "https://mock.example.com"))
        (principal-path (or (plist-get args :principal-path) "/principals/user/"))
        (home-path (or (plist-get args :addressbook-home-path) "/addressbooks/user/")))
    (ecard-carddav-mock-server
     :base-url base-url
     :principal-path principal-path
     :addressbook-home-path home-path)))

(defun ecard-carddav-mock-add-addressbook (mock path disp-name description)
  "Add address book to MOCK server at PATH with DISP-NAME and DESCRIPTION."
  (puthash path
           (ecard-carddav-mock-addressbook
            :path path
            :display-name disp-name
            :description description)
           (oref mock addressbooks)))

(defun ecard-carddav-mock-put-ecard (mock path ecard-obj)
  "Add or update vCard in MOCK server at PATH with ECARD-OBJ."
  (let* ((addressbook (ecard-carddav-mock--find-addressbook-for-path mock path))
         (ecard-data (ecard-serialize ecard-obj))
         (etag (number-to-string (oref mock next-etag))))

    (unless addressbook
      (signal 'error (list "Address book not found for path" path)))

    (oset mock next-etag (1+ (oref mock next-etag)))

    (puthash path
             (ecard-carddav-mock-resource
              :path path
              :etag etag
              :ecard ecard-obj
              :ecard-data ecard-data)
             (oref addressbook resources))

    (ecard-carddav-mock--increment-ctag addressbook)
    (ecard-carddav-mock--increment-sync-token addressbook)

    etag))

(defun ecard-carddav-mock-install (mock)
  "Install MOCK server as active mock, intercepting url.el requests."
  (setq ecard-carddav-mock--active-server mock)
  (unless ecard-carddav-mock--original-retrieve
    (setq ecard-carddav-mock--original-retrieve
          (symbol-function 'url-retrieve-synchronously))
    (advice-add 'url-retrieve-synchronously
                :override #'ecard-carddav-mock--url-retrieve-synchronously)))

(defun ecard-carddav-mock-uninstall ()
  "Uninstall mock server, restoring normal url.el behavior."
  (when ecard-carddav-mock--original-retrieve
    (advice-remove 'url-retrieve-synchronously
                   #'ecard-carddav-mock--url-retrieve-synchronously)
    (setq ecard-carddav-mock--original-retrieve nil))
  (setq ecard-carddav-mock--active-server nil))

(provide 'ecard-carddav-mock)
;;; ecard-carddav-mock.el ends here
