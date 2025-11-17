;;; vcard-carddav-mock.el --- Mock CardDAV server for testing -*- lexical-binding: t; -*-

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
;;   (setq mock (vcard-carddav-mock-server-create
;;               :base-url "https://test.example.com"))
;;
;;   ;; Add an address book
;;   (vcard-carddav-mock-add-addressbook
;;    mock "/addressbooks/user/contacts/"
;;    "Contacts" "My contacts")
;;
;;   ;; Add a vCard
;;   (vcard-carddav-mock-put-vcard
;;    mock "/addressbooks/user/contacts/john.vcf"
;;    vcard-obj)
;;
;;   ;; Install mock URL handler
;;   (vcard-carddav-mock-install mock)
;;
;;   ;; Run tests...
;;
;;   ;; Uninstall mock handler
;;   (vcard-carddav-mock-uninstall)

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'url)
(require 'vcard)

;;; Custom group

(defgroup vcard-carddav-mock nil
  "Mock CardDAV server for testing."
  :group 'vcard-carddav
  :prefix "vcard-carddav-mock-")

;;; EIEIO Classes

(defclass vcard-carddav-mock-server ()
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

(defclass vcard-carddav-mock-addressbook ()
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

(defclass vcard-carddav-mock-resource ()
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
   (vcard
    :initarg :vcard
    :initform nil
    :type (or null vcard)
    :documentation "vCard object.")
   (vcard-data
    :initarg :vcard-data
    :initform nil
    :type (or null string)
    :documentation "Serialized vCard data."))
  "Mock vCard resource.")

;;; Global state

(defvar vcard-carddav-mock--active-server nil
  "Currently active mock server, or nil if not mocking.")

(defvar vcard-carddav-mock--original-retrieve nil
  "Original url-retrieve-synchronously function.")

;;; XML helpers

(defun vcard-carddav-mock--xml-escape (text)
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

(defun vcard-carddav-mock--xml-to-string (xml)
  "Convert XML s-expression to string."
  (with-temp-buffer
    (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    (xml-print (list xml))
    (buffer-string)))

(defun vcard-carddav-mock--make-multistatus (responses)
  "Create 207 Multi-Status response with RESPONSES list."
  (vcard-carddav-mock--xml-to-string
   `(multistatus ((xmlns . "DAV:")
                  (xmlns:C . "urn:ietf:params:xml:ns:carddav")
                  (xmlns:CS . "http://calendarserver.org/ns/"))
                 ,@responses)))

(defun vcard-carddav-mock--make-response (href &rest propstats)
  "Create response element with HREF and PROPSTATS."
  `(response nil
             (href nil ,href)
             ,@propstats))

(defun vcard-carddav-mock--make-propstat (props &optional status)
  "Create propstat element with PROPS and STATUS."
  (let ((status (or status "HTTP/1.1 200 OK")))
    `(propstat nil
               (prop nil ,@props)
               (status nil ,status))))

;;; Request parsing

(defun vcard-carddav-mock--parse-request-body (body)
  "Parse XML request BODY.
Returns parsed XML s-expression."
  (when body
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (car (xml-parse-region (point-min) (point-max))))))

(defun vcard-carddav-mock--extract-prop-names (xml)
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

(defun vcard-carddav-mock--handle-options (mock path)
  "Handle OPTIONS request for MOCK server at PATH."
  (list :status 200
        :headers '(("DAV" . "1, 2, 3, addressbook")
                   ("Allow" . "OPTIONS, GET, HEAD, POST, PUT, DELETE, PROPFIND, REPORT"))
        :body ""))

(defun vcard-carddav-mock--handle-propfind (mock path depth body)
  "Handle PROPFIND request for MOCK server at PATH with DEPTH and BODY."
  (let ((xml (vcard-carddav-mock--parse-request-body body))
        (prop-names (when body
                     (vcard-carddav-mock--extract-prop-names
                      (vcard-carddav-mock--parse-request-body body)))))
    (cond
     ;; Principal discovery
     ((string-match-p "/.well-known/carddav" path)
      (vcard-carddav-mock--propfind-principal mock))

     ;; Principal properties
     ((string= path (oref mock principal-path))
      (vcard-carddav-mock--propfind-principal-props mock))

     ;; Address book home
     ((string= path (oref mock addressbook-home-path))
      (vcard-carddav-mock--propfind-addressbook-home mock depth))

     ;; Specific address book
     (t
      (vcard-carddav-mock--propfind-addressbook mock path depth)))))

(defun vcard-carddav-mock--propfind-principal (mock)
  "Handle principal discovery PROPFIND for MOCK server."
  (let* ((base-url (oref mock base-url))
         (principal-path (oref mock principal-path))
         (principal-url (concat base-url principal-path))
         (response (vcard-carddav-mock--make-response
                    "/.well-known/carddav"
                    (vcard-carddav-mock--make-propstat
                     `((current-user-principal nil
                        (href nil ,principal-url)))))))
    (list :status 207
          :headers '(("Content-Type" . "application/xml; charset=utf-8"))
          :body (vcard-carddav-mock--make-multistatus (list response)))))

(defun vcard-carddav-mock--propfind-principal-props (mock)
  "Handle principal properties PROPFIND for MOCK server."
  (let* ((base-url (oref mock base-url))
         (home-path (oref mock addressbook-home-path))
         (home-url (concat base-url home-path))
         (principal-url (concat base-url (oref mock principal-path)))
         (response (vcard-carddav-mock--make-response
                    principal-url
                    (vcard-carddav-mock--make-propstat
                     `((C:addressbook-home-set nil
                        (href nil ,home-url)))))))
    (list :status 207
          :headers '(("Content-Type" . "application/xml; charset=utf-8"))
          :body (vcard-carddav-mock--make-multistatus (list response)))))

(defun vcard-carddav-mock--propfind-addressbook-home (mock depth)
  "Handle address book home PROPFIND for MOCK server with DEPTH."
  (let ((base-url (oref mock base-url))
        (home-path (oref mock addressbook-home-path)))
    (if (string= depth "0")
        ;; Just the home collection itself
        (let ((home-url (concat base-url home-path))
              (response (vcard-carddav-mock--make-response
                        (concat base-url home-path)
                        (vcard-carddav-mock--make-propstat
                         '((resourcetype nil (collection nil)))))))
          (list :status 207
                :headers '(("Content-Type" . "application/xml; charset=utf-8"))
                :body (vcard-carddav-mock--make-multistatus response)))

      ;; List all address books
      (let ((responses nil))
        (maphash (lambda (path addressbook)
                   (push (vcard-carddav-mock--make-response
                          (concat base-url path)
                          (vcard-carddav-mock--make-propstat
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
              :body (vcard-carddav-mock--make-multistatus (nreverse responses)))))))

(defun vcard-carddav-mock--propfind-addressbook (mock path depth)
  "Handle address book PROPFIND for MOCK server at PATH with DEPTH."
  (let ((base-url (oref mock base-url))
        (addressbook (gethash path (oref mock addressbooks))))
    (unless addressbook
      (signal 'error (list "Address book not found" path)))

    (if (string= depth "0")
        ;; Just the address book itself
        (let ((response (vcard-carddav-mock--make-response
                        (concat base-url path)
                        (vcard-carddav-mock--make-propstat
                         `((resourcetype nil
                             (collection nil)
                             (C:addressbook nil))
                           (displayname nil ,(oref addressbook display-name))
                           (CS:getctag nil ,(oref addressbook ctag))
                           (sync-token nil ,(format "http://mock.example.com/ns/sync/%s"
                                                   (oref addressbook sync-token))))))))
          (list :status 207
                :headers '(("Content-Type" . "application/xml; charset=utf-8"))
                :body (vcard-carddav-mock--make-multistatus response)))

      ;; List all resources
      (let ((responses (list (vcard-carddav-mock--make-response
                             (concat base-url path)
                             (vcard-carddav-mock--make-propstat
                              `((resourcetype nil
                                  (collection nil)
                                  (C:addressbook nil))))))))
        (maphash (lambda (resource-path resource)
                   (push (vcard-carddav-mock--make-response
                          (concat base-url resource-path)
                          (vcard-carddav-mock--make-propstat
                           `((getetag nil ,(format "\"%s\"" (oref resource etag)))
                             (getcontenttype nil "text/vcard; charset=utf-8"))))
                         responses))
                 (oref addressbook resources))
        (list :status 207
              :headers '(("Content-Type" . "application/xml; charset=utf-8"))
              :body (vcard-carddav-mock--make-multistatus (nreverse responses)))))))

(defun vcard-carddav-mock--handle-get (mock path)
  "Handle GET request for MOCK server at PATH."
  (let ((addressbook (vcard-carddav-mock--find-addressbook-for-path mock path)))
    (unless addressbook
      (signal 'error (list "Address book not found for path" path)))

    (let ((resource (gethash path (oref addressbook resources))))
      (if resource
          (list :status 200
                :headers `(("Content-Type" . "text/vcard; charset=utf-8")
                          ("ETag" . ,(format "\"%s\"" (oref resource etag))))
                :body (oref resource vcard-data))
        (list :status 404
              :headers '()
              :body "Not Found")))))

(defun vcard-carddav-mock--handle-put (mock path body if-match)
  "Handle PUT request for MOCK server at PATH with BODY and IF-MATCH."
  (let ((addressbook (vcard-carddav-mock--find-addressbook-for-path mock path)))
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
          (let* ((vcard-obj (vcard-parse body))
                 (new-etag (number-to-string (oref mock next-etag)))
                 (is-new (null resource)))

            ;; Increment etag counter
            (oset mock next-etag (1+ (oref mock next-etag)))

            ;; Create or update resource
            (puthash path
                     (vcard-carddav-mock-resource
                      :path path
                      :etag new-etag
                      :vcard vcard-obj
                      :vcard-data body)
                     (oref addressbook resources))

            ;; Update address book metadata
            (vcard-carddav-mock--increment-ctag addressbook)
            (vcard-carddav-mock--increment-sync-token addressbook)

            (list :status (if is-new 201 204)
                  :headers `(("ETag" . ,(format "\"%s\"" new-etag)))
                  :body ""))
        (error
         (list :status 400
               :headers '()
               :body (format "Invalid vCard: %s" err)))))))

(defun vcard-carddav-mock--handle-delete (mock path if-match)
  "Handle DELETE request for MOCK server at PATH with IF-MATCH."
  (let ((addressbook (vcard-carddav-mock--find-addressbook-for-path mock path)))
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
      (vcard-carddav-mock--increment-ctag addressbook)
      (vcard-carddav-mock--increment-sync-token addressbook)

      (list :status 204
            :headers '()
            :body ""))))

(defun vcard-carddav-mock--handle-report (mock path body)
  "Handle REPORT request for MOCK server at PATH with BODY."
  (let ((xml (vcard-carddav-mock--parse-request-body body)))
    (cond
     ((dom-by-tag xml 'C:addressbook-multiget)
      (vcard-carddav-mock--handle-multiget mock path xml))

     ((dom-by-tag xml 'sync-collection)
      (vcard-carddav-mock--handle-sync-collection mock path xml))

     ((dom-by-tag xml 'C:addressbook-query)
      (vcard-carddav-mock--handle-query mock path xml))

     (t
      (list :status 400
            :headers '()
            :body "Unsupported REPORT type")))))

(defun vcard-carddav-mock--handle-multiget (mock path xml)
  "Handle addressbook-multiget REPORT for MOCK server at PATH with XML."
  (let* ((base-url (oref mock base-url))
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
            (push (vcard-carddav-mock--make-response
                   href
                   (vcard-carddav-mock--make-propstat
                    `((getetag nil ,(format "\"%s\"" (oref resource etag)))
                      (C:address-data nil ,(vcard-carddav-mock--xml-escape (oref resource vcard-data))))))
                  responses)
          (push (vcard-carddav-mock--make-response
                 href
                 (vcard-carddav-mock--make-propstat
                  '() "HTTP/1.1 404 Not Found"))
                responses))))

    (list :status 207
          :headers '(("Content-Type" . "application/xml; charset=utf-8"))
          :body (vcard-carddav-mock--make-multistatus (nreverse responses)))))

(defun vcard-carddav-mock--handle-sync-collection (mock path xml)
  "Handle sync-collection REPORT for MOCK server at PATH with XML."
  (let* ((base-url (oref mock base-url))
         (addressbook (gethash path (oref mock addressbooks)))
         (sync-token-node (dom-by-tag xml 'sync-token))
         (old-token (when sync-token-node (dom-text (car sync-token-node))))
         (current-token (oref addressbook sync-token))
         (responses nil))

    ;; For simplicity, always return all current resources
    ;; A real implementation would track changes
    (maphash (lambda (resource-path resource)
               (push (vcard-carddav-mock--make-response
                      (concat base-url resource-path)
                      (vcard-carddav-mock--make-propstat
                       `((getetag nil ,(format "\"%s\"" (oref resource etag))))))
                     responses))
             (oref addressbook resources))

    ;; Add sync-token to response
    (let ((token-element `(sync-token nil
                           ,(format "http://mock.example.com/ns/sync/%s" current-token))))
      (list :status 207
            :headers '(("Content-Type" . "application/xml; charset=utf-8"))
            :body (vcard-carddav-mock--xml-to-string
                   `(multistatus ((xmlns . "DAV:")
                                  (xmlns:C . "urn:ietf:params:xml:ns:carddav")
                                  (xmlns:CS . "http://calendarserver.org/ns/"))
                                 ,@(nreverse responses)
                                 ,token-element))))))

(defun vcard-carddav-mock--handle-query (mock path xml)
  "Handle addressbook-query REPORT for MOCK server at PATH with XML."
  ;; Simplified - just return all resources
  (let* ((base-url (oref mock base-url))
         (addressbook (gethash path (oref mock addressbooks)))
         (responses nil))

    (maphash (lambda (resource-path resource)
               (push (vcard-carddav-mock--make-response
                      (concat base-url resource-path)
                      (vcard-carddav-mock--make-propstat
                       `((getetag nil ,(format "\"%s\"" (oref resource etag)))
                         (C:address-data nil ,(vcard-carddav-mock--xml-escape (oref resource vcard-data))))))
                     responses))
             (oref addressbook resources))

    (list :status 207
          :headers '(("Content-Type" . "application/xml; charset=utf-8"))
          :body (vcard-carddav-mock--make-multistatus (nreverse responses)))))

;;; Helper functions

(defun vcard-carddav-mock--find-addressbook-for-path (mock path)
  "Find address book containing PATH in MOCK server."
  (let ((result nil))
    (maphash (lambda (ab-path addressbook)
               (when (string-prefix-p ab-path path)
                 (setq result addressbook)))
             (oref mock addressbooks))
    result))

(defun vcard-carddav-mock--increment-ctag (addressbook)
  "Increment CTag for ADDRESSBOOK."
  (let ((current (string-to-number (oref addressbook ctag))))
    (oset addressbook ctag (number-to-string (1+ current)))))

(defun vcard-carddav-mock--increment-sync-token (addressbook)
  "Increment sync-token for ADDRESSBOOK."
  (let ((current (string-to-number (oref addressbook sync-token))))
    (oset addressbook sync-token (number-to-string (1+ current)))))

;;; Mock URL handler

(defun vcard-carddav-mock--url-retrieve-synchronously (url &optional silent inhibit-cookies timeout)
  "Mock replacement for `url-retrieve-synchronously'."
  (if (and vcard-carddav-mock--active-server
           (string-prefix-p (oref vcard-carddav-mock--active-server base-url) url))
      (vcard-carddav-mock--handle-request url)
    ;; Not a mock URL - use original function
    (funcall vcard-carddav-mock--original-retrieve url silent inhibit-cookies timeout)))

(defun vcard-carddav-mock--handle-request (url-string)
  "Handle mock HTTP request to URL-STRING."
  (let* ((url (url-generic-parse-url url-string))
         (mock vcard-carddav-mock--active-server)
         (method url-request-method)
         (path (url-filename url))
         (body (when url-request-data
                 (decode-coding-string url-request-data 'utf-8)))
         (headers url-request-extra-headers)
         (depth (cdr (assoc "Depth" headers)))
         (if-match (cdr (assoc "If-Match" headers)))
         (response (condition-case err
                       (pcase method
                         ("OPTIONS" (vcard-carddav-mock--handle-options mock path))
                         ("PROPFIND" (vcard-carddav-mock--handle-propfind mock path depth body))
                         ("GET" (vcard-carddav-mock--handle-get mock path))
                         ("PUT" (vcard-carddav-mock--handle-put mock path body if-match))
                         ("DELETE" (vcard-carddav-mock--handle-delete mock path if-match))
                         ("REPORT" (vcard-carddav-mock--handle-report mock path body))
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
(defun vcard-carddav-mock-server-create (&rest args)
  "Create mock CardDAV server from ARGS.

ARGS is a plist with keys:
  :base-url STRING - Base URL for mock server (default: https://mock.example.com)
  :principal-path STRING - Principal path (default: /principals/user/)
  :addressbook-home-path STRING - Address book home (default: /addressbooks/user/)

Example:
  (vcard-carddav-mock-server-create
   :base-url \"https://test.example.com\")"
  (let ((base-url (or (plist-get args :base-url) "https://mock.example.com"))
        (principal-path (or (plist-get args :principal-path) "/principals/user/"))
        (home-path (or (plist-get args :addressbook-home-path) "/addressbooks/user/")))
    (vcard-carddav-mock-server
     :base-url base-url
     :principal-path principal-path
     :addressbook-home-path home-path)))

(defun vcard-carddav-mock-add-addressbook (mock path display-name description)
  "Add address book to MOCK server at PATH with DISPLAY-NAME and DESCRIPTION."
  (puthash path
           (vcard-carddav-mock-addressbook
            :path path
            :display-name display-name
            :description description)
           (oref mock addressbooks)))

(defun vcard-carddav-mock-put-vcard (mock path vcard-obj)
  "Add or update vCard in MOCK server at PATH with VCARD-OBJ."
  (let* ((addressbook (vcard-carddav-mock--find-addressbook-for-path mock path))
         (vcard-data (vcard-serialize vcard-obj))
         (etag (number-to-string (oref mock next-etag))))

    (unless addressbook
      (signal 'error (list "Address book not found for path" path)))

    (oset mock next-etag (1+ (oref mock next-etag)))

    (puthash path
             (vcard-carddav-mock-resource
              :path path
              :etag etag
              :vcard vcard-obj
              :vcard-data vcard-data)
             (oref addressbook resources))

    (vcard-carddav-mock--increment-ctag addressbook)
    (vcard-carddav-mock--increment-sync-token addressbook)

    etag))

(defun vcard-carddav-mock-install (mock)
  "Install MOCK server as active mock, intercepting url.el requests."
  (setq vcard-carddav-mock--active-server mock)
  (unless vcard-carddav-mock--original-retrieve
    (setq vcard-carddav-mock--original-retrieve
          (symbol-function 'url-retrieve-synchronously))
    (advice-add 'url-retrieve-synchronously
                :override #'vcard-carddav-mock--url-retrieve-synchronously)))

(defun vcard-carddav-mock-uninstall ()
  "Uninstall mock server, restoring normal url.el behavior."
  (when vcard-carddav-mock--original-retrieve
    (advice-remove 'url-retrieve-synchronously
                   #'vcard-carddav-mock--url-retrieve-synchronously)
    (setq vcard-carddav-mock--original-retrieve nil))
  (setq vcard-carddav-mock--active-server nil))

(provide 'vcard-carddav-mock)
;;; vcard-carddav-mock.el ends here
