;;; ecard-carddav-tests.el --- Tests for CardDAV implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: comm, data, carddav, testing

;;; Commentary:

;; Comprehensive test suite for ecard-carddav modules.
;; Tests use the mock CardDAV server to avoid network dependencies.
;;
;; Run tests with:
;;   emacs -batch -L . -l ecard.el -l ecard-carddav-auth.el \
;;         -l ecard-carddav.el -l ecard-carddav-sync.el \
;;         -l ecard-carddav-mock.el -l ecard-carddav-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Or interactively:
;;   M-x load-file RET ecard-carddav-tests.el RET
;;   M-x ert RET t RET

;;; Code:

(require 'ert)
(require 'ecard)
(require 'ecard-carddav-auth)
(require 'ecard-carddav)
(require 'ecard-carddav-sync)
(require 'ecard-carddav-mock)

;;; Test fixtures

(defvar ecard-carddav-test--mock-server nil
  "Mock server for testing.")

(defvar ecard-carddav-test--temp-dir nil
  "Temporary directory for cache files.")

(defun ecard-carddav-test--setup ()
  "Set up test fixtures."
  ;; Create temporary directory
  (setq ecard-carddav-test--temp-dir
        (make-temp-file "ecard-carddav-test-" t))

  ;; Create mock server
  (setq ecard-carddav-test--mock-server
        (ecard-carddav-mock-server-create
         :base-url "https://test.example.com"))

  ;; Add address book
  (ecard-carddav-mock-add-addressbook
   ecard-carddav-test--mock-server
   "/addressbooks/user/contacts/"
   "Test Contacts"
   "Test address book")

  ;; Install mock
  (ecard-carddav-mock-install ecard-carddav-test--mock-server))

(defun ecard-carddav-test--teardown ()
  "Tear down test fixtures."
  ;; Uninstall mock
  (ecard-carddav-mock-uninstall)

  ;; Clean up temp directory
  (when (and ecard-carddav-test--temp-dir
             (file-directory-p ecard-carddav-test--temp-dir))
    (delete-directory ecard-carddav-test--temp-dir t))

  (setq ecard-carddav-test--mock-server nil
        ecard-carddav-test--temp-dir nil))

(defun ecard-carddav-test--create-test-ecard (fn)
  "Create a test vCard with formatted name FN."
  (ecard-create
   :fn fn
   :n (list "Doe" "John" "" "" "")
   :email "john@example.com"
   :tel "+1-555-1234"
   :uid (format "test-%s@example.com" (downcase (replace-regexp-in-string " " "-" fn)))))

;;; Authentication tests

(ert-deftest ecard-carddav-test-auth-basic-create ()
  "Test Basic Auth credential creation."
  (let ((auth (ecard-carddav-auth-basic-create
               :username "testuser"
               :password "testpass")))
    (should (ecard-carddav-auth-basic-p auth))
    (should (eq (oref auth type) :basic))
    (should (string= (oref auth username) "testuser"))
    (should (string= (oref auth password) "testpass"))))

(ert-deftest ecard-carddav-test-auth-basic-header ()
  "Test Basic Auth header generation."
  (let ((auth (ecard-carddav-auth-basic-create
               :username "user"
               :password "pass")))
    (should (string= (ecard-carddav-auth-get-header auth)
                    "Basic dXNlcjpwYXNz"))))

(ert-deftest ecard-carddav-test-auth-basic-valid ()
  "Test Basic Auth validation."
  (let ((auth (ecard-carddav-auth-basic-create
               :username "user"
               :password "pass")))
    (should (ecard-carddav-auth-valid-p auth))))

(ert-deftest ecard-carddav-test-auth-bearer-create ()
  "Test Bearer token creation."
  (let ((auth (ecard-carddav-auth-bearer-create
               :token "abc123"
               :expires-at (+ (float-time) 3600))))
    (should (ecard-carddav-auth-bearer-p auth))
    (should (eq (oref auth type) :bearer))
    (should (string= (oref auth token) "abc123"))))

(ert-deftest ecard-carddav-test-auth-bearer-header ()
  "Test Bearer token header generation."
  (let ((auth (ecard-carddav-auth-bearer-create
               :token "mytoken123")))
    (should (string= (ecard-carddav-auth-get-header auth)
                    "Bearer mytoken123"))))

(ert-deftest ecard-carddav-test-auth-bearer-expired ()
  "Test Bearer token expiration."
  (let ((auth (ecard-carddav-auth-bearer-create
               :token "expired"
               :expires-at (- (float-time) 3600))))
    (should-not (ecard-carddav-auth-valid-p auth))))

;;; Core protocol tests

(ert-deftest ecard-carddav-test-server-create ()
  "Test CardDAV server creation."
  (let* ((auth (ecard-carddav-auth-basic-create
                :username "user"
                :password "pass"))
         (server (ecard-carddav-server-create
                  :url "https://example.com"
                  :auth auth)))
    (should (ecard-carddav-server-p server))
    (should (string= (oref server url) "https://example.com"))
    (should (eq (oref server auth) auth))))

(ert-deftest ecard-carddav-test-discover-principal ()
  "Test principal discovery."
  (ecard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (ecard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (ecard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth)))
        (ecard-carddav-discover-principal server)
        (should (string= (oref server principal-url) "https://test.example.com/principals/user/")))
    (ecard-carddav-test--teardown)))

(ert-deftest ecard-carddav-test-discover-addressbook-home ()
  "Test address book home discovery."
  (ecard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (ecard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (ecard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth)))
        (ecard-carddav-discover-addressbook-home server)
        (should (string= (oref server addressbook-home-url) "https://test.example.com/addressbooks/user/")))
    (ecard-carddav-test--teardown)))

(ert-deftest ecard-carddav-test-discover-addressbooks ()
  "Test address book discovery."
  (ecard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (ecard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (ecard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth)))
        (let ((addressbooks (ecard-carddav-discover-addressbooks server)))
          (should (= (length addressbooks) 1))
          (let ((ab (car addressbooks)))
            (should (string= (oref ab display-name) "Test Contacts"))
            (should (string= (oref ab description) "Test address book")))))
    (ecard-carddav-test--teardown)))

(ert-deftest ecard-carddav-test-put-and-get-ecard ()
  "Test creating and retrieving a vCard."
  (ecard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (ecard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (ecard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (ecard-carddav-discover-addressbooks server))
             (ab (car addressbooks))
             (ecard (ecard-carddav-test--create-test-ecard "John Doe")))

        ;; PUT vCard
        (let ((resource (ecard-carddav-put-ecard
                         ab
                         "/addressbooks/user/contacts/john.vcf"
                         ecard)))
          (should (ecard-carddav-resource-p resource))
          (should (oref resource etag)))

        ;; GET vCard
        (let ((resource (ecard-carddav-get-resource
                         ab
                         "/addressbooks/user/contacts/john.vcf")))
          (should (ecard-carddav-resource-p resource))
          (should (ecard-p (oref resource ecard)))
          (should (string= (ecard-get-property-value (oref resource ecard) 'fn)
                           "John Doe"))))
    (ecard-carddav-test--teardown)))

(ert-deftest ecard-carddav-test-list-resources ()
  "Test listing resources in address book."
  (ecard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (ecard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (ecard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (ecard-carddav-discover-addressbooks server))
             (ab (car addressbooks)))

        ;; Add some vCards
        (ecard-carddav-put-ecard
         ab "/addressbooks/user/contacts/john.vcf"
         (ecard-carddav-test--create-test-ecard "John Doe"))
        (ecard-carddav-put-ecard
         ab "/addressbooks/user/contacts/jane.vcf"
         (ecard-carddav-test--create-test-ecard "Jane Smith"))

        ;; List resources
        (let ((resources (ecard-carddav-list-resources ab)))
          (should (= (length resources) 2))
          (should (member "/addressbooks/user/contacts/john.vcf"
                         (mapcar (lambda (r) (oref r path)) resources)))
          (should (member "/addressbooks/user/contacts/jane.vcf"
                         (mapcar (lambda (r) (oref r path)) resources)))))
    (ecard-carddav-test--teardown)))

(ert-deftest ecard-carddav-test-update-with-etag ()
  "Test updating vCard with ETag validation."
  (ecard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (ecard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (ecard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (ecard-carddav-discover-addressbooks server))
             (ab (car addressbooks))
             (vcard1 (ecard-carddav-test--create-test-ecard "John Doe")))

        ;; Create initial vCard
        (let ((resource1 (ecard-carddav-put-ecard
                         ab "/addressbooks/user/contacts/john.vcf"
                         vcard1)))
          (should (oref resource1 etag))

          ;; Update with correct ETag
          (let ((vcard2 (ecard-carddav-test--create-test-ecard "John Smith")))
            (let ((resource2 (ecard-carddav-put-ecard
                             ab "/addressbooks/user/contacts/john.vcf"
                             vcard2
                             (oref resource1 etag))))
              (should (not (string= (oref resource1 etag)
                                   (oref resource2 etag))))))))
    (ecard-carddav-test--teardown)))

(ert-deftest ecard-carddav-test-update-with-wrong-etag ()
  "Test ETag conflict detection."
  (ecard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (ecard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (ecard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (ecard-carddav-discover-addressbooks server))
             (ab (car addressbooks))
             (vcard1 (ecard-carddav-test--create-test-ecard "John Doe")))

        ;; Create initial vCard
        (ecard-carddav-put-ecard
         ab "/addressbooks/user/contacts/john.vcf"
         vcard1)

        ;; Try to update with wrong ETag
        (let ((vcard2 (ecard-carddav-test--create-test-ecard "John Smith")))
          (should-error
           (ecard-carddav-put-ecard
            ab "/addressbooks/user/contacts/john.vcf"
            vcard2
            "wrong-etag")
           :type 'ecard-carddav-conflict-error)))
    (ecard-carddav-test--teardown)))

(ert-deftest ecard-carddav-test-delete-resource ()
  "Test deleting a vCard resource."
  (ecard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (ecard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (ecard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (ecard-carddav-discover-addressbooks server))
             (ab (car addressbooks))
             (ecard (ecard-carddav-test--create-test-ecard "John Doe")))

        ;; Create vCard
        (ecard-carddav-put-ecard
         ab "/addressbooks/user/contacts/john.vcf"
         ecard)

        ;; Delete it
        (should (ecard-carddav-delete-resource
                 ab "/addressbooks/user/contacts/john.vcf"))

        ;; Verify it's gone
        (should-error
         (ecard-carddav-get-resource
          ab "/addressbooks/user/contacts/john.vcf")
         :type 'ecard-carddav-not-found-error))
    (ecard-carddav-test--teardown)))

;;; Synchronization tests

(ert-deftest ecard-carddav-test-sync-create ()
  "Test sync manager creation."
  (ecard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (ecard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (ecard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (ecard-carddav-discover-addressbooks server))
             (ab (car addressbooks))
             (sync (ecard-carddav-sync-create
                    :addressbook ab
                    :cache-dir (expand-file-name "cache" ecard-carddav-test--temp-dir)
                    :strategy :server-wins)))
        (should (ecard-carddav-sync-p sync))
        (should (eq (oref sync strategy) :server-wins)))
    (ecard-carddav-test--teardown)))

(ert-deftest ecard-carddav-test-sync-full ()
  "Test full synchronization."
  (ecard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (ecard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (ecard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (ecard-carddav-discover-addressbooks server))
             (ab (car addressbooks))
             (sync (ecard-carddav-sync-create
                    :addressbook ab
                    :cache-dir (expand-file-name "cache" ecard-carddav-test--temp-dir))))

        ;; Add some vCards to server
        (ecard-carddav-put-ecard
         ab "/addressbooks/user/contacts/john.vcf"
         (ecard-carddav-test--create-test-ecard "John Doe"))
        (ecard-carddav-put-ecard
         ab "/addressbooks/user/contacts/jane.vcf"
         (ecard-carddav-test--create-test-ecard "Jane Smith"))

        ;; Perform full sync
        (let ((updated (ecard-carddav-sync-full sync)))
          (should (= (length updated) 2))

          ;; Verify local cache
          (let ((local-john (ecard-carddav-sync-get-local
                            sync "/addressbooks/user/contacts/john.vcf"))
                (local-jane (ecard-carddav-sync-get-local
                            sync "/addressbooks/user/contacts/jane.vcf")))
            (should (ecard-p local-john))
            (should (ecard-p local-jane))
            (should (string= (ecard-get-property-value local-john 'fn) "John Doe"))
            (should (string= (ecard-get-property-value local-jane 'fn) "Jane Smith")))))
    (ecard-carddav-test--teardown)))

(ert-deftest ecard-carddav-test-sync-incremental ()
  "Test incremental synchronization."
  (ecard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (ecard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (ecard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (ecard-carddav-discover-addressbooks server))
             (ab (car addressbooks))
             (sync (ecard-carddav-sync-create
                    :addressbook ab
                    :cache-dir (expand-file-name "cache" ecard-carddav-test--temp-dir))))

        ;; Add initial vCard
        (ecard-carddav-put-ecard
         ab "/addressbooks/user/contacts/john.vcf"
         (ecard-carddav-test--create-test-ecard "John Doe"))

        ;; Initial sync
        (ecard-carddav-sync-full sync)

        ;; Add another vCard
        (ecard-carddav-put-ecard
         ab "/addressbooks/user/contacts/jane.vcf"
         (ecard-carddav-test--create-test-ecard "Jane Smith"))

        ;; Incremental sync
        (let ((result (ecard-carddav-sync-incremental sync)))
          (should (member "/addressbooks/user/contacts/jane.vcf"
                         (plist-get result :added)))

          ;; Verify both are in local cache
          (let ((all-local (ecard-carddav-sync-get-all-local sync)))
            (should (= (length all-local) 2)))))
    (ecard-carddav-test--teardown)))

(ert-deftest ecard-carddav-test-sync-cache-persistence ()
  "Test cache index persistence."
  (ecard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (ecard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (ecard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (ecard-carddav-discover-addressbooks server))
             (ab (car addressbooks))
             (cache-dir (expand-file-name "cache" ecard-carddav-test--temp-dir)))

        ;; Create sync manager and add data
        (let ((sync1 (ecard-carddav-sync-create
                     :addressbook ab
                     :cache-dir cache-dir)))
          (ecard-carddav-put-ecard
           ab "/addressbooks/user/contacts/john.vcf"
           (ecard-carddav-test--create-test-ecard "John Doe"))
          (ecard-carddav-sync-full sync1))

        ;; Create new sync manager with same cache dir
        (let ((sync2 (ecard-carddav-sync-create
                     :addressbook ab
                     :cache-dir cache-dir)))
          (ecard-carddav-sync--load-cache-index sync2)

          ;; Verify data persisted
          (let ((local (ecard-carddav-sync-get-local
                       sync2 "/addressbooks/user/contacts/john.vcf")))
            (should (ecard-p local))
            (should (string= (ecard-get-property-value local 'fn) "John Doe")))))
    (ecard-carddav-test--teardown)))

;;; Mock server tests

(ert-deftest ecard-carddav-test-mock-server-create ()
  "Test mock server creation."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.mock.com")))
    (should (ecard-carddav-mock-server-p mock))
    (should (string= (oref mock base-url) "https://test.mock.com"))))

(ert-deftest ecard-carddav-test-mock-add-addressbook ()
  "Test adding address book to mock server."
  (let ((mock (ecard-carddav-mock-server-create)))
    (ecard-carddav-mock-add-addressbook
     mock "/test/ab/" "Test" "Test AB")
    (should (gethash "/test/ab/" (oref mock addressbooks)))))

(ert-deftest ecard-carddav-test-mock-put-ecard ()
  "Test adding vCard to mock server."
  (let ((mock (ecard-carddav-mock-server-create))
        (ecard (ecard-carddav-test--create-test-ecard "Test User")))
    (ecard-carddav-mock-add-addressbook
     mock "/test/ab/" "Test" "Test AB")
    (let ((etag (ecard-carddav-mock-put-ecard
                 mock "/test/ab/test.vcf" ecard)))
      (should (stringp etag))
      (let ((ab (gethash "/test/ab/" (oref mock addressbooks))))
        (should (gethash "/test/ab/test.vcf" (oref ab resources)))))))

;;; Integration tests

(ert-deftest ecard-carddav-test-list-resources-excludes-collection ()
  "Verify that addressbook collection is not included in resource list.
Radicale returns text/vcard content-type for both the collection
and individual resources, so we must explicitly filter out the collection."
  (ecard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (ecard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (ecard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (ecard-carddav-discover-addressbooks server))
             (ab (car addressbooks))
             (ab-url (oref ab url)))

        ;; Add some vCards to the addressbook
        (ecard-carddav-put-ecard
         ab "/addressbooks/user/contacts/alice.vcf"
         (ecard-carddav-test--create-test-ecard "Alice Johnson"))
        (ecard-carddav-put-ecard
         ab "/addressbooks/user/contacts/bob.vcf"
         (ecard-carddav-test--create-test-ecard "Bob Williams"))
        (ecard-carddav-put-ecard
         ab "/addressbooks/user/contacts/charlie.vcf"
         (ecard-carddav-test--create-test-ecard "Charlie Brown"))

        ;; List resources
        (let ((resources (ecard-carddav-list-resources ab)))
          ;; Should have exactly 3 resources (not 4)
          (should (= (length resources) 3))

          ;; Verify none of the resources have the addressbook URL
          (dolist (resource resources)
            (should-not (string= (oref resource url) ab-url)))

          ;; Verify all expected vCard resources are present
          (let ((paths (mapcar (lambda (r) (oref r path)) resources)))
            (should (member "/addressbooks/user/contacts/alice.vcf" paths))
            (should (member "/addressbooks/user/contacts/bob.vcf" paths))
            (should (member "/addressbooks/user/contacts/charlie.vcf" paths))
            ;; Ensure the collection path is NOT in the list
            (should-not (member "/addressbooks/user/contacts/" paths)))))
    (ecard-carddav-test--teardown)))

(ert-deftest ecard-carddav-test-list-resources-without-content-type ()
  "Test listing resources when server doesn't return content-type property.
Some servers (like Radicale in certain configurations) may not return
the getcontenttype property in PROPFIND responses. We should still be
able to list resources by assuming all child items are vCards."
  (ecard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (ecard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (ecard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (ecard-carddav-discover-addressbooks server))
             (ab (car addressbooks)))

        ;; Add vCards
        (ecard-carddav-put-ecard
         ab "/addressbooks/user/contacts/david.vcf"
         (ecard-carddav-test--create-test-ecard "David Lee"))
        (ecard-carddav-put-ecard
         ab "/addressbooks/user/contacts/emma.vcf"
         (ecard-carddav-test--create-test-ecard "Emma Watson"))

        ;; Mock a PROPFIND response without content-type
        (cl-letf (((symbol-function 'ecard-carddav--parse-resources)
                   (lambda (xml addressbook base-url)
                     ;; Simulate response without content-type by parsing and
                     ;; removing content-type nodes
                     (let ((responses (ecard-carddav--dom-by-tag-qname xml 'response ecard-carddav-ns-dav))
                           (addressbook-url (oref addressbook url))
                           (resources nil))
                       (dolist (response responses)
                         (let* ((href-node (ecard-carddav--dom-by-tag-qname response 'href ecard-carddav-ns-dav))
                                (href (when href-node (dom-text (car href-node))))
                                (propstat (ecard-carddav--dom-by-tag-qname response 'propstat ecard-carddav-ns-dav))
                                (prop (when propstat (ecard-carddav--dom-by-tag-qname (car propstat) 'prop ecard-carddav-ns-dav)))
                                ;; Simulate missing content-type by not extracting it
                                (content-type nil))
                           ;; Apply same logic as real parser but with nil content-type
                           (when (and href
                                      (or (null content-type)
                                          (string-match-p "text/vcard" content-type)))
                             (let* ((url (ecard-carddav--resolve-url href base-url))
                                    (is-collection (string= url addressbook-url)))
                               (unless is-collection
                                 (let* ((etag-node (when prop (ecard-carddav--dom-by-tag-qname (car prop) 'getetag ecard-carddav-ns-dav)))
                                        (etag (when etag-node (dom-text (car etag-node))))
                                        (etag (when etag (string-trim etag "\"" "\"")))
                                        (path (url-filename (url-generic-parse-url url))))
                                   (push (ecard-carddav-resource
                                          :addressbook addressbook
                                          :url url
                                          :path path
                                          :etag etag)
                                         resources)))))))
                       (nreverse resources)))))

          (let ((resources (ecard-carddav-list-resources ab)))
            (should (= (length resources) 2))
            (let ((paths (mapcar (lambda (r) (oref r path)) resources)))
              (should (member "/addressbooks/user/contacts/david.vcf" paths))
              (should (member "/addressbooks/user/contacts/emma.vcf" paths))))))
    (ecard-carddav-test--teardown)))

(ert-deftest ecard-carddav-test-complete-workflow ()
  "Test complete CardDAV workflow end-to-end."
  (ecard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (ecard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (ecard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (cache-dir (expand-file-name "cache" ecard-carddav-test--temp-dir)))

        ;; Discover server structure
        (ecard-carddav-discover-principal server)
        (should (oref server principal-url))

        (ecard-carddav-discover-addressbook-home server)
        (should (oref server addressbook-home-url))

        (let ((addressbooks (ecard-carddav-discover-addressbooks server)))
          (should (= (length addressbooks) 1))

          (let ((ab (car addressbooks)))
            ;; Create contacts
            (ecard-carddav-put-ecard
             ab "/addressbooks/user/contacts/alice.vcf"
             (ecard-carddav-test--create-test-ecard "Alice Johnson"))
            (ecard-carddav-put-ecard
             ab "/addressbooks/user/contacts/bob.vcf"
             (ecard-carddav-test--create-test-ecard "Bob Williams"))

            ;; Set up sync
            (let ((sync (ecard-carddav-sync-create
                        :addressbook ab
                        :cache-dir cache-dir
                        :strategy :server-wins)))

              ;; Initial sync
              (let ((updated (ecard-carddav-sync-full sync)))
                (should (= (length updated) 2)))

              ;; Add another contact
              (ecard-carddav-put-ecard
               ab "/addressbooks/user/contacts/charlie.vcf"
               (ecard-carddav-test--create-test-ecard "Charlie Brown"))

              ;; Incremental sync
              (let ((result (ecard-carddav-sync-incremental sync)))
                (should (member "/addressbooks/user/contacts/charlie.vcf"
                               (plist-get result :added))))

              ;; Verify all contacts in cache
              (let ((all-contacts (ecard-carddav-sync-get-all-local sync)))
                (should (= (length all-contacts) 3))
                (let ((names (mapcar (lambda (pair)
                                      (ecard-get-property-value (cdr pair) 'fn))
                                    all-contacts)))
                  (should (member "Alice Johnson" names))
                  (should (member "Bob Williams" names))
                  (should (member "Charlie Brown" names))))))))
    (ecard-carddav-test--teardown)))

;;; UID Change Operation Tests

(ert-deftest ecard-carddav-test-change-uid-success ()
  "Test successful UID change operation."
  (unwind-protect
      (progn
        (ecard-carddav-test--setup)
        (let* ((server (ecard-carddav-server-create
                        :url "https://test.example.com"
                        :auth (ecard-carddav-auth-basic-create
                               :username "testuser"
                               :password "testpass")))
               (ab (car (ecard-carddav-discover-addressbooks server)))
               (old-uid "urn:uuid:old-uid-12345")
               (new-uid "urn:uuid:new-uid-67890")
               (test-ecard (ecard-create
                            :fn "Test Person"
                            :email "test@example.com"
                            :uid old-uid)))

          ;; Create initial contact
          (ecard-carddav-put-ecard ab "/addressbooks/user/contacts/old.vcf" test-ecard)

          ;; Verify old contact exists
          (let ((old-resource (ecard-carddav-get-resource ab "/addressbooks/user/contacts/old.vcf")))
            (should old-resource)
            (should (string= old-uid (ecard-get-property-value (oref old-resource ecard) 'uid))))

          ;; Change UID
          (let ((new-resource (ecard-carddav-change-uid ab "/addressbooks/user/contacts/old.vcf" new-uid)))
            (should new-resource)
            (should (string= new-uid (ecard-get-property-value (oref new-resource ecard) 'uid)))

            ;; Verify new contact exists at new path
            (let ((fetched (ecard-carddav-get-resource ab (oref new-resource url))))
              (should fetched)
              (should (string= new-uid (ecard-get-property-value (oref fetched ecard) 'uid)))
              (should (string= "Test Person" (ecard-get-property-value (oref fetched ecard) 'fn))))

            ;; Verify old contact is deleted
            (should-error (ecard-carddav-get-resource ab "/addressbooks/user/contacts/old.vcf")
                         :type 'ecard-carddav-not-found-error))))
    (ecard-carddav-test--teardown)))

(ert-deftest ecard-carddav-test-change-uid-nonexistent-resource ()
  "Test UID change fails for non-existent resource."
  (unwind-protect
      (progn
        (ecard-carddav-test--setup)
        (let* ((server (ecard-carddav-server-create
                        :url "https://test.example.com"
                        :auth (ecard-carddav-auth-basic-create
                               :username "testuser"
                               :password "testpass")))
               (ab (car (ecard-carddav-discover-addressbooks server))))

          ;; Try to change UID of non-existent contact
          (should-error (ecard-carddav-change-uid ab "/addressbooks/user/contacts/nonexistent.vcf"
                                                  "urn:uuid:new-uid")
                       :type 'ecard-carddav-not-found-error)))
    (ecard-carddav-test--teardown)))

(ert-deftest ecard-carddav-test-change-uid-preserves-properties ()
  "Test that UID change preserves all other properties."
  (unwind-protect
      (progn
        (ecard-carddav-test--setup)
        (let* ((server (ecard-carddav-server-create
                        :url "https://test.example.com"
                        :auth (ecard-carddav-auth-basic-create
                               :username "testuser"
                               :password "testpass")))
               (ab (car (ecard-carddav-discover-addressbooks server)))
               (old-uid "urn:uuid:preserve-old-123")
               (new-uid "urn:uuid:preserve-new-456")
               (test-ecard (ecard-create
                            :fn "John Doe"
                            :n (list "Doe" "John" "Q" "Dr." "Jr.")
                            :email "john@example.com"
                            :tel "+1-555-1234"
                            :org (list "Acme Corp" "Engineering")
                            :title "Senior Developer"
                            :note "Important contact"
                            :uid old-uid)))

          ;; Create initial contact
          (ecard-carddav-put-ecard ab "/addressbooks/user/contacts/preserve.vcf" test-ecard)

          ;; Change UID
          (let ((new-resource (ecard-carddav-change-uid ab "/addressbooks/user/contacts/preserve.vcf" new-uid)))
            (should new-resource)
            (let ((new-ecard (oref new-resource ecard)))
              ;; Verify UID changed
              (should (string= new-uid (ecard-get-property-value new-ecard 'uid)))

              ;; Verify all other properties preserved
              (should (string= "John Doe" (ecard-get-property-value new-ecard 'fn)))
              (should (equal (list "Doe" "John" "Q" "Dr." "Jr.")
                            (ecard-get-property-value new-ecard 'n)))
              (should (string= "john@example.com" (ecard-get-property-value new-ecard 'email)))
              (should (string= "+1-555-1234" (ecard-get-property-value new-ecard 'tel)))
              (should (equal (list "Acme Corp" "Engineering")
                            (ecard-get-property-value new-ecard 'org)))
              (should (string= "Senior Developer" (ecard-get-property-value new-ecard 'title)))
              (should (string= "Important contact" (ecard-get-property-value new-ecard 'note)))))))
    (ecard-carddav-test--teardown)))

(provide 'ecard-carddav-tests)
;;; ecard-carddav-tests.el ends here
