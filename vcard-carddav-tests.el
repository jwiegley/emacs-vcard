;;; vcard-carddav-tests.el --- Tests for CardDAV implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: comm, data, carddav, testing

;;; Commentary:

;; Comprehensive test suite for vcard-carddav modules.
;; Tests use the mock CardDAV server to avoid network dependencies.
;;
;; Run tests with:
;;   emacs -batch -L . -l vcard.el -l vcard-carddav-auth.el \
;;         -l vcard-carddav.el -l vcard-carddav-sync.el \
;;         -l vcard-carddav-mock.el -l vcard-carddav-tests.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Or interactively:
;;   M-x load-file RET vcard-carddav-tests.el RET
;;   M-x ert RET t RET

;;; Code:

(require 'ert)
(require 'vcard)
(require 'vcard-carddav-auth)
(require 'vcard-carddav)
(require 'vcard-carddav-sync)
(require 'vcard-carddav-mock)

;;; Test fixtures

(defvar vcard-carddav-test--mock-server nil
  "Mock server for testing.")

(defvar vcard-carddav-test--temp-dir nil
  "Temporary directory for cache files.")

(defun vcard-carddav-test--setup ()
  "Set up test fixtures."
  ;; Create temporary directory
  (setq vcard-carddav-test--temp-dir
        (make-temp-file "vcard-carddav-test-" t))

  ;; Create mock server
  (setq vcard-carddav-test--mock-server
        (vcard-carddav-mock-server-create
         :base-url "https://test.example.com"))

  ;; Add address book
  (vcard-carddav-mock-add-addressbook
   vcard-carddav-test--mock-server
   "/addressbooks/user/contacts/"
   "Test Contacts"
   "Test address book")

  ;; Install mock
  (vcard-carddav-mock-install vcard-carddav-test--mock-server))

(defun vcard-carddav-test--teardown ()
  "Tear down test fixtures."
  ;; Uninstall mock
  (vcard-carddav-mock-uninstall)

  ;; Clean up temp directory
  (when (and vcard-carddav-test--temp-dir
             (file-directory-p vcard-carddav-test--temp-dir))
    (delete-directory vcard-carddav-test--temp-dir t))

  (setq vcard-carddav-test--mock-server nil
        vcard-carddav-test--temp-dir nil))

(defun vcard-carddav-test--create-test-vcard (fn)
  "Create a test vCard with formatted name FN."
  (vcard-create
   :fn fn
   :n (list "Doe" "John" "" "" "")
   :email "john@example.com"
   :tel "+1-555-1234"
   :uid (format "test-%s@example.com" (downcase (replace-regexp-in-string " " "-" fn)))))

;;; Authentication tests

(ert-deftest vcard-carddav-test-auth-basic-create ()
  "Test Basic Auth credential creation."
  (let ((auth (vcard-carddav-auth-basic-create
               :username "testuser"
               :password "testpass")))
    (should (vcard-carddav-auth-basic-p auth))
    (should (eq (oref auth type) :basic))
    (should (string= (oref auth username) "testuser"))
    (should (string= (oref auth password) "testpass"))))

(ert-deftest vcard-carddav-test-auth-basic-header ()
  "Test Basic Auth header generation."
  (let ((auth (vcard-carddav-auth-basic-create
               :username "user"
               :password "pass")))
    (should (string= (vcard-carddav-auth-get-header auth)
                    "Basic dXNlcjpwYXNz"))))

(ert-deftest vcard-carddav-test-auth-basic-valid ()
  "Test Basic Auth validation."
  (let ((auth (vcard-carddav-auth-basic-create
               :username "user"
               :password "pass")))
    (should (vcard-carddav-auth-valid-p auth))))

(ert-deftest vcard-carddav-test-auth-bearer-create ()
  "Test Bearer token creation."
  (let ((auth (vcard-carddav-auth-bearer-create
               :token "abc123"
               :expires-at (+ (float-time) 3600))))
    (should (vcard-carddav-auth-bearer-p auth))
    (should (eq (oref auth type) :bearer))
    (should (string= (oref auth token) "abc123"))))

(ert-deftest vcard-carddav-test-auth-bearer-header ()
  "Test Bearer token header generation."
  (let ((auth (vcard-carddav-auth-bearer-create
               :token "mytoken123")))
    (should (string= (vcard-carddav-auth-get-header auth)
                    "Bearer mytoken123"))))

(ert-deftest vcard-carddav-test-auth-bearer-expired ()
  "Test Bearer token expiration."
  (let ((auth (vcard-carddav-auth-bearer-create
               :token "expired"
               :expires-at (- (float-time) 3600))))
    (should-not (vcard-carddav-auth-valid-p auth))))

;;; Core protocol tests

(ert-deftest vcard-carddav-test-server-create ()
  "Test CardDAV server creation."
  (let* ((auth (vcard-carddav-auth-basic-create
                :username "user"
                :password "pass"))
         (server (vcard-carddav-server-create
                  :url "https://example.com"
                  :auth auth)))
    (should (vcard-carddav-server-p server))
    (should (string= (oref server url) "https://example.com"))
    (should (eq (oref server auth) auth))))

(ert-deftest vcard-carddav-test-discover-principal ()
  "Test principal discovery."
  (vcard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (vcard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (vcard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth)))
        (vcard-carddav-discover-principal server)
        (should (string= (oref server principal-url) "https://test.example.com/principals/user/")))
    (vcard-carddav-test--teardown)))

(ert-deftest vcard-carddav-test-discover-addressbook-home ()
  "Test address book home discovery."
  (vcard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (vcard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (vcard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth)))
        (vcard-carddav-discover-addressbook-home server)
        (should (string= (oref server addressbook-home-url) "https://test.example.com/addressbooks/user/")))
    (vcard-carddav-test--teardown)))

(ert-deftest vcard-carddav-test-discover-addressbooks ()
  "Test address book discovery."
  (vcard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (vcard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (vcard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth)))
        (let ((addressbooks (vcard-carddav-discover-addressbooks server)))
          (should (= (length addressbooks) 1))
          (let ((ab (car addressbooks)))
            (should (string= (oref ab display-name) "Test Contacts"))
            (should (string= (oref ab description) "Test address book")))))
    (vcard-carddav-test--teardown)))

(ert-deftest vcard-carddav-test-put-and-get-vcard ()
  "Test creating and retrieving a vCard."
  (vcard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (vcard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (vcard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (vcard-carddav-discover-addressbooks server))
             (ab (car addressbooks))
             (vcard (vcard-carddav-test--create-test-vcard "John Doe")))

        ;; PUT vCard
        (let ((resource (vcard-carddav-put-vcard
                        ab
                        "/addressbooks/user/contacts/john.vcf"
                        vcard)))
          (should (vcard-carddav-resource-p resource))
          (should (oref resource etag)))

        ;; GET vCard
        (let ((resource (vcard-carddav-get-vcard
                        ab
                        "/addressbooks/user/contacts/john.vcf")))
          (should (vcard-carddav-resource-p resource))
          (should (vcard-p (oref resource vcard)))
          (should (string= (vcard-get-property-value (oref resource vcard) 'fn)
                          "John Doe"))))
    (vcard-carddav-test--teardown)))

(ert-deftest vcard-carddav-test-list-resources ()
  "Test listing resources in address book."
  (vcard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (vcard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (vcard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (vcard-carddav-discover-addressbooks server))
             (ab (car addressbooks)))

        ;; Add some vCards
        (vcard-carddav-put-vcard
         ab "/addressbooks/user/contacts/john.vcf"
         (vcard-carddav-test--create-test-vcard "John Doe"))
        (vcard-carddav-put-vcard
         ab "/addressbooks/user/contacts/jane.vcf"
         (vcard-carddav-test--create-test-vcard "Jane Smith"))

        ;; List resources
        (let ((resources (vcard-carddav-list-resources ab)))
          (should (= (length resources) 2))
          (should (member "/addressbooks/user/contacts/john.vcf"
                         (mapcar (lambda (r) (oref r path)) resources)))
          (should (member "/addressbooks/user/contacts/jane.vcf"
                         (mapcar (lambda (r) (oref r path)) resources)))))
    (vcard-carddav-test--teardown)))

(ert-deftest vcard-carddav-test-update-with-etag ()
  "Test updating vCard with ETag validation."
  (vcard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (vcard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (vcard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (vcard-carddav-discover-addressbooks server))
             (ab (car addressbooks))
             (vcard1 (vcard-carddav-test--create-test-vcard "John Doe")))

        ;; Create initial vCard
        (let ((resource1 (vcard-carddav-put-vcard
                         ab "/addressbooks/user/contacts/john.vcf"
                         vcard1)))
          (should (oref resource1 etag))

          ;; Update with correct ETag
          (let ((vcard2 (vcard-carddav-test--create-test-vcard "John Smith")))
            (let ((resource2 (vcard-carddav-put-vcard
                             ab "/addressbooks/user/contacts/john.vcf"
                             vcard2
                             (oref resource1 etag))))
              (should (not (string= (oref resource1 etag)
                                   (oref resource2 etag))))))))
    (vcard-carddav-test--teardown)))

(ert-deftest vcard-carddav-test-update-with-wrong-etag ()
  "Test ETag conflict detection."
  (vcard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (vcard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (vcard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (vcard-carddav-discover-addressbooks server))
             (ab (car addressbooks))
             (vcard1 (vcard-carddav-test--create-test-vcard "John Doe")))

        ;; Create initial vCard
        (vcard-carddav-put-vcard
         ab "/addressbooks/user/contacts/john.vcf"
         vcard1)

        ;; Try to update with wrong ETag
        (let ((vcard2 (vcard-carddav-test--create-test-vcard "John Smith")))
          (should-error
           (vcard-carddav-put-vcard
            ab "/addressbooks/user/contacts/john.vcf"
            vcard2
            "wrong-etag")
           :type 'vcard-carddav-conflict-error)))
    (vcard-carddav-test--teardown)))

(ert-deftest vcard-carddav-test-delete-resource ()
  "Test deleting a vCard resource."
  (vcard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (vcard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (vcard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (vcard-carddav-discover-addressbooks server))
             (ab (car addressbooks))
             (vcard (vcard-carddav-test--create-test-vcard "John Doe")))

        ;; Create vCard
        (vcard-carddav-put-vcard
         ab "/addressbooks/user/contacts/john.vcf"
         vcard)

        ;; Delete it
        (should (vcard-carddav-delete-resource
                ab "/addressbooks/user/contacts/john.vcf"))

        ;; Verify it's gone
        (should-error
         (vcard-carddav-get-vcard
          ab "/addressbooks/user/contacts/john.vcf")
         :type 'vcard-carddav-not-found-error))
    (vcard-carddav-test--teardown)))

;;; Synchronization tests

(ert-deftest vcard-carddav-test-sync-create ()
  "Test sync manager creation."
  (vcard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (vcard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (vcard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (vcard-carddav-discover-addressbooks server))
             (ab (car addressbooks))
             (sync (vcard-carddav-sync-create
                    :addressbook ab
                    :cache-dir (expand-file-name "cache" vcard-carddav-test--temp-dir)
                    :strategy :server-wins)))
        (should (vcard-carddav-sync-p sync))
        (should (eq (oref sync strategy) :server-wins)))
    (vcard-carddav-test--teardown)))

(ert-deftest vcard-carddav-test-sync-full ()
  "Test full synchronization."
  (vcard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (vcard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (vcard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (vcard-carddav-discover-addressbooks server))
             (ab (car addressbooks))
             (sync (vcard-carddav-sync-create
                    :addressbook ab
                    :cache-dir (expand-file-name "cache" vcard-carddav-test--temp-dir))))

        ;; Add some vCards to server
        (vcard-carddav-put-vcard
         ab "/addressbooks/user/contacts/john.vcf"
         (vcard-carddav-test--create-test-vcard "John Doe"))
        (vcard-carddav-put-vcard
         ab "/addressbooks/user/contacts/jane.vcf"
         (vcard-carddav-test--create-test-vcard "Jane Smith"))

        ;; Perform full sync
        (let ((updated (vcard-carddav-sync-full sync)))
          (should (= (length updated) 2))

          ;; Verify local cache
          (let ((local-john (vcard-carddav-sync-get-local
                            sync "/addressbooks/user/contacts/john.vcf"))
                (local-jane (vcard-carddav-sync-get-local
                            sync "/addressbooks/user/contacts/jane.vcf")))
            (should (vcard-p local-john))
            (should (vcard-p local-jane))
            (should (string= (vcard-get-property-value local-john 'fn) "John Doe"))
            (should (string= (vcard-get-property-value local-jane 'fn) "Jane Smith")))))
    (vcard-carddav-test--teardown)))

(ert-deftest vcard-carddav-test-sync-incremental ()
  "Test incremental synchronization."
  (vcard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (vcard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (vcard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (vcard-carddav-discover-addressbooks server))
             (ab (car addressbooks))
             (sync (vcard-carddav-sync-create
                    :addressbook ab
                    :cache-dir (expand-file-name "cache" vcard-carddav-test--temp-dir))))

        ;; Add initial vCard
        (vcard-carddav-put-vcard
         ab "/addressbooks/user/contacts/john.vcf"
         (vcard-carddav-test--create-test-vcard "John Doe"))

        ;; Initial sync
        (vcard-carddav-sync-full sync)

        ;; Add another vCard
        (vcard-carddav-put-vcard
         ab "/addressbooks/user/contacts/jane.vcf"
         (vcard-carddav-test--create-test-vcard "Jane Smith"))

        ;; Incremental sync
        (let ((result (vcard-carddav-sync-incremental sync)))
          (should (member "/addressbooks/user/contacts/jane.vcf"
                         (plist-get result :added)))

          ;; Verify both are in local cache
          (let ((all-local (vcard-carddav-sync-get-all-local sync)))
            (should (= (length all-local) 2)))))
    (vcard-carddav-test--teardown)))

(ert-deftest vcard-carddav-test-sync-cache-persistence ()
  "Test cache index persistence."
  (vcard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (vcard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (vcard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (addressbooks (vcard-carddav-discover-addressbooks server))
             (ab (car addressbooks))
             (cache-dir (expand-file-name "cache" vcard-carddav-test--temp-dir)))

        ;; Create sync manager and add data
        (let ((sync1 (vcard-carddav-sync-create
                     :addressbook ab
                     :cache-dir cache-dir)))
          (vcard-carddav-put-vcard
           ab "/addressbooks/user/contacts/john.vcf"
           (vcard-carddav-test--create-test-vcard "John Doe"))
          (vcard-carddav-sync-full sync1))

        ;; Create new sync manager with same cache dir
        (let ((sync2 (vcard-carddav-sync-create
                     :addressbook ab
                     :cache-dir cache-dir)))
          (vcard-carddav-sync--load-cache-index sync2)

          ;; Verify data persisted
          (let ((local (vcard-carddav-sync-get-local
                       sync2 "/addressbooks/user/contacts/john.vcf")))
            (should (vcard-p local))
            (should (string= (vcard-get-property-value local 'fn) "John Doe")))))
    (vcard-carddav-test--teardown)))

;;; Mock server tests

(ert-deftest vcard-carddav-test-mock-server-create ()
  "Test mock server creation."
  (let ((mock (vcard-carddav-mock-server-create
               :base-url "https://test.mock.com")))
    (should (vcard-carddav-mock-server-p mock))
    (should (string= (oref mock base-url) "https://test.mock.com"))))

(ert-deftest vcard-carddav-test-mock-add-addressbook ()
  "Test adding address book to mock server."
  (let ((mock (vcard-carddav-mock-server-create)))
    (vcard-carddav-mock-add-addressbook
     mock "/test/ab/" "Test" "Test AB")
    (should (gethash "/test/ab/" (oref mock addressbooks)))))

(ert-deftest vcard-carddav-test-mock-put-vcard ()
  "Test adding vCard to mock server."
  (let ((mock (vcard-carddav-mock-server-create))
        (vcard (vcard-carddav-test--create-test-vcard "Test User")))
    (vcard-carddav-mock-add-addressbook
     mock "/test/ab/" "Test" "Test AB")
    (let ((etag (vcard-carddav-mock-put-vcard
                 mock "/test/ab/test.vcf" vcard)))
      (should (stringp etag))
      (let ((ab (gethash "/test/ab/" (oref mock addressbooks))))
        (should (gethash "/test/ab/test.vcf" (oref ab resources)))))))

;;; Integration tests

(ert-deftest vcard-carddav-test-complete-workflow ()
  "Test complete CardDAV workflow end-to-end."
  (vcard-carddav-test--setup)
  (unwind-protect
      (let* ((auth (vcard-carddav-auth-basic-create
                    :username "user"
                    :password "pass"))
             (server (vcard-carddav-server-create
                      :url "https://test.example.com"
                      :auth auth))
             (cache-dir (expand-file-name "cache" vcard-carddav-test--temp-dir)))

        ;; Discover server structure
        (vcard-carddav-discover-principal server)
        (should (oref server principal-url))

        (vcard-carddav-discover-addressbook-home server)
        (should (oref server addressbook-home-url))

        (let ((addressbooks (vcard-carddav-discover-addressbooks server)))
          (should (= (length addressbooks) 1))

          (let ((ab (car addressbooks)))
            ;; Create contacts
            (vcard-carddav-put-vcard
             ab "/addressbooks/user/contacts/alice.vcf"
             (vcard-carddav-test--create-test-vcard "Alice Johnson"))
            (vcard-carddav-put-vcard
             ab "/addressbooks/user/contacts/bob.vcf"
             (vcard-carddav-test--create-test-vcard "Bob Williams"))

            ;; Set up sync
            (let ((sync (vcard-carddav-sync-create
                        :addressbook ab
                        :cache-dir cache-dir
                        :strategy :server-wins)))

              ;; Initial sync
              (let ((updated (vcard-carddav-sync-full sync)))
                (should (= (length updated) 2)))

              ;; Add another contact
              (vcard-carddav-put-vcard
               ab "/addressbooks/user/contacts/charlie.vcf"
               (vcard-carddav-test--create-test-vcard "Charlie Brown"))

              ;; Incremental sync
              (let ((result (vcard-carddav-sync-incremental sync)))
                (should (member "/addressbooks/user/contacts/charlie.vcf"
                               (plist-get result :added))))

              ;; Verify all contacts in cache
              (let ((all-contacts (vcard-carddav-sync-get-all-local sync)))
                (should (= (length all-contacts) 3))
                (let ((names (mapcar (lambda (pair)
                                      (vcard-get-property-value (cdr pair) 'fn))
                                    all-contacts)))
                  (should (member "Alice Johnson" names))
                  (should (member "Bob Williams" names))
                  (should (member "Charlie Brown" names))))))))
    (vcard-carddav-test--teardown)))

(provide 'vcard-carddav-tests)
;;; vcard-carddav-tests.el ends here
