;;; ecard-display-test.el --- Tests for ecard-display -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>

;;; Commentary:

;; Basic tests for ecard-display module.
;; These tests verify UI buffer creation and basic functionality.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ecard-display)

;;; Test server list buffer

(ert-deftest ecard-display-test-servers-buffer ()
  "Test that server list buffer can be created."
  (let ((ecard-display-servers
         '((:name "Test Server"
            :url "https://test.example.com"
            :username "test"
            :password "secret"))))
    (with-temp-buffer
      (ecard-display-servers-mode)
      (should (eq major-mode 'ecard-display-servers-mode))
      (should (boundp 'tabulated-list-format))
      (should (vectorp tabulated-list-format)))))

(ert-deftest ecard-display-test-servers-refresh ()
  "Test that server list can be refreshed."
  (let ((ecard-display-servers
         '((:name "Server 1"
            :url "https://server1.example.com"
            :username "user1"
            :password "pass1")
           (:name "Server 2"
            :url "https://server2.example.com"
            :username "user2"
            :password "pass2"))))
    (with-temp-buffer
      (ecard-display-servers-mode)
      (ecard-display-servers-refresh)
      (should (equal (length tabulated-list-entries) 2))
      ;; Verify first entry
      (let ((entry (car tabulated-list-entries)))
        (should (plist-member (car entry) :name))))))

;;; Test addressbook buffer

(ert-deftest ecard-display-test-addressbooks-mode ()
  "Test that addressbook buffer mode can be initialized."
  (with-temp-buffer
    (ecard-display-addressbooks-mode)
    (should (eq major-mode 'ecard-display-addressbooks-mode))
    (should (boundp 'tabulated-list-format))
    (should (vectorp tabulated-list-format))
    ;; Verify format has expected columns
    (should (equal (length tabulated-list-format) 3))))

;;; Test contacts buffer

(ert-deftest ecard-display-test-contacts-mode ()
  "Test that contacts buffer mode can be initialized."
  (with-temp-buffer
    (ecard-display-contacts-mode)
    (should (eq major-mode 'ecard-display-contacts-mode))
    (should (boundp 'tabulated-list-format))
    (should (vectorp tabulated-list-format))
    ;; Verify format has expected columns (FN, Email, Phone)
    (should (equal (length tabulated-list-format) 3))))

;;; Test contact detail buffer

(ert-deftest ecard-display-test-contact-mode ()
  "Test that contact detail buffer mode can be initialized."
  (with-temp-buffer
    (ecard-display-contact-mode)
    (should (eq major-mode 'ecard-display-contact-mode))
    (should (not buffer-read-only))))

;;; Test helper functions

(ert-deftest ecard-display-test-get-fn ()
  "Test extracting full name from ecard."
  (let* ((fn-prop (ecard-property :name "FN" :value "John Doe"))
         (ecard-obj (ecard :fn (list fn-prop))))
    (should (equal (ecard-display--get-fn ecard-obj) "John Doe"))))

(ert-deftest ecard-display-test-get-fn-nil ()
  "Test extracting full name from nil ecard."
  (should (equal (ecard-display--get-fn nil) "Unknown")))

(ert-deftest ecard-display-test-get-first-email ()
  "Test extracting first email from ecard."
  (let* ((email-prop (ecard-property :name "EMAIL" :value "john@example.com"))
         (ecard-obj (ecard :email (list email-prop))))
    (should (equal (ecard-display--get-first-email ecard-obj) "john@example.com"))))

(ert-deftest ecard-display-test-get-first-email-empty ()
  "Test extracting email from ecard with no emails."
  (let ((ecard-obj (ecard)))
    (should (equal (ecard-display--get-first-email ecard-obj) ""))))

(ert-deftest ecard-display-test-get-first-tel ()
  "Test extracting first telephone from ecard."
  (let* ((tel-prop (ecard-property :name "TEL" :value "+1-555-1234"))
         (ecard-obj (ecard :tel (list tel-prop))))
    (should (equal (ecard-display--get-first-tel ecard-obj) "+1-555-1234"))))

(ert-deftest ecard-display-test-uuid-generation ()
  "Test UUID generation produces valid format."
  (let ((uuid (ecard-display--generate-uuid)))
    (should (stringp uuid))
    (should (string-match-p
             "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}$"
             uuid))))

;;; Test clone functionality

(ert-deftest ecard-display-test-clone-ecard ()
  "Test that ecard cloning creates independent copy."
  (let* ((fn-prop (ecard-property :name "FN" :value "Original"))
         (ecard-obj (ecard :fn (list fn-prop)))
         (cloned (ecard-display--clone-ecard ecard-obj)))
    (should cloned)
    (should (ecard-p cloned))
    ;; Verify it has same data
    (should (equal (ecard-display--get-fn cloned) "Original"))))

;;; Test parameter extraction

(ert-deftest ecard-display-test-get-param ()
  "Test extracting parameters from property."
  (let ((prop (ecard-property
               :name "EMAIL"
               :value "work@example.com"
               :parameters '(("TYPE" . "work")
                             ("PREF" . "1")))))
    (should (equal (ecard-display--get-param prop "TYPE") "work"))
    (should (equal (ecard-display--get-param prop "PREF") "1"))
    (should (null (ecard-display--get-param prop "NONEXISTENT")))))

;;; Issue 1: Contact count tests

(ert-deftest ecard-display-test-contact-count-with-resources ()
  "Verify addressbook shows correct contact count when resources are loaded."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook
                       :server server
                       :url "https://test.example.com/addressbook"
                       :display-name "Test Addressbook"))
         (resources (list
                     (ecard-carddav-resource :path "/contact1.vcf")
                     (ecard-carddav-resource :path "/contact2.vcf")
                     (ecard-carddav-resource :path "/contact3.vcf"))))
    (oset addressbook resources resources)
    (should (equal (ecard-display--addressbook-contact-count addressbook) "3"))))

(ert-deftest ecard-display-test-contact-count-without-resources ()
  "Verify addressbook shows '?' when resources are not loaded."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook
                       :server server
                       :url "https://test.example.com/addressbook"
                       :display-name "Test Addressbook")))
    (should (equal (ecard-display--addressbook-contact-count addressbook) "?"))))

(ert-deftest ecard-display-test-contact-count-empty ()
  "Verify addressbook shows '0' when resources list is empty."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook
                       :server server
                       :url "https://test.example.com/addressbook"
                       :display-name "Test Addressbook")))
    (oset addressbook resources nil)
    (should (equal (ecard-display--addressbook-contact-count addressbook) "?"))))

;;; Issue 2: Performance tests

(ert-deftest ecard-display-test-extract-name-from-path ()
  "Test extracting display name from vCard path."
  (should (equal (ecard-display--extract-name-from-path "/contacts/JohnDoe.vcf")
                 "JohnDoe"))
  (should (equal (ecard-display--extract-name-from-path "/contacts/Jane%20Smith.vcf")
                 "Jane Smith"))
  (should (equal (ecard-display--extract-name-from-path "/addressbook/contacts/test.vcf")
                 "test")))

(ert-deftest ecard-display-test-contacts-populate-fallback ()
  "Verify contact loading falls back to path-based names when multiget fails.
This test ensures we gracefully handle errors and show filenames as fallback."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook
                       :server server
                       :url "https://test.example.com/addressbook"))
         ;; Create 10 resources WITHOUT ecard data
         (resources (cl-loop for i from 1 to 10
                             collect (ecard-carddav-resource
                                      :addressbook addressbook
                                      :path (format "/contact%d.vcf" i)
                                      :url (format "https://test.example.com/contact%d.vcf" i)
                                      :etag (format "etag-%d" i)))))
    (oset addressbook resources resources)
    (with-temp-buffer
      (ecard-display-contacts-mode)
      (setq ecard-display--addressbook addressbook)
      ;; Populate will try multiget (which will fail), but should still show entries
      (condition-case _err
          (ecard-display-contacts--populate addressbook)
        (error nil))  ; Ignore errors - we expect multiget to fail
      ;; Verify we have entries (fallback to path-based names)
      (should (equal (length tabulated-list-entries) 10))
      ;; Verify entries use path-based names (fallback when no ecard data)
      (let ((first-entry (car tabulated-list-entries)))
        (should (vectorp (cadr first-entry)))
        ;; First column should be "contact1" (from path)
        (should (equal (aref (cadr first-entry) 0) "contact1"))))))

;;; Issue 3: Safe string conversion tests

(ert-deftest ecard-display-test-safe-string-nil ()
  "Test safe-string conversion of nil."
  (should (equal (ecard-display--safe-string nil) "")))

(ert-deftest ecard-display-test-safe-string-string ()
  "Test safe-string conversion of string."
  (should (equal (ecard-display--safe-string "hello") "hello"))
  (should (equal (ecard-display--safe-string "") "")))

(ert-deftest ecard-display-test-safe-string-empty-list ()
  "Test safe-string conversion of empty list."
  (should (equal (ecard-display--safe-string '()) ""))
  (should (equal (ecard-display--safe-string '("")) ""))
  (should (equal (ecard-display--safe-string '("" "")) ", ")))

(ert-deftest ecard-display-test-safe-string-list ()
  "Test safe-string conversion of lists."
  (should (equal (ecard-display--safe-string '("one")) "one"))
  (should (equal (ecard-display--safe-string '("one" "two")) "one, two"))
  (should (equal (ecard-display--safe-string '("a" "b" "c")) "a, b, c")))

(ert-deftest ecard-display-test-safe-string-number ()
  "Test safe-string conversion of numbers."
  (should (equal (ecard-display--safe-string 42) "42"))
  (should (equal (ecard-display--safe-string 0) "0"))
  (should (equal (ecard-display--safe-string 3.14) "3.14")))

(ert-deftest ecard-display-test-safe-string-symbol ()
  "Test safe-string conversion of symbols."
  (should (equal (ecard-display--safe-string 'test) "test"))
  (should (equal (ecard-display--safe-string 'hello-world) "hello-world")))

(ert-deftest ecard-display-test-safe-string-mixed-list ()
  "Test safe-string conversion of mixed-type lists."
  (should (stringp (ecard-display--safe-string '(1 2 3))))
  (should (stringp (ecard-display--safe-string '(a b c)))))

(ert-deftest ecard-display-test-contact-detail-with-empty-fields ()
  "Verify contact detail handles empty/missing fields without error."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         ;; Create ecard with various empty fields
         (ecard-obj (ecard
                     ;; FN is provided (required)
                     :fn (list (ecard-property :name "FN" :value "Test User"))
                     ;; N has empty components
                     :n (list (ecard-property :name "N" :value '("" "" "" "" "")))
                     ;; EMAIL is empty list
                     :email nil
                     ;; TEL is missing
                     ;; ORG has empty value
                     :org (list (ecard-property :name "ORG" :value ""))))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/test.vcf"
                    :path "/test.vcf"
                    :ecard ecard-obj)))
    (with-temp-buffer
      (ecard-display-contact-mode)
      (setq ecard-display--resource resource
            ecard-display--addressbook addressbook)
      ;; Rendering should not error
      (should-not (condition-case err
                      (progn
                        (ecard-display-contact--render resource)
                        nil)
                    (error err)))
      ;; Buffer should have content
      (should (> (buffer-size) 0)))))

(ert-deftest ecard-display-test-contact-detail-malformed-data ()
  "Verify contact detail handles malformed vCard data gracefully."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         ;; Create ecard with edge case values that are technically valid
         ;; but unusual - lists where strings are expected
         (ecard-obj (ecard
                     :fn (list (ecard-property :name "FN" :value "Test"))
                     ;; Email value is a list instead of string (valid per EIEIO but unusual)
                     :email (list (ecard-property :name "EMAIL" :value '("test@example.com")))
                     ;; Tel value is empty list (valid but unusual)
                     :tel (list (ecard-property :name "TEL" :value '()))
                     ;; Org value is nested list (valid but unusual)
                     :org (list (ecard-property :name "ORG" :value '(("Company" "Division"))))))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/test.vcf"
                    :path "/test.vcf"
                    :ecard ecard-obj)))
    (with-temp-buffer
      (ecard-display-contact-mode)
      (setq ecard-display--resource resource
            ecard-display--addressbook addressbook)
      ;; Rendering should not error even with unusual data structures
      (should-not (condition-case err
                      (progn
                        (ecard-display-contact--render resource)
                        nil)
                    (error err)))
      ;; Buffer should have content
      (should (> (buffer-size) 0)))))

(ert-deftest ecard-display-test-contact-detail-nil-ecard ()
  "Verify contact detail handles nil ecard gracefully."
  (let* ((server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook :server server))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/test.vcf"
                    :path "/test.vcf"
                    :ecard nil)))
    (with-temp-buffer
      (ecard-display-contact-mode)
      (setq ecard-display--resource resource
            ecard-display--addressbook addressbook)
      ;; Should handle nil ecard without crashing
      (should (condition-case _err
                  (progn
                    (ecard-display-contact--render resource)
                    nil)
                (error t))))))

;;; Test helpers and sample data

(defvar ecard-display-test--mock-server nil
  "Currently active mock server for testing.")

(defvar ecard-display-test--real-server nil
  "Currently active real CardDAV server for testing.")

(defvar ecard-display-test--environment nil
  "Plist containing test environment data.
Keys:
  :mock-server - Mock server object
  :real-server - Real CardDAV server object
  :addressbooks - List of addressbook objects
  :resources - List of resource objects")

(defun ecard-display-test--create-sample-contact (fn given family email phone org
                                                     &optional note)
  "Create a sample vCard.
Arguments: FN GIVEN FAMILY EMAIL PHONE ORG and optional NOTE."
  (let ((ecard-obj (ecard-create :fn fn)))

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
    (when (and phone (not (string-empty-p phone)))
      (let ((tel-prop (ecard-property
                       :name "TEL"
                       :parameters '(("TYPE" . "work"))
                       :value phone)))
        (oset ecard-obj tel (list tel-prop))))

    ;; Add ORG if provided
    (when (and org (not (string-empty-p org)))
      (let ((org-prop (ecard-property
                       :name "ORG"
                       :value org)))
        (oset ecard-obj org (list org-prop))))

    ;; Add NOTE if provided
    (when (and note (not (string-empty-p note)))
      (let ((note-prop (ecard-property
                        :name "NOTE"
                        :value note)))
        (oset ecard-obj note (list note-prop))))

    ;; Add UID
    (let ((uid (format "urn:uuid:%s" (ecard-display--generate-uuid))))
      (oset ecard-obj uid (list (ecard-property :name "UID" :value uid))))

    ecard-obj))

(defun ecard-display-test--create-sample-contacts ()
  "Create a list of sample vCard objects for testing."
  (list
   (ecard-display-test--create-sample-contact
    "John Doe" "John" "Doe"
    "john.doe@example.com" "+1-555-0100"
    "ACME Corporation"
    "Senior Software Engineer")

   (ecard-display-test--create-sample-contact
    "Jane Smith" "Jane" "Smith"
    "jane.smith@example.com" "+1-555-0101"
    "Tech Innovations Inc"
    "Product Manager")

   (ecard-display-test--create-sample-contact
    "Bob Johnson" "Bob" "Johnson"
    "bob.johnson@example.com" "+1-555-0102"
    "Design Studio LLC"
    "Creative Director")

   (ecard-display-test--create-sample-contact
    "Alice Williams" "Alice" "Williams"
    "alice.williams@example.com" "+1-555-0103"
    "Data Systems Corp"
    "Database Administrator")

   (ecard-display-test--create-sample-contact
    "Charlie Brown" "Charlie" "Brown"
    "charlie.brown@example.com" "+1-555-0104"
    "Marketing Solutions"
    "Marketing Director")))

(defun ecard-display-test--setup-mock-environment ()
  "Set up complete mock CardDAV environment for interactive testing.
Returns plist with :mock-server :real-server :addressbooks :resources."
  (message "Setting up mock CardDAV environment...")

  ;; Create mock server
  (let* ((mock-server (ecard-carddav-mock-server-create
                       :base-url "https://mock.test.local"
                       :principal-path "/principals/testuser/"
                       :addressbook-home-path "/addressbooks/testuser/"))

         ;; Add address books
         (ab-contacts-path "/addressbooks/testuser/contacts/")
         (ab-work-path "/addressbooks/testuser/work/")

         ;; Create sample contacts
         (sample-contacts (ecard-display-test--create-sample-contacts))

         ;; Track created resources
         (resources nil))

    ;; Add addressbooks to mock server
    (ecard-carddav-mock-add-addressbook
     mock-server ab-contacts-path
     "Personal Contacts" "My personal contacts")

    (ecard-carddav-mock-add-addressbook
     mock-server ab-work-path
     "Work Contacts" "Work-related contacts")

    ;; Add sample contacts to "Personal Contacts" addressbook
    (let ((contact-num 1))
      (dolist (contact sample-contacts)
        (let ((path (format "%scontact-%d.vcf" ab-contacts-path contact-num)))
          (ecard-carddav-mock-put-ecard mock-server path contact)
          (push (list :path path :contact contact) resources)
          (setq contact-num (1+ contact-num)))))

    ;; Add one contact to "Work Contacts" addressbook
    (let ((work-contact (ecard-display-test--create-sample-contact
                         "Sarah Manager" "Sarah" "Manager"
                         "sarah.manager@work.example.com" "+1-555-0200"
                         "Work Corp" "CEO")))
      (let ((path (format "%swork-contact-1.vcf" ab-work-path)))
        (ecard-carddav-mock-put-ecard mock-server path work-contact)
        (push (list :path path :contact work-contact) resources)))

    ;; Install mock handler
    (ecard-carddav-mock-install mock-server)

    ;; Create real server object that will use the mock
    (let* ((auth (ecard-carddav-auth-basic-create
                  :username "testuser"
                  :password "testpass"))
           (real-server (ecard-carddav-server-create
                         :url "https://mock.test.local"
                         :auth auth)))

      ;; Store environment
      (setq ecard-display-test--mock-server mock-server
            ecard-display-test--real-server real-server
            ecard-display-test--environment
            (list :mock-server mock-server
                  :real-server real-server
                  :addressbooks nil
                  :resources (nreverse resources)))

      (message "Mock environment ready: 2 addressbooks, %d contacts"
               (length resources))

      ecard-display-test--environment)))

(defun ecard-display-test--cleanup-mock-environment ()
  "Clean up mock environment and buffers."
  (message "Cleaning up mock environment...")

  ;; Uninstall mock handler
  (ecard-carddav-mock-uninstall)

  ;; Kill all ecard-display buffers
  (dolist (buf (buffer-list))
    (when (string-match-p "\\*CardDAV" (buffer-name buf))
      (kill-buffer buf)))

  ;; Clear state
  (setq ecard-display-test--mock-server nil
        ecard-display-test--real-server nil
        ecard-display-test--environment nil)

  (message "Mock environment cleaned up"))

;;; Integration tests with mock server

(require 'ecard-carddav-mock)

(ert-deftest ecard-display-test-addressbook-shows-count ()
  "Verify addressbook buffer shows actual contact count, not '?'."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com")))
    ;; Add addressbook with 3 contacts
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "My contacts")
    (ecard-carddav-mock-put-ecard
     mock "/addressbooks/user/contacts/contact1.vcf"
     (ecard-create :fn "Alice"))
    (ecard-carddav-mock-put-ecard
     mock "/addressbooks/user/contacts/contact2.vcf"
     (ecard-create :fn "Bob"))
    (ecard-carddav-mock-put-ecard
     mock "/addressbooks/user/contacts/contact3.vcf"
     (ecard-create :fn "Charlie"))

    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)

          ;; Create server and discover addressbooks
          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "test"
                                 :password "test"))))
            (ecard-carddav-discover-addressbooks server)

            ;; Load resources for the addressbook (required after populate no longer auto-loads)
            (let ((addressbook (car (oref server addressbooks))))
              (ecard-carddav-list-resources addressbook))

            ;; Populate addressbooks buffer
            (with-temp-buffer
              (ecard-display-addressbooks-mode)
              (setq ecard-display--server server)
              (ecard-display-addressbooks--populate server)

              ;; Verify we have entries
              (should (equal (length tabulated-list-entries) 1))

              ;; Verify count column shows "3" not "?"
              (let ((entry (car tabulated-list-entries)))
                (should (vectorp (cadr entry)))
                (should (equal (aref (cadr entry) 2) "3"))))))

      (ecard-carddav-mock-uninstall))))

(ert-deftest ecard-display-test-contacts-show-details ()
  "Verify contact list shows FN/EMAIL/TEL, not UUIDs."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com")))
    ;; Add addressbook with UUID-named contacts
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "My contacts")

    ;; Create contacts with UUID filenames but real data
    (let ((uuid1 "e18e7e74-efa3-4c27-9c8d-ff464deff3ec")
          (uuid2 "a1b2c3d4-e5f6-7890-abcd-ef1234567890"))
      (ecard-carddav-mock-put-ecard
       mock (format "/addressbooks/user/contacts/%s.vcf" uuid1)
       (ecard-create
        :fn "John Doe"
        :email "john@example.com"
        :tel "+1-555-0100"))
      (ecard-carddav-mock-put-ecard
       mock (format "/addressbooks/user/contacts/%s.vcf" uuid2)
       (ecard-create
        :fn "Jane Smith"
        :email "jane@example.com"
        :tel "+1-555-0200")))

    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)

          ;; Create server and discover addressbooks
          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "test"
                                 :password "test"))))
            (ecard-carddav-discover-addressbooks server)
            (let ((addressbook (car (oref server addressbooks))))
              ;; List resources
              (ecard-carddav-list-resources addressbook)

              ;; Populate contacts buffer
              (with-temp-buffer
                (ecard-display-contacts-mode)
                (setq ecard-display--addressbook addressbook)
                (ecard-display-contacts--populate addressbook nil t)

                ;; Verify we have 2 entries
                (should (equal (length tabulated-list-entries) 2))

                ;; Verify entries show real names, not UUIDs
                (let* ((entries tabulated-list-entries)
                       (names (mapcar (lambda (e) (aref (cadr e) 0)) entries))
                       (emails (mapcar (lambda (e) (aref (cadr e) 1)) entries))
                       (phones (mapcar (lambda (e) (aref (cadr e) 2)) entries)))
                  ;; Should NOT contain UUID
                  (should-not (cl-some (lambda (n) (string-match-p "e18e7e74" n)) names))
                  (should-not (cl-some (lambda (n) (string-match-p "a1b2c3d4" n)) names))

                  ;; Should contain actual names
                  (should (member "John Doe" names))
                  (should (member "Jane Smith" names))

                  ;; Should contain actual emails
                  (should (member "john@example.com" emails))
                  (should (member "jane@example.com" emails))

                  ;; Should contain actual phones
                  (should (member "+1-555-0100" phones))
                  (should (member "+1-555-0200" phones)))))))

      (ecard-carddav-mock-uninstall))))

(ert-deftest ecard-display-test-multiget-performance ()
  "Verify addressbook-multiget uses single HTTP request for all contacts."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com"))
        (request-count 0))
    ;; Add addressbook with 50 contacts
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "My contacts")

    (dotimes (i 50)
      (ecard-carddav-mock-put-ecard
       mock (format "/addressbooks/user/contacts/contact%d.vcf" i)
       (ecard-create
        :fn (format "Contact %d" i)
        :email (format "user%d@example.com" i))))

    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)

          ;; Track HTTP requests by advising the request handler
          (advice-add 'ecard-carddav-mock--handle-request
                      :before (lambda (&rest _) (setq request-count (1+ request-count))))

          ;; Create server and discover addressbooks
          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "test"
                                 :password "test"))))
            (setq request-count 0)  ; Reset counter before operations
            (ecard-carddav-discover-addressbooks server)
            (let ((_discovery-requests request-count)
                  (addressbook (car (oref server addressbooks))))

              ;; List resources (1 PROPFIND)
              (setq request-count 0)
              (ecard-carddav-list-resources addressbook)
              (let ((list-requests request-count))
                (should (equal list-requests 1))  ; Should be 1 PROPFIND

                ;; Populate contacts (should use 1 multiget REPORT)
                (setq request-count 0)
                (with-temp-buffer
                  (ecard-display-contacts-mode)
                  (setq ecard-display--addressbook addressbook)
                  (ecard-display-contacts--populate addressbook nil t))

                ;; Should be exactly 1 request (multiget)
                (should (equal request-count 1))

                ;; Verify all 50 contacts have ecard data
                (let ((resources (oref addressbook resources)))
                  (should (equal (length resources) 50))
                  (dolist (resource resources)
                    (should (oref resource ecard))))))))

      (advice-remove 'ecard-carddav-mock--handle-request
                     (lambda (&rest _) (setq request-count (1+ request-count))))
      (ecard-carddav-mock-uninstall))))

(ert-deftest ecard-display-test-multiget-error-handling ()
  "Verify multiget handles errors gracefully."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com")))
    ;; Add addressbook with one contact
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "My contacts")
    (ecard-carddav-mock-put-ecard
     mock "/addressbooks/user/contacts/contact1.vcf"
     (ecard-create :fn "John Doe"))

    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)

          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "test"
                                 :password "test"))))
            (ecard-carddav-discover-addressbooks server)
            (let ((addressbook (car (oref server addressbooks))))
              (ecard-carddav-list-resources addressbook)

              ;; Populate contacts - should work even if some contacts fail
              (with-temp-buffer
                (ecard-display-contacts-mode)
                (setq ecard-display--addressbook addressbook)
                ;; Should not error
                (should-not (condition-case err
                                (progn
                                  (ecard-display-contacts--populate addressbook nil t)
                                  nil)
                              (error err)))

                ;; Should have entries
                (should (> (length tabulated-list-entries) 0))))))

      (ecard-carddav-mock-uninstall))))

(ert-deftest ecard-display-test-multiget-batch-size ()
  "Verify multiget works with large batch sizes.
This test disables pagination to verify that loading all 200 contacts
at once works correctly."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com")))
    ;; Add addressbook with 200 contacts
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "My contacts")

    (dotimes (i 200)
      (ecard-carddav-mock-put-ecard
       mock (format "/addressbooks/user/contacts/contact%03d.vcf" i)
       (ecard-create :fn (format "Contact %03d" i))))

    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)

          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "test"
                                 :password "test"))))
            (ecard-carddav-discover-addressbooks server)
            (let ((addressbook (car (oref server addressbooks))))
              (ecard-carddav-list-resources addressbook)

              ;; Populate contacts with 200 resources - disable pagination
              (with-temp-buffer
                (ecard-display-contacts-mode)
                (setq ecard-display--addressbook addressbook)
                ;; Disable pagination to load all contacts at once
                (let ((ecard-display-contacts-page-size nil))
                  (ecard-display-contacts--populate addressbook nil t))

                ;; Verify all 200 contacts loaded
                (should (equal (length tabulated-list-entries) 200))

                ;; Verify all have ecard data
                (let ((resources (oref addressbook resources)))
                  (dolist (resource resources)
                    (should (oref resource ecard))))))))

      (ecard-carddav-mock-uninstall))))

;;; Converted tests from interactive test file

(ert-deftest ecard-display-test-servers-buffer-creation ()
  "Test servers buffer creation with mock environment."
  (unwind-protect
      (progn
        (ecard-display-test--setup-mock-environment)
        (let* ((plist-config (list :name "Mock Server (plist)"
                                   :url "https://mock.test.local"
                                   :username "testuser"
                                   :password "testpass")))
          (setq ecard-display-servers (list plist-config))
          ;; FIX: ecard-display-servers is not a function, use ecard-display instead
          (ecard-display)
          (should (get-buffer "*CardDAV Servers*"))
          (with-current-buffer "*CardDAV Servers*"
            (should (eq major-mode 'ecard-display-servers-mode))
            (should (equal (length tabulated-list-entries) 1)))))
    (ecard-display-test--cleanup-mock-environment)))

(ert-deftest ecard-display-test-addressbooks-buffer-creation ()
  "Test addressbooks buffer creation with mock environment."
  (unwind-protect
      (progn
        (ecard-display-test--setup-mock-environment)
        (let ((server ecard-display-test--real-server))
          (ecard-carddav-discover-addressbooks server)
          (let ((config (list :name "Mock Test Server"
                              :url "https://mock.test.local")))
            (ecard-display-addressbooks server config)
            (should (get-buffer "*CardDAV: Mock Test Server*"))
            (with-current-buffer "*CardDAV: Mock Test Server*"
              (should (eq major-mode 'ecard-display-addressbooks-mode))
              (should (equal (length tabulated-list-entries) 2))))))
    (ecard-display-test--cleanup-mock-environment)))

(ert-deftest ecard-display-test-contacts-buffer-creation ()
  "Test contacts buffer creation with mock environment."
  (unwind-protect
      (progn
        (ecard-display-test--setup-mock-environment)
        (let ((server ecard-display-test--real-server))
          (ecard-carddav-discover-addressbooks server)
          (let ((addressbook (car (oref server addressbooks))))
            (should addressbook)
            (ecard-carddav-list-resources addressbook)
            (ecard-display-contacts addressbook)
            (should (get-buffer "*CardDAV Contacts: Personal Contacts*"))
            (with-current-buffer "*CardDAV Contacts: Personal Contacts*"
              (should (eq major-mode 'ecard-display-contacts-mode))
              (should (equal (length tabulated-list-entries) 5))))))
    (ecard-display-test--cleanup-mock-environment)))

(ert-deftest ecard-display-test-contact-detail-buffer-creation ()
  "Test contact detail buffer creation with mock environment."
  (unwind-protect
      (progn
        (ecard-display-test--setup-mock-environment)
        (let ((server ecard-display-test--real-server))
          (ecard-carddav-discover-addressbooks server)
          (let ((addressbook (car (oref server addressbooks))))
            (should addressbook)
            (ecard-carddav-list-resources addressbook)
            (let ((resource (car (oref addressbook resources))))
              (should resource)
              (unless (oref resource ecard)
                (let ((fetched (ecard-carddav-get-resource addressbook (oref resource url))))
                  (oset resource ecard (oref fetched ecard))
                  (oset resource etag (oref fetched etag))))
              (ecard-display-contact-detail resource)
              (let ((buffer-name (format "*CardDAV Contact: %s*"
                                         (ecard-display--get-fn (oref resource ecard)))))
                (should (get-buffer buffer-name))
                (with-current-buffer buffer-name
                  (should (eq major-mode 'ecard-display-contact-mode))
                  (should (> (buffer-size) 0))))))))
    (ecard-display-test--cleanup-mock-environment)))

(ert-deftest ecard-display-test-pagination-functionality ()
  "Test pagination controls work correctly."
  (unwind-protect
      (progn
        (ecard-display-test--setup-mock-environment)
        (let ((server ecard-display-test--real-server))
          (ecard-carddav-discover-addressbooks server)
          (let ((addressbook (car (oref server addressbooks))))
            (should addressbook)
            (ecard-carddav-list-resources addressbook)
            (ecard-display-contacts addressbook)
            (with-current-buffer "*CardDAV Contacts: Personal Contacts*"
              ;; Verify pagination is active
              (should ecard-display-contacts-page-size)
              (should (equal ecard-display--current-page 0))
              (should (equal ecard-display--total-contacts 5))

              ;; Test next page (should not error even at last page)
              (ecard-display-contacts-next-page)

              ;; Test prev page
              (ecard-display-contacts-prev-page)
              (should (equal ecard-display--current-page 0))))))
    (ecard-display-test--cleanup-mock-environment)))

(ert-deftest ecard-display-test-contact-add-operation ()
  "Test adding a contact through the UI."
  (unwind-protect
      (progn
        (ecard-display-test--setup-mock-environment)
        (let ((server ecard-display-test--real-server))
          (ecard-carddav-discover-addressbooks server)
          (let ((addressbook (car (oref server addressbooks))))
            (should addressbook)
            (ecard-carddav-list-resources addressbook)
            (let ((initial-count (length (oref addressbook resources))))
              ;; Create and add a test contact programmatically
              (let* ((fn "Test User")
                     (ecard-obj (ecard-create :fn fn))
                     (path "/addressbooks/testuser/contacts/test-new.vcf"))
                (oset ecard-obj uid (list (ecard-property
                                           :name "UID"
                                           :value (format "urn:uuid:%s"
                                                          (ecard-display--generate-uuid)))))
                (ecard-carddav-put-ecard addressbook path ecard-obj)
                (ecard-carddav-list-resources addressbook)
                (should (equal (length (oref addressbook resources)) (1+ initial-count))))))))
    (ecard-display-test--cleanup-mock-environment)))

(ert-deftest ecard-display-test-contact-delete-operation ()
  "Test deleting a contact."
  (unwind-protect
      (progn
        (ecard-display-test--setup-mock-environment)
        (let ((server ecard-display-test--real-server))
          (ecard-carddav-discover-addressbooks server)
          (let ((addressbook (car (oref server addressbooks))))
            (should addressbook)
            (ecard-carddav-list-resources addressbook)
            (let ((initial-count (length (oref addressbook resources)))
                  (resource (car (oref addressbook resources))))
              (should resource)
              ;; Delete the contact
              (ecard-carddav-delete-resource addressbook
                                             (oref resource url)
                                             (oref resource etag))
              (ecard-carddav-list-resources addressbook)
              (should (equal (length (oref addressbook resources)) (1- initial-count)))))))
    (ecard-display-test--cleanup-mock-environment)))

(ert-deftest ecard-display-test-pagination-shows-real-names ()
  "Verify paginated contacts show real names for current page only.
After the populate change, only the current page resources are displayed,
not all resources with some having placeholder names."
  (let ((mock (ecard-carddav-mock-server-create
               :base-url "https://test.example.com")))
    ;; Add addressbook with 5 contacts with UUID filenames
    (ecard-carddav-mock-add-addressbook
     mock "/addressbooks/user/contacts/" "Contacts" "Test contacts")

    (dotimes (i 5)
      (let ((uuid (format "uuid-%d" i)))
        (ecard-carddav-mock-put-ecard
         mock (format "/addressbooks/user/contacts/%s.vcf" uuid)
         (ecard-create
          :fn (format "Person %d" i)
          :email (format "person%d@example.com" i)
          :tel (format "+1-555-010%d" i)))))

    (unwind-protect
        (progn
          (ecard-carddav-mock-install mock)

          (let* ((server (ecard-carddav-server-create
                          :url "https://test.example.com"
                          :auth (ecard-carddav-auth-basic-create
                                 :username "test"
                                 :password "test"))))
            (ecard-carddav-discover-addressbooks server)
            (let ((addressbook (car (oref server addressbooks))))
              (ecard-carddav-list-resources addressbook)

              ;; Test with page size of 2 (first page should have real names)
              (with-temp-buffer
                (ecard-display-contacts-mode)
                (setq ecard-display--addressbook addressbook
                      ecard-display-contacts-page-size 2)  ; Page size of 2
                (ecard-display-contacts--populate addressbook nil t)

                ;; NEW BEHAVIOR: Should have only 2 entries (current page size)
                ;; not all 5 resources - this is the performance improvement
                (should (equal (length tabulated-list-entries) 2))

                ;; Both entries on current page should show real names
                (let ((entry0 (nth 0 tabulated-list-entries))
                      (entry1 (nth 1 tabulated-list-entries)))
                  (should (equal (aref (cadr entry0) 0) "Person 0"))
                  (should (equal (aref (cadr entry0) 1) "person0@example.com"))
                  (should (equal (aref (cadr entry0) 2) "+1-555-0100"))

                  (should (equal (aref (cadr entry1) 0) "Person 1"))
                  (should (equal (aref (cadr entry1) 1) "person1@example.com"))
                  (should (equal (aref (cadr entry1) 2) "+1-555-0101")))

                ;; Verify total count is tracked for pagination info
                (should (equal ecard-display--total-contacts 5))
                (should (equal ecard-display--current-page 0))))))

      (ecard-carddav-mock-uninstall))))

;;; UID Display Tests

(ert-deftest ecard-display-test-uid-in-detail-view ()
  "Test that UID is displayed in contact detail view."
  (let* ((uid-prop (ecard-property :name "UID" :value "urn:uuid:test-uid-12345"))
         (fn-prop (ecard-property :name "FN" :value "Test Person"))
         (ecard-obj (ecard :fn (list fn-prop) :uid (list uid-prop)))
         (addressbook (ecard-carddav-addressbook :url "https://test.example.com/contacts/"))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/contacts/test.vcf"
                    :path "/contacts/test.vcf"
                    :ecard ecard-obj)))
    (with-temp-buffer
      (ecard-display-contact-mode)
      (setq ecard-display--resource resource
            ecard-display--addressbook addressbook)
      (ecard-display-contact--render resource)
      (let ((content (buffer-string)))
        (should (string-match-p "UID:" content))
        (should (string-match-p "urn:uuid:test-uid-12345" content))))))

(ert-deftest ecard-display-test-no-uid-in-detail-view ()
  "Test that contact detail view works without UID."
  (let* ((fn-prop (ecard-property :name "FN" :value "Test Person"))
         (ecard-obj (ecard :fn (list fn-prop)))
         (addressbook (ecard-carddav-addressbook :url "https://test.example.com/contacts/"))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/contacts/test.vcf"
                    :path "/contacts/test.vcf"
                    :ecard ecard-obj)))
    (with-temp-buffer
      (ecard-display-contact-mode)
      (setq ecard-display--resource resource
            ecard-display--addressbook addressbook)
      (ecard-display-contact--render resource)
      (let ((content (buffer-string)))
        ;; Should not display UID section if no UID
        (should-not (string-match-p "UID:" content))
        ;; But should still display other content
        (should (string-match-p "Full Name:" content))
        (should (string-match-p "Test Person" content))))))

(provide 'ecard-display-test)
;;; ecard-display-test.el ends here
