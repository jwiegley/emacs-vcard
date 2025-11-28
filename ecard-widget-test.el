;;; ecard-widget-test.el --- Tests for ecard-widget -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>

;;; Commentary:

;; ERT tests for ecard-widget.el widget-based contact editing.
;; Tests cover widget creation, value extraction, and integration with
;; the CardDAV system.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ecard)
(require 'ecard-widget)
(require 'ecard-carddav)
(require 'ecard-carddav-mock)
(require 'ecard-display)

;;; Test helpers

(defun ecard-widget-test--create-sample-ecard ()
  "Create a sample ecard for testing."
  (let ((ecard-obj (ecard-create :fn "John Doe")))
    ;; Add N property
    (setf (ecard-n ecard-obj)
          (list (ecard-property
                 :name "N"
                 :value '("Doe" "John" "William" "Mr." "Jr."))))
    ;; Add emails
    (setf (ecard-email ecard-obj)
          (list (ecard-property
                 :name "EMAIL"
                 :parameters '(("TYPE" . "work"))
                 :value "john.doe@work.example.com")
                (ecard-property
                 :name "EMAIL"
                 :parameters '(("TYPE" . "home"))
                 :value "john.doe@home.example.com")))
    ;; Add telephones
    (setf (ecard-tel ecard-obj)
          (list (ecard-property
                 :name "TEL"
                 :parameters '(("TYPE" . "work"))
                 :value "+1-555-0100")
                (ecard-property
                 :name "TEL"
                 :parameters '(("TYPE" . "cell"))
                 :value "+1-555-0101")))
    ;; Add address
    (setf (ecard-adr ecard-obj)
          (list (ecard-property
                 :name "ADR"
                 :parameters '(("TYPE" . "work"))
                 :value '("" "" "123 Main Street" "Anytown" "CA" "90210" "USA"))))
    ;; Add org and title
    (setf (ecard-org ecard-obj)
          (list (ecard-property :name "ORG" :value "ACME Corporation")))
    (setf (ecard-title ecard-obj)
          (list (ecard-property :name "TITLE" :value "Senior Engineer")))
    ;; Add note
    (setf (ecard-note ecard-obj)
          (list (ecard-property :name "NOTE" :value "A test contact.")))
    ;; Add UID
    (setf (ecard-uid ecard-obj)
          (list (ecard-property :name "UID" :value "test-uid-12345")))
    ecard-obj))

(defun ecard-widget-test--with-widget-buffer (ecard-obj body)
  "Create widget buffer for ECARD-OBJ and execute BODY."
  (with-temp-buffer
    (ecard-widget-create ecard-obj)
    (funcall body)))

;;; Basic widget creation tests

(ert-deftest ecard-widget-test-create-basic ()
  "Test that widget form can be created for a basic ecard."
  (let ((ecard-obj (ecard-create :fn "Test Person")))
    (with-temp-buffer
      (ecard-widget-create ecard-obj)
      ;; Buffer should have content
      (should (> (buffer-size) 0))
      ;; Should have widgets
      (should ecard-widget--widgets)
      ;; Should have stored the ecard
      (should (eq ecard-widget--ecard ecard-obj)))))

(ert-deftest ecard-widget-test-create-full ()
  "Test widget form creation with full ecard data."
  (let ((ecard-obj (ecard-widget-test--create-sample-ecard)))
    (with-temp-buffer
      (ecard-widget-create ecard-obj)
      (let ((content (buffer-string)))
        ;; Should display full name
        (should (string-match-p "John Doe" content))
        ;; Should display UID (read-only)
        (should (string-match-p "test-uid-12345" content))
        ;; Should have name section
        (should (string-match-p "Name" content))
        ;; Should have email section
        (should (string-match-p "Email" content))
        ;; Should have phone section
        (should (string-match-p "Phone" content))
        ;; Should have organization section
        (should (string-match-p "Organization" content))))))

(ert-deftest ecard-widget-test-create-empty ()
  "Test widget form creation with empty ecard."
  (let ((ecard-obj (ecard)))
    (with-temp-buffer
      (ecard-widget-create ecard-obj)
      ;; Should not error
      (should (> (buffer-size) 0))
      ;; Should have widgets for empty form
      (should ecard-widget--widgets))))

;;; Widget value extraction tests

(ert-deftest ecard-widget-test-get-value-fn ()
  "Test extracting FN value from widgets."
  (let ((ecard-obj (ecard-create :fn "Original Name")))
    (with-temp-buffer
      (ecard-widget-create ecard-obj)
      ;; Modify the FN widget
      (let ((fn-widget (cdr (assq 'fn ecard-widget--widgets))))
        (when fn-widget
          (widget-value-set fn-widget "New Name")))
      ;; Extract and verify
      (let ((result (ecard-widget-get-value)))
        (should (ecard-p result))
        (should (equal (ecard-property-value (car (ecard-fn result)))
                       "New Name"))))))

(ert-deftest ecard-widget-test-get-value-structured-name ()
  "Test extracting N (structured name) value from widgets."
  (let ((ecard-obj (ecard-create :fn "Test")))
    (setf (ecard-n ecard-obj)
          (list (ecard-property :name "N" :value '("" "" "" "" ""))))
    (with-temp-buffer
      (ecard-widget-create ecard-obj)
      ;; Set name components
      (let ((given-w (cdr (assq 'n-given ecard-widget--widgets)))
            (family-w (cdr (assq 'n-family ecard-widget--widgets)))
            (additional-w (cdr (assq 'n-additional ecard-widget--widgets)))
            (prefix-w (cdr (assq 'n-prefix ecard-widget--widgets)))
            (suffix-w (cdr (assq 'n-suffix ecard-widget--widgets))))
        (when given-w (widget-value-set given-w "John"))
        (when family-w (widget-value-set family-w "Doe"))
        (when additional-w (widget-value-set additional-w "William"))
        (when prefix-w (widget-value-set prefix-w "Dr."))
        (when suffix-w (widget-value-set suffix-w "PhD")))
      ;; Extract and verify
      (let* ((result (ecard-widget-get-value))
             (n-val (ecard-property-value (car (ecard-n result)))))
        (should (equal (nth 0 n-val) "Doe"))        ; family
        (should (equal (nth 1 n-val) "John"))       ; given
        (should (equal (nth 2 n-val) "William"))    ; additional
        (should (equal (nth 3 n-val) "Dr."))        ; prefix
        (should (equal (nth 4 n-val) "PhD"))))))    ; suffix

(ert-deftest ecard-widget-test-get-value-emails ()
  "Test extracting email values from widgets."
  (let ((ecard-obj (ecard-create :fn "Test")))
    (setf (ecard-email ecard-obj)
          (list (ecard-property
                 :name "EMAIL"
                 :parameters '(("TYPE" . "work"))
                 :value "old@example.com")))
    (with-temp-buffer
      (ecard-widget-create ecard-obj)
      ;; Modify email value
      (let ((email-value-w (cdr (assq 'email-0-value ecard-widget--widgets))))
        (when email-value-w
          (widget-value-set email-value-w "new@example.com")))
      ;; Extract and verify
      (let* ((result (ecard-widget-get-value))
             (emails (ecard-email result)))
        (should (= (length emails) 1))
        (should (equal (ecard-property-value (car emails)) "new@example.com"))))))

(ert-deftest ecard-widget-test-get-value-telephones ()
  "Test extracting telephone values from widgets."
  (let ((ecard-obj (ecard-create :fn "Test")))
    (setf (ecard-tel ecard-obj)
          (list (ecard-property
                 :name "TEL"
                 :parameters '(("TYPE" . "cell"))
                 :value "+1-555-0000")))
    (with-temp-buffer
      (ecard-widget-create ecard-obj)
      ;; Modify telephone value
      (let ((tel-value-w (cdr (assq 'tel-0-value ecard-widget--widgets))))
        (when tel-value-w
          (widget-value-set tel-value-w "+1-555-1234")))
      ;; Extract and verify
      (let* ((result (ecard-widget-get-value))
             (tels (ecard-tel result)))
        (should (= (length tels) 1))
        (should (equal (ecard-property-value (car tels)) "+1-555-1234"))))))

(ert-deftest ecard-widget-test-get-value-addresses ()
  "Test extracting address values from widgets."
  (let ((ecard-obj (ecard-create :fn "Test")))
    (setf (ecard-adr ecard-obj)
          (list (ecard-property
                 :name "ADR"
                 :parameters '(("TYPE" . "home"))
                 :value '("" "" "1 Old Street" "Old City" "OS" "00000" "Old Country"))))
    (with-temp-buffer
      (ecard-widget-create ecard-obj)
      ;; Modify address values
      (let ((street-w (cdr (assq 'adr-0-street ecard-widget--widgets)))
            (city-w (cdr (assq 'adr-0-city ecard-widget--widgets)))
            (region-w (cdr (assq 'adr-0-region ecard-widget--widgets)))
            (postal-w (cdr (assq 'adr-0-postal ecard-widget--widgets)))
            (country-w (cdr (assq 'adr-0-country ecard-widget--widgets))))
        (when street-w (widget-value-set street-w "123 New Street"))
        (when city-w (widget-value-set city-w "New City"))
        (when region-w (widget-value-set region-w "NS"))
        (when postal-w (widget-value-set postal-w "12345"))
        (when country-w (widget-value-set country-w "New Country")))
      ;; Extract and verify
      (let* ((result (ecard-widget-get-value))
             (addrs (ecard-adr result))
             (adr-val (ecard-property-value (car addrs))))
        (should (= (length addrs) 1))
        (should (equal (nth 2 adr-val) "123 New Street"))
        (should (equal (nth 3 adr-val) "New City"))
        (should (equal (nth 4 adr-val) "NS"))
        (should (equal (nth 5 adr-val) "12345"))
        (should (equal (nth 6 adr-val) "New Country"))))))

(ert-deftest ecard-widget-test-get-value-organization ()
  "Test extracting organization values from widgets."
  (let ((ecard-obj (ecard-create :fn "Test")))
    (setf (ecard-org ecard-obj)
          (list (ecard-property :name "ORG" :value "Old Corp")))
    (setf (ecard-title ecard-obj)
          (list (ecard-property :name "TITLE" :value "Old Title")))
    (with-temp-buffer
      (ecard-widget-create ecard-obj)
      ;; Modify org and title
      (let ((org-w (cdr (assq 'org ecard-widget--widgets)))
            (title-w (cdr (assq 'title ecard-widget--widgets))))
        (when org-w (widget-value-set org-w "New Corp"))
        (when title-w (widget-value-set title-w "New Title")))
      ;; Extract and verify
      (let ((result (ecard-widget-get-value)))
        (should (equal (ecard-property-value (car (ecard-org result))) "New Corp"))
        (should (equal (ecard-property-value (car (ecard-title result))) "New Title"))))))

(ert-deftest ecard-widget-test-get-value-notes ()
  "Test extracting notes from widgets."
  (let ((ecard-obj (ecard-create :fn "Test")))
    (setf (ecard-note ecard-obj)
          (list (ecard-property :name "NOTE" :value "Old note")))
    (with-temp-buffer
      (ecard-widget-create ecard-obj)
      ;; Modify note
      (let ((note-w (cdr (assq 'note ecard-widget--widgets))))
        (when note-w
          (widget-value-set note-w "This is a new note\nWith multiple lines.")))
      ;; Extract and verify
      (let ((result (ecard-widget-get-value)))
        (should (string-match-p "new note" (ecard-property-value (car (ecard-note result)))))))))

;;; Modification detection tests

(ert-deftest ecard-widget-test-modified-p-no-change ()
  "Test that modified-p returns nil when no changes made."
  (let ((ecard-obj (ecard-create :fn "Test Person")))
    (with-temp-buffer
      (ecard-widget-create ecard-obj)
      ;; No modifications made
      (should-not (ecard-widget-modified-p)))))

(ert-deftest ecard-widget-test-modified-p-with-change ()
  "Test that modified-p returns t when changes made."
  (let ((ecard-obj (ecard-create :fn "Original")))
    (with-temp-buffer
      (ecard-widget-create ecard-obj)
      ;; Modify the FN widget
      (let ((fn-widget (cdr (assq 'fn ecard-widget--widgets))))
        (when fn-widget
          (widget-value-set fn-widget "Modified")))
      (should (ecard-widget-modified-p)))))

;;; Integration with ecard-display tests

(ert-deftest ecard-widget-test-display-integration ()
  "Test widget integration with ecard-display contact detail."
  (let* ((ecard-obj (ecard-widget-test--create-sample-ecard))
         (server (ecard-carddav-server))
         (addressbook (ecard-carddav-addressbook
                       :server server
                       :url "https://test.example.com/addressbook"))
         (resource (ecard-carddav-resource
                    :addressbook addressbook
                    :url "https://test.example.com/addressbook/test.vcf"
                    :path "/addressbook/test.vcf"
                    :ecard ecard-obj
                    :etag "etag-1")))
    (with-temp-buffer
      (ecard-display-contact-mode)
      (setq ecard-display--resource resource
            ecard-display--addressbook addressbook)
      ;; Create widget form
      (ecard-widget-create ecard-obj #'ecard-display-contact--on-change)
      (let ((content (buffer-string)))
        ;; Should display contact info
        (should (string-match-p "John Doe" content))
        (should (string-match-p "ACME Corporation" content))))))

;;; Mock server integration tests

(defvar ecard-widget-test--mock-server nil
  "Mock server for widget tests.")

(defun ecard-widget-test--setup-mock ()
  "Set up mock server for testing."
  (setq ecard-widget-test--mock-server
        (ecard-carddav-mock-server-create
         :base-url "https://widget-test.example.com"))
  (ecard-carddav-mock-add-addressbook
   ecard-widget-test--mock-server
   "/addressbooks/user/contacts/"
   "Test Contacts"
   "Test addressbook")
  (ecard-carddav-mock-install ecard-widget-test--mock-server))

(defun ecard-widget-test--teardown-mock ()
  "Tear down mock server."
  (ecard-carddav-mock-uninstall)
  (setq ecard-widget-test--mock-server nil))

(ert-deftest ecard-widget-test-server-save ()
  "Test saving edited contact to server via widgets."
  (unwind-protect
      (progn
        (ecard-widget-test--setup-mock)
        ;; Create and add a contact to mock server
        (let ((original-ecard (ecard-create :fn "Original Name")))
          (setf (ecard-uid original-ecard)
                (list (ecard-property :name "UID" :value "test-save-uid")))
          (ecard-carddav-mock-put-ecard
           ecard-widget-test--mock-server
           "/addressbooks/user/contacts/test.vcf"
           original-ecard)

          ;; Create server and addressbook objects
          (let* ((auth (ecard-carddav-auth-basic-create
                        :username "test" :password "test"))
                 (server (ecard-carddav-server-create
                          :url "https://widget-test.example.com"
                          :auth auth)))
            ;; Discover and get resource
            (ecard-carddav-discover-addressbooks server)
            (let ((addressbook (car (oref server addressbooks))))
              (ecard-carddav-list-resources addressbook)
              (let ((resource (car (oref addressbook resources))))
                ;; Fetch full ecard data
                (let ((fetched (ecard-carddav-get-resource addressbook (oref resource url))))
                  (oset resource ecard (oref fetched ecard))
                  (oset resource etag (oref fetched etag)))

                ;; Create widget form and edit
                (with-temp-buffer
                  (ecard-display-contact-mode)
                  (setq ecard-display--resource resource
                        ecard-display--addressbook addressbook
                        ecard-display--original-ecard
                        (ecard-parse (ecard-serialize (oref resource ecard))))
                  (ecard-widget-create (oref resource ecard))

                  ;; Modify the FN
                  (let ((fn-widget (cdr (assq 'fn ecard-widget--widgets))))
                    (when fn-widget
                      (widget-value-set fn-widget "Modified Name")))

                  ;; Save should work
                  (should-not (condition-case err
                                  (progn
                                    (ecard-display-contact-save)
                                    nil)
                                (error err)))

                  ;; Verify the change was saved to mock server
                  (let* ((mock-ab (gethash "/addressbooks/user/contacts/"
                                           (oref ecard-widget-test--mock-server addressbooks)))
                         (mock-res (gethash "/addressbooks/user/contacts/test.vcf"
                                            (oref mock-ab resources)))
                         (saved-ecard (oref mock-res ecard)))
                    (should (equal (ecard-property-value (car (ecard-fn saved-ecard)))
                                   "Modified Name")))))))))
    (ecard-widget-test--teardown-mock)))

(ert-deftest ecard-widget-test-server-revert ()
  "Test reverting edited contact discards widget changes."
  (let ((ecard-obj (ecard-create :fn "Original Name")))
    (with-temp-buffer
      (ecard-display-contact-mode)
      (setq ecard-display--original-ecard (ecard-parse (ecard-serialize ecard-obj)))
      (ecard-widget-create ecard-obj)

      ;; Modify the FN
      (let ((fn-widget (cdr (assq 'fn ecard-widget--widgets))))
        (when fn-widget
          (widget-value-set fn-widget "Modified Name")))

      ;; Should be modified
      (should (ecard-widget-modified-p))

      ;; Revert (non-interactively)
      (ecard-widget-create ecard-display--original-ecard)

      ;; Should no longer be modified
      (should-not (ecard-widget-modified-p))

      ;; FN should be back to original
      (let ((fn-widget (cdr (assq 'fn ecard-widget--widgets))))
        (when fn-widget
          (should (equal (widget-value fn-widget) "Original Name")))))))

;;; Edge case tests

(ert-deftest ecard-widget-test-empty-email-not-saved ()
  "Test that empty email fields are not saved."
  (let ((ecard-obj (ecard-create :fn "Test")))
    (with-temp-buffer
      (ecard-widget-create ecard-obj)
      ;; Leave email field empty (which is the default)
      (let ((result (ecard-widget-get-value)))
        ;; Should have no emails
        (should (null (ecard-email result)))))))

(ert-deftest ecard-widget-test-empty-address-not-saved ()
  "Test that empty address fields are not saved."
  (let ((ecard-obj (ecard-create :fn "Test")))
    (with-temp-buffer
      (ecard-widget-create ecard-obj)
      ;; Leave all address fields empty
      (let ((result (ecard-widget-get-value)))
        ;; Should have no addresses
        (should (null (ecard-adr result)))))))

(ert-deftest ecard-widget-test-partial-address-saved ()
  "Test that partially filled address is saved."
  (let ((ecard-obj (ecard-create :fn "Test")))
    (with-temp-buffer
      (ecard-widget-create ecard-obj)
      ;; Fill only city
      (let ((city-w (cdr (assq 'adr-0-city ecard-widget--widgets))))
        (when city-w (widget-value-set city-w "TestCity")))
      (let* ((result (ecard-widget-get-value))
             (addrs (ecard-adr result)))
        ;; Should have one address
        (should (= (length addrs) 1))
        (should (equal (nth 3 (ecard-property-value (car addrs))) "TestCity"))))))

(ert-deftest ecard-widget-test-type-parameter-preserved ()
  "Test that type parameters are preserved on extraction."
  (let ((ecard-obj (ecard-create :fn "Test")))
    (setf (ecard-email ecard-obj)
          (list (ecard-property
                 :name "EMAIL"
                 :parameters '(("TYPE" . "work"))
                 :value "test@example.com")))
    (with-temp-buffer
      (ecard-widget-create ecard-obj)
      (let* ((result (ecard-widget-get-value))
             (emails (ecard-email result))
             (params (ecard-property-parameters (car emails))))
        ;; Should preserve type parameter
        (should (equal (cdr (assoc "TYPE" params)) "work"))))))

(ert-deftest ecard-widget-test-roundtrip ()
  "Test that widget edit roundtrip preserves data integrity."
  (let ((original (ecard-widget-test--create-sample-ecard)))
    (with-temp-buffer
      (ecard-widget-create original)
      ;; Extract without changes
      (let ((result (ecard-widget-get-value)))
        ;; Should have same FN
        (should (equal (ecard-property-value (car (ecard-fn result)))
                       (ecard-property-value (car (ecard-fn original)))))
        ;; Should have same number of emails
        (should (= (length (ecard-email result))
                   (length (ecard-email original))))
        ;; Should have same number of telephones
        (should (= (length (ecard-tel result))
                   (length (ecard-tel original))))))))

;;; Notify callback tests

(ert-deftest ecard-widget-test-notify-callback ()
  "Test that notify callback is called on changes."
  (let ((ecard-obj (ecard-create :fn "Test"))
        (callback-called nil))
    (with-temp-buffer
      (ecard-widget-create ecard-obj (lambda () (setq callback-called t)))
      ;; Modify a widget
      (let ((fn-widget (cdr (assq 'fn ecard-widget--widgets))))
        (when fn-widget
          (widget-value-set fn-widget "Modified")))
      ;; Callback should have been called
      (should callback-called))))

(provide 'ecard-widget-test)
;;; ecard-widget-test.el ends here
