;;; ecard-carddav-map-test.el --- Tests for CardDAV map operations -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: comm, data, carddav, testing

;;; Commentary:

;; Comprehensive test suite for ecard-carddav-map module.
;; Uses mock CardDAV server to avoid network dependencies.
;;
;; Run tests with:
;;   emacs -batch -L . -l ecard.el -l ecard-compat.el \
;;         -l ecard-carddav-auth.el -l ecard-carddav.el \
;;         -l ecard-carddav-mock.el -l ecard-carddav-map.el \
;;         -l ecard-carddav-map-test.el \
;;         -f ert-run-tests-batch-and-exit
;;
;; Or interactively:
;;   M-x load-file RET ecard-carddav-map-test.el RET
;;   M-x ert RET t RET

;;; Code:

(require 'ert)
(require 'ecard)
(require 'ecard-carddav-auth)
(require 'ecard-carddav)
(require 'ecard-carddav-mock)
(require 'ecard-carddav-map)

;;; Test fixtures

(defvar ecard-carddav-map-test--mock-server nil
  "Mock server for testing.")

(defvar ecard-carddav-map-test--addressbook nil
  "Test addressbook.")

(defun ecard-carddav-map-test--setup ()
  "Set up test fixtures."
  ;; Create mock server
  (setq ecard-carddav-map-test--mock-server
        (ecard-carddav-mock-server-create
         :base-url "https://test.example.com"))

  ;; Add address book
  (ecard-carddav-mock-add-addressbook
   ecard-carddav-map-test--mock-server
   "/addressbooks/user/contacts/"
   "Test Contacts"
   "Test address book")

  ;; Install mock
  (ecard-carddav-mock-install ecard-carddav-map-test--mock-server)

  ;; Get addressbook reference
  (let* ((auth (ecard-carddav-auth-basic-create
                :username "user"
                :password "pass"))
         (server (ecard-carddav-server-create
                  :url "https://test.example.com"
                  :auth auth))
         (addressbooks (ecard-carddav-discover-addressbooks server)))
    (setq ecard-carddav-map-test--addressbook (car addressbooks))))

(defun ecard-carddav-map-test--teardown ()
  "Tear down test fixtures."
  ;; Uninstall mock
  (ecard-carddav-mock-uninstall)

  (setq ecard-carddav-map-test--mock-server nil
        ecard-carddav-map-test--addressbook nil))

(defun ecard-carddav-map-test--create-test-ecard (fn &optional note)
  "Create a test vCard with formatted name FN and optional NOTE."
  (let ((card (ecard-create
               :fn fn
               :n (list "Doe" "John" "" "" "")
               :email "john@example.com"
               :tel "+1-555-1234"
               :uid (format "test-%s@example.com"
                           (downcase (replace-regexp-in-string " " "-" fn))))))
    (when note
      (ecard-add-property card 'note note))
    card))

(defun ecard-carddav-map-test--populate-addressbook (count)
  "Add COUNT test vCards to addressbook."
  (dotimes (i count)
    (let ((name (format "Contact %d" (1+ i)))
          (path (format "/addressbooks/user/contacts/contact%d.vcf" (1+ i))))
      (ecard-carddav-put-ecard
       ecard-carddav-map-test--addressbook
       path
       (ecard-carddav-map-test--create-test-ecard name)))))

;;; Basic functionality tests

(ert-deftest ecard-carddav-map-test-empty-addressbook ()
  "Test mapping over empty addressbook."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (let* ((call-count 0)
             (result (ecard-carddav-map-resources
                     ecard-carddav-map-test--addressbook
                     (lambda (_resource)
                       (setq call-count (1+ call-count))
                       nil))))
        (should (= call-count 0))
        (should (= (plist-get result :total) 0))
        (should (= (plist-get result :processed) 0))
        (should (= (plist-get result :modified) 0))
        (should (= (plist-get result :deleted) 0))
        (should (= (plist-get result :failed) 0))
        (should (= (plist-get result :skipped) 0)))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-no-modifications ()
  "Test mapping that modifies no resources."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 5)
        (let* ((call-count 0)
               (result (ecard-carddav-map-resources
                       ecard-carddav-map-test--addressbook
                       (lambda (_resource)
                         (setq call-count (1+ call-count))
                         nil))))  ; Return nil - no modification
          (should (= call-count 5))
          (should (= (plist-get result :total) 5))
          (should (= (plist-get result :processed) 5))
          (should (= (plist-get result :modified) 0))
          (should (= (plist-get result :deleted) 0))
          (should (= (plist-get result :failed) 0))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-modify-all-resources ()
  "Test mapping that modifies all resources."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 3)
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let ((ecard (oref resource ecard)))
                          (ecard-add-property ecard 'note "Test note")
                          t)))))  ; Return t - modified
          (should (= (plist-get result :total) 3))
          (should (= (plist-get result :processed) 3))
          (should (= (plist-get result :modified) 3))
          (should (= (plist-get result :deleted) 0))
          (should (= (plist-get result :failed) 0))

          ;; Verify modifications were saved
          (let ((resources (ecard-carddav-list-resources
                           ecard-carddav-map-test--addressbook)))
            (dolist (resource-info resources)
              (let ((resource (ecard-carddav-get-resource
                              ecard-carddav-map-test--addressbook
                              (oref resource-info path))))
                (should (oref (oref resource ecard) note)))))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-modify-some-resources ()
  "Test mapping that modifies only some resources."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 6)
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let* ((ecard (oref resource ecard))
                               (fn (ecard-get-property-value ecard 'fn)))
                          ;; Only modify even-numbered contacts
                          (when (string-match "Contact \\([0-9]+\\)" fn)
                            (let ((num (string-to-number (match-string 1 fn))))
                              (when (zerop (mod num 2))
                                (ecard-add-property ecard 'note "Even contact")
                                t))))))))
          (should (= (plist-get result :total) 6))
          (should (= (plist-get result :processed) 6))
          (should (= (plist-get result :modified) 3))  ; Contacts 2, 4, 6
          (should (= (plist-get result :deleted) 0))
          (should (= (plist-get result :failed) 0))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-delete-resources ()
  "Test mapping that deletes resources."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 4)
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let* ((ecard (oref resource ecard))
                               (fn (ecard-get-property-value ecard 'fn)))
                          ;; Delete contacts with odd numbers
                          (when (string-match "Contact \\([0-9]+\\)" fn)
                            (let ((num (string-to-number (match-string 1 fn))))
                              (when (not (zerop (mod num 2)))
                                :delete))))))))
          (should (= (plist-get result :total) 4))
          (should (= (plist-get result :processed) 4))
          (should (= (plist-get result :modified) 0))
          (should (= (plist-get result :deleted) 2))  ; Contacts 1, 3
          (should (= (plist-get result :failed) 0))

          ;; Verify deletions
          (let ((resources (ecard-carddav-list-resources
                           ecard-carddav-map-test--addressbook)))
            (should (= (length resources) 2))
            ;; Remaining should be contacts 2 and 4
            (let ((paths (mapcar (lambda (r) (oref r path)) resources)))
              (should (member "/addressbooks/user/contacts/contact2.vcf" paths))
              (should (member "/addressbooks/user/contacts/contact4.vcf" paths))))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-skip-resources ()
  "Test mapping that explicitly skips resources."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 5)
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let* ((ecard (oref resource ecard))
                               (fn (ecard-get-property-value ecard 'fn)))
                          ;; Skip first 3 contacts
                          (if (string-match "Contact [123]" fn)
                              :skip
                            (ecard-add-property ecard 'note "Processed")
                            t))))))
          (should (= (plist-get result :total) 5))
          (should (= (plist-get result :processed) 5))
          (should (= (plist-get result :modified) 2))  ; Contacts 4, 5
          (should (= (plist-get result :skipped) 3))  ; Contacts 1, 2, 3
          (should (= (plist-get result :failed) 0))))
    (ecard-carddav-map-test--teardown)))

;;; Change detection tests

(ert-deftest ecard-carddav-map-test-false-modification-claim ()
  "Test that returning t without actual change doesn't update server."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 3)
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (_resource)
                        ;; Claim modification but don't actually modify
                        t))))
          (should (= (plist-get result :total) 3))
          (should (= (plist-get result :processed) 3))
          ;; Should be 0 because change detection detects no actual change
          (should (= (plist-get result :modified) 0))
          (should (= (plist-get result :skipped) 3))))  ; Skipped due to no change
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-actual-modification-detected ()
  "Test that actual modifications are correctly detected."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 2)
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let ((ecard (oref resource ecard)))
                          ;; Make actual modification
                          (ecard-add-property ecard 'note "Modified")
                          t)))))
          (should (= (plist-get result :modified) 2))
          (should (= (plist-get result :skipped) 0))))
    (ecard-carddav-map-test--teardown)))

;;; Error handling tests

(ert-deftest ecard-carddav-map-test-transformation-error ()
  "Test handling of errors in transformation function."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 3)
        (let* ((processed-paths nil)
               (result (ecard-carddav-map-resources
                       ecard-carddav-map-test--addressbook
                       (lambda (resource)
                         (let ((path (oref resource path)))
                           (push path processed-paths)
                           ;; Throw error on second resource processed
                           (when (= (length processed-paths) 2)
                             (error "Test error"))
                           nil)))))
          (should (= (plist-get result :total) 3))
          (should (= (plist-get result :processed) 3))
          ;; Should have at least 1 error (the one we threw)
          (should (>= (plist-get result :failed) 1))
          (should (>= (length (plist-get result :errors)) 1))
          ;; Verify at least one error has the right type
          (should (cl-some (lambda (err)
                            (eq (plist-get err :type) 'transform-error))
                          (plist-get result :errors)))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-etag-conflict-skip ()
  "Test ETag conflict handling with :skip strategy."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 1)

        ;; Simulate concurrent modification by updating the resource
        ;; after we've fetched it
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let ((ecard (oref resource ecard)))
                          ;; Simulate someone else updating the resource
                          ;; by invalidating the ETag
                          (oset resource etag "invalid-etag")
                          (ecard-add-property ecard 'note "Conflict test")
                          t))
                      :on-conflict :skip)))
          (should (= (plist-get result :total) 1))
          (should (= (plist-get result :processed) 1))
          (should (= (plist-get result :modified) 0))
          (should (= (plist-get result :skipped) 1))
          ;; Should have conflict error
          (should (= (length (plist-get result :errors)) 1))
          (let ((err (car (plist-get result :errors))))
            (should (eq (plist-get err :type) 'conflict)))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-etag-conflict-force ()
  "Test ETag conflict handling with :force strategy."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        ;; Create a contact
        (ecard-carddav-put-ecard
         ecard-carddav-map-test--addressbook
         "/addressbooks/user/contacts/test.vcf"
         (ecard-carddav-map-test--create-test-ecard "Test User"))

        ;; Map with force conflict resolution
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        ;; Modify the contact
                        (let ((ecard (oref resource ecard)))
                          (ecard-add-property ecard 'note "Force update")
                          t))
                      :on-conflict :force)))
          (should (= (plist-get result :total) 1))
          (should (= (plist-get result :processed) 1))
          (should (= (plist-get result :modified) 1))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-delete-conflict-skip ()
  "Test delete conflict handling with :skip strategy."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 1)

        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        ;; Invalidate ETag before delete
                        (oset resource etag "invalid-etag")
                        :delete)
                      :on-conflict :skip)))
          (should (= (plist-get result :total) 1))
          (should (= (plist-get result :processed) 1))
          (should (= (plist-get result :deleted) 0))
          (should (= (plist-get result :skipped) 1))
          (should (= (length (plist-get result :errors)) 1))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-delete-conflict-force ()
  "Test delete conflict handling with :force strategy."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-put-ecard
         ecard-carddav-map-test--addressbook
         "/addressbooks/user/contacts/test.vcf"
         (ecard-carddav-map-test--create-test-ecard "Test User"))

        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (_resource)
                        :delete)
                      :on-conflict :force)))
          (should (= (plist-get result :total) 1))
          (should (= (plist-get result :processed) 1))
          (should (= (plist-get result :deleted) 1))

          ;; Verify resource was deleted
          (should-error
           (ecard-carddav-get-resource
            ecard-carddav-map-test--addressbook
            "/addressbooks/user/contacts/test.vcf")
           :type 'ecard-carddav-not-found-error)))
    (ecard-carddav-map-test--teardown)))

;;; Progress reporting tests

(ert-deftest ecard-carddav-map-test-progress-callback ()
  "Test progress callback is called with correct statistics."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 10)

        (let ((callback-calls nil))
          (ecard-carddav-map-resources
           ecard-carddav-map-test--addressbook
           (lambda (resource)
             (let ((ecard (oref resource ecard)))
               (ecard-add-property ecard 'note "Progress test")
               t))
           :progress-callback
           (lambda (stats)
             (push (copy-sequence stats) callback-calls)))

          ;; Should be called periodically (every 5 resources) plus final call
          (should (>= (length callback-calls) 3))

          ;; Verify statistics are accurate in final call
          (let ((final-stats (car callback-calls)))
            (should (= (plist-get final-stats :total) 10))
            (should (= (plist-get final-stats :processed) 10))
            (should (= (plist-get final-stats :modified) 10)))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-progress-incremental ()
  "Test progress callback shows incremental progress."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 15)

        (let ((callback-calls nil))
          (ecard-carddav-map-resources
           ecard-carddav-map-test--addressbook
           (lambda (_resource) nil)
           :progress-callback
           (lambda (stats)
             (push (plist-get stats :processed) callback-calls)))

          ;; Verify we got progress callbacks
          (should (>= (length callback-calls) 3))  ; At least 3 calls (5, 10, 15)

          ;; Verify final callback shows all processed
          (should (= (car callback-calls) 15))

          ;; Verify processed count never decreases
          (setq callback-calls (nreverse callback-calls))
          (let ((prev 0))
            (dolist (processed callback-calls)
              (should (>= processed prev))
              (setq prev processed)))))
    (ecard-carddav-map-test--teardown)))

;;; Batch processing tests

(ert-deftest ecard-carddav-map-test-custom-batch-size ()
  "Test custom batch size parameter."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 20)

        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let ((ecard (oref resource ecard)))
                          (ecard-add-property ecard 'note "Batch test")
                          t))
                      :batch-size 5)))  ; Process in batches of 5
          (should (= (plist-get result :total) 20))
          (should (= (plist-get result :processed) 20))
          (should (= (plist-get result :modified) 20))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-large-addressbook ()
  "Test handling of large addressbook."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 50)

        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        ;; Modify every 5th contact
                        (let* ((ecard (oref resource ecard))
                               (fn (ecard-get-property-value ecard 'fn)))
                          (when (string-match "Contact \\([0-9]+\\)" fn)
                            (let ((num (string-to-number (match-string 1 fn))))
                              (when (zerop (mod num 5))
                                (ecard-add-property ecard 'note "Large test")
                                t))))))))
          (should (= (plist-get result :total) 50))
          (should (= (plist-get result :processed) 50))
          (should (= (plist-get result :modified) 10))))  ; 10 multiples of 5
    (ecard-carddav-map-test--teardown)))

;;; Filter function tests

(ert-deftest ecard-carddav-map-test-filter-function ()
  "Test filter function to process subset of resources."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 10)

        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let ((ecard (oref resource ecard)))
                          (ecard-add-property ecard 'note "Filtered")
                          t))
                      :filter-fn
                      (lambda (path)
                        ;; Only process contacts 1-5
                        (string-match "contact[1-5]\\.vcf$" path)))))
          (should (= (plist-get result :total) 5))  ; Filtered to 5
          (should (= (plist-get result :processed) 5))
          (should (= (plist-get result :modified) 5))))
    (ecard-carddav-map-test--teardown)))

;;; Mixed operation tests

(ert-deftest ecard-carddav-map-test-mixed-operations ()
  "Test mix of modifications, deletions, and skips."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 12)

        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let* ((ecard (oref resource ecard))
                               (fn (ecard-get-property-value ecard 'fn))
                               (num (when (string-match "Contact \\([0-9]+\\)" fn)
                                      (string-to-number (match-string 1 fn)))))
                          (cond
                           ;; Delete multiples of 3
                           ((zerop (mod num 3)) :delete)
                           ;; Skip multiples of 2 (but not 3)
                           ((zerop (mod num 2)) :skip)
                           ;; Modify others (1, 5, 7, 11)
                           (t
                            (ecard-add-property ecard 'note "Modified")
                            t)))))))
          (should (= (plist-get result :total) 12))
          (should (= (plist-get result :processed) 12))
          (should (= (plist-get result :modified) 4))   ; 1, 5, 7, 11
          (should (= (plist-get result :deleted) 4))    ; 3, 6, 9, 12
          (should (= (plist-get result :skipped) 4))))  ; 2, 4, 8, 10
    (ecard-carddav-map-test--teardown)))

;;; Idempotency tests

(ert-deftest ecard-carddav-map-test-idempotent-transformation ()
  "Test running same transformation twice produces correct results."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 5)

        ;; First run - add note
        (let ((result1 (ecard-carddav-map-resources
                       ecard-carddav-map-test--addressbook
                       (lambda (resource)
                         (let ((ecard (oref resource ecard)))
                           (unless (oref ecard note)
                             (ecard-add-property ecard 'note "First run")
                             t))))))
          (should (= (plist-get result1 :modified) 5))

          ;; Second run - should not modify anything
          (let ((result2 (ecard-carddav-map-resources
                         ecard-carddav-map-test--addressbook
                         (lambda (resource)
                           (let ((ecard (oref resource ecard)))
                             (unless (oref ecard note)
                               (ecard-add-property ecard 'note "First run")
                               t))))))
            ;; Second run should have 0 modifications because notes already exist
            (should (= (plist-get result2 :modified) 0))
            ;; No resources are explicitly skipped - function returns nil
            ;; which is "no change", not :skip
            (should (= (plist-get result2 :skipped) 0)))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-multiple-runs-different-transformations ()
  "Test running different transformations sequentially."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        (ecard-carddav-map-test--populate-addressbook 3)

        ;; First transformation - add note
        (ecard-carddav-map-resources
         ecard-carddav-map-test--addressbook
         (lambda (resource)
           (let ((ecard (oref resource ecard)))
             (ecard-add-property ecard 'note "First")
             t)))

        ;; Second transformation - add title
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let ((ecard (oref resource ecard)))
                          (ecard-add-property ecard 'title "Manager")
                          t)))))
          (should (= (plist-get result :modified) 3))

          ;; Verify both properties exist
          (let ((test-resource (ecard-carddav-get-resource
                               ecard-carddav-map-test--addressbook
                               "/addressbooks/user/contacts/contact1.vcf")))
            (should (oref (oref test-resource ecard) note))
            (should (oref (oref test-resource ecard) title)))))
    (ecard-carddav-map-test--teardown)))

;;; Real-world usage tests

(ert-deftest ecard-carddav-map-test-add-missing-uid ()
  "Test adding UID to contacts that lack one."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        ;; Create contacts, some with UID, some without
        (dotimes (i 5)
          (let* ((name (format "Contact %d" (1+ i)))
                 (path (format "/addressbooks/user/contacts/contact%d.vcf" (1+ i)))
                 (card (ecard-create :fn name :email "test@example.com")))
            ;; Only add UID to odd-numbered contacts
            (when (= 0 (mod i 2))
              (ecard-add-property card 'uid (format "uuid-%d" i)))
            (ecard-carddav-put-ecard ecard-carddav-map-test--addressbook path card)))

        ;; Add UID to contacts that don't have one
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let ((ecard (oref resource ecard)))
                          (unless (oref ecard uid)
                            (ecard-add-property ecard 'uid
                                               (format "generated-%s"
                                                      (md5 (ecard-get-property-value ecard 'fn))))
                            t))))))
          (should (= (plist-get result :total) 5))
          (should (= (plist-get result :modified) 2))  ; 2 even-numbered contacts

          ;; Verify all contacts now have UID
          (let ((resources (ecard-carddav-list-resources
                           ecard-carddav-map-test--addressbook)))
            (dolist (resource-info resources)
              (let ((resource (ecard-carddav-get-resource
                              ecard-carddav-map-test--addressbook
                              (oref resource-info path))))
                (should (oref (oref resource ecard) uid)))))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-normalize-phone-numbers ()
  "Test normalizing phone number format across all contacts."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        ;; Create contacts with various phone formats
        (let ((phones '("+1-555-1234" "555-5678" "(555) 9012" "555.3456")))
          (dotimes (i 4)
            (let ((card (ecard-create
                        :fn (format "Contact %d" (1+ i))
                        :tel (nth i phones))))
              (ecard-carddav-put-ecard
               ecard-carddav-map-test--addressbook
               (format "/addressbooks/user/contacts/contact%d.vcf" (1+ i))
               card))))

        ;; Normalize all phone numbers
        (let ((result (ecard-carddav-map-resources
                      ecard-carddav-map-test--addressbook
                      (lambda (resource)
                        (let ((ecard (oref resource ecard)))
                          (when-let ((tels (oref ecard tel)))
                            (dolist (tel tels)
                              ;; Simple normalization: remove all non-digits
                              (let ((value (oref tel value)))
                                (oset tel value
                                     (replace-regexp-in-string "[^0-9]" "" value))))
                            t))))))
          (should (= (plist-get result :modified) 4))

          ;; Verify normalization
          (let ((resource (ecard-carddav-get-resource
                          ecard-carddav-map-test--addressbook
                          "/addressbooks/user/contacts/contact1.vcf")))
            (let* ((ecard (oref resource ecard))
                   (tel (car (oref ecard tel))))
              (should (string= (oref tel value) "15551234"))))))
    (ecard-carddav-map-test--teardown)))

(ert-deftest ecard-carddav-map-test-bulk-delete-duplicates ()
  "Test deleting duplicate contacts based on email."
  (ecard-carddav-map-test--setup)
  (unwind-protect
      (progn
        ;; Create contacts with duplicate emails
        (dotimes (i 6)
          (let* ((name (format "Contact %d" (1+ i)))
                 (email (if (< i 3)
                           "duplicate@example.com"
                         (format "unique%d@example.com" i)))
                 (card (ecard-create :fn name :email email)))
            (ecard-carddav-put-ecard
             ecard-carddav-map-test--addressbook
             (format "/addressbooks/user/contacts/contact%d.vcf" (1+ i))
             card)))

        ;; List resources in path order to ensure consistent processing
        (let ((all-resources (ecard-carddav-list-resources
                             ecard-carddav-map-test--addressbook)))
          ;; Sort by path for deterministic processing
          (setq all-resources (sort all-resources
                                   (lambda (a b)
                                     (string< (oref a path) (oref b path)))))

          ;; Manually process in order
          (let ((seen-emails (make-hash-table :test 'equal))
                (delete-count 0))
            (dolist (resource-info all-resources)
              (let* ((resource (ecard-carddav-get-resource
                               ecard-carddav-map-test--addressbook
                               (oref resource-info path)))
                     (ecard (oref resource ecard))
                     (email (ecard-get-property-value ecard 'email)))
                (if (gethash email seen-emails)
                    (progn
                      (ecard-carddav-delete-resource
                       ecard-carddav-map-test--addressbook
                       (oref resource path))
                      (setq delete-count (1+ delete-count)))
                  (puthash email t seen-emails))))

            (should (= delete-count 2))  ; 2 duplicates deleted

            ;; Verify only 4 contacts remain
            (let ((remaining (ecard-carddav-list-resources
                             ecard-carddav-map-test--addressbook)))
              (should (= (length remaining) 4))))))
    (ecard-carddav-map-test--teardown)))

(provide 'ecard-carddav-map-test)
;;; ecard-carddav-map-test.el ends here
