;;; ecard-tools-test.el --- Tests for ecard-tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;;; Commentary:

;; Comprehensive test suite for ecard-tools using ERT.

;;; Code:

(require 'ert)
(require 'ecard)  ;; Required for ecard API
(require 'ecard-tools-adapter)  ;; Required for adapter layer
(require 'ecard-tools)
(require 'ecard-tools-curator)

;; ============================================================================
;; Test Data
;; ============================================================================

(defconst ecard-tools-test--simple-vcard
  "BEGIN:VCARD
VERSION:3.0
FN:John Doe
N:Doe;John;;;
EMAIL;TYPE=WORK:john@example.com
TEL;TYPE=CELL:555-1234
ORG:Acme Corp
TITLE:Developer
UID:test-uid-123
END:VCARD"
  "Simple VCard for testing.")

(defconst ecard-tools-test--multi-vcard
  "BEGIN:VCARD
VERSION:3.0
FN:Jane Smith
N:Smith;Jane;;;
EMAIL;TYPE=HOME:jane@example.com
UID:test-uid-456
END:VCARD
BEGIN:VCARD
VERSION:3.0
FN:Bob Johnson
N:Johnson;Bob;;;
EMAIL;TYPE=WORK:bob@company.com
TEL;TYPE=WORK:555-5678
UID:test-uid-789
END:VCARD"
  "Multi-entry VCard file for testing.")

(defconst ecard-tools-test--incomplete-vcard
  "BEGIN:VCARD
VERSION:3.0
EMAIL:incomplete@example.com
END:VCARD"
  "Incomplete VCard missing required fields.")

(defconst ecard-tools-test--vcard-with-special-chars
  "BEGIN:VCARD
VERSION:3.0
FN:Müller\\, José
N:Müller;José;;;
NOTE:Line 1\\nLine 2\\nLine 3
EMAIL:jose.muller@example.com
UID:test-special-123
END:VCARD"
  "VCard with special characters and escaping.")

(defconst ecard-tools-test--facebook-vcard
  "BEGIN:VCARD
VERSION:3.0
FN:Facebook User
EMAIL:user@example.com
EMAIL:facebook_user@facebook.com
UID:fb-test-123
END:VCARD"
  "VCard with Facebook email.")

;; ============================================================================
;; Parser Tests
;; ============================================================================

(ert-deftest ecard-tools-test-parse-simple ()
  "Test parsing a simple VCard."
  (let ((vcards (with-temp-buffer
                   (insert ecard-tools-test--simple-vcard)
                   (ecard-tools-parse-buffer (current-buffer)))))
    (should (= (length vcards) 1))
    (let ((vcard (car vcards)))
      (should (equal (ecard-tools-vcard-fn vcard) "John Doe"))
      (should (equal (ecard-tools-vcard-uid vcard) "test-uid-123"))
      (should (equal (ecard-tools-vcard-org vcard) "Acme Corp"))
      (should (equal (ecard-tools-vcard-title vcard) "Developer"))
      (should (= (length (ecard-tools-vcard-email vcard)) 1))
      (should (equal (ecard-tools-email-value
                     (car (ecard-tools-vcard-email vcard)))
                    "john@example.com"))
      (should (eq (ecard-tools-email-type
                  (car (ecard-tools-vcard-email vcard)))
                 'work)))))

(ert-deftest ecard-tools-test-parse-multi ()
  "Test parsing multi-entry VCard file."
  (let ((vcards (with-temp-buffer
                   (insert ecard-tools-test--multi-vcard)
                   (ecard-tools-parse-buffer (current-buffer)))))
    (should (= (length vcards) 2))
    (should (equal (ecard-tools-vcard-fn (nth 0 vcards)) "Jane Smith"))
    (should (equal (ecard-tools-vcard-fn (nth 1 vcards)) "Bob Johnson"))
    (should (equal (ecard-tools-vcard-uid (nth 0 vcards)) "test-uid-456"))
    (should (equal (ecard-tools-vcard-uid (nth 1 vcards)) "test-uid-789"))))

(ert-deftest ecard-tools-test-parse-special-chars ()
  "Test parsing VCard with special characters."
  (let ((vcards (with-temp-buffer
                   (insert ecard-tools-test--vcard-with-special-chars)
                   (ecard-tools-parse-buffer (current-buffer)))))
    (should (= (length vcards) 1))
    (let ((vcard (car vcards)))
      (should (equal (ecard-tools-vcard-fn vcard) "Müller, José"))
      (should (string-match-p "Line 1\nLine 2\nLine 3"
                              (ecard-tools-vcard-note vcard))))))

(ert-deftest ecard-tools-test-parse-name-components ()
  "Test parsing N field components."
  (let ((vcards (with-temp-buffer
                   (insert "BEGIN:VCARD\nVERSION:3.0\n")
                   (insert "FN:Given Family\n")
                   (insert "N:Family;Given;Additional;Prefix;Suffix\n")
                   (insert "END:VCARD")
                   (ecard-tools-parse-buffer (current-buffer)))))
    (let ((n-field (ecard-tools-vcard-n (car vcards))))
      (should (equal (nth 0 n-field) "Family"))
      (should (equal (nth 1 n-field) "Given"))
      (should (equal (nth 2 n-field) "Additional"))
      (should (equal (nth 3 n-field) "Prefix"))
      (should (equal (nth 4 n-field) "Suffix")))))

;; ============================================================================
;; Serialization Tests
;; ============================================================================

(ert-deftest ecard-tools-test-serialize ()
  "Test serializing a VCard."
  (let* ((vcards (with-temp-buffer
                    (insert ecard-tools-test--simple-vcard)
                    (ecard-tools-parse-buffer (current-buffer))))
         (vcard (car vcards))
         (serialized (ecard-tools-serialize vcard)))
    (should (string-match-p "BEGIN:VCARD" serialized))
    (should (string-match-p "VERSION:3.0" serialized))
    (should (string-match-p "FN:John Doe" serialized))
    (should (string-match-p "UID:test-uid-123" serialized))
    (should (string-match-p "END:VCARD" serialized))))

(ert-deftest ecard-tools-test-serialize-roundtrip ()
  "Test that parse->serialize->parse preserves data."
  (let* ((vcards1 (with-temp-buffer
                     (insert ecard-tools-test--simple-vcard)
                     (ecard-tools-parse-buffer (current-buffer))))
         (vcard1 (car vcards1))
         (serialized (ecard-tools-serialize vcard1))
         (vcards2 (with-temp-buffer
                     (insert serialized)
                     (ecard-tools-parse-buffer (current-buffer))))
         (vcard2 (car vcards2)))
    (should (equal (ecard-tools-vcard-fn vcard1)
                  (ecard-tools-vcard-fn vcard2)))
    (should (equal (ecard-tools-vcard-uid vcard1)
                  (ecard-tools-vcard-uid vcard2)))
    (should (equal (ecard-tools-vcard-org vcard1)
                  (ecard-tools-vcard-org vcard2)))))

;; ============================================================================
;; Validation Tests
;; ============================================================================

(ert-deftest ecard-tools-test-validation-valid ()
  "Test validation of valid VCard."
  (let* ((vcard (ecard-tools-vcard--create
                :fn "Test Name"
                :uid "test-123"
                :email (list (ecard-tools-email-create
                            :value "test@example.com"))))
         (result (ecard-tools-validate vcard)))
    (should (ecard-tools-result-success-p result))
    (should (null (ecard-tools-result-errors result)))))

(ert-deftest ecard-tools-test-validation-missing-fn ()
  "Test validation catches missing FN field."
  (let* ((vcard (ecard-tools-vcard--create :uid "test-123"))
         (result (ecard-tools-validate vcard)))
    (should-not (ecard-tools-result-success-p result))
    (should (member "Missing required field: FN (Formatted Name)"
                   (ecard-tools-result-errors result)))))

(ert-deftest ecard-tools-test-validation-invalid-email ()
  "Test validation catches invalid email."
  (let* ((vcard (ecard-tools-vcard--create
                :fn "Test"
                :email (list (ecard-tools-email-create
                            :value "invalid-email"))))
         (result (ecard-tools-validate vcard t)))
    (should (ecard-tools-result-success-p result))  ; Warnings don't fail
    (should (member "Invalid email format: invalid-email"
                   (ecard-tools-result-warnings result)))))

;; ============================================================================
;; Auto-Repair Tests
;; ============================================================================

(ert-deftest ecard-tools-test-auto-repair-uid ()
  "Test auto-repair adds missing UID."
  (let* ((vcard (ecard-tools-vcard--create :fn "Test"))
         (result (ecard-tools-auto-repair vcard))
         (repaired (ecard-tools-result-data result)))
    (should (ecard-tools-result-success-p result))
    (should (ecard-tools-vcard-uid repaired))
    (should (member "Added missing UID"
                   (ecard-tools-result-warnings result)))))

(ert-deftest ecard-tools-test-auto-repair-fn-from-email ()
  "Test auto-repair generates FN from email."
  (let* ((vcard (ecard-tools-vcard--create
                :email (list (ecard-tools-email-create
                            :value "john.doe@example.com"))))
         (result (ecard-tools-auto-repair vcard))
         (repaired (ecard-tools-result-data result)))
    (should (ecard-tools-result-success-p result))
    (should (equal (ecard-tools-vcard-fn repaired) "John Doe"))
    (should (member "Generated FN from email"
                   (ecard-tools-result-warnings result)))))

(ert-deftest ecard-tools-test-auto-repair-fn-from-n ()
  "Test auto-repair generates FN from N field."
  (let* ((vcard (ecard-tools-vcard--create
                :n '("Smith" "Jane" "" "Dr." "")))
         (result (ecard-tools-auto-repair vcard))
         (repaired (ecard-tools-result-data result)))
    (should (ecard-tools-result-success-p result))
    (should (equal (ecard-tools-vcard-fn repaired) "Dr. Jane Smith"))))

;; ============================================================================
;; Utility Function Tests
;; ============================================================================

(ert-deftest ecard-tools-test-email-validation ()
  "Test email validation function."
  (should (ecard-tools--valid-email-p "test@example.com"))
  (should (ecard-tools--valid-email-p "user.name+tag@example.co.uk"))
  (should (ecard-tools--valid-email-p "x@y.io"))
  (should-not (ecard-tools--valid-email-p "invalid"))
  (should-not (ecard-tools--valid-email-p "@example.com"))
  (should-not (ecard-tools--valid-email-p "user@"))
  (should-not (ecard-tools--valid-email-p "user@.com")))

(ert-deftest ecard-tools-test-phone-validation ()
  "Test phone validation function."
  (should (ecard-tools--valid-phone-p "+1-555-1234"))
  (should (ecard-tools--valid-phone-p "555 1234"))
  (should (ecard-tools--valid-phone-p "(555) 123-4567"))
  (should-not (ecard-tools--valid-phone-p "abc123"))
  (should-not (ecard-tools--valid-phone-p "555-CALL")))

(ert-deftest ecard-tools-test-name-guessing ()
  "Test name guessing from email."
  (should (equal (ecard-tools--guess-name-from-email "john.doe@example.com")
                "John Doe"))
  (should (equal (ecard-tools--guess-name-from-email "jane_smith@example.com")
                "Jane Smith"))
  (should (equal (ecard-tools--guess-name-from-email "bob-jones@example.com")
                "Bob Jones"))
  (should (equal (ecard-tools--guess-name-from-email "alice+tag@example.com")
                "Alice Tag")))

(ert-deftest ecard-tools-test-uid-generation ()
  "Test UID generation creates unique values."
  (let ((uid1 (ecard-tools--generate-uid))
        (uid2 (ecard-tools--generate-uid)))
    (should (stringp uid1))
    (should (stringp uid2))
    (should-not (equal uid1 uid2))
    (should (string-match-p "@emacs-ecard-tools$" uid1))))

;; ============================================================================
;; Tool Function Tests
;; ============================================================================

(ert-deftest ecard-tools-test-junk-detection ()
  "Test junk VCard detection."
  (let ((junk-vcard (ecard-tools-vcard--create
                    :email (list (ecard-tools-email-create
                                :value "noreply@example.com"))))
        (good-vcard (ecard-tools-vcard--create
                    :fn "John Doe"
                    :email (list (ecard-tools-email-create
                                :value "john@example.com")))))
    (should (ecard-tools--is-junk-vcard-p junk-vcard))
    (should-not (ecard-tools--is-junk-vcard-p good-vcard))))

(ert-deftest ecard-tools-test-facebook-email-removal ()
  "Test Facebook email removal logic."
  (let* ((vcards (with-temp-buffer
                   (insert ecard-tools-test--facebook-vcard)
                   ;; Keep buffer alive while parsing
                   (ecard-tools-parse-buffer (current-buffer))))
         (vcard (car vcards)))
    (should (= (length (ecard-tools-vcard-email vcard)) 2))

    ;; Filter Facebook emails
    (let ((filtered (seq-remove
                    (lambda (email)
                      (string-match-p "@facebook\\.com$"
                                    (ecard-tools-email-value email)))
                    (ecard-tools-vcard-email vcard))))
      (should (= (length filtered) 1))
      (should (equal (ecard-tools-email-value (car filtered))
                    "user@example.com")))))

(ert-deftest ecard-tools-test-simple-duplicate-key ()
  "Test simple duplicate key generation."
  (let ((vcard1 (ecard-tools-vcard--create
                :fn "John Doe"
                :email (list (ecard-tools-email-create
                            :value "john@example.com"))))
        (vcard2 (ecard-tools-vcard--create
                :fn "John Doe"
                :email (list (ecard-tools-email-create
                            :value "john@example.com"))))
        (vcard3 (ecard-tools-vcard--create
                :fn "Jane Smith"
                :email (list (ecard-tools-email-create
                            :value "jane@example.com")))))
    (should (equal (ecard-tools--vcard-simple-key vcard1)
                  (ecard-tools--vcard-simple-key vcard2)))
    (should-not (equal (ecard-tools--vcard-simple-key vcard1)
                      (ecard-tools--vcard-simple-key vcard3)))))

;; ============================================================================
;; Similarity Tests
;; ============================================================================

(ert-deftest ecard-tools-test-string-similarity ()
  "Test string similarity calculation."
  (should (= (ecard-tools--string-similarity "test" "test") 1.0))
  (should (= (ecard-tools--string-similarity "" "") 1.0))
  (should (> (ecard-tools--string-similarity "hello" "hallo") 0.5))
  (should (< (ecard-tools--string-similarity "abc" "xyz") 0.5))
  (should (> (ecard-tools--string-similarity "John Doe" "John Smith") 0.3)))

(ert-deftest ecard-tools-test-levenshtein-distance ()
  "Test Levenshtein distance calculation."
  (should (= (ecard-tools--levenshtein-distance "test" "test") 0))
  (should (= (ecard-tools--levenshtein-distance "cat" "hat") 1))
  (should (= (ecard-tools--levenshtein-distance "saturday" "sunday") 3))
  (should (= (ecard-tools--levenshtein-distance "" "test") 4))
  (should (= (ecard-tools--levenshtein-distance "test" "") 4)))

(ert-deftest ecard-tools-test-vcard-similarity ()
  "Test VCard similarity calculation."
  (let ((vcard1 (ecard-tools-vcard--create
                :fn "John Doe"
                :email (list (ecard-tools-email-create
                            :value "john@example.com"))
                :org "Acme Corp"))
        (vcard2 (ecard-tools-vcard--create
                :fn "John Doe"
                :email (list (ecard-tools-email-create
                            :value "john@example.com"))
                :org "Acme Corp"))
        (vcard3 (ecard-tools-vcard--create
                :fn "Jane Smith"
                :email (list (ecard-tools-email-create
                            :value "jane@example.com"))
                :org "Other Inc")))
    (should (= (ecard-tools--vcard-similarity vcard1 vcard2) 1.0))
    (should (< (ecard-tools--vcard-similarity vcard1 vcard3) 0.5))))

;; ============================================================================
;; Merge Tests
;; ============================================================================

(ert-deftest ecard-tools-test-merge-emails ()
  "Test merging VCards deduplicates emails."
  (let ((vcard1 (ecard-tools-vcard--create
                :fn "John Doe"
                :email (list (ecard-tools-email-create
                            :value "john@example.com")
                           (ecard-tools-email-create
                            :value "john@work.com"))))
        (vcard2 (ecard-tools-vcard--create
                :fn "John Doe"
                :email (list (ecard-tools-email-create
                            :value "john@example.com")
                           (ecard-tools-email-create
                            :value "john@personal.com")))))
    (let ((merged (ecard-tools-merge-vcards vcard1 vcard2)))
      (should (= (length (ecard-tools-vcard-email merged)) 3))
      (let ((emails (mapcar #'ecard-tools-email-value
                           (ecard-tools-vcard-email merged))))
        (should (member "john@example.com" emails))
        (should (member "john@work.com" emails))
        (should (member "john@personal.com" emails))))))

(ert-deftest ecard-tools-test-merge-fields ()
  "Test merging VCards combines fields correctly."
  (let ((vcard1 (ecard-tools-vcard--create
                :fn "John Doe"
                :org "Acme Corp"))
        (vcard2 (ecard-tools-vcard--create
                :fn "John Doe"
                :title "Developer"
                :note "Important contact")))
    (let ((merged (ecard-tools-merge-vcards vcard1 vcard2)))
      (should (equal (ecard-tools-vcard-org merged) "Acme Corp"))
      (should (equal (ecard-tools-vcard-title merged) "Developer"))
      (should (equal (ecard-tools-vcard-note merged) "Important contact")))))

;; ============================================================================
;; File I/O Tests
;; ============================================================================

(ert-deftest ecard-tools-test-file-io-roundtrip ()
  "Test file I/O operations with temporary files."
  (let ((temp-file (make-temp-file "vcard-test" nil ".vcf"))
        (vcard (ecard-tools-vcard--create
               :fn "Test User"
               :uid "test-io-123"
               :email (list (ecard-tools-email-create
                           :value "test@example.com")))))
    (unwind-protect
        (progn
          ;; Write
          (let ((result (ecard-tools-write-file vcard temp-file)))
            (should (ecard-tools-result-success-p result)))

          ;; Read
          (let* ((result (ecard-tools-read-file temp-file))
                 (vcards (ecard-tools-result-data result)))
            (should (ecard-tools-result-success-p result))
            (should (= (length vcards) 1))
            (let ((read-vcard (car vcards)))
              (should (equal (ecard-tools-vcard-fn read-vcard) "Test User"))
              (should (equal (ecard-tools-vcard-uid read-vcard) "test-io-123")))))
      (delete-file temp-file))))

(ert-deftest ecard-tools-test-default-filename ()
  "Test default filename generation."
  (let ((vcard-with-uid (ecard-tools-vcard--create
                        :uid "test-123"))
        (vcard-without-uid (ecard-tools-vcard--create
                          :fn "Test")))
    (should (equal (ecard-tools--default-filename vcard-with-uid)
                  "test_123.vcf"))
    (let ((filename (ecard-tools--default-filename vcard-without-uid)))
      (should (string-match-p "^[0-9]+-[a-f0-9]+\\.vcf$" filename)))))

;; ============================================================================
;; Integration Tests
;; ============================================================================

(ert-deftest ecard-tools-test-complete-workflow ()
  "Test complete workflow: parse, validate, repair, serialize."
  (let* ((vcards (with-temp-buffer
                   (insert ecard-tools-test--incomplete-vcard)
                   ;; Keep buffer alive while parsing
                   (ecard-tools-parse-buffer (current-buffer))))
         (vcard (car vcards)))

    ;; Initial validation should fail
    (let ((result (ecard-tools-validate vcard)))
      (should-not (ecard-tools-result-success-p result)))

    ;; Auto-repair
    (let* ((repair-result (ecard-tools-auto-repair vcard))
           (repaired (ecard-tools-result-data repair-result)))

      ;; Should have added UID and FN
      (should (ecard-tools-vcard-uid repaired))
      (should (ecard-tools-vcard-fn repaired))

      ;; Validation should now pass
      (let ((result (ecard-tools-validate repaired)))
        (should (ecard-tools-result-success-p result)))

      ;; Should serialize successfully
      (let ((serialized (ecard-tools-serialize repaired)))
        (should (string-match-p "BEGIN:VCARD" serialized))
        (should (string-match-p "END:VCARD" serialized))))))

;; ============================================================================
;; Performance Tests
;; ============================================================================

(ert-deftest ecard-tools-test-performance-large-file ()
  "Test performance with large number of VCards."
  (let ((start-time (float-time))
        (vcards (with-temp-buffer
                  (dotimes (_ 100)
                    (insert ecard-tools-test--simple-vcard)
                    (insert "\n"))
                  ;; Keep buffer alive while parsing
                  (ecard-tools-parse-buffer (current-buffer)))))
    (should (= (length vcards) 100))
    (let ((elapsed (- (float-time) start-time)))
      (message "Parsed 100 VCards in %.3f seconds" elapsed)
      (should (< elapsed 1.0)))))  ; Should parse in under 1 second

;; ============================================================================
;; Edge Case Tests
;; ============================================================================

(ert-deftest ecard-tools-test-empty-fields ()
  "Test handling of empty fields."
  (let ((vcard (ecard-tools-vcard--create
               :fn ""
               :email (list (ecard-tools-email-create :value "valid@example.com"))
               :tel (list (ecard-tools-tel-create :value "555-1234")))))
    (let ((serialized (ecard-tools-serialize vcard)))
      (should (string-match-p "FN:" serialized))
      (should (string-match-p "EMAIL" serialized))
      (should (string-match-p "TEL" serialized))

      ;; Verify that creating with truly empty values filters them out
      (let ((vcard-empty (ecard-tools-vcard--create
                          :fn "Test"
                          :email (list (ecard-tools-email-create :value ""))
                          :tel (list (ecard-tools-tel-create :value "")))))
        (should (equal (ecard-tools-vcard-fn vcard-empty) "Test"))
        ;; Empty email/tel should be filtered out
        (should (null (ecard-tools-vcard-email vcard-empty)))
        (should (null (ecard-tools-vcard-tel vcard-empty)))))))

(ert-deftest ecard-tools-test-malformed-vcard ()
  "Test handling of malformed VCard."
  (let* ((malformed "BEGIN:VCARD\nVERSION:3.0\nThis is not valid\nEND:VCARD")
         (vcards (with-temp-buffer
                   (insert malformed)
                   ;; Keep buffer alive while parsing
                   (ecard-tools-parse-buffer (current-buffer)))))
    (should (= (length vcards) 1))
    (let ((vcard (car vcards)))
      ;; Malformed vcard may still parse but should be missing key fields
      (should-not (ecard-tools-vcard-fn vcard))
      (should-not (ecard-tools-vcard-uid vcard)))))

(ert-deftest ecard-tools-test-line-folding ()
  "Test handling of folded lines in VCard."
  (let ((folded "BEGIN:VCARD\nVERSION:3.0\nFN:Very Long Name That\n Continues On Next Line\nEND:VCARD"))
    (let ((vcards (with-temp-buffer
                     (insert folded)
                     (ecard-tools-parse-buffer (current-buffer)))))
      (should (= (length vcards) 1))
      (let ((vcard (car vcards)))
        (should (equal (ecard-tools-vcard-fn vcard)
                      "Very Long Name That Continues On Next Line"))))))

;; ============================================================================
;; Run Tests
;; ============================================================================

(defun ecard-tools-run-tests ()
  "Run all ecard-tools tests."
  (interactive)
  (ert-run-tests-interactively "^ecard-tools-test-"))

(provide 'ecard-tools-test)

;;; ecard-tools-test.el ends here