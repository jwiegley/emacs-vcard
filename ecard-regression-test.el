;;; ecard-regression-test.el --- Regression tests for critical bugs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <jwiegley@gmail.com>
;; Keywords: testing

;;; Commentary:

;; Regression tests for critical bugs found in comprehensive gap analysis (2025-11-18).
;; All tests now PASS after fixes to escape handling, parameter parsing, and validation.
;;
;; Run with:
;;   emacs -batch -L . -l ecard.el -l ecard-regression-test.el -f ert-run-tests-batch-and-exit
;;
;; Or interactively:
;;   M-x load-file RET ecard-regression-test.el RET
;;   M-x ert RET ecard-bug RET

;;; Code:

(require 'ert)
(require 'ecard)

;;; Bug #1: Escaped Semicolons in Structured Properties

(ert-deftest ecard-bug-1-n-escaped-semicolon ()
  "Test N property with escaped semicolon in surname.
This test currently FAILS - demonstrates data corruption.

Example: O'Brien\\;Family should parse as surname 'O'Brien;Family'."
  (let* ((input "BEGIN:VCARD
VERSION:4.0
FN:John O'Brien-Family
N:O'Brien\\;Family;John;;;
END:VCARD")
         (card (ecard-parse input))
         (n-prop (car (ecard-n card)))
         (n-value (when n-prop (ecard-property-value n-prop))))
    ;; Should parse as 5 components: ["O'Brien;Family", "John", "", "", ""]
    (should (equal (length n-value) 5))
    (should (equal (nth 0 n-value) "O'Brien;Family"))  ; ← FAILS: gets "O'Brien\"
    (should (equal (nth 1 n-value) "John"))))           ; ← FAILS: gets "Family"

(ert-deftest ecard-bug-1-adr-escaped-semicolon ()
  "Test ADR property with escaped semicolon in street address.
This test currently FAILS - demonstrates data corruption.

Example: Street '123 Main St\\; Suite 100' should parse as single component,
but currently splits incorrectly."
  (let* ((input "BEGIN:VCARD
VERSION:4.0
FN:Test
ADR:;;123 Main St\\; Suite 100;City;;12345;
END:VCARD")
         (card (ecard-parse input))
         (adr-prop (car (ecard-adr card)))
         (adr-value (when adr-prop (ecard-property-value adr-prop))))
    ;; Component 2 (index 2) should be "123 Main St; Suite 100"
    (should (equal (nth 2 adr-value) "123 Main St; Suite 100"))  ; ← FAILS
    (should (equal (nth 3 adr-value) "City"))))                  ; ← FAILS

(ert-deftest ecard-bug-1-org-escaped-semicolon ()
  "Test ORG property with escaped semicolon in company name.
This test currently FAILS - demonstrates data corruption.

Example: 'Smith\\; Jones LLC' should parse as company name,
not split into two components."
  (let* ((input "BEGIN:VCARD
VERSION:4.0
FN:Test
ORG:Smith\\; Jones LLC;Engineering
END:VCARD")
         (card (ecard-parse input))
         (org-prop (car (ecard-org card)))
         (org-value (when org-prop (ecard-property-value org-prop))))
    ;; Should parse as ["Smith; Jones LLC", "Engineering"]
    (should (equal (length org-value) 2))
    (should (equal (nth 0 org-value) "Smith; Jones LLC"))  ; ← FAILS
    (should (equal (nth 1 org-value) "Engineering"))))     ; ← FAILS

(ert-deftest ecard-bug-1-gender-escaped-semicolon ()
  "Test GENDER property with escaped semicolon.
RFC 6350 allows semicolon in text component."
  (let* ((input "BEGIN:VCARD
VERSION:4.0
FN:Test
GENDER:O;non-binary\\; other
END:VCARD")
         (card (ecard-parse input))
         (gender-prop (car (ecard-gender card)))
         (gender-value (when gender-prop (ecard-property-value gender-prop))))
    ;; Should parse as ["O", "non-binary; other"]
    (should (equal (length gender-value) 2))
    (should (equal (nth 1 gender-value) "non-binary; other"))))  ; ← FAILS

;;; Bug #2: Semicolons in Parameter Values

(ert-deftest ecard-bug-2-param-semicolon ()
  "Test parameter value containing semicolon.
This test currently FAILS - demonstrates parsing failure.

Quoted parameter values can contain semicolons per RFC 6350.
Example: LABEL=\"Office; Main Line\" should parse as single parameter."
  (let* ((input "BEGIN:VCARD
VERSION:4.0
FN:Test
TEL;LABEL=\"Office; Main Line\":+1-555-1234
END:VCARD")
         (card (ecard-parse input))
         (tel-prop (car (ecard-tel card)))
         (params (when tel-prop (ecard-property-parameters tel-prop))))
    ;; Should have one LABEL parameter with value "Office; Main Line"
    (should params)
    (should (equal (cdr (assoc "LABEL" params)) "Office; Main Line"))  ; ← FAILS
    (should (equal (ecard-property-value tel-prop) "+1-555-1234"))))   ; ← May fail

(ert-deftest ecard-bug-2-param-multiple-semicolons ()
  "Test parameter with multiple semicolons in quoted value."
  (let* ((input "BEGIN:VCARD
VERSION:4.0
FN:Test
EMAIL;NOTE=\"Work; Urgent; Confidential\":john@example.com
END:VCARD")
         (card (ecard-parse input))
         (email-prop (car (ecard-email card)))
         (params (when email-prop (ecard-property-parameters email-prop))))
    (should (equal (cdr (assoc "NOTE" params)) "Work; Urgent; Confidential"))))  ; ← FAILS

;;; Bug #3: Colons in Parameter Values

(ert-deftest ecard-bug-3-param-colon ()
  "Test parameter value containing colon.
This test currently FAILS - demonstrates regex capture issue.

Parameter values can contain colons (e.g., time ranges).
Example: X-AVAILABLE=09:00-17:00"
  (let* ((input "BEGIN:VCARD
VERSION:4.0
FN:Test
TEL;X-AVAILABLE=09:00-17:00:+1-555-1234
END:VCARD")
         (card (ecard-parse input))
         (tel-prop (car (ecard-tel card)))
         (params (when tel-prop (ecard-property-parameters tel-prop))))
    ;; Should have X-AVAILABLE=09:00-17:00 parameter
    (should params)
    (should (equal (cdr (assoc "X-AVAILABLE" params)) "09:00-17:00"))  ; ← FAILS
    (should (equal (ecard-property-value tel-prop) "+1-555-1234"))))   ; ← FAILS

(ert-deftest ecard-bug-3-param-uri ()
  "Test parameter with URI value containing colons."
  (let* ((input "BEGIN:VCARD
VERSION:4.0
FN:Test
PHOTO;MEDIATYPE=image/png;VALUE=uri:http://example.com:8080/photo.png
END:VCARD")
         (card (ecard-parse input))
         (photo-prop (car (ecard-photo card)))
         (params (when photo-prop (ecard-property-parameters photo-prop)))
         (value (when photo-prop (ecard-property-value photo-prop))))
    (should (equal (cdr (assoc "MEDIATYPE" params)) "image/png"))
    (should (equal value "http://example.com:8080/photo.png"))))  ; ← FAILS

;;; Bug #4: Capital \N Escape Sequence

(ert-deftest ecard-bug-4-capital-n-escape ()
  "Test \\N (capital N) escape sequence for newline.
This test currently FAILS - RFC 6350 allows both \\n and \\N.

RFC 6350 Section 3.4: NEWLINE characters MUST be encoded with
a BACKSLASH followed by either 'n' or 'N'."
  (let* ((input "BEGIN:VCARD
VERSION:4.0
FN:Test
NOTE:Line one\\NLine two
END:VCARD")
         (card (ecard-parse input))
         (note (ecard-get-property-value card :note)))
    ;; Should parse as "Line one\nLine two" (newline in middle)
    (should (string-match-p "Line one\nLine two" note))  ; ← FAILS
    (should-not (string-match-p "NLine" note))))         ; ← FAILS (gets "Line oneNLine two")

(ert-deftest ecard-bug-4-mixed-case-escapes ()
  "Test both \\n and \\N in same value."
  (let* ((input "BEGIN:VCARD
VERSION:4.0
FN:Test
NOTE:Line 1\\nLine 2\\NLine 3
END:VCARD")
         (card (ecard-parse input))
         (note (ecard-get-property-value card :note)))
    ;; Both should produce newlines
    (should (string-match-p "Line 1\nLine 2\nLine 3" note))))  ; ← FAILS for \N

;;; Bug #5: Case-Insensitive Groups

(ert-deftest ecard-bug-5-group-case-insensitive ()
  "Test that group names are case-insensitive.
This test currently FAILS - RFC 6350 says groups are case-insensitive.

Example: item1.TEL and ITEM1.X-LABEL should be in same group."
  (let* ((input "BEGIN:VCARD
VERSION:4.0
FN:Test
item1.TEL:+1-555-1111
ITEM1.X-LABEL:Office
END:VCARD")
         (card (ecard-parse input))
         (tel-prop (car (ecard-tel card)))
         (tel-group (when tel-prop (ecard-property-group tel-prop)))
         (extended (ecard-extended card)))
    ;; TEL should have group (lowercase or uppercase)
    (should tel-group)
    ;; Find X-LABEL in extended properties
    (let* ((xlabel (seq-find (lambda (prop)
                               (string= (car prop) "X-LABEL"))
                             extended))
           ;; Extended props are alist: (name . (list of props))
           ;; So get first prop with (car (cdr xlabel))
           (xlabel-prop (when xlabel (car (cdr xlabel))))
           (xlabel-group (when xlabel-prop (ecard-property-group xlabel-prop))))
      ;; Both groups should match (case-insensitive)
      (should (string-equal-ignore-case tel-group xlabel-group)))))  ; ← FAILS

;;; Additional Edge Case Tests

(ert-deftest ecard-edge-multiple-version ()
  "Test rejection of multiple VERSION properties.
RFC 6350: VERSION cardinality is exactly 1."
  (let ((input "BEGIN:VCARD
VERSION:4.0
VERSION:4.0
FN:Test
END:VCARD"))
    ;; Should signal validation error for duplicate VERSION
    (should-error (ecard-parse input) :type 'ecard-validation-error)))

(ert-deftest ecard-edge-version-not-second ()
  "Test rejection of VERSION not immediately after BEGIN.
RFC 6350: VERSION MUST come immediately after BEGIN:VCARD."
  (let ((input "BEGIN:VCARD
FN:Test
VERSION:4.0
END:VCARD"))
    ;; Should signal validation error for VERSION position
    (should-error (ecard-parse input) :type 'ecard-validation-error)))

(ert-deftest ecard-edge-empty-fn ()
  "Test rejection of empty FN value.
RFC 6350: FN is required and should have content."
  (let ((input "BEGIN:VCARD
VERSION:4.0
FN:
END:VCARD"))
    ;; Should signal validation error for empty FN
    (should-error (ecard-parse input) :type 'ecard-validation-error)))

(ert-deftest ecard-edge-duplicate-bday ()
  "Test rejection of duplicate BDAY (cardinality *1).
RFC 6350: BDAY cardinality is at most 1."
  (let ((input "BEGIN:VCARD
VERSION:4.0
FN:Test
BDAY:1990-01-01
BDAY:1995-12-31
END:VCARD"))
    ;; Should signal validation error for duplicate single-value property
    (should-error (ecard-parse input) :type 'ecard-validation-error)))

(ert-deftest ecard-edge-duplicate-n ()
  "Test rejection of duplicate N (cardinality *1)."
  (let ((input "BEGIN:VCARD
VERSION:4.0
FN:Test
N:Doe;John;;;
N:Smith;Jane;;;
END:VCARD"))
    (should-error (ecard-parse input) :type 'ecard-validation-error)))

(ert-deftest ecard-security-long-line ()
  "Test handling of extremely long property value (DoS vector).
Should either parse with limit or reject cleanly."
  (let* ((long-value (make-string 100000 ?A))  ; 100KB of A's
         (input (concat "BEGIN:VCARD\nVERSION:4.0\nFN:Test\nNOTE:"
                       long-value
                       "\nEND:VCARD")))
    ;; Should either parse with limit or reject cleanly
    ;; Currently no limit - potential DoS
    (condition-case err
        (let ((card (ecard-parse input)))
          ;; If parsed, value should be truncated to reasonable limit
          (should (< (length (ecard-get-property-value card :note)) 10000)))
      (error
       ;; Or should signal clear error about line length
       (should (string-match-p "too long\\|limit\\|size" (error-message-string err)))))))

(ert-deftest ecard-security-many-properties ()
  "Test handling of excessive property count (DoS vector).
Should reject or limit property count."
  (let* ((emails (mapconcat (lambda (i)
                             (format "EMAIL:test%d@example.com" i))
                           (number-sequence 1 10000)
                           "\n"))
         (input (concat "BEGIN:VCARD\nVERSION:4.0\nFN:Test\n"
                       emails
                       "\nEND:VCARD")))
    ;; Should reject or limit property count
    (condition-case err
        (let ((card (ecard-parse input)))
          ;; If parsed, should have reasonable limit on email count
          (should (< (length (ecard-email card)) 1000)))
      (error
       ;; Or should signal clear error about property count
       (should (string-match-p "too many\\|limit" (error-message-string err)))))))

(ert-deftest ecard-edge-all-escapes-in-one-value ()
  "Test all escape sequences in single value.
RFC 6350 escape sequences: \\n \\N \\, \\; \\\\"
  (let* ((input "BEGIN:VCARD
VERSION:4.0
FN:Test
NOTE:Comma\\, semicolon\\; backslash\\\\ newline\\n capital-N\\N
END:VCARD")
         (card (ecard-parse input))
         (note (ecard-get-property-value card :note)))
    ;; Expected: "Comma, semicolon; backslash\ newline\n capital-N\n"
    (should (string-match-p "Comma," note))
    (should (string-match-p "semicolon;" note))
    (should (string-match-p "backslash\\\\" note))
    (should (string-match-p "newline\n" note))
    (should (string-match-p "capital-N\n" note))))  ; ← FAILS

(ert-deftest ecard-edge-double-backslash ()
  "Test double backslash (escaped backslash)."
  (let* ((input "BEGIN:VCARD
VERSION:4.0
FN:Test
NOTE:Path is C:\\\\Users\\\\John
END:VCARD")
         (card (ecard-parse input))
         (note (ecard-get-property-value card :note)))
    (should (equal note "Path is C:\\Users\\John"))))

(ert-deftest ecard-edge-backslash-at-end ()
  "Test backslash at end of value."
  (let* ((input "BEGIN:VCARD
VERSION:4.0
FN:Test
NOTE:Ends with backslash\\\\
END:VCARD")
         (card (ecard-parse input))
         (note (ecard-get-property-value card :note)))
    (should (equal note "Ends with backslash\\"))))

(ert-deftest ecard-interop-crlf-line-endings ()
  "Test Windows CRLF line endings.
RFC 6350 requires CRLF but parsers should accept LF."
  (let* ((input "BEGIN:VCARD\r\nVERSION:4.0\r\nFN:Test\r\nEND:VCARD\r\n")
         (card (ecard-parse input)))
    (should (equal (ecard-get-property-value card :fn) "Test"))))

(ert-deftest ecard-interop-cr-line-endings ()
  "Test old Mac CR line endings."
  (let* ((input "BEGIN:VCARD\rVERSION:4.0\rFN:Test\rEND:VCARD\r")
         (card (ecard-parse input)))
    (should (equal (ecard-get-property-value card :fn) "Test"))))

(provide 'ecard-regression-test)
;;; ecard-regression-test.el ends here
