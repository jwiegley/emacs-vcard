;;; vcard-test.el --- Tests for vcard.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;;; Commentary:

;; ERT tests for vcard.el.
;; Run tests with: M-x ert RET t RET
;; Or from command line:
;;   emacs -batch -l lisp/vcard.el -l lisp/vcard-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'vcard)
(require 'ert)

;;; Test data

(defconst vcard-test-simple
  "BEGIN:VCARD
VERSION:4.0
FN:John Doe
N:Doe;John;Q.;Mr.;Jr.
EMAIL:john.doe@example.com
TEL:+1-555-1234
ORG:Example Corporation
TITLE:Software Engineer
END:VCARD"
  "Simple vCard example.")

(defconst vcard-test-complex
  "BEGIN:VCARD
VERSION:4.0
FN:Jane Smith
N:Smith;Jane;Marie;Dr.;PhD
EMAIL;TYPE=work:jane.smith@corp.example.com
EMAIL;TYPE=home:jane@home.example.com
TEL;TYPE=work,voice:+1-555-5678
TEL;TYPE=home,voice:+1-555-8765
ADR;TYPE=work:;;123 Main St;Springfield;IL;62701;USA
ADR;TYPE=home:;;456 Oak Ave;Chicago;IL;60601;USA
ORG:Research Institute
TITLE:Senior Researcher
ROLE:Team Lead
URL:https://jane.example.com
NOTE:This is a test contact with\\nmultiple lines\\, commas\\; and semicolons.
UID:urn:uuid:12345678-1234-1234-1234-123456789012
BDAY:19800101
GENDER:F
CATEGORIES:researcher,colleague
GEO:geo:37.386013,-122.082932
TZ:-05:00
REV:20250115T120000Z
PRODID:-//Example Corp//vCard 1.0//EN
KIND:individual
END:VCARD"
  "Complex vCard with multiple properties and escaping.")

(defconst vcard-test-folded
  "BEGIN:VCARD
VERSION:4.0
FN:Very Long Name That Will Require Folding When The Line Exceeds Seventy
 Five Octets
NOTE:This is a very long note that contains a lot of text and will need to
  be folded across multiple lines when serialized according to RFC 6350 whi
 ch specifies that lines should be folded at 75 octets using a space or tab
  continuation character.
EMAIL:user@example.com
END:VCARD"
  "vCard with folded lines.")

(defconst vcard-test-extended
  "BEGIN:VCARD
VERSION:4.0
FN:Extended Properties Test
X-CUSTOM:Custom value
X-MANAGER:John Boss
item1.TEL:+1-555-1111
item1.X-LABEL:Main Office
item2.TEL:+1-555-2222
item2.X-LABEL:Branch Office
END:VCARD"
  "vCard with extended properties and groups.")

;;; Tests

(ert-deftest vcard-parse-simple-test ()
  "Test parsing a simple vCard."
  (let ((vc (vcard-parse vcard-test-simple)))
    (should (vcard-p vc))
    (should (string= (vcard-get-property-value vc 'fn) "John Doe"))
    (should (equal (vcard-get-property-value vc 'n)
                   '("Doe" "John" "Q." "Mr." "Jr.")))
    (should (string= (vcard-get-property-value vc 'email)
                     "john.doe@example.com"))
    (should (string= (vcard-get-property-value vc 'tel) "+1-555-1234"))
    (should (string= (vcard-get-property-value vc 'org)
                     "Example Corporation"))))

(ert-deftest vcard-parse-complex-test ()
  "Test parsing a complex vCard with multiple properties."
  (let ((vc (vcard-parse vcard-test-complex)))
    (should (vcard-p vc))

    ;; Check multiple emails
    (let ((emails (vcard-get-property-values vc 'email)))
      (should (= (length emails) 2))
      (should (member "jane.smith@corp.example.com" emails))
      (should (member "jane@home.example.com" emails)))

    ;; Check email parameters
    (let ((email-props (oref vc email)))
      (should (= (length email-props) 2))
      (should (equal (oref (car email-props) parameters)
                     '(("TYPE" . "work")))))

    ;; Check multiple telephones
    (let ((tels (vcard-get-property-values vc 'tel)))
      (should (= (length tels) 2)))

    ;; Check structured addresses
    (let ((addrs (vcard-get-property-values vc 'adr)))
      (should (= (length addrs) 2))
      (should (equal (nth 2 (car addrs)) "123 Main St")))

    ;; Check escaped values in note
    (let ((note (vcard-get-property-value vc 'note)))
      (should (string-match-p "multiple lines" note))
      (should (string-match-p "\n" note))
      (should (string-match-p "," note))
      (should (string-match-p ";" note)))))

(ert-deftest vcard-parse-folded-test ()
  "Test parsing a vCard with folded lines."
  (let ((vc (vcard-parse vcard-test-folded)))
    (should (vcard-p vc))

    ;; Check that folded FN is properly unfolded
    ;; Note: Per RFC 6350, CRLF+space is removed entirely, so "Seventy\n Five"
    ;; becomes "SeventyFive" (no space). This is correct per the spec.
    (let ((fn (vcard-get-property-value vc 'fn)))
      (should-not (string-match-p "\n" fn))
      (should (string-match-p "SeventyFive Octets" fn)))

    ;; Check that folded NOTE is properly unfolded
    (let ((note (vcard-get-property-value vc 'note)))
      (should (string-match-p "RFC 6350" note))
      (should (string-match-p "continuation character" note)))))

(ert-deftest vcard-parse-extended-test ()
  "Test parsing a vCard with extended properties and groups."
  (let ((vc (vcard-parse vcard-test-extended)))
    (should (vcard-p vc))

    ;; Check extended properties
    (let ((extended (oref vc extended)))
      (should (assoc "X-CUSTOM" extended))
      (should (assoc "X-MANAGER" extended))
      (should (assoc "X-LABEL" extended)))

    ;; Check X-CUSTOM value
    (let* ((x-custom-props (cdr (assoc "X-CUSTOM" (oref vc extended))))
           (x-custom-value (oref (car x-custom-props) value)))
      (should (string= x-custom-value "Custom value")))

    ;; Check grouped properties
    (let ((tel-props (oref vc tel)))
      (should (= (length tel-props) 2))
      (should (string= (oref (car tel-props) group) "item1"))
      (should (string= (oref (cadr tel-props) group) "item2")))))

(ert-deftest vcard-serialize-simple-test ()
  "Test serializing a simple vCard."
  (let* ((vc (vcard-parse vcard-test-simple))
         (serialized (vcard-serialize vc)))
    (should (string-match-p "BEGIN:VCARD" serialized))
    (should (string-match-p "VERSION:4.0" serialized))
    (should (string-match-p "FN:John Doe" serialized))
    (should (string-match-p "N:Doe;John;Q\\.;Mr\\.;Jr\\." serialized))
    (should (string-match-p "EMAIL:john\\.doe@example\\.com" serialized))
    (should (string-match-p "END:VCARD" serialized))))

(ert-deftest vcard-serialize-escaping-test ()
  "Test that serialization properly escapes special characters."
  (let* ((vc (vcard-parse vcard-test-complex))
         (serialized (vcard-serialize vc)))
    ;; Check that newlines are escaped as \n
    (should (string-match-p "\\\\n" serialized))
    ;; Check that commas are escaped as \,
    (should (string-match-p "\\\\," serialized))
    ;; Check that semicolons are escaped as \;
    (should (string-match-p "\\\\;" serialized))))

(ert-deftest vcard-round-trip-test ()
  "Test that parse -> serialize -> parse produces same result."
  (let* ((vc1 (vcard-parse vcard-test-complex))
         (serialized (vcard-serialize vc1))
         (vc2 (vcard-parse serialized)))

    ;; Compare key properties
    (should (string= (vcard-get-property-value vc1 'fn)
                     (vcard-get-property-value vc2 'fn)))
    (should (equal (vcard-get-property-value vc1 'n)
                   (vcard-get-property-value vc2 'n)))
    (should (equal (vcard-get-property-values vc1 'email)
                   (vcard-get-property-values vc2 'email)))
    (should (equal (vcard-get-property-value vc1 'note)
                   (vcard-get-property-value vc2 'note)))))

(ert-deftest vcard-create-test ()
  "Test creating a vCard programmatically."
  (let ((vc (vcard-create
             :fn "Alice Johnson"
             :n '("Johnson" "Alice" "Marie" "" "")
             :email '("alice@work.com" "alice@home.com")
             :tel "+1-555-9999"
             :org "Tech Startup Inc"
             :title "CTO"
             :url "https://alice.example.com"
             :note "Created programmatically"
             :uid "urn:uuid:test-12345"
             :bday "19900515"
             :gender "F")))

    (should (vcard-p vc))
    (should (string= (vcard-get-property-value vc 'fn) "Alice Johnson"))
    (should (equal (vcard-get-property-value vc 'n)
                   '("Johnson" "Alice" "Marie" "" "")))
    (should (= (length (vcard-get-property-values vc 'email)) 2))
    (should (string= (vcard-get-property-value vc 'org) "Tech Startup Inc"))

    ;; Test serialization
    (let ((serialized (vcard-serialize vc)))
      (should (string-match-p "FN:Alice Johnson" serialized))
      (should (string-match-p "ORG:Tech Startup Inc" serialized)))))

(ert-deftest vcard-property-access-test ()
  "Test property access helper functions."
  (let ((vc (vcard-create :fn "Test User")))

    ;; Test vcard-add-property
    (vcard-add-property vc 'email "test1@example.com"
                        '(("TYPE" . "work")))
    (vcard-add-property vc 'email "test2@example.com"
                        '(("TYPE" . "home")))

    (let ((emails (vcard-get-property-values vc 'email)))
      (should (= (length emails) 2))
      (should (member "test1@example.com" emails))
      (should (member "test2@example.com" emails)))

    ;; Test vcard-set-property
    (vcard-set-property vc 'tel "+1-555-0000")
    (should (string= (vcard-get-property-value vc 'tel) "+1-555-0000"))

    ;; Set again should replace
    (vcard-set-property vc 'tel "+1-555-1111")
    (should (string= (vcard-get-property-value vc 'tel) "+1-555-1111"))
    (should (= (length (vcard-get-property-values vc 'tel)) 1))))

(ert-deftest vcard-validation-missing-version-test ()
  "Test that missing VERSION property is detected."
  (should-error (vcard-parse "BEGIN:VCARD\nFN:Test\nEND:VCARD")
                :type 'vcard-validation-error))

(ert-deftest vcard-validation-missing-fn-test ()
  "Test that missing FN property is detected."
  (should-error (vcard-parse "BEGIN:VCARD\nVERSION:4.0\nEND:VCARD")
                :type 'vcard-validation-error))

(ert-deftest vcard-validation-wrong-version-test ()
  "Test that wrong VERSION is rejected."
  (should-error (vcard-parse "BEGIN:VCARD\nVERSION:3.0\nFN:Test\nEND:VCARD")
                :type 'vcard-validation-error))

(ert-deftest vcard-file-io-test ()
  "Test file reading and writing."
  (let ((temp-file (make-temp-file "vcard-test" nil ".vcf"))
        (vc (vcard-create :fn "File Test User"
                          :email "file@example.com"
                          :tel "+1-555-7777")))
    (unwind-protect
        (progn
          ;; Write to file
          (vcard-write-file vc temp-file)
          (should (file-exists-p temp-file))

          ;; Read from file
          (let ((vc2 (vcard-parse-file temp-file)))
            (should (string= (vcard-get-property-value vc2 'fn)
                             "File Test User"))
            (should (string= (vcard-get-property-value vc2 'email)
                             "file@example.com"))))

      ;; Cleanup
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest vcard-utf8-test ()
  "Test UTF-8 support in vCards."
  (let* ((vc (vcard-create
              :fn "François Müller"
              :email "françois@example.com"
              :note "Test with émojis: 😀 and Japanese: 日本語"))
         (serialized (vcard-serialize vc))
         (vc2 (vcard-parse serialized)))

    (should (string= (vcard-get-property-value vc2 'fn)
                     "François Müller"))
    (should (string= (vcard-get-property-value vc2 'email)
                     "françois@example.com"))
    (should (string-match-p "😀" (vcard-get-property-value vc2 'note)))
    (should (string-match-p "日本語" (vcard-get-property-value vc2 'note)))))

(provide 'vcard-test)
;;; vcard-test.el ends here
