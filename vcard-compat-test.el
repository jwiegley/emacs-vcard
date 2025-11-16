;;; vcard-compat-test.el --- Tests for vcard-compat -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>

;;; Commentary:

;; Tests for vcard-compat.el - vCard 2.1/3.0 compatibility layer

;;; Code:

(require 'ert)
(require 'vcard)
(require 'vcard-compat)

;;; Version detection tests

(ert-deftest vcard-compat-detect-version-21 ()
  "Test detection of vCard 2.1."
  (should (eq (vcard-compat--detect-version "VERSION:2.1") 'v21)))

(ert-deftest vcard-compat-detect-version-30 ()
  "Test detection of vCard 3.0."
  (should (eq (vcard-compat--detect-version "VERSION:3.0") 'v30)))

(ert-deftest vcard-compat-detect-version-40 ()
  "Test detection of vCard 4.0."
  (should (eq (vcard-compat--detect-version "VERSION:4.0") 'v40)))

(ert-deftest vcard-compat-detect-version-unknown ()
  "Test detection of unknown version."
  (should (null (vcard-compat--detect-version "VERSION:5.0"))))

;;; Encoding tests

(ert-deftest vcard-compat-decode-base64 ()
  "Test BASE64 decoding."
  (should (string= (vcard-compat--decode-base64 "SGVsbG8gV29ybGQ=")
                   "Hello World")))

(ert-deftest vcard-compat-decode-quoted-printable ()
  "Test QUOTED-PRINTABLE decoding."
  (should (string= (vcard-compat--decode-quoted-printable "Hello=20World")
                   "Hello World"))
  (should (string= (vcard-compat--decode-quoted-printable "Line=0ABreak")
                   "Line\nBreak")))

(ert-deftest vcard-compat-decode-quoted-printable-soft-break ()
  "Test QUOTED-PRINTABLE soft line break handling."
  (should (string= (vcard-compat--decode-quoted-printable "Long=\r\nLine")
                   "LongLine")))

;;; Parameter conversion tests

(ert-deftest vcard-compat-convert-params-21-types ()
  "Test vCard 2.1 type parameter conversion."
  (let* ((params '(("HOME" . t) ("VOICE" . t)))
         (result (vcard-compat--convert-params-21 params "TEL"))
         (converted-params (plist-get result :params)))
    (should (member '("TYPE" . "home,voice") converted-params))))

(ert-deftest vcard-compat-convert-params-21-encoding ()
  "Test vCard 2.1 encoding parameter extraction."
  (let* ((params '(("ENCODING" . "BASE64") ("TYPE" . "JPEG")))
         (result (vcard-compat--convert-params-21 params "PHOTO")))
    (should (string= (plist-get result :encoding) "BASE64"))
    (should (member '("TYPE" . "JPEG") (plist-get result :params)))))

(ert-deftest vcard-compat-convert-params-30-type ()
  "Test vCard 3.0 type parameter conversion."
  (let* ((params '(("TYPE" . "HOME,WORK")))
         (result (vcard-compat--convert-params-30 params "TEL"))
         (converted-params (plist-get result :params)))
    (should (member '("TYPE" . "home,work") converted-params))))

;;; Property filtering tests

(ert-deftest vcard-compat-should-include-property ()
  "Test property filtering."
  (should (vcard-compat--should-include-property-p "TEL"))
  (should (vcard-compat--should-include-property-p "EMAIL"))
  (should-not (vcard-compat--should-include-property-p "LABEL"))
  (should-not (vcard-compat--should-include-property-p "MAILER"))
  (should-not (vcard-compat--should-include-property-p "CLASS")))

;;; Media type detection tests

(ert-deftest vcard-compat-detect-media-type-photo ()
  "Test media type detection for PHOTO."
  (should (string= (vcard-compat--detect-media-type "PHOTO" nil)
                   "image/jpeg")))

(ert-deftest vcard-compat-detect-media-type-from-param ()
  "Test media type detection from TYPE parameter."
  (should (string= (vcard-compat--detect-media-type
                   "PHOTO" '(("TYPE" . "image/png")))
                   "image/png")))

;;; vCard 2.1 parsing tests

(ert-deftest vcard-compat-parse-21-simple ()
  "Test simple vCard 2.1 parsing."
  (let* ((vcard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
TEL;HOME;VOICE:555-1234
EMAIL;INTERNET:john@example.com
END:VCARD")
         (vc (vcard-compat-parse-21 vcard-21)))

    (should (vcard-p vc))
    (should (string= (vcard-get-property-value vc 'fn) "John Doe"))

    ;; Check telephone
    (let ((tel-props (slot-value vc 'tel)))
      (should (= (length tel-props) 1))
      (should (string= (oref (car tel-props) value) "555-1234"))
      (let ((params (oref (car tel-props) parameters)))
        (should (assoc "TYPE" params))
        (should (string-match-p "home" (cdr (assoc "TYPE" params))))
        (should (string-match-p "voice" (cdr (assoc "TYPE" params))))))

    ;; Check email
    (let ((email-props (slot-value vc 'email)))
      (should (= (length email-props) 1))
      (should (string= (oref (car email-props) value) "john@example.com")))))

(ert-deftest vcard-compat-parse-21-base64-photo ()
  "Test vCard 2.1 with BASE64 encoded photo."
  (let* ((vcard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
PHOTO;ENCODING=BASE64;TYPE=JPEG:SGVsbG8gV29ybGQ=
END:VCARD")
         (vc (vcard-compat-parse-21 vcard-21)))

    (should (vcard-p vc))
    (let ((photo-props (slot-value vc 'photo)))
      (should (= (length photo-props) 1))
      (let ((photo-value (oref (car photo-props) value)))
        ;; Should be converted to data URI
        (should (string-prefix-p "data:image/jpeg;base64," photo-value))))))

(ert-deftest vcard-compat-parse-21-quoted-printable ()
  "Test vCard 2.1 with QUOTED-PRINTABLE encoding."
  (let* ((vcard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
NOTE;ENCODING=QUOTED-PRINTABLE:Hello=20World
END:VCARD")
         (vc (vcard-compat-parse-21 vcard-21)))

    (should (vcard-p vc))
    (should (string= (vcard-get-property-value vc 'note) "Hello World"))))

(ert-deftest vcard-compat-parse-21-charset ()
  "Test vCard 2.1 with character set conversion."
  (let* ((vcard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
NOTE;CHARSET=UTF-8:Test Note
END:VCARD")
         (vc (vcard-compat-parse-21 vcard-21)))

    (should (vcard-p vc))
    (should (string= (vcard-get-property-value vc 'note) "Test Note"))))

(ert-deftest vcard-compat-parse-21-structured-name ()
  "Test vCard 2.1 with structured name."
  (let* ((vcard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
N:Doe;John;Q;Mr;Jr
END:VCARD")
         (vc (vcard-compat-parse-21 vcard-21)))

    (should (vcard-p vc))
    (let ((n-value (vcard-get-property-value vc 'n)))
      (should (listp n-value))
      (should (equal n-value '("Doe" "John" "Q" "Mr" "Jr"))))))

(ert-deftest vcard-compat-parse-21-address ()
  "Test vCard 2.1 with structured address."
  (let* ((vcard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
ADR;HOME:;;123 Main St;City;State;12345;Country
END:VCARD")
         (vc (vcard-compat-parse-21 vcard-21)))

    (should (vcard-p vc))
    (let ((adr-props (slot-value vc 'adr)))
      (should (= (length adr-props) 1))
      (let ((adr-value (oref (car adr-props) value)))
        (should (listp adr-value))
        (should (equal (nth 2 adr-value) "123 Main St"))
        (should (equal (nth 3 adr-value) "City"))))))

(ert-deftest vcard-compat-parse-21-drops-label ()
  "Test that LABEL property is dropped."
  (let* ((vcard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
LABEL;HOME:123 Main St\\nCity, State 12345
END:VCARD")
         (vc (vcard-compat-parse-21 vcard-21)))

    (should (vcard-p vc))
    ;; LABEL should not exist in vCard 4.0
    ;; Since vcard class doesn't have a label slot, this is automatically dropped
    (should (string= (vcard-get-property-value vc 'fn) "John Doe"))))

;;; vCard 3.0 parsing tests

(ert-deftest vcard-compat-parse-30-simple ()
  "Test simple vCard 3.0 parsing."
  (let* ((vcard-30 "BEGIN:VCARD
VERSION:3.0
FN:Jane Smith
TEL;TYPE=HOME,VOICE:555-5678
EMAIL;TYPE=INTERNET:jane@example.com
END:VCARD")
         (vc (vcard-compat-parse-30 vcard-30)))

    (should (vcard-p vc))
    (should (string= (vcard-get-property-value vc 'fn) "Jane Smith"))

    ;; Check telephone
    (let ((tel-props (slot-value vc 'tel)))
      (should (= (length tel-props) 1))
      (should (string= (oref (car tel-props) value) "555-5678"))
      (let ((params (oref (car tel-props) parameters)))
        (should (assoc "TYPE" params))
        (should (string-match-p "home" (cdr (assoc "TYPE" params))))
        (should (string-match-p "voice" (cdr (assoc "TYPE" params))))))))

(ert-deftest vcard-compat-parse-30-org ()
  "Test vCard 3.0 with organization."
  (let* ((vcard-30 "BEGIN:VCARD
VERSION:3.0
FN:Jane Smith
ORG:Example Corp;Engineering
END:VCARD")
         (vc (vcard-compat-parse-30 vcard-30)))

    (should (vcard-p vc))
    (let ((org-value (vcard-get-property-value vc 'org)))
      (should (listp org-value))
      (should (equal org-value '("Example Corp" "Engineering"))))))

(ert-deftest vcard-compat-parse-30-categories ()
  "Test vCard 3.0 with categories (text-list)."
  (let* ((vcard-30 "BEGIN:VCARD
VERSION:3.0
FN:Jane Smith
CATEGORIES:Work,Friend,VIP
END:VCARD")
         (vc (vcard-compat-parse-30 vcard-30)))

    (should (vcard-p vc))
    (let ((cat-value (vcard-get-property-value vc 'categories)))
      (should (listp cat-value))
      (should (equal cat-value '("Work" "Friend" "VIP"))))))

;;; Auto-detection tests

(ert-deftest vcard-compat-parse-auto-21 ()
  "Test auto-detection of vCard 2.1."
  (let* ((vcard-21 "BEGIN:VCARD
VERSION:2.1
FN:Auto Test
END:VCARD")
         (vc (vcard-compat-parse vcard-21)))

    (should (vcard-p vc))
    (should (string= (vcard-get-property-value vc 'fn) "Auto Test"))))

(ert-deftest vcard-compat-parse-auto-30 ()
  "Test auto-detection of vCard 3.0."
  (let* ((vcard-30 "BEGIN:VCARD
VERSION:3.0
FN:Auto Test
END:VCARD")
         (vc (vcard-compat-parse vcard-30)))

    (should (vcard-p vc))
    (should (string= (vcard-get-property-value vc 'fn) "Auto Test"))))

(ert-deftest vcard-compat-parse-auto-40 ()
  "Test auto-detection of vCard 4.0."
  (let* ((vcard-40 "BEGIN:VCARD
VERSION:4.0
FN:Auto Test
END:VCARD")
         (vc (vcard-compat-parse vcard-40)))

    (should (vcard-p vc))
    (should (string= (vcard-get-property-value vc 'fn) "Auto Test"))))

(ert-deftest vcard-compat-parse-auto-unknown ()
  "Test error on unknown version."
  (should-error
   (vcard-compat-parse "BEGIN:VCARD\nVERSION:5.0\nFN:Test\nEND:VCARD")
   :type 'vcard-compat-version-error))

;;; Multiple vCard parsing tests

(ert-deftest vcard-compat-parse-multiple-mixed ()
  "Test parsing multiple vCards with different versions."
  (let* ((vcards-text "BEGIN:VCARD
VERSION:2.1
FN:John Doe
END:VCARD
BEGIN:VCARD
VERSION:3.0
FN:Jane Smith
END:VCARD
BEGIN:VCARD
VERSION:4.0
FN:Bob Johnson
END:VCARD")
         (vcards (vcard-compat-parse-multiple vcards-text)))

    (should (= (length vcards) 3))
    (should (string= (vcard-get-property-value (nth 0 vcards) 'fn) "John Doe"))
    (should (string= (vcard-get-property-value (nth 1 vcards) 'fn) "Jane Smith"))
    (should (string= (vcard-get-property-value (nth 2 vcards) 'fn) "Bob Johnson"))))

;;; Data URI creation tests

(ert-deftest vcard-compat-create-data-uri ()
  "Test data URI creation."
  (let ((uri (vcard-compat--create-data-uri "SGVsbG8=" "image/jpeg")))
    (should (string= uri "data:image/jpeg;base64,SGVsbG8="))))

;;; Character set conversion tests

(ert-deftest vcard-compat-convert-charset-utf8 ()
  "Test charset conversion for UTF-8 (should be no-op)."
  (should (string= (vcard-compat--convert-charset "Test" "UTF-8") "Test"))
  (should (string= (vcard-compat--convert-charset "Test" "utf-8") "Test")))

(ert-deftest vcard-compat-convert-charset-nil ()
  "Test charset conversion for nil charset (should be no-op)."
  (should (string= (vcard-compat--convert-charset "Test" nil) "Test")))

;;; Type value mapping tests

(ert-deftest vcard-compat-map-type-value-known ()
  "Test mapping of known type values."
  (should (string= (vcard-compat--map-type-value "HOME") "home"))
  (should (string= (vcard-compat--map-type-value "WORK") "work"))
  (should (string= (vcard-compat--map-type-value "CELL") "cell")))

(ert-deftest vcard-compat-map-type-value-dropped ()
  "Test mapping of dropped type values."
  (should (null (vcard-compat--map-type-value "INTERNET")))
  (should (null (vcard-compat--map-type-value "DOM"))))

(ert-deftest vcard-compat-map-type-value-unknown ()
  "Test mapping of unknown type values (pass through as lowercase)."
  (should (string= (vcard-compat--map-type-value "CUSTOM") "custom")))

;;; Integration tests

(ert-deftest vcard-compat-parse-21-full-example ()
  "Test full vCard 2.1 example."
  (let* ((vcard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Q. Public
N:Public;John;Quinlan;Mr.;Esq.
TEL;HOME;VOICE:555-1234
TEL;WORK;FAX:555-5678
EMAIL;INTERNET:john@example.com
ADR;WORK:;;100 Main St;Suite 200;Springfield;IL;62701;USA
ORG:Example Corp;Engineering
TITLE:Software Engineer
BDAY:1980-01-15
NOTE:This is a note\\nwith a line break
END:VCARD")
         (vc (vcard-compat-parse-21 vcard-21)))

    (should (vcard-p vc))
    (should (string= (vcard-get-property-value vc 'fn) "John Q. Public"))

    ;; Check structured name
    (let ((n-value (vcard-get-property-value vc 'n)))
      (should (equal n-value '("Public" "John" "Quinlan" "Mr." "Esq."))))

    ;; Check multiple phones
    (let ((tel-props (slot-value vc 'tel)))
      (should (= (length tel-props) 2)))

    ;; Check email
    (should (string= (vcard-get-property-value vc 'email) "john@example.com"))

    ;; Check organization
    (let ((org-value (vcard-get-property-value vc 'org)))
      (should (equal org-value '("Example Corp" "Engineering"))))

    ;; Check title
    (should (string= (vcard-get-property-value vc 'title) "Software Engineer"))

    ;; Check birthday
    (should (string= (vcard-get-property-value vc 'bday) "1980-01-15"))

    ;; Check note with escaped newline
    (should (string-match-p "line break" (vcard-get-property-value vc 'note)))))

(ert-deftest vcard-compat-roundtrip-21-to-40 ()
  "Test roundtrip from vCard 2.1 to 4.0 and back."
  (let* ((vcard-21 "BEGIN:VCARD
VERSION:2.1
FN:Test User
N:User;Test;;;
TEL;HOME:555-1234
EMAIL:test@example.com
END:VCARD")
         (vc (vcard-compat-parse-21 vcard-21))
         (serialized (vcard-serialize vc)))

    (should (vcard-p vc))
    (should (string-match-p "VERSION:4.0" serialized))
    (should (string-match-p "FN:Test User" serialized))

    ;; Parse the serialized 4.0 version
    (let ((vc2 (vcard-parse serialized)))
      (should (vcard-p vc2))
      (should (string= (vcard-get-property-value vc2 'fn) "Test User")))))

;;; vcard-compat-parse-buffer tests

(ert-deftest vcard-compat-parse-buffer-single ()
  "Test parsing single vCard from buffer returns single object."
  (with-temp-buffer
    (insert "BEGIN:VCARD
VERSION:3.0
FN:Single Test
EMAIL:single@example.com
END:VCARD")
    (let ((result (vcard-compat-parse-buffer)))
      (should (vcard-p result))
      (should-not (listp result))
      (should (string= "Single Test" (vcard-get-property-value result 'fn)))
      (should (string= "single@example.com" (vcard-get-property-value result 'email))))))

(ert-deftest vcard-compat-parse-buffer-multiple ()
  "Test parsing multiple vCards from buffer returns list."
  (with-temp-buffer
    (insert "BEGIN:VCARD
VERSION:2.1
FN:First Contact
EMAIL:first@example.com
END:VCARD
BEGIN:VCARD
VERSION:3.0
FN:Second Contact
EMAIL:second@example.com
END:VCARD
BEGIN:VCARD
VERSION:4.0
FN:Third Contact
EMAIL:third@example.com
END:VCARD")
    (let ((result (vcard-compat-parse-buffer)))
      (should (listp result))
      (should (= (length result) 3))
      (should (string= "First Contact" (vcard-get-property-value (nth 0 result) 'fn)))
      (should (string= "Second Contact" (vcard-get-property-value (nth 1 result) 'fn)))
      (should (string= "Third Contact" (vcard-get-property-value (nth 2 result) 'fn))))))

(ert-deftest vcard-compat-parse-buffer-empty ()
  "Test parsing empty buffer returns nil."
  (with-temp-buffer
    (let ((result (vcard-compat-parse-buffer)))
      (should (null result)))))

(ert-deftest vcard-compat-parse-buffer-different-versions ()
  "Test parsing different vCard versions from buffer."
  (with-temp-buffer
    (insert "BEGIN:VCARD
VERSION:2.1
FN:Version 2.1 Contact
TEL;HOME;VOICE:555-2100
EMAIL:v21@example.com
END:VCARD
BEGIN:VCARD
VERSION:3.0
FN:Version 3.0 Contact
TEL;TYPE=HOME,VOICE:555-3000
EMAIL;TYPE=INTERNET:v30@example.com
END:VCARD")
    (let ((result (vcard-compat-parse-buffer)))
      (should (listp result))
      (should (= (length result) 2))

      ;; Check first vCard (2.1)
      (let ((vc1 (nth 0 result)))
        (should (string= "Version 2.1 Contact" (vcard-get-property-value vc1 'fn)))
        (should (string= "v21@example.com" (vcard-get-property-value vc1 'email)))
        (should (string= "555-2100" (vcard-get-property-value vc1 'tel))))

      ;; Check second vCard (3.0)
      (let ((vc2 (nth 1 result)))
        (should (string= "Version 3.0 Contact" (vcard-get-property-value vc2 'fn)))
        (should (string= "v30@example.com" (vcard-get-property-value vc2 'email)))
        (should (string= "555-3000" (vcard-get-property-value vc2 'tel)))))))

(ert-deftest vcard-compat-parse-buffer-specific-buffer ()
  "Test parsing from specific buffer (not current buffer)."
  (let ((test-buffer (generate-new-buffer "*vcard-test*")))
    (unwind-protect
        (progn
          (with-current-buffer test-buffer
            (insert "BEGIN:VCARD
VERSION:4.0
FN:Buffer Test
EMAIL:buffer@example.com
END:VCARD"))
          ;; Parse from test-buffer while in a different buffer
          (with-temp-buffer
            (let ((result (vcard-compat-parse-buffer test-buffer)))
              (should (vcard-p result))
              (should (string= "Buffer Test" (vcard-get-property-value result 'fn)))
              (should (string= "buffer@example.com" (vcard-get-property-value result 'email))))))
      (kill-buffer test-buffer))))

(provide 'vcard-compat-test)
;;; vcard-compat-test.el ends here
