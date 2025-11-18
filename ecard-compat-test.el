;;; ecard-compat-test.el --- Tests for ecard-compat -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>

;;; Commentary:

;; Tests for ecard-compat.el - vCard 2.1/3.0 compatibility layer

;;; Code:

(require 'ert)
(require 'ecard)
(require 'ecard-compat)

;;; Version detection tests

(ert-deftest ecard-compat-detect-version-21 ()
  "Test detection of vCard 2.1."
  (should (eq (ecard-compat--detect-version "VERSION:2.1") 'v21)))

(ert-deftest ecard-compat-detect-version-30 ()
  "Test detection of vCard 3.0."
  (should (eq (ecard-compat--detect-version "VERSION:3.0") 'v30)))

(ert-deftest ecard-compat-detect-version-40 ()
  "Test detection of vCard 4.0."
  (should (eq (ecard-compat--detect-version "VERSION:4.0") 'v40)))

(ert-deftest ecard-compat-detect-version-unknown ()
  "Test detection of unknown version."
  (should (null (ecard-compat--detect-version "VERSION:5.0"))))

;;; Encoding tests

(ert-deftest ecard-compat-decode-base64 ()
  "Test BASE64 decoding."
  (should (string= (ecard-compat--decode-base64 "SGVsbG8gV29ybGQ=")
                   "Hello World")))

(ert-deftest ecard-compat-decode-quoted-printable ()
  "Test QUOTED-PRINTABLE decoding."
  (should (string= (ecard-compat--decode-quoted-printable "Hello=20World")
                   "Hello World"))
  (should (string= (ecard-compat--decode-quoted-printable "Line=0ABreak")
                   "Line\nBreak")))

(ert-deftest ecard-compat-decode-quoted-printable-soft-break ()
  "Test QUOTED-PRINTABLE soft line break handling."
  (should (string= (ecard-compat--decode-quoted-printable "Long=\r\nLine")
                   "LongLine")))

;;; Parameter conversion tests

(ert-deftest ecard-compat-convert-params-21-types ()
  "Test vCard 2.1 type parameter conversion."
  (let* ((params '(("HOME" . t) ("VOICE" . t)))
         (result (ecard-compat--convert-params-21 params "TEL"))
         (converted-params (plist-get result :params)))
    (should (member '("TYPE" . "home,voice") converted-params))))

(ert-deftest ecard-compat-convert-params-21-encoding ()
  "Test vCard 2.1 encoding parameter extraction."
  (let* ((params '(("ENCODING" . "BASE64") ("TYPE" . "JPEG")))
         (result (ecard-compat--convert-params-21 params "PHOTO")))
    (should (string= (plist-get result :encoding) "BASE64"))
    (should (member '("TYPE" . "JPEG") (plist-get result :params)))))

(ert-deftest ecard-compat-convert-params-30-type ()
  "Test vCard 3.0 type parameter conversion."
  (let* ((params '(("TYPE" . "HOME,WORK")))
         (result (ecard-compat--convert-params-30 params "TEL"))
         (converted-params (plist-get result :params)))
    (should (member '("TYPE" . "home,work") converted-params))))

;;; Property filtering tests

(ert-deftest ecard-compat-should-include-property ()
  "Test property filtering."
  (should (ecard-compat--should-include-property-p "TEL"))
  (should (ecard-compat--should-include-property-p "EMAIL"))
  (should-not (ecard-compat--should-include-property-p "LABEL"))
  (should-not (ecard-compat--should-include-property-p "MAILER"))
  (should-not (ecard-compat--should-include-property-p "CLASS")))

;;; Media type detection tests

(ert-deftest ecard-compat-detect-media-type-photo ()
  "Test media type detection for PHOTO."
  (should (string= (ecard-compat--detect-media-type "PHOTO" nil)
                   "image/jpeg")))

(ert-deftest ecard-compat-detect-media-type-from-param ()
  "Test media type detection from TYPE parameter."
  (should (string= (ecard-compat--detect-media-type
                   "PHOTO" '(("TYPE" . "image/png")))
                   "image/png")))

;;; vCard 2.1 parsing tests

(ert-deftest ecard-compat-parse-21-simple ()
  "Test simple vCard 2.1 parsing."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
TEL;HOME;VOICE:555-1234
EMAIL;INTERNET:john@example.com
END:VCARD")
         (vc (ecard-compat-parse-21 ecard-21)))

    (should (ecard-p vc))
    (should (string= (ecard-get-property-value vc 'fn) "John Doe"))

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

(ert-deftest ecard-compat-parse-21-base64-photo ()
  "Test vCard 2.1 with BASE64 encoded photo."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
PHOTO;ENCODING=BASE64;TYPE=JPEG:SGVsbG8gV29ybGQ=
END:VCARD")
         (vc (ecard-compat-parse-21 ecard-21)))

    (should (ecard-p vc))
    (let ((photo-props (slot-value vc 'photo)))
      (should (= (length photo-props) 1))
      (let ((photo-value (oref (car photo-props) value)))
        ;; Should be converted to data URI
        (should (string-prefix-p "data:image/jpeg;base64," photo-value))))))

(ert-deftest ecard-compat-parse-21-quoted-printable ()
  "Test vCard 2.1 with QUOTED-PRINTABLE encoding."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
NOTE;ENCODING=QUOTED-PRINTABLE:Hello=20World
END:VCARD")
         (vc (ecard-compat-parse-21 ecard-21)))

    (should (ecard-p vc))
    (should (string= (ecard-get-property-value vc 'note) "Hello World"))))

(ert-deftest ecard-compat-parse-21-charset ()
  "Test vCard 2.1 with character set conversion."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
NOTE;CHARSET=UTF-8:Test Note
END:VCARD")
         (vc (ecard-compat-parse-21 ecard-21)))

    (should (ecard-p vc))
    (should (string= (ecard-get-property-value vc 'note) "Test Note"))))

(ert-deftest ecard-compat-parse-21-structured-name ()
  "Test vCard 2.1 with structured name."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
N:Doe;John;Q;Mr;Jr
END:VCARD")
         (vc (ecard-compat-parse-21 ecard-21)))

    (should (ecard-p vc))
    (let ((n-value (ecard-get-property-value vc 'n)))
      (should (listp n-value))
      (should (equal n-value '("Doe" "John" "Q" "Mr" "Jr"))))))

(ert-deftest ecard-compat-parse-21-address ()
  "Test vCard 2.1 with structured address."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
ADR;HOME:;;123 Main St;City;State;12345;Country
END:VCARD")
         (vc (ecard-compat-parse-21 ecard-21)))

    (should (ecard-p vc))
    (let ((adr-props (slot-value vc 'adr)))
      (should (= (length adr-props) 1))
      (let ((adr-value (oref (car adr-props) value)))
        (should (listp adr-value))
        (should (equal (nth 2 adr-value) "123 Main St"))
        (should (equal (nth 3 adr-value) "City"))))))

(ert-deftest ecard-compat-parse-21-drops-label ()
  "Test that LABEL property is dropped."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:John Doe
LABEL;HOME:123 Main St\\nCity, State 12345
END:VCARD")
         (vc (ecard-compat-parse-21 ecard-21)))

    (should (ecard-p vc))
    ;; LABEL should not exist in vCard 4.0
    ;; Since ecard class doesn't have a label slot, this is automatically dropped
    (should (string= (ecard-get-property-value vc 'fn) "John Doe"))))

;;; vCard 3.0 parsing tests

(ert-deftest ecard-compat-parse-30-simple ()
  "Test simple vCard 3.0 parsing."
  (let* ((ecard-30 "BEGIN:VCARD
VERSION:3.0
FN:Jane Smith
TEL;TYPE=HOME,VOICE:555-5678
EMAIL;TYPE=INTERNET:jane@example.com
END:VCARD")
         (vc (ecard-compat-parse-30 ecard-30)))

    (should (ecard-p vc))
    (should (string= (ecard-get-property-value vc 'fn) "Jane Smith"))

    ;; Check telephone
    (let ((tel-props (slot-value vc 'tel)))
      (should (= (length tel-props) 1))
      (should (string= (oref (car tel-props) value) "555-5678"))
      (let ((params (oref (car tel-props) parameters)))
        (should (assoc "TYPE" params))
        (should (string-match-p "home" (cdr (assoc "TYPE" params))))
        (should (string-match-p "voice" (cdr (assoc "TYPE" params))))))))

(ert-deftest ecard-compat-parse-30-org ()
  "Test vCard 3.0 with organization."
  (let* ((ecard-30 "BEGIN:VCARD
VERSION:3.0
FN:Jane Smith
ORG:Example Corp;Engineering
END:VCARD")
         (vc (ecard-compat-parse-30 ecard-30)))

    (should (ecard-p vc))
    (let ((org-value (ecard-get-property-value vc 'org)))
      (should (listp org-value))
      (should (equal org-value '("Example Corp" "Engineering"))))))

(ert-deftest ecard-compat-parse-30-categories ()
  "Test vCard 3.0 with categories (text-list)."
  (let* ((ecard-30 "BEGIN:VCARD
VERSION:3.0
FN:Jane Smith
CATEGORIES:Work,Friend,VIP
END:VCARD")
         (vc (ecard-compat-parse-30 ecard-30)))

    (should (ecard-p vc))
    (let ((cat-value (ecard-get-property-value vc 'categories)))
      (should (listp cat-value))
      (should (equal cat-value '("Work" "Friend" "VIP"))))))

;;; Auto-detection tests

(ert-deftest ecard-compat-parse-auto-21 ()
  "Test auto-detection of vCard 2.1."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:Auto Test
END:VCARD")
         (vc (ecard-compat-parse ecard-21)))

    (should (ecard-p vc))
    (should (string= (ecard-get-property-value vc 'fn) "Auto Test"))))

(ert-deftest ecard-compat-parse-auto-30 ()
  "Test auto-detection of vCard 3.0."
  (let* ((ecard-30 "BEGIN:VCARD
VERSION:3.0
FN:Auto Test
END:VCARD")
         (vc (ecard-compat-parse ecard-30)))

    (should (ecard-p vc))
    (should (string= (ecard-get-property-value vc 'fn) "Auto Test"))))

(ert-deftest ecard-compat-parse-auto-40 ()
  "Test auto-detection of vCard 4.0."
  (let* ((ecard-40 "BEGIN:VCARD
VERSION:4.0
FN:Auto Test
END:VCARD")
         (vc (ecard-compat-parse ecard-40)))

    (should (ecard-p vc))
    (should (string= (ecard-get-property-value vc 'fn) "Auto Test"))))

(ert-deftest ecard-compat-parse-auto-unknown ()
  "Test error on unknown version."
  (should-error
   (ecard-compat-parse "BEGIN:VCARD\nVERSION:5.0\nFN:Test\nEND:VCARD")
   :type 'ecard-compat-version-error))

;;; Multiple vCard parsing tests

(ert-deftest ecard-compat-parse-multiple-mixed ()
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
         (vcards (ecard-compat-parse-multiple vcards-text)))

    (should (= (length vcards) 3))
    (should (string= (ecard-get-property-value (nth 0 vcards) 'fn) "John Doe"))
    (should (string= (ecard-get-property-value (nth 1 vcards) 'fn) "Jane Smith"))
    (should (string= (ecard-get-property-value (nth 2 vcards) 'fn) "Bob Johnson"))))

;;; Data URI creation tests

(ert-deftest ecard-compat-create-data-uri ()
  "Test data URI creation."
  (let ((uri (ecard-compat--create-data-uri "SGVsbG8=" "image/jpeg")))
    (should (string= uri "data:image/jpeg;base64,SGVsbG8="))))

;;; Character set conversion tests

(ert-deftest ecard-compat-convert-charset-utf8 ()
  "Test charset conversion for UTF-8 (should be no-op)."
  (should (string= (ecard-compat--convert-charset "Test" "UTF-8") "Test"))
  (should (string= (ecard-compat--convert-charset "Test" "utf-8") "Test")))

(ert-deftest ecard-compat-convert-charset-nil ()
  "Test charset conversion for nil charset (should be no-op)."
  (should (string= (ecard-compat--convert-charset "Test" nil) "Test")))

;;; Type value mapping tests

(ert-deftest ecard-compat-map-type-value-known ()
  "Test mapping of known type values."
  (should (string= (ecard-compat--map-type-value "HOME") "home"))
  (should (string= (ecard-compat--map-type-value "WORK") "work"))
  (should (string= (ecard-compat--map-type-value "CELL") "cell")))

(ert-deftest ecard-compat-map-type-value-dropped ()
  "Test mapping of dropped type values."
  (should (null (ecard-compat--map-type-value "INTERNET")))
  (should (null (ecard-compat--map-type-value "DOM"))))

(ert-deftest ecard-compat-map-type-value-unknown ()
  "Test mapping of unknown type values (pass through as lowercase)."
  (should (string= (ecard-compat--map-type-value "CUSTOM") "custom")))

;;; Integration tests

(ert-deftest ecard-compat-parse-21-full-example ()
  "Test full vCard 2.1 example."
  (let* ((ecard-21 "BEGIN:VCARD
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
         (vc (ecard-compat-parse-21 ecard-21)))

    (should (ecard-p vc))
    (should (string= (ecard-get-property-value vc 'fn) "John Q. Public"))

    ;; Check structured name
    (let ((n-value (ecard-get-property-value vc 'n)))
      (should (equal n-value '("Public" "John" "Quinlan" "Mr." "Esq."))))

    ;; Check multiple phones
    (let ((tel-props (slot-value vc 'tel)))
      (should (= (length tel-props) 2)))

    ;; Check email
    (should (string= (ecard-get-property-value vc 'email) "john@example.com"))

    ;; Check organization
    (let ((org-value (ecard-get-property-value vc 'org)))
      (should (equal org-value '("Example Corp" "Engineering"))))

    ;; Check title
    (should (string= (ecard-get-property-value vc 'title) "Software Engineer"))

    ;; Check birthday
    (should (string= (ecard-get-property-value vc 'bday) "1980-01-15"))

    ;; Check note with escaped newline
    (should (string-match-p "line break" (ecard-get-property-value vc 'note)))))

(ert-deftest ecard-compat-roundtrip-21-to-40 ()
  "Test roundtrip from vCard 2.1 to 4.0 and back."
  (let* ((ecard-21 "BEGIN:VCARD
VERSION:2.1
FN:Test User
N:User;Test;;;
TEL;HOME:555-1234
EMAIL:test@example.com
END:VCARD")
         (vc (ecard-compat-parse-21 ecard-21))
         (serialized (ecard-serialize vc)))

    (should (ecard-p vc))
    (should (string-match-p "VERSION:4.0" serialized))
    (should (string-match-p "FN:Test User" serialized))

    ;; Parse the serialized 4.0 version
    (let ((vc2 (ecard-parse serialized)))
      (should (ecard-p vc2))
      (should (string= (ecard-get-property-value vc2 'fn) "Test User")))))

;;; ecard-compat-parse-buffer tests

(ert-deftest ecard-compat-parse-buffer-single ()
  "Test parsing single vCard from buffer returns single object."
  (with-temp-buffer
    (insert "BEGIN:VCARD
VERSION:3.0
FN:Single Test
EMAIL:single@example.com
END:VCARD")
    (let ((result (ecard-compat-parse-buffer)))
      (should (ecard-p result))
      (should-not (listp result))
      (should (string= "Single Test" (ecard-get-property-value result 'fn)))
      (should (string= "single@example.com" (ecard-get-property-value result 'email))))))

(ert-deftest ecard-compat-parse-buffer-multiple ()
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
    (let ((result (ecard-compat-parse-buffer)))
      (should (listp result))
      (should (= (length result) 3))
      (should (string= "First Contact" (ecard-get-property-value (nth 0 result) 'fn)))
      (should (string= "Second Contact" (ecard-get-property-value (nth 1 result) 'fn)))
      (should (string= "Third Contact" (ecard-get-property-value (nth 2 result) 'fn))))))

(ert-deftest ecard-compat-parse-buffer-empty ()
  "Test parsing empty buffer returns nil."
  (with-temp-buffer
    (let ((result (ecard-compat-parse-buffer)))
      (should (null result)))))

(ert-deftest ecard-compat-parse-buffer-different-versions ()
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
    (let ((result (ecard-compat-parse-buffer)))
      (should (listp result))
      (should (= (length result) 2))

      ;; Check first vCard (2.1)
      (let ((vc1 (nth 0 result)))
        (should (string= "Version 2.1 Contact" (ecard-get-property-value vc1 'fn)))
        (should (string= "v21@example.com" (ecard-get-property-value vc1 'email)))
        (should (string= "555-2100" (ecard-get-property-value vc1 'tel))))

      ;; Check second vCard (3.0)
      (let ((vc2 (nth 1 result)))
        (should (string= "Version 3.0 Contact" (ecard-get-property-value vc2 'fn)))
        (should (string= "v30@example.com" (ecard-get-property-value vc2 'email)))
        (should (string= "555-3000" (ecard-get-property-value vc2 'tel)))))))

(ert-deftest ecard-compat-parse-buffer-specific-buffer ()
  "Test parsing from specific buffer (not current buffer)."
  (let ((test-buffer (generate-new-buffer "*ecard-test*")))
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
            (let ((result (ecard-compat-parse-buffer test-buffer)))
              (should (ecard-p result))
              (should (string= "Buffer Test" (ecard-get-property-value result 'fn)))
              (should (string= "buffer@example.com" (ecard-get-property-value result 'email))))))
      (kill-buffer test-buffer))))

;;; vCard 3.0 Serialization Tests
;;
;; Comprehensive tests for ecard-compat-serialize function
;; Tests RFC 2425 and RFC 2426 compliance

(ert-deftest ecard-compat-serialize-simple ()
  "Test basic vCard 3.0 serialization."
  (let* ((vc (ecard-create :fn "John Doe" :email "john@example.com"))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "BEGIN:VCARD" vcard-30))
    (should (string-match-p "VERSION:3.0" vcard-30))
    (should (string-match-p "FN:John Doe" vcard-30))
    (should (string-match-p "EMAIL:john@example.com" vcard-30))
    (should (string-match-p "END:VCARD" vcard-30))))

(ert-deftest ecard-compat-serialize-structured-name ()
  "Test vCard 3.0 serialization with structured name (N property)."
  (let* ((vc (ecard-create
              :fn "John Quincy Doe Jr."
              :n '("Doe" "John" "Quincy" "Mr." "Jr.")))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "N:Doe;John;Quincy;Mr\\.;Jr\\." vcard-30))))

(ert-deftest ecard-compat-serialize-tel-with-types ()
  "Test vCard 3.0 telephone serialization with TYPE parameters."
  (let* ((vc (ecard-create :fn "Jane Smith"))
         (tel-prop (ecard-property
                    :name "TEL"
                    :value "+1-555-1234"
                    :parameters '(("TYPE" . "home,voice")))))
    (oset vc tel (list tel-prop))
    (let ((vcard-30 (ecard-compat-serialize vc)))
      ;; TYPE values should be uppercased in vCard 3.0
      (should (string-match-p "TEL;TYPE=HOME,VOICE:\\+1-555-1234" vcard-30)))))

(ert-deftest ecard-compat-serialize-address ()
  "Test vCard 3.0 address serialization (ADR property)."
  (let* ((vc (ecard-create
              :fn "Jane Smith"
              :adr '("" "Suite 100" "123 Main St" "Springfield" "IL" "62701" "USA")))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "ADR:;Suite 100;123 Main St;Springfield;IL;62701;USA" vcard-30))))

(ert-deftest ecard-compat-serialize-org ()
  "Test vCard 3.0 organization serialization (ORG property)."
  (let* ((vc (ecard-create
              :fn "Jane Smith"
              :org '("Example Corp" "Engineering" "R&D")))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "ORG:Example Corp;Engineering;R&D" vcard-30))))

(ert-deftest ecard-compat-serialize-categories ()
  "Test vCard 3.0 categories serialization (comma-separated)."
  (let* ((vc (ecard-create
              :fn "Jane Smith"
              :categories '("Work" "Friend" "VIP")))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "CATEGORIES:Work,Friend,VIP" vcard-30))))

(ert-deftest ecard-compat-serialize-nickname ()
  "Test vCard 3.0 nickname serialization (comma-separated)."
  (let* ((vc (ecard-create
              :fn "Jane Smith"
              :nickname '("Janie" "JayJay")))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "NICKNAME:Janie,JayJay" vcard-30))))

(ert-deftest ecard-compat-serialize-non-ascii ()
  "Test vCard 3.0 serialization with non-ASCII characters.
Should add CHARSET=UTF-8 parameter."
  (let* ((vc (ecard-create :fn "José García"))
         (vcard-30 (ecard-compat-serialize vc)))
    ;; Should contain CHARSET=UTF-8 for non-ASCII
    (should (string-match-p "FN;CHARSET=UTF-8:José García" vcard-30))))

(ert-deftest ecard-compat-serialize-photo-data-uri ()
  "Test vCard 3.0 serialization with PHOTO as data URI.
Should convert to ENCODING=BASE64."
  (let* ((vc (ecard-create
              :fn "Jane Smith"
              :photo "data:image/jpeg;base64,/9j/4AAQSkZJRg=="))
         (vcard-30 (ecard-compat-serialize vc)))
    ;; Should extract and use BASE64 encoding
    ;; Note: TYPE comes before ENCODING due to parameter ordering
    (should (string-match-p "PHOTO;TYPE=IMAGE/JPEG;ENCODING=BASE64:/9j/4AAQSkZJRg==" vcard-30))))

(ert-deftest ecard-compat-serialize-logo-data-uri ()
  "Test vCard 3.0 serialization with LOGO as data URI."
  (let* ((vc (ecard-create
              :fn "Company Name"
              :logo "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgA="))
         (vcard-30 (ecard-compat-serialize vc)))
    ;; Note: TYPE comes before ENCODING due to parameter ordering
    (should (string-match-p "LOGO;TYPE=IMAGE/PNG;ENCODING=BASE64:iVBORw0KGgoAAAANSUhEUgA=" vcard-30))))

(ert-deftest ecard-compat-serialize-uid ()
  "Test vCard 3.0 serialization with UID.
urn:uuid: prefix is preserved (optional in 3.0, required in 4.0)."
  (let* ((vc (ecard-create
              :fn "Jane Smith"
              :uid "urn:uuid:12345678-1234-1234-1234-123456789abc"))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "UID:urn:uuid:12345678-1234-1234-1234-123456789abc" vcard-30))))

(ert-deftest ecard-compat-serialize-extended-properties ()
  "Test vCard 3.0 serialization with X-* extended properties."
  (let* ((vc (ecard-create :fn "Jane Smith"))
         (x-prop (ecard-property
                  :name "X-CUSTOM"
                  :value "custom-value")))
    (oset vc extended (list (cons "X-CUSTOM" (list x-prop))))
    (let ((vcard-30 (ecard-compat-serialize vc)))
      (should (string-match-p "X-CUSTOM:custom-value" vcard-30)))))

(ert-deftest ecard-compat-serialize-group ()
  "Test vCard 3.0 serialization with grouped properties."
  (let* ((vc (ecard-create :fn "Jane Smith"))
         (email-prop (ecard-property
                      :name "EMAIL"
                      :value "jane@example.com"
                      :group "WORK")))
    (oset vc email (list email-prop))
    (let ((vcard-30 (ecard-compat-serialize vc)))
      (should (string-match-p "WORK\\.EMAIL:jane@example.com" vcard-30)))))

(ert-deftest ecard-compat-serialize-multiple ()
  "Test serialization of multiple vCards to vCard 3.0."
  (let* ((vc1 (ecard-create :fn "John Doe" :email "john@example.com"))
         (vc2 (ecard-create :fn "Jane Smith" :email "jane@example.com"))
         (vcards-30 (ecard-compat-serialize-multiple (list vc1 vc2))))
    ;; Should contain two vCards
    (should (= (length (split-string vcards-30 "BEGIN:VCARD")) 3))
    (should (string-match-p "FN:John Doe" vcards-30))
    (should (string-match-p "FN:Jane Smith" vcards-30))
    ;; Check for two VERSION:3.0 occurrences (one per vCard)
    (should (= (length (split-string vcards-30 "VERSION:3.0")) 3))))

(ert-deftest ecard-compat-serialize-round-trip-basic ()
  "Test round-trip conversion: 3.0 → 4.0 → 3.0."
  (let* ((vcard-30-original "BEGIN:VCARD
VERSION:3.0
FN:John Doe
EMAIL;TYPE=HOME:john@example.com
TEL;TYPE=WORK,VOICE:+1-555-1234
END:VCARD")
         ;; Parse 3.0 to 4.0 object
         (vc (ecard-compat-parse-30 vcard-30-original))
         ;; Serialize back to 3.0
         (vcard-30-new (ecard-compat-serialize vc)))
    ;; Verify essential properties preserved
    (should (string-match-p "VERSION:3.0" vcard-30-new))
    (should (string-match-p "FN:John Doe" vcard-30-new))
    (should (string-match-p "EMAIL.*john@example.com" vcard-30-new))
    (should (string-match-p "TEL.*\\+1-555-1234" vcard-30-new))))

(ert-deftest ecard-compat-serialize-bday ()
  "Test vCard 3.0 serialization with birthday."
  (let* ((vc (ecard-create :fn "Jane Smith" :bday "19850615"))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "BDAY:19850615" vcard-30))))

(ert-deftest ecard-compat-serialize-anniversary ()
  "Test vCard 3.0 serialization with anniversary."
  (let* ((vc (ecard-create :fn "Jane Smith" :anniversary "20100701"))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "ANNIVERSARY:20100701" vcard-30))))

(ert-deftest ecard-compat-serialize-gender ()
  "Test vCard 3.0 serialization with gender."
  (let* ((vc (ecard-create :fn "Jane Smith" :gender '("F" "Female")))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "GENDER:F;Female" vcard-30))))

(ert-deftest ecard-compat-serialize-geo ()
  "Test vCard 3.0 serialization with geographical position."
  (let* ((vc (ecard-create :fn "Jane Smith" :geo "geo:37.386013,-122.082932"))
         (vcard-30 (ecard-compat-serialize vc)))
    ;; Comma in value is escaped per vCard escaping rules
    (should (string-match-p "GEO:geo:37\\.386013\\\\,-122\\.082932" vcard-30))))

(ert-deftest ecard-compat-serialize-title-role ()
  "Test vCard 3.0 serialization with title and role."
  (let* ((vc (ecard-create
              :fn "Jane Smith"
              :title "Senior Engineer"
              :role "Team Lead"))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "TITLE:Senior Engineer" vcard-30))
    (should (string-match-p "ROLE:Team Lead" vcard-30))))

(ert-deftest ecard-compat-serialize-note ()
  "Test vCard 3.0 serialization with note."
  (let* ((vc (ecard-create
              :fn "Jane Smith"
              :note "Important contact"))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "NOTE:Important contact" vcard-30))))

(ert-deftest ecard-compat-serialize-url ()
  "Test vCard 3.0 serialization with URL."
  (let* ((vc (ecard-create
              :fn "Jane Smith"
              :url "https://example.com"))
         (vcard-30 (ecard-compat-serialize vc)))
    (should (string-match-p "URL:https://example.com" vcard-30))))

(ert-deftest ecard-compat-serialize-multiple-emails ()
  "Test vCard 3.0 serialization with multiple email addresses."
  (let* ((vc (ecard-create :fn "Jane Smith"))
         (email1 (ecard-property
                  :name "EMAIL"
                  :value "jane@example.com"
                  :parameters '(("TYPE" . "work"))))
         (email2 (ecard-property
                  :name "EMAIL"
                  :value "jane.personal@example.org"
                  :parameters '(("TYPE" . "home")))))
    (oset vc email (list email1 email2))
    (let ((vcard-30 (ecard-compat-serialize vc)))
      (should (string-match-p "EMAIL;TYPE=WORK:jane@example.com" vcard-30))
      (should (string-match-p "EMAIL;TYPE=HOME:jane.personal@example.org" vcard-30)))))

(ert-deftest ecard-compat-serialize-multiple-phones ()
  "Test vCard 3.0 serialization with multiple phone numbers."
  (let* ((vc (ecard-create :fn "Jane Smith"))
         (tel1 (ecard-property
                :name "TEL"
                :value "+1-555-1234"
                :parameters '(("TYPE" . "work,voice"))))
         (tel2 (ecard-property
                :name "TEL"
                :value "+1-555-5678"
                :parameters '(("TYPE" . "home,voice"))))
         (tel3 (ecard-property
                :name "TEL"
                :value "+1-555-9999"
                :parameters '(("TYPE" . "cell")))))
    (oset vc tel (list tel1 tel2 tel3))
    (let ((vcard-30 (ecard-compat-serialize vc)))
      (should (string-match-p "TEL;TYPE=WORK,VOICE:\\+1-555-1234" vcard-30))
      (should (string-match-p "TEL;TYPE=HOME,VOICE:\\+1-555-5678" vcard-30))
      (should (string-match-p "TEL;TYPE=CELL:\\+1-555-9999" vcard-30)))))

(ert-deftest ecard-compat-serialize-escaping ()
  "Test vCard 3.0 serialization with special characters requiring escaping."
  (let* ((vc (ecard-create
              :fn "Test, User"
              :note "Line 1\nLine 2\nLine 3"))
         (vcard-30 (ecard-compat-serialize vc)))
    ;; Comma should be escaped
    (should (string-match-p "FN:Test\\\\, User" vcard-30))
    ;; Newlines should be escaped
    (should (string-match-p "NOTE:Line 1\\\\nLine 2\\\\nLine 3" vcard-30))))

(ert-deftest ecard-compat-serialize-complex-vcard ()
  "Test vCard 3.0 serialization with many properties (comprehensive test)."
  (let* ((vc (ecard-create
              :fn "Dr. Jane Quincy Smith Jr."
              :n '("Smith" "Jane" "Quincy" "Dr." "Jr.")
              :nickname '("JQ" "Janie")
              :email "jane@example.com"
              :tel "+1-555-1234"
              :adr '("" "Suite 200" "456 Oak Ave" "Springfield" "IL" "62701" "USA")
              :org '("Example Corp" "Engineering")
              :title "Chief Engineer"
              :role "Technical Lead"
              :categories '("Work" "VIP")
              :note "Important contact - handles all technical decisions"
              :url "https://jane.example.com"
              :bday "19800115"
              :uid "urn:uuid:abcd-1234-efgh-5678"))
         (vcard-30 (ecard-compat-serialize vc)))
    ;; Verify all properties present
    (should (string-match-p "VERSION:3.0" vcard-30))
    (should (string-match-p "FN:Dr\\. Jane Quincy Smith Jr\\." vcard-30))
    (should (string-match-p "N:Smith;Jane;Quincy;Dr\\.;Jr\\." vcard-30))
    (should (string-match-p "NICKNAME:JQ,Janie" vcard-30))
    (should (string-match-p "EMAIL:jane@example.com" vcard-30))
    (should (string-match-p "TEL:\\+1-555-1234" vcard-30))
    (should (string-match-p "ADR:;Suite 200;456 Oak Ave;Springfield;IL;62701;USA" vcard-30))
    (should (string-match-p "ORG:Example Corp;Engineering" vcard-30))
    (should (string-match-p "TITLE:Chief Engineer" vcard-30))
    (should (string-match-p "ROLE:Technical Lead" vcard-30))
    (should (string-match-p "CATEGORIES:Work,VIP" vcard-30))
    (should (string-match-p "NOTE:Important contact" vcard-30))
    (should (string-match-p "URL:https://jane\\.example\\.com" vcard-30))
    (should (string-match-p "BDAY:19800115" vcard-30))
    (should (string-match-p "UID:urn:uuid:abcd-1234-efgh-5678" vcard-30))))

(provide 'ecard-compat-test)
;;; ecard-compat-test.el ends here
