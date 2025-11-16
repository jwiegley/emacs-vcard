;;; vcard-compat-examples.el --- Usage examples for vcard-compat -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;;; Commentary:

;; Practical examples demonstrating vcard-compat.el usage

;;; Code:

(require 'vcard-compat)

;;; Example 1: Parse legacy vCard 2.1

(defun vcard-compat-example-parse-21 ()
  "Example: Parse vCard 2.1 with encoding."
  (let* ((vcard-21-text "BEGIN:VCARD
VERSION:2.1
FN:John Q. Public
N:Public;John;Quinlan;Mr.;Esq.
TEL;HOME;VOICE:+1-555-1234
TEL;WORK;FAX:+1-555-5678
EMAIL;INTERNET:john.public@example.com
ADR;WORK:;;100 Main Street;Suite 200;Springfield;IL;62701;USA
ORG:Example Corporation;Engineering Division
TITLE:Senior Software Engineer
BDAY:1980-01-15
NOTE;ENCODING=QUOTED-PRINTABLE:Important contact.=0ACall before 5pm.
PHOTO;ENCODING=BASE64;TYPE=JPEG:
 /9j/4AAQSkZJRgABAQEAYABgAAD/2wBDAAgGBgcGBQgHBwcJCQgKDBQNDAsLDBkSEw8U
 HRofHh0aHBwgJC4nICIsIxwcKDcpLDAxNDQ0Hyc5PTgyPC4zNDL/wAALCAABAAEBAREA
 /8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQA
 AAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJico
 KSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKT
 lJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo
 6erx8vP09fb3+Pn6/9oACAEBAAA/AOg/4Q/yH/oJ/r0o/wCEP8h/6Cf69KKKKKKKK//Z
END:VCARD")
         (contact (vcard-compat-parse-21 vcard-21-text)))

    ;; Display parsed information
    (message "=== Parsed vCard 2.1 ===")
    (message "Name: %s" (vcard-get-property-value contact 'fn))

    ;; Access structured name
    (let ((n-value (vcard-get-property-value contact 'n)))
      (message "Structured name: %s" (mapconcat #'identity n-value " ")))

    ;; Show all phone numbers with their types
    (let ((tel-props (slot-value contact 'tel)))
      (message "Phones:")
      (dolist (tel tel-props)
        (let ((number (oref tel value))
              (params (oref tel parameters)))
          (message "  %s (%s)" number
                   (or (cdr (assoc "TYPE" params)) "unspecified")))))

    ;; Show decoded note (QUOTED-PRINTABLE was decoded)
    (message "Note: %s" (vcard-get-property-value contact 'note))

    ;; Show photo as data URI
    (let ((photo (vcard-get-property-value contact 'photo)))
      (message "Photo: %s..." (substring photo 0 50)))

    ;; Serialize to vCard 4.0
    (message "\n=== Converted to vCard 4.0 ===")
    (message "%s" (vcard-serialize contact))

    contact))

;;; Example 2: Parse vCard 3.0 with categories

(defun vcard-compat-example-parse-30 ()
  "Example: Parse vCard 3.0 with text lists."
  (let* ((vcard-30-text "BEGIN:VCARD
VERSION:3.0
FN:Jane Smith
N:Smith;Jane;Marie;;
TEL;TYPE=HOME,VOICE:+1-555-9876
TEL;TYPE=WORK,CELL:+1-555-5432
EMAIL;TYPE=INTERNET,PREF:jane@example.com
ORG:Tech Innovations Inc.;Research;AI Division
CATEGORIES:Colleague,AI Expert,Speaker
NICKNAME:Janey,JMS
URL:https://janesmith.example.com
NOTE:Met at AI conference 2024.\\nExpert in machine learning.
END:VCARD")
         (contact (vcard-compat-parse-30 vcard-30-text)))

    (message "=== Parsed vCard 3.0 ===")
    (message "Name: %s" (vcard-get-property-value contact 'fn))

    ;; Show organization structure
    (let ((org-value (vcard-get-property-value contact 'org)))
      (message "Organization: %s" (car org-value))
      (message "Department: %s" (mapconcat #'identity (cdr org-value) " > ")))

    ;; Show categories (parsed from comma-separated list)
    (let ((categories (vcard-get-property-value contact 'categories)))
      (message "Categories: %s" (mapconcat #'identity categories ", ")))

    ;; Show nicknames (parsed from comma-separated list)
    (let ((nicknames (vcard-get-property-value contact 'nickname)))
      (message "Nicknames: %s" (mapconcat #'identity nicknames ", ")))

    ;; Show note with escaped newlines
    (message "Note:\n%s" (vcard-get-property-value contact 'note))

    contact))

;;; Example 3: Auto-detect version and parse

(defun vcard-compat-example-auto-detect ()
  "Example: Auto-detect vCard version."
  (let ((vcard-texts
         '(("vCard 2.1" . "BEGIN:VCARD\nVERSION:2.1\nFN:Person One\nTEL;HOME:555-0001\nEND:VCARD")
           ("vCard 3.0" . "BEGIN:VCARD\nVERSION:3.0\nFN:Person Two\nTEL;TYPE=HOME:555-0002\nEND:VCARD")
           ("vCard 4.0" . "BEGIN:VCARD\nVERSION:4.0\nFN:Person Three\nTEL;TYPE=home:555-0003\nEND:VCARD"))))

    (message "=== Auto-detecting vCard versions ===")
    (dolist (entry vcard-texts)
      (let* ((label (car entry))
             (text (cdr entry))
             (version (vcard-compat--detect-version text))
             (contact (vcard-compat-parse text)))

        (message "\n%s (detected: %s)" label version)
        (message "  Name: %s" (vcard-get-property-value contact 'fn))
        (message "  Phone: %s" (vcard-get-property-value contact 'tel))))))

;;; Example 4: Parse multiple vCards from file

(defun vcard-compat-example-parse-multiple (filename)
  "Example: Parse multiple vCards from FILENAME."
  (interactive "fvCard file: ")

  (condition-case err
      (let ((contacts (vcard-compat-parse-file filename)))

        ;; Handle both single and multiple contacts
        (setq contacts (if (listp contacts) contacts (list contacts)))

        (message "=== Parsed %d contact(s) from %s ===" (length contacts) filename)

        (dolist (contact contacts)
          (let ((fn (vcard-get-property-value contact 'fn))
                (emails (vcard-get-property-values contact 'email))
                (phones (vcard-get-property-values contact 'tel)))

            (message "\nContact: %s" fn)
            (when emails
              (message "  Emails: %s" (mapconcat #'identity emails ", ")))
            (when phones
              (message "  Phones: %d number(s)" (length phones)))))

        contacts)

    (error
     (message "Error parsing file: %s" (error-message-string err))
     nil)))

;;; Example 5: Convert legacy vCards to vCard 4.0 files

(defun vcard-compat-example-batch-convert (input-dir output-dir)
  "Example: Convert all vCards in INPUT-DIR to vCard 4.0 in OUTPUT-DIR."
  (interactive "DInput directory: \nDOutput directory: ")

  (unless (file-directory-p output-dir)
    (make-directory output-dir t))

  (let ((vcf-files (directory-files input-dir t "\\.vcf\\'" t))
        (converted 0)
        (errors 0))

    (dolist (file vcf-files)
      (condition-case err
          (let* ((contacts (vcard-compat-parse-file file))
                 (contacts-list (if (listp contacts) contacts (list contacts))))

            ;; Write each contact as separate vCard 4.0 file
            (dolist (contact contacts-list)
              (let* ((fn (vcard-get-property-value contact 'fn))
                     (safe-name (replace-regexp-in-string "[^a-zA-Z0-9-]" "_" fn))
                     (output-file (expand-file-name
                                  (format "%s.vcf" safe-name)
                                  output-dir)))

                (vcard-write-file contact output-file)
                (setq converted (1+ converted)))))

        (error
         (message "Error converting %s: %s" file (error-message-string err))
         (setq errors (1+ errors)))))

    (message "Conversion complete: %d contacts, %d errors" converted errors)))

;;; Example 6: Working with encoded data

(defun vcard-compat-example-encoded-photo ()
  "Example: Parse vCard with BASE64-encoded photo."
  (let* ((vcard-text "BEGIN:VCARD
VERSION:2.1
FN:Photo Contact
PHOTO;ENCODING=BASE64;TYPE=JPEG:
 /9j/4AAQSkZJRgABAQEAYABgAAD/2wBDAAgGBgcGBQgHBwcJCQgKDBQNDAsLDBkSEw8U
 HRofHh0aHBwgJC4nICIsIxwcKDcpLDAxNDQ0Hyc5PTgyPC4zNDL/wAALCAABAAEBAREA
 /8QAHwAAAQUBAQEBAQEAAAAAAAAAAAECAwQFBgcICQoL/8QAtRAAAgEDAwIEAwUFBAQA
 AAF9AQIDAAQRBRIhMUEGE1FhByJxFDKBkaEII0KxwRVS0fAkM2JyggkKFhcYGRolJico
 KSo0NTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqDhIWGh4iJipKT
 lJWWl5iZmqKjpKWmp6ipqrKztLW2t7i5usLDxMXGx8jJytLT1NXW19jZ2uHi4+Tl5ufo
 6erx8vP09fb3+Pn6/9oACAEBAAA/AOg/4Q/yH/oJ/r0o/wCEP8h/6Cf69KKKKKKKK//Z
END:VCARD")
         (contact (vcard-compat-parse vcard-text)))

    (message "=== Photo handling ===")

    ;; Photo is automatically converted to data URI
    (let ((photo-uri (vcard-get-property-value contact 'photo)))
      (message "Photo data URI (first 100 chars):\n%s..."
               (substring photo-uri 0 (min 100 (length photo-uri))))

      ;; Check if it's a valid data URI
      (if (string-prefix-p "data:image/jpeg;base64," photo-uri)
          (message "✓ Photo successfully converted to data URI")
        (message "✗ Photo conversion failed")))

    contact))

;;; Example 7: Error handling

(defun vcard-compat-example-error-handling ()
  "Example: Demonstrate error handling."
  (message "=== Error Handling Examples ===\n")

  ;; Example 1: Unknown version
  (message "1. Unknown version:")
  (condition-case err
      (vcard-compat-parse "BEGIN:VCARD\nVERSION:5.0\nFN:Test\nEND:VCARD")
    (vcard-compat-version-error
     (message "   Caught version error: %s" (error-message-string err))))

  ;; Example 2: Missing FN (validation error from vcard.el)
  (message "\n2. Missing required FN property:")
  (condition-case err
      (vcard-compat-parse "BEGIN:VCARD\nVERSION:2.1\nEND:VCARD")
    (vcard-validation-error
     (message "   Caught validation error: %s" (error-message-string err))))

  ;; Example 3: Invalid BASE64
  (message "\n3. Invalid BASE64 encoding:")
  (condition-case err
      (vcard-compat-parse-21 "BEGIN:VCARD\nVERSION:2.1\nFN:Test\nPHOTO;ENCODING=BASE64:!!!\nEND:VCARD")
    (vcard-compat-encoding-error
     (message "   Caught encoding error: %s" (error-message-string err))))

  (message "\n✓ Error handling working correctly"))

;;; Example 8: Property type conversion

(defun vcard-compat-example-type-conversion ()
  "Example: Show type parameter conversion."
  (let* ((vcard-21 "BEGIN:VCARD
VERSION:2.1
FN:Type Test
TEL;HOME;VOICE;PREF:555-1111
TEL;WORK;FAX:555-2222
EMAIL;INTERNET:test@example.com
ADR;HOME;POSTAL:;;123 Main St;City;ST;12345;USA
END:VCARD")
         (contact (vcard-compat-parse-21 vcard-21)))

    (message "=== Type Parameter Conversion (2.1 → 4.0) ===\n")

    ;; Show telephone type conversion
    (let ((tel-props (slot-value contact 'tel)))
      (message "Telephones:")
      (dolist (tel tel-props)
        (let ((value (oref tel value))
              (params (oref tel parameters)))
          (message "  %s" value)
          (message "    Original (2.1): TEL;HOME;VOICE;PREF or TEL;WORK;FAX")
          (message "    Converted (4.0): TEL;TYPE=%s"
                   (cdr (assoc "TYPE" params))))))

    ;; Show email (INTERNET type should be dropped)
    (let ((email-props (slot-value contact 'email)))
      (message "\nEmail:")
      (dolist (email email-props)
        (let ((value (oref email value))
              (params (oref email parameters)))
          (message "  %s" value)
          (message "    Original (2.1): EMAIL;INTERNET")
          (message "    Converted (4.0): EMAIL (TYPE dropped)")
          (unless (assoc "TYPE" params)
            (message "    ✓ INTERNET type correctly dropped")))))

    contact))

;;; Run all examples

(defun vcard-compat-run-all-examples ()
  "Run all vcard-compat examples."
  (interactive)

  (message "\n╔════════════════════════════════════════════════════════════╗")
  (message "║          vCard Compatibility Layer Examples                ║")
  (message "╚════════════════════════════════════════════════════════════╝\n")

  (message "\n--- Example 1: Parse vCard 2.1 ---")
  (vcard-compat-example-parse-21)

  (message "\n\n--- Example 2: Parse vCard 3.0 ---")
  (vcard-compat-example-parse-30)

  (message "\n\n--- Example 3: Auto-detect version ---")
  (vcard-compat-example-auto-detect)

  (message "\n\n--- Example 6: Encoded photo ---")
  (vcard-compat-example-encoded-photo)

  (message "\n\n--- Example 7: Error handling ---")
  (vcard-compat-example-error-handling)

  (message "\n\n--- Example 8: Type conversion ---")
  (vcard-compat-example-type-conversion)

  (message "\n\n╔════════════════════════════════════════════════════════════╗")
  (message "║                  Examples Complete                          ║")
  (message "╚════════════════════════════════════════════════════════════╝\n"))

(provide 'vcard-compat-examples)
;;; vcard-compat-examples.el ends here
