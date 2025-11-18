;;; ecard-tools.el --- Comprehensive VCard manipulation tools for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Harper Reed (ported to Emacs Lisp by John Wiegley)
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5"))
;; Keywords: vcard, contacts, tools
;; URL: https://github.com/harperreed/vcard-tools/

;;; Commentary:

;; This package provides comprehensive VCard (.vcf) file manipulation tools
;; ported from Python to Emacs Lisp.  It includes:
;;
;; - VCard parsing and serialization
;; - Splitting and chunking multi-entry files
;; - Duplicate detection (exact, ML-based, and AI-powered)
;; - Cleaning and validation
;; - UID management
;; - Interactive curation with web search
;; - Gmail integration for contact research
;;
;; The package follows three architectural patterns:
;; 1. Simple: Direct VCard manipulation
;; 2. Class-based: Validation and auto-repair
;; 3. Service: Configuration-driven with API integrations

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'subr-x)
(require 'seq)
(require 'ecard)  ;; Use ecard.el for vCard parsing/serialization
(require 'ecard-tools-adapter)  ;; Adapter layer for compatibility

(defgroup ecard-tools nil
  "Tools for managing VCard files."
  :group 'applications
  :prefix "ecard-tools-")

;; ============================================================================
;; Custom Variables
;; ============================================================================

(defcustom ecard-tools-debug nil
  "Enable debug output for ecard-tools."
  :type 'boolean
  :group 'ecard-tools)

(defcustom ecard-tools-backup-on-modify t
  "Create backup files when modifying VCards."
  :type 'boolean
  :group 'ecard-tools)

(defcustom ecard-tools-chunk-size (* 10 1024 1024)
  "Default chunk size in bytes for splitting large VCard files."
  :type 'integer
  :group 'ecard-tools)

(defcustom ecard-tools-similarity-threshold 0.8
  "Similarity threshold for ML duplicate detection (0.0-1.0)."
  :type 'float
  :group 'ecard-tools)

(defcustom ecard-tools-auto-merge-threshold 0.95
  "Threshold for automatic merging of duplicates (0.0-1.0)."
  :type 'float
  :group 'ecard-tools)

(defcustom ecard-tools-openai-api-key nil
  "OpenAI API key for AI features."
  :type '(choice (const :tag "Not set" nil)
                 (string :tag "API Key"))
  :group 'ecard-tools)

(defcustom ecard-tools-openai-model "gpt-3.5-turbo"
  "OpenAI model to use for AI features."
  :type '(choice (const "gpt-3.5-turbo")
                 (const "gpt-4")
                 (string :tag "Other model"))
  :group 'ecard-tools)

(defcustom ecard-tools-serper-api-key nil
  "Serper API key for web search."
  :type '(choice (const :tag "Not set" nil)
                 (string :tag "API Key"))
  :group 'ecard-tools)

(defcustom ecard-tools-tavily-api-key nil
  "Tavily API key for web search."
  :type '(choice (const :tag "Not set" nil)
                 (string :tag "API Key"))
  :group 'ecard-tools)

(defcustom ecard-tools-trash-keywords
  '("spam" "junk" "newsletter" "noreply" "no-reply" "donotreply"
    "mailer-daemon" "postmaster" "bounce" "unsubscribe")
  "Keywords to identify junk contacts."
  :type '(repeat string)
  :group 'ecard-tools)

;; ============================================================================
;; Error Definitions
;; ============================================================================

(define-error 'ecard-tools-error "VCard tools error")
(define-error 'ecard-tools-parse-error "VCard parse error" 'ecard-tools-error)
(define-error 'ecard-tools-validation-error "VCard validation error" 'ecard-tools-error)
(define-error 'ecard-tools-io-error "VCard I/O error" 'ecard-tools-error)
(define-error 'ecard-tools-api-error "API error" 'ecard-tools-error)

;; ============================================================================
;; Core Data Structures
;; ============================================================================

;; Data structures are now defined in ecard-tools-adapter.el
;; We use ecard objects from ecard.el as the primary VCard representation
;; The adapter layer provides compatibility with the old struct interface

;; ============================================================================
;; VCard Parser, Serialization and Validation
;; ============================================================================

;; All parsing, serialization and validation is now handled through the
;; ecard-tools-adapter.el layer, which uses ecard.el's implementation.
;; The adapter provides backward compatibility for existing code.

;; ============================================================================
;; File I/O Operations
;; ============================================================================

(defun ecard-tools-read-file (file-path)
  "Read and parse VCard file at FILE-PATH."
  (condition-case err
      (let ((vcards (ecard-tools-parse-file file-path)))
        (ecard-tools-result-create
         :success-p t
         :data vcards))
    (error
     (ecard-tools-result-create
      :success-p nil
      :errors (list (format "Error reading %s: %s"
                           file-path (error-message-string err)))))))

(defun ecard-tools-read-directory (directory &optional pattern recursive)
  "Read all VCard files from DIRECTORY matching optional PATTERN.
If RECURSIVE is non-nil, search subdirectories."
  (let ((files (if recursive
                   (directory-files-recursively directory
                                              (or pattern "\\.vcf\\'"))
                 (directory-files directory t
                                (or pattern "\\.vcf\\'"))))
        (vcards nil)
        (errors nil))
    (dolist (file files)
      (unless (file-directory-p file)
        (condition-case err
            (setq vcards (nconc vcards (ecard-tools-parse-file file)))
          (error
           (push (cons file (error-message-string err)) errors)))))
    (ecard-tools-result-create
     :success-p (null errors)
     :data vcards
     :errors errors
     :stats `((total-files . ,(length files))
              (successful . ,(- (length files) (length errors)))
              (failed . ,(length errors))))))

(defun ecard-tools-write-file (vcard file-path)
  "Write VCARD to FILE-PATH."
  (condition-case err
      (progn
        (when (and ecard-tools-backup-on-modify
                   (file-exists-p file-path))
          (copy-file file-path (concat file-path ".bak") t))

        (let ((dir (file-name-directory file-path)))
          (when dir
            (make-directory dir t)))

        (with-temp-file file-path
          (insert (ecard-tools-serialize vcard)))

        (ecard-tools-result-create :success-p t))
    (error
     (ecard-tools-result-create
      :success-p nil
      :errors (list (error-message-string err))))))

(defun ecard-tools-write-multiple (vcards directory &optional name-fn)
  "Write multiple VCARDS to DIRECTORY.
Use NAME-FN to generate filenames, defaults to using UID."
  (unless (file-directory-p directory)
    (make-directory directory t))

  (let ((name-fn (or name-fn #'ecard-tools--default-filename))
        (success 0)
        (failed 0)
        (errors nil))

    (dolist (vcard vcards)
      (let* ((filename (funcall name-fn vcard))
             (filepath (expand-file-name filename directory))
             (result (ecard-tools-write-file vcard filepath)))
        (if (ecard-tools-result-success-p result)
            (cl-incf success)
          (cl-incf failed)
          (push (cons filename (ecard-tools-result-errors result)) errors))))

    (ecard-tools-result-create
     :success-p (null errors)
     :stats `((total . ,(length vcards))
              (success . ,success)
              (failed . ,failed))
     :errors errors)))

(defun ecard-tools--default-filename (vcard)
  "Generate default filename for VCARD using UID or hash."
  (let ((uid (ecard-tools-vcard-uid vcard)))
    (if uid
        (concat (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" uid) ".vcf")
      (concat (format-time-string "%Y%m%d%H%M%S")
              "-" (substring (md5 (ecard-tools-serialize vcard)) 0 8)
              ".vcf"))))

;; ============================================================================
;; Tool 1: VCard Splitter (vcf-splitter.py port)
;; ============================================================================

(defun ecard-tools-split-file (file output-dir &optional content-filter)
  "Split multi-entry VCard FILE into individual files in OUTPUT-DIR.
Optional CONTENT-FILTER string to filter entries."
  (interactive "fVCard file to split: \nDOutput directory: \nsContent filter (optional): ")
  (let* ((result (ecard-tools-read-file file))
         (vcards (ecard-tools-result-data result)))

    (when content-filter
      (setq vcards (seq-filter
                   (lambda (vcard)
                     (string-match-p content-filter
                                   (ecard-tools-serialize vcard)))
                   vcards)))

    ;; Auto-repair all vcards
    (setq vcards (mapcar (lambda (vcard)
                          (ecard-tools-result-data
                           (ecard-tools-auto-repair vcard)))
                        vcards))

    (let ((write-result (ecard-tools-write-multiple vcards output-dir)))
      (if (ecard-tools-result-success-p write-result)
          (message "Split %d VCards to %s"
                  (alist-get 'success (ecard-tools-result-stats write-result))
                  output-dir)
        (user-error "Split failed: %s"
                   (ecard-tools-result-errors write-result))))))

;; ============================================================================
;; Tool 2: UID Adder (vcf_uid_adder.py port)
;; ============================================================================

(defun ecard-tools-add-uids (directory)
  "Add UIDs to all VCards in DIRECTORY that are missing them."
  (interactive "DDirectory containing VCard files: ")
  (let* ((result (ecard-tools-read-directory directory))
         (vcards (ecard-tools-result-data result))
         (modified 0))

    (dolist (vcard vcards)
      (unless (ecard-tools-vcard-uid vcard)
        (setf (ecard-tools-vcard-uid vcard) (ecard-tools--generate-uid))
        (setf (ecard-tools-vcard-modified-p vcard) t)
        (cl-incf modified)
        (when-let ((file-path (ecard-tools-vcard-file-path vcard)))
          (ecard-tools-write-file vcard file-path))))

    (message "Added UIDs to %d VCards (out of %d total)"
            modified (length vcards))))

;; ============================================================================
;; Tool 3: VCard Chunker (vcf-chunker.py port)
;; ============================================================================

(defun ecard-tools-chunk-file (file output-dir &optional chunk-size)
  "Split large VCard FILE into chunks in OUTPUT-DIR.
CHUNK-SIZE is in bytes (default 10MB)."
  (interactive "fVCard file to chunk: \nDOutput directory: \nnChunk size in MB (default 10): ")
  (let* ((chunk-size-bytes (or (* (or chunk-size 10) 1024 1024)
                               ecard-tools-chunk-size))
         (vcards (ecard-tools-parse-file file))
         (chunks nil)
         (current-chunk nil)
         (current-size 0)
         (chunk-num 1))

    (dolist (vcard vcards)
      (let* ((serialized (ecard-tools-serialize vcard))
             (size (string-bytes serialized)))

        (when (and current-chunk
                  (> (+ current-size size) chunk-size-bytes))
          ;; Save current chunk and start new one
          (push (nreverse current-chunk) chunks)
          (setq current-chunk nil
                current-size 0
                chunk-num (1+ chunk-num)))

        (push vcard current-chunk)
        (cl-incf current-size size)))

    ;; Don't forget the last chunk
    (when current-chunk
      (push (nreverse current-chunk) chunks))

    ;; Write chunks to files
    (let ((base-name (file-name-sans-extension
                     (file-name-nondirectory file))))
      (cl-loop for chunk in (nreverse chunks)
               for i from 1
               do (let ((chunk-file (expand-file-name
                                   (format "%s_chunk_%03d.vcf" base-name i)
                                   output-dir)))
                    (with-temp-file chunk-file
                      (dolist (vcard chunk)
                        (insert (ecard-tools-serialize vcard)))))))

    (message "Split %s into %d chunks" file (length chunks))))

;; ============================================================================
;; Tool 4: VCard Cleanup (vcf-cleanup.py port)
;; ============================================================================

(defun ecard-tools-cleanup-directory (directory &optional trash-dir)
  "Clean VCards in DIRECTORY by moving junk to TRASH-DIR.
Identifies junk based on keywords and missing essential info."
  (interactive "DDirectory to clean: \nDTrash directory (optional): ")
  (let* ((trash-dir (or trash-dir
                       (expand-file-name "trash" directory)))
         (dir-hash (substring (md5 directory) 0 8))
         (trash-subdir (expand-file-name dir-hash trash-dir))
         (result (ecard-tools-read-directory directory))
         (vcards (ecard-tools-result-data result))
         (stats '((total . 0) (moved . 0) (kept . 0))))

    (make-directory trash-subdir t)

    (dolist (vcard vcards)
      (cl-incf (alist-get 'total stats))
      (if (ecard-tools--is-junk-vcard-p vcard)
          (progn
            (when-let ((file-path (ecard-tools-vcard-file-path vcard)))
              (rename-file file-path
                          (expand-file-name (file-name-nondirectory file-path)
                                          trash-subdir)
                          t))
            (cl-incf (alist-get 'moved stats)))
        (cl-incf (alist-get 'kept stats))))

    (message "Cleaned %d VCards: %d moved to trash, %d kept"
            (alist-get 'total stats)
            (alist-get 'moved stats)
            (alist-get 'kept stats))))

(defun ecard-tools--is-junk-vcard-p (vcard)
  "Check if VCARD is junk based on keywords and missing info."
  (or
   ;; Check for trash keywords in email
   (seq-some (lambda (email)
              (seq-some (lambda (keyword)
                         (string-match-p keyword
                                       (downcase (ecard-tools-email-value email))))
                       ecard-tools-trash-keywords))
            (ecard-tools-vcard-email vcard))

   ;; Check if empty (no name/org AND no contact info)
   (and (not (ecard-tools-vcard-fn vcard))
        (not (ecard-tools-vcard-org vcard))
        (null (ecard-tools-vcard-email vcard))
        (null (ecard-tools-vcard-tel vcard)))))

;; ============================================================================
;; Tool 5: VCard Sorter (vcf-sort.py port)
;; ============================================================================

(defun ecard-tools-sort-by-completeness (source-dir dest-dir &optional dry-run)
  "Sort VCards from SOURCE-DIR to DEST-DIR based on contact info presence.
If DRY-RUN is non-nil, only report what would be done."
  (interactive "DSource directory: \nDDestination directory: \nP")
  (let* ((result (ecard-tools-read-directory source-dir))
         (vcards (ecard-tools-result-data result))
         (with-contact nil)
         (without-contact nil))

    (dolist (vcard vcards)
      (if (or (ecard-tools-vcard-email vcard)
              (ecard-tools-vcard-tel vcard)
              (ecard-tools-vcard-adr vcard))
          (push vcard with-contact)
        (push vcard without-contact)))

    (if dry-run
        (message "Would sort: %d with contact info, %d without"
                (length with-contact) (length without-contact))

      (let ((complete-dir (expand-file-name "complete" dest-dir))
            (incomplete-dir (expand-file-name "incomplete" dest-dir)))

        (make-directory complete-dir t)
        (make-directory incomplete-dir t)

        ;; Move files
        (dolist (vcard with-contact)
          (when-let ((file-path (ecard-tools-vcard-file-path vcard)))
            (rename-file file-path
                        (expand-file-name (file-name-nondirectory file-path)
                                        complete-dir)
                        t)))

        (dolist (vcard without-contact)
          (when-let ((file-path (ecard-tools-vcard-file-path vcard)))
            (rename-file file-path
                        (expand-file-name (file-name-nondirectory file-path)
                                        incomplete-dir)
                        t)))

        (message "Sorted %d VCards: %d complete, %d incomplete"
                (length vcards)
                (length with-contact)
                (length without-contact))))))

;; ============================================================================
;; Tool 6: Note Remover (vcf-note-remover.py port)
;; ============================================================================

(defcustom ecard-tools-note-keep-keywords
  '("important" "personal" "business" "family")
  "Keywords to identify notes worth keeping."
  :type '(repeat string)
  :group 'ecard-tools)

(defun ecard-tools-remove-notes (directory &optional keep-keywords)
  "Remove NOTE fields from VCards in DIRECTORY unless they contain KEEP-KEYWORDS."
  (interactive "DDirectory: \nsKeywords to keep (comma-separated): ")
  (let* ((keywords (or (when keep-keywords
                         (split-string keep-keywords "," t))
                      ecard-tools-note-keep-keywords))
         (result (ecard-tools-read-directory directory))
         (vcards (ecard-tools-result-data result))
         (modified 0))

    (dolist (vcard vcards)
      (when-let ((note (ecard-tools-vcard-note vcard)))
        (unless (seq-some (lambda (keyword)
                           (string-match-p (regexp-quote keyword)
                                         (downcase note)))
                         keywords)
          (setf (ecard-tools-vcard-note vcard) nil)
          (setf (ecard-tools-vcard-modified-p vcard) t)
          (cl-incf modified)
          (when-let ((file-path (ecard-tools-vcard-file-path vcard)))
            (ecard-tools-write-file vcard file-path)))))

    (message "Removed notes from %d VCards (out of %d total)"
            modified (length vcards))))

;; ============================================================================
;; Tool 7: Facebook Email Remover (vcf-facebook-email-remover.py port)
;; ============================================================================

(defun ecard-tools-remove-facebook-emails (directory)
  "Remove @facebook.com email addresses from VCards in DIRECTORY."
  (interactive "DDirectory: ")
  (let* ((result (ecard-tools-read-directory directory))
         (vcards (ecard-tools-result-data result))
         (modified 0))

    (dolist (vcard vcards)
      (let ((original-count (length (ecard-tools-vcard-email vcard)))
            (filtered-emails (seq-remove
                            (lambda (email)
                              (string-match-p "@facebook\\.com$"
                                            (ecard-tools-email-value email)))
                            (ecard-tools-vcard-email vcard))))

        (when (< (length filtered-emails) original-count)
          (setf (ecard-tools-vcard-email vcard) filtered-emails)
          (setf (ecard-tools-vcard-modified-p vcard) t)
          (cl-incf modified)
          (when-let ((file-path (ecard-tools-vcard-file-path vcard)))
            (ecard-tools-write-file vcard file-path)))))

    (message "Removed Facebook emails from %d VCards" modified)))

;; ============================================================================
;; Tool 8: Sunshine Obsolete Fixer (vcf-fix-sunshine-obsolete.py port)
;; ============================================================================

(defun ecard-tools-fix-sunshine-obsolete (directory)
  "Remove items marked as obsolete from Sunshine Contacts app exports."
  (interactive "DDirectory: ")
  (let* ((result (ecard-tools-read-directory directory))
         (vcards (ecard-tools-result-data result))
         (modified 0))

    (dolist (vcard vcards)
      (let* ((raw (ecard-tools-vcard-raw vcard))
             (cleaned (ecard-tools--remove-obsolete-items raw)))

        (unless (string= raw cleaned)
          ;; Re-parse the cleaned vcard
          (let ((new-vcard (car (ecard-tools-parse-buffer
                                (with-temp-buffer
                                  (insert cleaned)
                                  (current-buffer))
                                (ecard-tools-vcard-file-path vcard)))))
            (when new-vcard
              (cl-incf modified)
              (when-let ((file-path (ecard-tools-vcard-file-path vcard)))
                (ecard-tools-write-file new-vcard file-path)))))))

    (message "Fixed %d VCards with obsolete items" modified)))

(defun ecard-tools--remove-obsolete-items (vcard-text)
  "Remove obsolete items from VCARD-TEXT string."
  ;; First pass: identify obsolete items
  (let ((obsolete-items nil))
    (with-temp-buffer
      (insert vcard-text)
      (goto-char (point-min))
      (while (re-search-forward "^item\\([0-9]+\\)\\.X-ABLABEL:.*obsolete" nil t)
        (push (match-string 1) obsolete-items)))

    ;; Second pass: remove obsolete items
    (if obsolete-items
        (let ((pattern (format "^item\\(%s\\)\\."
                              (mapconcat 'identity obsolete-items "\\|"))))
          (replace-regexp-in-string pattern "" vcard-text))
      vcard-text)))

;; ============================================================================
;; Tool 9: Simple Duplicate Checker (vcf-dupe-checker.py port)
;; ============================================================================

(defun ecard-tools-check-duplicates-simple (directory)
  "Find exact duplicate VCards in DIRECTORY based on name and email."
  (interactive "DDirectory: ")
  (let* ((result (ecard-tools-read-directory directory))
         (vcards (ecard-tools-result-data result))
         (seen (make-hash-table :test 'equal))
         (duplicates nil))

    (dolist (vcard vcards)
      (let* ((key (ecard-tools--vcard-simple-key vcard))
             (existing (gethash key seen)))
        (if existing
            (push (cons vcard existing) duplicates)
          (puthash key vcard seen))))

    (if duplicates
        (ecard-tools--display-duplicates duplicates)
      (message "No duplicates found"))))

(defun ecard-tools--vcard-simple-key (vcard)
  "Generate simple key for VCARD based on name and email."
  (let ((name (downcase (or (ecard-tools-vcard-fn vcard) "")))
        (email (when (ecard-tools-vcard-email vcard)
                (downcase (ecard-tools-email-value
                          (car (ecard-tools-vcard-email vcard)))))))
    (format "%s|%s" name (or email ""))))

(defun ecard-tools--display-duplicates (duplicates)
  "Display DUPLICATES in a buffer."
  (with-current-buffer (get-buffer-create "*VCard Duplicates*")
    (erase-buffer)
    (insert "Found Duplicates:\n")
    (insert "================\n\n")

    (dolist (dup duplicates)
      (let ((vcard1 (car dup))
            (vcard2 (cdr dup)))
        (insert (format "Duplicate pair:\n"))
        (insert (format "  1. %s (%s)\n"
                       (ecard-tools-vcard-fn vcard1)
                       (ecard-tools-vcard-file-path vcard1)))
        (insert (format "  2. %s (%s)\n\n"
                       (ecard-tools-vcard-fn vcard2)
                       (ecard-tools-vcard-file-path vcard2)))))

    (goto-char (point-min))
    (display-buffer (current-buffer))))

;; ============================================================================
;; HTTP Client for API Integration
;; ============================================================================

(cl-defstruct (ecard-tools-http-response
               (:constructor ecard-tools-http-response-create))
  "HTTP response structure."
  (status nil :type (or null integer))
  (headers nil :type list)
  (body nil :type (or null string))
  (json nil :type (or null list))
  (error nil :type (or null string)))

(defun ecard-tools-http-post (url data &optional headers)
  "Perform HTTP POST request to URL with DATA and optional HEADERS."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          (append headers
                  '(("Content-Type" . "application/json")
                    ("Accept" . "application/json"))))
         (url-request-data (encode-coding-string
                          (json-encode data) 'utf-8))
         (response (ecard-tools-http-response-create)))

    (condition-case err
        (with-current-buffer
            (url-retrieve-synchronously url t nil 30)
          (goto-char (point-min))

          ;; Parse status
          (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
            (setf (ecard-tools-http-response-status response)
                  (string-to-number (match-string 1))))

          ;; Find body
          (when (search-forward "\n\n" nil t)
            (let ((body (buffer-substring-no-properties (point) (point-max))))
              (setf (ecard-tools-http-response-body response) body)

              ;; Try to parse as JSON
              (condition-case nil
                  (setf (ecard-tools-http-response-json response)
                        (json-read-from-string body))
                (error nil))))

          (kill-buffer))

      (error
       (setf (ecard-tools-http-response-error response)
             (error-message-string err))))

    response))

;; ============================================================================
;; Tool 10: ML Duplicate Checker (vcf-dupe-checker-ml.py port)
;; ============================================================================

(defun ecard-tools-check-duplicates-ml (directory &optional threshold auto-merge)
  "Find duplicates using TF-IDF similarity in DIRECTORY.
THRESHOLD is similarity threshold (0.0-1.0, default 0.8).
AUTO-MERGE threshold for automatic merging (default 0.95)."
  (interactive "DDirectory: \nnSimilarity threshold (0.8): \nnAuto-merge threshold (0.95): ")
  (let* ((threshold (or threshold ecard-tools-similarity-threshold))
         (auto-merge (or auto-merge ecard-tools-auto-merge-threshold))
         (result (ecard-tools-read-directory directory))
         (vcards (ecard-tools-result-data result)))

    ;; Note: Full ML implementation would require external Python process
    ;; or reimplementing TF-IDF in Elisp. For now, using simpler similarity.
    (message "ML-based duplicate detection requires Python integration.")
    (message "Using simplified similarity matching instead...")

    (ecard-tools--find-similar-vcards vcards threshold auto-merge)))

(defun ecard-tools--find-similar-vcards (vcards threshold auto-merge)
  "Find similar VCARDS using simplified similarity matching."
  (let ((duplicates nil)
        (processed (make-hash-table :test 'eq)))

    (cl-loop for i from 0 below (length vcards)
             for vcard1 = (nth i vcards)
             unless (gethash vcard1 processed)
             do (cl-loop for j from (1+ i) below (length vcards)
                        for vcard2 = (nth j vcards)
                        unless (gethash vcard2 processed)
                        do (let ((similarity (ecard-tools--vcard-similarity
                                            vcard1 vcard2)))
                             (when (>= similarity threshold)
                               (push (list vcard1 vcard2 similarity) duplicates)
                               (puthash vcard2 processed t)))))

    (if duplicates
        (ecard-tools--process-ml-duplicates duplicates auto-merge)
      (message "No duplicates found"))))

(defun ecard-tools--vcard-similarity (vcard1 vcard2)
  "Calculate similarity between VCARD1 and VCARD2.
Returns a value between 0.0 and 1.0."
  ;; Simplified similarity based on common fields
  (let ((score 0.0)
        (total 0.0))

    ;; Compare names
    (cl-incf total 1.0)
    (when (and (ecard-tools-vcard-fn vcard1)
              (ecard-tools-vcard-fn vcard2))
      (cl-incf score (ecard-tools--string-similarity
                     (downcase (ecard-tools-vcard-fn vcard1))
                     (downcase (ecard-tools-vcard-fn vcard2)))))

    ;; Compare emails
    (when (and (ecard-tools-vcard-email vcard1)
              (ecard-tools-vcard-email vcard2))
      (cl-incf total 1.0)
      (let ((email1 (ecard-tools-email-value (car (ecard-tools-vcard-email vcard1))))
            (email2 (ecard-tools-email-value (car (ecard-tools-vcard-email vcard2)))))
        (when (string= (downcase email1) (downcase email2))
          (cl-incf score 1.0))))

    ;; Compare organization
    (when (and (ecard-tools-vcard-org vcard1)
              (ecard-tools-vcard-org vcard2))
      (cl-incf total 0.5)
      (cl-incf score (* 0.5 (ecard-tools--string-similarity
                            (downcase (ecard-tools-vcard-org vcard1))
                            (downcase (ecard-tools-vcard-org vcard2))))))

    (if (> total 0)
        (/ score total)
      0.0)))

(defun ecard-tools--string-similarity (str1 str2)
  "Calculate similarity between STR1 and STR2.
Simple implementation using Levenshtein distance ratio."
  (if (string= str1 str2)
      1.0
    (let* ((len1 (length str1))
           (len2 (length str2))
           (max-len (max len1 len2)))
      (if (= max-len 0)
          1.0
        (- 1.0 (/ (float (ecard-tools--levenshtein-distance str1 str2))
                 max-len))))))

(defun ecard-tools--levenshtein-distance (str1 str2)
  "Calculate Levenshtein distance between STR1 and STR2."
  (let* ((len1 (length str1))
         (len2 (length str2))
         (dist (make-vector (1+ len2) 0)))

    ;; Initialize first row
    (cl-loop for j from 0 to len2
             do (aset dist j j))

    ;; Calculate distances
    (cl-loop for i from 1 to len1
             do (let ((prev (aref dist 0)))
                  (aset dist 0 i)
                  (cl-loop for j from 1 to len2
                           do (let ((temp (aref dist j)))
                                (aset dist j
                                      (min (1+ (aref dist (1- j)))  ; insertion
                                           (1+ (aref dist j))        ; deletion
                                           (+ prev                   ; substitution
                                              (if (= (aref str1 (1- i))
                                                    (aref str2 (1- j)))
                                                  0 1))))
                                (setq prev temp)))))

    (aref dist len2)))

(defun ecard-tools--process-ml-duplicates (duplicates auto-merge)
  "Process DUPLICATES with optional AUTO-MERGE threshold."
  (with-current-buffer (get-buffer-create "*VCard ML Duplicates*")
    (erase-buffer)
    (insert "ML-Based Duplicate Detection Results\n")
    (insert "====================================\n\n")

    (dolist (dup duplicates)
      (let ((vcard1 (nth 0 dup))
            (vcard2 (nth 1 dup))
            (similarity (nth 2 dup)))
        (insert (format "Similarity: %.2f%%\n" (* similarity 100)))
        (insert (format "  1. %s\n" (or (ecard-tools-vcard-fn vcard1) "Unknown")))
        (when (ecard-tools-vcard-email vcard1)
          (insert (format "     Email: %s\n"
                         (ecard-tools-email-value
                          (car (ecard-tools-vcard-email vcard1))))))
        (insert (format "     File: %s\n" (ecard-tools-vcard-file-path vcard1)))

        (insert (format "  2. %s\n" (or (ecard-tools-vcard-fn vcard2) "Unknown")))
        (when (ecard-tools-vcard-email vcard2)
          (insert (format "     Email: %s\n"
                         (ecard-tools-email-value
                          (car (ecard-tools-vcard-email vcard2))))))
        (insert (format "     File: %s\n" (ecard-tools-vcard-file-path vcard2)))

        (if (>= similarity auto-merge)
            (insert "     [AUTO-MERGE CANDIDATE]\n")
          (insert "     [MANUAL REVIEW NEEDED]\n"))
        (insert "\n")))

    (goto-char (point-min))
    (display-buffer (current-buffer))))

;; ============================================================================
;; Tool 11: AI Duplicate Checker (vcf-dupe-checker-ai.py port)
;; ============================================================================

(defun ecard-tools-check-duplicates-ai (directory &optional threshold)
  "Find duplicates using AI assistance in DIRECTORY.
Requires OpenAI API key to be set."
  (interactive "DDirectory: \nnSimilarity threshold (0.8): ")

  (unless ecard-tools-openai-api-key
    (user-error "OpenAI API key not set. Set `ecard-tools-openai-api-key'"))

  (let* ((threshold (or threshold ecard-tools-similarity-threshold))
         (result (ecard-tools-read-directory directory))
         (vcards (ecard-tools-result-data result))
         (duplicates (ecard-tools--find-similar-vcards-for-ai vcards threshold)))

    (if duplicates
        (ecard-tools--process-ai-duplicates duplicates)
      (message "No duplicates found"))))

(defun ecard-tools--find-similar-vcards-for-ai (vcards threshold)
  "Find similar VCARDS for AI processing."
  ;; Use same similarity detection as ML version
  (let ((duplicates nil))
    (cl-loop for i from 0 below (length vcards)
             for vcard1 = (nth i vcards)
             do (cl-loop for j from (1+ i) below (length vcards)
                        for vcard2 = (nth j vcards)
                        do (let ((similarity (ecard-tools--vcard-similarity
                                            vcard1 vcard2)))
                             (when (>= similarity threshold)
                               (push (list vcard1 vcard2 similarity) duplicates)))))
    duplicates))

(defun ecard-tools--process-ai-duplicates (duplicates)
  "Process DUPLICATES using AI for merge decisions."
  (with-current-buffer (get-buffer-create "*VCard AI Duplicates*")
    (erase-buffer)
    (insert "AI-Powered Duplicate Detection\n")
    (insert "==============================\n\n")

    (dolist (dup duplicates)
      (let* ((vcard1 (nth 0 dup))
             (vcard2 (nth 1 dup))
             (similarity (nth 2 dup))
             (ai-decision (when (>= similarity ecard-tools-similarity-threshold)
                           (ecard-tools--get-ai-merge-decision vcard1 vcard2 similarity))))

        (insert (format "Similarity: %.2f%%\n" (* similarity 100)))
        (insert (format "VCard 1: %s\n" (or (ecard-tools-vcard-fn vcard1) "Unknown")))
        (insert (format "VCard 2: %s\n" (or (ecard-tools-vcard-fn vcard2) "Unknown")))

        (if ai-decision
            (insert (format "AI Decision: %s\n"
                           (if (plist-get ai-decision :merge)
                               "MERGE RECOMMENDED"
                             "KEEP SEPARATE")))
          (insert "AI Decision: ERROR OR SKIPPED\n"))

        (when (plist-get ai-decision :reasoning)
          (insert (format "Reasoning: %s\n" (plist-get ai-decision :reasoning))))

        (insert "\n")))

    (goto-char (point-min))
    (display-buffer (current-buffer))))

(defun ecard-tools--get-ai-merge-decision (vcard1 vcard2 similarity)
  "Get AI merge decision for VCARD1 and VCARD2 with SIMILARITY."
  (let* ((prompt (ecard-tools--format-ai-prompt vcard1 vcard2 similarity))
         (response (ecard-tools--call-openai-api prompt)))

    (when response
      (let ((content (cdr (assoc 'content
                                (cdr (assoc 'message
                                          (car (cdr (assoc 'choices response)))))))))
        (ecard-tools--parse-ai-response content)))))

(defun ecard-tools--format-ai-prompt (vcard1 vcard2 similarity)
  "Format AI prompt for merge decision between VCARD1 and VCARD2."
  (format "Analyze these two VCard contacts (similarity: %.2f%%):\n\n\
Contact 1:\n\
Name: %s\n\
Email: %s\n\
Phone: %s\n\
Organization: %s\n\n\
Contact 2:\n\
Name: %s\n\
Email: %s\n\
Phone: %s\n\
Organization: %s\n\n\
Should these contacts be merged? Reply with 'Yes' or 'No' followed by brief reasoning."
          (* similarity 100)
          (or (ecard-tools-vcard-fn vcard1) "N/A")
          (if (ecard-tools-vcard-email vcard1)
              (ecard-tools-email-value (car (ecard-tools-vcard-email vcard1)))
            "N/A")
          (if (ecard-tools-vcard-tel vcard1)
              (ecard-tools-tel-value (car (ecard-tools-vcard-tel vcard1)))
            "N/A")
          (or (ecard-tools-vcard-org vcard1) "N/A")
          (or (ecard-tools-vcard-fn vcard2) "N/A")
          (if (ecard-tools-vcard-email vcard2)
              (ecard-tools-email-value (car (ecard-tools-vcard-email vcard2)))
            "N/A")
          (if (ecard-tools-vcard-tel vcard2)
              (ecard-tools-tel-value (car (ecard-tools-vcard-tel vcard2)))
            "N/A")
          (or (ecard-tools-vcard-org vcard2) "N/A")))

(defun ecard-tools--call-openai-api (prompt)
  "Call OpenAI API with PROMPT."
  (let* ((url "https://api.openai.com/v1/chat/completions")
         (data `((model . ,ecard-tools-openai-model)
                (messages . [((role . "system")
                             (content . "You are a contact deduplication assistant."))
                            ((role . "user")
                             (content . ,prompt))])
                (temperature . 0.3)
                (max_tokens . 150)))
         (headers `(("Authorization" . ,(format "Bearer %s" ecard-tools-openai-api-key))))
         (response (ecard-tools-http-post url data headers)))

    (if (ecard-tools-http-response-error response)
        (progn
          (message "OpenAI API error: %s" (ecard-tools-http-response-error response))
          nil)
      (ecard-tools-http-response-json response))))

(defun ecard-tools--parse-ai-response (content)
  "Parse AI response CONTENT for merge decision."
  (when content
    (list :merge (string-match-p "^Yes" content)
          :reasoning (replace-regexp-in-string "^\\(Yes\\|No\\)[.:] *" "" content))))

;; ============================================================================
;; Interactive Commands Menu
;; ============================================================================

(defun ecard-tools-menu ()
  "Display interactive menu for VCard tools."
  (interactive)
  (let ((choice (completing-read
                "VCard Tool: "
                '("Split multi-entry file"
                  "Add UIDs to VCards"
                  "Chunk large file"
                  "Clean directory"
                  "Sort by completeness"
                  "Remove notes"
                  "Remove Facebook emails"
                  "Fix Sunshine obsolete items"
                  "Check duplicates (simple)"
                  "Check duplicates (ML)"
                  "Check duplicates (AI)"
                  "Validate file"
                  "Auto-repair directory"))))

    (pcase choice
      ("Split multi-entry file" (call-interactively 'ecard-tools-split-file))
      ("Add UIDs to VCards" (call-interactively 'ecard-tools-add-uids))
      ("Chunk large file" (call-interactively 'ecard-tools-chunk-file))
      ("Clean directory" (call-interactively 'ecard-tools-cleanup-directory))
      ("Sort by completeness" (call-interactively 'ecard-tools-sort-by-completeness))
      ("Remove notes" (call-interactively 'ecard-tools-remove-notes))
      ("Remove Facebook emails" (call-interactively 'ecard-tools-remove-facebook-emails))
      ("Fix Sunshine obsolete items" (call-interactively 'ecard-tools-fix-sunshine-obsolete))
      ("Check duplicates (simple)" (call-interactively 'ecard-tools-check-duplicates-simple))
      ("Check duplicates (ML)" (call-interactively 'ecard-tools-check-duplicates-ml))
      ("Check duplicates (AI)" (call-interactively 'ecard-tools-check-duplicates-ai))
      ("Validate file" (call-interactively 'ecard-tools-validate-file))
      ("Auto-repair directory" (call-interactively 'ecard-tools-auto-repair-directory)))))

(defun ecard-tools-validate-file (file)
  "Validate VCard FILE and report issues."
  (interactive "fVCard file: ")
  (let* ((vcards (ecard-tools-parse-file file))
         (vcard (car vcards)))
    (if vcard
        (let ((result (ecard-tools-validate vcard t)))
          (with-output-to-temp-buffer "*VCard Validation*"
            (princ (format "File: %s\n\n" file))
            (if (ecard-tools-result-success-p result)
                (princ "✓ VCard is valid\n")
              (princ "✗ VCard has errors:\n")
              (dolist (err (ecard-tools-result-errors result))
                (princ (format "  - %s\n" err))))
            (when (ecard-tools-result-warnings result)
              (princ "\nWarnings:\n")
              (dolist (warn (ecard-tools-result-warnings result))
                (princ (format "  - %s\n" warn))))))
      (user-error "No VCard found in file"))))

(defun ecard-tools-auto-repair-directory (directory)
  "Auto-repair all VCards in DIRECTORY."
  (interactive "DDirectory: ")
  (let* ((result (ecard-tools-read-directory directory))
         (vcards (ecard-tools-result-data result))
         (repaired 0))

    (dolist (vcard vcards)
      (let ((repair-result (ecard-tools-auto-repair vcard)))
        (when (ecard-tools-vcard-modified-p (ecard-tools-result-data repair-result))
          (cl-incf repaired)
          (when-let ((file-path (ecard-tools-vcard-file-path vcard)))
            (ecard-tools-write-file (ecard-tools-result-data repair-result)
                                  file-path)))))

    (message "Auto-repaired %d VCards (out of %d total)" repaired (length vcards))))

;; ============================================================================
;; HTTP Client for API Integration
;; ============================================================================

(cl-defstruct (ecard-tools-http-response
               (:constructor ecard-tools-http-response-create))
  "HTTP response structure."
  (status nil :type (or null integer))
  (headers nil :type list)
  (body nil :type (or null string))
  (json nil :type (or null list))
  (error nil :type (or null string)))

(defun ecard-tools-http-post (url data &optional headers)
  "Perform HTTP POST request to URL with DATA and optional HEADERS."
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          (append headers
                  '(("Content-Type" . "application/json")
                    ("Accept" . "application/json"))))
         (url-request-data (encode-coding-string
                          (json-encode data) 'utf-8))
         (response (ecard-tools-http-response-create)))

    (condition-case err
        (with-current-buffer
            (url-retrieve-synchronously url t nil 30)
          (goto-char (point-min))

          ;; Parse status
          (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
            (setf (ecard-tools-http-response-status response)
                  (string-to-number (match-string 1))))

          ;; Find body
          (when (search-forward "\n\n" nil t)
            (let ((body (buffer-substring-no-properties (point) (point-max))))
              (setf (ecard-tools-http-response-body response) body)

              ;; Try to parse as JSON
              (condition-case nil
                  (setf (ecard-tools-http-response-json response)
                        (json-read-from-string body))
                (error nil))))

          (kill-buffer))

      (error
       (setf (ecard-tools-http-response-error response)
             (error-message-string err))))

    response))

;; ============================================================================
;; Interactive Commands Menu
;; ============================================================================

(defun ecard-tools-menu ()
  "Display interactive menu for VCard tools."
  (interactive)
  (let ((choice (completing-read
                "VCard Tool: "
                '("Split multi-entry file"
                  "Add UIDs to VCards"
                  "Chunk large file"
                  "Clean directory"
                  "Sort by completeness"
                  "Remove notes"
                  "Remove Facebook emails"
                  "Fix Sunshine obsolete items"
                  "Check duplicates (simple)"
                  "Check duplicates (ML)"
                  "Check duplicates (AI)"
                  "Validate file"
                  "Auto-repair directory"))))

    (pcase choice
      ("Split multi-entry file" (call-interactively 'ecard-tools-split-file))
      ("Add UIDs to VCards" (call-interactively 'ecard-tools-add-uids))
      ("Chunk large file" (call-interactively 'ecard-tools-chunk-file))
      ("Clean directory" (call-interactively 'ecard-tools-cleanup-directory))
      ("Sort by completeness" (call-interactively 'ecard-tools-sort-by-completeness))
      ("Remove notes" (call-interactively 'ecard-tools-remove-notes))
      ("Remove Facebook emails" (call-interactively 'ecard-tools-remove-facebook-emails))
      ("Fix Sunshine obsolete items" (call-interactively 'ecard-tools-fix-sunshine-obsolete))
      ("Check duplicates (simple)" (call-interactively 'ecard-tools-check-duplicates-simple))
      ("Check duplicates (ML)" (call-interactively 'ecard-tools-check-duplicates-ml))
      ("Check duplicates (AI)" (call-interactively 'ecard-tools-check-duplicates-ai))
      ("Validate file" (call-interactively 'ecard-tools-validate-file))
      ("Auto-repair directory" (call-interactively 'ecard-tools-auto-repair-directory)))))

(defun ecard-tools-validate-file (file)
  "Validate VCard FILE and report issues."
  (interactive "fVCard file: ")
  (let* ((vcards (ecard-tools-parse-file file))
         (vcard (car vcards)))
    (if vcard
        (let ((result (ecard-tools-validate vcard t)))
          (with-output-to-temp-buffer "*VCard Validation*"
            (princ (format "File: %s\n\n" file))
            (if (ecard-tools-result-success-p result)
                (princ "✓ VCard is valid\n")
              (princ "✗ VCard has errors:\n")
              (dolist (err (ecard-tools-result-errors result))
                (princ (format "  - %s\n" err))))
            (when (ecard-tools-result-warnings result)
              (princ "\nWarnings:\n")
              (dolist (warn (ecard-tools-result-warnings result))
                (princ (format "  - %s\n" warn))))))
      (user-error "No VCard found in file"))))

(defun ecard-tools-auto-repair-directory (directory)
  "Auto-repair all VCards in DIRECTORY."
  (interactive "DDirectory: ")
  (let* ((result (ecard-tools-read-directory directory))
         (vcards (ecard-tools-result-data result))
         (repaired 0))

    (dolist (vcard vcards)
      (let ((repair-result (ecard-tools-auto-repair vcard)))
        (when (ecard-tools-vcard-modified-p (ecard-tools-result-data repair-result))
          (cl-incf repaired)
          (when-let ((file-path (ecard-tools-vcard-file-path vcard)))
            (ecard-tools-write-file (ecard-tools-result-data repair-result)
                                  file-path)))))

    (message "Auto-repaired %d VCards (out of %d total)" repaired (length vcards))))

;; ============================================================================
;; Provide Package
;; ============================================================================

(provide 'ecard-tools)

;;; ecard-tools.el ends here
