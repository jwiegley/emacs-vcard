;;; ecard-carddav-map.el --- Map transformations over CardDAV resources -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: comm, data, carddav
;; URL: https://github.com/jwiegley/dot-emacs

;;; Commentary:

;; This module provides mapping and transformation capabilities for CardDAV
;; addressbook resources. It allows applying transformation functions to all
;; resources in an addressbook with automatic change detection, conflict
;; resolution, and error handling.
;;
;; Features:
;; - Apply transformation functions to all resources in an addressbook
;; - Automatic change detection via serialized vCard comparison
;; - ETag-based conflict detection and resolution
;; - Progress reporting via callback
;; - Comprehensive error handling and recovery
;; - Support for resource modification and deletion
;; - Statistics tracking (processed, modified, deleted, failed, skipped)
;;
;; Example usage:
;;
;;   ;; Add NOTE to all contacts
;;   (ecard-carddav-map-resources
;;    addressbook
;;    (lambda (resource)
;;      (let ((ecard (oref resource ecard)))
;;        (unless (ecard-note ecard)
;;          (ecard-add-property ecard 'note "Imported from sync")
;;          t))))  ; Return t to indicate modification
;;
;;   ;; Delete contacts without email
;;   (ecard-carddav-map-resources
;;    addressbook
;;    (lambda (resource)
;;      (let ((ecard (oref resource ecard)))
;;        (unless (ecard-email ecard)
;;          :delete))))  ; Return :delete to delete resource
;;
;;   ;; Fix phone number format with progress reporting
;;   (ecard-carddav-map-resources
;;    addressbook
;;    (lambda (resource)
;;      (let ((ecard (oref resource ecard)))
;;        (when-let ((tels (ecard-tel ecard)))
;;          (dolist (tel tels)
;;            (oset tel value (normalize-phone (oref tel value))))
;;          t)))
;;    :progress-callback
;;    (lambda (stats)
;;      (message "Progress: %d/%d processed, %d modified"
;;               (plist-get stats :processed)
;;               (plist-get stats :total)
;;               (plist-get stats :modified))))

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'ecard)
(require 'ecard-carddav)

;;; Custom group

(defgroup ecard-carddav-map nil
  "Map transformations over CardDAV resources."
  :group 'ecard-carddav
  :prefix "ecard-carddav-map-")

(defcustom ecard-carddav-map-batch-size 10
  "Number of resources to fetch in a single multiget request.
Larger values improve performance but use more memory."
  :type 'integer
  :group 'ecard-carddav-map)

;;; Error conditions

(define-error 'ecard-carddav-map-error "CardDAV map error")

;;; Internal helper functions


(defun ecard-carddav-map--make-stats (&rest args)
  "Create statistics plist with ARGS.
Accepts keyword arguments for :total, :processed, :modified, :deleted,
:failed, :skipped, :errors."
  (let ((stats (list :total 0
                     :processed 0
                     :modified 0
                     :deleted 0
                     :failed 0
                     :skipped 0
                     :errors nil)))
    (while args
      (let ((key (car args))
            (val (cadr args)))
        (plist-put stats key val)
        (setq args (cddr args))))
    stats))

(defun ecard-carddav-map--update-stats (stats &rest updates)
  "Update STATS plist with UPDATES.
Returns new stats plist. UPDATES is a plist of keys and values to update."
  (let ((new-stats (copy-sequence stats)))
    (while updates
      (let ((key (car updates))
            (val (cadr updates)))
        (pcase key
          (:processed (plist-put new-stats :processed val))
          (:modified (plist-put new-stats :modified val))
          (:deleted (plist-put new-stats :deleted val))
          (:failed (plist-put new-stats :failed val))
          (:skipped (plist-put new-stats :skipped val))
          (:add-error
           (plist-put new-stats :errors
                      (append (plist-get new-stats :errors)
                              (list val)))))
        (setq updates (cddr updates))))
    new-stats))

(defun ecard-carddav-map--handle-resource-update (addressbook resource stats on-conflict)
  "Update RESOURCE on server if modified.
ADDRESSBOOK is the parent addressbook.
STATS is the current statistics plist.
ON-CONFLICT is the conflict resolution strategy (:skip, :retry-once, :force).
Returns updated stats plist."
  (condition-case err
      (progn
        ;; Update with ETag validation
        (ecard-carddav-put-ecard
         addressbook
         (oref resource path)
         (oref resource ecard)
         (oref resource etag))
        ;; Success - increment modified count
        (ecard-carddav-map--update-stats
         stats
         :modified (1+ (plist-get stats :modified))))
    (ecard-carddav-conflict-error
     ;; ETag conflict - resource changed on server
     (pcase on-conflict
       (:skip
        ;; Skip this update
        (ecard-carddav-map--update-stats
         stats
         :skipped (1+ (plist-get stats :skipped))
         :add-error (list :path (oref resource path)
                          :type 'conflict
                          :message "Resource modified on server (skipped)")))
       (:retry-once
        ;; Fetch latest version and retry once
        (condition-case retry-err
            (let* ((latest (ecard-carddav-get-resource
                           addressbook
                           (oref resource path)))
                   (latest-etag (oref latest etag)))
              ;; Update the ecard data but keep transformation result
              ;; This is a simple merge strategy
              (ecard-carddav-put-ecard
               addressbook
               (oref resource path)
               (oref resource ecard)
               latest-etag)
              (ecard-carddav-map--update-stats
               stats
               :modified (1+ (plist-get stats :modified))))
          (error
           ;; Retry failed
           (ecard-carddav-map--update-stats
            stats
            :failed (1+ (plist-get stats :failed))
            :add-error (list :path (oref resource path)
                             :type 'retry-failed
                             :message (error-message-string retry-err))))))
       (:force
        ;; Force update by fetching latest ETag
        (condition-case force-err
            (let* ((latest (ecard-carddav-get-resource
                           addressbook
                           (oref resource path)))
                   (latest-etag (oref latest etag)))
              (ecard-carddav-put-ecard
               addressbook
               (oref resource path)
               (oref resource ecard)
               latest-etag)
              (ecard-carddav-map--update-stats
               stats
               :modified (1+ (plist-get stats :modified))))
          (error
           (ecard-carddav-map--update-stats
            stats
            :failed (1+ (plist-get stats :failed))
            :add-error (list :path (oref resource path)
                             :type 'force-failed
                             :message (error-message-string force-err))))))
       (_
        ;; Unknown strategy - fail
        (ecard-carddav-map--update-stats
         stats
         :failed (1+ (plist-get stats :failed))
         :add-error (list :path (oref resource path)
                          :type 'conflict
                          :message "ETag conflict")))))
    (error
     ;; Other error
     (ecard-carddav-map--update-stats
      stats
      :failed (1+ (plist-get stats :failed))
      :add-error (list :path (oref resource path)
                       :type 'update-error
                       :message (error-message-string err))))))

(defun ecard-carddav-map--handle-resource-delete (addressbook resource stats on-conflict)
  "Delete RESOURCE from server.
ADDRESSBOOK is the parent addressbook.
STATS is the current statistics plist.
ON-CONFLICT is the conflict resolution strategy.
Returns updated stats plist."
  (condition-case err
      (progn
        ;; Delete with ETag validation
        (ecard-carddav-delete-resource
         addressbook
         (oref resource path)
         (oref resource etag))
        ;; Success - increment deleted count
        (ecard-carddav-map--update-stats
         stats
         :deleted (1+ (plist-get stats :deleted))))
    (ecard-carddav-conflict-error
     ;; ETag conflict
     (pcase on-conflict
       (:skip
        (ecard-carddav-map--update-stats
         stats
         :skipped (1+ (plist-get stats :skipped))
         :add-error (list :path (oref resource path)
                          :type 'delete-conflict
                          :message "Resource modified on server (delete skipped)")))
       (:force
        ;; Force delete by fetching latest ETag
        (condition-case force-err
            (let* ((latest (ecard-carddav-get-resource
                           addressbook
                           (oref resource path)))
                   (latest-etag (oref latest etag)))
              (ecard-carddav-delete-resource
               addressbook
               (oref resource path)
               latest-etag)
              (ecard-carddav-map--update-stats
               stats
               :deleted (1+ (plist-get stats :deleted))))
          (error
           (ecard-carddav-map--update-stats
            stats
            :failed (1+ (plist-get stats :failed))
            :add-error (list :path (oref resource path)
                             :type 'delete-force-failed
                             :message (error-message-string force-err))))))
       (_
        (ecard-carddav-map--update-stats
         stats
         :failed (1+ (plist-get stats :failed))
         :add-error (list :path (oref resource path)
                          :type 'delete-conflict
                          :message "ETag conflict on delete")))))
    (ecard-carddav-not-found-error
     ;; Resource already deleted - not an error
     (ecard-carddav-map--update-stats
      stats
      :deleted (1+ (plist-get stats :deleted))))
    (error
     ;; Other error
     (ecard-carddav-map--update-stats
      stats
      :failed (1+ (plist-get stats :failed))
      :add-error (list :path (oref resource path)
                       :type 'delete-error
                       :message (error-message-string err))))))

;;; Public API

;;;###autoload
(defun ecard-carddav-map-resources (addressbook fn &rest args)
  "Map transformation function FN over all resources in ADDRESSBOOK.

FN is called for each resource with the resource object as argument.
The resource's ecard slot contains the parsed vCard object.

FN should return:
  nil       - No change made
  t         - Resource was modified (will be PUT to server)
  :delete   - Resource should be deleted from server
  :skip     - Skip this resource (don't process further)

Any other return value is treated as nil (no change).

ARGS is a plist supporting:

  :progress-callback FUNCTION
    Called periodically with current statistics plist.
    Statistics keys:
      :total :processed :modified :deleted :failed :skipped :errors

  :on-conflict STRATEGY
    How to handle ETag conflicts (resource changed on server).
    Values: :skip (default), :retry-once, :force
    :skip       - Skip the update/delete for this resource
    :retry-once - Fetch latest version and retry update once
    :force      - Force update/delete with latest ETag

  :batch-size INTEGER
    Number of resources to fetch in a single multiget request.
    Defaults to `ecard-carddav-map-batch-size'.

  :filter-fn FUNCTION
    Optional filter function called with each resource path before fetching.
    If it returns nil, resource is skipped. Useful for processing subset.

Returns statistics plist with keys:
  :total      - Total resources in addressbook
  :processed  - Number of resources processed by FN
  :modified   - Number of resources successfully updated
  :deleted    - Number of resources successfully deleted
  :failed     - Number of operations that failed
  :skipped    - Number of resources skipped (conflicts or by FN)
  :errors     - List of error plists (:path :type :message)

Example:
  (ecard-carddav-map-resources
   addressbook
   (lambda (resource)
     (let ((ecard (oref resource ecard)))
       (unless (ecard-note ecard)
         (ecard-add-property ecard \\='note \"Imported\")
         t)))
   :progress-callback
   (lambda (stats)
     (message \"Progress: %d/%d\"
              (plist-get stats :processed)
              (plist-get stats :total))))"
  (let* ((progress-callback (plist-get args :progress-callback))
         (on-conflict (or (plist-get args :on-conflict) :skip))
         (batch-size (or (plist-get args :batch-size)
                         ecard-carddav-map-batch-size))
         (filter-fn (plist-get args :filter-fn))
         ;; List all resources in addressbook
         (all-resources (ecard-carddav-list-resources addressbook))
         (resource-paths (mapcar (lambda (r) (oref r path)) all-resources))
         ;; Filter paths if filter-fn provided
         (filtered-paths (if filter-fn
                            (cl-remove-if-not filter-fn resource-paths)
                          resource-paths))
         (total (length filtered-paths))
         (stats (ecard-carddav-map--make-stats :total total)))

    ;; Process resources in batches using multiget
    (let ((remaining-paths filtered-paths)
          (processed 0))
      (while remaining-paths
        (let* ((batch (cl-subseq remaining-paths 0 (min batch-size (length remaining-paths))))
               (batch-resources (condition-case _
                                   (ecard-carddav-multiget-resources addressbook batch)
                                 (error
                                  ;; Multiget failed - try fetching individually
                                  (mapcar (lambda (path)
                                           (condition-case fetch-err
                                               (ecard-carddav-get-resource addressbook path)
                                             (error
                                              (setq stats
                                                    (ecard-carddav-map--update-stats
                                                     stats
                                                     :failed (1+ (plist-get stats :failed))
                                                     :add-error (list :path path
                                                                     :type 'fetch-error
                                                                     :message (error-message-string fetch-err))))
                                              nil)))
                                         batch)))))

          ;; Process each resource in batch
          (dolist (resource batch-resources)
            (when resource  ; Skip nil entries from failed fetches
              (setq processed (1+ processed))
              (setq stats (plist-put stats :processed processed))

              ;; Serialize before transformation for change detection
              (let ((before-data (condition-case serialize-err
                                     (ecard-serialize (oref resource ecard))
                                   (error
                                    (setq stats
                                          (ecard-carddav-map--update-stats
                                           stats
                                           :failed (1+ (plist-get stats :failed))
                                           :add-error (list :path (oref resource path)
                                                           :type 'serialize-error
                                                           :message (error-message-string serialize-err))))
                                    nil))))
                (when before-data  ; Only process if serialization succeeded
                  ;; Call transformation function with error handling
                  (let ((result (condition-case transform-err
                                    (funcall fn resource)
                                  (error
                                   (setq stats
                                         (ecard-carddav-map--update-stats
                                          stats
                                          :failed (1+ (plist-get stats :failed))
                                          :add-error (list :path (oref resource path)
                                                          :type 'transform-error
                                                          :message (error-message-string transform-err))))
                                   nil))))

                  ;; Handle result
                  (pcase result
                    (:delete
                     ;; Delete resource
                     (setq stats (ecard-carddav-map--handle-resource-delete
                                 addressbook resource stats on-conflict)))
                    (:skip
                     ;; Skip resource
                     (setq stats (ecard-carddav-map--update-stats
                                 stats
                                 :skipped (1+ (plist-get stats :skipped)))))
                    ('t
                     ;; Modification claimed - verify by change detection
                     ;; Compare serialization before and after transformation
                     (let ((after-data (ecard-serialize (oref resource ecard))))
                       (if (not (string= before-data after-data))
                           (setq stats (ecard-carddav-map--handle-resource-update
                                       addressbook resource stats on-conflict))
                         ;; No actual change detected - skip
                         (setq stats (ecard-carddav-map--update-stats
                                     stats
                                     :skipped (1+ (plist-get stats :skipped)))))))
                    (_
                     ;; nil or other value - no change
                     nil)))))

              ;; Call progress callback if provided
              (when (and progress-callback (zerop (mod processed 5)))
                (funcall progress-callback stats))))

          ;; Move to next batch
          (setq remaining-paths (nthcdr batch-size remaining-paths)))))

    ;; Final progress callback
    (when progress-callback
      (funcall progress-callback stats))

    ;; Return final statistics
    stats))

(provide 'ecard-carddav-map)
;;; ecard-carddav-map.el ends here
