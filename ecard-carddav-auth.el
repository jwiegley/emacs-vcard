;;; ecard-carddav-auth.el --- CardDAV authentication handlers -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: comm, data, carddav, auth
;; URL: https://github.com/jwiegley/dot-emacs

;;; Commentary:

;; This module provides authentication support for CardDAV operations.
;; It supports multiple authentication methods:
;;
;; 1. Basic Authentication over HTTPS (RFC 2617)
;; 2. OAuth2 (structure in place for future implementation)
;; 3. Bearer tokens
;;
;; The authentication system is designed to be extensible, allowing
;; additional authentication methods to be added easily.
;;
;; Example usage:
;;
;;   ;; Create Basic Auth credentials
;;   (setq auth (ecard-carddav-auth-basic-create
;;               :username "user"
;;               :password "secret"))
;;
;;   ;; Get Authorization header
;;   (ecard-carddav-auth-get-header auth)
;;   => "Basic dXNlcjpzZWNyZXQ="
;;
;;   ;; Create Bearer token auth
;;   (setq auth (ecard-carddav-auth-bearer-create
;;               :token "abcdef123456"))

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'auth-source)

;;; Custom group

(defgroup ecard-carddav-auth nil
  "Authentication for CardDAV operations."
  :group 'ecard
  :prefix "ecard-carddav-auth-")

;;; Error conditions

(define-error 'ecard-carddav-auth-error "CardDAV authentication error")

;;; Base authentication class

(defclass ecard-carddav-auth ()
  ((type
    :initarg :type
    :initform nil
    :type symbol
    :documentation "Authentication type (:basic, :oauth2, :bearer).")
   (last-refresh
    :initarg :last-refresh
    :initform nil
    :type (or null number)
    :documentation "Timestamp of last credential refresh."))
  "Base class for CardDAV authentication."
  :abstract t)

;;; Basic Authentication (RFC 2617)

(defclass ecard-carddav-auth-basic (ecard-carddav-auth)
  ((username
    :initarg :username
    :initform nil
    :type (or null string)
    :documentation "Username for Basic Auth.")
   (password
    :initarg :password
    :initform nil
    :type (or null string)
    :documentation "Password for Basic Auth."))
  "Basic Authentication credentials for CardDAV.")

(cl-defmethod initialize-instance :after ((auth ecard-carddav-auth-basic) &rest _slots)
  "Initialize Basic Auth object AUTH."
  (oset auth type :basic))

(cl-defmethod ecard-carddav-auth-get-header ((auth ecard-carddav-auth-basic))
  "Get HTTP Authorization header value for Basic AUTH.
Returns string like \"Basic base64(username:password)\"."
  (let ((username (oref auth username))
        (password (oref auth password)))
    (unless (and username password)
      (signal 'ecard-carddav-auth-error
              '("Basic Auth requires username and password")))
    (let ((credentials (base64-encode-string
                       (format "%s:%s" username password)
                       t)))
      (format "Basic %s" credentials))))

(cl-defmethod ecard-carddav-auth-valid-p ((auth ecard-carddav-auth-basic))
  "Check if Basic AUTH credentials are valid.
Returns non-nil if username and password are present."
  (and (oref auth username)
       (oref auth password)
       t))

(cl-defmethod ecard-carddav-auth-refresh ((auth ecard-carddav-auth-basic))
  "Refresh Basic AUTH credentials.
For Basic Auth, this is a no-op since credentials don't expire.
Returns AUTH unchanged."
  (oset auth last-refresh (float-time))
  auth)

;;;###autoload
(defun ecard-carddav-auth-basic-create (&rest args)
  "Create Basic Authentication credentials from ARGS.

ARGS is a plist with keys:
  :username STRING - Username for authentication (required)
  :password STRING - Password for authentication (required)

Example:
  (ecard-carddav-auth-basic-create
   :username \"user@example.com\"
   :password \"secret\")"
  (let ((username (plist-get args :username))
        (password (plist-get args :password)))
    (unless username
      (signal 'ecard-carddav-auth-error '("Username required for Basic Auth")))
    (unless password
      (signal 'ecard-carddav-auth-error '("Password required for Basic Auth")))
    (ecard-carddav-auth-basic
     :username username
     :password password)))

;;; Bearer Token Authentication

(defclass ecard-carddav-auth-bearer (ecard-carddav-auth)
  ((token
    :initarg :token
    :initform nil
    :type (or null string)
    :documentation "Bearer token for authentication.")
   (expires-at
    :initarg :expires-at
    :initform nil
    :type (or null number)
    :documentation "Timestamp when token expires."))
  "Bearer token authentication for CardDAV.")

(cl-defmethod initialize-instance :after ((auth ecard-carddav-auth-bearer) &rest _slots)
  "Initialize Bearer Auth object AUTH."
  (oset auth type :bearer))

(cl-defmethod ecard-carddav-auth-get-header ((auth ecard-carddav-auth-bearer))
  "Get HTTP Authorization header value for Bearer AUTH.
Returns string like \"Bearer <token>\"."
  (let ((token (oref auth token)))
    (unless token
      (signal 'ecard-carddav-auth-error
              '("Bearer Auth requires token")))
    (format "Bearer %s" token)))

(cl-defmethod ecard-carddav-auth-valid-p ((auth ecard-carddav-auth-bearer))
  "Check if Bearer AUTH token is valid and not expired.
Returns non-nil if token exists and hasn't expired."
  (and (oref auth token)
       (let ((expires-at (oref auth expires-at)))
         (or (null expires-at)
             (< (float-time) expires-at)))
       t))

(cl-defmethod ecard-carddav-auth-refresh ((auth ecard-carddav-auth-bearer))
  "Refresh Bearer AUTH token.
This is a placeholder for future OAuth2 implementation.
Returns AUTH unchanged."
  (oset auth last-refresh (float-time))
  auth)

;;;###autoload
(defun ecard-carddav-auth-bearer-create (&rest args)
  "Create Bearer token authentication from ARGS.

ARGS is a plist with keys:
  :token STRING - Bearer token (required)
  :expires-at NUMBER - Unix timestamp when token expires (optional)

Example:
  (ecard-carddav-auth-bearer-create
   :token \"abc123def456\"
   :expires-at (+ (float-time) 3600))"
  (let ((token (plist-get args :token))
        (expires-at (plist-get args :expires-at)))
    (unless token
      (signal 'ecard-carddav-auth-error '("Token required for Bearer Auth")))
    (ecard-carddav-auth-bearer
     :token token
     :expires-at expires-at)))

;;; OAuth2 Authentication (structure for future implementation)

(defclass ecard-carddav-auth-oauth2 (ecard-carddav-auth)
  ((client-id
    :initarg :client-id
    :initform nil
    :type (or null string)
    :documentation "OAuth2 client ID.")
   (client-secret
    :initarg :client-secret
    :initform nil
    :type (or null string)
    :documentation "OAuth2 client secret.")
   (access-token
    :initarg :access-token
    :initform nil
    :type (or null string)
    :documentation "OAuth2 access token.")
   (refresh-token
    :initarg :refresh-token
    :initform nil
    :type (or null string)
    :documentation "OAuth2 refresh token.")
   (token-url
    :initarg :token-url
    :initform nil
    :type (or null string)
    :documentation "OAuth2 token endpoint URL.")
   (expires-at
    :initarg :expires-at
    :initform nil
    :type (or null number)
    :documentation "Timestamp when access token expires."))
  "OAuth2 authentication for CardDAV (future implementation).")

(cl-defmethod initialize-instance :after ((auth ecard-carddav-auth-oauth2) &rest _slots)
  "Initialize OAuth2 Auth object AUTH."
  (oset auth type :oauth2))

(cl-defmethod ecard-carddav-auth-get-header ((auth ecard-carddav-auth-oauth2))
  "Get HTTP Authorization header value for OAuth2 AUTH.
Returns string like \"Bearer <access-token>\"."
  (let ((token (oref auth access-token)))
    (unless token
      (signal 'ecard-carddav-auth-error
              '("OAuth2 requires access token")))
    (format "Bearer %s" token)))

(cl-defmethod ecard-carddav-auth-valid-p ((auth ecard-carddav-auth-oauth2))
  "Check if OAuth2 AUTH access token is valid and not expired.
Returns non-nil if token exists and hasn't expired."
  (and (oref auth access-token)
       (let ((expires-at (oref auth expires-at)))
         (or (null expires-at)
             (< (float-time) expires-at)))
       t))

(cl-defmethod ecard-carddav-auth-refresh ((_auth ecard-carddav-auth-oauth2))
  "Refresh OAuth2 AUTH access token using refresh token.
This is a placeholder for future OAuth2 implementation.
Signals error if not implemented."
  (signal 'ecard-carddav-auth-error
          '("OAuth2 token refresh not yet implemented")))

;;;###autoload
(defun ecard-carddav-auth-oauth2-create (&rest args)
  "Create OAuth2 authentication from ARGS (future implementation).

ARGS is a plist with keys:
  :client-id STRING - OAuth2 client ID (required)
  :client-secret STRING - OAuth2 client secret (required)
  :access-token STRING - Current access token (optional)
  :refresh-token STRING - Refresh token (optional)
  :token-url STRING - Token endpoint URL (required)
  :expires-at NUMBER - Token expiration timestamp (optional)

This is a structure for future implementation."
  (let ((client-id (plist-get args :client-id))
        (client-secret (plist-get args :client-secret))
        (token-url (plist-get args :token-url))
        (access-token (plist-get args :access-token))
        (refresh-token (plist-get args :refresh-token))
        (expires-at (plist-get args :expires-at)))
    (unless client-id
      (signal 'ecard-carddav-auth-error '("Client ID required for OAuth2")))
    (unless client-secret
      (signal 'ecard-carddav-auth-error '("Client secret required for OAuth2")))
    (unless token-url
      (signal 'ecard-carddav-auth-error '("Token URL required for OAuth2")))
    (ecard-carddav-auth-oauth2
     :client-id client-id
     :client-secret client-secret
     :access-token access-token
     :refresh-token refresh-token
     :token-url token-url
     :expires-at expires-at)))

;;; auth-source integration

(defun ecard-carddav-auth-from-authinfo (host &optional port)
  "Create Basic Auth credentials from auth-source for HOST and PORT.
Searches auth-source (e.g., ~/.authinfo.gpg) for credentials.
Returns `ecard-carddav-auth-basic' object or nil if not found.

PORT defaults to 443 (HTTPS)."
  (let* ((port (or port 443))
         (found (auth-source-search
                 :host host
                 :port port
                 :max 1
                 :require '(:user :secret))))
    (when found
      (let* ((entry (car found))
             (user (plist-get entry :user))
             (secret (plist-get entry :secret))
             (password (if (functionp secret)
                          (funcall secret)
                        secret)))
        (when (and user password)
          (ecard-carddav-auth-basic-create
           :username user
           :password password))))))

;;; Helper functions

(defun ecard-carddav-auth-ensure-valid (auth)
  "Ensure AUTH credentials are valid, refreshing if necessary.
Returns AUTH (possibly refreshed).
Signals error if credentials cannot be validated."
  (unless (ecard-carddav-auth-valid-p auth)
    ;; Try to refresh
    (setq auth (ecard-carddav-auth-refresh auth))
    ;; Check again
    (unless (ecard-carddav-auth-valid-p auth)
      (signal 'ecard-carddav-auth-error
              '("Unable to obtain valid credentials"))))
  auth)

(provide 'ecard-carddav-auth)
;;; ecard-carddav-auth.el ends here
