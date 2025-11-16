;; -*- mode: eask; lexical-binding: t -*-

(package "vcard"
         "1.0.0"
         "vCard 4.0 (RFC 6350) parser and serializer")

(website-url "https://github.com/jwiegley/emacs-vcard")
(keywords "programming" "lisp")

(package-file "vcard.el")

(files
 "vcard-compat.el"
 "vcard-org.el"
 )

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")

(depends-on "emacs" "30.1")
(development
 (depends-on "org"))

(add-hook 'eask-before-compile-hook
           (lambda ()
             (setq byte-compile-error-on-warn t)
             (setq byte-compile-docstring-max-column 1000)))
