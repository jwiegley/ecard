;; -*- mode: eask; lexical-binding: t -*-

(package "ecard"
         "1.0.0"
         "vCard 4.0 (RFC 6350) parser and serializer")

(website-url "https://github.com/jwiegley/ecard")
(keywords "programming" "lisp")

(package-file "ecard.el")

(files
 "ecard-benchmark.el"
 "ecard-carddav-auth.el"
 "ecard-carddav-mock.el"
 "ecard-carddav-sync.el"
 "ecard-carddav.el"
 "ecard-compat-examples.el"
 "ecard-compat.el"
 "ecard-org.el"
 "ecard.el"
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
