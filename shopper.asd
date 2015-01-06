;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(asdf:defsystem :shopper
  :version "1.0"
  :serial t
  :depends-on (:hunchentoot
	       :elephant
	       :cl-who
	       :alexandria
	       :local-time
	       :url-rewrite
	       :split-sequence
	       :closer-mop
	       :cl-json
	       :trivial-shell
	       :cl-gd
	       :cl-fad
	       :drakma
	       :md5
	       :puri
	       :cl-html-parse
	       :trivial-shell
	       :uuid
	       :cl-stripe
	       :relations)
  :components ((:file "package")
	       (:file "specials")
	       (:file "grid")
	       (:file "base_classes")
	       (:file "shopper")
	       (:file "utilities")
	       (:file "branding")
	       (:file "user")
	       (:file "js")
	       (:file "widgets")
	       (:file "item")
	       (:file "artwork")
	       (:file "gallery")
               (:file "qlist")
	       (:file "single-item")
	       (:file "bundle")
	       (:file "images")
	       (:file "store")
               (:file "forms")
	       (:file "tags")
	       (:file "geo")
	       (:file "json")
	       (:file "validation")
	       (:file "cart")
	       (:file "customer")
	       (:file "static-content")
	       (:file "display")
	       (:file "navigation")
	       (:file "pages")
	       (:file "order")))

(asdf:defsystem :shopper-test
  :components ((:module "test"
			:serial t
			:components ((:file "package")
				     (:file "test"))))
  :depends-on (:shopper :lift))
