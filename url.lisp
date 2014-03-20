(in-package #:shopper)

;; Here's where we do URL routing and assignent

;; URLs are of the format:

;; /new/<class>               ->   creates a new instance of class (GET and POST)

;; /edit/<identifier>         ->   edit page for an instance or class
;; /edit/<identifier>/<page>  ->   particular edit mode

;; /delete/<identifier>       ->   delete an instance

;; /view/<identifier>         ->   view page for an instance or class
;; /view/<identifier>/page    ->   particular view page
;;
;; Identifiers can be either a class name or an object identifier.
;; Class names will be checked first.

(defparameter *regexes->handlers*
  '(("/new/\w+" . new-item-page)
    ("/edit.*" . edit-handler)
    ("/delete.*" . delete-handler)
    ("/view.*" . view-handler)))

