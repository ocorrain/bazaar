(in-package #:shopper)

(defparameter *routes* nil)

;; (defun add-route (template handler-function)
;;   )

(defun utility-func ()
  (lambda (&rest args)
    args))

(defparameter *mapper* (make-instance 'routes:mapper))

(defparameter *templates*
  '("new/:(class)"
    "edit/:(class)/:(identifier)/:(page)"
    "delete/:(class)/:(identifier)"
    "/edit/m/:class"))

(dolist (tmp *templates*)
  (let ((route (routes:make-route tmp)))
    (routes:connect *mapper* route)))
