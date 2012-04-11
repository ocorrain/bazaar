;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(defmethod maybe-create ((type (eql 'single-item)) parameters)
  "Object creation template.  Once we have satisfied the minimal
  criteria for an object (the existence of a title), we fill in as
  many other slots as possible and redirect to the object page"
  (multiple-value-bind (valid errors) (validate 'single-item parameters)
    (flet ((assoc-val (val) (cdr (assoc val valid))))
      (if-let (title (assoc-val 'title))
	(let ((item (make-instance 'single-item)))
	  (set-valid-fields item valid)
	  (when errors (setf (hunchentoot:session-value 'errors) errors))
	  (hunchentoot:redirect (get-url item)))))))

(defmethod maybe-create ((type (eql 'bundle)) parameters)
  (multiple-value-bind (valid errors) (validate 'bundle parameters)
    (flet ((assoc-val (val) (cdr (assoc val valid))))
      (if-let (title (assoc-val 'title))
	(let ((item (make-instance 'bundle)))
	  (set-valid-fields item valid)
	  (when errors (setf (hunchentoot:session-value 'errors) errors))
	  (hunchentoot:redirect (get-url item)))))))

(defmethod maybe-delete-images ((item line-item) parameters)
  (when-let (images-to-delete
	     (remove-if-not (lambda (alist)
			      (eq (car alist) 'imgdel))
			    parameters))
    (dolist (i (mapcar #'cdr images-to-delete))
      (setf (images item)
	    (remove i (images item) :key #'namestring :test #'string-equal)))))

(defmethod maybe-update ((item line-item) parameters)
  (maybe-delete-images item parameters)
  (multiple-value-bind (valid errors) (validate item parameters)
    (set-valid-fields item valid)
    (when errors (setf (hunchentoot:session-value 'errors) errors))
    (hunchentoot:redirect (get-url item))))

(defmethod maybe-update :after ((item bundle) parameters)
  (hunchentoot:log-message* :info "~S" parameters))

(defun maybe-add-image (picture line-item)
  (destructuring-bind (path filename content-type) picture
    (declare (ignore content-type))
    (add-image path filename line-item)))

(defun pathname-name-concat (path concat-string)
  (make-pathname :directory (pathname-directory path)
		 :name (concatenate 'string (pathname-name path) concat-string)
		 :type (pathname-type path)))

(defun get-thumb-path (path)
  (pathname-name-concat path "_thumb"))

(defun get-full-size-path (path)
  (pathname-name-concat path "_full"))


(defmethod set-valid-fields ((item single-item) alist)
  (set-generic-fields item alist)
  (flet ((assoc-val (val) (cdr (assoc val alist))))
    (when-let (weight (assoc-val 'weight))
      (setf (weight item) weight))
    (when-let (price (assoc-val 'price))
      (setf (price item) price))))

(defmethod set-valid-fields ((item bundle) alist)
  (set-generic-fields item alist))

(defun set-generic-fields (line-item alist)
  (flet ((assoc-val (val) (cdr (assoc val alist))))
    (when-let (picture (assoc-val 'picture))
      (maybe-add-image picture line-item))
    (when-let (title (assoc-val 'title))
      (setf (title line-item) title))
    (when-let (short-description (assoc-val 'short-description))
      (setf (short-description line-item) short-description))
    (when-let (long-description (assoc-val 'long-description))
      (setf (long-description line-item) long-description))
    (when-let (packing-weight (assoc-val 'packing-weight))
      (setf (packing-weight line-item) packing-weight))
    (if-let (published (assoc-val 'published))
      (setf (published line-item) t)
      (setf (published line-item) nil))
    (if-let (featured (assoc-val 'featured))
      (setf (featured line-item) t)
      (setf (featured line-item) nil)))) 

(defmethod validate ((type (eql 'single-item)) parameters)
  (multiple-value-bind (valid-generic errors-generic)
      (validate-generic-line-item-fields parameters)
    (multiple-value-bind (valid-single errors-single)
	(validate-single-item-fields parameters)
      (values (append valid-generic valid-single)
	      (append errors-generic errors-single)))))

(defmethod validate ((type (eql 'bundle)) parameters)
  (validate-generic-line-item-fields parameters))

(defmethod validate ((item single-item) parameters)
  (validate 'single-item parameters))

(defmethod validate ((item bundle) parameters)
  (validate 'bundle parameters))

(defun validate-generic-line-item-fields (alist)
  "Tries to validate alist, returns two values, an alist with
validated values, and a simple list of invalid fields from the first
list"
  (flet ((assoc-val (val) (cdr (assoc val alist))))
    (let ((errors '())
	  (valid '()))
      (if-let (title (validate-as-string (assoc-val 'title)))
	(push (cons 'title title) valid)
	(push 'title errors))
      (if-let (picture (validate-as-image-upload (assoc-val 'picture)))
	(push (cons 'picture picture) valid)
	(push 'picture errors))
      (if-let (short-description (validate-as-string (assoc-val 'short-description) t))
	(push (cons 'short-description short-description) valid)
	(push 'short-description errors))
      (if-let (long-description (validate-as-string (assoc-val 'long-description) t))
	(push (cons 'long-description long-description) valid)
	(push 'long-description errors))
      (if-let (packing-weight (validate-number (assoc-val 'packing-weight)))
	(push (cons 'packing-weight packing-weight) valid)
	(push 'packing-weight errors))
      (if (assoc-val 'published)
	  (push (cons 'published t) valid)
	  (push (cons 'published nil) valid))
      (if (assoc-val 'featured)
	  (push (cons 'featured t) valid)
	  (push (cons 'featured nil) valid))
      (values valid errors))))

(defun validate-single-item-fields (alist)
  (flet ((assoc-val (val) (cdr (assoc val alist))))
    (let ((valid '())
	  (errors '()))
      (if-let (weight (validate-number (assoc-val 'weight)))
	(push (cons 'weight weight) valid)
	(push 'weight errors))
      (if-let (price (validate-number (assoc-val 'price)))
	(push (cons 'price price) valid)
	(push 'price errors))
      (values valid errors))))

(defun validate-as-string (thing &optional allow-empty)
  (and (stringp thing)
       (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return #\Linefeed)
				   thing)))
	 (if (not allow-empty)
	     (unless (zerop (length trimmed))
	       trimmed)
	     trimmed))))

(defun validate-number (thing &optional allow-negative)
  (cond ((integerp thing) (if (minusp thing)
			      (when allow-negative
				thing)
			      thing))
	((stringp thing) (when-let (parsed (parse-integer thing :junk-allowed t))
			   (validate-number parsed)))
	(t nil)))

(defun validate-as-image-upload (thing)
  (when (and (listp thing)
	     (= (length thing) 3)
	     (>= (length (third thing)) 5)
	     (string-equal (subseq (third thing) 0 5) "image"))
    thing))

