;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(defvar *web-store* nil "Variable to hold a reference to the default web store")


(defun get-config-file ()
  (make-pathname :defaults (base-path *web-store*)
		 :name "config"
		 :type "sexp"))

(defun get-config-option (option)
  (let ((filespec (get-config-file)))
    (unless (probe-file filespec)
      (error "Missing configuration file: ~A." (namestring filespec)))
    (with-open-file (config filespec)
      (cdr (assoc option (read config))))))

(defun create-config ()
  (with-open-file (f (get-config-file) :direction :output :if-exists :supersede)
    (write *default-configuration-options* :stream f :pretty t :right-margin 60 :case :downcase)))

(defparameter *default-configuration-options*
  '((:thumbnail-width . 300)
    (:thumbnail-height . 300)
    (:display-width . 500)
    (:display-height . 500)
    (:small-width . 100)
    (:small-height . 100)))

(ele:defpclass web-store ()
  ((sku-prefix :initarg :sku-prefix :accessor sku-prefix)
   (sku-counter :initform 1 :accessor sku-counter)
   (order-prefix :initarg :order-prefix :accessor order-prefix)
   (order-counter :initform 1 :accessor order-counter)
   (store-name :initarg :store-name :accessor store-name)
   (store-customers :initarg :store-customers :accessor store-customers
		    :initform (ele:make-btree))
   (item-btree :initform (ele:add-to-root 'items (ele:make-btree)) :accessor items)
   (acceptor :initform nil :transient t :accessor acceptor)
   (image-path :initarg :image-path
	       :initform #p"" :accessor image-path :type pathname)
   (files-path :initarg :files-path
	       :initform #p"" :accessor files-path :type pathname)
   (xml-path   :initarg :xml-path
	       :initform #p"" :accessor xml-path :type pathname)
   (audit-path :initarg :audit-path
	       :initform #p"" :accessor audit-path :type pathname)
   (base-path :initarg :base-path :accessor base-path :type pathname)
   (open :initarg :open :initform nil :accessor store-open)))

(defun open-web-store (dir)
  (ele:open-store (list :bdb (dirconcat dir "store")))
  (setf *web-store* (ele:get-from-root 'web-store)))

(defun ensure-pathname-directory (string)
  (if (pathnamep string)
      (ensure-pathname-directory (namestring string))
      (if (char-equal (char string (1- (length string))) #\/)
	  (pathname string)
	  (pathname (format nil "~A/" string)))))


(defun dirconcat (directory &rest subdirs)
  (make-pathname
   :directory (append (pathname-directory (ensure-pathname-directory directory))
		      subdirs)))


(defun make-store-directory-hierarchy (directory)
  (let ((base-path (ensure-pathname-directory directory)))
    (ensure-directories-exist base-path)
    (dolist (path (list "store" "images" "files" "xml" "audit"))
      (ensure-directories-exist (dirconcat base-path path)
				:verbose t))
    (dirconcat base-path "store")))

(defun new-web-store (store-name sku-prefix order-prefix directory)
  (let* ((base-path (ensure-pathname-directory directory))
	 (store-path (make-store-directory-hierarchy directory))
	 (store (ele:open-store (list :bdb (namestring store-path)))))
    (flet ((path-append (subdir)
	     (make-pathname :directory (append (pathname-directory base-path)
					       (list subdir)))))
      (ele:add-to-root 'web-store
		     (make-instance 'web-store
				    :sku-prefix sku-prefix
				    :order-prefix order-prefix
				    :store-name store-name
				    :image-path (path-append "images")
				    :files-path (path-append "files")
				    :xml-path (path-append "xml")
				    :audit-path (path-append "audit")
				    :base-path base-path)
		     :sc store))
    (setf *web-store* (ele:get-from-root 'web-store))
    (create-config)))

(defun get-next-sku (&optional (store *web-store*))
  (ele:with-transaction ()
      (prog1
	  (format nil "~A~7,'0d" (sku-prefix store) (sku-counter store))
	(incf (sku-counter store)))))

(defun close-web-store ()
  (ele:close-store)
  (setf *web-store* nil))

(defmethod initialize-instance :after ((item line-item) &rest stuff)
  (declare (ignore stuff))
  (setf (sku item) (get-next-sku *web-store*))
  (setf (ele:get-value (sku item) (items *web-store*))
	item))

(defun get-twitter-bootstrap-path ()
  (make-pathname :directory (append (pathname-directory (base-path *web-store*))
				    (list "files" "bootstrap"))))
