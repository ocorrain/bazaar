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
   (open :initarg :open :initform nil :accessor store-open)
   (store-type :initarg :store-type :initform :web-store :accessor store-type)
   (store-active-classes :initarg :store-active-classes
			 :initform '(:line-item :tag :geography :static-content)
			 :accessor store-active-classes)))

(ele:defpclass cms ()
  ((featured :initarg :featured :initform nil :accessor featured
	     :type boolean :index t
	     :documentation "Is this to be published to the front-page
	     / featured page?")
   (published :initarg :published :initform nil
	      :accessor published :index t
	      :documentation "Is this to be published to the site?"
	      :type boolean)))

(ele:defpclass images-mixin ()
  ((images :initform '() :accessor images
	   :documentation "List of images of this item"
	   :type list)
   (image-counter :initform 0 :accessor image-counter
		  :documentation "counter for image filenames"
		  :type number)))

(ele:defpclass tags-mixin ()
  ((tags :initarg :tags :initform (ele:make-pset)
	 :accessor tags
	 :documentation "A list of categories or tags into which
	       this item falls")))

(defmethod get-featured (class-name)
  (remove-if-not #'featured (get-all-objects class-name)))

(defmethod display-index-page ((store web-store))
  (display-index-page (store-type store)))

(defmethod main-navigation-tabs ((store web-store))
  (main-navigation-tabs (store-type store)))

(defmethod get-edit-tabs ((store web-store))
  (get-edit-tabs (store-type store)))

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
    (dolist (path (list "store" "images" "files" "xml" "audit" "log"))
      (ensure-directories-exist (dirconcat base-path path)
				:verbose t))
    (dirconcat base-path "store")))

(defmethod install-bootstrap ((store web-store))
  "put css in place"
  (trivial-shell:shell-command
   (concatenate 'string
		"unzip "
		(namestring (merge-pathnames (pathname "bootstrap/bootstrap.zip")
					     (asdf:system-source-directory 'shopper)))
		" -d "
		(namestring (files-path *web-store*)))))

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
    (install-bootstrap *web-store*)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level object API
(defgeneric get-object (class identifier))
(defgeneric get-all-objects (class))

(defgeneric get-identifier (obj))

(defgeneric render-object (object))

(defgeneric edit-object (item page))
(defgeneric edit-object/post (item page))

(defgeneric get-edit-url (obj))
(defgeneric get-edit-page-url (obj page-symbol))

(defgeneric get-edit-tabs (obj))

(defgeneric get-multi-edit-url (class-symbol))
(defgeneric get-multi-edit-page-url (obj page-symbol))
(defgeneric edit-multiple-objects (class-symbol objs))
(defgeneric edit-multiple-objects-page (class-symbol page-symbol))
(defgeneric get-delete-url (obj))
(defgeneric delete-object (obj))

(defgeneric get-new-url (class-symbol))

(defgeneric get-view-url (obj))

(defgeneric get-form (obj-or-class-symbol))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Default implementations
(defmethod edit-object ((obj cms) (page (eql :edit)))
  (get-form obj))

(defmethod edit-object ((obj cms) (page (eql :view)))
  (view-object obj))

(defmethod get-edit-url ((obj cms))
  (get-edit-page-url obj :edit))

(defmethod get-edit-page-url ((obj cms) page-symbol)
  (restas:genurl 'shopper-edit:edit-object-page
		 :class (string-downcase (symbol-name (type-of obj))) 
		 :identifier (get-identifier obj)
		 :page (string-downcase (symbol-name page-symbol))))

(defmethod get-delete-url ((obj cms))
  (restas:genurl 'shopper-edit:delete-object-page
		 :class (string-downcase (symbol-name (type-of obj)))
		 :identifier (get-identifier obj)))

(defmethod get-new-url (class-symbol)
  (let ((class-name (symbol-name class-symbol)))
    (if (subtypep (symbolicate class-name) 'cms)
	(restas:genurl 'shopper-edit:new-object-page
		       :class (string-downcase class-name)))))

(defmethod get-multi-edit-url (class-symbol)
  (let ((class-name (symbol-name class-symbol)))
    (if (subtypep (symbolicate class-name) 'cms)
	(restas:genurl 'shopper-edit:edit-multi-page
		       :class (string-downcase class-name)))))

(defmethod delete-object ((obj cms))
  (ele:drop-instance obj))

(defmethod render-object ((obj cms))
  (with-html-output-to-string (s)
    (:pre (describe obj s))))

(defun labelise (symbol)
  (string-capitalize
   (substitute #\Space #\-
	       (string-downcase (symbol-name symbol)))
   :end 1))


(defmethod render-edit-tabs ((obj cms) active)
  (nav-tabs (mapcar (lambda (tab)
		      (cons
		       (get-edit-page-url obj tab)
		       (labelise tab)))
		    (get-edit-tabs obj))
	    (labelise active)))

(defmethod get-view-url ((obj cms))
  (format nil "/view/~A/~A" (string-downcase (symbol-name (type-of obj))) (get-identifier obj))
  ;; (restas:genurl 'view-object-page 
  ;; 		 :class (string-downcase (symbol-name (type-of obj)))
  ;; 		 :identifier (get-identifier obj))
  )

(defmethod get-all-published-objects (class-name)
  (remove-if-not #'published (get-all-objects class-name)))
