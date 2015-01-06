;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-

;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)


(defun get-config-file ()
  (make-pathname :name "config" :type "sexp" :defaults (base-path *web-store*)))

(defun get-config-option (option)
  (let ((filespec (get-config-file)))
    (unless (probe-file filespec)
      (error "Missing configuration file: ~A." (namestring filespec)))
    (with-open-file (config filespec)
      (cdr (assoc option (read config))))))

(defun create-config (pathname)
  (with-open-file (f pathname :direction :output :if-exists :supersede)
    (write *default-configuration-options* :stream f :pretty t :right-margin 60 :case :downcase)))

(defparameter *default-configuration-options*
  '((:thumbnail-width . 300)
    (:thumbnail-height . 300)
    (:display-width . 500)
    (:display-height . 500)
    (:small-width . 100)
    (:small-height . 100)))

(defmethod make-designator ()
  (uuid:print-bytes nil (uuid:make-v4-uuid)))


(defmethod initialize-instance :before ((cms cms) &rest args)
  (declare (ignore args))
  (unless *web-store*
    (error "No store is present, so it is meaningless to add CMS objects"))
  cms)

(defmethod initialize-instance :after ((cms cms) &rest args)
  (declare (ignore args))
  (setf (store cms)
	(store-name *web-store*)
	(ele:get-value (get-designator cms) (store-objects *web-store*))
	cms)
  cms)

(defmethod get-object-by-designator ((designator string))
  (ele:get-value designator (store-objects *web-store*)))

(defmethod get-featured (class-name)
  (remove-if-not #'featured (get-all-objects class-name)))

(defmethod display-index-page ((store web-store))
  (display-index-page (store-type store)))

(defmethod main-navigation-tabs ((store web-store))
  (main-navigation-tabs (store-type store)))

(defmethod get-edit-tabs ((store web-store))
  (get-edit-tabs (store-type store)))

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

(defun initialize-store (directory)
  (let* ((store-path (make-store-directory-hierarchy directory))
	 (store (ele:open-store (list :bdb (namestring store-path)))))
    store))

(defun create-web-store-object (sku order name regex image-path files-path xml-path audit-path base-path)
  (make-instance 'web-store
		 :sku-prefix sku
		 :order-prefix order
		 :store-name name
		 :store-regex regex
		 :image-path image-path
		 :files-path files-path
		 :xml-path xml-path
		 :audit-path audit-path
		 :base-path base-path))

(defun new-web-store (shopper-sites store-name regex sku-prefix order-prefix)
  (let* ((base-path (dirconcat (database-root shopper-sites) store-name)))
    (logger :debug "NEW-WEB-STORE - Base path is: ~S" base-path)
    (flet ((path-append (subdir)
	     (make-pathname :directory (append (pathname-directory base-path)
					       (list subdir)))))

    (logger :debug "NEW-WEB-STORE - Making store directory hierarchy")
    (make-store-directory-hierarchy base-path)
    (logger :debug "NEW-WEB-STORE - Making store object")
    (let ((new-store (create-web-store-object sku-prefix order-prefix regex store-name
					      (path-append "images") (path-append "files")
					      (path-append "xml") (path-append "audit")
					      base-path)))
      (logger :debug "NEW-WEB-STORE - Adding new store to sites")
      (push new-store (get-stores shopper-sites))
      (logger :debug "NEW-WEB-STORE - Creating configuration file")
      (create-config (make-pathname :defaults base-path
				    :name "config"
				    :type "sexp"))
      (logger :debug "NEW-WEB-STORE - Setting dispatch table")
      (set-dispatch-table new-store)
      new-store))))

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
  #p"/home/ocorrain/dovinia/files/bootstrap/")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level object API
(defgeneric get-object (class identifier))
(defgeneric get-all-objects (class))

(defgeneric get-identifier (obj))

(defgeneric render-object (object))

(defgeneric edit-object (item))
;(defgeneric edit-object/post (item page))

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
(defmethod edit-object ((obj cms))
  (when-let (params (fix-alist (hunchentoot:post-parameters*)))
    (maybe-update obj params))
  
  (with-html-output-to-string (s)
    (:div :class "container" 
	  (str (get-form obj)))))

(defmethod edit-object ((obj line-item))
  (when-let (image-to-delete (hunchentoot:parameter "unrelateimage"))
    (when-let (image (get-object-by-designator image-to-delete))
      (unrelate obj image :image)))
  (when-let (params (hunchentoot:post-parameters*))
    (maybe-update obj (fix-alist params)))
  (grid (6 (get-form obj) 6 (grid (6 (image-edit-page obj))
				  (3 (tag-cloud obj) 3 (geo-cloud obj))))))


;(grid ((6 . (get-form obj))))

(defmethod get-edit-url ((obj cms))
  (get-edit-page-url obj :edit))

(defmethod get-edit-page-url ((obj cms) page-symbol)
  (format nil "/edit/~A/~A" 
	  (string-downcase (symbol-name (type-of obj)))
	  (get-identifier obj)))

  ;; (restas:genurl 'shopper-edit:edit-object-page
  ;; 		 :class  
  ;; 		 :identifier 
  ;; 		 :page (string-downcase (symbol-name page-symbol))))

;; (defmethod get-delete-url ((obj cms))
;;   (restas:genurl 'shopper-edit:delete-object-page
;; 		 :class (string-downcase (symbol-name (type-of obj)))
;; 		 :identifier (get-identifier obj)))

(defmethod get-new-url (class-symbol)
  (let ((class-name (symbol-name class-symbol)))
    (when (subtypep (symbolicate class-name) 'cms)
      (format nil "/new/~A" (string-downcase class-name)))))

;; (defmethod get-multi-edit-url (class-symbol)
;;   (let ((class-name (symbol-name class-symbol)))
;;     (if (subtypep (symbolicate class-name) 'cms)
;; 	(restas:genurl 'shopper-edit:edit-multi-page
;; 		       :class (string-downcase class-name)))))

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
