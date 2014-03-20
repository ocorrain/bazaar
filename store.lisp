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






(defun make-designator ()
  (uuid:print-bytes nil (uuid:make-v4-uuid)))

(defmethod relation-exists ((from cms) (to cms) role)
  (let ((to-designator (get-designator to))
	(relations-from (get-relations-from from)))
    (when-let (role-data (assoc role relations-from))
      (member to-designator (cdr role-data) :key #'car :test #'equal))))

(defmethod role-exists ((cms cms) role)
  (when-let (role-data (assoc role (get-relations-to cms)))
    (not (null (cdr role-data)))))

(defmethod get-related-objects ((cms cms) role)
  (when-let ((role-data (assoc role (get-relations-from cms))))
    (mapcar (compose #'get-object-by-designator #'car) (cdr role-data))))

(defmethod get-related-to-objects ((cms cms) role)
  (when-let ((role-data (assoc role (get-relations-to cms))))
    (mapcar (compose #'get-object-by-designator #'car) (cdr role-data))))

(defmethod get-related-objects-by-info ((cms cms) role info)
  (mapcar (compose #'get-object-by-designator #'car)
	  (get-related-designators-by-info cms role info)))

(defmethod get-related-designators-by-info ((cms cms) role info)
  (when-let ((role-data (assoc role (get-relations-from cms))))
    (remove-if-not (lambda (data)
		     (equalp info (cdr data)))
		   (cdr role-data))))

(defmethod get-relation-data ((cms cms) role)
  (mapcar (lambda (e)
	    (cons (get-object-by-designator (car e))
		  (cdr e)))
	  (cdr (assoc role (get-relations-from cms)))))

(defmethod get-related-to-data ((cms cms) role)
  (mapcar (lambda (e)
	    (cons (get-object-by-designator (car e))
		  (cdr e)))
	  (cdr (assoc role (get-relations-to cms)))))

;; (defmethod relation-exists ((cms cms) role designator &key (accessor #'get-relations-from))
;;   (member designator (relation-designators cms role :accessor accessor) :test #'equal))

;; (defmethod relation-p ((cms cms) role &key (accessor #'get-relations-from))
;;   (assoc role (funcall accessor cms)))

(defmethod relation-designators ((cms cms) role)
  (when-let (role-data (assoc role (get-relations-from cms)))
    (mapcar #'car (cdr role-data))))


;; (defmethod relation-data ((cms cms) role &key (accessor #'get-relations-from))
;;   (when-let (relations (relation-p cms role :accessor accessor))
;;     (cdr relations)))

(defmethod construct-relation (designator &optional info)
  (append (list designator) (ensure-list info)))

;; (defmethod add-relation ((cms cms) role designator &key info (accessor #'get-relations-from))
;;   (let ((relation-to-add (construct-relation designator info)))
;;     (if (relation-p cms role :accessor accessor)
;; 	(let ((relation-data (relation-data cms role :accessor accessor)))
;; 	  (add-new-relation-data (cons relation-to-add
;; 				       (remove designator relation-data :key #'car :test #'equal))
;; 				 cms role))
;; 	(add-new-relation-data (list relation-to-add) cms role))))

(defun merge-relation (role designator info existing-relations)
  (if-let (existing-role (assoc role existing-relations))
    (let ((relation-data (cdr existing-role)))
      (cons (cons role (cons (construct-relation designator info)
			     (remove designator relation-data :key #'car :test #'equal)))
	    (remove role existing-relations :key #'car)))
    (cons (cons role (list (construct-relation designator info)))
	  (remove role existing-relations :key #'car))))

;; (defmethod new-relation ((cms cms) role designator &key info (accessor #'get-relations-from))
;;   (let ((relation-to-add (construct-relation designator info)))
;;     (if (relation-p cms role :accessor accessor)
;; 	(let ((relation-data (relation-data cms role :accessor accessor)))
;; 	  (add-new-relation-data (cons relation-to-add
;; 				       (remove designator relation-data :key #'car :test #'equal))
;; 				 cms role))
;; 	(add-new-relation-data (list relation-to-add) cms role))))

(defun remove-relation (role designator existing-relations)
  (let ((role-data (assoc role existing-relations)))
    (cons (cons role (remove designator (cdr role-data) :key #'car :test #'equal))
	  (remove role existing-relations :key #'car))))

(defun remove-role (role existing-relations)
  (remove role existing-relations :key #'car))

(defmethod relate ((from cms) (to cms) role &optional info)
  (setf (get-relations-from from) (merge-relation role (get-designator to) info (get-relations-from from))
	(get-relations-to to) (merge-relation role (get-designator from) info (get-relations-to to))))

(defmethod relate ((from t) (to t) role &optional info)
  (when-let ((from-obj (get-object-by-designator from))
	     (to-obj (get-object-by-designator to)))
    (relate from-obj to-obj role info)))


(defmethod relate ((from cms) (to t) role &optional info)
  (when-let ((obj (get-object-by-designator to)))
    (relate from obj role info)))


(defmethod relate ((from t) (to cms) role &optional info)
  (when-let (obj (get-object-by-designator from))
    (relate obj to role info)))


(defmethod unrelate ((from cms) (to cms) role)
  (setf (get-relations-from from) (remove-relation role (get-designator to) (get-relations-from from))
	(get-relations-to to) (remove-relation role (get-designator from) (get-relations-to to))))

(defmethod unrelate ((from t) (to t) role)
  (when-let ((fromo (get-object-by-designator from))
	     (too (get-object-by-designator to)))
    (unrelate fromo too role)))

(defmethod unrelate ((from cms) (to t) role)
  (when-let (obj (get-object-by-designator to))
    (unrelate from  obj role)))

(defmethod unrelate ((from t) (to cms) role)
  (when-let (obj (get-object-by-designator from))
    (unrelate obj to role)))




;; (defmethod add-new-relation-data (data (cms cms) role &key (accessor #'get-relations-from))
;;   (let ((relations (funcall accessor cms)))
;;     (setf (funcall accessor cms)
;; 	  (cons (cons role data)
;; 		(remove role relations :key #'car)))))




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

(defun get-object-by-designator (designator)
  (ele:get-value designator (store-objects *web-store*)))

;; (ele:defpclass images-mixin ()
;;   ((images :initform '() :accessor images
;; 	   :documentation "List of images of this item"
;; 	   :type list)
;;    (image-counter :initform 0 :accessor image-counter
;; 		  :documentation "counter for image filenames"
;; 		  :type number)))

;; (ele:defpclass tags-mixin ()
;;   ((tags :initarg :tags :initform (ele:make-pset)
;; 	 :accessor tags
;; 	 :documentation "A list of categories or tags into which
;; 	       this item falls")))

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

(defun initialize-store (directory)
  (let* ((store-path (make-store-directory-hierarchy directory))
	 (store (ele:open-store (list :bdb (namestring store-path)))))
    store))

(defun new-web-store (shopper-sites store-name regex sku-prefix order-prefix)
  (let* ((base-path (dirconcat (database-root shopper-sites) store-name)))
    (flet ((path-append (subdir)
	     (make-pathname :directory (append (pathname-directory base-path)
					       (list subdir)))))
      (make-store-directory-hierarchy base-path)
      (let ((new-store (make-instance 'web-store
				      :sku-prefix sku-prefix
				      :order-prefix order-prefix
				      :store-name store-name
				      :store-regex regex
				      :image-path (path-append "images")
				      :files-path (path-append "files")
				      :xml-path (path-append "xml")
				      :audit-path (path-append "audit")
				      :base-path base-path)))
	(push new-store (get-stores shopper-sites))
	(create-config (make-pathname :defaults base-path
				  :name "config"
				  :type "sexp"))
	(set-dispatch-table new-store)
	new-store))))


;; (defun create-web-store (store-name sku-prefix order-prefix directory)
;;   (flet ((path-append (subdir)
;; 	   (make-pathname :directory (append (pathname-directory base-path)
;; 					     (list subdir)))))
;;     (ele:add-to-root 'web-store
;; 		     (make-instance 'web-store
;; 				    :sku-prefix sku-prefix
;; 				    :order-prefix order-prefix
;; 				    :store-name store-name
;; 				    :image-path (path-append "images")
;; 				    :files-path (path-append "files")
;; 				    :xml-path (path-append "xml")
;; 				    :audit-path (path-append "audit")
;; 				    :base-path base-path)
;; 		     :sc store)))

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
  (when-let (toggle-tag (hunchentoot:parameter "tag"))
    (let ((tag (get-object :tag toggle-tag)))
      (if (tagged? obj tag)
	  (untag-item obj tag)
	  (tag-item obj tag))))
  (with-html-output-to-string (s)
    (:div :class "container"
	  (:div :class "row"
		(:div :class "span6"
		      (str (get-form obj)))
		(:div :class "span6"
		      (str (image-edit-page obj))
		      (str (tag-cloud obj))));;  (:div :id "editContent" :class "tab-content"
		;; (:div :class "tab-pane fade in active" :id "item"
		;;       (str (get-form obj)))
		;; (:div :class "tab-pane fade" :id "images"
		;;       (str (image-edit-page obj)))
		;; (:div :class "tab-pane fade" :id "tags"
		;;       (str (tags-widget obj))))
		      )))

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
