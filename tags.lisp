;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

;; tags will be implemented as persistent objects with a persistent
;; set of members.  Circularly, line items contain a list of tags that
;; are associated with them

(ele:defpclass tag (cms)
  ((name :initarg :name :initform "" :accessor tag-name :index t
	 :documentation "Tag name" :type string)
   (description :initarg :description :initform "" :accessor description)
   (webform :accessor webform :index t :documentation "Web safe form
   of the tag name for transmission" :type string)
   (appears-in-menu :initarg :appears-in-menu :initform nil :accessor appears-in-menu)))

(defmethod initialize-instance :after ((instance tag) &rest stuff)
  (declare (ignore stuff))
  (setf (webform instance) (get-webform (tag-name instance))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level API

(defmethod get-object ((tag (eql :tag)) webform)
  (get-tag webform))

(defmethod get-all-objects ((tag (eql :tag)))
  (ele:get-instances-by-value 'tag 'store (store-name *web-store*)))

(defmethod get-identifier ((tag tag))
  (webform tag))

(defmethod render-object ((tag tag))
  (with-html-output-to-string (s)
    (when (and (description tag) (not (zerop (length (description tag)))))
      (htm ((:div :class "well") (str (description tag)))))
    (when-let (thumbs (remove-if-not #'published (get-tagged-objects tag)))
      (str (thumbnails thumbs #'render-thumb)))))


(defmethod title ((tag tag))
  (tag-name tag))

(defmethod get-form ((tag (eql :tag)))
  (tag-form))

(defmethod get-form ((tag tag))
  (tag-form tag))


(defmethod get-edit-tabs ((tag tag))
  '(:view :edit))


(defmethod view-object ((obj tag))
  (tag-display-page obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod tag-item ((item cms) (tag tag))
  (relate item tag :tag))


(defmethod untag-item ((item cms) (tag tag))
  (unrelate item tag :tag))

(defun tagged? (item tag)
  (relation-exists item tag :tag))

(defun empty-tag (tag)
  (not (role-exists tag :tag)))

(defun tags-with-members ()
  (remove-if #'empty-tag (get-all-objects :tag)))

(defun menu-tags ()
  (remove-if-not (lambda (tag)
		   (and (appears-in-menu tag)
			(not (featured tag))))
		 (tags-with-members)))

(defun featured-tags ()
  (remove-if-not #'featured (tags-with-members)))

(defun get-tag (webform)
  (ele:get-instance-by-value 'tag 'webform webform))

(defun get-webform (tag-title)
  (string-downcase (remove-if-not #'alphanumericp tag-title)))

(defun tag-widget-printer (item stream)
  ;; FIXME, this is broken somehow
  (with-html-output (s stream)
    ((:div :id "tags")
     ((:form :action "/tags" :method "post")
      (when (get-all-objects :tag)
	(htm (fmt "Select from the following tags:")
	     (:br))
	(dolist (tag (get-all-objects :tag))
	  (htm ((:label :for (webform tag)) (esc (tag-name tag)))
	       (:input :id (webform tag)
		       :name (format nil "tags{~A}" (webform tag))
		       :type "checkbox" :checked (tagged? item tag))
	       (:br))))
      (:br)
      ((:label :for "newtag") "Create a new tag and tag this item with it")
      (:input :type "text" :id "newtag" :name "newtag")
      (:input :type "hidden" :name "sku" :value (sku item))
      (:br)
      (:input :type "submit" :value "tag")))))

;; (defun get-tag-url (tag)
;;   (url-rewrite:add-get-param-to-url "/display-tag" "name" (webform tag)))

;; (defmethod get-view-url ((tag tag))
;;   (restas:genurl 'r/view-tag :tag (webform tag)))

(defmethod get-tagged-objects ((tag tag))
  (get-related-to-objects tag :tag))

(defmethod get-tags ((item cms))
  (get-related-objects item :tag))


;; (defmethod get-edit-view-url ((tag tag))
;;   (restas:genurl 'shopper-edit:r/edit-tag/view :tag (webform tag)))

;; (defmethod get-edit-edit-url ((tag tag))
;;   (restas:genurl 'shopper-edit:r/edit-tag/edit :tag (webform tag)))

;; (defmethod get-delete-url ((obj line-item))
;;   (restas:genurl 'shopper-edit:r/delete-item :sku (sku obj)))

(defun render-tags (list-of-tags)
  (with-html-output-to-string (s)
    (dolist (tag list-of-tags)
      (if (appears-in-menu tag)
	  (if (featured tag)
	      (htm ((:a :href (url-rewrite:add-get-param-to-url
			       (hunchentoot:script-name*) "tag" (webform tag))
			:class "btn btn-small btn-success")
		    (str (tag-name tag))))
	      (htm ((:a :href (url-rewrite:add-get-param-to-url
			       (hunchentoot:script-name*) "tag" (webform tag))
			:class "btn btn-small btn-primary")
		    (str (tag-name tag)))))
	  (htm ((:span :class "btn btn-small")
		((:a :href (url-rewrite:add-get-param-to-url
			    (hunchentoot:script-name*) "tag" (webform tag)))
		 (str (tag-name tag)))))))))

(defun collect-tags-with (func)
  (remove-if-not (lambda (tag)
		   (funcall func tag))
		 (get-all-objects :tag)))

(defun tag->nav (list-of-tags)
  (mapcar (lambda (tag)
	    (cons (get-view-url tag)
		  (tag-name tag)))
	  list-of-tags))


;;fixme
(defmethod display-an-image ((tag tag) &optional (image-func (thumbnail-element 300 300)))
  (when-let (images (mappend (lambda (m)
			       (get-related-objects m :image))
			     (get-tagged-objects tag)))
    (with-html-output-to-string (s)
      (:img :src (funcall image-func (random-elt images))))))




;; objects that have tags associated with them
;; REFACTOR
;; (defmethod edit-object ((obj cms) (page (eql :tags)))
;;   (when-let (toggle-tag (hunchentoot:get-parameter "tag"))
;;     (when-let (toggle-tag-obj (get-tag toggle-tag))
;;       (if (tagged? obj toggle-tag-obj)
;; 	  (untag-item obj toggle-tag-obj)
;; 	  (tag-item obj toggle-tag-obj))))
;;   (make-tags-page obj))

(defmethod delete-object :before ((obj cms))
  (dolist (from (get-relations-from obj))
    (let ((role (car from))
	  (objects (mapcar #'car (cdr from))))
      (dolist (object objects)
	(unrelate obj object role))))
  (dolist (to (get-relations-to obj))
    (let ((role (car to))
	  (objects (mapcar #'car (cdr to))))
      (dolist (object objects)
	(unrelate object obj role)))))

(defun toggle-tag (obj tag)
  (if (tagged? obj tag)
      (untag-item obj tag)
      (tag-item obj tag)))
