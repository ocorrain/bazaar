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
   (members :initarg :members :initform (ele:make-pset) :accessor tag-members
	    :documentation "Pset of items tagged with this tag")
   (appears-in-menu :initarg :appears-in-menu :initform nil :accessor appears-in-menu)))

(defmethod initialize-instance :after ((instance tag) &rest stuff)
  (declare (ignore stuff))
  (setf (webform instance) (get-webform (tag-name instance))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level API

(defmethod get-object ((tag (eql :tag)) webform)
  (get-tag webform))

(defmethod get-all-objects ((tag (eql :tag)))
  (all-tags))

(defmethod get-identifier ((tag tag))
  (webform tag))

(defmethod render-object ((tag tag))
  (with-html-output-to-string (s)
    (when (and (description tag) (not (zerop (length (description tag)))))
      (htm ((:div :class "well") (str (description tag)))))
    (when-let (thumbs (remove-if-not #'published
				     (ele:pset-list (tag-members tag))))
      (str (thumbnails thumbs #'render-thumb)))))

(defmethod edit-object/post ((tag tag) (page (eql :edit)))
  (maybe-update tag (fix-alist (hunchentoot:post-parameters*)))
  (edit-object tag :edit))

(defmethod delete-object ((obj tag))
  (dolist (item (ele:pset-list (tag-members obj)))
	(untag-item item obj))
  (ele:drop-instance obj))

(defmethod get-form ((tag (eql :tag)))
  (tag-form))

(defmethod get-form ((tag tag))
  (tag-form tag))


(defmethod get-edit-tabs ((tag tag))
  '(:view :edit))

(defmethod edit-multiple-objects ((tag (eql :tag)) objs)
  (make-page (labelise tag)
	     (thumbnails objs (lambda (tag) (render-thumb tag t)))
	     :sidebar (edit-bar tag)))

(defmethod view-object ((obj tag))
  (tag-display-page obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod tag-item ((item tags-mixin) (tag tag))
  (ele:with-transaction ()
    (ele:insert-item item (tag-members tag))
    (ele:insert-item tag (tags item))))


(defmethod untag-item ((item tags-mixin) (tag tag))
  (ele:remove-item item (tag-members tag))
  (ele:remove-item tag (tags item)))

(defun tagged? (item tag)
  (ele:find-item tag (tags item)))

(defun empty-tag (tag)
  (null (ele:pset-list (tag-members tag))))

(defun all-tags ()
  (ele:get-instances-by-class 'tag))

(defun tags-with-members ()
  (remove-if #'empty-tag (all-tags)))

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
      (when (all-tags)
	(htm (fmt "Select from the following tags:")
	     (:br))
	(dolist (tag (all-tags))
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

(defun get-tag-linked-list (item stream)
  (with-slots (tags) item
    (list-of-tags (ele:pset-list tags) stream)))

(defun list-of-tags (tags stream)
  (with-html-output (s stream)
    (:ul
     (dolist (tag tags)
       (htm (:li ((:a :href (get-tag-url tag)) (str (tag-name tag)))))))))

;; (defun get-tag-url (tag)
;;   (url-rewrite:add-get-param-to-url "/display-tag" "name" (webform tag)))

;; (defmethod get-view-url ((tag tag))
;;   (restas:genurl 'r/view-tag :tag (webform tag)))

(defun get-tagged-items (tag)
  (ele:pset-list (tag-members tag)))

(defmethod get-tags ((item tags-mixin))
  (ele:pset-list (tags item)))


(defmethod get-edit-view-url ((tag tag))
  (restas:genurl 'shopper-edit:r/edit-tag/view :tag (webform tag)))

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
		 (all-tags)))

(defun tag->nav (list-of-tags)
  (mapcar (lambda (tag)
	    (cons (get-view-url tag)
		  (tag-name tag)))
	  list-of-tags))

(defmethod display-an-image ((tag tag) &optional (image-func #'get-thumb-url))
  (when-let (member-list (ele:pset-list (tag-members tag)))
    (when-let (images (mappend #'images member-list))
      (with-html-output-to-string (s)
	(:img :src (funcall image-func (random-elt images)))))))




;; objects that have tags associated with them
(defmethod edit-object ((obj tags-mixin) (page (eql :tags)))
  (when-let (toggle-tag (hunchentoot:get-parameter "tag"))
    (when-let (toggle-tag-obj (get-tag toggle-tag))
      (if (tagged? obj toggle-tag-obj)
	  (untag-item obj toggle-tag-obj)
	  (tag-item obj toggle-tag-obj))))
  (make-tags-page obj))

(defmethod delete-object :before ((obj tags-mixin))
  (dolist (tag (ele:pset-list (tags obj)))
    (untag-item obj tag)))
