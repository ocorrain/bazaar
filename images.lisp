;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(ele:defpclass image (cms)
  ((file :initarg :file :initform "" :accessor get-file)
   (content-type :initarg :content-type :initform "" :accessor get-content-type)))

(defmethod get-object ((image (eql :image)) designator)
  (get-object-by-designator designator))

(defmethod get-all-objects ((image (eql :image)))
  (ele:get-instances-by-value 'image 'store (store-name *web-store*)))

(defmethod get-identifier ((image image))
  (get-designator image))

(defmethod make-thumbnail ((image image) box-x box-y &key (force nil))
  "High level method for making a thumbnail of an image.  Creates the
thumbnail file and relates it to the base image with
relationship :thumbnail."
  (if (or force (not (get-related-objects-by-info image
						  :thumbnail (list box-x box-y))))
      (with-slots (file) image
	(let* ((source-path (merge-pathnames file (image-path *web-store*)))
	       (designator (make-designator))
	       (image-type (pathname-type file))
	       (dest-path (make-pathname :name designator
					 :type image-type
					 :defaults (image-path *web-store*)))
	       (image-obj (make-instance 'image
					 :file (make-pathname :name designator
							      :type image-type)
					 :designator designator)))
	  (create-thumbnail source-path dest-path box-x box-y)
	  (relate image image-obj :thumbnail (list box-x box-y))
	  image-obj))))



(defun create-thumbnail (source-path dest-path box-x box-y
			 &optional (frame-color (list 0 0 0)))
  "Low level function to actually resize a source image and create a
thumbnail image.  Calls cl-gd."
  (cl-gd:with-image-from-file (img source-path)
    (multiple-value-bind (image-x image-y)
	(cl-gd:image-size img)
      (multiple-value-bind (new-x new-y)
	  (get-resize-dimensions image-x image-y box-x box-y)
	(cl-gd:with-image (new box-x box-y t)
	  (cl-gd:copy-image img new 0 0 
			    (floor (/ (- box-x new-x) 2))
			    (floor (/ (- box-y new-y) 2))
			    image-x image-y
			    :resize t :resample t
			    :dest-width new-x :dest-height new-y)
	  (cl-gd:write-image-to-file dest-path
				     :if-exists :supersede
				     :image new))
	(values new-x new-y)))))

(defun get-resize-dimensions (image-x image-y box-x box-y)
  (if (and (< image-x box-y)
	   (< image-y box-y))
      (values image-x image-y)
      (apply #'values
	     (mapcar #'round
		     (if (> (/ image-x image-y) (/ box-x box-y))
			 (list box-x (* image-y (/ box-x image-x)))
			 (list (* image-x (/ box-y image-y)) box-y))))))

(defmethod get-thumb-url ((image image))
  (concatenate 'string "/images/"
	       (namestring (get-thumb-path (get-file image)))))

(defmethod get-thumb-url ((image t))
  (get-thumb-url (get-object-by-designator image)))

(defmethod get-full-url ((image image))
  (concatenate 'string "/images/"
	       (namestring (get-full-size-path (get-file image)))))

(defmethod get-full-url ((image t))
  (get-full-url (get-object-by-designator image)))

(defmethod get-small-url ((image image))
  (concatenate 'string "/images/"
	       (namestring (get-small-size-path (get-file image)))))

(defmethod get-small-url ((image t))
  (get-small-url (get-object-by-designator image)))

(defun image-thumbnails (list render-func)
  (with-html-output-to-string (s)
    ((:ul :class "thumbnails")
     (dolist (obj list)
       (htm ((:li :class "span2")
	     (str (funcall render-func obj))))))))

(defmethod get-images ((cms cms))
  "Get images related to cms with relationship :image"
  (get-related-objects cms :image))

(defmethod display-an-image ((item cms) &optional (image-func (thumbnail-element 300 300)))
  (when-let (images (get-images item))
    (with-html-output-to-string (s)
      (str (funcall image-func (random-elt images))))))





(defun display-gallery (images id)
  (with-html-output-to-string (s)
    (lightbox-gallery s id)
    ((:div :id id)
     (:ul
      (dolist (i images)
	(htm (:li ((:a :href (get-full-url i))
		   (:img :src (get-thumb-url i))))))))))


(defun add-image (path original-filename line-item)
  (let* ((type (string-downcase (pathname-type original-filename)))
	 (stub (make-designator))
	 (image-file (make-pathname :name stub :type type))
	 (image-obj (make-instance 'image
				   :file image-file
				   :designator stub)))
    (cl-fad:copy-file path (merge-pathnames image-file (image-path *web-store*)))
    (make-thumbnail image-obj
		    (get-config-option :thumbnail-width)
		    (get-config-option :thumbnail-height))
    (make-thumbnail image-obj
		    (get-config-option :display-width)
		    (get-config-option :display-height))
    (make-thumbnail image-obj
		    (get-config-option :small-width)
		    (get-config-option :small-height))
    (relate line-item image-obj :image)))


;; Objects that have a gallery of images associates with them

;; (defmethod edit-object ((obj cms) (page (eql :images)))
;;   (when-let (image-to-delete (hunchentoot:get-parameter "delete"))
;;     (unrelate obj image-to-delete))
;;   (image-edit-page obj))

(defmethod get-images ((cms cms))
  (get-related-objects cms :image))

(defmethod delete-object :before ((obj image))
  (delete-file (merge-pathnames (get-file obj) (image-path *web-store*))))

(defmethod image-element ((obj image))
  (with-html-output-to-string (s)
    (:img :src (image-web-path obj))))

(defmethod image-web-path ((obj image))
  (format nil "/images/~A" (namestring (get-file obj))))
