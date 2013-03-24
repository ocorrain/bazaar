;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

;; (defun create-thumbnail (filename thumbname width height)
;;   "Create a thumbnail the image in FILENAME with a max size of WIDTH x HEIGHT
;; pixel (but with the original aspect ratio) and save it in THUMBNAME."
;;   (if (or (pathnamep filename)
;; 	  (pathnamep thumbname))
;;       (create-thumbnail (namestring filename) (namestring thumbname) width height)
;;       (lisp-magick:with-magick-wand (wand :load filename)
;; 	(let ((a (/ (lisp-magick:magick-get-image-width wand)
;; 		    (lisp-magick:magick-get-image-height wand))))
;; 	  (if (> a (/ width height))
;; 	      (lisp-magick:magick-scale-image wand width (truncate (/ width a)))
;; 	      (lisp-magick:magick-scale-image wand (truncate (* a height)) height)))
;; 	(lisp-magick:magick-write-image wand thumbname))))

;; (defun create-thumbnail (source-path dest-path box-x box-y &optional (frame-color (list 0 0 0)))
;;   (cl-gd:with-image-from-file (img source-path)
;;     (multiple-value-bind (image-x image-y)
;; 	(cl-gd:image-size img)
;;       (multiple-value-bind (new-x new-y)
;; 	  (get-resize-dimensions image-x image-y box-x box-y)
;; 	(cl-gd:with-image (new new-x new-y t)
;; 	  (cl-gd:copy-image img new 0 0 0 0 image-x image-y
;; 			    :resize t :resample t
;; 			    :dest-width new-x :dest-height new-y)
;; 	  (cl-gd:write-image-to-file dest-path
;; 				     :if-exists :supersede
;; 				     :image new))
;; 	(values new-x new-y)))))


(defun create-thumbnail (source-path dest-path box-x box-y &optional (frame-color (list 0 0 0)))
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



;; (defun create-thumbnail (filename thumbname width height &optional (frame-color (list 0 0 0)))
;;   "Create a thumbnail the image in FILENAME with a max size of WIDTH x HEIGHT
;; pixel (but with the original aspect ratio) and save it in THUMBNAME."
;;   (if (or (pathnamep filename)
;; 	  (pathnamep thumbname))
;;       (create-thumbnail (namestring filename) (namestring thumbname) width height)
;;       (lisp-magick:with-magick-wand (wand :load filename)
;; 	(lisp-magick:with-pixel-wand (pwand :comp (255 255 255))
;; 	  (multiple-value-bind (new-width new-height padding-width padding-height)
;; 	      (get-thumbnail-dimensions (lisp-magick:magick-get-image-width wand)
;; 					(lisp-magick:magick-get-image-height wand)
;; 					width height)
;; 	    (lisp-magick:magick-scale-image wand new-width new-height)
;; 	    (lisp-magick:magick-frame-image wand pwand 
;; 	    				    padding-width
;; 	    				    padding-height
;; 	    				    0 0))
	  
;; 	  (lisp-magick:magick-write-image wand thumbname)))))


;; (defun get-thumbnail-dimensions (width height box-width box-height)
;;   (let ((image-aspect (/ width height))
;; 	(box-aspect (/ box-width box-height)))
;; ;    (format t "Image aspect: ~A ; box aspect: ~A" image-aspect box-aspect)
;;     (let* ((new-width (if (> image-aspect box-aspect)
;; 			  box-width (truncate (* box-height image-aspect))))
;; 	   (new-height (if (> image-aspect box-aspect)
;; 			   (truncate (/ box-width image-aspect)) box-height))
	   
;; 	   (padding-width (truncate (/ (- box-width new-width) 2)))
;; 	   (padding-height (truncate (/ (- box-height new-height) 2))))
;;       (values new-width new-height padding-width padding-height))))

(defun get-thumb-url (path)
  (concatenate 'string "/images/"
	       (namestring (get-thumb-path path))))

(defun get-full-url (path)
  (concatenate 'string "/images/"
	       (namestring (get-full-size-path path))))

(defun get-small-url (path)
  (concatenate 'string "/images/"
	       (namestring (get-small-size-path path))))

(defun resize-all-images (class &optional (frame-color '(255 255 255)))
  (dolist (item (get-all-objects class))
    (when-let (images (images item))
      (dolist (i images)
	(let* ((dest-path (make-pathname
			   :name (pathname-name i) :type (pathname-type i)
			   :defaults (image-path *web-store*)))
	       (thumb-path (get-thumb-path dest-path))
	       (full-size-path (get-full-size-path dest-path))
	       (small-size-path (get-small-size-path dest-path)))
					;		 (format t "Making ~A~%" thumb-path)
	  (create-thumbnail dest-path thumb-path
			    (get-config-option :thumbnail-width)
			    (get-config-option :thumbnail-height)
			    frame-color)
					;		 (format t "Making ~A~%" full-size-path)
	  (create-thumbnail dest-path full-size-path
			    (get-config-option :display-width)
			    (get-config-option :display-height)
			    frame-color)
					;		 (format t "Making ~A~%" small-size-path)
	  (create-thumbnail dest-path small-size-path
			    (get-config-option :small-width)
			    (get-config-option :small-height)
			    frame-color))))))

(defun image-thumbnails (list render-func)
  (with-html-output-to-string (s)
    ((:ul :class "thumbnails")
     (dolist (obj list)
       (htm ((:li :class "span2")
	     (str (funcall render-func obj))))))))

(defun item-gallery (item)
  (image-thumbnails (images item)
	      (lambda (image)
		(with-html-output-to-string (s)
		  (:img :src (get-thumb-url image))))))

(defmethod display-an-image ((item images-mixin) &optional (image-func #'get-thumb-url))
  (with-html-output-to-string (s)
    (:img :src (funcall image-func (random-elt (images item))))))

(defmethod display-a-small-image ((item images-mixin))
  (with-html-output-to-string (s)
    (:img :src (get-small-url (random-elt (images item))))))


(defun display-gallery (images id)
  (let ((thumb-width (get-config-option :thumbnail-width))
	  (thumb-height (get-config-option :thumbnail-height)))
      (with-html-output-to-string (s)
	(lightbox-gallery s id)
	((:div :id id)
	 (:ul
	  (dolist (i images)
	    (htm (:li ((:a :href (get-full-url i))
		       (:img :src (get-thumb-url i)))))))))))


(defmethod edit-display-images ((item line-item))
  (when (images item)
    (let ((thumb-width (get-config-option :thumbnail-width))
	  (thumb-height (get-config-option :thumbnail-height)))
      (with-html-output-to-string (s)
	(lightbox-gallery s "gallery")
	((:div :id "gallery")
	 ((:form :action (get-url item) :method :post)
	  (:ul
	   (dolist (i (images item))
	     (htm (:li ((:a :href (get-full-url i))
			(:img :src (get-thumb-url i)))
		       (:input :type "checkbox" :name "imgdel" :value i)))))
	  (:input :type "submit" :value "Delete")))))))

(defmethod display-images ((item line-item))
  (when (images item)
    (let ((thumb-width (get-config-option :thumbnail-width))
	  (thumb-height (get-config-option :thumbnail-height)))
      (with-html-output-to-string (s)
	((:div :id "gallery")
	 (:ul
	  (dolist (i (images item))
	    (htm (:li ((:a :href (get-full-url i))
		       (:img :src (get-thumb-url i))))))))))))

(defun add-image (path original-filename line-item)
  (let ((type (string-downcase (pathname-type original-filename)))
	(stub (get-next-image-stub line-item)))
    (let ((dest-path (make-pathname
		      :name stub :type type
		      :defaults (image-path *web-store*))))
      (cl-fad:copy-file path dest-path)
      (create-thumbnail dest-path (get-thumb-path dest-path)
			(get-config-option :thumbnail-width)
			(get-config-option :thumbnail-height))
      (create-thumbnail dest-path (get-full-size-path dest-path)
			(get-config-option :display-width)
			(get-config-option :display-height))
      (create-thumbnail dest-path (get-small-size-path dest-path)
			(get-config-option :small-width)
			(get-config-option :small-height))
      (push (make-pathname :name stub :type type) (images line-item)))))


;; Objects that have a gallery of images associates with them
(defmethod edit-object/post ((obj images-mixin) (page (eql :images)))
  (when-let (picture (hunchentoot:post-parameter "picture"))
    (maybe-add-image picture obj))
  (image-edit-page obj))

(defmethod edit-object ((obj images-mixin) (page (eql :images)))
  (when-let (image-to-delete (hunchentoot:get-parameter "delete"))
    (setf (images obj) (remove-if (lambda (i)
				    (string-equal image-to-delete
						  (get-image-number-as-string i)))
				  (images obj))))
  (image-edit-page obj))
