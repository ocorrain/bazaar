(in-package #:shopper)

(ele:defpclass branding (cms)
  ())

(defmethod get-branding ((store web-store))
  (if-let (branding (branding store))
    (get-object-by-designator branding)
    (let ((branding (make-instance 'branding)))
      (setf (branding store) (get-designator branding))
      branding)))

(defmethod banner-form ((branding branding))
  (create-image-form "/admin/edit/store" "banner"))

(defmethod favico-form ((branding branding))
  (create-image-form "/admin/edit/store" "favico"))

(defmethod thumbnail-form ((branding branding))
  (create-image-form "/admin/edit/store" "thumbnail"))

(defun get-banner (branding)
  (get-related-objects branding :banner))

(defun get-favico (branding)
  (get-related-objects branding :favico))

(defun get-thumbnail (branding)
  (get-related-objects branding :thumbnail))

(defun update-branding (branding)
  (simple-add-image "banner" branding :banner)
  (simple-add-image "favico" branding :favico 32 32)
  (simple-add-image "thumbnail" branding :thumbnail 128 128))


(defun simple-add-image (post-parameter branding relation &optional box-x box-y)
  (when-let (image (post-parameter post-parameter))
    (destructuring-bind (path filename content-type) image
      (declare (ignore content-type))
      (let* ((type (string-downcase (pathname-type filename)))
	     (stub (make-designator))
	     (image-file (make-pathname :name stub :type type))
	     (image-obj (make-instance 'image
				       :file image-file
				       :designator stub)))
	(cl-fad:copy-file path (merge-pathnames image-file (image-path *web-store*)))
	(if (and box-x box-y)
	    (with-slots (file) image-obj
	      (let* ((source-path (merge-pathnames file (image-path *web-store*)))
		     (designator (make-designator))
		     (image-type (pathname-type file))
		     (dest-path (make-pathname :name designator
					       :type image-type
					       :defaults (image-path *web-store*)))
		     (thumb-obj (make-instance 'image
					       :file (make-pathname :name designator
								    :type image-type)
					       :designator designator)))
		(create-thumbnail source-path dest-path box-x box-y)
		(set-branding-relation branding thumb-obj relation)))
	    (set-branding-relation branding image-obj relation))))))

(defun set-branding-relation (branding obj relation)
  (when-let (related (get-related-objects branding relation))
    (dolist (r related)
      (unrelate branding r relation)))
  (relate branding obj relation))

(defun get-branding-relation (branding relation)
  (when-let (objs (get-related-objects branding relation))
    (first objs)))

(defun edit-branding (branding)
  (with-html-output-to-string (s)
    (:h4 "Banner")
    (str (banner-form branding))
    (:h4 "Thumbnail")
    (str (thumbnail-form branding))
    (when-let (thumb (get-branding-relation branding :thumbnail))
      (htm (str (image-element thumb))))
    (:h4 "Favico")
    (str (favico-form branding))
    (when-let (thumb (get-branding-relation branding :favico))
      (htm (str (image-element thumb))))))

