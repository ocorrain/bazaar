(in-package #:shopper)

(ele:defpclass artwork (cms)
  ((title :initarg :title :initform "" :accessor title)
   (description :initarg :description :initform "" :accessor description)
   (webform :initarg :webform :initform nil :accessor webform :index t)))

(defmethod initialize-instance :after ((instance artwork) &rest stuff)
  (declare (ignore stuff))
  (setf (webform instance) (get-webform (title instance))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level API

(defmethod get-object ((artwork (eql :artwork)) webform)
  (ele:get-instance-by-value 'artwork 'webform webform))

(defmethod get-all-objects ((artwork (eql :artwork)))
  (ele:get-instances-by-value 'artwork 'store (store-name *web-store*)))

(defmethod get-identifier ((artwork artwork))
  (webform artwork))

(defmethod render-object ((artwork artwork))
  (with-html-output-to-string (s)
    ((:div :class "container")
     ((:div :class "row")
     ((:div :class "span6")
      (:h2 (str (title artwork)))
      ((:p :class "lead") (str (description artwork))))
     ((:div :class "span6")
      (str (carousel "imageCarousel" (get-related-objects artwork :image)
		     (thumbnail-element 500 500)))))
    (:script "$('.carousel').carousel()"))))



(defmethod get-form ((item (eql :artwork)))
  (artwork-form))

(defmethod get-form ((artwork artwork))
  (artwork-form artwork))

(defmethod get-edit-tabs ((artwork artwork))
  '(:view :edit :images :tags))


(defmethod view-object ((obj artwork))
  (render-object obj))

;;;;;;;;;;;;;
(defmethod maybe-update ((obj artwork) parameters)
  (flet ((assoc-val (val) (cdr (assoc val parameters))))
    (when-let (title (validate-as-string (assoc-val 'title)))
      (setf (title obj) title)
      (setf (webform obj) (get-webform (title obj)))
      (when-let (description (validate-as-string (assoc-val 'description)))
	(setf (description obj) description)))))

(defmethod maybe-create ((type (eql :artwork)) parameters)
  (flet ((assoc-val (val) (cdr (assoc val parameters))))
    (if-let (title (validate-as-string (assoc-val 'title)))
      (let ((webform (get-webform title)))
	(if-let (artwork (get-object :artwork webform))
	  (redirect (get-edit-url artwork))
	  (let ((artwork-obj (make-instance 'artwork :title title)))
	    (maybe-update artwork-obj parameters)
	    (redirect (get-edit-url artwork-obj))))))))

(defmethod render-thumb ((obj artwork) &optional edit)
  (declare (ignore edit))
  (with-html-output-to-string (s)
    ((:a :href (get-view-url obj))
     (when (images obj)
       (htm (str (display-an-image obj)))))
    (:p :class "lead"
	((:a :href (get-view-url obj)) (str (title obj))))
    (:p (str (description obj)))
    (when edit
      (htm ((:a :class "btn btn-mini" :href (get-edit-url obj))
	    "Edit")
	   ((:a :class "btn btn-mini btn-danger pull-right" :href (get-delete-url obj))
	    "Delete")))))

(defmethod render-full ((obj artwork) &optional edit)
  (declare (ignore edit))
  (with-html-output-to-string (s)
    ((:a :href (get-view-url obj))
     (when (get-related-objects obj :image) 
       (htm (str (display-an-image obj (thumbnail-element 500 500))))))
    (:p :class "lead"
	((:a :href (get-view-url obj)) (str (title obj))))
    (:p (str (description obj)))
    (when edit
      (htm ((:a :class "btn btn-mini" :href (get-edit-url obj))
	    "Edit")
	   ((:a :class "btn btn-mini btn-danger pull-right" :href (get-delete-url obj))
	    "Delete")))))

(defun import-from-bubble (bubble-directory)
  (let* ((store-path (make-pathname :name "store" :defaults bubble-directory))
	 (bubble-path (make-pathname :name "bubble" :defaults bubble-directory))
	 (bubble-images-dir (merge-pathnames #p"images/" bubble-directory))
	 (store-tags (with-open-file (store store-path)
		       (let ((store-config (cdr (read store))))
			 (map 'list (lambda (str)
				      (make-instance 'exhibition :name str))
			      (getf store-config :tags))))))
    (with-open-file (bubble bubble-path)
      (do ((obj (read bubble nil 'done) (read bubble nil 'done)))
	  ((eq obj 'done) 'done)
	(when (eq (first obj) 'picture)
	  (let ((specs (cdr obj)))
	    (print specs)
	    (let ((webform (get-webform (getf specs :title))))
	      (if-let (artwork (get-object :artwork webform))
		(let ((image (getf specs :file)))
		  (add-image (make-pathname :name (pathname-name image)
					    :type (pathname-type image)
					    :defaults bubble-images-dir)
			     image
			     artwork))
		(let ((artwork (make-instance 'artwork :title (getf specs :title)
					      :description (getf specs :description)))
		      (tags (getf specs :tags))
		      (image (getf specs :file)))
		  (dolist (tag tags)
		    (when-let (tag-obj (elt store-tags tag))
		      (tag-item artwork tag-obj)))
		  (add-image (make-pathname :name (pathname-name image)
					    :type (pathname-type image)
					    :defaults bubble-images-dir)
			     image
			     artwork)))))))
      
      (dolist (tag store-tags)
	(when (emptyp (tag-name tag))
	  (delete-object tag))))))

(defun get-bubble-tags (bubble-directory)
  (let* ((store-path (make-pathname :name "store" :defaults bubble-directory))
	 (store-tags (with-open-file (store store-path)
		       (let ((store-config (cdr (read store))))
			 (getf store-config :tags)))))
    store-tags))


;; (defun import-from-directory (directory exhibition-name)
;;   "Imports jpegs from a directory and adds them to the exhibition
;;   named by exhibition-name.  If this does not exist, creates a new
;;   exhibition called exhibition-name and adds the images to that as
;;   separate artworks."
;;   (let ((exhibition (or (get-object :exhibition (get-webform exhibition-name))
;; 			(make-instance 'exhibition :name exhibition-name))))
;;     (dolist (f (cl-fad:list-directory directory))
;;       (let ((artwork (make-instance 'artwork :title (pathname-name f))))
;; 	(add-image f f artwork)
;; 	(tag-item artwork exhibition))
;;       (print (pathname-name f)))))
  
