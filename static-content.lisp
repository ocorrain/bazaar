(in-package #:shopper)

(ele:defpclass static-content (cms)
  ((title :initarg :title :initform nil :accessor title :index t)
   (content :initarg :content :initform nil :accessor content)
   (appears-in-menu :initarg :appears-in-menu :initform nil
		    :accessor appears-in-menu :index t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level API

(defmethod get-object ((static (eql :static-content)) identifier)
  (get-content-from-webform identifier))

(defmethod get-all-objects ((static (eql :static-content)))
  (ele:get-instances-by-value 'static-content 'store (store-name *web-store*)))


(defmethod get-identifier ((static static-content))
  (get-webform (title static)))

(defmethod get-form ((stat (eql :static-content)))
  (static-content-form))

(defmethod get-form ((stat static-content))
  (static-content-form stat))

(defmethod get-edit-tabs ((stat static-content))
  '(:view :edit))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun static-content-form (&optional static-content)
  (with-html-output-to-string (s nil :indent t)
    (:script :type "text/javascript" :src "/js/rte-light-read-only/jquery.rte.js")
    
    ((:form :action (if static-content
			(get-edit-url static-content)
			(get-new-url :static-content))
	    :method "post")
     (textfield "title" s "Title" "Page title"
		(when static-content (title static-content)))
     (checkbox "menu" s "Appears in menu?"
	       (when static-content (appears-in-menu static-content)))
     (textarea "content" s "Content" "Page contents"
	       (when static-content (content static-content)))
     (:br)
     (submit-button "Submit" s)
     (:script :type "text/javascript" (str "$('.rte-zone').rte(\"css url\", \"toolbox images url\");"))
)))

;; (defun static-content-edit-page (content)
;;   (make-page (format nil "Editing ~A" (title content))
;; 	       (with-html-output-to-string (s)
;; 		 (str (static-content-form content)))
;; 	       :sidebar (edit-bar "")))

;; (defun static-content-new-page ()
;;   (make-page "Create new static content"
;; 	     (static-content-form)
;; 	     :sidebar (edit-bar "")))

;; (defun static-content-nav ()
;;   (mapcar (lambda (content)
;; 	    (cons (restas:genurl 'r/view-static-content
;; 				 :contentform (get-webform (title content)))
;; 		  (title content)))
;; 	  (ele:get-instances-by-value 'static-content 'appears-in-menu t)))



(defun get-content-from-webform (webform)
  (find-if (lambda (content)
		    (equal webform
			   (get-webform (title content))))
	   (get-all-objects :static-content)))

(defmethod maybe-create ((type (eql :static-content)) parameters)
  (flet ((assoc-val (val) (cdr (assoc val parameters))))
    (if-let (title (validate-as-string (assoc-val 'title)))
      (let ((webform (get-webform title)))
	(if-let (content (get-content-from-webform webform))
	  (hunchentoot:redirect (get-edit-url content))
	  (let ((static-content-obj (make-instance 'static-content :title title)))
	    (maybe-update static-content-obj parameters)
	    (hunchentoot:redirect (get-edit-url static-content-obj))))))))



(defmethod maybe-update ((obj static-content) parameters)
  (flet ((assoc-val (val) (cdr (assoc val parameters))))
    (when-let (title (validate-as-string (assoc-val 'title)))
      (setf (title obj) title))
    (when-let (content (validate-as-string (assoc-val 'content)))
      (setf (content obj) content))
    (if (assoc-val 'menu)
	(setf (appears-in-menu obj) t)
	(setf (appears-in-menu obj) nil))))



(defun view-static-content (content)
  (with-html-output-to-string (s)
    (:h1 (str (title content)))
    (str (content content))))

(defun view-static-content-page (content)
  (standard-page (title content) nil
		 (view-static-content content)))

(defmethod view-object ((obj static-content))
  (view-static-content obj))
