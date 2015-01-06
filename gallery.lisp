(in-package #:shopper)

(defun gallery-grid-page (title body &optional navigation)
  "each of TITLE BODY and SIDEBAR should return strings.  This
function just makes sure the right components are included and sets up
some basic CSS."
  (with-html-output-to-string (s nil :prologue t :indent t)
    ((:div :class "container")
       (str body))))

(defmethod display-index-page ((store (eql :gallery)))
  (gallery-grid-page "cathycarman.com"
		     (display-exhibition (first (get-featured :exhibition)))))

(defun display-exhibition (tag)
  (with-html-output-to-string (s)
    ((:div :class "row")
     ((:div :class "span8")
      (str (carousel "exhibitionCarousel" (get-tagged-objects tag)
		     #'render-full)))
     ((:div :class "span4")
      (:h1 (str (tag-name tag)))))
    ((:div :class "row")
     (let ((others (remove tag (get-all-published-objects :exhibition))))
       (str (thumbnails (if (< (length others) 4)
			    others
			    (subseq others 0 4))
			    #'render-very-short
			    4))))))

;; (defmethod render-exhibition-small ((obj tag))
;;   (with-html-output-to-string (s)
;;     ((:a :href (get-view-url obj))
;;      (htm (str (display-an-image obj #'get-small-url)))
;;      (:h5 (str (tag-name obj))))
;;     (:p (str (description obj)))))

(ele:defpclass exhibition (tag)
  ())

(defmethod get-object ((exhibition (eql :exhibition)) webform)
  (ele:get-instance-by-value 'exhibition 'webform webform))

(defmethod get-all-objects ((exhibition (eql :exhibition)))
  (ele:get-instances-by-value 'exhibition 'store (store-name *web-store*)))

(defmethod get-form ((exhibition (eql :exhibition)))
  (exhibition-form))

(defmethod get-form ((exhibition exhibition))
  (exhibition-form exhibition))

(defun exhibition-form (&optional exhibition)
  (with-html-output-to-string (s nil :indent t)
    ((:form :action (if exhibition (get-edit-url exhibition)
			"/new/exhibition")
	    :method :post)
     (textfield "name" s "Name" "The name of this exhibition" (when exhibition (tag-name exhibition)))
     (checkbox "appearsinmenu" s "Appears in menu?"
	       (when exhibition (appears-in-menu exhibition)))
     (checkbox "featured" s "Featured?"
	       (when exhibition (featured exhibition)))
     (checkbox "published" s "Published?"
	       (when exhibition (published exhibition)))
     (textarea "description" s "Description"
	       "An (optional) description of the exhibition.  If present,
this will be used as a blurb when viewing this exhibition"
	       (when exhibition (description exhibition)))
     (submit-button "Submit" s))))

(defmethod maybe-create ((type (eql :exhibition)) parameters)
  (flet ((assoc-val (val) (cdr (assoc val parameters))))
    (if-let (name (validate-as-string (assoc-val 'name)))
      (let ((webform (get-webform name)))
	(if-let (exhibition (get-object :exhibition webform))
	  (redirect (get-edit-url exhibition))
;	  (redirect (get-edit-edit-url exhibition))
	  (let ((exhibition (make-instance 'exhibition :name name)))
	    (maybe-update exhibition parameters)
	    (redirect (get-edit-url exhibition))
	    )))
      (redirect (get-new-url :exhibition)))))

(defmethod maybe-update ((exhibition exhibition) parameters)
  (flet ((assoc-val (val) (cdr (assoc val parameters))))
    (when-let (name (validate-as-string (assoc-val 'name)))
      (setf (tag-name exhibition) name)
      (setf (webform exhibition) (get-webform name)))
    (when-let (description (validate-as-string (assoc-val 'description)))
      (setf (description exhibition) description))
    (if (assoc-val 'appearsinmenu)
	(setf (appears-in-menu exhibition) t)
	(setf (appears-in-menu exhibition) nil))
    (if (assoc-val 'featured)
	(setf (featured exhibition) t)
	(setf (featured exhibition) nil))
    (if (assoc-val 'published)
	(setf (published exhibition) t)
	(setf (published exhibition) nil))))


(defmethod view-object ((obj exhibition))
  (gallery-grid-page (tag-name obj)
		     (display-exhibition obj)))

(defmethod main-navigation-tabs ((store-type (eql :gallery)))
  (let ((exhibitions (get-all-objects :exhibition)))
    (append (main-nav-tabs)
	    (static-content-nav)
	    (tag->nav (remove-if-not #'featured exhibitions))
	    (nav-dropdown "Exhibitions"
			  (tag->nav (sort (remove-if-not #'published exhibitions)
					  #'string< :key #'tag-name) )))))


(defun gallery-navigation (&optional navigation)
  (with-html-output-to-string (s)
    ((:div :class "navbar")
     ((:div :class "nav-collapse collapse")
      (str (nav-tabs (main-navigation-tabs *web-store*)
		     navigation :class "nav nav-tabs")))
     ;; (str (navigation-cart))
     )))
