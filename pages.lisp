;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(defun edit-store-page ()
  (let ((branding (get-branding *web-store*)))
    (when-let (parameters (post-parameters*))
      (update-branding branding)
      (flet ((get-p (parameter)
	       (when-let (value (cdr (assoc parameter parameters :test #'string-equal)))
		 (when (and (stringp value) (not (zerop (length value))))
		   value))))
	(when-let (name (get-p "name"))
	  (setf (store-name *web-store*) name))
	(when-let (skup (get-p "sku"))
	  (setf (sku-prefix *web-store*) skup))
	(when-let (orderp (get-p "order"))
	  (setf (order-prefix *web-store*) orderp))
	(if (get-p "open")
	    (setf (store-open *web-store*) t)
	    (setf (store-open *web-store*) nil))
	(when-let (stripekey (get-p "stripekey"))
	  (setf (stripe-api-key *web-store*) stripekey))
	(when-let (pubkey (get-p "stripepubkey"))
	  (setf (stripe-public-key *web-store*) pubkey))))

    (standard-page "Edit store parameters" nil
		   (with-html-output-to-string (s)
			   (:div :class "container"
				 (:div :class "row"
				       (:div :class "span6"
					     (str (edit-store-form)))
				       (:div :class "span6"
					     (str (edit-branding branding))))))
		   (minimal-edit-bar))))


(defun store-open-dependent-page (page-func)
  (if (store-open *web-store*)
      (funcall page-func)
      (standard-page (format nil "~A is closed" (store-name *web-store*)) nil
		     (with-html-output-to-string (s)
		       ((:div :class "container")
			((:div :class "hero-unit")
			 (:h3 (fmt "~A is closed" (store-name *web-store*)))))))))


(defun shopping-cart-page ()
  (let ((cart (get-or-initialize-cart)))
    (when-let (parameters (post-parameters*))
      (maybe-update-cart cart parameters))
    (standard-page "View shopping cart" nil
		   (shopping-cart-form (get-or-initialize-cart)))))


(defun enter-details-page ()
  (if (or (parameter "change") (not (get-customer)))
      (standard-page "Enter customer details" nil
		     (with-html-output-to-string (s)
		       (:h2 "Enter shipping details")
		       (str (customer-address-form))))
      (place-order)))



(defun paypal-errors (sent received)
  (with-html-output-to-string (s)
    ((:div :class "container")
     ((:div :class "row")
      ((:div :class "span5")
       (:h2 "Sent")
       (:dl
	(dolist (item sent)
	  (htm (:dt (fmt "~A" (car item)))
	       (:dd (fmt "~A" (cdr item)))))))
      ((:div :class "span5")
       (:h2 "Received")
       (:dl
	(dolist (item received)
	  (htm (:dt (fmt "~A" (car item)))
	       (:dd (fmt "~A" (cdr item)))))))))))

(defun paypal-error-page (sent received)
  (standard-page "Paypal error" nil
	      (paypal-errors sent received)))


(defmethod index-page ()
  (standard-page (format nil "Welcome to ~A" (store-name *web-store*))
		 nil
		 (with-html-output-to-string (s)
		   ((:div :class "container")
		    ((:div :class "row")
		     ((:div :class "span6")

		      (when-let (index (get-object :static-content "index"))
			(str (content index))))
		  
		     ((:div :class "span6")
		      (str (carousel "featuredCarousel"
				     (get-featured-items 10)
				     (lambda (item)
				       (with-html-output-to-string (s)
					 (str (display-an-image item (thumb 500 500)))
					 (:div :class "carousel-caption"
					       (:h4 ((:a :class "muted"
							 :href (get-view-url item))
						     (str (title item))) )
					       (:p (str (short-description item))))))))))
		    (when (current-user)
		      (str (top-edit-bar (get-object :static-content "index")))))
		
	     
		   (:script "$('.carousel').carousel()"))))


;; (defun store-index-page ()
;;   )

(defun get-featured-items (&optional number)
  "If NUMBER is specified, return a list of featured items at most
  NUMBER long, otherwise return all featured items."
  (let ((featured (remove-if-not #'featured (get-all-objects :line-item))))
    (cond (number (if (> (length featured) number)
		      (subseq (shuffle featured) 0 number)
		      featured))
	  (t featured))))

(defun http-get-link (key value link-text)
  "Creates a link back to this page with an added GET parameter"
  (with-html-output-to-string (s)
    ((:a :href (url-rewrite:add-get-param-to-url
		(script-name*) key value))
     (str link-text))))


(defmethod geo-cloud ((item line-item))
  (get-cloud (get-all-objects :geography) 
	     (lambda (o) (item-available-in? item o))
	     "geo"
	     #'get-identifier
	     #'geo-name
	     (lambda (label)
	       (when-let (geo (get-object :geography label))
		 (toggle-geo item geo)))))

(defmethod tag-cloud ((cms cms))
  (get-cloud (get-all-objects :tag)
	     (lambda (o) (tagged? cms o))
	     "cloudtag"
	     #'webform
	     #'tag-name
	     (lambda (label)
	       (when-let (tag (get-object :tag label))
		 (toggle-tag cms tag)))))

(defun get-cloud (objects predicate get-parameter link-writer label-writer &optional toggle-func)
  (when-let (label (get-parameter get-parameter))
    (and toggle-func (funcall toggle-func label)))
  (with-html-output-to-string (s)
    (:div :class "container-fluid"
	  (:ul :class "inline-block"
	       (dolist (o objects)
		 (let ((link (http-get-link get-parameter
					    (funcall link-writer o)
					    (funcall label-writer o))))
		   (if (funcall predicate o)
		       (htm (:li (:strong (str link))))
		       (htm (:li (str link)))))))) ))


(defun tags-widget (item)
  (with-html-output-to-string (s)
    (:h5 "Current tags")
    (:p (str (render-tags (get-tags item)))
	(:h5 "Available tags")
	(:p (str (render-tags (remove-if (lambda (tag)
					   (tagged? item tag))
					 (get-all-objects :tag))))))))
 


(defun tag-display-page (tag)
  (with-html-output-to-string (s)
    ((:div :class "container")
     (when (and (description tag) (not (zerop (length (description tag)))))
       (htm ((:div :class "well") (str (description tag)))))
     (when-let (thumbs (get-tagged-objects tag))
       (htm (str (thumbnails thumbs #'render-thumb)))))))



(defun get-image-number-as-string (image)
  (second (split-sequence:split-sequence #\_
					 (pathname-name image))))

;; FIXME to add delete functionality
(defun image-edit-page (item)
  (let ((this-url (get-edit-page-url item :images)))
    (concatenate 'string
		 (image-form item)
		 (image-thumbnails (get-images item)
				   (lambda (image)
				     (with-html-output-to-string (s)
				       (str (funcall (thumbnail-element 500 500) image))
				       ((:a :class "btn btn-danger"
					    :href (url-rewrite:add-get-param-to-url
						   this-url
						   "unrelateimage"
						   (get-designator image)))
					"Delete")))))))


(defun thumbnails (list render-func &optional (items-across 4))
  (let ((rows (partition-list list items-across)))
    (with-html-output-to-string (s)
      (dolist (row rows)
	(htm ((:div :class "row-fluid")
	      (dolist (item row)
		(let ((span (format nil "span~A" (truncate 12 items-across))))
		  (htm ((:div :class span)
			(str (funcall render-func item))))))))))))




(defmethod render-thumb ((obj line-item) &optional edit)
  (with-html-output-to-string (s)
    ((:a :href (get-view-url obj))
     (when (get-related-objects obj :image)
       (htm (str (display-an-image obj)))))

    (:p :class "lead"
	((:a :href (get-view-url obj)) (str (title obj)))
	(when (not edit)
	  (htm (str (cart-widget obj)))))
    
    ;((:a :href (get-view-url obj)) (:h5 (str (title obj))))

    
    (when edit
      (htm (:p (when (published obj)
		 (htm ((:span :class "label") "Published")
		      (when (featured obj)
			(htm ((:span :class "label label-success")
			      "Featured"))))))))
    

    (:p (str (short-description obj))
	(:br)
	(:em "Price: ")
	(str (print-price (get-price obj))))

    (when edit
      (htm ((:a :class "btn btn-mini" :href (get-edit-url obj))
	    "Edit")
	   ((:a :class "btn btn-mini btn-danger pull-right" :href (get-delete-url obj))
	    "Delete")))

))

(defmethod render-thumb ((obj tag) &optional edit)
  (with-html-output-to-string (s)
    ((:a :href (get-view-url obj))
     (htm (str (display-an-image obj)))
     (:h5 (str (tag-name obj))))
 
    (when edit
      (htm (:p (when (appears-in-menu obj)
	    (htm ((:span :class "label") "Menu")
		 (when (featured obj)
		   (htm ((:span :class "label label-success") "Featured"))))))))
    
    (:p (str (description obj)))
    
    (when edit
      (htm ((:a :class "btn btn-mini" :href (get-edit-url obj))
	    "Edit")
	   ((:a :class "btn btn-mini btn-danger pull-right" :href (get-delete-url obj))
	    "Delete")))))

(defmethod render-thumb ((obj tag) &optional edit)
  (with-html-output-to-string (s)
    ((:a :href (get-view-url obj))
     (htm (str (display-an-image obj)))
     (:h5 (str (tag-name obj))))
 
    (when edit
      (htm (:p (when (appears-in-menu obj)
	    (htm ((:span :class "label") "Menu")
		 (when (featured obj)
		   (htm ((:span :class "label label-success") "Featured"))))))))
    
    (:p (str (description obj)))
    
    (when edit
      (htm ((:a :class "btn btn-mini" :href (get-edit-url obj))
	    "Edit")
	   ((:a :class "btn btn-mini btn-danger pull-right"
		:href (get-delete-url obj))
	    "Delete")))))

(defmethod render-very-short ((obj tag))
  (with-html-output-to-string (s)
    ((:a :href (get-view-url obj))
     (htm (str (display-an-image obj #'get-small-url)))
     (:h5 (str (tag-name obj))))
    (:p (str (description obj)))))


;; (defmethod get-delete-url ((tag tag))
;;   (format nil "/delete/tag/~A" (webform tag)))

(defun safe-read-from-string (string)
  (read-from-string (remove #\: string :test #'char-equal)))


;; (defun new-item-page ()
;;   (make-page "Create new single item" (item-form)
;; 	     :sidebar (edit-bar "New item")))
;; FIXME: deal with colons and other nasties in the symbols

(defun find-cms-class (class-string)
  (let ((symbol (safe-read-from-string class-string)))
    (logger :debug "FIND-CMS-CLASS: ~S" class-string)
    (when (and (symbolp symbol)
	       (subtypep symbol 'cms))
      (make-keyword symbol))))

(defun find-object-and-page (class-string identifier page-string)
  (let ((class (safe-read-from-string class-string))
	(page (safe-read-from-string page-string)))
    (if (and (symbolp class) (subtypep class 'cms))
	(if-let (object (get-object (make-keyword class) identifier))
	  (if (symbolp page)
	      (values object (make-keyword page))
	      hunchentoot:+http-bad-request+)
	  hunchentoot:+http-not-found+)
	hunchentoot:+http-bad-request+)))


;; (defun edit-item-edit-page (item &optional debug)
;;   (make-page (format nil "Editing ~A" (sku item))
;; 	     (with-html-output-to-string (s)
;; 	       (when debug
;; 		 (htm (:pre (esc (format nil "~S" debug)))))
;; 	       (str (edit-tabs item "Edit"))
;; 	       (str (item-form item)))
;; 	     :sidebar (edit-bar "All items")))





(defgeneric get-form (symbol-or-instance))

;; (defmethod edit-object :around ((obj cms) page)
;;   (make-page (format nil "Editing ~A" (get-identifier obj))
;; 	     (with-html-output-to-string (s)
;; 	       (str (render-edit-tabs obj page))
;; 	       (str (call-next-method)))))




(defmethod get-edit-tabs ((store-type (eql :web-store)))
  (reduce #'append (mapcar (lambda (class)
			     (list (labelise class)
				   (cons (get-new-url class) "create new")
				   (cons (get-multi-edit-url class) "list all")))
			   (store-active-classes *web-store*))))

(defmethod get-edit-tabs (store-type)
  (reduce #'append (mapcar (lambda (class)
			     (list (labelise class)
				   ;(cons (get-new-url class) "create new")
				   (cons (get-multi-edit-url class) "list all")))
			   (store-active-classes *web-store*))))

;; (defmethod get-edit-tabs ((store-type (eql :web-store)))
;;   `("Items"
;;     ("/new/item" . "New item")
;;     ("/edit/items" . "All items")
;;     ("/edit/items/published" . "Published items")
;;     ("/edit/items/unpublished" . "Unpublished items")
;;     ("/edit/items/featured" . "Featured items")
;;     "Tags"
;;     ("/new/tag" . "New tag")
;;     ("/edit/tags" . "All tags")
;;     ("/edit/tags/menu" . "Menu tags")
;;     ("/edit/tags/featured" . "Featured tags")
;;     "Geo"
;;     ("/new/geo" . "New geography")
;;     ("/edit/geos" . "All geographies")
;;     "Static content"
;;     ("/new/static" . "New static page")
;;     ("/edit/all/static" . "All static pages")))

(defun edit-bar (active)
  (nav-tabs (get-edit-tabs *web-store*)
	    active
	    :class "nav nav-list"))

(defun main-site-bar (active)
  (let ((bar '()))
    (when (featured-tags)
      (push (list "Featured") bar)
      (push (tag->nav (featured-tags)) bar))
    (when (menu-tags)
      (push (list "Categories") bar)
      (push (tag->nav (menu-tags)) bar))
    (nav-tabs (reduce #'append (reverse bar)) active
	      :class "nav nav-list")))


(defun display-item-text (item)
  (with-html-output-to-string (s)
    (:h2 (str (title item)))
    (:p :class "lead" (str (print-price (get-price item))))
    (str (cart-widget item))
    ((:p :class "lead") (str (short-description item)))
    (:p (str (long-description item)))))

(defun display-item-content (item)
  (grid (6 (display-item-text item)
	 6 (carousel "imageCarousel"
		     (get-related-objects item :image) 
		     (thumbnail-element 500 500)))))
    
;    (:script "$('.carousel').carousel()")))

(defun thumbnail-element (x y)
  (lambda (image)
    (with-html-output-to-string (s)
    (let ((thumbnail (car (get-related-objects-by-info image :thumbnail (list x y)))))
      (htm ((:p :align "center")
	    (if thumbnail
		(htm (:img :src (format nil "/images/~A" (namestring (get-file thumbnail)))))
		(htm (:img :src (format nil "/images/~A" (namestring (get-file image))) 
			   :height y :width x)))))))))

(defun thumb (x y)
  (lambda (image)
    (with-html-output-to-string (s)
      (let ((thumbnail (car (get-related-objects-by-info image :thumbnail (list x y)))))
	(if thumbnail
	    (htm (:img :src (format nil "/images/~A" (namestring (get-file thumbnail)))))
	    (htm (:img :src (format nil "/images/~A" (namestring (get-file image))) 
		       :height y :width x)))))))

(defun carousel (carousel-id elements render-function)
  (when elements
    (if (> (length elements) 1)
	(with-html-output-to-string (s)
	  ((:div :id carousel-id :class "carousel slide")
	   ((:div :class "carousel-inner" :style "text-align:center")
	    (let ((first (car elements)))
	      (htm ((:div :class "active item")
		    (str (funcall render-function first)))))
	    (dolist (element (cdr elements))
	      (htm ((:div :class "item")
		    (str (funcall render-function element))))))
	   ((:a :class "carousel-control left" :href (format nil "#~A" carousel-id)
		:data-slide "prev") (str "&lsaquo;"))
	   ((:a :class "carousel-control right" :href (format nil "#~A" carousel-id)
		:data-slide "next") (str "&rsaquo;"))))
	(funcall render-function (first elements)))))

