;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(defun edit-store-page (&optional parameters)
  (when parameters
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
	  (setf (store-open *web-store*) nil))))
  (basic-page "Edit store parameters"
	      (with-html-output-to-string (s)
		((:div :class "container")
		 ;; (:pre (describe *web-store* s))
		 ;; (:pre (fmt "~S" parameters))
		 (:h1 "Edit global parameters")
		 (str (edit-store-form))))))

(defun store-open-dependent-page (page-func)
  (if (store-open *web-store*)
      (funcall page-func)
      (basic-page (format nil "~A is closed" (store-name *web-store*))
		  (with-html-output-to-string (s)
		    ((:div :class "container")
		     ((:div :class "hero-unit")
		      (:h3 (fmt "~A is closed" (store-name *web-store*)))))))))


(defun shopping-cart-page ()
  (basic-page "View shopping cart"
	      (with-html-output-to-string (s)
		  ((:div :class "container")
		 (str (shopping-cart-form (get-or-initialize-cart)))))))

(defun enter-details-page ()
  (basic-page "Enter customer details"
	      (with-html-output-to-string (s)
		((:div :class "container")
		 (:h2 "Enter shipping details")
		 (str (customer-address-form))))))

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
  (basic-page "Paypal error"
	      (paypal-errors sent received)))

(defmethod display-index-page ((type (eql :web-store)))
  (basic-page (format nil "Welcome to ~A" (store-name *web-store*))
	      (with-html-output-to-string (s)
		;; ((:div :class "well")
					;(:h2 "Featured categories")
		((:div :class "container")
		 ((:div :class "row")
		  ((:div :class "span6")
		   (:h1 (str (store-name *web-store*)))
		   (:p "Chocolate. There are few foods that people feel as
	       	passionate about&mdash;a passion that goes beyond a love
	       	for the 'sweetness' of most candies or desserts: after
	       	all, few people crave caramel, whipped cream, or
	       	bubble gum. Chocolate is, well, different. For the
	       	true chocoholic, just thinking about chocolate can
	       	evoke a pleasurable response. You may want to grab a
	       	bar or make a nice cup of hot cocoa before you begin
	       	exploring here.")
		   ;; (:p ((:a :class "btn btn-success btn-large pull-right"
		   ;; 	    :href "/featured")
		   ;; 	"See our featured items!"))
		   )
		  ((:div :class "span6")
		   (str (carousel "featuredCarousel"
				  (get-featured-items 10)
				  (lambda (item)
				    (with-html-output-to-string (s)
				      (str (display-an-image item #'get-full-url))
				      (:div :class "carousel-caption"
					    (:h4 ((:a :class "muted"
						      :href (get-view-url item))
						  (str (title item))) )
					    (:p (str (short-description item)))))))))))
	     
		(:script "$('.carousel').carousel()"))
	      "Home"))

;; (defun store-index-page ()
;;   )

(defun get-featured-items (&optional number)
  "If NUMBER is specified, return a list of featured items at most
  NUMBER long, otherwise return all featured items."
  (let ((featured (remove-if-not #'featured (all-items))))
    (cond (number (if (> (length featured) number)
		      (subseq (shuffle featured) 0 number)
		      featured))
	  (t featured))))

(defun featured-items-page ()
  (make-page (format nil "Featured items")
	     (with-html-output-to-string (s)
	       (:h2 "Featured items")
	       (str (thumbnails (remove-if-not #'featured (all-items))
				#'render-thumb)))
	     :sidebar (main-site-bar "")))

(defun make-tags-page (item)
  (concatenate 'string
	       (with-html-output-to-string (s)
		 (:h5 "Current tags")
		 (:p (str (render-tags (ele:pset-list (tags item)))))
		 (:h5 "Available tags")
		 (:p (str (render-tags (remove-if (lambda (tag)
						    (tagged? item tag))
						  (all-tags))))))))
 
;; (defun edit-tag-page (test title)
;;   (make-page title
;; 	     (thumbnails (collect-tags-with test)
;; 			 (lambda (item)
;; 			   (render-thumb item t)))
;; 	     :sidebar (edit-bar title)))

(defun tag-edit-page (tag)
  (make-page (format nil "Editing ~A" (tag-name tag))
	     (concatenate 'string
			  (edit-tabs tag "Edit")
			  (tag-form tag))
	     :sidebar (edit-bar "New tag")))

(defun tag-display-page (tag)
  (with-html-output-to-string (s)
    ((:div :class "container")
     (when (and (description tag) (not (zerop (length (description tag)))))
      (htm ((:div :class "well") (str (description tag)))))
    (when-let (thumbs (remove-if-not #'published
				     (ele:pset-list (tag-members tag))))
      (str (thumbnails thumbs #'render-thumb))))))



; existing items
(defun edit-item-page (test title)
  (make-page title
	     (thumbnails (collect-items-with test)
			 (lambda (item)
			   (render-thumb item t)))
	     :sidebar (edit-bar title)))

(defun get-image-number-as-string (image)
  (second (split-sequence:split-sequence #\_
					 (pathname-name image))))

(defun image-edit-page (item)
  (let ((this-url (get-edit-page-url item :images)))
    (concatenate 'string
		 (image-form item)
		 (image-thumbnails (images item)
				   (lambda (image)
				     (with-html-output-to-string (s)
				       (:img :src (get-thumb-url image))
				       (:br)
				       ((:a :class "btn btn-danger"
					    :href (url-rewrite:add-get-param-to-url
						   this-url
						   "delete"
						   (get-image-number-as-string image)))
					"Delete")))))))

(defun edit-front-page ()
  (make-page
   "Edit"
   (with-html-output-to-string (s)
     ((:div :class "hero-unit")
      (:h1 "Edit and create items")
      (:p "Here you can add new items, and edit existing items")
      ((:a :class "btn btn-primary btn-large" :href "/new/item")
       "Create a new item")
      ((:a :class "btn btn-primary btn-large pull-right" :href "/edit/items")
       "Edit an existing item"))

     ((:div :class "hero-unit")
      (:h1 "Edit and create tags")
      (:p "Here you can add new items, and edit existing items")
      ((:a :class "btn btn-primary btn-large" :href "/new/tag")
       "Create a new tag")
      ((:a :class "btn btn-primary btn-large pull-right" :href "/edit/tags")
       "Edit an existing tag")))
   :sidebar (edit-bar "Edit")))

(defun thumbnails (list render-func &optional (items-across 4))
  (let ((rows (partition-list list items-across)))
    (with-html-output-to-string (s)
      (dolist (row rows)
	(htm ((:div :class "row-fluid")
	      (dolist (item row)
		(let ((span (format nil "span~A" (truncate 12 items-across))))
		  (htm ((:div :class span)
			(str (funcall render-func item))))))))))))

(defmethod render-very-short ((obj line-item))
  (with-html-output-to-string (s)
    ((:a :href (get-view-url obj))
     (when (images obj)
       (htm (str (display-an-image obj #'get-small-url))))
     (:h5 (str (title obj))))))


(defmethod render-thumb ((obj line-item) &optional edit)
  (with-html-output-to-string (s)
    ((:a :href (get-view-url obj))
     (when (images obj)
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
;    (hunchentoot:log-message* :info "~S" class-string)
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

(defmethod new-object-form (class)
  (make-page (format nil "create new ~A" (string-downcase class))
	     (get-form class)
	     :sidebar (edit-bar (format nil "New ~A" (string-downcase class)))))

;; (defun edit-item-edit-page (item &optional debug)
;;   (make-page (format nil "Editing ~A" (sku item))
;; 	     (with-html-output-to-string (s)
;; 	       (when debug
;; 		 (htm (:pre (esc (format nil "~S" debug)))))
;; 	       (str (edit-tabs item "Edit"))
;; 	       (str (item-form item)))
;; 	     :sidebar (edit-bar "All items")))





(defgeneric get-form (symbol-or-instance))

(defmethod edit-object :around ((obj cms) page)
  (make-page (format nil "Editing ~A" (get-identifier obj))
	     (with-html-output-to-string (s)
	       (str (render-edit-tabs obj page))
	       (str (call-next-method)))
	     :sidebar (edit-bar "All items")))

(defmethod edit-object/post :around ((obj cms) page)
  (make-page (format nil "Editing ~A" (get-identifier obj))
	     (with-html-output-to-string (s)
	       (hunchentoot:log-message* :error "in edit-object/post")
	       (str (render-edit-tabs obj page))
	       (str (call-next-method)))
	     :sidebar (edit-bar "All items")))

(defmethod new-object (class-sym)
  (make-page (format nil "Creating new ~A" (labelise class-sym))
	     (get-form class-sym)
	     :sidebar (edit-bar "All items")))

(defmethod get-edit-tabs ((store-type (eql :web-store)))
  (reduce #'append (mapcar (lambda (class)
			     (list (labelise class)
				   (cons (get-new-url class) "create new")
				   (cons (get-multi-edit-url class) "list all")))
			   (store-active-classes *web-store*))))

(defmethod get-edit-tabs (store-type)
  (reduce #'append (mapcar (lambda (class)
			     (list (labelise class)
				   (cons (get-new-url class) "create new")
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

(defmethod edit-tabs ((item line-item) active)
  (nav-tabs `((,(get-edit-view-url item) . "View")
	      (,(get-edit-edit-url item) . "Edit")
	      (,(get-edit-image-url item) . "Images")
	      (,(get-edit-tags-url item) . "Tags")
	      (,(get-edit-contents-url item) . "Contents"))
	    active))

(defmethod edit-tabs ((tag tag) active)
  (nav-tabs `((,(get-edit-view-url tag) . "View")
	      (,(get-edit-edit-url tag) . "Edit"))
	    active))

(defun display-item-page (item)
  (make-page (format nil "Viewing ~A" (title item))
	     (concatenate 'string
			  (edit-tabs item "View")
			  (display-item-content item))
	     :sidebar (edit-bar "All items")
	     :end-matter (with-html-output-to-string (s)
			   (:script "$('.carousel').carousel()"))))


(defun display-item-content (item)
  (with-html-output-to-string (s)
    ((:div :class "row")
     ((:div :class "span6")
      (:h2 (str (title item)))
      (:p :class "lead" (str (print-price (get-price item))))
      (str (cart-widget item))
      ((:p :class "lead") (str (short-description item)))
      (:p (str (long-description item)))

      (when (not (empty? (get-children-qlist item)))
	(htm (:h5 "Contains")
	     (:ul (dolist (i (items (get-children-qlist item)))
		    (destructuring-bind (ii iq) i
		      (htm (:li (fmt "~A x ~A" iq (title ii))))))))))
     ((:div :class "span6")
      (str (carousel "imageCarousel" (images item) #'full-image-element))))
    
    (:script "$('.carousel').carousel()")))

(defun full-image-element (image)
  (with-html-output-to-string (s)
    ((:p :align "center")
     (:img :src (get-full-url image)))))

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

