(in-package #:shopper)

(restas:define-module #:shopper-edit
  (:use #:cl #:shopper #:restas #:alexandria)
  ;; (:decorators '@http-auth-require)
  (:export #:r/edit

	   #:edit-object-page
	   #:edit-object-page/post

	   #:delete-object-page

	   #:new-object-page
	   #:new-object-page/post
	   
	   #:store/edit/parameters

	   #:geo/edit/postage
	   #:geo/edit 
	   #:geo/delete

	   #:/r/edit
	   #:r/delete-item 

	   #:r/edit-item/view 
	   #:r/edit-item/edit 
	   #:r/edit-item/images 
	   #:r/edit-item/tags 
	   #:r/edit-item/contents 
	   #:r/edit-item/images 

	   #:r/edit-tag/view 
	   #:r/edit-tag/edit 

	   #:new-static-content
	   #:static-content-edit 
	   #:static-content-delete

	   #:edit-multi-page))


(in-package #:shopper-edit)

;; ITEMs, bundles or single items

; new items

(defmacro with-class ((obj string) &body body)
  (let ((g (gensym)))
    `(let ((,g (read-from-string ,string)))
       (hunchentoot:log-message* :info "~S ~S" ,obj ,string)
       (if (and (symbolp ,g) (subtypep ,g 'cms))
	   (let ((,obj ,g))
	     (progn ,@body))
	   hunchentoot:+http-bad-request+))))


(defmacro with-object ((object class identifier page) &body body)
  (let ((g (gensym))
	(pagesym (gensym)))
    `(with-class (class-sym ,class)
       (let ((,pagesym (read-from-string ,page)))
	 (if (and (symbolp ,pagesym)
		  (page-permitted class-sym ,pagesym))
	     (if-let (,g (get-object ,class ,identifier))
	       (let ((,object ,g))
		 ,@body)
	       hunchentoot:+http-not-found+)
	     hunchentoot:+http-bad-request+)))))



(restas:define-route new-object-page
    ("new/:(class)")
  (if-let (keyword (shopper::find-cms-class class))
    (shopper::new-object keyword)))


(restas:define-route new-object-page/post
    ("new/:(class)" :method :post)
  (if-let (keyword (shopper:find-cms-class class))
    (maybe-create keyword (fix-alist (hunchentoot:post-parameters*)))
    hunchentoot:+http-bad-request+))

(restas:define-route edit-object-page
    ("edit/:(class)/:(identifier)/:(page)")
  (multiple-value-bind (obj p)
      (find-object-and-page class identifier page)
    (edit-object obj p)))

(restas:define-route edit-object-page/post
    ("edit/:(class)/:(identifier)/:(page)" :method :post)
  (multiple-value-bind (obj p)
      (find-object-and-page class identifier page)
    (edit-object/post obj p)))

(restas:define-route delete-object-page
    ("delete/:(class)/:(identifier)")
  (when-let* ((class-sym (find-cms-class class))
		(obj (get-object class-sym identifier)))
      (delete-object obj))
  (hunchentoot:redirect (hunchentoot:referer)))



;; Groups of objects
(restas:define-route edit-multi-page
    ("/edit/m/:class")
  (if-let (class-sym (find-cms-class class))
    (edit-multiple-objects class-sym (get-all-objects class-sym))
    hunchentoot:+http-bad-request+))

(restas:define-route /r/edit
    ("/edit")
  (edit-front-page))

(restas:define-route r/edit-items
    ("edit/items")
  (edit-item-page #'identity "All items"))

(restas:define-route r/edit-published
    ("edit/items/published")
  (edit-item-page #'published "Published items"))

(restas:define-route r/edit-unpublished
    ("edit/items/unpublished")
  (edit-item-page (lambda (item) (not (published item))) "Unpublished items"))

(restas:define-route r/edit-featured
    ("edit/items/featured")
  (edit-item-page #'featured "Featured items"))


;; (restas:define-route r/edit-item/view 
;;     ("edit/item/:(sku)/view")
;;   (if-let (item (get-item sku))
;;     (display-item-page item)
;;     hunchentoot:+http-not-found+))

;; (restas:define-route r/edit-item/images
;;     ("edit/item/:(sku)/images")
;;   (if-let (item (get-item sku))
;;     (progn
;;       (when-let (image-to-delete (hunchentoot:get-parameter "delete"))
;; 	(setf (images item) (remove-if (lambda (i)
;; 					 (string-equal image-to-delete
;; 						       (get-image-number-as-string i)))
;; 				       (images item))))
;;       (image-edit-page item))
;;     hunchentoot:+http-not-found+))

;; (restas:define-route r/edit-item/images/post
;;     ("edit/item/:(sku)/images" :method :post)
;;   (if-let (item (get-item sku))
;;     (progn
;;       (when-let (picture (hunchentoot:post-parameter "picture"))
;; 	(maybe-add-image picture item))
;;       (image-edit-page item))
;;     hunchentoot:+http-not-found+))

;; (restas:define-route r/edit-item/contents
;;     ("edit/item/:(sku)/contents")
;;   (if-let (item (get-item sku))
;;     (bundle-edit-page item)
;;     hunchentoot:+http-not-found+))

;; (restas:define-route r/edit-item/contents/post
;;     ("edit/item/:(sku)/contents" :method :post)
;;   (if-let (item (get-item sku))
;;     (progn (maybe-update-bundle item)
;; 	   (bundle-edit-page item))
;;     hunchentoot:+http-not-found+))

;; (restas:define-route r/delete-tag
;;     ("delete/tag/:(tag)")
;;   (if-let (obj (get-tag tag))
;;     (progn
;;       (delete-object obj)
;;       (hunchentoot:redirect (hunchentoot:referer))) 
;;     hunchentoot:+http-not-found+))

;; (restas:define-route r/edit-item/tags
;;     ("edit/item/:(sku)/tags")
;;   (if-let (item (get-item sku))
;;     (progn
;;       (when-let (toggle-tag (hunchentoot:get-parameter "tag"))
;; 	(when-let (toggle-tag-obj (get-tag toggle-tag))
;; 	  (if (tagged? item toggle-tag-obj)
;; 	      (untag-item item toggle-tag-obj)
;; 	      (tag-item item toggle-tag-obj))))
;;       (make-tags-page item)) 
;;     hunchentoot:+http-not-found+))

;; (restas:define-route r/edit-item/contents
;;     ("/edit/item/:(sku)/contents")
;;   (cl-who:with-html-output-to-string (s)
;;     (format s "Editing ~A contents" sku)))

;; (restas:define-route r/edit-item/edit
;;     ("edit/item/:(sku)/edit")
;;   (if-let (item (get-item sku))
;;     (edit-item-edit-page item)
;;     hunchentoot:+http-not-found+))

;; (restas:define-route r/edit-item/edit/post
;;     ("edit/item/:(sku)/edit" :method :post)
;;   (if-let (item (get-item sku))
;;     (progn
;;       (update-geos item (hunchentoot:post-parameters*))
;;       (maybe-update item (fix-alist (hunchentoot:post-parameters*)))
;;       (edit-item-edit-page item))
;;     hunchentoot:+http-not-found+))

;; TAGS

;; (restas:define-route r/new-tag
;;     ("/new/tag")
;;   (make-page "Create new tag" (tag-form)
;; 	     :sidebar (edit-bar "New tag")))

;; (restas:define-route r/new-tag/post
;;     ("/new/tag" :method :post)
;;   (maybe-create 'tag (fix-alist (hunchentoot:post-parameters*))))

(restas:define-route r/edit-tags
    ("/edit/tags")
  (edit-tag-page #'identity "All tags"))

(restas:define-route r/edit-menu-tags
    ("/edit/tags/menu")
  (edit-tag-page #'appears-in-menu "Menu tags"))

(restas:define-route r/edit-featured-tags
    ("/edit/tags/featured")
  (edit-tag-page (lambda (tag)
		   (and (featured tag) (appears-in-menu tag))) "Featured tags"))

;; (restas:define-route r/edit-tag/view 
;;     ("/edit/tag/:(tag)/view")
;;   (if-let (tag-object (get-tag tag))
;;     (make-page (tag-name tag-object)
;; 	       (concatenate 'string
;; 			    (edit-tabs tag-object "View")
;; 			    (tag-display-page tag-object))
	       
	       
;; 	       :sidebar (edit-bar (tag-name tag-object)))
;;     hunchentoot:+http-not-found+))


;; (restas:define-route r/edit-tag/edit
;;     ("/edit/tag/:(tag)/edit")
;;   (if-let (tag-obj (get-tag tag))
;;     (tag-edit-page tag-obj)
;;     hunchentoot:+http-not-found+))

;; (restas:define-route r/edit-tag/edit/post
;;     ("/edit/tag/:(tag)/edit" :method :post)
;;   (if-let (tag-obj (get-tag tag))
;;     (progn
;;       (maybe-update tag-obj (fix-alist (hunchentoot:post-parameters*)))
;;       (tag-edit-page tag-obj))
;;     hunchentoot:+http-not-found+))

;; geography

;; (restas:define-route geo/new
;;     ("/new/geo")
;;   (make-page "Create a new geography" (geo-form)
;; 	     :sidebar (edit-bar "New geography")))

;; (restas:define-route geo/new/post
;;     ("/new/geo" :method :post)
;;   (when-let (name (hunchentoot:post-parameter "title"))
;;     (if-let (already-existing (ele:get-instance-by-value 'geography
;; 							  'geography-name
;; 							  name))
;; 	     (hunchentoot:redirect (get-geo-edit-url already-existing))
;; 	     (let ((new-geo (make-instance 'geography :name name)))
;; 	       (setf (geo-members new-geo)
;; 		     (remove-if-not (lambda (p)
;; 				      (get-country-info-from-iso-code p))
;; 				    (mapcar #'car (hunchentoot:post-parameters*))))
;; 	       (hunchentoot:redirect (get-geo-edit-url new-geo))))))

;; (restas:define-route geo/delete
;;     ("geo/delete/:(geoid)")
;;   (when-let (geo (ele:get-instance-by-value 'geography
;; 					    'geography-name
;; 					    (hunchentoot:url-decode geoid)))
;;     (ele:drop-instance geo)
;;     (hunchentoot:redirect "/edit/geos")))

(restas:define-route geos/edit
    ("edit/geos")
  (edit-geographies-page))

;; (restas:define-route geo/edit
;;     ("edit/geo/:(geoid)")
;;   (geo-form-page geoid))

;; (restas:define-route geo/edit/post
;;     ("edit/geo/:(geoid)" :method :post)
;;   (geo-form-page geoid (hunchentoot:post-parameters*)))

;; (restas:define-route geo/edit/postage
;;     ("edit/geo/:(geoid)/postage")
;;   (shopper::geo-postage-page geoid (hunchentoot:get-parameters*)))

(restas:define-route store/edit/parameters
    ("edit/store")
  (edit-store-page))

(restas:define-route store/edit/parameters/post
    ("edit/store" :method :post)
  (edit-store-page (hunchentoot:post-parameters*)))

;; static content
;; (restas:define-route static-content-edit
;;     ("/edit/static/:(contentform)")
;;   (if-let (content (get-content-from-webform contentform))
;;     (static-content-edit-page content)))

;; (restas:define-route static-content-edit/post
;;     ("/edit/static/:(contentform)" :method :post)
;;   (if-let (content (get-content-from-webform contentform))
;;     (progn
;;       (maybe-update content (fix-alist (hunchentoot:post-parameters*)))
;;       (static-content-edit-page content))))


;; (restas:define-route new-static-content
;;     ("/new/static")
;;   (static-content-new-page))

;; (restas:define-route new-static-content/post
;;     ("/new/static" :method :post)
;;   (maybe-create 'static-content (fix-alist (hunchentoot:post-parameters*))))

;; (restas:define-route static-content-delete
;;     ("static/delete/:(contentform)")
;;   (when-let (content (get-content-from-webform contentform))
;;     (ele:drop-instance content)
;;     (restas:redirect 'static-content-edit-view)))

(restas:define-route static-content-edit-view
    ("edit/all/static")
  (edit-static-content-page))
