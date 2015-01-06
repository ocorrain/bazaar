(in-package :cl-user)

(defpackage #:shopper
  (:use #:cl #:cl-who #:alexandria #:relations #:hunchentoot)
  ;; (:import-from :hunchentoot :redirect 
  ;; 		:parameter :get-parameter :post-parameter :post-parameters* :get-parameters*
  ;; 		:referer :session-value  :delete-session-value
  ;; 		:url-encode :url-decode :content-type*  :script-name*
  ;; 		:content-type* :acceptor-message-log-destination :headers-in* :header-in
  ;; 		)
  (:export #:get-item #:new-item-page #:maybe-create #:maybe-update
	   #:items #:images #:basic-page #:get-image-number-as-string #:get-tag
	   #:tag-members #:tag #:line-item #:fix-alist #:tags #:item #:items
	   #:untag-item #:*web-store* #:edit-store-page #:make-page #:thumbnails
	   #:collect-items-with #:render-thumb #:edit-bar #:published #:featured
	   #:display-item-page #:edit-item-page #:edit-front-page
	   #:image-edit-page #:maybe-add-image #:bundle-edit-page
	   #:maybe-update-bundle #:untag-item #:toggle-tag #:tagged?  #:tag-item
	   #:make-tags-page #:tag-form #:edit-tag-page #:appears-in-menu
	   #:tag-name #:edit-tabs #:tag-display-page #:tag-edit-page #:geo-form
	   #:geo-form-page #:get-geo-edit-url #:geo-members
	   #:get-country-info-from-iso-code #:edit-geographies-page
	   #:edit-item-edit-page #:update-geos #:geo-postage-page
	   #:image-edit-page #:static-content #:get-content-from-webform #:get-webform
	   #:static-content-edit-page #:static-content-new-page
	   #:edit-static-content-page #:delete-object #:edit-object
	   #:edit-object/post #:find-cms-class #:new-object
	   #:edit-multiple-objects #:get-all-objects #:get-object
	   #:find-object-and-page #:view-object-page #:make-designator #:initialize-store
	   #:shopper-sites #:new-web-store #:setup-store-controller #:setup-dispatch-table
	   #:title #:sku #:add-image #:random-elt #:tags #:web-store
	   #:get-stores #:get-images #:get-tagged-objects #:get-tags
	   #:geography))


