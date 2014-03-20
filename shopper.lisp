(in-package #:shopper)

;; set the HTML5 doctype
(setf *prologue* "<!DOCTYPE html>")

;; stores in shopper-sites is an alist of the form ( REGEX . WEB-STORE )


(defmethod hunchentoot:start :before ((shopper-sites shopper-sites))
  (let ((sc (ele:open-store
	     (list :bdb (namestring (dirconcat (database-root shopper-sites)
					       "store"))))))
    (when sc
      (setf (store-controller shopper-sites) sc)
      (let ((stores (ele:get-instances-by-class 'web-store )))
	(dolist (store stores)
	  (set-dispatch-table store))
	(setf (get-stores shopper-sites) stores))))
  shopper-sites)



(defmethod set-dispatch-table ((web-store web-store))
  (setf (dispatch-table web-store)
	(list (hunchentoot:create-folder-dispatcher-and-handler "/images/" (image-path web-store))
	      (hunchentoot:create-folder-dispatcher-and-handler "/s/" (get-twitter-bootstrap-path))
	      (hunchentoot:create-folder-dispatcher-and-handler "/js/" 
								#p"/home/ocorrain/lisp/dev/gallery/js/")
	      (hunchentoot:create-regex-dispatcher "^/view/([\\w-]+)/(\\w+)$" #'view)
	      (hunchentoot:create-regex-dispatcher "^/edit/.*$"
						   (secure-page #'edit :edit-objects))
	      (hunchentoot:create-regex-dispatcher "^/new/([\\w-]+)"
						   (secure-page #'new :edit-objects))
	      (hunchentoot:create-regex-dispatcher "^/class/([\\w-]+)"
						   (secure-page #'class-page :edit-objects))
	      (hunchentoot:create-regex-dispatcher "^/login$" #'login-page)
	      (hunchentoot:create-prefix-dispatcher "/add-to-cart" #'add-to-cart)
	      (hunchentoot:create-prefix-dispatcher "/enter-details" #'enter-details)
	      (hunchentoot:create-prefix-dispatcher "/place-order" #'place-order)
	      (hunchentoot:create-prefix-dispatcher "/admin/orders"
						    (secure-page #'admin-orders :admin-orders))
	      (hunchentoot:create-prefix-dispatcher "/order/complete" #'view-completed-order)
	      (hunchentoot:create-regex-dispatcher "^/admin/view/order/([\\w-]+)$" #'admin-view)
	      (hunchentoot:create-prefix-dispatcher "/admin/edit/store" 
						    (secure-page #'edit-store-page :admin-store))
	      (hunchentoot:create-prefix-dispatcher "/shopping-cart" #'shopping-cart)
	      (hunchentoot:create-prefix-dispatcher "/process-payment" #'process-payment)
	      (hunchentoot:create-regex-dispatcher "^/logout" #'logout)
	      (hunchentoot:create-regex-dispatcher "^/delete/.*$" #'delete-obj)
	      (lambda (r) (declare (ignore r)) #'index-page))))

(defmethod hunchentoot:stop :after ((shopper-sites shopper-sites) &key soft)
  (declare (ignore soft))
  (ele:close-store (store-controller shopper-sites))
  shopper-sites)

(defmethod hunchentoot:acceptor-dispatch-request ((shopper-sites shopper-sites) request)
  (dolist (store (get-stores hunchentoot:*acceptor*))
    (if (cl-ppcre:scan (store-regex store) (hunchentoot:host))
	(mapc (lambda (dispatcher)
		  (let ((handler (funcall dispatcher request)))
		    (when handler ; Handler found. FUNCALL it and return result
		      (return-from tbnl:acceptor-dispatch-request 
			(if-let (*web-store* store)
			  (progn (hunchentoot:log-message* :error "~A : webstore is ~S" 
							   (hunchentoot:host) *web-store*)
				 (funcall handler))
			  (error "No web store"))))))
		(dispatch-table store))))
  (call-next-method))

(defmethod get-headers ((cms cms))
  nil)

(defmethod print-class-of ((cms cms))
  (format nil "~A" (string-capitalize (symbol-name (type-of cms)))))

(defun get-class-url (class-sym)
  (format nil "/class/~A" (string-downcase (symbol-name class-sym))))

(defun minimal-edit-bar ()
  (with-html-output-to-string (s)
    (:div :class "navbar navbar-fixed-bottom"
	  (:div :class "navbar-inner"
		(:a :class "brand" :href "/" (str (store-name *web-store*)))
		(:ul :class "nav"
		     
		     (:li :class "dropdown"
			  (:a :class "dropdown-toggle" :data-toggle "dropdown" :href "#" 
			      "New >") 
			  (:ul :class "dropdown-menu" :role "menu"
			       (dolist (class (store-active-classes *web-store*))
				 (str (htm (:li :class ""
						(:a :href (get-new-url class) 
						    (str (string-capitalize (symbol-name class))))))))))
		     (:li :class "dropdown"
			  (:a :class "dropdown-toggle" :data-toggle "dropdown" :href "#" 
			      "Classes >") 
			  (:ul :class "dropdown-menu" :role "menu"
			       (dolist (class (store-active-classes *web-store*))
				 (str (htm (:li :class ""
						(:a :href (get-class-url class)
						    (str (string-capitalize (symbol-name class)))))))))))
		(:ul :class "nav pull-right"
		     (:li :class "dropdown"
			  (:a :class "dropdown-toggle" :data-toggle "dropdown" :href "#"
			      (:i :class "user") (str (current-user)))
			  (:ul :class "dropdown-menu" :role "menu"
			       (:li :class ""
				    (:a :href "/logout" "Log out")))))))))


(defun top-edit-bar (obj)
  (with-html-output-to-string (s nil :indent t)
    (:div :class "navbar navbar-fixed-bottom"
	  (:div :class "navbar-inner"
		(:a :class "brand" :href "/" (str (store-name *web-store*)))
		(:ul :class "nav"
		     (:li :class ""
			  (:a :href (get-edit-url obj)  "Edit"))
		     (:li :class ""
			  (:a :href (get-view-url obj)  "View"))
		     (:li :class "divider-vertical")
		     (:li :class "dropdown"
			  (:a :class "dropdown-toggle" :data-toggle "dropdown" :href "#" 
			      "New >") 
			  (:ul :class "dropdown-menu" :role "menu"
			       (dolist (class (store-active-classes *web-store*))
				 (str (htm (:li :class ""
						(:a :href (get-new-url class) 
						    (str (string-capitalize (symbol-name class))))))))))
		     (:li :class "dropdown"
			  (:a :class "dropdown-toggle" :data-toggle "dropdown" :href "#" 
			      "Classes >") 
			  (:ul :class "dropdown-menu" :role "menu"
			       (dolist (class (store-active-classes *web-store*))
				 (str (htm (:li :class ""
						(:a :href (get-class-url class)
						    (str (string-capitalize (symbol-name class)))))))))))
		(:ul :class "nav pull-right"
		     (:li :class "dropdown"
			  (:a :class "dropdown-toggle" :data-toggle "dropdown" :href "#"
			      (:i :class "icon-user") (str (current-user)))
			  (:ul :class "dropdown-menu" :role "menu"
			       (:li :class ""
				    (:a :href "/logout" "Log out")))))))))



;; REFACTOR
(defun view ()
  (hunchentoot:log-message* :error "webstore is ~S" *web-store*)
  (cl-ppcre:register-groups-bind (class id)
      ("^/view/([\\w-]+)/(\\w+)$" (hunchentoot:script-name*))
    (if-let (class-sym (find-cms-class class))
      (progn
	(hunchentoot:log-message* :error "class is ~S" class-sym)
	(hunchentoot:log-message* :error "~S" (hunchentoot:headers-in*))
	(if-let (obj (get-object class-sym id))

	  (if (string-equal (hunchentoot:header-in* :accept) "application/json")
	      (json-object-page obj)
	      (object-page obj))
	  (progn
	    (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
	    (hunchentoot:abort-request-handler)))) 
      (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))

(defun admin-view ()
  (hunchentoot:log-message* :error "webstore is ~S" *web-store*)
  (cl-ppcre:register-groups-bind (id)
      ("^/admin/view/order/(\\w+)$" (hunchentoot:script-name*))
    (if-let (obj (get-order id))
      (object-page obj)
      (progn
	(setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
	(hunchentoot:abort-request-handler)))))






(defun log-describe (object)
  (hunchentoot:log-message* :debug "~A"
			    (with-output-to-string (s)
			      (describe object s))))

(defun new ()
  (cl-ppcre:register-groups-bind (class)
      ("^/new/([\\w-]+)" (hunchentoot:script-name*))
    (when-let (class-sym (find-cms-class class))
      (if-let (parameters (hunchentoot:post-parameters*))
	(maybe-create class-sym (fix-alist parameters))

	(standard-page (format nil "New ~A" (string-capitalize class)) 
		       nil (get-form class-sym))))))

(defun class-page ()
  (cl-ppcre:register-groups-bind (class)
      ("^/class/([\\w-]+)" (hunchentoot:script-name*))
    (when-let (class-sym (find-cms-class class))
      (let ((objects (get-all-objects class-sym)))
	(if (string-equal (hunchentoot:header-in* :accept) "application/json")
	    (json-object-page objects)
	    (standard-page (string-capitalize class) 
			   nil (list (class-table class objects)
				     (minimal-edit-bar))))))))



(defun class-list (class)
  (let ((objects (sort (get-all-objects class) #'string-lessp :key #'title)))
    (with-html-output-to-string (s)
      (:h3 (fmt "Class: ~A ~S members" (string-upcase class) (length objects)))
      (:ul
       (dolist (o objects)
	 (htm (:li (:a :href (get-view-url o) (str (title o))))))))))

(defun class-table (class objects)
  (let ((sorted-objects (sort objects #'string-lessp :key #'title)))
    (with-html-output-to-string (s)
      (:table
       (:tr
	(dolist (header (table-row class))
	  (htm (:th (str header)))))
       (dolist (o sorted-objects)
	 (htm (:tr
	       (dolist (c (table-row o))
		 (htm (:td (str c)))))))))))

(defmethod table-row ((class t))
  (list "Title" "Featured" "Published"))

(defun view-link (obj)
  (with-html-output-to-string (s) (:a :href (get-view-url obj) (str (title obj)))))

(defmethod table-row ((cms cms))
  (list (view-link cms)
	(if (featured cms)
	    "X" "")
	(if (published cms)
	    "X" "")))

;; REFACTOR
(defun edit ()
  (cl-ppcre:register-groups-bind (class id)
      ("^/edit/([\\w-]+)/(\\w+)$" (hunchentoot:script-name*))
    (if-let (class-sym (find-cms-class class))
      (progn
	(hunchentoot:log-message* :error "class is ~S" class-sym)
	(if-let (obj (get-object class-sym id))
	  (edit-object-page obj)
	  
	  (progn
	    (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
	    (hunchentoot:abort-request-handler)))) 
      (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+))))


(defun delete-obj ()
  "Delete page")

(defun get-valid-objects-from-post (parameters)
  (let ((valid '()))
    (dolist (p parameters)
      (when-let (item (get-item (car p)))
	(when-let (quantity (validate-number (cdr p)))
	  (push (cons item quantity) valid))))
    valid))

(defun add-get-parameters-to-url (url alist)
  (dolist (entry alist)
    (setf url (url-rewrite:add-get-param-to-url
	       url
	       (format nil "~A" (car entry))
	       (format nil "~A" (cdr entry)))))
  url)


(defun boot ()
  (defvar *sites* (make-instance 'shopper-sites :port 9292 :db-root "/home/ocorrain/testsites"))
  (defvar *msg-log* (open "/home/ocorrain/sites.log" :direction :output :if-exists :append :if-does-not-exist :create))
  (setf (hunchentoot:acceptor-message-log-destination *sites*) *msg-log*)
  (hunchentoot:start *sites*))
