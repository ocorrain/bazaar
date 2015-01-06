(in-package #:shopper)

;; set the HTML5 doctype
(setf *prologue* "<!DOCTYPE html>")

;; stores in shopper-sites is an alist of the form ( REGEX . WEB-STORE )

(defun logger (&rest args)
  (apply #'log-message* args))

(defmethod setup-store-controller ((shopper-sites shopper-sites))
  (let ((sc (ele:open-store
	     (list :bdb (namestring (dirconcat (database-root shopper-sites)
					       "store"))))))
    (when sc
      (setf (store-controller shopper-sites) sc))))


(defmethod setup-dispatch-table ((shopper-sites shopper-sites))
  (let ((stores (get-stores shopper-sites)))
    (dolist (store stores)
	  (set-dispatch-table store))))

(defmethod setup-logging ((shopper-sites shopper-sites))
  (let ((root (pathname (database-root shopper-sites))))
    (setf (acceptor-message-log-destination shopper-sites)
	  (merge-pathnames #p"log/message.log" root)
	  (acceptor-access-log-destination shopper-sites)
	  (merge-pathnames #p"log/access.log" root))))

(defmethod start :before ((shopper-sites shopper-sites))
  (setup-dispatch-table shopper-sites)
  (setup-logging shopper-sites)
  shopper-sites)



(defmethod set-dispatch-table ((web-store web-store))
  (setf (dispatch-table web-store)
	(list (create-folder-dispatcher-and-handler "/images/" (image-path web-store))
	      (create-folder-dispatcher-and-handler "/s/" (get-twitter-bootstrap-path))
	      (create-folder-dispatcher-and-handler "/js/" 
								#p"/home/ocorrain/lisp/dev/gallery/js/")
	      (create-regex-dispatcher "^/view/([\\w-]+)/(\\w+)$" #'view)
	      (create-regex-dispatcher "^/edit/.*$"
						   (secure-page #'edit :edit-objects))
	      (create-regex-dispatcher "^/new/([\\w-]+)"
						   (secure-page #'new :edit-objects))
	      (create-regex-dispatcher "^/class/([\\w-]+)"
						   (secure-page #'class-page :edit-objects))
	      (create-regex-dispatcher "^/login$" #'login-page)
	      (create-prefix-dispatcher "/add-to-cart" #'add-to-cart)
	      (create-prefix-dispatcher "/enter-details" #'enter-details)
	      (create-prefix-dispatcher "/place-order" #'place-order)
	      (create-prefix-dispatcher "/admin/orders"
						    (secure-page #'admin-orders :admin-orders))
	      (create-prefix-dispatcher "/order/complete" #'view-completed-order)
	      (create-regex-dispatcher "^/admin/view/order/([\\w-]+)$" #'admin-view)
	      (create-prefix-dispatcher "/admin/edit/store" 
						    (secure-page #'edit-store-page :admin-store))
	      (create-prefix-dispatcher "/shopping-cart" #'shopping-cart)
	      (create-prefix-dispatcher "/process-payment" #'process-payment)
	      (create-regex-dispatcher "^/logout" #'logout)
	      (create-regex-dispatcher "^/delete/.*$" #'delete-obj)
	      (lambda (r) (declare (ignore r)) #'index-page))))

(defmethod stop :after ((shopper-sites shopper-sites) &key soft)
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
			  (progn (logger :debug "ACCEPTOR-DISPATCH-REQUEST: host - ~A ; webstore - ~S" 
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
  (top-edit-bar))

(defun nav-edit-buttons (obj)
  (with-html-output-to-string (s)
    (:li :class ""
	 (:a :href (get-edit-url obj)  "Edit"))
    (:li :class ""
	 (:a :href (get-view-url obj)  "View"))
    (:li :class "divider-vertical")))

(defun nav-new-button ()
  (with-html-output-to-string (s)
    (:li :class "dropdown"
	 (:a :class "dropdown-toggle" :data-toggle "dropdown" :href "#" 
	     "New >") 
	 (:ul :class "dropdown-menu" :role "menu"
	      (dolist (class (store-active-classes *web-store*))
		(str (htm (:li :class ""
			       (:a :href (get-new-url class) 
				   (str (string-capitalize (symbol-name class))))))))))))

(defun nav-classes-menu ()
  (with-html-output-to-string (s)
    (:li :class "dropdown"
	 (:a :class "dropdown-toggle" :data-toggle "dropdown" :href "#" 
	     "Classes >") 
	 (:ul :class "dropdown-menu" :role "menu"
	      (dolist (class (store-active-classes *web-store*))
		(str (htm (:li :class ""
			       (:a :href (get-class-url class)
				   (str (string-capitalize (symbol-name class))))))))))))

(defun nav-orders-button ()
  (with-html-output-to-string (s)
    (:li :class "divider-vertical")
    (:li :class ""
	 (:a :href "/admin/orders"  "Orders"))))

(defun nav-store-button ()
  (with-html-output-to-string (s)
    (:li :class "divider-vertical")
    (:li :class ""
	 (:a :href "/admin/edit/store"  "Store"))))

(defun nav-user-button (user)
  (with-html-output-to-string (s)
    (:ul :class "nav pull-right"
	 (:li :class "dropdown"
	      (:a :class "dropdown-toggle" :data-toggle "dropdown" :href "#"
		  (:i :class "icon-user") (str (current-user)))
	      (:ul :class "dropdown-menu" :role "menu"
		   (:li :class ""
			(:a :href "/logout" "Log out")))))))

(defun top-edit-bar (&optional obj)
  (when-let (user (get-logged-in-user))
    (with-slots (capabilities) user
	(with-html-output-to-string (s nil :indent t)
	  (:div :class "navbar navbar-fixed-bottom"
		(:div :class "navbar-inner"
		      (:a :class "brand" :href "/" (str (store-name *web-store*)))
		      (:ul :class "nav"
			   (when (and obj (has-capability :edit-objects user))
			     (str (nav-edit-buttons obj)))
			   (when (has-capability :edit-objects user)
			     (str (nav-new-button)))
			   
			   (str (nav-classes-menu))
			   (when (has-capability :admin-orders user)
			     (str (nav-orders-button)))
			   (when (has-capability :admin-store user)
			     (str (nav-store-button))))
		      (str (nav-user-button user))))))))




;; REFACTOR
(defun view ()
  (logger :debug "VIEW - webstore is ~S" *web-store*)
  (cl-ppcre:register-groups-bind (class id)
      ("^/view/([\\w-]+)/(\\w+)$" (script-name*))
    (if-let (class-sym (find-cms-class class))
      (progn
	(logger :debug "VIEW - class is ~S" class-sym)
	(logger :debug "VIEW - headers are ~S" (headers-in*))
	(if-let (obj (get-object class-sym id))

	  (if (string-equal (header-in* :accept) "application/json")
	      (json-object-page obj)
	      (object-page obj))
	  (progn
	    (setf (return-code*) +http-not-found+)
	    (abort-request-handler)))) 
      (setf (return-code*) +http-bad-request+))))

(defun admin-view ()
  (logger :debug "ADMIN-VIEW - webstore is ~S" *web-store*)
  (cl-ppcre:register-groups-bind (id)
      ("^/admin/view/order/(\\w+)$" (script-name*))
    (if-let (obj (get-order id))
      (object-page obj)
      (progn
	(setf (return-code*) +http-not-found+)
	(abort-request-handler)))))






(defun log-describe (object)
  (logger :debug "~A" (with-output-to-string (s) (describe object s))))

(defun new ()
  (cl-ppcre:register-groups-bind (class)
      ("^/new/([\\w-]+)" (script-name*))
    (when-let (class-sym (find-cms-class class))
      (if-let (parameters (post-parameters*))
	(maybe-create class-sym (fix-alist parameters))

	(standard-page (format nil "New ~A" (string-capitalize class)) 
		       nil (get-form class-sym))))))

(defun class-page ()
  (cl-ppcre:register-groups-bind (class)
      ("^/class/([\\w-]+)" (script-name*))
    (when-let (class-sym (find-cms-class class))
      (let ((objects (get-all-objects class-sym)))
	(if (string-equal (header-in* :accept) "application/json")
	    (json-object-page objects)
	    (standard-page (string-capitalize class) 
			   nil (class-table class objects)
			   (minimal-edit-bar)))))))



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
      ("^/edit/([\\w-]+)/(\\w+)$" (script-name*))
    (if-let (class-sym (find-cms-class class))
      (progn
	(logger :debug "EDIT - class is ~S" class-sym)
	(if-let (obj (get-object class-sym id))
	  (edit-object-page obj)
	  
	  (progn
	    (setf (return-code*) hunchentoot:+http-not-found+)
	    (abort-request-handler)))) 
      (setf (return-code*) hunchentoot:+http-bad-request+))))


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
  (setf (acceptor-message-log-destination *sites*) *msg-log*)
  (start *sites*))
