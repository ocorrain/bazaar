(in-package #:shopper)

(defun main-nav-tabs ()
  `(("/" . "Home")))

(defun main-navigation (&optional navigation)
  (with-html-output-to-string (s)
    (:div :class "navbar"
	  (str (nav-tabs (main-navigation-tabs *web-store*)
			 navigation :class "nav nav-tabs"))
 	  (str (navigation-cart)))))

(defmethod main-navigation-tabs ((store-type (eql :web-store)))
  (append (main-nav-tabs)
					;			     (static-content-nav)
	  (tag->nav (featured-tags))
	  ;; (nav-dropdown "More Chocolate!"
	  ;; 		(tag->nav (menu-tags)))
	  ))

(defmethod main-navigation-tabs (store-type)
  (append (main-nav-tabs)
					;			     (static-content-nav)
	  (tag->nav (featured-tags))
	  ;; (nav-dropdown "More Chocolate!"
	  ;; 		(tag->nav (menu-tags)))
	  ))

(defun login-tabs ()
  (when (session-value :user)
    '(("/edit" . "Edit")
      ("/logout" . "Log out"))))

(defun nav-dropdown (label nav)
  (list (cons nav label)))

(defun navigation-cart ()
  (if-let ((store-open-p (store-open *web-store*))
	   (cart (get-cart)))
    (with-html-output-to-string (s)
      ;; ((:a :href "/enter-details" :class "pull-right btn btn btn-primary")
      ;;  "CHECKOUT")
      ((:a :href "/shopping-cart" :class "pull-right btn")
       (:i :class "icon-shopping-cart")
       (fmt "~A items (~A)  Checkout." (count-items-in cart) (print-price (get-price cart)))
       (:i :class "icon-chevron-right")))
    ""))

(defun nav-tabs (alist active &key (class "nav nav-tabs"))
  "ALIST cells of the form (CONTENT . LABEL), NIL for dividers, or
plain strings for headers.  CONTENT can be an alist, in which case a
submenu is created"
  (with-html-output-to-string (s)
    ((:ul :class class)
     (dolist (item alist)
       (cond ((stringp item) (htm ((:li :class "nav-header") (str item))))
	     ((null item) (htm (:li :class "divider")))
	     (t (destructuring-bind (url . label)
		    item
		  (cond ((listp url)
			 (htm ((:li :class "dropdown")
			       ((:a :class "dropdown-toggle"
				    :data-toggle "dropdown"
				    :href "#")
				(str label)
				(:b :class "caret"))
			       (str (nav-tabs url nil :class "dropdown-menu")))))
			(t (if (string-equal label active)
			       (htm ((:li :class "active")
				     ((:a :href url)
				      (str label))))
			       (htm (:li
				     ((:a :href url)
				      (str label))))))))))))))
