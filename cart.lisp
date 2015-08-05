;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(ele:defpclass shopping-cart (quantity-list)
  ((cookie :initarg :cookie :initform nil :accessor cookie :index t
	   :documentation "The cookie id corresponding to this shopping cart")
   (last-active :initform (get-universal-time) :accessor last-active
		:documentation "The last time this cart was accessed")
   (postage :initform nil :accessor postage :documentation "Postage price associated with this cart")
   (customer :initarg :customer :initform nil :accessor customer
	     :documentation "Object holding customer details for this cart")))

(defmethod reify ((cart shopping-cart))
  (let ((items '()))
    (dolist (i (items cart))
      (let ((line-item (qlist-entry-item i))
	    (quantity (qlist-entry-quantity i)))
	(push (list (sku line-item)
		    (title line-item)
		    quantity
		    (price line-item)
		    (weight line-item))
	      items)))
    items))


(defmethod count-items-in ((cart shopping-cart))
  (reduce #'+ (mapcar #'qlist-entry-quantity (items cart))))

(defun random-item ()
  (random-elt (ele:get-instances-by-class 'single-item)))

(defmethod add-item :after ((item line-item) (cart shopping-cart) quantity)
  "Update the last-active field if something is added"
  (setf (last-active cart) (get-universal-time)))

(defmethod remove-item :after ((item line-item) (cart shopping-cart))
    "Update the last-active field if something is removed"
  (setf (last-active cart) (get-universal-time)))

(defmethod empty-qlist :after ((cart shopping-cart))
  "Update the last-active field if the cart is emptied"
  (setf (last-active cart) (get-universal-time)))

(defgeneric cart-widget (item))

(defmethod cart-widget ((item line-item))
  "Widget that goes on display item pages with Add to cart
  functionality"
  (if (store-open *web-store*)
      (with-html-output-to-string (s)
	((:div :id "cart")
	 ((:form :action "/add-to-cart" :method "post")
	   ((:div :class "input-append")
	    ((:button :class "btn btn-success" :type "submit") "Add to cart")
	    ((:select :class "input-mini" :name "number" :id "number")
	     ((:option :value 1 :selected "selected") (str 1))
	     (dotimes (i 49)
	       (htm ((:option :value (+ i 2))
		     (str (+ i 2))))))
	    (:input :type "hidden" :name "sku" :value (sku item))
	    ))))
      ""))

(defun add-to-cart ()
  (maybe-add-items-to-cart)
  (redirect (referer)))

(defun shopping-cart ()
  (store-open-dependent-page #'shopping-cart-page))

(defun enter-details ()
  (store-open-dependent-page #'enter-details-page))

(defun place-order ()
  (store-open-dependent-page #'order-edit-details-page))

(defun get-or-initialize-cart ()
  (if-let (cart (session-value :cart))
    cart
    (let ((cart (make-instance 'shopping-cart)))
      (setf (session-value :cart) cart)
      cart)))

(defun get-cart ()
  (session-value :cart))

;; (defun display-shopping-cart ()
;;   (let ((cart (get-or-initialize-cart)))
;;     (standard-page "Shopping cart"
;; 	      (lambda (stream)
;; 		(print-shopping-cart cart stream)))))

;; (defmethod display-link ((item line-item))
;;   (with-html-output-to-string (s)
;;     ((:a :href (get-url item))
;;      (str (title item)))))

(defun print-shopping-cart (cart)
  (with-html-output-to-string (s)
    (if (empty? cart)
	(htm (str "The shopping cart is empty"))
	(htm (:table (:thead
		      (:tr (:th "Quantity") (:th "Price")
			   (:th "Item")))
		     (:tfoot
		      (:tr (:th (fmt "Total weight: ~Ag" (get-weight cart)))
			   (:th (str (print-price (get-price cart))))
			   (:th "TOTAL")))
		     (:tbody
		      (dolist (i (items cart))
			(let* ((quantity (qlist-entry-quantity i))
			       (item (qlist-entry-item i))
			       (price (get-price item)))
			  (htm (:tr (:td (str quantity))
				    (:td (str (print-price (* price quantity))))
				    (:td ((:a :href (get-view-url item))
					  (str (title item)))
					 ;; (when (typep item 'bundle)
					 ;;   (funcall (simple-bundle-list item) s))
					 )))))))))))



(defun shopping-cart-form (cart)
  (with-html-output-to-string (s)
    ((:div :class "row")
     ((:div :class "span7")
      (:h3 "Shopping cart")
      (:p "These are the items in your shopping cart.  You can edit
    the number of each item by entering the quantity beside the item
    and clicking 'Update'.  To remove an item, set its quantity to zero."))
     ((:div :class "span5")
      ((:div :class "well well-small")
       ((:dl :class "dl-horizontal")
	(:dt "Total price")
	(:dd (str (print-price (get-price cart))))
	(:dt "Weight")
	(:dd (fmt "~Ag" (get-weight cart))))
       )))
    
    (if (items cart)
	(htm ((:a :href "/enter-details" :class "pull-right btn btn-primary")
	      "Enter details & check out>>")
	     ((:form :class "form-horizontal" :action "/shopping-cart" :method :post)
	      ((:div :class "row")
	       ((:button :type "submit" :class "btn pull-left") "Update totals"))
	      (:hr)
	      (dolist (i (items cart))
		(destructuring-bind (item quantity) i
		  (htm ((:div :class "row")
			((:div :class "span2")
			 (:input :type "text" :class "input-mini" :name (sku item)
				 :value quantity))
			((:div :class "span2")
			 (str (display-an-image item (thumbnail-element 100 100))))
			((:div :class "span8")
			 (:h5 ((:a :href (get-view-url item))
			       (str (title item))))
			 (:p (str (short-description item)))
			 (:p (:em "Item price: ") (str (print-price (get-price item)))))))))))
	(htm (:strong (str "Your shopping cart is empty"))))))


(defun all-carts ()
  (ele:get-instances-by-class 'shopping-cart))

(defun cart->order (cart)
  (make-instance 'order
		 :order-state :order-generated
		 :order-timestamps (list (cons :order-generated (get-universal-time)))
		 :cart cart
		 :reified-cart (reify cart)
		 :postage (postage cart)
		 :customer (get-customer)
		 :order-price (get-price cart)))

(defun process-payment ()
  (if-let ((stripe-token (parameter "stripeToken"))
	   (cart (get-cart)))
    (if-let ((stripe-reply (ignore-errors (stripe:create-charge :amount (+ (get-price cart)
									   (cdr (postage cart)))
								:currency "eur"
								:card stripe-token
								:api-key (stripe-api-key *web-store*)))))
      (if-let ((paid (stripe::sstruct-get stripe-reply :paid))
	       (id (stripe:sstruct-get stripe-reply :id)))
	(if (eql paid :true)
	    (let ((order (cart->order cart)))
	      (setf (session-value :cart) nil)
	      (setf (session-value :last-order) order)
	      (setf (correlation-id order) id)
	      (view-completed-order))
	    (standard-page "Stripe error" nil
			   (with-html-output-to-string (s)
			     (:h1 "Payment error")
			     (:p "Your payment did not go through.")
			     (:p (:i (str (stripe:sstruct-get stripe-reply :failure-message)))))))
	
	(standard-page "Stripe error" nil
		       (with-html-output-to-string (s)
			 (:h1 "Payment error")
			 (:p "Your payment did not go through.")
			 (:p (:i "No reply from stripe")))))
      (setf (return-code*) hunchentoot:+http-service-unavailable+))))

(defun escape-describe (object)
  (with-html-output-to-string (str)
    (:pre (str (escape-string-all (with-output-to-string (s)
				    (describe object s)
				    (format s "~S" object)))))))


(defun stripe-checkout (cart &optional (postage 0) (label "Pay now with stripe"))
  (let ((customer (get-or-initialize-customer)))
    (with-html-output-to-string (s nil :indent t)
      (:form :action "/process-payment" :method :post
	     (:script
	      :src "https://checkout.stripe.com/checkout.js" :class "stripe-button"
	      :data-key "pk_test_aq2axR6BzY9AL03K5U3oc2Lb"
	      :data-amount (+ (get-price cart) postage)
	      :data-name (store-name *web-store*)
	      :data-email (email customer)
	      :data-description (format nil "~A items (~A)" (reduce #'+ (mapcar #'second (items cart)))
					(print-price (get-price cart)))
	      :data-image (when-let (br (get-branding-relation (get-branding *web-store*)
                                                               :thumbnail))
                            (image-web-path br)) 
	      :data-currency "EUR"
	      :data-label label)))))

(defun maybe-update-cart (cart parameters)
  (dolist (item-q (get-valid-objects-from-post parameters))
    (destructuring-bind (item . quantity) item-q
      (set-item-quantity item cart quantity))))

(defun order-edit-details-page ()
  (multiple-value-bind (customer errors)
      (maybe-create/update-customer)
    (multiple-value-bind (valid verrors)
	(is-valid-customer? customer)
      (if valid
	  (basic-finalize-page)
	  (basic-edit-address-page (append errors (list verrors)))))))


(defun get-valid-rate (rates)
  (logger :debug "rates: ~S" rates)
  (if (= (length rates) 1)
      (car rates)
      (when-let (shipping  (parameter "shipping"))
	(find shipping rates :key #'car :test #'string-equal))))


(defun view-rates (cart customer)
  (with-html-output-to-string (s)
    (when-let (rates (postage-options cart customer))
      (progn (when-let (valid-rate (get-valid-rate rates))
	       (destructuring-bind (name . cost) valid-rate
		 (htm (str (stripe-checkout cart cost (format nil "Order now via ~A" name)))))))

      (when (>= (length rates) 2)
	(htm (:h5 "Choose a shipping method")
	     (:form (:table :class "table"
			    (:thead (:tr (:th "Shipping method")
					 (:th "Shipping cost")
					 (:th "TOTAL")
					 (:th "")))
			    
			    (:tbody
			     (dolist (rate rates)
			       (destructuring-bind (name . cost) rate
				 (htm (:tr (:td (str name))
					   (:td (str (print-price cost)))
					   (:td (str (print-price (+ cost (get-price cart)))))
					   (:td (:input :type "radio" :name "shipping" :id "shippingRadios1"
							:value (str name)))))))))
		    (:button :class "btn btn-info pull-right" :type "submit" "Continue")))))))


(defun checkout-cart (cart)
  (with-html-output-to-string (s)
    (:h4 "Order contents")
    ((:a :href "/shopping-cart" :class "btn btn-small pull-right")
     "Change shopping cart")
    (:table :class "table"
	    (:thead (:tr  (:th "Item") (:th "Unit price") (:th "Quantity") (:th "Subtotal")))
	    (:tfoot (:tr  (:th "") (:th "") (:th "Total (-pp)") (:th (str (print-price (get-price cart)))))
		    (when-let (postage (postage cart))
		      (destructuring-bind (name . cost) postage
			(htm (:tr (:th "") (:th (str name) (:th "Shipping")
						(:th (str (print-price cost)))))
			     (:tr  (:th "") (:th "") (:th "TOTAL") (:th (str (print-price (+ cost (get-price cart)))))))))
		    
		    
		    (:tr  (:th "") (:th "") (:th "Total weight") (:th (fmt "~Ag" (get-weight cart)))))
	    
	    (dolist (i (items cart))
	      (destructuring-bind (item quantity) i
		(htm (:tr (:td (str (title item)))
			  (:td (str (print-price (get-price item))))
			  (:td (str quantity))
			  (:td (str (print-price (* quantity (get-price item))))))))))))

(defun invalid-items (invalid)
  (when invalid
    (with-html-output-to-string (s)
      ((:div :class "alert alert-error")
       (:p "The following items are not available in your geographical area.  They have been removed from your shopping cart")
       (:ul
	(dolist (inv invalid)
	  (htm (:li (str (title (qlist-entry-item inv)))))))))))

(defun basic-finalize-page ()
  (if-let ((cart (get-cart))
	   (customer (get-customer)))
    (multiple-value-bind (valid invalid)
	(check-order cart customer)
      (if valid
	  (if-let (rates (postage-options cart customer))
	    (progn
	      (when-let (valid-rate (get-valid-rate rates))
		(setf (postage cart) valid-rate))
	      (setf (items cart) valid)
	      (standard-page "Finalize order" nil
			     (with-html-output-to-string (s)
			       ((:div :class "container")
				(:div :class "row"
				      (:div :class "span3"
					    (str (customer-details customer)))
				      (:div :class "span9"
					    (str (invalid-items invalid))
					    (str (checkout-cart cart))
					    (str (view-rates cart customer))))))))
	    (standard-page "No providers" nil
			   (with-html-output-to-string (s)
			     ((:div :class "alert alert-error")
			      (:p "We're sorry, but we have no postal
			      providers servicing your country.  We
			      will not be able to complete this
			      order.")))))
	  (standard-page "No geographies" nil
			 (with-html-output-to-string (s)
			   ((:div :class "alert alert-error")
			      (:p "We're sorry, but none of the items
			      in your cart are available in your
			      geography.  We will not be able to
			      complete this order."))))  ))))

(defun maybe-add-items-to-cart ()
  (let* ((sku (post-parameter "sku"))
	 (quantity (post-parameter "number"))
	 (sku-item (get-item sku))
	 (valid-quantity (validate-number quantity)))
    (when (and sku-item valid-quantity)
      (add-item sku-item (get-or-initialize-cart) valid-quantity))))
