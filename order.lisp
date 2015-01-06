;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(defun get-next-order (&optional (store *web-store*))
  (ele:with-transaction ()
      (prog1
	  (format nil "~A~7,'0d" (order-prefix store) (order-counter store))
	(incf (order-counter store)))))

(ele:defpclass order ()
  ((order-number :initarg :order-number :initform (get-next-order) :accessor order-number
		 :index t)
   (order-state :initarg :order-state :initform nil :accessor order-state)
   (order-timestamps :initarg :order-timestamps :initform nil :accessor order-timestamps)
   (customer :initarg :customer :initform nil :accessor customer
	     :documentation "Object holding customer details for this cart")
   (cart :initarg :cart :initform nil :accessor cart)
   (reified-cart :initarg :reified-cart :initform nil :accessor reified-cart)
   (gateway-ref :initarg :gateway-ref :initform nil :accessor gateway-ref
		:index t)
   (correlation-id :accessor correlation-id :initform nil :index t)
   (payer-id :accessor payer-id :initform nil)
   (order-postage-price :initarg :postage :accessor postage-price :initform 0)
   (order-price :initarg :order-price :initform 0 :accessor order-price)))

(defmethod add-order-timestamp (order timestamp-id)
  (push (cons timestamp-id (get-universal-time)) (order-timestamps order))
  (setf (order-state order) timestamp-id))

(defmethod get-shipping-costs ((order order))
  ;; fixme
  100)

(defun get-order (order-id)
  (ele:get-instance-by-value 'order 'order-number order-id))

(defun get-order-by-gateway-ref (gateway-ref)
  (ele:get-instance-by-value 'order 'gateway-ref gateway-ref))

;; (defmethod confirmation-url ((order order))
;;   (restas:genurl 'order/confirm :order-ref (order-number order)))

;; (restas:define-route order/confirm
;;     ("/confirm/:(order-ref)")
;;   (if-let (order (get-order order-ref))
;;     (progn
;;       (add-order-timestamp order :order-confirmed)
;;       (if (paypal-api-call-doexpresscheckoutpayment order)
;; 	  (progn
;; 	    (delete-session-value :cart)
;; 	    (make-page "Order confirmed"
;; 		       (with-html-output-to-string (s)
;; 			 (:h2 "Your order has been confirmed")
;; 			 (:p (:strong "Your reference ID: ")
;; 			     (str order-ref)))))))))


;; (defmethod cancellation-url ((order order))
;;   (restas:genurl 'order/cancel :order-ref (order-number order)))

;; (restas:define-route order/cancel
;;     ("/cancel/:(order-ref)")
;;   (if-let (order (get-order order-ref))
;;     (progn
;;       (setf (session-value :cart) (cart order))
;;       (ele:drop-instance order)
;;       (order-cancelled-page))))


;; (restas:define-route order/cancel/paypal
;;     ("/cancel")
;;   (if-let (token (get-parameter "token"))
;;     (if-let (order (get-order-by-gateway-ref token))
;;       (progn
;; 	(setf (session-value :cart) (cart order))
;; 	(ele:drop-instance order)
;; 	(order-cancelled-page)))))

;; (restas:define-route order-details
;;     ("/admin/orders")
;;   (make-page "Manage orders"
;; 	     (order-manage-page)
;; 	     :sidebar (edit-bar "Orders")))
(defun admin-orders ()
  (standard-page "Manage orders" nil (order-manage-page) (minimal-edit-bar)))

(defun order-manage-page ()
  (let ((orders (ele:get-instances-by-class 'order)))
    (with-html-output-to-string (s)
      ((:div :class "row")
       (:table :class "table"
	(:tr (:th "Order number")
	     (:th "State")
	     (:th "Timestamps")
	     (:th "Postage price")
	     (:th "Order price"))
	  (dolist (order orders)
	    (with-slots (order-number order-state order-timestamps order-postage-price order-price)
		order
	      (htm (:tr (:td (:a :href (get-admin-view-url order) (str order-number)))
			(:td (str (string-capitalize (symbol-name order-state))))
			(:td (str (cdr (car order-timestamps))))
			(:td (str (print-price order-postage-price)))
			(:td (str (print-price order-price))))))))))))

(defun get-most-recent-order-timestamp (order)
  (local-time:universal-to-timestamp (cdar (order-timestamps order))))

(defun order-cancelled-page ()
  (standard-page "Order cancelled" nil
		 (with-html-output-to-string (s)
		   ((:div :class "container")
		    (:h2 "Your order has been cancelled")
		    (:p "Items from your order have been returned to your shopping cart")
		    (:hr)
		    (str (shopping-cart-form (session-value :cart)))))))

(defun check-order (cart customer)
  (logger :debug "CHECK-ORDER - Cart: ~S" cart)
  (if-let (geo (get-geo-from-country-code (country customer)))
    (let ((valid '())
  	  (invalid '()))
      (dolist (i (items cart))
  	(if (item-available-in?
  	     (qlist-entry-item i)
  	     geo)
  	    (push i valid)
  	    (push i invalid)))
      (values valid invalid))))


(defmethod get-all-objects ((order (eql :order)))
  (ele:get-instances-by-class 'order);;  (let ((orders '()))
    ;; (ele:map-btree (lambda (k v)
    ;; 		     (declare (ignore k))
    ;; 		     (push v items))
    ;; 		   (items *web-store*))
    ;; items)
  )

(defmethod get-admin-view-url ((order order))
  (format nil "/admin/view/order/~A" (order-number order)))

(defun mapreduce (list accessor reducer)
  (reduce reducer (mapcar accessor list)))

(defun total-price (reified-cart)
  (mapreduce reified-cart (lambda (item)
			    (* (third item) (fourth item))) #'+))

(defun total-weight (reified-cart)
  (mapreduce reified-cart #'fifth #'+))

(defmethod object-page ((order order))
  (with-slots (reified-cart customer order-number) order
    (standard-page (format nil "Order: ~A" (order-number order))
		   nil
		   (display-order order)
		   (minimal-edit-bar))))

(defun display-order (order)
  (with-slots (reified-cart customer order-number order-postage-price) order
    (destructuring-bind (postage-provider . postage-cost) order-postage-price
      (with-html-output-to-string (s)
	(:pre (describe reified-cart s))
	(:h2 (fmt "Order: ~A" order-number))
	(:p :class "text-right" (str (display-customer-address customer)))
	(:hr)
	(:table :class "table"
		(:thead
		 (:tr (:th "SKU")
		      (:th "Title")
		      (:th "Price")
		      (:th "Quantity")
		      (:th "Weight")))
		(:tfoot
		 (:tr (:th "")
		      (:th "")
		      (:th "Subtotal")
		      (:th (str (print-price (total-price reified-cart))))
		      (:th (str (total-weight reified-cart))))
		 (:tr (:th "")
		      (:th (str postage-provider))
		      (:th "Postage")
		      (:th (str (print-price postage-cost)))
		      (:th ""))
		 (:tr (:th "")
		      (:th "")
		      (:th "TOTAL")
		      (:th (str (print-price (+ postage-cost (total-price reified-cart)))))
		      (:th "")))
		(:tbody
		 (dolist (i reified-cart)
		   (htm (:tr
			 (destructuring-bind (sku title quantity price weight)
			     i
			   (htm (:td (str sku)))
			   (htm (:td (str title)))
			   (htm (:td (str (print-price price))))
			   (htm (:td (str quantity)))
			   (htm (:td (str weight)))))) )))))))

		    
(defun view-completed-order ()
  (when-let (order (session-value :last-order))
    (standard-page "Order placed.  Thank you" nil
		   (with-html-output-to-string (s)
		     (str (post-parameters*)))
		   (display-order order))))

