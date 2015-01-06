;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; Copyright (c) 2012, Tiarnán Ó Corráin  All rights reserved.

(in-package #:shopper)

(ele:defpclass line-item (cms)
  ((title :initarg :title :initform ""
	  :accessor title :index t
	  :documentation "Title or name of the line item" :type string)
   (short-description :initarg :short-description :initform ""
		      :accessor short-description
		      :documentation "A one-line description of the item"
		      :type string)
   (long-description :initarg :long-description :initform ""
		     :accessor long-description
		     :documentation "A long (paragraph length)
		     description of the item"
		     :type string)
   (packing-weight :initarg :packing-weight
		   :initform 0 :accessor packing-weight
		   :documentation "The extra weight of packaging:
		   ie. packing-weight + weight equals the total
		   shipping weight")
   (weight :initarg :weight :initform 0 :accessor weight
	   :documentation "The weight of the item in grams" :type integer)
   (price :initarg :price :initform 0 :accessor price
	  :documentation "The price of the item in euro cents" :type integer)

   (sku  :initarg :sku :initform nil :accessor sku
	 :index t :documentation "Stock-keeping unit ID"
	 :type string)
   (meta :initarg :meta :initform '() :accessor meta
	 :documentation "Meta tags to be added to page for HTML
	 searchability"
	 :type list)
   
   (geographies :initform nil :accessor geographies
		:documentation "geographies in which this item is available")
   (children-qlist :initform (make-instance 'quantity-list)
		   :accessor get-children-qlist
		   :documentation "If children exists, this
			  should be a quantity list mapping each child
			  to the quantity contained in the bundle")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level API

(defmethod get-object ((item (eql :line-item)) sku)
  (get-item sku))

(defmethod get-all-objects ((item (eql :line-item)))
  (let ((items '()))
    (ele:map-btree (lambda (k v)
		     (declare (ignore k))
		     (push v items))
		   (items *web-store*))
    items))

(defmethod get-identifier ((item line-item))
  (sku item))

(defmethod render-object ((item line-item))
  (display-item-page item))



(defmethod get-form ((item (eql :line-item)))
  (item-form))

(defmethod get-form ((item line-item))
  (item-form item))

(defmethod get-edit-tabs ((item line-item))
  '(:view :edit :images :tags :contents))


(defmethod view-object ((obj line-item))
  (display-item-content obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-item (sku)
  (ele:get-value sku (items *web-store*)))

(defgeneric get-price (item))


(defgeneric get-weight (item))


(defmethod get-next-image-stub ((obj cms))
  (make-designator))

(defun collect-items-with (func)
  (let ((result '()))
    (ele:map-btree (lambda (k v)
		     (declare (ignore k))
		     (when (funcall func v)
		       (push v result)))
		   (items *web-store*))
    result))

(defun collect-skus-with (func)
  (let ((result '()))
    (ele:map-btree (lambda (k v)
		     (when (funcall func v)
		       (push k result)))
		   (items *web-store*))
    result))

(defmethod get-weight ((item line-item))
  (weight item))

(defmethod get-price ((item line-item))
  (price item))


(defun item-list->table-form (items item-input-func item-display-func action-url)
  (with-html-output-to-string (s)
    ((:table :class "table table-striped")
     ((:form :method :post :action action-url)
      (dolist (item items)
	(htm (:tr (:td (str (funcall item-display-func item)))
		  (:td (:input :type "text" :class "input-mini"
			       :name (funcall item-input-func item)
			       :value 0)))))
      ((:button :type "submit" :class "btn btn-primary") "Add to bundle")))))

