(in-package #:shopper)

;; (ele:defpclass bundle (line-item quantity-list)
;;   ((discount :initarg :discount :initform 0 :accessor discount
;; 	     :documentation "Percentage discount for a bundle")))

;; (defmethod get-images ((bundle bundle))
;;   (remove-duplicates
;;    (flatten (mapcar (compose #'images #'car) (items bundle)))))

;; (defmethod get-price :around ((bundle bundle))
;;   "Applies the bundle discount"
;;   (let ((initial-price (call-next-method)))
;;     (round (* initial-price (/ (- 100 (discount bundle)) 100)))))

(defun maybe-update-bundle (bundle)
  (dolist (item-q (get-valid-objects-from-post (post-parameters*)))
    (destructuring-bind (item . quantity) item-q
      (when (valid-item-for-bundle item bundle)
	(relate bundle item :member-of-bundle quantity)))))

(defun valid-item-for-bundle (item bundle)
  ;; FIXME
  t)

(defun bundle-edit-page (item)
  (let ((available-items (remove-if (lambda (i)
				      (or (contains? item i)
					  (equal item i)))
				    (get-all-objects :line-item))))
    (with-html-output-to-string (s)
      (when (empty? (get-children-qlist item))
	(htm (:p "This item is currently defined as a single
	item. This means that it has no contents. To make it into a
	bundle, click 'Add new items'")
	     (:pre (fmt "~S" (get-parameters*)))))
      (when available-items
	(htm ((:a :href (url-rewrite:add-get-param-to-url
			 (get-edit-page-url item :contents)
			 "action" "add")
		  :class "btn btn-primary") "Add new items")))
      (str (qlist->table-form (get-children-qlist item)
			      #'sku #'title
			      (get-edit-page-url item :contents)))
      (when (and available-items
		 (string-equal (hunchentoot:get-parameter "action") "add")) 
	(htm (:hr)
	     (str (item-list->table-form available-items
					 #'sku #'title
					 (get-edit-page-url item :contents))))))))
