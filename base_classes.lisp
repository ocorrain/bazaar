(in-package #:shopper)

(defclass shopper-sites (hunchentoot:acceptor)
  ((stores :initarg :stores :accessor get-stores :initform nil)
   (database-root :initarg :db-root :initform nil :accessor database-root)
   (sc :initarg :sc :initform nil :accessor store-controller)))

(ele:defpclass web-store ()
  ((store-name :initarg :store-name :accessor store-name)
   (store-regex :initarg :store-regex :accessor store-regex)
   (sku-prefix :initarg :sku-prefix :accessor sku-prefix)
   (sku-counter :initform 1 :accessor sku-counter)
   (order-prefix :initarg :order-prefix :accessor order-prefix)
   (order-counter :initform 1 :accessor order-counter)
   (store-customers :initarg :store-customers :accessor store-customers
		    :initform (ele:make-btree))
   (objects :initform (ele:make-btree) :accessor store-objects)
   (item-btree :initform (ele:add-to-root 'items (ele:make-btree)) :accessor items)
   (acceptor :initform nil :transient t :accessor acceptor)
   (image-path :initarg :image-path
	       :initform #p"" :accessor image-path :type pathname)
   (files-path :initarg :files-path
	       :initform #p"" :accessor files-path :type pathname)
   (xml-path   :initarg :xml-path
	       :initform #p"" :accessor xml-path :type pathname)
   (audit-path :initarg :audit-path
	       :initform #p"" :accessor audit-path :type pathname)
   (base-path :initarg :base-path :accessor base-path :type pathname)
   (open :initarg :open :initform nil :accessor store-open)
   (store-type :initarg :store-type :initform :web-store :accessor store-type)
   (store-active-classes :initarg :store-active-classes
			 :initform '(:line-item :tag :geography :static-content)
			 :accessor store-active-classes)
   (dispatch-table :initarg :dispatch-table :initform nil :accessor dispatch-table :transient t)
   (stripe-api-key :initarg :stripe-api-key :initform nil :accessor stripe-api-key)
   (stripe-public-api-key :initarg :stripe-public-api-key :initform nil :accessor stripe-public-key)
   (branding :initarg :branding :initform nil :accessor branding)))

(ele:defpclass persistent-relational-object (relational-object)
  ((relations-to :initarg :relations :initform nil :accessor get-relations-to)
   (relations-from :initarg :relations-from :initform nil :accessor get-relations-from)))

(ele:defpclass cms (persistent-relational-object)
  ((designator :initarg :designator :initform (make-designator) :accessor get-designator :index t)
   (featured :initarg :featured :initform nil :accessor featured
	     :type boolean :index t
	     :documentation "Is this to be published to the front-page
	     / featured page?")
   (published :initarg :published :initform nil
	      :accessor published :index t
	      :documentation "Is this to be published to the site?"
	      :type boolean)
   (store :initform nil :accessor store :index t :documentation "Parent store for this object")))
