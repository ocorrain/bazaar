(in-package #:shopper)

(defun import-from-file (filename)
  (with-open-file (f filename)
    (cl-json:decode-json f)))

(defun setup-import-store (filename)
  (let* ((spec (import-from-file filename))
         (*web-store* (car (import-class :web-store spec))))
    (import-class :tag spec)
    (import-class :line-item spec)
    (import-class :provider spec)
    (import-class :geography spec)
    (import-class :static-content spec)))

(defun import-image (img item)
  (relate item (make-instance 'image :file img) :image))

(defmethod import-object ((obj (eql :tag)) spec)
  (flet ((get-param (p) (cdr (assoc p spec))))
    (make-instance 'tag
                   :name  (get-param :name) 
                   :description  (get-param :description) 
                   :webform  (get-param :webform) 
                   :appears-in-menu  (get-param :appears-in-menu))))

(defmethod import-object ((obj (eql :static-content)) spec)
  (flet ((get-param (p) (cdr (assoc p spec))))
    (make-instance 'static-content
                   :title (get-param :title)
                   :content (get-param :content)
                   :appears-in-menu (get-param :appears-in-menu))))

(defmethod import-object ((obj (eql :provider)) spec)
  (flet ((get-param (p) (cdr (assoc p spec))))
    (make-instance 'provider
                   :items (get-param :items)
                   :name (get-param  :provider-name))))

(defmethod import-object ((obj (eql :geography)) spec)
  (flet ((get-param (p) (cdr (assoc p spec))))
    (let ((geo (make-instance 'geography
                              :name (get-param :geography-name)
                              :members (get-param  :geography-members))))
      (dolist (prov (get-param  :providers))
        (push (ele:get-instance-by-value 'provider 'provider-name prov)
              (geo-providers geo)))
      geo)))

(defmethod import-object ((obj (eql :tag)) spec)
  (flet ((get-param (p) (cdr (assoc p spec))))
    (make-instance 'tag
                   :name  (get-param :name) 
                   :description  (get-param :description) 
                   :webform  (get-param :webform) 
                   :appears-in-menu  (get-param :appears-in-menu)
                   :featured (get-param :featured))))


(defmethod import-object ((obj (eql :web-store)) spec)
  (flet ((get-param (p) (cdr (assoc p spec))))
    (create-web-store-object (get-param :sku-prefix)
                             (get-param :order-prefix)
                             (get-param :store-name)
                             "example.com"
                             (get-param :image-path)
                             (get-param :files-path)
                             (get-param :xml-path)
                             (get-param :audit-path)
                             (get-param :base-path))))

(defmethod import-object ((obj (eql :line-item)) spec)
  (flet ((get-param (p) (cdr (assoc p spec))))
    (let ((item (make-instance 'line-item
                               :title (get-param :title)
                               :short-description (get-param :short-description)
                               :long-description (get-param :long-description)
                               :packing-weight (get-param :packing-weight)
                               :weight (get-param :weight)
                               :price (get-param :price)
                               :sku (get-param :sku)
                               :meta (get-param :meta)
                               :featured (get-param :featured)
                               :published (get-param :published)
                               :image-counter (get-param :image-counter)
                                        ;                   :tags (get-param :tags)
                               ;; :geographies (get-param :geographies)
                               )))
      (dolist (tag (get-param :tags))
        (let ((tag-obj (get-tag tag)))
          (when tag-obj
            (tag-item item tag-obj))))
      (dolist (geo (get-param :geographies))
        (let ((geo-obj (get-geo geo)))
          (when geo-obj
            (push geo-obj (geographies item)))))
      (dolist (img (get-param :images))
        (import-image img item))
      item)))

(defmethod import-object ((obj t) spec)
  nil)

(defun import-class (class struct)
  (let ((this (assoc class struct)))
    (mapcar (lambda (o)
              (import-object (car this) o))
            (cdr this))))


