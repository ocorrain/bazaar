(in-package #:shopper)

(defmethod json-export ((obj tag))
  (with-slots (name description webform appears-in-menu featured members) obj
    (let ((attributes (plist-alist (list :name name :description description
                                         :webform webform :appears-in-menu appears-in-menu
                                         :featured featured))))
      (cons (cons :members 
                   (mapcar (lambda (i) (sku i)) (ele:pset-list members)))
             attributes))))

(defmethod json-export ((obj provider))
  (with-slots (items provider-name) obj
      (plist-alist (list :items items :provider-name provider-name))))

(defmethod json-export ((obj static-content))
  (with-slots (title content appears-in-menu) obj
    (plist-alist (list :title title :content content :appears-in-menu appears-in-menu))))

(defmethod json-export ((obj user))
  (with-slots (
               USERNAME    
               PWHASH      
               SALT        
               CAPABILITIES
               ) obj
    (plist-alist (list
                  :USERNAME USERNAME
                  :PWHASH PWHASH
                  :SALT SALT
                  :CAPABILITIES CAPABILITIES
                  ))))



(defmethod json-export ((obj geography))
  (with-slots (geography-name geography-members providers) obj
    (append
     (plist-alist (list :geography-name geography-name :geography-members geography-members))
     (list (cons :providers (mapcar #'provider-name providers))))))

(defmethod json-export ((obj web-store))
  (with-slots (
               SKU-PREFIX
               SKU-COUNTER
               ORDER-PREFIX
               ORDER-COUNTER
               STORE-NAME
               STORE-CUSTOMERS
               ITEM-BTREE
               IMAGE-PATH
               FILES-PATH
               XML-PATH
               AUDIT-PATH
               BASE-PATH
               OPEN
               STATE
               ) obj
   (plist-alist (list
                   :SKU-PREFIX SKU-PREFIX
                   :SKU-COUNTER SKU-COUNTER
                   :ORDER-PREFIX ORDER-PREFIX
                   :ORDER-COUNTER ORDER-COUNTER
                   :STORE-NAME STORE-NAME
                   :IMAGE-PATH (namestring IMAGE-PATH)
                   :FILES-PATH (namestring FILES-PATH)
                   :XML-PATH (namestring XML-PATH)
                   :AUDIT-PATH (namestring AUDIT-PATH)
                   :BASE-PATH (namestring BASE-PATH)))))


(defmethod json-export ((obj line-item))
  (with-slots (
               TITLE              
               SHORT-DESCRIPTION  
               LONG-DESCRIPTION   
               PACKING-WEIGHT     
               WEIGHT             
               PRICE              
               TAGS               
               SKU                
               META               
               FEATURED           
               PUBLISHED          
               IMAGES             
               IMAGE-COUNTER      
               GEOGRAPHIES        
               CHILDREN-QLIST) obj
    (let ((attributes (list
                                    :TITLE TITLE
                                    :SHORT-DESCRIPTION SHORT-DESCRIPTION
                                    :LONG-DESCRIPTION LONG-DESCRIPTION
                                    :PACKING-WEIGHT PACKING-WEIGHT
                                    :WEIGHT WEIGHT
                                    :PRICE PRICE
                                    ;; :TAGS TAGS
                                    :SKU SKU
                                    :META META
                                    :FEATURED FEATURED
                                    :PUBLISHED PUBLISHED
                                    ;; :IMAGES IMAGES
                                    :IMAGE-COUNTER IMAGE-COUNTER
                                    ;; :GEOGRAPHIES GEOGRAPHIES
                                    ;; :CHILDREN-QLIST CHILDREN-QLIST
                                    )))
      (append (plist-alist attributes)
              (list (cons :tags
                          (mapcar #'webform (ele:pset-list tags)))
                    (cons :images
                          (mapcar #'namestring images))
                    (cons :geographies
                          (mapcar #'geo-name geographies))
                    (cons :children
                           (items children-qlist)))))))

(defmethod json-encode (obj)
  (cl-json:encode-json (json-export obj)))

(defun json-export-all ()
  (let ((ht (make-hash-table)))
    (dolist (sym '(line-item tag geography provider web-store static-content))
      (setf (gethash sym ht )
            (mapcar #'json-export (ele:get-instances-by-class sym))))
    
    ht))

    
  ;; do (s '(line-item tag geography provider))
  ;; (mapcar (lambda (sym)
  ;;             (cons (make-keyword (symbol-name sym))
  ;;                   (mapcar #'json-export (ele:get-instances-by-class sym))))
  ;;         '(line-item tag geography provider)))

(defun json-encode-all (output-path)
  (with-open-file (f output-path :direction :output :if-exists :supersede)
    (cl-json:encode-json (json-export-all) f)))

(defun json-decode (filename)
  (with-open-file (f filename)
    (cl-json:decode-json f)))

