(in-package #:shopper)

;;   ((get-form obj) (((image-edit-page obj))
;; 		   ((tag-cloud obj) (geo-cloud obj))))

;; (grid (6 (get-form obj) 6 (grid (image-edit-page obj)
;; 				(3 (tag-cloud obj) 3 (geo-cloud obj)))))

;; (defvar *grid-obj* '((6 (get-form obj) 6 (grid (image-edit-page obj)
;; 					       (3 (tag-cloud obj) 3 (geo-cloud obj))))))

(defmacro grid (&body grid-form)
  (grid-expand `,grid-form))

(defun grid-expand (grid)
  `(with-html-output-to-string (s)
     ,@(mapcar #'expand-row grid)))


(defun expand-row (row)
  `(:div :class "row" ,@(expand-columns row)))

(defun expand-columns (row)
  (if (null row)
      nil
      (let ((span (first row))
	    (code (second row)))
	(cons `(:div :class ,(format nil "span~A" span)
		     (str ,code))
	      (expand-columns (cddr row))))))
