(in-package #:shopper)

					; JSON export

(defun map-some-slots (function object exclude)
  (loop for slot in (closer-mop:class-slots (class-of object))
     for slot-name = (closer-mop:slot-definition-name slot)
     if (and (slot-boundp object slot-name) (not (member slot-name exclude)))
     do (funcall function slot-name (slot-value object slot-name))))

(defmethod json:encode-json ((o cms) &optional (stream json:*json-output*))
  (json:with-object (stream)
    (map-some-slots (json:stream-object-member-encoder stream) o '(elephant::spec elephant::oid))))

(defmethod json:encode-json ((o provider) &optional (stream json:*json-output*))
  (json:with-object (stream)
    (map-some-slots (json:stream-object-member-encoder stream) o '(elephant::spec elephant::oid))))

(defmethod json:encode-json ((o quantity-list) &optional (stream json:*json-output*))
  (json:with-object (stream)
    (map-some-slots (json:stream-object-member-encoder stream) o '(elephant::spec elephant::oid))))

(defmethod json:encode-json ((o pathname) &optional (stream json:*json-output*))
  (json:encode-json (namestring o) stream))

(defun json-object-page (obj)
  (setf (content-type*) "application/json")
  (with-output-to-string (s)
    (json:encode-json obj s)))
