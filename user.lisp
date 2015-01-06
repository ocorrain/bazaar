(in-package #:shopper)


(defun all-users ()
  (ele:get-instances-by-class 'user))

(defun add-user (username password)
  (multiple-value-bind (hash salt)
      (get-password-digest password)
    (make-instance 'user
		   :username username
		   :pwhash hash
		   :salt salt)))

(defun get-user (username password)
  (when-let (user-obj (ele:get-instance-by-value 'user 'username username))
    (with-slots (pwhash salt) user-obj
      (when (equal (get-password-digest password salt) pwhash)
	user-obj))))


(defun get-password-digest (password &optional salt-in)
  (let* ((salt (or salt-in
		   (hunchentoot::create-random-string 
		    (+ 10 (random 10)) 36)) )
	 (salted-password (concatenate 'string salt password)))
    (values (format nil "~{~X~}"
		    (map 'list #'identity
			 (md5:md5sum-sequence salted-password))) 
	    salt)))

(ele:defpclass user ()
  ((username :initarg :username :initform nil :accessor username :index t)
   (pwhash :initarg :pwhash :initform nil :accessor pwhash)
   (salt :initarg :salt :initform nil :accessor salt)
   (capabilities :initarg :capabilities :initform nil :accessor capabilities)))

(defun has-capability (capability user)
  (member capability (capabilities user)))

(defun add-capability (capability user)
  (push capability (capabilities user)))

(defun login-form (&optional errors)
  (standard-page "Log in" nil
	      (who:with-html-output-to-string (s)
		((:div :class "container")
		 (:h2 "Sign in")
		 (when errors
		   (who:htm ((:p :class "text-error")
			     "The login details you supplied were incorrect")))
		 ((:form :class "form-horizontal" :method "post" :action "/login")
		  ((:div :class "control-group")
		   ((:label :class "control-label" :for "username")
		    "Username")
		   ((:div :class "controls")
		    (:input :name "username" :type "text"
			    :id "username" :placeholder "Username")))
		  ((:div :class "control-group")
		   ((:label :class "control-label" :for "password")
		    "Password")
		   ((:div :class "controls")
		    (:input :name "password" :type "password"
			    :id "password" :placeholder "Password")))
		  ((:div :class "control-group")
		   ((:div :class "controls")
		    ((:button :type "submit" :class "btn")
		     "Sign in"))))))))


(defun login-page ()
  (if (eql (hunchentoot:request-method*) :post)
      (let ((username (hunchentoot:post-parameter "username"))
	    (password (hunchentoot:post-parameter "password")))
	(if-let (userobj (and username password (get-user username password)))
	  (progn (hunchentoot:start-session)
		 (setf (hunchentoot:session-value :user) userobj)
		 (hunchentoot:redirect "/"))
	  (login-form t)))
      (login-form)))

(defun logout ()
  (hunchentoot:delete-session-value :user)
  (hunchentoot:redirect "/"))

(defun current-user ()
  (when-let (current-user (hunchentoot:session-value :user))
    (username current-user)))

(defun get-logged-in-user ()
  (when-let (user (hunchentoot:session-value :user))
    user))

(defun secure-page (function capability)
  (lambda ()
    (if-let (user (hunchentoot:session-value :user))
      (if (has-capability capability user)
	  (funcall function)
	  (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+))
      (hunchentoot:redirect "/login"))))

