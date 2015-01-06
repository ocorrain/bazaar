(in-package #:shopper)

(defun login-form (&optional errors)
  (basic-page "Log in"
	      (who:with-html-output-to-string (s)
;		(:pre (who:fmt "~S" (hunchentoot:session-value :user)))
		((:div :class "container")
		 (:h2 "Sign in")
		(when errors
		  (who:htm ((:p :class "text-error")
			    "The login details you supplied were incorrect")))
		((:form :class "form-horizontal" :method "post" :action (restas:genurl 'login))
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





