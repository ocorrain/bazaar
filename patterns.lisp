(in-package #:shopper)

;; Software License Agreement

;; Copyright © 1998-2002 by Peter Norvig.

;; Permission is granted to anyone to use this software, in source or
;; object code form, on any computer system, and to modify, compile,
;; decompile, run, and redistribute it to anyone else, subject to the
;; following restrictions:

;; The author makes no warranty of any kind, either expressed or implied,
;; about the suitability of this software for any purpose.

;; The author accepts no liability of any kind for damages or other
;; consequences of the use of this software, even if they arise from
;; defects in the software.

;; The origin of this software must not be misrepresented, either by
;; explicit claim or by omission.

;; Altered versions must be plainly marked as such, and must not be
;; misrepresented as being the original software. Altered versions may be
;; distributed in packages under other licenses (such as the GNU
;; license).

;; If you find this software useful, it would be nice if you let
;; me (peter@norvig.com) know about it, and nicer still if you send me
;; modifications that you are willing to share. However, you are not
;; required to do so.

(defconstant fail nil)
(defconstant no-bindings '((t . t)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
        ((variable-p pattern) (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input) bindings)))
        (t fail)))

(defun match-variable (var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defun make-binding (var val) (cons var val))

(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq bindings no-bindings)
            nil
            bindings)))

(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(defun url->pattern (string)
  (mapcar #'intern
	  (split-sequence:split-sequence #\/ string
					 :remove-empty-subseqs t)))
