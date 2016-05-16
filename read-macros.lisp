(cl:defpackage :schemish)
(cl:in-package :schemish)
(cl:intern "+SCHEME-TRUE+")
(cl:intern "+SCHEME-FALSE+")

(cl:defpackage :schemish-read-macros
  (:use :common-lisp)
  (:export :enable-scheme-read-syntax
	   :disable-scheme-read-syntax))

(cl:in-package :schemish-read-macros)

(defvar *readtable-stack* nil)

(defun push-readtable ()
  (push *readtable* *readtable-stack*)
  (setf *readtable* (copy-readtable)))

(defun pop-readtable ()
  (setf *readtable* (pop *readtable-stack*)))

(defun enable-scheme-read-syntax ()
  (push-readtable)
  (set-dispatch-macro-character #\# #\T
				(lambda (&rest fuck-you)
				  (declare (ignore fuck-you))
				  'schemish::+scheme-true+))
  (set-dispatch-macro-character #\# #\F
				(lambda (&rest fuck-you)
				  (declare (ignore fuck-you))
				  'schemish::+scheme-false+)))

(defun disable-scheme-read-syntax ()
  (pop-readtable))
