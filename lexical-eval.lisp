#-(or sbcl clisp) (error "Your Lisp implementation is not supported.")
(cl:defpackage :lexical-eval
  #+sbcl (:use :common-lisp :sb-kernel :sb-di)
  #+clisp (:use :ext :common-lisp)
  (:export :lexical-eval :with-inner-repl))

(cl:in-package :lexical-eval)


(defconstant +documentation-string+
    "Evaluates the FORM in the same lexical environment where the LEXICAL-EVAL
form is found. Example:

 (let ((x 2))
   (setf *a-global-var* x)
   (lexical-eval 'x)) ==> 2

NOTE: Your compiler may not create a variable declared in a LET or
      LAMBDA form if that variable is not used or if its value has no
      effect on the behavior or return value of the function. In that
      case, the variable may be unbound. Example;

 (let ((x 2))
    (lexical-eval 'x)) ==> The variable X is unbound.

")

#+clisp
(defmacro lexical-eval (form)
    "Evaluates the FORM in the same lexical environment where the LEXICAL-EVAL
form is found. Example:

 (let ((x 2))
   (setf *a-global-var* x)
   (lexical-eval 'x)) ==> 2

NOTE: Your compiler may not create a variable declared in a LET or
      LAMBDA form if that variable is not used or if its value has no
      effect on the behavior or return value of the function. In that
      case, the variable may be unbound. Example;

 (let ((x 2))
    (lexical-eval 'x)) ==> The variable X is unbound.

"
  `(eval-env ,form (the-environment)))

#+sbcl
(defun current-frame ()
  (second-value (find-caller-name-and-frame)))

(defun frame-repl (frame)
  "Starts a REPL that evaluates expressions within the given FRAME.
The source of the FRAME depends on the Lisp implementation.

A restart named LEXICAL-EVAL::*RESUME-INNER-REPL is created to return
to the REPL in the event of an error.

The REPL terminates when :QUIT is input for evaluation."
  (format t "Type :QUIT to leave the REPL.~%")
  (loop for input = (progn
		      #+sbcl (format t "~a>>> " frame)
		      #-sbcl (format t ">>> ")
		      (finish-output)
		      (read))
       until (eql input :quit)
       do (restart-case
	      (let ((results (multiple-value-list
			      #+sbcl (eval-in-frame frame input)
			      #+clisp (eval-env input frame))))
		(loop for r in results do
		     (print r)
		     (terpri))
		(setf * (car results)
		      / results
		      - +
		      + input))
	    (*resume-inner-repl () :report "Return to the INNER REPL."
			  nil))))

#+sbcl
(defmacro with-inner-repl (&body body)
  "Adds a restart that starts a REPL where all the forms evaluate in
the scope where the WITH-INNER-REPL form appeared."
  `(restart-case
       (progn ,@body)
     (inner-repl () :report "Start a REPL in the scope where the error occurred."
	  (let ((frame (second-value (find-caller-name-and-frame))))
	    (frame-repl frame)))))

#+clisp
(defmacro with-inner-repl (&body body)
  "Adds a restart that starts a REPL where all the forms evaluate in
the scope where the WITH-INNER-REPL form appeared."
  (let ((env (gensym)))
    `(let ((,env (the-environment)))
       (restart-case (progn ,@body)
	 (inner-repl () :report "Start a REPL in the scope where the error occurred."
		     (frame-repl ,env))))))


#+sbcl
(defmacro second-value (form)
  `(multiple-value-bind (a b)
       ,form
     (declare (ignore a))
     b))

#+sbcl
(defun lexical-eval* (form)
  (eval-in-frame (second-value (find-caller-name-and-frame))
		 form))

#+sbcl
(defmacro lexical-eval (form)
  "Evaluates the FORM in the same lexical environment where the LEXICAL-EVAL
form is found. Example:

 (let ((x 2))
   (setf *a-global-var* x)
   (lexical-eval 'x)) ==> 2

NOTE: Your compiler may not create a variable declared in a LET or
      LAMBDA form if that variable is not used or if its value has no
      effect on the behavior or return value of the function. In that
      case, the variable may be unbound. Example;

 (let ((x 2))
    (lexical-eval 'x)) ==> The variable X is unbound.

"
  `(lexical-eval* ,form))

#+sbcl
(progn
  (defun test (x)
    (declare (optimize (debug 3)))
    (with-continue y)
    (find-caller-name-and-frame))

  (defvar *global* nil)
  (defvar *nil* nil)

  (defun test2 (x)
    (declare (optimize (speed 0) (debug 3)))
    (lexical-eval 'x))

  (defun test3 (x)
    "Example of how LEXICAL-EVAL could be used in a BASIC compiler."
    (unless *nil*
      ;; This is never executed, but the compiler can't prove that.
      ;; Therefore, X is not removed as a variable...
      (setf *global* x))
;;; And therefore, we can LEXICAL-EVAL 'X.
    (lexical-eval 'x)))
