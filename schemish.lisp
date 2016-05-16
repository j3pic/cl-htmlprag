;; Note: Since this was written specifically to port GPL'd HTMLPrag to Common Lisp, it
;;       most likely counts as a derivative work of HTMLPrag, and therefore is itself
;;       subject to the GPL.

;; 90% of the work of porting HTMLPrag to Common Lisp is done here.

(cl:defpackage :schemish
  (:documentation "A Scheme-like environment for porting HTMLPrag to Common Lisp. It implements only those features of
Scheme that were actually used by HTMLPrag.")
  (:export :let :LAMBDA :Λ :IF :COND :EQ? :PAIR? :CAR :CDR :DEFINE :APPLY :MAP :LENGTH
	   :STRING-LENGTH :REVERSE :LIST->STRING :STRING->LIST :NOT :AND :OR :EQUAL?
	   :NULL? :LIST? :EOF-OBJECT? :CLOSE-OUTPUT-PORT :CLOSE-INPUT-PORT
	   :STRING->SYMBOL :SYMBOL? :STRING? :INTEGER? :EQV? :LIST-REF :STRING->NUMBER
	   :VECTOR-REF :VECTOR-SET! :STRING-REF :STRING-SET! :SET! :FILTER :LETREC
	   :OPEN-INPUT-STRING :DISPLAY :NEWLINE :DISPLAYLN :BEGIN :ELSE :ZERO? :+scheme-true+ :+scheme-false+
	   :string->number :exact->inexact :inexact->exact :number? :exact? :inexact? :integer->char
	   :char->integer :set-car! :set-cdr! :string-append :read-char :member :memq :memv :cadr :cddr :open-output-string
	   :get-output-string :case :current-output-port :write-char :symbol->string :for-each :let* :input-port?
	   :assoc :assq :assv := :< :> :<= :>= :write-string :char? :char-alphabetic? :char-numeric? :string=? :string :string-append
	   :call-with-values))

(cl:in-package :schemish)

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defparameter *common-lisp-symbols*
    '(cl:&body cl:&rest cl:&optional cl:&key cl:cons cl:list cl:+ cl:- cl:/ cl:*
      cl:char-downcase cl:char-upcase cl:string-downcase cl:string-upcase
      cl:vector cl:write cl:values  cl:append cl:list*))
  (cl:import *common-lisp-symbols*)
  (cl:export *common-lisp-symbols*))

(cl:defconstant +scheme-false+ 'scheme-false)
(cl:defconstant +scheme-true+ 'scheme-true)


(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (schemish-read-macros:enable-scheme-read-syntax))

(cl:defconstant else #t)
(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defmacro named-let (name defs &body body)
    (cl:let ((variables (cl:mapcar #'cl:car defs))
	     (values (cl:mapcar #'cl:cadr defs)))
      `(cl:labels ((,name ,variables ,@body))
	 (let () ;; to make inner (define ...) forms work.
	   (,name ,@values)))))
  
  (cl:defmacro let (&body body)
    `(cl:progn ,@body)))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:defun scheme-lambda-list->cl-lambda-list (lambda-list)
    (named-let loop ((scheme-lambda-list lambda-list)
		     (cl-lambda-list '()))
      (cl:cond ((cl:null scheme-lambda-list)
		(cl:reverse cl-lambda-list))
	       ((cl:not (cl:consp scheme-lambda-list))
		(loop '()
		   (cl:list* scheme-lambda-list 'cl:&rest cl-lambda-list)))
	       (cl:t
		(loop (cl:cdr scheme-lambda-list)
		   (cl:cons (cl:car scheme-lambda-list)
			    cl-lambda-list))))))

  (cl:defmacro lambda (lambda-list &body body)
    `(cl:lambda ,(scheme-lambda-list->cl-lambda-list lambda-list) ,@body))

  (cl:defmacro λ (lambda-list &body body)
    `(lambda ,lambda-list ,@body))


  (cl:defmacro let (defs &body body)
    ;; We expand LET into (FUNCALL (LAMBDA (vars) ...)) instead
    ;; of using CL:LET. The reason for this is because our LAMBDA
    ;; allows the use of nested DEFINE forms, which is the same
    ;; semantics required by Scheme's LET.
    (cl:cond
      ((cl:null defs)
       `(cl:funcall (lambda () ,@body)))
      ((cl:symbolp defs)
       `(named-let ,defs ,@body))
      (cl:t `(cl:funcall
	      (lambda ,(cl:mapcar #'cl:first defs) ,@body)
	      ,@(cl:mapcar #'cl:second defs)))))


  (cl:defmacro if (condition true-expr &optional false-expr)
    `(cl:if (cl:eq ,condition #f)
	    ,false-expr
	    ,true-expr))

  (cl:defmacro cond (&rest cases)
    (let ((result '()))
      (cl:loop for (condition . body) in (cl:reverse cases)
	       do (cl:setf result `(if ,condition
				       (cl:progn ,@body)
				       ,result)))
      result))


  (cl:defun eq? (a b)
    (cl:if (cl:eq a b)
	   #t
	   #f))


  (cl:defun pair? (obj)
    (cl:if (cl:consp obj)
	   #t
	   #f))


  (cl:defun car (obj)
    (cl:declare (cl:type cl:cons obj))
    (cl:car obj))


  (cl:defun cdr (obj)
    (cl:declare (cl:type cl:cons obj))
    (cl:cdr obj))


  (cl:defmacro define-function ((function &rest arguments) &rest body)
    "A Scheme-like define form. Supports inner define forms, which expand to LABELS or LET."
    (let ((args cl:nil)
	  (inner-functions cl:nil))
      (cl:flet ((scheme-args->lisp-args (arguments)
		  (let ((args cl:nil))
		    (cl:loop for (arg . rest) on arguments
			     do (cl:push arg args)
			     (cl:unless (cl:listp rest)
			       (cl:push '&rest args)
			       (cl:push rest args)
			       (cl:return)))
		    args)))
	(cl:setf args (scheme-lambda-list->cl-lambda-list arguments))
	(cl:loop for form in body do
		 (optima:match form
		   ((cons 'define (cons (cons function-name args) local-body))
		    (cl:push `(,function-name ,(scheme-lambda-list->cl-lambda-list args) ,@local-body) inner-functions)
		    (cl:pop body))
		   ((list 'define variable value)
		    (cl:push `(local-var ,variable ,value) inner-functions)
		    (cl:pop body))))
	(cl:push cl:nil body)
	(cl:push 'cl:let body)
	(cl:if inner-functions
	       (cl:loop for (prefix . func) in inner-functions do
			(cl:if (cl:eq prefix 'local-var)
			       (cl:setf body
					`(let (,func) ,body))
			       (cl:setf body
					`(cl:labels ((,prefix ,@func)) ,body)))))
	`(cl:defun ,function ,args ,body))))

  (cl:defmacro define-values (variables value-form)
    (let ((gensyms (cl:loop for v in variables
			    collect (cl:gensym))))
      `(cl:progn
	 ,@(cl:loop for v in variables collect
		    `(cl:defparameter ,v cl:nil))
	 (cl:multiple-value-bind ,gensyms ,value-form
	   ,@(cl:loop for v in variables
		      for g in gensyms
		      collect `(cl:setf ,v ,g)))
	 (cl:values))))

  (cl:defmacro define (name &rest other-args)
    (cl:if (cl:listp name)
	   `(define-function ,name ,@other-args)
	   `(define-values (,name) ,@other-args)))

  (define (cadr obj)
      (car (cdr obj)))

  (define (caar obj)
      (car (car obj)))
  
  (define (cddr obj)
      (cdr (cdr obj)))

  (define (apply func . args)
      (cl:apply #'cl:apply (cons func args)))

  (define (map func . lists)
      (apply #'cl:mapcar (cons func lists)))

  (define (length list)
      (cl:declare (cl:type cl:list list))
    (cl:length list))

  (define (string-length list)
      (cl:declare (cl:type cl:string list))
    (cl:length list))


  (define (reverse list)
      (cl:declare (cl:type cl:list list))
    (cl:reverse list))

  (define (list->string list)
      (cl:declare (cl:type cl:list list))
    (cl:coerce list 'cl:string))

  (define (string->list string)
      (cl:declare (cl:type cl:string string))
    (cl:coerce string 'cl:list))

  (define (symbol->string symbol)
      (cl:symbol-name symbol))

  (define (not obj)
      (if obj
	  #f
	  #t))


  (cl:defmacro and (&rest conditions)
    (let ((it (cl:gensym)))
      (cl:if (cl:null conditions)
	     '#t
	     (cl:if (cl:cdr conditions)
		    `(if ,(car conditions)
			 (and ,@(cdr conditions))
			 #f)
		    `(let ((,it ,(car conditions)))
		       (if ,it ,it #f))))))


  (cl:defmacro or (&rest conditions)
    (let ((it (cl:gensym)))
      (cl:if (cl:null conditions)
	     '#f
	     `(let ((,it ,(car conditions)))
		(if ,it
		    ,it
		    (or ,@(cdr conditions)))))))


  (define (equal? a b)
      (cl:typecase a
	(cl:symbol
	 (cl:null (null? b))
	 (equal? (symbol->string a)
		 (symbol->string b)))
	(cl:string (and (string? b)
			(not (eq? (cl:equalp a b)
				  cl:nil))))
	(cl:list (and (pair? b)
		      (equal? (car a) (car b))
		      (equal? (cdr a) (cdr b))))
	(cl:otherwise (cl:if (cl:eq (cl:equalp a b)
				    cl:nil)
			     #f #t))))

  (define (null? obj)
      (eq? obj cl:nil))


  (define (list? obj)
      (or (null? obj)
	  (and (pair? obj)
	       (list? (cdr obj)))))

  (cl:defstruct eof-object)

  (define (eof-object? obj)
      (not (null? (eof-object-p obj))))

  (define (close-output-port port)
      (cl:close port))

  (define (close-input-port port)
      (cl:close port))


  (define (string->symbol string)
      (cl:intern string :keyword))

  (define (symbol? obj)
      (not (null? (cl:symbolp obj))))

  (define (string? obj)
      (not (null? (cl:stringp obj))))

  (define (char? obj)
      (not (null? (cl:characterp obj))))

  (define (char-alphabetic? obj)
      (not (null? (cl:alpha-char-p obj))))
      
  (define (char-numeric? obj)
      (not (null? (cl:digit-char-p obj))))
  
  (define (integer? obj)
      (not (null? (cl:integerp obj))))

  (define (string=? a b)
      (not (null? (cl:string= a b))))
 
  (define (eqv? a b)
      (not (null? (cl:eql a b))))


  (define (list-ref list n)
      (cl:declare (cl:type list list))
    (cl:elt list n))

  (cl:defun string->number (string &optional (radix 10))
    (parse-number:parse-number string :radix radix))

  (define (exact->inexact number)
      (cl:coerce number 'cl:float))

  (define (inexact->exact number)
      (cl:rationalize number))

  (define (number? number)
      (not (null? (cl:numberp number))))

  (define (exact? number)
      (not (null? (cl:rationalp number))))

  (define (inexact? number)
      (and (number? number)
	   (not (exact? number))))

  (define (vector-ref vec n)
      (cl:aref vec n))

  (define (string . args)
      (list->string args))
  
  (define (vector-set! vec n new-value)
      (cl:setf (cl:aref vec n) new-value))


  (define (string-ref str n)
      (cl:aref str n))

  (define (string-set! str n new-value)
      (cl:setf (cl:aref str n) new-value))

  (cl:defmacro set! (var value)
    `(begin
       (cl:setq ,var ,value)
       (values)))

  (define (filter pred list)
      (cl:declare (cl:type cl:list list))
    (cl:remove-if-not (lambda (item)
			(cl:not (cl:eq (cl:funcall pred item)
				       #f)))
		      list))

  (define (%member v list test)
      (cond ((null? list)
	     #f)
	    ((cl:funcall test v (car list))
	     #t)
	    (else
	     (%member v (cdr list) test))))

  (define (member v list)
      (%member v list #'equal?))

  (define (memv v list)
      (%member v list #'eqv?))

  (define (memq v list)
      (%member v list #'eq?))

  (cl:defmacro letrec (bindings &body body)
    "Like LET, but with a fucked-up hack that causes bindings to lambda forms to end up
in the function cell, providing a half-assed emulation of Lisp-1. It doesn't work too
well because there are a number of functions that Neil van Dyke wraps in conditional
expressions, which defeat my analyzer."
    (let ((lambda-bindings (filter (λ (binding)
				     (and (list? (cl:cadr binding))
					  (member (cl:caadr binding) '(lambda λ))))
				   bindings)))
      `(let ,(cl:loop for (var value) in bindings
		      collect `(,var cl:nil))
	 (cl:labels ,(cl:loop for (function (lambda lambda-list . body)) in lambda-bindings
			      collect `(,function ,lambda-list ,@body))
	   ,@(cl:loop for (var value) in bindings
		      collect `(cl:setf ,var ,value))       
	   ,@body))))

  (define (open-input-string string)
      (cl:make-string-input-stream string))

  (cl:defun write-string (string &optional (output-port cl:*standard-output*))
    (cl:loop for ch across string
	     do (write-char ch output-port)))
  
  (cl:defun display (value cl:&optional (out cl:*standard-output*))
    (write-string
     (cond ((or (char? value)
	       (string? value)
	       (symbol? value))
	    (cl:format cl:nil "~a" value))
	   (else (cl:format cl:nil "~S" value)))
     out)
    (cl:values))

  (cl:defun newline (cl:&optional (out cl:*standard-output*))
    (cl:terpri out)
    (cl:values))

  (cl:defun displayln (value cl:&optional (out cl:*standard-output*))
    (display value out)
    (cl:terpri out)
    (cl:values))


  (cl:defmacro begin (&body body)
    `(cl:progn ,@body))


  (define (zero? num)
      (eq? num 0))

  (define (call-with-values value-generating-function value-consuming-function)
      (cl:multiple-value-call value-consuming-function (cl:funcall value-generating-function)))

  (define (integer->char int)
      (cl:code-char int))

  (define (char->integer ch)
      (cl:char-code ch))

  (define (set-car! pair new-car)
      (cl:rplaca pair new-car))

  (define (set-cdr! pair new-cdr)
      (cl:rplacd pair new-cdr))

  (define (string-append . strings)
      (apply #'cl:concatenate (cons 'cl:string strings)))

  (cl:defstruct output-string-port
    contents)

  (define (write-char-to-string char output-string-port)
      (cl:declare (cl:type output-string-port output-string-port))
    (cl:vector-push-extend char (cl:slot-value output-string-port 'contents)))

  (define (open-output-string)
      (make-output-string-port :contents (cl:make-array '(0) :element-type 'cl:character :adjustable cl:t :fill-pointer cl:t)))

  (define (get-output-string stream &optional (reset? #f))
      (cl:prog1
	  (cl:slot-value stream 'contents)
	(if reset?
	    (cl:setf (cl:slot-value stream 'contents)
		     (cl:make-array '(0) :element-type 'cl:character :adjustable cl:t :fill-pointer cl:t)))))

  (cl:defun write-char (char &optional (port cl:*standard-output*))
    (cl:if (output-string-port-p port)
	   (write-char-to-string char port)
	   (cl:write-char char port)))

  (cl:defun read-char (&optional (port cl:*standard-input*))
    (cl:read-char port cl:nil (make-eof-object)))


  (cl:defmacro case (key &rest cases)
    `(cl:case ,key
       ,@(cl:loop for (matches . body) in cases
		  collect `(,(if (eq? matches 'else)
				 'cl:otherwise
				 matches) ,@body))))

  (cl:defun current-output-port (&optional new-port)
    (cl:prog1 cl:*standard-output*
      (cl:when new-port
	(set! cl:*standard-output* new-port))))

  (cl:defmacro when (condition &body body)
    `(if ,condition
	 (begin ,@body)))

  (cl:defmacro unless (condition &body body)
    `(when (not ,condition) ,@body))

  (define (for-each function . lists)
      (let loop ((lists lists))
	   (unless (memq '() lists)
	     (apply function (map #'car lists))
	     (loop (map #'cdr lists)))))

  (cl:defmacro let* (defs &body body)
    (if (null? defs)
	`(let () ,@body)
	`(let (,(car defs))
	   (let* ,(cdr defs)
	     ,@body))))

  (define (%assoc object alist matches?)
      (cond ((null? alist)
	     #f)
	    ((cl:funcall matches? (caar alist)
		       object)
	     (car alist))
	    (else
	     (%assoc object (cdr alist) matches?))))

  (define (assoc object alist)
      (%assoc object alist #'equal?))

  (define (assq object alist)
      (%assoc object alist #'eq?))

  (define (assv object alist)
      (%assoc object alist #'eqv?))

  (cl:defun = (&rest numbers)
    (not (null? (apply #'cl:= numbers))))

  (cl:defun < (&rest numbers)
    (not (null? (apply #'cl:< numbers))))

  (cl:defun > (&rest numbers)
    (not (null? (apply #'cl:> numbers))))

  (cl:defun >= (&rest numbers)
    (not (null? (apply #'cl:>= numbers))))

  (cl:defun <= (&rest numbers)
    (not (null? (apply #'cl:<= numbers))))
  
  (define (input-port? port)
      (not (null? (cl:streamp port)))))
