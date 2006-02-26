;************************************************************************
; myenv.scm
; This file is the part of SSAX package (http://ssax.sourceforge.net),
; which is in public domain.


;KL
; Chicken-enabled version of Oleg Kiselyov's Standard Scheme "Prelude"
;

; 		   My Standard Scheme "Prelude"
;
; This version of the prelude contains several forms and procedures
; that are specific to a Gambit-C 3.0 system.
; See myenv-scm.scm, myenv-bigloo.scm, etc. for versions
; of this prelude that are tuned to other Scheme systems.
;


;;; assert truth of an expression (or a sequence of expressions)
;;; if there is more than one expression, they're 'AND'ed
(define-macro (assert . x)
                  (if (null? (cdr x))
                    `(or ,@x (error "failed assertion" ',@x))
                    `(or (and ,@x) (error "failed assertion" '(,@x)))))

;;;
(define (assure exp error-msg)
  (or exp (error error-msg)))


;;; Gambit's include and declare are disabled
;(define-macro (include file) `(begin #f))
;(define-macro (declare . x)  `(begin #f))


;;; like cout << arguments << args
;;;   where argument can be any Scheme object. If it's a procedure
;;;   (without args) it's executed rather than printed (like newline)
(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

(define (cerr . args)
  (for-each (lambda (x)
    (if (procedure? x) (x (current-error-port)) (display x (current-error-port))))
            args))

(define nl (string #\newline))

(define (identify-error msg args . disposition-msgs)
  (let ((port (current-error-port)))
    (newline port)
    (display "ERROR" port)
    (display msg port)
    (for-each (lambda (msg) (display msg port))
	      (append args disposition-msgs))
    (newline port)))

; Some useful increment/decrement operators
; Note, ##fixnum prefix is Gambit-specific, it means that the
; operands assumed FIXNUM (as they ought to be anyway).
; This perfix could be safely removed: it'll leave the code just as
; correct, but more portable (and less efficient)

; Mutable increment
(define-macro (++! x) `(set! ,x (+ 1 ,x)))

; Read-only increment
(define-macro (++ x) `(add1 ,x))

; Mutable decrement
(define-macro (--! x) `(set! ,x (- ,x 1)))

; Read-only decrement
(define-macro (-- x) `(sub1 ,x))


; Some useful control operators

			; if condition is true, execute stmts in turn
			; and return the result of the last statement
			; otherwise, return #f
(define-macro (when condition . stmts)
  `(and ,condition (begin ,@stmts)))
  

			; if condition is false execute stmts in turn
			; and return the result of the last statement
			; otherwise, return #t
			; This primitive is often called 'unless'
(define-macro (whennot condition . stmts)
  `(or ,condition (begin ,@stmts)))


			; Execute a sequence of forms and return the
			; result of the _first_ one. Like PROG1 in Lisp.
			; Typically used to evaluate one or more forms with
			; side effects and return a value that must be
			; computed before some or all of the side effects
			; happen.
(define-macro (begin0 form . forms)
  (let ((var (gensym)))
    `(let ((,var ,form)) ,@forms ,var)))

			; Prepend an ITEM to a LIST, like a Lisp macro PUSH
			; an ITEM can be an expression, but ls must be a VAR
(define-macro (push! item ls)
  `(set! ,ls (cons ,item ,ls)))

			; Is str the empty string?
			; string-null? str -> bool
			; See Olin Shiver's Underground String functions
(define-macro (string-null? str) `(zero? (string-length ,str)))


; Like let* but allowing for multiple-value bindings
(define-macro (let-values* bindings . body)
  (if (null? bindings) (cons 'begin body)
      (apply (lambda (vars initializer)
	 (let ((cont 
		(cons 'let-values* 
		      (cons (cdr bindings) body))))
	   (cond
	    ((not (pair? vars))		; regular let case, a single var
	     `(let ((,vars ,initializer)) ,cont))
	    ((null? (cdr vars))		; single var, see the prev case
	     `(let ((,(car vars) ,initializer)) ,cont))
	   (else			; the most generic case
	    `(receive ,vars ,initializer ,cont)))))
       (car bindings))))


			; assoc-primitives with a default clause
			; If the search in the assoc list fails, the
			; default action argument is returned. If this
			; default action turns out to be a thunk,
			; the result of its evaluation is returned.
			; If the default action is not given, an error
			; is signaled

(define-macro (assq-def key alist . default-action-arg)
  (let ((default-action
        (if (null? default-action-arg)
          `(error "failed to assq key '" ,key "' in a list " ,alist)
          (let ((defact-symb (car default-action-arg)))
            `(if (procedure? ,defact-symb) (,defact-symb) ,defact-symb)))))
    `(or (assq ,key ,alist) ,default-action)))

(define-macro (assv-def key alist . default-action-arg)
  (let ((default-action
        (if (null? default-action-arg)
          `(error "failed to assv key '" ,key "' in a list " ,alist)
          (let ((defact-symb (car default-action-arg)))
            `(if (procedure? ,defact-symb) (,defact-symb) ,defact-symb)))))
    `(or (assv ,key ,alist) ,default-action)))

(define-macro (assoc-def key alist . default-action-arg)
  (let ((default-action
        (if (null? default-action-arg)
          `(error "failed to assoc key '" ,key "' in a list " ,alist)
          (let ((defact-symb (car default-action-arg)))
            `(if (procedure? ,defact-symb) (,defact-symb) ,defact-symb)))))
    `(or (assoc ,key ,alist) ,default-action)))


; Convenience macros to avoid quoting of symbols
; being deposited/looked up in the environment
(define-macro (env.find key) `(%%env.find ',key))
(define-macro (env.demand key) `(%%env.demand ',key))
(define-macro (env.bind key value) `(%%env.bind ',key ,value))

