;(define command-line argv)
(define system ##shell-command)

(define close-output-string close-output-port)

;; Port information: see lib/_io.scm from gambit sources.

(##define-macro (port-input-char-count port)                                    
   `(##vector-ref ,port 16)) 

(##define-macro (port-input-line-count port)                                    
   `(##vector-ref ,port 17)) 

(##define-macro (port-input-line-start port)                                    
   `(##vector-ref ,port 18))                                                    

(##define-macro (port-name port)
  `(##vector-ref ,port 4))

; This function is based on SRFI-1 Reference implementation by Olin Shivers. 
; Original version may be found at  http://www.ai.mit.edu/~shivers/
;  Changes: Parameter-checking is removed
;           Dotted lists permitted
(define (filter pred lis)			
  (let recur ((lis lis))		
    (if (not (pair? lis)) lis			
	(let ((head (car lis))
	      (tail (cdr lis)))
	  (if (pred head)
	      (let ((new-tail (recur tail)))	; Replicate the RECUR call so
		(if (eq? tail new-tail) lis
		    (cons head new-tail)))
	      (recur tail))))))			; this one can be a tail call.

; SRFI-2 and-let* : it evaluates a sequence of forms one after another
; till the first one that yields #f; the non-#f result of a form can be bound
; to a fresh variable and used in the subsequent forms.
;  From Oleg Kiselyovs's vland.scm,v 1.0 1998/02/21 
(define-macro (and-let* claws . body)
  (let* ((new-vars '()) (result (cons 'and '())) (growth-point result))
			; We need a way to report a syntax error
			; the following is how Gambit compiler does it...
    (##define-macro (ct-error-syntax msg . args)
      `(##signal '##signal.syntax-error #t ,msg ,@args))

    (define (andjoin! clause)
      (let ((prev-point growth-point) (clause-cell (cons clause '())))
        (set-cdr! growth-point clause-cell)
        (set! growth-point clause-cell)))

    (if (not (list? claws))
      (ct-error-syntax "bindings must be a list " bindings))
    (for-each 
      (lambda (claw)
        (cond
          ((symbol? claw)	; BOUND-VARIABLE form
            (andjoin! claw))
          ((and (pair? claw) (null? (cdr claw))) ; (EXPRESSION) form
            (andjoin! (car claw)))
						; (VARIABLE EXPRESSION) form
          ((and (pair? claw) (symbol? (car claw))
              (pair? (cdr claw)) (null? (cddr claw)))
            (let* ((var (car claw)) (var-cell (cons var '())))
              (if (memq var new-vars)
                (ct-error-syntax "duplicate variable " var " in the bindings"))
              (set! new-vars (cons var new-vars))
              (set-cdr! growth-point `((let (,claw) (and . ,var-cell))))
              (set! growth-point var-cell)))
          (else
            (ct-error-syntax "An ill-formed binding in a syntactic form land* " 
              claw))
        ))
      claws)
    (if (not (null? body))
      (andjoin! `(begin ,@body)))
    result))

