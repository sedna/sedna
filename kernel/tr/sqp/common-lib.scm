
; File:  common-lib.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

(declare (unit common-lib) (uses srfi-1))

;Lib of common functions
;Uses SRFI-1
;Namespace prefix is "cl:". It stands for common lib



(define debug #f)

;---------------------------------------------------------------------
(define (debug:print str expr)
  (if debug
      (begin (display str) (display ":") (newline)
             (display expr) expr)
      expr))

;---------------------------------------------------------------------
(define (debug:print-strs . args)
  (if debug
      (map (lambda (x) (display x) (newline)) args)
      '()))

;---------------------------------------------------------------------
;This function is used when we want to stop processing because of the error
(define (error str . strs)
;  (raise-exception (cons str strs))
  (display str) (newline)
  (map (lambda (x) (display x) (newline)) strs)
  (exit))


;---------------------------------------------------------------------
(define (cl:exception-helper type)
  (lambda (code . msg)
    (signal
     (make-property-condition
      'exn
      'type type
      'code code
      'message (apply
                string-append
                (map
                 (lambda (single)
                   (cond
                     ((string? single) single)
                     ((symbol? single) (symbol->string single))
                     ((and (pair? single) (symbol? (car single)))
                      (string-append "operation "
                                     (symbol->string (car single))))
                     (else "")))
                 msg))))))

(define cl:signal-user-error (cl:exception-helper 'user))
(define cl:signal-input-error (cl:exception-helper 'input))

 
;---------------------------------------------------------------------
(define (cl:remove-dublicates l)
  (if (null? l)
      '()
      (cons 
       (car l)
       (cl:remove-dublicates
        (remove 
         (lambda (x) (equal? x (car l)))
         (cdr l))))))

;---------------------------------------------------------------------
; Returns (list type code msg)
; type - symbol, msg - string 
(define (cl:get-exception-message exn)
  (reverse
   (cons
    ((condition-property-accessor 'exn 'message) exn)
    (handle-exceptions
     huy
     `(,SE5100 system)
     (list ((condition-property-accessor 'exn 'code) exn)
           ((condition-property-accessor 'exn 'type) exn))))))

;---------------------------------------------------------------------
; converts scheme list represented as string to scheme list
(define (cl:string->scheme-list str)
  (read (open-input-string str)))

;---------------------------------------------------------------------
; converts scheme list to scheme list represented as string
(define (cl:scheme-list->string lst)
  (let ((o (open-output-string)))
    (write lst o)
    (get-output-string o)))

;---------------------------------------------------------------------
; writes argument to file
(define (cl:write-to-file x file)
  (let* ((port (open-output-file file)))
    (if (list? x)
        (for-each (lambda (y) (begin (write y port) (newline port) (newline port))) x)
        (write x port))
    (close-output-port port)))

;---------------------------------------------------------------------
; read from file
(define (cl:read-from-file file)
  (let* ((port (open-input-file file))
         (result (read port))) 
    (close-input-port port)
    result))

