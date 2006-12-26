
; File:  xquery-lr-lib.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

(declare (unit xquery-lr) (uses common-lib srfi-1))

(define (xlr:var? x) (eq? (car x) 'var))
(define (xlr:ivar? x) (eq? (car x) 'ivar))

;Replaces all free occurances of var (i.e. (var symbol))with value in expr
(define (xlr:substitute-var-value var value expr)
  (let rpt ((expr expr))
    (cond 
      ((xlr:var? expr)
       (if (equal? (xlr:var-value expr)  ; list of 2 members
                   (xlr:var-value var))
           value expr))
      ((xlr:ivar? expr) expr)
      ((xlr:const? expr) expr)
      ((xlr:type? expr) expr)
      ((xlr:fun-def? expr)
       (if
        (member var (map cadr (xlr:var-defs expr)))
        ; was: (lset<= equal? (list var) (map cadr (xlr:var-defs expr)))
        ; was: (lset<= eq? (list (xlr:var-value var))
        ;                  (map cadr (xlr:var-defs expr)))
        expr
        `(fun-def ,(xlr:var-defs expr)
                  ,(rpt (xlr:fun-body expr)))))
      (else
       `(,(car expr) ,@(map rpt (xlr:op-args expr)))))))


;;error fixed
;;(substitute-var-value 's '(const 1) '(return s (fun-def ((unk s)) s)))
;;returned
;;(return (const 1) (fun-def ((unk s)) (const 1)))
;
;;(display "xquery-lr") (newline)

(define (xlr:op-name expr)
  (car expr))

(define (xlr:op-args expr)
  (cdr expr))

(define (xlr:type-value expr)
  (cadr expr))

(define (xlr:type? expr)
  (and (list? expr)
       (not (null? expr))
       (eq? (car expr) 'type)))

(define (xlr:const? expr)
  (and (list? expr)
       (not (null? expr))
       (eq? (car expr) 'const)))
  

(define (xlr:const-value expr)
  (caddr expr))

(define (xlr:var-value expr)
  (cadr expr))

(define (xlr:var-eq? var1 var2)
  (eq? (xlr:var-value var1) (xlr:var-value var2)))

;Functios for QName - (const (type !xs!QName) ("namespace-uri-name" "local-name"))  
(define (xlr:namespace-name expr)
  (car (xlr:const-value expr)))

(define (xlr:local-name expr)
  (cadr (xlr:const-value expr)))

;Functions for fun-def
(define (xlr:fun-def? x) 
  (and (list? x)
       (not (null? x))
       (eq? (car x) 'fun-def)))

(define (xlr:var-defs fun-def) (cadr fun-def))

(define (xlr:fun-body fun-def) (caddr fun-def))


;==========================================================================
; DL: my addition

; Error signalling
; Clone from "sa.scm"
(define (xlr:signal-error . msg)
  (let ((string-msg
         (apply
          string-append
          (map
           (lambda (single)
             (cond
               ((string? single) single)
               ((symbol? single) (symbol->string single))
               ((and (pair? single) (symbol? (car single)))
                (string-append "operation " (symbol->string (car single))))
               (else "")))
           msg))))
    (signal (make-property-condition 'exn 'message string-msg))))

;-------------------------------------------------
; Accessors for a query

(define (xlr:get-query-prolog query)
  (cond
    ((assq 'prolog (cdr query))
     => cdr)
    (else
     (xlr:signal-error
      "xlr:get-query-prolog: prolog not found for query: " query))))

(define (xlr:get-query-body query)
  (cond
    ((assq 'query-body (cdr query))
     => cadr)
    (else
     (xlr:signal-error
      "xlr:get-query-body: query body not found for query: " query))))


;-------------------------------------------------
; Context-involved operations

;  var-def ::= (list (list var-type) var-name)
(define (xlr:var-name var-def)
  (cadr var-def))

; Adds a new member to context
;  context ::= (listof  var-def)
;  added-element ::= var-def
(define (xlr:add-one-to-context context added-element)
  (cons
   added-element
   (let ((added-name (xlr:var-name added-element)))
     (filter
      (lambda (x) (not (eq? (xlr:var-name x) added-name)))
      context))))

; Returns var-def
(define (xlr:find-in-context context var-name)
  (let loop ((cnt context))
    (cond
      ((null? cnt)  ; nothing found
       (xlr:signal-error "xlr:find-in-context: variable " var
                         " not defined in context: " context))
      ((eq? (xlr:var-name (car cnt)) var-name)
       (car cnt))
      (else
       (loop (cdr cnt))))))  


;-------------------------------------------------
; Functions over constants
;  constant ::= (list 'const  (list 'type type)  value)
;  type - Scheme symbol
;  value - depends on type

; Constructor
(define (xlr:make-const type value)
  `(const (type ,type) ,value))

; Constructors for typed constants
(define (xlr:make-bool-const value)
  (xlr:make-const '!xs!boolean value))
(define (xlr:make-integer-const value)
  (xlr:make-const '!xs!integer value))
(define (xlr:make-string-const value)
  (xlr:make-const '!xs!string value))

; Predicates for typed constants
(define (xlr:const-boolean? expr)
  (and (xlr:const? expr)
       (xlr:type? (car (xlr:op-args expr)))
       (eq? (cadadr expr) '!xs!boolean)))
(define (xlr:const-string? expr)
  (and (xlr:const? expr)
       (xlr:type? (car (xlr:op-args expr)))
       (eq? (cadadr expr) '!xs!string)))

; Predicates for constant boolean values
(define (xlr:const-bool-true? expr)
  (and (xlr:const? expr)
       (xlr:type? (car (xlr:op-args expr)))
       (eq? (cadadr expr) '!xs!boolean)
       (equal? (cadr (xlr:op-args expr)) #t)  ; DL: not sure about this
       ))
(define (xlr:const-bool-false? expr)
  (and (xlr:const? expr)
       (xlr:type? (car (xlr:op-args expr)))
       (eq? (cadadr expr) '!xs!boolean)
       (equal? (cadr (xlr:op-args expr)) #f)  ; DL: not sure about this
       ))


;-------------------------------------------------
; Deals with named functions declarations
;  named-func ::= (list  'declare-function
;                        name  formal-args  return-type
;                        body-expr)

; Unsafe accessor
(define (xlr:named-func-args-u named-func)
  (cadr (xlr:op-args named-func)))

; Safer accessor
(define (xlr:named-func-args named-func)
  (let ((formal-args (cadr (xlr:op-args named-func))))
    (if
     (member #f
             (map
              (lambda (arg)
                (and (list? arg) (= (length arg) 2)
                     (or
                      (symbol? (cadr arg))
                      (and (list? (cadr arg))
                           (eq? 'var (caadr arg))))))
              formal-args))
     (xlr:signal-error
      "xlr:named-func-args: improper argument list for function: "
      named-func)
     formal-args)))

(define (xlr:named-func-body named-func)
  (cadddr (xlr:op-args named-func)))

; Analog of 'apply-fun-in-context'
;  actual-params ::= (listof expr)
; Returns expr to be inlined instead of the function call
(define (xlr:inline-fun-call named-func actual-params)
  (letrec
      ((xlr:inline
        ; alist ::= (listof (list param-name . param-value))
        (lambda (expr alist)
          ;(display "recursive inline entered: ")
          ;(pp expr)
          ;(newline)
          (cond
            ((or (not (pair? expr))  ; leave nodes
                 (xlr:const? expr) (xlr:type? expr))
             expr)
            ((xlr:var? expr)
             (cond
               ((assq (car (xlr:op-args expr))  ; var name
                      alist)
                => cdr)
               (else  ; it is a free variable occurence
                expr)))
            ((xlr:fun-def? expr)
             (let ((new-alist
                    (let ((rebound  ; parameters rebound in fun-def body
                           (map cadr (xlr:var-defs expr))))
                      (filter
                       (lambda (pair)
                         (not (memq (car pair) rebound)))
                       alist))))
               (if (null? new-alist)  ; everything is rebound
                   expr
                   (list (xlr:op-name expr)
                         (xlr:var-defs expr)
                         (xlr:inline (xlr:fun-body expr) new-alist)))))            
            (else  ; propagates inlining
             (cons
              (xlr:op-name expr)
              (map
               (lambda (subexpr) (xlr:inline subexpr alist))               
               (xlr:op-args expr))))))))
    (let ((formal-args (xlr:named-func-args named-func)))
      (if
       (not (= (length formal-args) (length actual-params)))
       (xlr:signal-error
        "xlr:inline-fun-call: number of formal and actual parameters "
        "mismatch for function call to " named-func " with arguments "
        actual-params)       
       (xlr:inline
        (xlr:named-func-body named-func)
        (map
         (lambda (formal actual)
           (cons (cadr formal) actual))
         formal-args actual-params))))))


;-------------------------------------------------
; Whether an empty sequence
(define (xlr:nil? x)
  (and (pair? x) (null? (cdr x)) (eq? (car x) 'sequence)))


;-------------------------------------------------
; Applies to iterator operations (such as return, select, some, every)
; and returns left arg (that supplies the sequence being iterated)
(define (xlr:left-arg expr)
  (car (xlr:op-args expr)))
