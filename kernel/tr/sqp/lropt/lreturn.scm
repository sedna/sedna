
; File:  lreturn.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

(declare (unit lreturn) (uses xquery-lr))

;; Basic logical optimization:
;   1. lreturn
;   2. Combining XPath axes
; 
; If the second step doesn't contain position-based predicates, the following
; replacement are made:
;  a) descendant-or-self::node()/child::?  --> descendant::?
;  b) descendant-or-self::node()/attribute::?  --> descendant-attr::?
;  c) descendant-or-self::node()/self::?  --> descendant-or-self::?
;
; The prefix for this module is "mlr:"

; Combination of map and append
(define (mlr:map-append f lst)
  (if (null? lst)
      lst
      (append (f (car lst))
              (mlr:map-append f (cdr lst)))))

; Returns the list of all free variable names presented in the expr
; Result: (listof var-name)
; The list may contain duplicates
(define (mlr:find-free-vars expr)
  (letrec
      ((expr-walk
        ; bound-vars ::= (listof var-name)
        (lambda (expr bound-vars)         
          (cond
            ((or (null? expr) (not (pair? expr)))
             '())
            ((xlr:var? expr)
             (if
              (memq (xlr:var-name expr) bound-vars)
              '()
              (list (xlr:var-name expr))))
            ((memq (xlr:op-name expr) '(const type ivar))
             '())
            ((xlr:fun-def? expr)
             (expr-walk
              (xlr:fun-body expr)
              (append
               (map cadr (xlr:var-defs expr))  ; function argument names
               bound-vars)))
            (else  ; any other expression
             (mlr:map-append              
              (lambda (sub-expr) (expr-walk sub-expr bound-vars))
              (xlr:op-args expr)))))))
    (expr-walk expr '())))


;-------------------------------------------------
; Finds lreturns and combines XPath axes
;  called-once? - whether the current expression will be called just once, or
; within the iteration

; Applies mlr:lreturn+xpath to subexpression and reconstructs the expr
(define (mlr:propagate expr called-once?)
  (cons (xlr:op-name expr)
        (map
         (lambda (subexpr)
           (mlr:lreturn+xpath subexpr called-once?))
         (xlr:op-args expr))))

(define (mlr:lreturn+xpath expr called-once?)
  (cond
    ((or (null? expr) (not (pair? expr)))
     expr)
    ((or (xlr:var? expr)
         (memq (xlr:op-name expr) '(const type ivar)))
     expr)
    ((xlr:fun-def? expr)
     `(,(car expr)  ; 'fun-def
       ,(xlr:var-defs expr)
       ,(mlr:lreturn+xpath (xlr:fun-body expr) called-once?)))
    ((memq (xlr:op-name expr) '(attr-axis child self))
     (let ((arg1 (car (xlr:op-args expr))))
       (if
        ; not a DDO
        (or (null? arg1) (not (pair? arg1))
            (not (eq? (xlr:op-name arg1) 'ddo)))
        (mlr:propagate expr called-once?)
        (let ((arg2 (car (xlr:op-args arg1))))
          ; For rewriting, arg2 must be descendant-or-self::node()
          (if
           (or (null? arg2) (not (pair? arg2))
               (not (eq? (xlr:op-name arg2) 'descendant-or-self))
               (not (equal? (cadr (xlr:op-args arg2)) '(type (node-test)))))
           (mlr:propagate expr called-once?)
           `(,(cond
                ((assq (xlr:op-name expr)
                       '((child . descendant)                         
                         (attr-axis . descendant-attr)
                         (self . descendant-or-self)))
                 => cdr)
                (else
                 (xlr:signal-error
                  "mlr:lreturn+xpath: internal error: " expr)))
             ,(mlr:lreturn+xpath (car (xlr:op-args arg2)) called-once?)
             ,(cadr (xlr:op-args expr))))))))
    ((memq (xlr:op-name expr) '(return select some every))
     (cons
      (if
       (and (not called-once?)
            (null? (mlr:find-free-vars (car (xlr:op-args expr)))))            
       (cond
         ((assq (xlr:op-name expr)
                '((return . lreturn)
                  (select . lselect)
                  (some . lsome)
                  (every . levery)))
          => cdr)
         (else
          (xlr:signal-error
           "mlr:lreturn+xpath: internal error: " expr)))
       (xlr:op-name expr))
      (map
       (lambda (subexpr) (mlr:lreturn+xpath subexpr #f))
       (xlr:op-args expr))))
    ((eq? (xlr:op-name expr) 'replace)  ; update replace operation
     (let ((target (mlr:lreturn+xpath
                    (car (xlr:op-args expr)) called-once?))
           (fun (mlr:lreturn+xpath
                 (cadr (xlr:op-args expr)) #f)))
       (if
        (not (and (xlr:fun-def? fun)
                  (= (length (xlr:var-defs fun)) 1)))
        (xlr:signal-error
         "mlr:lreturn+xpath: improper representation for replace update: " expr)
        `(replace
          (return
           ,target
           (,(car fun)  ; 'fun-def
             ,(xlr:var-defs fun)
             (sequence
               ,(cadr  ; variable
                 (car  ; fun-def has the only argument
                  (xlr:var-defs fun)))
               ,(xlr:fun-body fun)
               (const (type !se!separator) 1))))))))
    (else
     (mlr:propagate expr called-once?))))

; Rewrites a function declaration
;  named-func ::= (list  'declare-function
;                        name  formal-args  return-type
;                        body-expr)
(define (mlr:rewrite-func-declaration decl)
  (if
   (or (null? decl) (not (pair? decl))
       (not (eq? (xlr:op-name decl) 'declare-function)))
   decl  ; don't rewrite it
   (list (xlr:op-name decl)
         (car (xlr:op-args decl))
         (xlr:named-func-args decl)
         (caddr (xlr:op-args decl))  ; return type
         (mlr:lreturn+xpath (xlr:named-func-body decl) #t))))

;-------------------------------------------------
; High-level function

; Rewrites the query:
;  1. Creates LReturns where appropriate
;  2. Rewrites XPath axes that involve descendant-or-self
(define (mlr:rewrite-query query)
  (cond
    ((or (null? query) (not (pair? query)))
     ; nothing to do, although it's strange
     query)
    ((eq? (xlr:op-name query) 'query)
     `(query
       (prolog
        ,@(map mlr:rewrite-func-declaration
               (xlr:get-query-prolog query)))
       (query-body
        ,(mlr:lreturn+xpath (xlr:get-query-body query) #t))))
    (else  ; update or smth
     (mlr:lreturn+xpath query #t))))


;=========================================================================
; In this section, the new API for keeping only required 'ddo operations
; is constructed
; In:
;  expr - expression to process
;  called-once? - inherited from lreturn
;  order-required? - whether order required for the result of this expr
;  var-types - alist for each variable type in scope
;  prolog - query prolog, for user-declared function calls
;  processed-funcs - alist of user-declared functions with rewritten bodies
;
;  var-types ::= (listof (cons var-name var-type))
;  var-name ::= (list namespace-uri local-part)
;  namespace-uri, local-part ::= strings
;  var-type - logical representation for XQuery sequence type
; Example: ((("" "e") . (one (node-test)))
;           (("" "s") . (zero-or-more (node-test)) ))
;
; processed-funcs ::= (listof (list  func-name
;                                    ddo-required-for-result?
;                                    (listof  order-required-for-argument?)
;                                    rewritten-declare-function-clause
;                             ))
; ddo-required-for-result? - may be #f for one function call and may become
;  #t for another function call. In such a case, the alist entry for the
;  given function is replaced
;
; Out - in the form of values:
;  expr - the rewritten expression
;  ddo-auto? - whether DDO is supported automatically by expression
;  zero-or-one? - whether zero-or-one node is returned by the expr
;  single-level? - whether all nodes on a single level
;  processed-funcs - as in In-part
;  order-for-variables - whether order required for variables encountered inside
;
;  order-for-variables ::= (listof (cons var-name order-required?))
;  var-name - the same as in In-part
