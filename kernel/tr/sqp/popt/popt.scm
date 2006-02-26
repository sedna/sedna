
; File:  popt.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)


; for Chicken
(declare (unit popt) (uses common-lib dschema xquery-lr))

(declare (foreign-declare "char* c_popt(const char*);"))
(define popt:c-popt (foreign-callback-lambda c-string* "c_popt" c-string))

(define debug-mode #f)

; for PLT
;(require (rename (lib "pretty.ss") pp pretty-print))
;
;(define debug-mode #t)




; Unit implements physical (cost based) optimization for Sedna
; Namespace prefix is "popt:". It stands for physical optimizer
; Also namespace prefix "att:" is used for operations on attributed
; tree


;===============================================================================
; Attributed tree (ATT)
;===============================================================================
; ATT ::= (att-attr att-value)
;
; att-attr ::= xpath-attr
;            | xpath-pred-attr
; att-value is an LR operation
;
; === XPath attribute ===
; xpath-attr ::= ('xpath (xp-entity xp-ent-name) xp-preds-num xp-op)
; xp-entity ::= 'collection
;             | 'document
; xp-ent-name ::= "string"
; xp-preds-num ::= int
;
; === XPath predicate attribute ===
; xpath-pred-attr ::= ('xpath-pred var xp-op)
;
; === XPath operation ===
; xp-op ::= ()
;         | ('path abs-path xp-op)
;         | ('pred xp-preds-num var xp-op xp-op)
;         | ('and@ xp-op*)
;         | ('or@ xp-op*)
;         | ('const ...)   ; like in LR
;         | ('=@  xp-op xp-op)
;         | ('!=@ xp-op xp-op)
;         | ('<@  xp-op xp-op)
;         | ('>@  xp-op xp-op)
;         | ('<=@ xp-op xp-op)
;         | ('>=@ xp-op xp-op)
;         | ('eq@ xp-op xp-op)
;         | ('ne@ xp-op xp-op)
;         | ('lt@ xp-op xp-op)
;         | ('le@ xp-op xp-op)
;         | ('gt@ xp-op xp-op)
;         | ('ge@ xp-op xp-op)
;
; abs-path ::= (node-test-or . . . node-test-or)
; node-test-or ::= (node-test . . . node-test)
; node-test ::= (axis-op node-test-type node-test-data)
;
; axis-op ::= PPAxisChild
;           | PPAxisAttribute
;           | PPAxisParent
;           | PPAxisSelf
;           | PPAxisDescendant
;           | PPAxisDescendantOrSelf
;           | PPAxisDescendantAttr
; node-test-type ::= processing_instruction
;                  | comment
;                  | text
;                  | node
;                  | string
;                  | qname
;                  | wildcard_star
;                  | wildcard_ncname_star
;                  | wildcard_star_ncname
;                  | function_call
;                  | var_name
;

;-------------------------------------------------------------------------------
; get attribute of the ATT
(define att:attr first)

;-------------------------------------------------------------------------------
; get value of the ATT
(define att:value second)

;-------------------------------------------------------------------------------
; abstract function for testing is tree is ATT with the specified prefix (symb)
(define (att:attributed-tree? tree symb)
  (and (list? tree)
       (not (null? tree))
       (list? (car tree))
       (not (null? (car tree)))
       (eq? (caar tree) symb)))

;-------------------------------------------------------------------------------
; is XPath ATT?
(define (att:xpath? expr)
  (att:attributed-tree? expr 'xpath))

;-------------------------------------------------------------------------------
; is XPath-pred ATT?
(define (att:xpath-pred? expr)
  (att:attributed-tree? expr 'xpath-pred))

;-------------------------------------------------------------------------------
; debug function for displaing ATT attribute
(define (att:display-attr attr)
  (let ((attr-name (car attr)))
    (cond
      ((eq? attr-name 'xpath)
       (begin
         (display "XPath:")
         (newline)
         (pp attr)
         (newline)))
      ((eq? attr-name 'xpath-pred)
       (begin
         (display "XPath Pred:")
         (newline)
         (pp attr)
         (newline)))
      (else 
       (begin
         (display "unknown")
         (newline))))))

;-------------------------------------------------------------------------------
; get xp-op of xpath pred att
(define att:xpath-pred-xp-op third)




;===============================================================================
; Utils
;===============================================================================

;-------------------------------------------------------------------------------
; functions converts node-test from LR notation to POR notation
; if it fails it calls (cont #f) where cont is a continuation
; passed to the functions
; node-test-or ::= (node-test . . . node-test)
; node-test ::= (axis-op node-test-type node-test-data)
(define (lr:node-test->po:node-test lr:node-test cont)
  (define (make-node-test-type-and-data lr:node-test-type)
    (define (process-qname qname)
      (match qname
        ['* '(wildcard_star ())]
        [`(,nc1 ,nc2)
          (cond 
            ((and (string? nc1)
                  (eq? nc2 '*))
             `(wildcard_ncname_star ,nc1))
            ((and (eq? nc1 '*)
                  (string? nc2))
             `(wildcard_star_ncname ,nc2))
            ((and (string? nc1)
                  (string? nc2))
             `(qname (,nc1 ,nc2))))]
        [_ (cont #f)]))
    (match lr:node-test-type
      [`(type
         (,test))
        (cond 
          ((eq? test 'pi-test)
           '(processing_instruction ()))
          ((eq? test 'comment-test)
           '(comment ()))
          ((eq? test 'text-test)
           '(text ()))
          ((eq? test 'node-test)
           '(node ()))
          (else (cont #f)))]
      [`(type
         (,test ,test-arg))
        (cond 
          ((eq? test 'attr-test)
           (match test-arg
             [`(ename 
                (const (type !xs!QName) ,qname)
                (type *)
                (const (type !xs!string) "non-nil"))
               (process-qname qname)]
             [_ (cont #f)]))
          ((eq? test 'elem-test)
           (match test-arg
             [`(ename 
                (const (type !xs!QName) ,qname)
                (type *)
                (const (type !xs!string) "non-nil"))
               (process-qname qname)]
             [_ (cont #f)]))
          (else (cont #f)))]
      [_ (cont #f)]))
  (let ((lr:axis (first lr:node-test))
        (lr:node-test-type (third lr:node-test)))
    (cond 
      ((eq? lr:axis 'attr-axis)
       (cons 'PPAxisAttribute
             (make-node-test-type-and-data lr:node-test-type)))
      ((eq? lr:axis 'child)
       (cons 'PPAxisChild
             (make-node-test-type-and-data lr:node-test-type)))
      ((eq? lr:axis 'descendant)
       (cons 'PPAxisDescendant
             (make-node-test-type-and-data lr:node-test-type)))
      ((eq? lr:axis 'descendant-attr)
       (cons 'PPAxisDescendantAttr
             (make-node-test-type-and-data lr:node-test-type)))
      ((eq? lr:axis 'descendant-or-self)
       (cons 'PPAxisDescendantOrSelf
             (make-node-test-type-and-data lr:node-test-type)))
      ((eq? lr:axis 'self)
       (cons 'PPAxisSelf
             (make-node-test-type-and-data lr:node-test-type)))
      (else (cont #f)))))






;===============================================================================
; Find XPath in a given query and optimize it (by calling C/C++ function)
;===============================================================================

;-------------------------------------------------------------------------------
; if 'expr' is a XPath expression than optimize it by calling corresponding
; C/C++ function (for debug mode just print out the XPath expression that have
; been found)
(define (popt:optimize-xpath expr)
  (if (att:xpath? expr)
      (let ((attr  (att:attr expr))
            (value (att:value expr)))
        (if debug-mode
            (begin
              (att:display-attr attr)
              value)
            (let* ((attr-str (cl:scheme-list->string attr))
                   (optimized-attr-str (popt:c-popt attr-str)))
              ;(display "!!!")(display optimized-attr-str)(display "!!!")
              ;(newline)
              (if (= (string-length optimized-attr-str) 0)
                  (begin ;(display "not optimized") (newline)
                    value)
                  (begin ;(display "optimized") (newline)
                    (cl:string->scheme-list optimized-attr-str))))))
      expr))


;-------------------------------------------------------------------------------
; Traverse function for suspecting XPath pred
; result ::= op | ((xpath-pred ...) op)
(define (popt:traverse-xpath-pred op var attr)
  (define (traverse op)
    (cond
      ((or (null? op) (not (pair? op)))
       op)
      
      ;; var
      ((and (xlr:var? op)
            (equal? (xlr:var-value op) ( xlr:var-value var)))
       `((xpath-pred ,(xlr:var-value var) ()) ,op))
      
      ;; const
      ((xlr:const? op)
       `((xpath-pred ,(xlr:var-value var) (const ,(xlr:type-value (second op)) ,(xlr:const-value op))) ,op))
      
      ;; XPath axis
      ((memq (xlr:op-name op) '(attr-axis 
                                child
                                descendant
                                descendant-attr
                                descendant-or-self
                                self))
       (let ((arg (traverse (first (xlr:op-args op))))
             (return (lambda (arg) `(,(xlr:op-name op)
                                      ,arg
                                      ,(second (xlr:op-args op))))))
         (cond 
           ((and (att:xpath-pred? arg)
                 (let ((xp-op (att:xpath-pred-xp-op (att:attr arg))))
                   (or (null? xp-op)
                       (eq? (car xp-op) 'path)
                       (eq? (car xp-op) 'const))))
            (cond
              ((call/cc 
                (lambda (cont)
                  (lr:node-test->po:node-test op cont))) 
               => (lambda (po:node-test)
                    (let ((xp-op (att:xpath-pred-xp-op (att:attr arg))))
                      `((xpath-pred 
                         ,(xlr:var-value var)
                         ,(cond 
                            ((and (not (null? xp-op)) (eq? (first xp-op) 'path))
                             `(path ,(append (second xp-op) (list (list po:node-test))) 
                                    ,(third xp-op)))
                            (else `(path ,(list (list po:node-test))
                                         ,xp-op))))
                        ,(return (att:value arg))))))
              (else (return (popt:optimize-xpath arg)))))
           (else (return arg)))))
      
      ;; comparison
      ((memq (xlr:op-name op) '(=@ !=@ <@ >@ <=@ >=@ eq@ ne@ lt@ le@ gt@ ge@))
       (let ((arg1 (traverse (first (xlr:op-args op))))
             (arg2 (traverse (second (xlr:op-args op))))
             (return (lambda (arg1 arg2) `(,(xlr:op-name op)
                                            ,arg1
                                            ,arg2))))
         (cond 
           ((and (att:xpath-pred? arg1)
                 (att:xpath-pred? arg2)
                 (let ((xp-op1 (att:xpath-pred-xp-op (att:attr arg1)))
                       (xp-op2 (att:xpath-pred-xp-op (att:attr arg2))))
                   (and (eq? (car xp-op1) 'const)
                        (or (null? xp-op2)
                            (eq? (car xp-op2) 'path)))))
            `((xpath-pred 
               ,(xlr:var-value var)
               (,(xlr:op-name op)
                 ,(att:xpath-pred-xp-op (att:attr arg2))
                 ,(att:xpath-pred-xp-op (att:attr arg1))))
              ,(return (att:value arg1) (att:value arg2))))
           ((and (att:xpath-pred? arg1)
                 (att:xpath-pred? arg2)
                 (let ((xp-op1 (att:xpath-pred-xp-op (att:attr arg1)))
                       (xp-op2 (att:xpath-pred-xp-op (att:attr arg2))))
                   (and (eq? (car xp-op2) 'const)
                        (or (null? xp-op1)
                            (eq? (car xp-op1) 'path)))))
            `((xpath-pred 
               ,(xlr:var-value var)
               (,(xlr:op-name op)
                 ,(att:xpath-pred-xp-op (att:attr arg1))
                 ,(att:xpath-pred-xp-op (att:attr arg2))))
              ,(return (att:value arg1) (att:value arg2))))
           (else (return arg1 arg2)))))
      
      ;; logical
      ((memq (xlr:op-name op) '(and@ or@))
       (let ((arg1 (traverse (first (xlr:op-args op))))
             (arg2 (traverse (second (xlr:op-args op))))
             (return (lambda (arg1 arg2) `(,(xlr:op-name op)
                                            ,arg1
                                            ,arg2))))
         (cond 
           ((and (att:xpath-pred? arg1)
                 (att:xpath-pred? arg2)
                 (let ((xp-op1 (att:xpath-pred-xp-op (att:attr arg1)))
                       (xp-op2 (att:xpath-pred-xp-op (att:attr arg2))))
                   (and (not (null? xp-op1))
                        (not (null? xp-op2))
                        (memq (car xp-op1) '(=@ !=@ <@ >@ <=@ >=@ eq@ ne@ lt@ le@ gt@ ge@ and@ or@))
                        (memq (car xp-op2) '(=@ !=@ <@ >@ <=@ >=@ eq@ ne@ lt@ le@ gt@ ge@ and@ or@)))))
            ;            (display (second (att:attr arg1)))
            ;            (newline)
            ;            (newline)
            ;(if (eq? (first (second (att:attr arg1))) (xlr:op-name op))
            ;    (cdr (att:xpath-pred-xp-op (att:attr arg1)))
            ;    (list (att:xpath-pred-xp-op (att:attr arg1))))
            `((xpath-pred 
               ,(xlr:var-value var)
               (,(xlr:op-name op)
                 ,@(if (eq? (first (att:xpath-pred-xp-op (att:attr arg1))) (xlr:op-name op))
                       (cdr (att:xpath-pred-xp-op (att:attr arg1)))
                       (list (att:xpath-pred-xp-op (att:attr arg1))))
                 ,@(if (eq? (first (att:xpath-pred-xp-op (att:attr arg2))) (xlr:op-name op))
                       (cdr (att:xpath-pred-xp-op (att:attr arg2)))
                       (list (att:xpath-pred-xp-op (att:attr arg2))))))
              ,(return (att:value arg1) (att:value arg2))))
           (else (return arg1 arg2)))))
      
      ;; DDO
      ((eq? (xlr:op-name op) 'ddo)
       (let ((arg (traverse (first (xlr:op-args op)))))
         (cond ((att:xpath-pred? arg) arg)
               (else `(ddo ,arg)))))
      
      (else op)))  
  
  (let ((expr (traverse op)))
    (if (att:xpath-pred? expr) 
        (let* ((attr  (att:attr expr))
               (xp-op (att:xpath-pred-xp-op attr)))
          ;(att:display-attr attr)
          (if (and (not (null? xp-op))
                   (or (memq (first xp-op) '(=@ !=@ <@ >@ <=@ >=@ eq@ ne@ lt@ le@ gt@ ge@))
                       (memq (first xp-op) '(and@ or@))))
              (if (eq? (first xp-op) 'and@)
                  expr
                  `((xpath-pred
                     ,(xlr:var-value var)
                     (and@ ,xp-op))
                    ,(att:value expr)))
              (att:value expr))) 
        (begin 
          ;          (display expr)
          ;          (newline)
          expr))))

;-------------------------------------------------------------------------------
; Cost based optimization of an operation
(define (popt:optimize-op op)
  (popt:optimize-xpath (popt:traverse-op op)))

;-------------------------------------------------------------------------------
; Basic traverse function of cost based optimization
; result ::= op | ((xpath ...) op)
(define (popt:traverse-op op)
  ;-----------------------------------------------------------------------------
  ; default action for popt:traverse-op (optimize args of the operation given)
  (define (popt:default-traverse-op op)
    (cons (xlr:op-name op) 
          (map popt:optimize-op (xlr:op-args op))))
  ;-----------------------------------------------------------------------------
  ; process predicate op (return, lreturn, select, lselect)
  (define (popt:process-predicate-op op var arg pred return-fun)
    (let ((arg-p (popt:traverse-op arg)))
      (cond 
        ((att:xpath? arg-p)
         (let ((pred-p (popt:traverse-xpath-pred pred var (att:attr arg-p)))
               (arg-attr (att:attr arg-p)))
           (cond 
             ((att:xpath-pred? pred-p)
              `((xpath ,(second arg-attr)
                       ,(+ (third arg-attr) 1)
                       ,(let ((arg-xp-op (fourth arg-attr))
                              (pred-xp-op (att:xpath-pred-xp-op (att:attr pred-p))))
                          
                          (cond 
                            ((eq? (first arg-xp-op) 'path)
                             `(pred ,(third arg-attr) ,(xlr:var-value var) ,arg-xp-op ,pred-xp-op))
                            ((eq? (first arg-xp-op) 'pred)
                             `(pred ,(second arg-xp-op)
                                    ,(xlr:var-value var)
                                    ,(third arg-xp-op)
                                    (and ,(fourth arg-xp-op)
                                         ,pred-xp-op)))
                            (else (cl:signal-input-error SE1051 "popt:traverse-op for return/lreturn"))))) 
                ,(return-fun (att:value arg-p)
                             (att:value pred-p))))
             (else (return-fun (popt:optimize-xpath arg-p)
                               pred-p)))))
        (else (return-fun arg-p
                          (popt:optimize-op pred))))))
  
  (cond
    ((or (null? op) (not (pair? op)))
     op)
    ((or (xlr:var? op)
         (memq (xlr:op-name op) '(const type ivar)))
     op)
    
    ;; fun-def
    ((xlr:fun-def? op)
     `(,(car op)  ; 'fun-def
        ,(xlr:var-defs op)
        ,(popt:optimize-op (xlr:fun-body op))))
    
    ;; fn:document, collection
    ((and (or (eq? (xlr:op-name op) '!fn!document)
              (eq? (xlr:op-name op) '!fn!collection))
          (xlr:const? (car (xlr:op-args op)))
          (let ((value (xlr:const-value (car (xlr:op-args op)))))
            (and (string? value)
                 (not (eq? (car (string->list value)) #\$)))))
     `((xpath (document ,(xlr:const-value (car (xlr:op-args op)))) 0 (path () ())) ,op))
    
    ;; XPath axis
    ((memq (xlr:op-name op) '(attr-axis 
                              child
                              descendant
                              descendant-attr
                              descendant-or-self
                              self))
     (let ((arg (popt:traverse-op (first (xlr:op-args op))))
           (return (lambda (arg) `(,(xlr:op-name op)
                                    ,arg
                                    ,(second (xlr:op-args op))))))
       (cond 
         ((att:xpath? arg)
          (cond
            ((call/cc 
              (lambda (cont)
                (lr:node-test->po:node-test op cont))) 
             => (lambda (po:node-test)
                  (let ((attr (att:attr arg)))
                    `((xpath 
                       ,(second attr) 
                       ,(third attr) 
                       ,(let ((xp-op (fourth attr)))
                          (cond 
                            ((eq? (first xp-op) 'path)
                             `(path ,(append (second xp-op) (list (list po:node-test))) 
                                    ,(third xp-op)))
                            ((eq? (first xp-op) 'pred)
                             `(path ,(list (list po:node-test))
                                    ,xp-op))
                            (else (cl:signal-input-error SE1051 "popt:traverse-op for axis")))))
                      ,(return (att:value arg))))))
            (else (return (popt:optimize-xpath arg)))))
         (else (return arg)))))
    
    ;; DDO
    ((eq? (xlr:op-name op) 'ddo)
     (let ((arg (popt:traverse-op (first (xlr:op-args op)))))
       (cond ((att:xpath? arg) arg)
             (else `(ddo ,arg)))))
    
    ;; return and lreturn
    ((memq (xlr:op-name op) '(return lreturn))
     (let ((op-name (xlr:op-name op)))
       (match op
         [`(,op-name
             ,arg
             (fun-def
              ((,type ,var1))
              (if@
               ,pred
               ,var2
               (sequence))))         
           
           (if (equal? var1 var2)
               (popt:process-predicate-op op var1 arg pred 
                                          (lambda (arg pred)
                                            `(,op-name
                                               ,arg
                                               (fun-def
                                                ((,type ,var1))
                                                (if@
                                                 ,pred
                                                 ,var1
                                                 (sequence))))))
               (popt:default-traverse-op op))]
         [_ (popt:default-traverse-op op)])))
    
    ;; select and lselect
    ((memq (xlr:op-name op) '(select lselect))
     (let ((op-name (xlr:op-name op)))
       (match op
         [`(,op-name
             ,arg
             (fun-def
              ((,type ,var))
              ,pred))
           (popt:process-predicate-op op var arg 
                                      (lambda (arg pred) 
                                        `(,op-name
                                           ,arg
                                           (fun-def
                                            ((,type ,var))
                                            ,pred))))]
         [_ (popt:default-traverse-op op)])))
    
    ;; default action
    (else
     (popt:default-traverse-op op))))


;-------------------------------------------------------------------------------
; Cost based optimization of a user-defined function
;  named-func ::= (list  'declare-function
;                        name  formal-args  return-type
;                        body-expr)
(define (popt:optimize-func-declaration decl)
  (if
   (or (null? decl) (not (pair? decl))
       (not (eq? (xlr:op-name decl) 'declare-function)))
   decl  ; don't rewrite it
   (list (xlr:op-name decl)
         (car (xlr:op-args decl))
         (xlr:named-func-args decl)
         (caddr (xlr:op-args decl))  ; return type
         (popt:optimize-op (xlr:named-func-body decl)))))

;-------------------------------------------------------------------------------
; High-level function
; Cost based optimization of a query
(define (popt:optimize-query query)
  (cond
    ((or (null? query) (not (pair? query)))
     ; nothing to do, although it's strange
     query)
    ((eq? (xlr:op-name query) 'query)
     `(query
       (prolog
        ,@(map popt:optimize-func-declaration
               (xlr:get-query-prolog query)))
       (query-body
        ,(popt:optimize-op (xlr:get-query-body query)))))
    (else  ; update or smth
     (popt:optimize-op query))))


;(declare (foreign-declare "char* c_opt_print(const char*);"))
;(define c-opt-print (foreign-callback-lambda c-string "c_opt_print" c-string))
;(define (popt:optimize-query query)
;  (newline)
;  (display (c-opt-print "hello"))
;  (newline)
;  query)


;(define (popt:optimize-query query)
;  (newline)
;  (display (dscm:get-dschema "auction" 'document))
;  (cl:write-to-file (dscm:get-dschema "auction" 'document) "dschema")
;  (newline)
;  query)
