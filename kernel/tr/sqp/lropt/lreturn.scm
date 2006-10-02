
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
; Prefix for this part of the module is `lropt:'
;
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
;                                    order-required-for-result?
;                                    (listof  order-required-for-argument?)
;                                    rewritten-declare-function-clause
;                             ))
; order-required-for-result? - may be #f for one function call and may become
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

; Accessors to a member of `processed-funcs'
;  processed-func ::= (list  func-name
;                            order-required-for-result?
;                            (listof  order-required-for-argument?)
;                            rewritten-declare-function-clause)
(define procced-func-name        car)
(define procced-func-for-result  cadr)
(define procced-func-for-args    caddr)
(define procced-func-declaration cadddr)                  

; Default result to recover from unexpected input error
(define (lropt:input-error expr processed-funcs)
  ; Can add some message diagnostics here
  (values expr #f #f #f processed-funcs '()))
                        
; General Expr
; Prototype borrowed from `sa:analyze-expr' and `xquery-functions'
; Out - in the form of values:
;  expr - the rewritten expression
;  ddo-auto? - whether DDO is supported automatically by expression
;  zero-or-one? - whether zero-or-one node is returned by the expr
;  single-level? - whether all nodes on a single level
;  processed-funcs - as in In-part
;  order-for-variables - whether order required for variables encountered inside
(define (lropt:expr expr called-once? order-required?
                    var-types prolog processed-funcs)
  (if
   (not (pair? expr))
   (lropt:input-error expr processed-funcs)
   (case (xlr:op-name expr)  ; operation name
     ((var)
      (sa:variable-wrapped expr vars funcs ns-binding default-ns))
     ;-------------------
     ; 2.2 Constants
     ((const)
      ; The same result for functions with no arguments
      (values expr #t #t #t processed-funcs '()))
     ;-------------------
     ; Axes
     ((attr-axis child self)
      ; These axes should have been processed by `lropt:ddo'
      (display (xlr:op-name expr))
      #f)
     ((ancestor ancestor-or-self  descendant descendant-or-self
                following following-sibling parent preceding
                preceding-sibling)
      ; ATTENTION: namespace axis
      (display (xlr:op-name expr))
      (sa:analyze-axis expr vars funcs ns-binding default-ns))
     ;-------------------
     ; 2.4 Sequence
     ((sequence space-sequence spaceseq)
      (lropt:propagate expr called-once? order-required?
                       var-types prolog processed-funcs
                       #f  ; no automatical DDO
                       #f  ; not zero-or-one
                       #f  ; not single-level
                       ))
     ((unio)
      #f)
     ;-------------------
     ; 2.5, 2.6 Arithmetic and comparison operations
     ((+@ -@ *@ div@ idiv@ mod@ /@ unary+@ unary-@
        eq@ ne@ lt@ le@ gt@ ge@ is@ <<@ >>@)
      ; ATTENTION [*]: In general, ordering is required for arguments,
      ; since an argument may consist of several duplicate values
      (lropt:propagate expr called-once?
                       #f  ; see attention comment [*] above
                       var-types prolog processed-funcs
                       #t #t #t))
     ((to@)
      (lropt:propagate expr called-once?
                       #f  ; [*]
                       var-types prolog processed-funcs
                       #t  ; values can be considered ordered
                       #f  ; in general, contains more than one value
                       #t))
     ((=@ !=@ <@ <=@ >@ >=@)
      (lropt:propagate expr called-once?
                       #f  ; the order of arguments is not important
                       var-types prolog processed-funcs
                       #t #t #t))
     ;-------------------
     ; 2.7 Conditional operation
     ((if if@)
      (sa:analyze-if expr vars funcs ns-binding default-ns))
     ;-------------------
     ; 2.8 Logical operations
     ((and@ or@)
      ; The same as for general comparison operators
      ; TODO: think of introducing a function for this
      (lropt:propagate expr called-once?
                       #f  ; EBW is taken => order not important
                       var-types prolog processed-funcs
                       #t #t #t))
     ;-------------------
     ; 2.9 Constructors
     ((element attribute pi namespace)
      (lropt:propagate expr called-once?
                       ; Only a single item in name => order not important,
                       ; minus duplicates [*]
                       (list #f #t)
                       var-types prolog processed-funcs
                       #t #t #t))
     ((document text comment)
      (lropt:propagate expr called-once?
                       #t  ; spaceseq implicitly taken => order is important
                       var-types prolog processed-funcs
                       #t #t #t))
     ;-------------------
     ; 2.10 FLWOR Operations
     ((let@)
      (sa:analyze-let@ expr vars funcs ns-binding default-ns))
     ((return)
      (sa:analyze-return expr vars funcs ns-binding default-ns))
     ((predicate)
      (sa:analyze-predicate expr vars funcs ns-binding default-ns))
     ((order-by)
      (sa:analyze-order-by expr vars funcs ns-binding default-ns))
     ((orderspecs)
      (sa:analyze-multiple-orderspecs expr vars funcs ns-binding default-ns))
     ;-------------------
     ; 2.11 Expressions on Sequence Types
     ((ts)
      (sa:analyze-typeswitch expr vars funcs ns-binding default-ns))
     ((treat instance-of)
      (sa:analyze-treat expr vars funcs ns-binding default-ns))
     ((cast)
      (sa:analyze-cast expr vars funcs ns-binding default-ns))
     ((castable)
      (sa:analyze-castable expr vars funcs ns-binding default-ns))
     ;-------------------
     ; 2.14 Distinct document order
     ((ddo)
      (lropt:ddo expr called-once? order-required?
                 var-types prolog processed-funcs))
     ;-------------------
     ; 3.6. Quantified expressions
     ((some every)
      (sa:some-every expr vars funcs ns-binding default-ns))
     ;-------------------
     ; 3.7 XQuery 1.0 Functions
     ((!fn!document)
      (lropt:propagate expr called-once? #f  ; [*]
                       var-types prolog processed-funcs
                       #t
                       #t  ; only a single item
                       #t))
     ((!fn!collection)
      (lropt:propagate expr called-once? #f  ; [*]
                       var-types prolog processed-funcs
                       #t
                       #f  ; generally, more than one item
                       #t))
     ; Functions with no arguments
     ((!fn!position !fn!last !fn!true !fn!false !se!checkpoint)
      (values expr #t #t #t processed-funcs '()))
     ((!fn!count !fn!sum !fn!avg !fn!max !fn!min)
      (lropt:propagate expr called-once?
                       #t  ; argument ordering required
                       var-types prolog processed-funcs
                       #t #t #t))
     ((!fn!name
       !fn!document-uri
       !fn!node-name
       !fn!node-kind
       !fn!namespace-uri
       !fn!boolean
       !fn!not)
      #f)
     ((!fn!empty !fn!exists)
      (lropt:propagate expr called-once? #f  ; order not required
                       var-types prolog processed-funcs
                       #t #t #t))
     ((!fn!data
       !fn!error
       !fn!trace
       !fn!insert-before
       !fn!remove
       !fn!reverse
       !fn!zero-or-one
       !fn!one-or-more
       !fn!exactly-one
       !fn!concat
       !fn!distinct-values
       !fn!string-value
       !fn!string-length
       !fn!typed-value
       !fn!string
       !fn!contains
       !fn!translate
       !fn!deep-equal
       !fn!replace
       !fn!matches
       !fn!subsequence)
      #f)
     ((!fn!years-from-duration
       !fn!months-from-duration !fn!days-from-duration !fn!hours-from-duration       
       !fn!minutes-from-duration !fn!seconds-from-duration !fn!year-from-dateTime
       !fn!month-from-dateTime !fn!day-from-dateTime !fn!hours-from-dateTime
       !fn!minutes-from-dateTime !fn!seconds-from-dateTime
       !fn!timezone-from-dateTime !fn!year-from-date !fn!month-from-date
       !fn!day-from-date !fn!timezone-from-date !fn!hours-from-time
       !fn!minutes-from-time !fn!seconds-from-time !fn!timezone-from-time
       !fn!adjust-dateTime-to-timezone !fn!adjust-date-to-timezone
       !fn!adjust-time-to-timezone)
      (lropt:propagate expr called-once? #f  ; [*]
                       var-types prolog processed-funcs
                       #t #t #t))
      ((!fn!sql-exec-update
       !fn!sql-connect
       !fn!sql-prepare
       !fn!sql-execute
       !fn!sql-close
       !fn!sql-commit
       !fn!sql-rollback
       !fn!index-scan
       !fn!index-scan-between
       !fn!ftindex-scan
       !fn!ftscan
       !fn!fthighlight
       !fn!fthighlight2
       !fn!is_ancestor
       !fn!filter_entry_level
       !fn!item-at
       !fn!test
       !fn!local-name)
      #f)
     ;-------------------
     ; Union operations
     ((union@ intersect@ except@)       
      (lropt:propagate expr called-once? #f
                       var-types prolog processed-funcs
                       #t  ; union operations perform ordering themselves
                       #f #f))
     ;-------------------
     ; Not expressed in the new logical representation
     ((fun-call)
      #f
      ;(sa:analyze-fun-call expr vars funcs ns-binding default-ns)
      )
     ;-------------------    
     (else  ; unknown operations
      (lropt:input-error expr processed-funcs)))))

;-------------------------------------------------
; Propagating

; Unites 2 alists of the form `order-for-variables'
;  order-for-variables ::= (listof (cons var-name order-required?))
(define (lropt:unite-order-for-variables alist1 alist2)
  (if (null? alist1)
      alist2
      (let ((pair1 (car alist1)))
        (cond
          ((assoc (car pair1) alist2)
           => (lambda (pair2)
                (if
                 (or (eqv? (cdr pair1) (cdr pair2))
                     ; ordering required for pair2 => we can disregard pair1
                     (cdr pair2))
                 (lropt:unite-order-for-variables (cdr alist1) alist2)
                 (cons pair1
                       (lropt:unite-order-for-variables
                        (cdr alist1)                        
                        (filter  ; TODO: can probably optimize
                         (lambda (entry) (not (eq? entry pair2)))
                         alist2))))))
          (else
           (cons
            (pair1)
            (lropt:unite-order-for-variables (cdr alist1) alist2)))))))

; Propagate processing the expression to its subexpressions
; The function has the same general signature, except for
; 1. Parts of the result are passed as arguments, namely,
;    ddo-auto? zero-or-one? single-level?
; 2. order-required-for-args ::= order-required? |
;                                (listof order-required?)
; order-required-for-args is a boolean when the same ordering is required
; for all the arguments of the expression.
; order-required-for-args is a list when different arguments require
; different ordering specifications. 
; (length (listof order-required?)) = (length (xlr:op-args expr))
(define (lropt:propagate expr called-once? order-required-for-args
                         var-types prolog processed-funcs
                         ddo-auto? zero-or-one? single-level?)
  (if  ; optimizing the case for an expression with no arguments
   (null? (xlr:op-args expr))
   (values expr ddo-auto? zero-or-one? single-level?
           processed-funcs
           '()  ; no variables inside
           )
   (call-with-values
    (lambda ()
      (if (pair? order-required-for-args)
          (values car cdr)
          (values (lambda (x) x) (lambda (x) x))))
    (lambda (order-accessor order-next)
      (let loop ((args (xlr:op-args expr))
                 (order-required-for-args order-required-for-args)
                 (res '())
                 (processed-funcs processed-funcs)
                 (order-for-variables '()))
        (if
         (null? args)  ; everyone processed
         (values (cons (xlr:op-name expr)
                       (reverse res))
                 ddo-auto? zero-or-one? single-level?
                 processed-funcs order-for-variables)
         (call-with-values
          (lambda () (lropt:expr (car args)
                                 called-once?
                                 (order-accessor order-required-for-args)
                                 var-types prolog processed-funcs))
          (lambda (new-arg dummy-auto? dummy-0-or-1? dummy-level?
                           processed-funcs order-for-vars-in-arg)
            (loop (cdr args)
                  (order-next order-required-for-args)
                  (cons new-arg res)
                  processed-funcs
                  (if (null? order-for-variables)
                      order-for-vars-in-arg
                      (lropt:unite-order-for-variables
                       order-for-vars-in-arg  ; this list is generally shorter
                       order-for-variables)))))))))))


;=========================================================================
; Rewriting special logical operations

; DDO
(define (lropt:ddo expr called-once? order-required?
                   var-types prolog processed-funcs)
  (let ((arg0 (car (xlr:op-args expr))))
    (if
     (memq (xlr:op-name arg0) '(attr-axis child self))
     (let ((arg1 (car (xlr:op-args arg0))))
       (if  ; Another 'ddo inside
        (and (pair? arg1)
             (eq? (xlr:op-name arg1) 'ddo))
        ; Can either rewrite these axes into descendant-attr, etc. or
        ; eliminate the current 'ddo operation by pushing it down            
        (let ((arg2 (car (xlr:op-args arg1))))
          ; For rewriting, arg2 must be descendant-or-self::node()
          (if
           (and (pair? arg2)
                (eq? (xlr:op-name arg2) 'descendant-or-self)
                (equal? (cadr (xlr:op-args arg2))
                        '(type (node-test))))
           (call-with-values
            (lambda () (lropt:expr (car (xlr:op-args arg2))
                                   called-once?
                                   #f  ; as if already ordered
                                   var-types prolog processed-funcs))
            (lambda (new-arg2 ddo-auto? zero-or-one? single-level?
                              processed-funcs order-for-variables)
              (let ((new-arg0
                     (list
                      (cond
                        ((assq (xlr:op-name arg0)
                               '((child . descendant)                         
                                 (attr-axis . descendant-attr)
                                 (self . descendant-or-self)))
                         => cdr)
                        (else
                         'lreturn-module-internal-error))
                      new-arg2
                      (cadr (xlr:op-args arg0))  ; node test
                      )))
                (values
                 (if (or
                      (and
                       ddo-auto?  ; order achieved automatically
                       ; all nodes on single level => the result of applying
                       ; descendant axis is ordered automatically
                       single-level?)
                      (not order-required?))
                     new-arg0
                     (list (xlr:op-name expr)  ; == 'ddo
                           new-arg0))
                 (or order-required?  ; if order required, it is achieved
                     (and ddo-auto? single-level?)  ; or automatically
                     )
                 ; Was rewritten from:
                 ;(and (not order-required?)  ; it was not required
                 ;     ; no order automatically
                 ;     (not (and ddo-auto? single-level?)))
                 (and zero-or-one?
                      ; zero-or-one if both new-arg2 produces zero-or-one
                      ; item and the 'self axis is applied
                      (eq? (xlr:op-name arg0) 'self))
                 single-level?
                 processed-funcs order-for-variables))))
           ; Otherwise - process nested 'ddo recursively
           (call-with-values
            (lambda () (lropt:ddo arg1 called-once?
                                  order-required?  ; was: #f
                                  var-types prolog processed-funcs))
            (lambda (new-arg1 ddo-auto? zero-or-one? single-level?
                              processed-funcs order-for-variables)
              (values
               (list (xlr:op-name arg0)  ; axis name
                     new-arg1
                     (cadr (xlr:op-args arg0)))
               ; If order was required from arg1, it was achieved
               ddo-auto?  ; was: order-required?
               (and zero-or-one?
                    ; zero-or-one if both new-arg2 produces zero-or-one
                    ; item and the 'self axis is applied
                    (eq? (xlr:op-name arg0) 'self))
               single-level?  ; is preserved
               processed-funcs order-for-variables)))))
        ; Either attr-axis, child or self, but no 'ddo inside
        (call-with-values
         (lambda () (lropt:expr arg1 called-once?
                                #f  ; as if order achieved
                                var-types prolog processed-funcs))
         (lambda (new-arg1 ddo-auto? zero-or-one? single-level?
                           processed-funcs order-for-variables)
           (values
            (cond
              ((or ddo-auto? (not order-required?))
               (list (xlr:op-name arg0)  ; 'child or 'attr-axis or 'self
                     new-arg1
                     (cadr (xlr:op-args arg0))))
              ; order required
              ((eq? (xlr:op-name arg0) 'self)
               ; Self axis => order after filtering
               (list
                (xlr:op-name expr)  ; == 'ddo
                (list (xlr:op-name arg0)  ; 'child or 'attr-axis or 'self
                      new-arg1
                      (cadr (xlr:op-args arg0)))))
              (else  ; order before applying the axis
               (list (xlr:op-name arg0)  ; 'child or 'attr-axis or 'self
                     (list (xlr:op-name expr)  ; == 'ddo
                           new-arg1)
                     (cadr (xlr:op-args arg0)))))
            ; Order _not_ achieved when
            ; 1. It was not required, and
            ; 2. It is not achieved automatically
            (or order-required? ddo-auto?)  ; was: order-required?
            ; Was rewritten from:
            ; (not (and (not order-required?)
            ;           (not ddo-auto?)))
            (and zero-or-one?
                 ; zero-or-one if both new-arg2 produces zero-or-one
                 ; item and the 'self axis is applied
                 (eq? (xlr:op-name arg0) 'self))
            single-level?  ; is preserved
            processed-funcs order-for-variables)))))
       ; not attr-axis, child and self
       (call-with-values
        (lambda () (lropt:expr arg0 called-once?
                               #f  ; as if already ordered
                               var-types prolog processed-funcs))
        (lambda (new-arg0 ddo-auto? zero-or-one? single-level?
                          processed-funcs order-for-variables)
          (values
           (if (or ddo-auto?  ; order achieved automatically
                   (not order-required?))
               new-arg0
               (list (xlr:op-name expr)  ; == 'ddo
                     new-arg0))
           (or order-required?  ; if order is required, it is achieved
               ddo-auto?)
           zero-or-one? single-level?
           processed-funcs order-for-variables))))))


;=========================================================================
; High-level function

; Rewrites the query:
;  1. Creates LReturns where appropriate
;  2. Rewrites XPath axes that involve descendant-or-self
;  3. Eliminates unnecessary 'ddo operations
(define (lropt:rewrite-query query)
  (cond
    ((or (null? query) (not (pair? query)))
     ; nothing to do, although it's strange
     query)
    ((eq? (xlr:op-name query) 'query)
     (let ((prolog (xlr:get-query-prolog query)))
       `(query
         (prolog
          ,@prolog
          ;,@(map mlr:rewrite-func-declaration
          ;       (xlr:get-query-prolog query))
          )
       (query-body
        ,(call-with-values
          (lambda ()
            (lropt:expr
             (xlr:get-query-body query) 
             #t  ; called once
             #t  ; TODO: ordered or unordered depending on prolog
             '()  ; TODO: probably, external variables here
             prolog
             '()  ; no functions processed yet
             ))
          (lambda (body dummy-auto? dummy-0-or-1? dummy-level?
                        processed-funcs unite-order-for-variables)
            ; TODO: combine processed funcs with prolog
            body))))))
    (else  ; update or smth
     ;(mlr:lreturn+xpath query #t)
     #f
     )))
