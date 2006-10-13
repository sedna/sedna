
; File:  lreturn.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

(declare (unit lreturn) (uses xquery-lr))

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
;  var-types ::= (listof  var-type-assoc)
;  var-type-assoc ::= (list var-name var-type) |
;                     (list var-name var-type
;                           ddo-auto? zero-or-one? single-level?)
;  var-name ::= (list namespace-uri local-part)
;  namespace-uri, local-part ::= strings
;  var-type - logical representation for XQuery sequence type
; Example: ((("" "e") (one (node-test)))
;           (("" "s") (zero-or-more (node-test)) ))
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
      (lropt:var expr called-once? order-required?
                 var-types prolog processed-funcs))
     ;-------------------
     ; 2.2 Constants
     ((const)
      ; The same result for functions with no arguments
      (values expr #t #t #t processed-funcs '()))
     ;-------------------
     ; Axes
     ((ancestor ancestor-or-self)
      (lropt:ancestor expr called-once? order-required?
                      var-types prolog processed-funcs))     
     ((attr-axis child)
      (lropt:attr-axis expr called-once? order-required?
                       var-types prolog processed-funcs))
     ((descendant descendant-or-self)
      (lropt:descendant expr called-once? order-required?
                        var-types prolog processed-funcs))
     ((following)
      (lropt:following expr called-once? order-required?
                       var-types prolog processed-funcs))
     ((following-sibling)
      (lropt:following-sibling expr called-once? order-required?
                               var-types prolog processed-funcs))
     ; ATTENTION: namespace axis
     ((parent)
      (lropt:parent expr called-once? order-required?
                    var-types prolog processed-funcs))
     ((preceding)
      (lropt:preceding expr called-once? order-required?
                       var-types prolog processed-funcs))
     ((preceding)
      (lropt:preceding-sibling expr called-once? order-required?
                               var-types prolog processed-funcs))
     ((self)
      (lropt:self expr called-once? order-required?
                  var-types prolog processed-funcs))
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
      ; Generally, arguments of unio are variables, each variable
      ; is bound with a single item exactly. Ordering is thus not
      ; required for unio arguments.
      ; This supposition requires attention in the future, however.
      (lropt:propagate expr called-once? #f
                       var-types prolog processed-funcs
                       #t #t #t))
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
      (let ((and-last-two
             (lambda (test branch alternative)
               (or branch alternative))))
        (lropt:propagate expr called-once?
                         ; order not required for test condition
                         (list #f order-required? order-required?)
                         var-types prolog processed-funcs
                         and-last-two and-last-two and-last-two)))
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
      (lropt:let expr called-once? order-required?
                 var-types prolog processed-funcs))
     ((return)
      (lropt:return expr called-once? order-required?
                    var-types prolog processed-funcs))
     ((predicate)
      (lropt:predicate expr called-once? order-required?
                       var-types prolog processed-funcs))
     ((order-by)
      (lropt:order-by expr called-once? order-required?
                      var-types prolog processed-funcs))
     ((orderspecs)
      (lropt:orderspecs expr called-once? order-required?
                        var-types prolog processed-funcs))
     ;-------------------
     ; 2.11 Expressions on Sequence Types
     ((ts)
      (lropt:ts expr called-once? order-required?
                var-types prolog processed-funcs))
     ((cast treat)
      (lropt:cast+treat expr called-once? order-required?
                          var-types prolog processed-funcs))
     ((instance-of castable)
      (lropt:instance-of+castable expr called-once? order-required?
                                  var-types prolog processed-funcs))
     ;-------------------
     ; 2.14 Distinct document order
     ((ddo)
      (lropt:ddo expr called-once? order-required?
                 var-types prolog processed-funcs))
     ;-------------------
     ; 3.6. Quantified expressions
     ((some every)
      (lropt:some-every expr called-once? order-required?
                        var-types prolog processed-funcs))
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
     ((!fn!position !fn!last !fn!true !fn!false
                    !se!checkpoint !fn!current-dateTime)
      (values expr #t #t #t processed-funcs '()))
     ((!fn!count !fn!deep-equal !fn!sum !fn!avg !fn!max !fn!min)
      (lropt:propagate expr called-once?
                       #t  ; argument ordering required
                       var-types prolog processed-funcs
                       #t #t #t))
     ; Accept zero-or-one items for each argument,
     ; return zero-or-one item in the result
     ((; 2 Accessors
       !fn!node-name
       !fn!string !fn!document-uri
       ; XQuery datamodel accessors
       !fn!node-kind !fn!string-value !fn!typed-value
       ; 6.4 Functions on Numeric Values
       !fn!abs !fn!ceiling !fn!floor !fn!round !fn!round-half-to-even
       ; 7.4 Functions on String Values
       !fn!concat !fn!string-length !fn!translate
       ; 7.5 Functions Based on Substring Matching
       !fn!contains
       ; 7.6 String Functions that Use Pattern Matching
       !fn!replace !fn!matches
       ; 9.3 Functions on Boolean Values
       !fn!not !fn!boolean
       ; 14 Functions and Operators on Nodes
       !fn!name !fn!local-name !fn!namespace-uri !fn!number
       ; 15.2 Functions That Test the Cardinality of Sequences
       !fn!exactly-one)
      ; The same semantics as for !fn!document
      (lropt:propagate expr called-once? #f  ; [*]
                       var-types prolog processed-funcs
                       #t #t #t))
     ((!fn!empty !fn!exists)
      (lropt:propagate expr called-once? #f  ; order not required
                       var-types prolog processed-funcs
                       #t #t #t))
     ((!fn!data !fn!distinct-values !fn!zero-or-one !fn!one-or-more)
      (let ((identity (lambda (x) x)))
        ; NOTE: Should be updated when collations are introduced to
        ; fn:distinct-values in Sednas
        (lropt:propagate expr called-once? order-required?
                         var-types prolog processed-funcs
                         identity identity identity)))
     ((!fn!error)  ; 3 The Error Function
      (lropt:propagate expr called-once?
                       (lambda (arg-lng)
                         (if (= arg-lng 3)
                             '(#f #f #t)
                             (xlr:make-list #f arg-lng)))
                       var-types prolog processed-funcs
                       #t #t #t))
     ((!fn!trace)  ; 4 The Trace Function
      ; Note XQuery Functions and Operators, Sect. 4:
      ; "The ordering of output from invocations of the fn:trace()
      ; function is implementation dependent."
      (let ((return-first
             (lambda (value label) value)))
        (lropt:propagate expr called-once?
                         (list order-required? #f)
                         var-types prolog processed-funcs
                         return-first return-first return-first)))
     ((!fn!insert-before)
      (lropt:propagate expr called-once?
                       (list #t
                             #f  ; see [*]
                             #t)
                       var-types prolog processed-funcs
                       #f #f #f))
     ((!fn!remove)
      (let ((return-first
             (lambda (target position) target)))
        (lropt:propagate expr called-once?
                         (list #t #f)
                         var-types prolog processed-funcs
                         ; Order, zero-or-one and single-level are preserved
                         ; when an item is removed from a sequences
                         return-first return-first return-first)))
     ((!fn!reverse)
      (let ((identity (lambda (x) x)))
        (lropt:propagate expr called-once? order-required?
                         var-types prolog processed-funcs
                         #f  ; order not preserved
                         ; zero-or-one and single-level are preserved after
                         ; reversing a sequence
                         identity identity)))
     ((!fn!subsequence)
      (let ((return-first  ; can be supplied with either 2 or 3 arguments
             (lambda x (car x))))
        (lropt:propagate expr called-once?
                         (lambda (arg-lng)
                           (if (= arg-lng 2)
                               '(#t #f)
                               '(#t #f #f)))
                         var-types prolog processed-funcs
                         ; Order, zero-or-one and single-level are preserved
                         ; when selecting a subsequence
                         return-first return-first return-first)))
     ; 10.5 Component Extraction Functions on Durations, Dates and Times
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
     ; SQL connection functions
     ((!fn!sql-connect !fn!sql-close !fn!sql-commit !fn!sql-rollback
                       !fn!sql-prepare !fn!sql-exec-update)
      (lropt:propagate expr called-once? #f  ; [*]
                       var-types prolog processed-funcs
                       #t #t #t))
     ((!fn!sql-execute)
      (lropt:propagate expr called-once? #f  ; [*]
                       var-types prolog processed-funcs
                       #f #f #f))
     ; Index scan functions and full-text search functions
     ((!fn!index-scan !fn!index-scan-between !fn!ftindex-scan)
      (lropt:propagate expr called-once? #f  ; [*]
                       var-types prolog processed-funcs
                       #f #f #f))
     ((!fn!ftscan)
      (let ((return-first  ; can be supplied with either 2 or 3 arguments
             (lambda x (car x))))
        (lropt:propagate expr called-once?
                         (lambda (arg-lng)
                           (if (= arg-lng 3)
                               (list order-required? #f #f)
                               (list order-required? #f #f #f)))
                         var-types prolog processed-funcs
                         return-first return-first return-first)))
     ((!fn!fthighlight !fn!fthighlight2)
      ; I failed to find any documentation for highlight functions, so
      ; I do not care much about correct ordering for them
      (lropt:propagate expr called-once? #f  ; [*]
                       var-types prolog processed-funcs
                       #f #f #f))
     ((!fn!is_ancestor)
      (lropt:propagate expr called-once? #t
                       var-types prolog processed-funcs
                       #t #t #t))
     ((!fn!filter_entry_level)
      (let ((identity (lambda (x) x)))
        (lropt:propagate expr called-once? order-required?
                         var-types prolog processed-funcs
                         identity identity identity)))
     ((!fn!item-at)
      (lropt:propagate expr called-once? (list #t #f)
                       var-types prolog processed-funcs
                       #t #t #t))
     ((!fn!test)
      ; Do not know the correct semantics
      (lropt:propagate expr called-once? #f
                       var-types prolog processed-funcs
                       #f #f #f))
     ;-------------------
     ; Union operations
     ((union@ intersect@ except@)       
      (lropt:propagate expr called-once? #f
                       var-types prolog processed-funcs
                       #t  ; union operations perform ordering themselves
                       #f #f))
     ;-------------------
     ; Function call
     ((fun-call)
      (lropt:fun-call expr called-once? order-required?
                      var-types prolog processed-funcs))
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
            pair1
            (lropt:unite-order-for-variables (cdr alist1) alist2)))))))

; Constructs the (listof value), whose length is num
(define (xlr:make-list value num)
  (if (= num 0)
      '()
      (cons value (xlr:make-list value (- num 1)))))

; Propagate processing the expression to its subexpressions
; The function has the same general signature, except for
; 1. Parts of the result are passed as arguments, namely,
;    ddo-auto-for-result zero-or-one single-level
; 2. ddo-auto-for-result ::= ddo-auto? |
;                            (lambda (ddo-auto1? ddo-auto2? ...) ...)
;  ddo-auto-for-result can be a boolean when the value of ddo-auto? for
;  expression result is independent of ddo-auto? values for expression
;  arguments. Otherwise, ddo-auto-for-result can be a predicate that
;  accepts the ddo-auto? values for all expression arguments and
;  calculates ddo-auto? for expression result
; 3. The same semantics applies to zero-or-one and single-level
; 4. order-required-for-args ::= order-required? |
;                                (listof order-required?)
;  order-required-for-args is a boolean when the same ordering is required
;  for all the arguments of the expression.
;  order-required-for-args is a list when different arguments require
;  different ordering specifications. 
;  (length (listof order-required?)) = (length (xlr:op-args expr))
(define (lropt:propagate expr called-once? order-required-for-args
                         var-types prolog processed-funcs
                         ddo-auto-for-result zero-or-one single-level)
  (if  ; optimizing the case for an expression with no arguments
   (null? (xlr:op-args expr))
   (values expr
           (if (procedure? ddo-auto-for-result)
               (ddo-auto-for-result)
               ddo-auto-for-result)
           (if (procedure? zero-or-one)
               (zero-or-one)
               zero-or-one)
           (if (procedure? single-level)
               (single-level)
               single-level)
           processed-funcs
           '()  ; no variables inside
           )
   (let ((order-required-for-args
          (if (procedure? order-required-for-args)
              (order-required-for-args  ; for the variable number of arguments
               (length (xlr:op-args expr)))
              order-required-for-args)))
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
                   (order-for-variables '())
                   (ddo-auto-lst '())
                   (zero-or-1-lst '())
                   (level-lst '()))
          (if
           (null? args)  ; everyone processed
           (values (cons (xlr:op-name expr)
                         (reverse res))
                   (if (procedure? ddo-auto-for-result)
                       (apply ddo-auto-for-result (reverse ddo-auto-lst))
                       ddo-auto-for-result)
                   (if (procedure? zero-or-one)
                       (apply zero-or-one (reverse zero-or-1-lst))
                       zero-or-one)
                   (if (procedure? single-level)
                       (apply single-level (reverse level-lst))
                       single-level)
                   processed-funcs order-for-variables)
           (call-with-values
            (lambda () (lropt:expr (car args)
                                   called-once?
                                   (order-accessor order-required-for-args)
                                   var-types prolog processed-funcs))
            (lambda (new-arg auto? zero-or-1? level?
                             processed-funcs order-for-vars-in-arg)
              (loop (cdr args)
                    (order-next order-required-for-args)
                    (cons new-arg res)
                    processed-funcs
                    (if (null? order-for-variables)
                        order-for-vars-in-arg
                        (lropt:unite-order-for-variables
                         order-for-vars-in-arg  ; this list is generally shorter
                         order-for-variables))
                    (cons auto? ddo-auto-lst)
                    (cons zero-or-1? zero-or-1-lst)
                    (cons level? level-lst)))))))))))


;=========================================================================
; Rewriting special logical operations

; Whether variable contains no more than one item according to
; its type specification
(define (lropt:var-type-zero-or-one? type-spec)
  (or
   (and (pair? type-spec)
        (memq (car type-spec) '(optional one)))
   (and (symbol? type-spec)
        ; Simple atomic type
        (not (memq type-spec '(xs:anyType !xs!anyType !xs!anytype))))))
  
; Variable reference
(define (lropt:var expr called-once? order-required?
                   var-types prolog processed-funcs)
  (let* ((var-name (car (xlr:op-args expr)))
         (type-entry
          (cond
            ((assoc var-name var-types)
             => (lambda (x) x))
            (else
             ; Variable type declaration not found -
             ; this should not happen!
             (list var-name #f))))
         (zero-or-one?
          (lropt:var-type-zero-or-one? (cadr type-entry))))
    (if
     (null? (cddr type-entry))  ; entry contains just 2 members
     (values expr
             zero-or-one?  ; would stand for ddo-auto?
             zero-or-one?  ; zero-or-one?
             zero-or-one?  ; single-level?
             processed-funcs             
             (list  ; order-for-variables
              (cons var-name
                    (and order-required? (not zero-or-one?)))))
     (values expr
             (or  ; value for ddo-auto?:
              zero-or-one?  ; in accordance with type
              (list-ref type-entry 2)  ; the real situation
              )
             (or  ; zero-or-one?
              zero-or-one?  ; in accordance with type
              (list-ref type-entry 3)  ; the real situation
              )
             (or  ; single-level?
              zero-or-one? (list-ref type-entry 4))
             processed-funcs             
             (list  ; order-for-variables
              (cons var-name
                    (and order-required?
                         (not zero-or-one?)
                         (not (list-ref type-entry 2))  ; no ddo-auto
                         )))))))

;-------------------
; Axes

; can-sustain-order? - provided that argument expression is ordered, is this
;  axis capable of producing the automatically ordered result
(define (lropt:axis-helper can-sustain-order? ddo-auto-handler
                           zero-or-one-handler single-level-handler)
  (lambda (expr called-once? order-required?
                var-types prolog processed-funcs)
    (call-with-values
     (lambda ()
       (lropt:expr
        (car (xlr:op-args expr))  ; child expression
        called-once?
        (and order-required? can-sustain-order?)
        var-types prolog processed-funcs))
     (lambda (new-expr ddo-auto? zero-or-one? single-level?
                       processed-funcs order-for-vars)
       (values
        (list (xlr:op-name expr)  ; axis name
              new-expr
              (cadr (xlr:op-args expr))  ; node test specification
              )
        (ddo-auto-handler ddo-auto? zero-or-one? single-level?)
        (zero-or-one-handler zero-or-one? single-level?)
        (single-level-handler zero-or-one? single-level?)
        processed-funcs
        order-for-vars)))))

(define lropt:ancestor
  (lropt:axis-helper
   #f  ; can-sustain-order?
   (lambda (ddo-auto? zero-or-one? single-level?) #f)
   (lambda (zero-or-one? single-level?) #f)
   (lambda (zero-or-one? single-level?) #f)))
   
(define lropt:ancestor-or-self lropt:ancestor)

; ATTENTION: lropt:child is implemented as an alias for this function
(define lropt:attr-axis
  (lropt:axis-helper
   #t  ; can-sustain-order?
   (lambda (ddo-auto? zero-or-one? single-level?) ddo-auto?)
   (lambda (zero-or-one? single-level?) #f)  ; zero-or-one-handler
   ; single-level-handler
   (lambda (zero-or-one? single-level?) single-level?)))

(define lropt:child lropt:attr-axis)

(define lropt:descendant
  (lropt:axis-helper
   #f  ; can-sustain-order?
   (lambda (ddo-auto? zero-or-one? single-level?)
     (and ddo-auto? single-level?))
   (lambda (zero-or-one? single-level?) #f)
   (lambda (zero-or-one? single-level?) #f)))

(define lropt:descendant-or-self lropt:descendant)

(define lropt:following
  (lropt:axis-helper
   #f  ; can-sustain-order?
   (lambda (ddo-auto? zero-or-one? single-level?) zero-or-one?)
   (lambda (zero-or-one? single-level?) #f)
   (lambda (zero-or-one? single-level?) #f)))

(define lropt:following-sibling
  (lropt:axis-helper
   #f  ; can-sustain-order?
   (lambda (ddo-auto? zero-or-one? single-level?) zero-or-one?)
   (lambda (zero-or-one? single-level?) #f)
   (lambda (zero-or-one? single-level?) single-level?)))

(define lropt:parent
  (lropt:axis-helper
   #f  ; can-sustain-order?
   (lambda (ddo-auto? zero-or-one? single-level?) #f)
   (lambda (zero-or-one? single-level?) zero-or-one?)
   (lambda (zero-or-one? single-level?) single-level?)))

(define lropt:preceding
  (lropt:axis-helper
   #f  ; can-sustain-order?
   ; ATTENTION: preceding axis can yield ddo-auto == #t if
   ;  a. zero-or-one? and
   ;  b. The implementation of preceding axis returns its result in
   ; document order (corresponds to XQuery spec.)
   (lambda (ddo-auto? zero-or-one? single-level?) #f)
   (lambda (zero-or-one? single-level?) #f)
   (lambda (zero-or-one? single-level?) #f)))

(define lropt:preceding-sibling
  (lropt:axis-helper
   #f  ; can-sustain-order?
   (lambda (ddo-auto? zero-or-one? single-level?) #f)
   (lambda (zero-or-one? single-level?) #f)
   (lambda (zero-or-one? single-level?) single-level?)))

(define lropt:self
  (lropt:axis-helper
   #t  ; can-sustain-order?
   (lambda (ddo-auto? zero-or-one? single-level?) ddo-auto?)
   (lambda (zero-or-one? single-level?) zero-or-one?)
   (lambda (zero-or-one? single-level?) single-level?)))

;-------------------
; 2.10 FLWOR Operations

; Remove variable entries from alist
;  vars ::= (listof var-name)
;  alist ::= (listof (cons var-name whatever))
; var-name is not an atomic object. Namely,
;  var-name ::= (list namespace-uri local-name)) 
(define (lropt:remove-vars-from-alist vars alist)
  (filter
   (lambda (key-value)
     (not (member (car key-value) vars)))
   alist))

; Let-expression
(define (lropt:let expr called-once? order-required?
                   var-types prolog processed-funcs)
  (let* ((fun-def (cadr (xlr:op-args expr)))
         (arg  ; fun-def argument
          (caar (xlr:op-args fun-def)))
         (var-name
          (car (xlr:op-args  ; removing embracing 'var
                (cadr arg)  ; '(var ..)
                ))))
    (call-with-values
     (lambda ()
       (lropt:expr
        (cadr (xlr:op-args fun-def))  ; function body
        called-once?
        order-required?
        (cons (list var-name
                    (car arg)  ; argument type
                    )
              var-types)
        prolog processed-funcs))
     (lambda (new-body body-ddo-auto? body-0-or-1? body-level?
                       processed-funcs body-order-for-vars)
       (call-with-values
        (lambda ()
          (lropt:expr
           (car (xlr:op-args expr))  ; child expr inside let@
           called-once?
           (and  ; ordering could not be fulfilled in fun-def body
            order-required?
            (not body-ddo-auto?))
           var-types
           prolog processed-funcs))
        (lambda (new-child child-ddo-auto? child-0-or-1? child-level?
                           processed-funcs child-order-for-vars)
          (call-with-values
           (lambda ()
             (if
              (and  ; ordering could not be fulfilled in fun-def body
               order-required?
               (not body-ddo-auto?))
              ; Re-processing fun-def body once again to fulfil ordering
              (lropt:expr
               (cadr (xlr:op-args fun-def))  ; function body
               called-once?
               order-required?
               (cons (list var-name
                           (car arg)  ; argument type
                           child-ddo-auto?  ; child expr was ordered
                           child-0-or-1?
                           child-level?)
                     var-types)
               prolog processed-funcs)
              ; Identity
              (values new-body body-ddo-auto? body-0-or-1? body-level?
                      processed-funcs body-order-for-vars)))
            (lambda (new-body body-ddo-auto? body-0-or-1? body-level?
                              processed-funcs body-order-for-vars)
              (values
               (list (xlr:op-name expr)  ; == 'let@
                     new-child
                     (list (xlr:op-name fun-def)  ; == 'fun-def
                           (car (xlr:op-args fun-def))
                           new-body))
               body-ddo-auto? body-0-or-1? body-level?
               processed-funcs
               (lropt:unite-order-for-variables
                (lropt:remove-vars-from-alist (list var-name)
                                              body-order-for-vars)
                child-order-for-vars))))))))))

; Return
(define (lropt:return expr called-once? order-required?
                      var-types prolog processed-funcs)
  (let* ((fun-def (cadr (xlr:op-args expr)))
         (args  ; fun-def arguments
          (car (xlr:op-args fun-def)))
         (var-names
          (map
           (lambda (pair)
             (car (xlr:op-args  ; removing embracing 'var
                   (cadr pair)  ; '(var ..)
                   )))
           args)))
    (call-with-values
     (lambda ()
       (lropt:expr
        (cadr (xlr:op-args fun-def))  ; function body
        #f  ; called more than once
        order-required?
        (append
         (map
          (lambda (name pair)
            (list name
                  `(one  ; each variable is bound with exactly 1 item
                    ,(car pair)  ; argument type
                    )))
          var-names
          args)
         var-types)
        prolog processed-funcs))
     (lambda (new-body body-ddo-auto? body-0-or-1? body-level?
                       processed-funcs body-order-for-vars)
       (call-with-values
        (lambda ()
          (lropt:expr
           (car (xlr:op-args expr))  ; child expr inside return
           called-once?
           order-required?
           var-types
           prolog processed-funcs))
        (lambda (new-child child-ddo-auto? child-0-or-1? child-level?
                           processed-funcs child-order-for-vars)
          (values
           (list
            (if
             (and (not called-once?)
                  (null? (mlr:find-free-vars (car (xlr:op-args expr)))))
             'lreturn 'return)
            new-child
            (list (xlr:op-name fun-def)  ; == 'fun-def
                  args
                  new-body))
           (and body-ddo-auto? child-ddo-auto?)
           (and body-0-or-1? child-0-or-1?)
           (and body-level? child-level?)
           processed-funcs
           (lropt:unite-order-for-variables
            (lropt:remove-vars-from-alist var-names
                                          body-order-for-vars)
            child-order-for-vars))))))))

; Predicate
; Implemented by analogue with `lropt:return'
(define (lropt:predicate expr called-once? order-required?
                         var-types prolog processed-funcs)
  (let* ((fun-def (cadr (xlr:op-args expr)))
         (args  ; fun-def arguments
          (car (xlr:op-args fun-def)))
         (var-names
          (map
           (lambda (pair)
             (car (xlr:op-args  ; removing embracing 'var
                   (cadr pair)  ; '(var ..)
                   )))
           args)))
    (call-with-values
     (lambda ()
       (lropt:expr
        (cadr (xlr:op-args fun-def))  ; function body
        #f  ; called more than once
        #f  ; order not required for fun-def body, see also: [*]
        (append
         (map
          (lambda (name pair)
            (list name
                  `(one  ; each variable is bound with exactly 1 item
                    ,(car pair)  ; argument type
                    )))
          var-names
          args)
         var-types)
        prolog processed-funcs))
     (lambda (new-body body-ddo-auto? body-0-or-1? body-level?
                       processed-funcs body-order-for-vars)
       (call-with-values
        (lambda ()
          (lropt:expr
           (car (xlr:op-args expr))  ; child expr inside return
           called-once?
           order-required?
           var-types
           prolog processed-funcs))
        (lambda (new-child child-ddo-auto? child-0-or-1? child-level?
                           processed-funcs child-order-for-vars)
          (values
           (list
            (xlr:op-name expr)  ; == 'predicate
            new-child
            (list (xlr:op-name fun-def)  ; == 'fun-def
                  args
                  new-body))
           child-ddo-auto? child-0-or-1? child-level?
           processed-funcs
           (lropt:unite-order-for-variables
            (lropt:remove-vars-from-alist var-names
                                          body-order-for-vars)
            child-order-for-vars))))))))

; Processes fun-def
; Additional arguments: arg-cardinality-one? additional-arg-info
;  arg-cardinality-one? - whether the cardinality of the anonymous function
; argument is (zero or) one.
;  additional-arg-info ::=
;     '() |
;     (list arg-ddo-auto? arg-zero-or-one? arg-single-level?)
; Additional return value: (listof argument-name)
(define (lropt:fun-def expr called-once? order-required?
                       var-types prolog processed-funcs
                       arg-cardinality-one? additional-arg-info)
  (let* ((args  ; fun-def arguments
          (car (xlr:op-args expr)))
         (var-names
          (map
           (lambda (pair)
             (car (xlr:op-args  ; removing embracing 'var
                   (cadr pair)  ; '(var ..)
                   )))
           args)))
    (call-with-values
     (lambda ()
       (lropt:expr
        (cadr (xlr:op-args expr))  ; function body
        called-once? order-required?
        (append         
         (map
          (if
           arg-cardinality-one?
           (lambda (name pair)
             (cons name
                   (cons
                    `(one  ; each variable is bound with exactly 1 item
                      ,(car pair)  ; argument type
                      )
                    additional-arg-info)))
           (lambda (name pair)
             (cons name
                   (cons (car pair) additional-arg-info))))
          var-names
          args)
         var-types)
        prolog processed-funcs))
     (lambda (new-body body-ddo-auto? body-0-or-1? body-level?
                       processed-funcs body-order-for-vars)
       (values
        (list (xlr:op-name expr)  ; == 'fun-def
              args
              new-body)
        body-ddo-auto? body-0-or-1? body-level?
        processed-funcs body-order-for-vars
        var-names)))))

(define (lropt:order-by expr called-once? order-required?
                        var-types prolog processed-funcs)
  (call-with-values
   (lambda ()
     (lropt:fun-def (cadr (xlr:op-args expr))  ; fun-def
                    called-once?
                    #f  ; order not required
                    var-types prolog processed-funcs
                    #t  ; value for arg-cardinality-one?
                    '()  ; additional-arg-info
                    ))
   (lambda (new-fun-def def-ddo-auto? def-0-or-1? def-level?
                        processed-funcs def-order-for-vars var-names)
     (call-with-values
      (lambda ()
        (lropt:expr
         (car (xlr:op-args expr))  ; child expr inside return
         called-once?
         #f  ; order not required - child is reordered anyway
         var-types
         prolog processed-funcs))
      (lambda (new-child child-ddo-auto? child-0-or-1? child-level?
                         processed-funcs child-order-for-vars)
        (values
         (list (xlr:op-name expr)  ; == 'order-by
               new-child new-fun-def)
         #f  ; in general, ddo is not achieved
         child-0-or-1? child-level?
         processed-funcs
         (lropt:unite-order-for-variables
          (lropt:remove-vars-from-alist var-names def-order-for-vars)
          child-order-for-vars)))))))

(define (lropt:orderspecs expr called-once? order-required?
                          var-types prolog processed-funcs)
  (let loop ((src (xlr:op-args expr))
             (res '())
             (processed-funcs processed-funcs)
             (order-for-vars '()))
    (cond
      ((null? src)
       (values
        (cons (xlr:op-name expr)  ; == 'orderspecs
              (reverse res))
        #f #f #f  ; dummy values for ddo-auto? zero-or-one? single-level?
        processed-funcs order-for-vars))
      ((and (pair? (car src))
            (eq? (xlr:op-name (car src)) 'orderspec))
       (call-with-values
        (lambda ()
          (lropt:expr
           (cadr (xlr:op-args (car src)))  ; expr in ordermodifier
           called-once?
           #f  ; order not required
           var-types
           prolog processed-funcs))
        (lambda (new-child dummy-ddo-auto? dummy-0-or-1? dummy-level?
                           processed-funcs child-order-for-vars)
          (loop (cdr src)
                (cons
                 (list (xlr:op-name (car src))  ; == 'orderspec
                       (car (xlr:op-args (car src)))  ; ordermodifier
                       new-child)
                 res)
                processed-funcs
                (lropt:unite-order-for-variables
                 child-order-for-vars order-for-vars)))))
      (else
       (loop (cdr src)
             (cons (car src) res)
             processed-funcs order-for-vars)))))

;-------------------
; 2.11 Expressions on Sequence Types

; The typeswitch operator has the following logical representation:
; (ts
;  (if@
;   (lt@ (const (type !xs!integer) "1") (const (type !xs!integer) "2"))
;   (const (type !xs!integer) "3")
;   (const (type !xs!double) "4.5E4"))
;  (cases
;   (case (type (one !xs!string))
;    (fun-def
;     ((!xs!anytype (var ("" "i"))))
;     (element
;      (const (type !xs!QName) ("" "wrap"))
;      (const (type !xs!string) "test failed"))))      
;   (default
;    (fun-def
;     ((!xs!anytype (var ("" "%v"))))
;     (element
;      (const (type !xs!QName) ("" "wrap"))
;      (const (type !xs!string) "test failed"))))))
(define (lropt:ts expr called-once? order-required?
                  var-types prolog processed-funcs)
  (letrec
      ((probe-subexpr
        (lambda (demand-order?)
          (call-with-values
           (lambda ()
             (lropt:expr
              (car (xlr:op-args expr))  ; subexpr inside typeswitch
              called-once?
              demand-order?
              var-types
              prolog processed-funcs))
           (lambda (new-child child-ddo-auto? child-0-or-1? child-level?
                              processed-funcs child-order-for-vars)
             (let ((cases-node  ; bound to '(cases ...)
                    (cadr (xlr:op-args expr))))
               (let loop ((src (xlr:op-args cases-node))
                          (res '())
                          (ddo-auto? #t)
                          (zero-or-one? #t)
                          (single-level? #t)
                          (processed-funcs processed-funcs)
                          (order-for-vars child-order-for-vars))
                 (cond
                   ((null? src)
                    (if
                     (and order-required?  ; order was required
                          ; we didn't demand order from subexpr
                          (not demand-order?)
                          ; and ordering cannot be achieved for
                          ; rewritten expr
                          (not ddo-auto?))
                     ; Demand order from subexpr now
                     (probe-subexpr #t)
                     (values
                      (list (xlr:op-name expr)  ; == 'ts
                            new-child
                            (cons (xlr:op-name cases-node)  ; == 'cases
                                  (reverse res)))
                      ddo-auto? zero-or-one? single-level?
                      processed-funcs order-for-vars)))
                   ((and (pair? (car src))
                         (eq? (xlr:op-name (car src)) 'case))
                    (let ((op-args (xlr:op-args (car src))))
                      (let ((type (car op-args))  ; bound to '(type ...)
                            (fun-def (cadr op-args)))
                        (call-with-values
                         (lambda ()
                           (lropt:fun-def
                            fun-def
                            called-once? order-required?
                            var-types prolog processed-funcs
                            (lropt:var-type-zero-or-one?  ; Cardinality-one?
                             (car (xlr:op-args type)))
                            (list  ; Additional-arg-info
                             child-ddo-auto? child-0-or-1? child-level?)))
                         (lambda (new-fun-def
                                  def-ddo-auto? def-0-or-1? def-level?
                                  processed-funcs
                                  def-order-for-vars var-names)
                           (loop
                            (cdr src)
                            (cons
                             (list (xlr:op-name (car src))  ; == 'case
                                   type
                                   new-fun-def)
                             res)
                            ; ddo-auto for every branch?
                            (and ddo-auto? def-ddo-auto?)
                            (and zero-or-one? def-0-or-1?)
                            (and single-level? def-level?)
                            processed-funcs
                            (lropt:unite-order-for-variables
                             (lropt:remove-vars-from-alist
                              var-names
                              def-order-for-vars)
                             order-for-vars)))))))
                   ((and (pair? (car src))
                         (eq? (xlr:op-name (car src)) 'default))
                    ; Much borrowed from 'case branch, think of
                    ; uniting them
                    (call-with-values
                     (lambda ()
                       (lropt:fun-def
                        (car (xlr:op-args (car src)))
                        called-once? order-required?
                        var-types prolog processed-funcs
                        #f  ; cardinality-one?
                        (list  ; Additional-arg-info
                         child-ddo-auto? child-0-or-1? child-level?)))
                     (lambda (new-fun-def
                              def-ddo-auto? def-0-or-1? def-level?
                              processed-funcs
                              def-order-for-vars var-names)
                       (loop
                        (cdr src)
                        (cons
                         (list (xlr:op-name (car src))  ; == 'default
                               new-fun-def)
                         res)
                        ; ddo-auto for every branch?
                        (and ddo-auto? def-ddo-auto?)
                        (and zero-or-one? def-0-or-1?)
                        (and single-level? def-level?)
                        processed-funcs
                        (lropt:unite-order-for-variables
                         (lropt:remove-vars-from-alist
                          var-names
                          def-order-for-vars)
                         order-for-vars)))))
                   (else
                    ; This should not happen!
                    ; Recover the situation:
                    (loop (cdr src)
                          (cons (car src) res)
                          #t #t #t
                          processed-funcs order-for-vars))))))))))
    (probe-subexpr #f)))

; Processing for instance-of and castable
(define (lropt:instance-of+castable expr called-once? order-required?
                                    var-types prolog processed-funcs)
  (call-with-values
   (lambda () (lropt:expr (car (xlr:op-args expr))
                          called-once?
                          #f  ; [*]
                          var-types prolog processed-funcs))
   (lambda (child ddo-auto? zero-or-one? single-level?
                  processed-funcs order-for-variables)
     (values
      (list (xlr:op-name expr)
            child
            (cadr (xlr:op-args expr))  ; type specifier
            )
      #t #t #t  ; result is a boolean value
      processed-funcs order-for-variables))))
  
; cast and treat operations
(define (lropt:cast+treat expr called-once? order-required?
                          var-types prolog processed-funcs)
  (let ((type-0-or-1?
         (lropt:var-type-zero-or-one?
          (car (xlr:op-args  ; extracting type
                 (cadr (xlr:op-args expr))  ; selects '(type ...)
                 )))))
    (call-with-values
     (lambda () (lropt:expr (car (xlr:op-args expr))
                            called-once?
                            (and (not type-0-or-1?)  ; see [*]
                                 order-required?)
                            var-types prolog processed-funcs))
     (lambda (child ddo-auto? zero-or-one? single-level?
                    processed-funcs order-for-variables)
       (values
        (list (xlr:op-name expr)
              child
              (cadr (xlr:op-args expr))  ; type specifier
              )
        (or ddo-auto? type-0-or-1?)
        (or zero-or-one? type-0-or-1?)
        (or single-level? type-0-or-1?)        
        processed-funcs order-for-variables)))))

;-------------------
; 2.14 Distinct document order

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
                 ; Never a single-level after applying descendant axis
                 #f  ; DL: was: single-level?
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

;-------------------
; 3.6. Quantified expressions
     
; Some and every
; Implemented by analogue with `lropt:return'
(define (lropt:some-every expr called-once? order-required?
                          var-types prolog processed-funcs)
  (let* ((fun-def (cadr (xlr:op-args expr)))
         (args  ; fun-def arguments
          (car (xlr:op-args fun-def)))
         (var-names
          (map
           (lambda (pair)
             (car (xlr:op-args  ; removing embracing 'var
                   (cadr pair)  ; '(var ..)
                   )))
           args)))
    (call-with-values
     (lambda ()
       (lropt:expr
        (cadr (xlr:op-args fun-def))  ; function body
        #f  ; called more than once
        #f  ; no order for result required, see also: [*]
        (append
         (map
          (lambda (name pair)
            (list name
                  `(one  ; each variable is bound with exactly 1 item
                    ,(car pair)  ; argument type
                    )))
          var-names
          args)
         var-types)
        prolog processed-funcs))
     (lambda (new-body body-ddo-auto? body-0-or-1? body-level?
                       processed-funcs body-order-for-vars)
       (call-with-values
        (lambda ()
          (lropt:expr
           (car (xlr:op-args expr))  ; child expr inside return
           called-once?
           #f  ; order not required
           var-types
           prolog processed-funcs))
        (lambda (new-child child-ddo-auto? child-0-or-1? child-level?
                           processed-funcs child-order-for-vars)
          (values
           (list
            (if
             (and (not called-once?)
                  (null? (mlr:find-free-vars (car (xlr:op-args expr)))))
             (cond
               ((assq (xlr:op-name expr)
                      '((some . lsome) (every . levery)))
                => cdr)
               (else
                (xlr:signal-error
                 "lropt:some-every: internal error: " expr)))
             (xlr:op-name expr))
            new-child
            (list (xlr:op-name fun-def)  ; == 'fun-def
                  args
                  new-body))
           #t  ; ddo-auto? == #t, since atomic value
           #t #t  ; zero-or-one? and single-level?
           processed-funcs
           (lropt:unite-order-for-variables
            (lropt:remove-vars-from-alist var-names
                                          body-order-for-vars)
            child-order-for-vars))))))))

;-------------------
; Function call
; The structure of `processed-funcs' repeated here for convenience:
;  processed-funcs - alist of user-declared functions with rewritten bodies
; processed-funcs ::= (listof (list  func-name
;                                    order-required-for-result?
;                                    (listof  order-required-for-argument?)
;                                    rewritten-declare-function-clause
;                             ))
; order-required-for-result? - may be #f for one function call and may become
;  #t for another function call. In such a case, the alist entry for the
;  given function is replaced

; Accessors to a member of `processed-funcs'
;  processed-func ::= (list  func-name
;                            order-required-for-result?
;                            (listof  order-required-for-argument?)
;                            rewritten-declare-function-clause
;                            ddo-auto? zero-or-one? single-level?)
(define lropt:procced-func-name        car)
(define lropt:procced-func-for-result  cadr)
(define lropt:procced-func-for-args    caddr)
(define lropt:procced-func-declaration cadddr)
(define (lropt:procced-func-ddo-auto? entry)
  (list-ref entry 4))
(define (lropt:procced-func-0-or-1? entry)
  (list-ref entry 5))
(define (lropt:procced-func-single-level? entry)
  (list-ref entry 6))

; Constructor for `processed-func'
; Facilitates further code support
(define (lropt:make-procced-func name order-for-result? order-for-args
                                 declaration
                                 ddo-auto? zero-or-one? single-level?)
  (list name order-for-result? order-for-args declaration
        ddo-auto? zero-or-one? single-level?))

; ATTENTION: Since a function is declared in the global scope, only global
; variables are to be passed to `var-types'. At the momemt (06.10.06)
; variable declarations are not supported in XQuery prolog in Sedna;
; `var-types' should thus be null.
; In the result returned, `order-for-variables' contains information about
; function arguments.
(define (lropt:function-declaration expr called-once? order-required?
                                    var-types prolog processed-funcs)
  (let ((args
         ; Argument specification, e.g.
         ; '(((one (node-test))          (var ("" "n")))
         ;   ((zero-or-more (node-test)) (var ("" "seq"))))
         (cadr (xlr:op-args expr)))
        (res-type
         ; Result type, e.g. '(result-type (zero-or-more (node-test)))
         (caddr (xlr:op-args expr)))
        (body
         ; Function body: '(body ...)
         (cadddr (xlr:op-args expr))))
    (call-with-values
     (lambda ()
       (lropt:expr
        (cadr body)  ; body expression
        ; DL: Legacy value for `called-once?' is #t, although I am not
        ; sure this is always appropriate
        #t  ; called-once?
        order-required?
        (append
         (map 
          (lambda (pair)
            (list 
             (car (xlr:op-args  ; removing embracing 'var
                   (cadr pair)  ; '(var ..)
                   ))
             (car pair)  ; argument type
             ))
          args)
         var-types)
        prolog processed-funcs))
     (lambda (new-body-expr body-ddo-auto? body-0-or-1? body-level?
                            processed-funcs body-order-for-vars)
       (let ((res-0-or-1?  ; whether result type specifies zero-or-one item
              (lropt:var-type-zero-or-one?
               (car (xlr:op-args res-type))  ; remove embracing 'result-type
               )))
         (values
          (list (xlr:op-name expr)  ; == 'declare-function
                (car (xlr:op-args expr))  ; function name
                args
                res-type
                (list (xlr:op-name body)  ; == 'body
                      new-body-expr))
          (or res-0-or-1? body-ddo-auto?)
          (or res-0-or-1? body-0-or-1?)
          (or res-0-or-1? body-level?)
          processed-funcs
          body-order-for-vars))))))

; Getting `processed-func' by function name, adding it to `processed-funcs'
; if it is not there yet
;  processed-func ::= (list  func-name
;                            order-required-for-result?
;                            (listof  order-required-for-argument?)
;                            rewritten-declare-function-clause
;                            ddo-auto? zero-or-one? single-level?)
; Returns: (values processed-func new-processed-funcs)
(define (lropt:get+add-processed-func func-name called-once? order-required?
                                      var-types prolog processed-funcs)
  (cond
    ((let ((entry
            (assoc func-name processed-funcs)))
       (and entry  ; rewritten function found
            (or
             ; Function body processed for order-required? == #t
             (lropt:procced-func-for-result entry)
             ; (lropt:procced-func-for-result entry) == order-required? == #f
             (not order-required?))
            entry  ; pass it
            ))
     => (lambda (entry)
          (values entry processed-funcs)))
    ; Function with `func-name' not found among rewritten functions
    ((assoc func-name
            (map
             (lambda (part)
               (cons (car (xlr:op-args part))  ; function name
                     part))
             (filter
              (lambda (part)
                (and (pair? part)
                     (eq? (xlr:op-name part) 'declare-function)))
              prolog)))
     => (lambda (pair)  ; declaration found
          (let* ((declaration (cdr pair))
                 (args  ; argument specification
                  (cadr (xlr:op-args declaration))))
            (call-with-values
             (lambda ()
               (lropt:function-declaration
                declaration
                called-once? order-required?
                '()  ; var-types
                ; ATTENTION: when global variable declarations are added
                ; to query prolog in Sedna, their variable types are to be
                ; passed to `var-types'
                prolog
                (cons  ; terminate loop for recursive XQuery functions
                 (let ((res-0-or-1?  ; result type specifies zero-or-one item?
                        (lropt:var-type-zero-or-one?
                         (car (xlr:op-args  ; remove embracing 'result-type
                               ; Result type
                               (caddr (xlr:op-args declaration)))))))
                   ; Can use type information here only
                   (lropt:make-procced-func
                    func-name
                    #t  ; order-required-for-result?
                    ; construct (listof  order-required-for-argument?)
                    (map
                     (lambda (arg)
                       (not
                        (lropt:var-type-zero-or-one?
                         (car arg)  ; argument type
                         )))
                     args)
                    declaration
                    res-0-or-1?  ; ddo-auto?
                    res-0-or-1?  ; zero-or-one?
                    res-0-or-1?  ; single-level?
                    ))
                 processed-funcs)))
             (lambda (declaration res-ddo-auto? res-0-or-1? res-level?
                                  processed-funcs order-for-vars)
               (let ((entry
                      (lropt:make-procced-func
                       func-name
                       order-required?
                       (map
                        (lambda (arg)
                          (cond
                            ((assoc
                              (car (xlr:op-args  ; remove embracing 'var
                                    (cadr arg)))
                              order-for-vars)
                             => cdr)
                            (else
                             ; Argument not used inside function body
                             #f)))
                        args)
                       declaration
                       res-ddo-auto?
                       res-0-or-1?
                       res-level?)))
                 (values entry (cons entry processed-funcs))))))))
    ; TODO: search among 'declare-external function
    (else  ; function not declared in the prolog
     ; This should not happen!!
     ;(display "Function name not found: ")
     ;(write func-name)
     ;(newline)
     (values #f processed-funcs))))

(define (lropt:fun-call expr called-once? order-required?
                        var-types prolog processed-funcs)
  (call-with-values
   (lambda ()
     (lropt:get+add-processed-func (car (xlr:op-args expr))  ; func-name
                                   called-once? order-required?
                                   var-types prolog processed-funcs))
   (lambda (entry processed-funcs)
     (if
      entry
      (lropt:propagate expr
                       called-once?
                       (cons #f  ; for function name
                             (lropt:procced-func-for-args entry))
                       var-types prolog processed-funcs
                       (lropt:procced-func-ddo-auto? entry)
                       (lropt:procced-func-0-or-1? entry)
                       (lropt:procced-func-single-level? entry))
      ; declare-function for this function not found in prolog
      ; Recover
      (lropt:propagate expr called-once?
                       #t  ; order-required for subexprs
                       var-types prolog processed-funcs
                       #f #f #f)))))

; Rewrites query prolog by replacing function-declaration with their
; processed analogues.
; Returns rewritten prolog
; The function should be called after a query body is processed
(define (lropt:rewrite-prolog prolog processed-funcs)
  (let loop ((prolog prolog)
             (processed-funcs processed-funcs)
             (res '()))
    (cond
      ((null? prolog)  ; finish processing
       (reverse res))
      ((and (pair? (car prolog))
            (eq? (xlr:op-name (car prolog)) 'declare-function))
       (call-with-values
        (lambda ()
          (lropt:get+add-processed-func
           (car (xlr:op-args (car prolog)))  ; func-name
           #f  ; called-once? - we do not care
           #f  ; order-required? - bind to 0 to choose any existing entry
           '()  ; var-types
           prolog processed-funcs))
        (lambda (entry processed-funcs)
          (loop (cdr prolog)
                processed-funcs                
                (cons
                 (if entry
                     (lropt:procced-func-declaration entry)
                     ; on internal error, keep function-declaration as is
                     (car prolog))
                 res)))))
      (else  ; any other prolog entry
       (loop (cdr prolog)
             processed-funcs
             (cons (car prolog) res))))))


;=========================================================================
; Update, manage and retrieve-metadata queries

;------------------------------------------------
; Update

; Update replace operation
(define (lropt:replace expr called-once? order-required?
                       var-types prolog processed-funcs)
  (let* ((fun-def (cadr (xlr:op-args expr)))
         (args  ; fun-def arguments
          (car (xlr:op-args fun-def)))
         (var-names
          (map
           (lambda (pair)
             (car (xlr:op-args  ; removing embracing 'var
                   (cadr pair)  ; '(var ..)
                   )))
           args)))
    (call-with-values
     (lambda ()
       (lropt:expr
        (cadr (xlr:op-args fun-def))  ; function body
        #f  ; called more than once
        order-required?
        (append
         (map
          (lambda (name pair)
            (list name
                  `(one  ; each variable is bound with exactly 1 item
                    ,(car pair)  ; argument type
                    )))
          var-names
          args)
         var-types)
        prolog processed-funcs))
     (lambda (new-body body-ddo-auto? body-0-or-1? body-level?
                       processed-funcs body-order-for-vars)
       (call-with-values
        (lambda ()
          (lropt:expr
           (car (xlr:op-args expr))  ; child expr inside return
           called-once?
           order-required?
           var-types
           prolog processed-funcs))
        (lambda (target child-ddo-auto? child-0-or-1? child-level?
                        processed-funcs child-order-for-vars)
          (values
           (list
            (xlr:op-name expr)  ; == 'replace
            (list 'return
                  target
                  (list (xlr:op-name fun-def)  ; == 'fun-def
                        args
                        `(sequence
                           ,(cadr  ; yields '(var ...)
                             (car  ; fun-def has the only argument
                              args))
                           ,new-body
                           (const (type !se!separator) 1)))))
           ; we do not care about ddo-auto, zero-or-one and single-level in
           ; the caller
           #f #f #f
           processed-funcs
           (lropt:unite-order-for-variables
            (lropt:remove-vars-from-alist var-names
                                          body-order-for-vars)
            child-order-for-vars))))))))
         
; Update operation
(define (lropt:update expr called-once? order-required?
                      var-types prolog processed-funcs)
  (if
   (and (pair? expr) (eq? (car expr) 'replace))
   (lropt:replace expr called-once? order-required?
                  var-types prolog processed-funcs)
   (lropt:propagate expr called-once? order-required?
                    var-types prolog processed-funcs
                    #f #f #f)))

;------------------------------------------------
; Manage

; Create index operation
(define (lropt:create-index expr called-once? order-required?
                            var-types prolog processed-funcs)
  (call-with-values
   (lambda ()
     (lropt:expr (cadr (xlr:op-args expr))  ; second argument
                 called-once? order-required?
                 var-types prolog processed-funcs))
   (lambda (second second-ddo-auto? second-0-or-1? second-level?
                   processed-funcs second-order-for-vars)
     (call-with-values
      (lambda ()
        (lropt:expr (caddr (xlr:op-args expr))  ; third argument
                    called-once? order-required?
                    var-types prolog processed-funcs))
      (lambda (third third-ddo-auto? third-0-or-1? third-level?
                     processed-funcs third-order-for-vars)
        (values
         (list
          (xlr:op-name expr)  ; == 'create-index
          (car (xlr:op-args expr))
          second
          third
          (cadddr (xlr:op-args expr)))
         ; we do not care about ddo-auto, zero-or-one and single-level in
         ; the caller
         #f #f #f
         processed-funcs
         (lropt:unite-order-for-variables
          second-order-for-vars third-order-for-vars)))))))
  
; Manage operation
(define (lropt:manage expr called-once? order-required?
                      var-types prolog processed-funcs)
  (if
   (and (pair? expr) (eq? (car expr) 'create-index))
   (lropt:create-index expr called-once? order-required?
                       var-types prolog processed-funcs)
   (lropt:propagate expr called-once? order-required?
                    var-types prolog processed-funcs
                    #f #f #f)))


;=========================================================================
; High-level function

; Rewrites the query:
;  1. Creates LReturns where appropriate
;  2. Rewrites XPath axes that involve descendant-or-self
;  3. Eliminates unnecessary 'ddo operations
(define (lropt:rewrite-query query)
  (if
   (not (pair? query))
   query  ; nothing to do, although it's strange   
   (let ((prolog (xlr:get-query-prolog query)))
     (call-with-values
      (lambda ()
        (if
         (memq (xlr:op-name query) '(query retrieve-metadata))
         (let ((identity (lambda (x) x)))
           (lropt:propagate (cadr (xlr:op-args query))
                            #t  ; called once
                            #t  ; TODO: ordered/unordered depending on prolog
                            '()  ; TODO: probably, external variables here
                            prolog
                            '()  ; no functions processed yet
                            identity identity identity))
         ((case (xlr:op-name query)
            ((update) lropt:update)
            ((manage) lropt:manage)
            (else  ; this should not happen
             lropt:expr))
          (cadr (xlr:op-args query))
          #t #t '() prolog '())))
      (lambda (body dummy-auto? dummy-0-or-1? dummy-level?
                    processed-funcs dummy-order-for-variables)
        (list (xlr:op-name query)
              (cons 'prolog
                    (lropt:rewrite-prolog prolog processed-funcs))
              body))))))

; Alias for backward compatibility
(define mlr:rewrite-query lropt:rewrite-query)


;=========================================================================
; Older implementation, without DDO handling

;;; Basic logical optimization:
;;   1. lreturn
;;   2. Combining XPath axes
;; 
;; If the second step doesn't contain position-based predicates, the following
;; replacement are made:
;;  a) descendant-or-self::node()/child::?  --> descendant::?
;;  b) descendant-or-self::node()/attribute::?  --> descendant-attr::?
;;  c) descendant-or-self::node()/self::?  --> descendant-or-self::?
;;
;; The prefix for this module is "mlr:"

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
;
;
;;-------------------------------------------------
;; Finds lreturns and combines XPath axes
;;  called-once? - whether the current expression will be called just once, or
;; within the iteration
;
;; Applies mlr:lreturn+xpath to subexpression and reconstructs the expr
;(define (mlr:propagate expr called-once?)
;  (cons (xlr:op-name expr)
;        (map
;         (lambda (subexpr)
;           (mlr:lreturn+xpath subexpr called-once?))
;         (xlr:op-args expr))))
;
;(define (mlr:lreturn+xpath expr called-once?)
;  (cond
;    ((or (null? expr) (not (pair? expr)))
;     expr)
;    ((or (xlr:var? expr)
;         (memq (xlr:op-name expr) '(const type ivar)))
;     expr)
;    ((xlr:fun-def? expr)
;     `(,(car expr)  ; 'fun-def
;       ,(xlr:var-defs expr)
;       ,(mlr:lreturn+xpath (xlr:fun-body expr) called-once?)))
;    ((memq (xlr:op-name expr) '(attr-axis child self))
;     (let ((arg1 (car (xlr:op-args expr))))
;       (if
;        ; not a DDO
;        (or (null? arg1) (not (pair? arg1))
;            (not (eq? (xlr:op-name arg1) 'ddo)))
;        (mlr:propagate expr called-once?)
;        (let ((arg2 (car (xlr:op-args arg1))))
;          ; For rewriting, arg2 must be descendant-or-self::node()
;          (if
;           (or (null? arg2) (not (pair? arg2))
;               (not (eq? (xlr:op-name arg2) 'descendant-or-self))
;               (not (equal? (cadr (xlr:op-args arg2)) '(type (node-test)))))
;           (mlr:propagate expr called-once?)
;           `(,(cond
;                ((assq (xlr:op-name expr)
;                       '((child . descendant)                         
;                         (attr-axis . descendant-attr)
;                         (self . descendant-or-self)))
;                 => cdr)
;                (else
;                 (xlr:signal-error
;                  "mlr:lreturn+xpath: internal error: " expr)))
;             ,(mlr:lreturn+xpath (car (xlr:op-args arg2)) called-once?)
;             ,(cadr (xlr:op-args expr))))))))
;    ((memq (xlr:op-name expr) '(return select some every))
;     (cons
;      (if
;       (and (not called-once?)
;            (null? (mlr:find-free-vars (car (xlr:op-args expr)))))            
;       (cond
;         ((assq (xlr:op-name expr)
;                '((return . lreturn)
;                  (select . lselect)
;                  (some . lsome)
;                  (every . levery)))
;          => cdr)
;         (else
;          (xlr:signal-error
;           "mlr:lreturn+xpath: internal error: " expr)))
;       (xlr:op-name expr))
;      (map
;       (lambda (subexpr) (mlr:lreturn+xpath subexpr #f))
;       (xlr:op-args expr))))
;    ((eq? (xlr:op-name expr) 'replace)  ; update replace operation
;     (let ((target (mlr:lreturn+xpath
;                    (car (xlr:op-args expr)) called-once?))
;           (fun (mlr:lreturn+xpath
;                 (cadr (xlr:op-args expr)) #f)))
;       (if
;        (not (and (xlr:fun-def? fun)
;                  (= (length (xlr:var-defs fun)) 1)))
;        (xlr:signal-error
;         "mlr:lreturn+xpath: improper representation for replace update: " expr)
;        `(replace
;          (return
;           ,target
;           (,(car fun)  ; 'fun-def
;             ,(xlr:var-defs fun)
;             (sequence
;               ,(cadr  ; variable
;                 (car  ; fun-def has the only argument
;                  (xlr:var-defs fun)))
;               ,(xlr:fun-body fun)
;               (const (type !se!separator) 1))))))))
;    (else
;     (mlr:propagate expr called-once?))))
;
;; Rewrites a function declaration
;;  named-func ::= (list  'declare-function
;;                        name  formal-args  return-type
;;                        body-expr)
;(define (mlr:rewrite-func-declaration decl)
;  (if
;   (or (null? decl) (not (pair? decl))
;       (not (eq? (xlr:op-name decl) 'declare-function)))
;   decl  ; don't rewrite it
;   (list (xlr:op-name decl)
;         (car (xlr:op-args decl))
;         (xlr:named-func-args decl)
;         (caddr (xlr:op-args decl))  ; return type
;         (mlr:lreturn+xpath (xlr:named-func-body decl) #t))))
;
;;-------------------------------------------------
;; High-level function
;
;; Rewrites the query:
;;  1. Creates LReturns where appropriate
;;  2. Rewrites XPath axes that involve descendant-or-self
;(define (mlr:rewrite-query query)
;  (cond
;    ((or (null? query) (not (pair? query)))
;     ; nothing to do, although it's strange
;     query)
;    ((eq? (xlr:op-name query) 'query)
;     `(query
;       (prolog
;        ,@(map mlr:rewrite-func-declaration
;               (xlr:get-query-prolog query)))
;       (query-body
;        ,(mlr:lreturn+xpath (xlr:get-query-body query) #t))))
;    (else  ; update or smth
;     (mlr:lreturn+xpath query #t))))
