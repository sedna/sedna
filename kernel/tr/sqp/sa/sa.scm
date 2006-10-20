; File:  sa.scm
; Copyright (C) 2004 The Institute for System Programming
; of the Russian Academy of Sciences (ISP RAS)

;; Static Analysis for XQuery logical representation
; Primary actions:
;  - Semantic analysis of the query
;  - Resolution of qualified names
; Prefix for this module is "sa:"
(declare (unit sa) (uses common-lib scm-error-codes))

;==========================================================================
; Trivial datatypes, accessors and predicates

; Global identifiers for expression datatypes
(define sa:type-nodes 'sa:nodes)
(define sa:type-atomic 'sa:atomic)
(define sa:type-any 'sa:any)

;-------------------------------------------------
; Query logical representation

; Identifier for representing a context item
(define sa:context-item '(var ("" "$%v")))

; Whether a variable
(define sa:var? symbol?)

; Trivial accessors for future extensions
(define sa:op-name car)
(define sa:op-args cdr)

; Asserts that the given expr contains the given number of arguments
; Either returns #t or raises the exception
(define (sa:assert-num-args expr num)
  (or
   (= (length (sa:op-args expr)) num)
   (cl:signal-input-error SE5000 expr)))

;-------------------------------------------------
; Query prolog and body

(define (sa:get-query-prolog query)
  (cond
    ((assq 'prolog (cdr query))
     => cdr)
    (else
     (cl:signal-input-error SE5001 query))))

(define (sa:get-query-body query)
  (cond
    ((assq 'query-body (cdr query))
     => cadr)
    (else
     (cl:signal-input-error SE5002 query))))

;-------------------------------------------------
; Misc list processing functions

(define (filter pred lis)			
  (let rpt ((l lis))		
    (if (null? l) 
      '()
       (if (pred (car l))
	 (cons (car l) (rpt (cdr l)))
	 (rpt (cdr l))))))

; Constructs the (listof value), whose length is num
(define (sa:make-list value num)
  (if (= num 0)
      '()
      (cons value (sa:make-list value (- num 1)))))

;------------------------------------------------
; Simple string parsing

; Splits a string in accordance with char-lst
(define (sa:string-split str char-lst)
  (let loop ((src (string->list str))
             (curr '())
             (res '()))
    (cond
      ((null? src)  ; over
       (reverse (cons (list->string (reverse curr))
                      res)))
      ((memv (car src) char-lst)  ; delimiter
       (loop (cdr src)
             '()
             (cons (list->string (reverse curr)) res)))
      (else  ; ordinary char
       (loop (cdr src)
             (cons (car src) curr)
             res)))))

; Removes boundary spaces from str
(define (sa:remove-boundary-spaces str)
  (letrec
      ((remove-starting-space
        (lambda (char-lst)
          (if (or (null? char-lst)
                  (not (memv (car char-lst)
                             (map integer->char '(32 9 10 13)))))
              char-lst
              (remove-starting-space (cdr char-lst))))))
    (list->string
     (reverse
      (remove-starting-space
       (reverse
        (remove-starting-space (string->list str))))))))

; str = "indent=yes; method=xml;"
; delim - a character to delimit several suboptions, #\; in the above example
; Result: '(("indent" . "yes") ("method" . "xml"))
(define (sa:string->key-value-pairs str delim)
  (map
   (lambda (pair)
     (cons (sa:remove-boundary-spaces (car pair))
           (sa:remove-boundary-spaces (cadr pair))))
   (filter
    (lambda (lst) (= (length lst) 2))  ; name-value pairs
    (map
     (lambda (sub) (sa:string-split sub '(#\=)))
     (sa:string-split str (list delim))))))

; str = "indent=yes; method=xml;"
; key = "indent" => result = "yes"
; If key not found, returns #f
(define (sa:extract-suboption str key delim)
  (cond
   ((assoc key (sa:string->key-value-pairs str delim))
    => cdr)
   (else #f))
;  (let ((pattern (append (string->list key) '(#\=))))
;    (let loop ((src (string->list str))
;               (key pattern))
;      (cond
;        ((null? src)  ; not found
;         #f)
;        ((null? key)  ; pattern found
;         (let rpt ((src src)
;                   (res '()))
;           (if
;            (or (null? src) (char=? (car src) #\;))
;            (list->string (reverse res))
;            (rpt (cdr src)
;                 (cons (car src) res)))))
;        ((char=? (car src) (car key))
;         (loop (cdr src) (cdr key)))
;        (else
;         (loop (cdr src) pattern)))))
  )
       

;==========================================================================
; Query parsing

; Highest-level function
; Returns a new query with resolved names or
; raises an exception and returns #f in case of semantic error detected
(define (sa:analyze-query query)
  (if
   (or (not (pair? query)) (null? query))
   (cl:signal-input-error SE5003 query)
   (and
    (sa:assert-num-args query 2)
    (let ((prolog-res (sa:analyze-prolog (sa:get-query-prolog query))))
      (and
       prolog-res  ; processed correctly
       (let* ((new-prolog (car prolog-res))
              (funcs (cadr prolog-res))
              (ns-binding (caddr prolog-res))
              (default-ns (cadddr prolog-res)))
         (cond           
           ((eq? (sa:op-name query) 'query)
            (let ((pair   ; = (new-query . type)
                   (sa:analyze-expr
                    (sa:get-query-body query) '()
                    funcs ns-binding default-ns)))
              (and
               pair
               `(query ,(cons 'prolog new-prolog)
                       (query-body ,(car pair))))))
           ((assq (sa:op-name query)
                  `((update . ,sa:analyze-update)
                    (manage . ,sa:analyze-manage)
                    (retrieve-metadata . ,sa:analyze-retrieve-metadata)))
            => (lambda (pair)
                 (let ((new ((cdr pair)
                             (cadr (sa:op-args query))
                             '() funcs ns-binding default-ns)))
                   (and new
                        (list (sa:op-name query)
                              (cons 'prolog new-prolog)
                              new)))))
           (else
            (cl:signal-input-error SE5004 query)))))))))

; Analyzes an expression
;  vars ::= (listof (cons var-name var-type))
;  var-name ::= 
;   sa:type-atomic - a sequence that can containg atomic types
;   | sa:type-nodes - can contain nodes
;
;  funcs ::=(listof
;             (list fun-uri fun-name
;                   min-args max-args (lambda (num-args) ...)
;                   return-type
;                   std-fun-name-or-flag-external? ))
;  fun-uri, fun-name - strings
;  min-args - (a number) minimal number of arguments
;  max-args - (number or #f) maximal number of arguments
;  (lambda (num-args) ... ) - given num-args, produces (listof arg-type)
;  arg-type ::= var-type
;  return-type ::= var-type
;  std-fun-name-or-flag-external - an optional parameter. If omitted,
;   denotes a function declared in the prolog. If presented and is #f,
;   denotes an external function. Otherwise is a symbol that denotes the
;   name of the standard function call, e.g. '!fn!document
;
; Returns
;  either (cons new-query var-type)
;  or #f - semantic error detected - the message is displayed as a side effect
(define (sa:analyze-expr expr vars funcs ns-binding default-ns)
  (if
   (not (pair? expr))
   (cl:signal-input-error SE5005 expr)
   ;(sa:var? expr)
   ; (sa:analyze-variable expr vars funcs ns-binding default-ns)
   (case (sa:op-name expr)  ; operation name
     ; new representation for variables     
     ((var)
      (sa:variable-wrapped expr vars funcs ns-binding default-ns))
     ;-------------------
     ; 2.2 Constants
     ((const)
      (sa:analyze-const expr vars funcs ns-binding default-ns))
     ;-------------------
     ; Axes
     ((ancestor ancestor-or-self attr-axis child descendant descendant-or-self
                following following-sibling parent preceding
                preceding-sibling self)
      ; ATTENTION: namespace axis
      (sa:analyze-axis expr vars funcs ns-binding default-ns))
     ;-------------------
     ; 2.4 Sequence
     ((sequence space-sequence unio)
      (sa:analyze-sequence expr vars funcs ns-binding default-ns))
     ;-------------------
     ; 2.5 Arithmetic operations
     ((+@ -@ *@ div@ idiv@ mod@ /@ to@)
      (sa:binary-arithmetic expr vars funcs ns-binding default-ns))
     ((unary+@ unary-@)
      (sa:unary-arithmetic expr vars funcs ns-binding default-ns))
     ;-------------------
     ; 2.6 Comparison operations
     ((eq@ ne@ lt@ le@ gt@ ge@ =@ !=@ <@ <=@ >@ >=@)
      (sa:analyze-comparison expr vars funcs ns-binding default-ns))
     ((is@ <<@ >>@)
      (sa:analyze-node-comparison expr vars funcs ns-binding default-ns))
     ;-------------------
     ; 2.7 Conditional operation
     ((if if@)
      (sa:analyze-if expr vars funcs ns-binding default-ns))
     ;-------------------
     ; 2.8 Logical operations
     ;((not@)
     ; (sa:analyze-not@ expr vars funcs ns-binding default-ns))
     ((and@ or@)
      (sa:analyze-and@-or@ expr vars funcs ns-binding default-ns))
     ;-------------------
     ; 2.9 Constructors
     ((element)
      (sa:analyze-element-constructor expr vars funcs ns-binding default-ns))
     ((attribute pi namespace)
      (sa:attribute-pi-namespace expr vars funcs ns-binding default-ns))
     ((document
       text
       comment)
      (sa:document-text-comment expr vars funcs ns-binding default-ns))
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
     ((castable)
      (sa:analyze-castable expr vars funcs ns-binding default-ns))
     ;-------------------
     ; 2.14 Distinct document order
     ((ddo)
      (sa:analyze-ddo expr vars funcs ns-binding default-ns))
     ;-------------------
     ; 3.3. XPath
     ((congen1)
      (sa:analyze-congen1 expr vars funcs ns-binding default-ns))
     ((congen2)
      (sa:analyze-congen2 expr vars funcs ns-binding default-ns))
     ;-------------------
     ; 3.6. Quantified expressions
     ((some every)
      (sa:some-every expr vars funcs ns-binding default-ns))
     ;-------------------
     ; 3.7 XQuery 1.0 Functions
     ((!fn!document !fn!collection)
      (sa:document-collection expr vars funcs ns-binding default-ns))
     ((!fn!position !fn!last !fn!true !fn!false)
      (sa:position-last-true-false expr vars funcs ns-binding default-ns))
     ((!fn!node-name !fn!node-kind !fn!name !fn!not !fn!empty !fn!count
                     !fn!error !fn!document-uri)
      (sa:basic-singlearg-atomic expr vars funcs ns-binding default-ns))
     ((!fn!sum !fn!avg !fn!max !fn!min !fn!contains !fn!translate
               !fn!distinct-values !fn!concat !fn!string-value !fn!typed-value)
      (sa:basic-multiarg-atomic expr vars funcs ns-binding default-ns))
     ((!fn!replace)
      (sa:basic-replace expr vars funcs ns-binding default-ns))
     ;-------------------
     ; Union operations
     ((union@ intersect@ except@)
      (sa:analyze-union-intersect expr vars funcs ns-binding default-ns))
     ;-------------------
     ; Not expressed in the new logical representation
     ((fun-call)
      (sa:analyze-fun-call expr vars funcs ns-binding default-ns))
     ((cast)
      (sa:analyze-cast expr vars funcs ns-binding default-ns))
     ((spaceseq)
      (sa:propagate expr vars funcs ns-binding default-ns 'sa:atomic))
     ;-------------------     
     (else  ; unknown operations
      (cl:signal-input-error SE5006 expr)))))

; Propagates semantic analysis to subexpressions
;  return - what type the operation should return if arguments are ok
(define (sa:propagate expr vars funcs ns-binding default-ns return)
  (let ((lst
         (map
          (lambda (subexpr) (sa:analyze-expr subexpr vars funcs ns-binding default-ns))
          (sa:op-args expr))))
    (if (memq #f lst)  ; error detected        
        #f
        (cons (cons (sa:op-name expr)
                    (map car lst))                      
              return))))


;==========================================================================
; Analysis for XQuery prolog

(define sa:fn-ns "http://www.w3.org/2004/07/xpath-functions")
(define sa:sql-ns "http://modis.ispras.ru/Sedna/SQL")
(define sa:xs-ns "http://www.w3.org/2001/XMLSchema")
(define sa:se-ns "http://www.modis.ispras.ru/sedna")
(define sa:xdt-ns "http://www.w3.org/2004/07/xpath-datatypes")

(define sa:predefined-ns-prefixes
  `(("xml" . "http://www.w3.org/XML/1998/namespace")
    ("xs" . ,sa:xs-ns)
    ("xsi" . "http://www.w3.org/2001/XMLSchema-instance")
    ("fn" . ,sa:fn-ns)
    ("xdt" . ,sa:xdt-ns)
    ("local" . "http://www.w3.org/2005/xquery-local-functions"
     ; was: "http://www.w3.org/2004/07/xquery-local-functions"
     )
    ("se" . ,sa:se-ns)
    ))

(define sa:default-ns (list "" sa:fn-ns))

; Functions from XQuery 1.0 and XPath 2.0 Functions and Operators
;  funcs ::=(listof
;             (list fun-uri fun-name
;                   min-args max-args (lambda (num-args) ...)
;                   return-type))
;  fun-uri, fun-name - strings
;  min-args - (a number) minimal number of arguments
;  max-args - (number or #f) maximal number of arguments
;  (lambda (num-args) ... ) - given num-args, produces (listof arg-type)
;  arg-type ::= sa:type-atomic | sa:type-nodes
;  return-type ::= sa:type-atomic | sa:type-nodes
(define xquery-functions
  (append
  `(
    ;----------------------------------------
    ; Document and collection
    (,sa:fn-ns "doc" 1 2
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-nodes !fn!document)
    ; this one for backward compatiblity
    (,sa:fn-ns "document" 1 2
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-nodes !fn!document)
    (,sa:fn-ns "collection" 1 1
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-nodes !fn!collection)
    ;----------------------------------------
    ; Function without arguments
    (,sa:fn-ns "position" 0 0
     ,(lambda (num-args) '())
     ,sa:type-atomic !fn!position)
    (,sa:fn-ns "last" 0 0
     ,(lambda (num-args) '())
     ,sa:type-atomic !fn!last)
    (,sa:fn-ns "true" 0 0
     ,(lambda (num-args) '())
     ,sa:type-atomic !fn!true)
    (,sa:fn-ns "false" 0 0
     ,(lambda (num-args) '())
     ,sa:type-atomic !fn!false)
    ;----------------------------------------
    ; URI-related functions
    (,sa:fn-ns "static-base-uri" 0 0
     ,(lambda (num-args) '())
     ,sa:type-atomic !fn!static-base-uri)
    (,sa:fn-ns "encode-for-uri" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic !fn!encode-for-uri)
    (,sa:fn-ns "iri-to-uri" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic !fn!iri-to-uri)
    (,sa:fn-ns "escape-html-uri" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic !fn!escape-html-uri)
    (,sa:fn-ns "resolve-uri" 1 2
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-nodes !fn!resolve-uri)
    ;----------------------------------------
    ; 6.4 Functions on Numeric Values
    (,sa:fn-ns "abs" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic !fn!abs)
    (,sa:fn-ns "ceiling" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic !fn!ceiling)
    (,sa:fn-ns "floor" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic !fn!floor)
    (,sa:fn-ns "round" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic !fn!round)
    (,sa:fn-ns "round-half-to-even" 1 2
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-nodes !fn!round-half-to-even)
    ;----------------------------------------
    ; Single-arg atomic function
    (,sa:fn-ns "name" 0 1
     ,(lambda (num-args) (sa:make-list sa:type-nodes num-args))
     ,sa:type-atomic !fn!name)
    (,sa:fn-ns "document-uri" 1 1
     ,(lambda (num-args) (sa:make-list sa:type-nodes num-args))
     ,sa:type-atomic !fn!document-uri)
    (,sa:fn-ns "node-name" 1 1
     ,(lambda (num-args) (sa:make-list sa:type-nodes num-args))
     ,sa:type-atomic !fn!node-name)
    (,sa:fn-ns "node-kind" 1 1
     ,(lambda (num-args) (sa:make-list sa:type-nodes num-args))
     ,sa:type-atomic !fn!node-kind)
    (,sa:fn-ns "namespace-uri" 0 1
     ,(lambda (num-args) (sa:make-list sa:type-nodes num-args))
     ,sa:type-atomic !fn!namespace-uri)
    (,sa:fn-ns "number" 0 1
     ,(lambda (num-args) (sa:make-list sa:type-any num-args))
     ,sa:type-atomic !fn!number)
    (,sa:fn-ns "boolean" 1 1
     ,(lambda (num-args) `(,sa:type-any))
     ,sa:type-atomic !fn!boolean)
    (,sa:fn-ns "not" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic !fn!not)
    (,sa:fn-ns "empty" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic !fn!empty)
    (,sa:fn-ns "exists" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))     
     ,sa:type-atomic !fn!exists)
    (,sa:fn-ns "data" 1 1
     ,(lambda (num-args) `(,sa:type-any))
     ,sa:type-atomic !fn!data)
    (,sa:fn-ns "count" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic !fn!count)
    (,sa:fn-ns "error" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic !fn!error)
    ;----------------------------------------
    (,sa:fn-ns "trace" 2 2
     ,(lambda (num-args) (list sa:type-any sa:type-atomic))
     ,sa:type-any !fn!trace)
    (,sa:fn-ns "insert-before" 3 3
     ,(lambda (num-args) (list sa:type-any sa:type-atomic sa:type-any))
     ,sa:type-any !fn!insert-before)
    (,sa:fn-ns "remove" 2 2
     ,(lambda (num-args) (list sa:type-any sa:type-atomic))
     ,sa:type-any !fn!remove)
    (,sa:fn-ns "reverse" 1 1
     ,(lambda (num-args) `(,sa:type-any))
     ,sa:type-any !fn!reverse)
    (,sa:fn-ns "zero-or-one" 1 1
     ,(lambda (num-args) `(,sa:type-any))
     ,sa:type-any !fn!zero-or-one)
    (,sa:fn-ns "one-or-more" 1 1
     ,(lambda (num-args) `(,sa:type-any))
     ,sa:type-any !fn!one-or-more)
    (,sa:fn-ns "exactly-one" 1 1
     ,(lambda (num-args) `(,sa:type-any))
     ,sa:type-any !fn!exactly-one)
    ;----------------------------------------
    ; Multiarg atomic functions
    (,sa:fn-ns "sum" 0 #f
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic !fn!sum)
    (,sa:fn-ns "avg" 0 #f
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic !fn!avg)
    (,sa:fn-ns "max" 0 #f
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic !fn!max)
    (,sa:fn-ns "min" 0 #f
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic !fn!min)
    (,sa:fn-ns "concat" 2 #f
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic !fn!concat)
    ;----------------------------------------
    ; String functions
    (,sa:fn-ns "distinct-values" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic !fn!distinct-values)
    (,sa:fn-ns "string-value" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic !fn!string-value)
    (,sa:fn-ns "string-length" 0 1
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic !fn!string-length)
    (,sa:fn-ns "typed-value" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic !fn!typed-value)
    (,sa:fn-ns "string" 0 1
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic !fn!string)
    (,sa:fn-ns "contains" 2 2
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic !fn!contains)
    (,sa:fn-ns "translate" 3 3
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic !fn!translate)
    (,sa:fn-ns "deep-equal" 2 2
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic !fn!deep-equal)
    (,sa:fn-ns "replace" 3 4
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic !fn!replace)
    (,sa:fn-ns "matches" 2 3
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic !fn!matches)
    (,sa:fn-ns "subsequence" 2 3
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic !fn!subsequence)
    ;----------------------------------------
    ; XML Date/Time functions
    (,sa:fn-ns "current-dateTime" 0 0
     ,(lambda (num-args) '())
     ,sa:type-atomic !fn!current-dateTime)
    (,sa:fn-ns "current-date" 0 0
     ,(lambda (num-args) '())
     ,sa:type-atomic !fn!current-date)
    (,sa:fn-ns "current-time" 0 0
     ,(lambda (num-args) '())
     ,sa:type-atomic !fn!current-time)
    (,sa:fn-ns "implicit-timezone" 0 0
     ,(lambda (num-args) '())
     ,sa:type-atomic !fn!implicit-timezone)
   (,sa:fn-ns "dateTime" 2 2
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!dateTime)
   (,sa:fn-ns "years-from-duration" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!years-from-duration)
   (,sa:fn-ns "months-from-duration" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!months-from-duration)
   (,sa:fn-ns "days-from-duration" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!days-from-duration)
   (,sa:fn-ns "hours-from-duration" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!hours-from-duration)
   (,sa:fn-ns "minutes-from-duration" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!minutes-from-duration)
   (,sa:fn-ns "seconds-from-duration" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!seconds-from-duration)
   (,sa:fn-ns "year-from-dateTime" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!year-from-dateTime)
   (,sa:fn-ns "month-from-dateTime" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!month-from-dateTime)
   (,sa:fn-ns "day-from-dateTime" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!day-from-dateTime)
   (,sa:fn-ns "hours-from-dateTime" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!hours-from-dateTime)
   (,sa:fn-ns "minutes-from-dateTime" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!minutes-from-dateTime)
   (,sa:fn-ns "seconds-from-dateTime" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!seconds-from-dateTime)
   (,sa:fn-ns "timezone-from-dateTime" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!timezone-from-dateTime)
   (,sa:fn-ns "year-from-date" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!year-from-date)
   (,sa:fn-ns "month-from-date" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!month-from-date)
   (,sa:fn-ns "day-from-date" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!day-from-date)
   (,sa:fn-ns "timezone-from-date" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!timezone-from-date)
   (,sa:fn-ns "hours-from-time" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!hours-from-time)
   (,sa:fn-ns "minutes-from-time" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!minutes-from-time)
   (,sa:fn-ns "seconds-from-time" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!seconds-from-time)
   (,sa:fn-ns "timezone-from-time" 1 1
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!timezone-from-time)
   (,sa:fn-ns "adjust-dateTime-to-timezone" 1 2
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!adjust-dateTime-to-timezone)
   (,sa:fn-ns "adjust-date-to-timezone" 1 2
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!adjust-date-to-timezone)
   (,sa:fn-ns "adjust-time-to-timezone" 1 2
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-atomic !fn!adjust-time-to-timezone)
   (,sa:xs-ns "yearMonthDuration" 1 1
     ,(lambda (num-args) (list sa:type-any))
     ,sa:type-atomic
     (cast !xs!yearMonthDuration))
   (,sa:xs-ns "dayTimeDuration" 1 1
     ,(lambda (num-args) (list sa:type-any))
     ,sa:type-atomic
     (cast !xs!dayTimeDuration))
   ;----------------------------------------
   ; SQL extension by Roman Pastuhov
   (,sa:sql-ns "exec-update" 2 #f
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-nodes !fn!sql-exec-update)
   (,sa:sql-ns "connect" 1 4
    ,(lambda (num-args)
       (if (= num-args 4)
           (append (sa:make-list sa:type-atomic (- num-args 1))
                   (list sa:type-nodes))
           (sa:make-list sa:type-atomic num-args)))
    ,sa:type-atomic !fn!sql-connect)
   (,sa:sql-ns "prepare" 2 3
    ,(lambda (num-args)
       (if (= num-args 3)
           (append (sa:make-list sa:type-atomic (- num-args 1))
                   (list sa:type-nodes))
           (sa:make-list sa:type-atomic num-args)))
    ,sa:type-atomic !fn!sql-prepare)
   (,sa:sql-ns "execute" 2 #f
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-nodes !fn!sql-execute)
   (,sa:sql-ns "close" 1 1
    ,(lambda (num-args) `(,sa:type-atomic))
    ,sa:type-nodes !fn!sql-close)
   (,sa:sql-ns "commit" 1 1
    ,(lambda (num-args) `(,sa:type-atomic))
    ,sa:type-nodes !fn!sql-commit)
   (,sa:sql-ns "rollback" 1 1
    ,(lambda (num-args) `(,sa:type-atomic))
    ,sa:type-nodes !fn!sql-rollback)
   ;----------------------------------------
   ; Index functions
   (,sa:fn-ns "index-scan" 3 3
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-nodes !fn!index-scan)
   (,sa:fn-ns "index-scan-between" 4 4
    ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
    ,sa:type-nodes !fn!index-scan-between)
    ;----------------------------------------
    ; Full text search functions
    (,sa:fn-ns "ftindex-scan" 2 2
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-nodes !fn!ftindex-scan)
    (,sa:fn-ns "ftscan" 3 4
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-nodes !fn!ftscan)
    (,sa:fn-ns "fthighlight" 2 2
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-nodes !fn!fthighlight)
    (,sa:fn-ns "fthighlight-blocks" 2 2
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-nodes !fn!fthighlight2)
    ;----------------------------------------
    ; Encyclopedia-specific ordering functions
    (,sa:fn-ns "is_ancestor" 2 2
     ,(lambda (num-args) (sa:make-list sa:type-nodes num-args))
     ,sa:type-atomic !fn!is_ancestor)
    (,sa:fn-ns "filter_entry_level" 1 1
     ,(lambda (num-args) (sa:make-list sa:type-nodes num-args))
     ,sa:type-nodes !fn!filter_entry_level)
    ;----------------------------------------
    ; Checkpoint
    (,sa:se-ns "checkpoint" 0 0
     ,(lambda (num-args) '())
     ,sa:type-atomic !se!checkpoint)
    ;----------------------------------------
    ; Legacy
    (,sa:fn-ns "item-at" 2 2
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic !fn!item-at)
    (,sa:fn-ns "test" 0 #f
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-any !fn!test)
    (,sa:fn-ns "local-name" 0 1
     ,(lambda (num-args) (sa:make-list sa:type-nodes num-args))
     ,sa:type-atomic !fn!local-name)
    (,sa:fn-ns "ends-with" 2 3
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic)        
    ; This one is not from Functions and Operators
    (,sa:fn-ns "QName" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic)
    
    )
  (map
   (lambda (type-spec)
     (list sa:xs-ns type-spec 1 1
           (lambda (num-args) (list sa:type-any))
           sa:type-atomic
           `(cast ,(string->symbol (string-append "!xs!" type-spec)))))
   '("string"
     "boolean"
     "decimal"
     "float"
     "double"
     "duration"
     "dateTime"
     "time"
     "date"
     "gYearMonth"
     "gYear"
     "gMonthDay"
     "gDay"
     "gMonth"
     "hexBinary"
     "base64Binary"
     "anyURI"
     "QName"
     "NOTATION"
     "normalizedString"
     "token"
     "language"
     "NMTOKEN"
     "NMTOKENS"
     "Name"
     "NCName"
     "ID"
     "IDREF"
     "IDREFS"
     "ENTITY"
     "ENTITIES"
     "integer"
     "nonPositiveInteger"
     "negativeInteger"
     "long"
     "int"
     "short"
     "byte"
     "nonNegativeInteger"
     "unsignedLong"
     "unsignedInt"
     "unsignedShort"
     "unsignedByte"
     "untypedAtomic"
     "positiveInteger"))
  ))

;-------------------------------------------------
; XQuery prolog-part analysis

; Converts a QName to its human-readable string representation
(define (sa:qname->string qname-const)
  (let ((pair (sa:proper-qname qname-const)))
    (if (string=? (car pair) "")  ; no namespace URI
        (cadr pair)
        (string-append (car pair) ":" (cadr pair)))))

; Returns #f if semantic error was detected in the prolog-part
; Otherwise, returns: (list new-prolog funcs ns-binding default-ns)
;  new-prolog - modified logical representation for prolog
;  ns-binding ::= (listof (cons prefix ns-URI))
;  default-ns ::= (list default-element-ns default-function-ns)
(define (sa:analyze-prolog prolog)
  ; new-prlg contains #f members at the places of function declarationss
  ;; triples ::= (listof (fun-body formal-args return-type))
  ; triples ::= (listof (declare-function formal-args return-type))
  (let loop ((new-prlg '())
             (funcs xquery-functions)
             (triples '())
             (ns-binding sa:predefined-ns-prefixes)
             (default-elem-ns #f)
             (default-func-ns #f)
             (prolog prolog))
    (if
     (null? prolog)  ; prolog viewed
     (let ((default-ns (list (or default-elem-ns "")
                             (sa:default-func-ns->string default-func-ns))))
       (let rpt ((new-prlg new-prlg)
                 (triples triples)
                 (prolog-res '()))
         (cond
           ((null? new-prlg)  ; all function bodies checked
            (list prolog-res
                  funcs
                  ns-binding default-ns))
           (; #f is added to `new-prlg' for 'declare-function
            (car new-prlg)  ; doesn't correspond to function declaration
            (rpt (cdr new-prlg)
                 triples
                 (cons (car new-prlg) prolog-res)))
           (else  ; corresponds to function declaration from (car triples)
            (let* ((declaration (caar triples))                 
                   (body (cadr (list-ref (sa:op-args (caar triples)) 3)))
                   (vars (cadar triples))
                   (return-type (caddar triples))
                   (res (sa:analyze-expr
                         body vars funcs ns-binding default-ns)))
              (cond
                ((not res) #f)  ; error in body
                ((sa:can-be-cast? (cdr res) return-type)
                 (rpt (cdr new-prlg)
                      (cdr triples)
                      (cons
                       (let ((func-decl (caar triples)))
                         (list
                          (sa:op-name func-decl)
                          (car (sa:op-args func-decl))  ; name
                          (map  ; args
                           (lambda (pair)
                             (list
                              (car pair)  ; argument type
                              (list
                               (caadr pair)   ; = 'var
                               (sa:expand-var-name
                                (cadadr pair)  ; var-name
                                ns-binding
                                (or default-elem-ns "")))))
                           (cadr (sa:op-args func-decl)))
                          (caddr (sa:op-args func-decl))  ; return type
                          (list
                           (car (cadddr (sa:op-args func-decl)))  ; ='body
                           (car res))))
                       prolog-res)))
                (else
                 (cl:signal-user-error XPTY0004  ; was: SE5007
                                       body))))))))
     (let ((expr (car prolog)))
       (case (sa:op-name expr)
         ((boundary-space-decl)  ; Boundary space
          (and
           (sa:assert-num-args expr 1)
           (sa:analyze-string-const (cadr expr) '() '() '() sa:default-ns)
           (cond
             ((not
               (member (cadr expr)  ; predefined values
                       '((const (type !xs!string) "strip")
                         (const (type !xs!string) "preserve"))))
              (cl:signal-user-error SE5054 (cadr expr)))
             ((assq 'boundary-space-decl
                    (filter pair? new-prlg))
              ; Multiple boundary-space declarations
              => (lambda (entry)
                   (cl:signal-user-error
                    XQST0068
                    (string-append
                     (caddr  ; const value
                      (car (sa:op-args entry))  ; '(const ...)
                      )
                     " and "
                     (caddr 
                      (car (sa:op-args expr)))))))
             (else
              (loop (cons expr new-prlg)
                    funcs triples
                    ns-binding default-elem-ns default-func-ns
                    (cdr prolog))))))
         ((declare-default-order)  ; Default order for empty sequences
          ; Clone of boundary-space-decl
          (and
           (sa:assert-num-args expr 1)
           (sa:analyze-string-const (cadr expr) '() '() '() sa:default-ns)
           (if
            (member (cadr expr)  ; predefined values
                    '((const (type !xs!string) "empty-greatest")
                      (const (type !xs!string) "empty-least")))
            (loop (cons expr new-prlg)
                  funcs triples
                  ns-binding default-elem-ns default-func-ns
                  (cdr prolog))
            (cl:signal-user-error SE5054 (cadr expr)))))         
         ((declare-option)
          (let ((new-prlg
                 ; Processing moved to a separate function
                 (sa:prolog-declare-option expr new-prlg ns-binding)))
            (and new-prlg  ; no error detected
                 (loop new-prlg funcs triples
                       ns-binding default-elem-ns default-func-ns
                       (cdr prolog)))))
         ((declare-namespace)
          (and
           (sa:assert-num-args expr 2)
           (sa:analyze-string-const (caddr expr) '() '() '() sa:default-ns)
           (if
            (not (symbol? (cadr expr)))  ; prefix
            (cl:signal-input-error SE5008 (cadr expr))
            (let ((prefix (symbol->string (car (sa:op-args expr))))
                  (ns-uri (cadr (sa:op-args
                                 (cadr (sa:op-args expr))))))
              (cond
                ((or (member prefix '("xml" "xmlns"))
                     (member ns-uri
                             '("http://www.w3.org/XML/1998/namespace")))
                 => (lambda (reason)
                      (cl:signal-user-error XQST0070 (car reason))))
                ((and
                  (assoc prefix ns-binding)  ; prefix redeclaration
                  (not
                   (member prefix   ; can redefine
                           '("xs" "xsi" "fn" "xdt" "local" "se"))))
                 (cl:signal-user-error XQST0033 prefix)  ; was: SE5009
                 )
                (else
                 (loop (cons expr new-prlg)
                       funcs triples
                       (cons (cons prefix ns-uri) ns-binding)
                       default-elem-ns default-func-ns
                       (cdr prolog))))))))
         ((declare-default-element-namespace)
          (and
           (sa:assert-num-args expr 1)
           (sa:analyze-string-const (cadr expr) '() '() '() sa:default-ns)
           (if
            default-elem-ns  ; default prefix already declared
            (cl:signal-user-error XQST0066)  ; was: SE5010
            (loop (cons expr new-prlg)
                  funcs triples
                  ns-binding
                  (cadr (sa:op-args (car (sa:op-args expr))))
                  default-func-ns
                  (cdr prolog)))))
         ((declare-default-function-namespace)
          (and
           (sa:assert-num-args expr 1)
           (sa:analyze-string-const (cadr expr) '() '() '() sa:default-ns)
           (if
            default-func-ns  ; default prefix already declared
            (cl:signal-user-error XQST0066)  ; was: SE5011
            (loop (cons expr new-prlg)
                  funcs triples
                  ns-binding
                  default-elem-ns
                  (cadr (sa:op-args (car (sa:op-args expr))))
                  (cdr prolog)))))
         ((declare-external-function)
          (and
           (sa:assert-num-args expr 3)
           (let ((fun-qname
                  (sa:resolve-qname
                   (car (sa:op-args expr))
                   ns-binding
                   (sa:default-func-ns->string default-func-ns)))
                 (formal-args
                  (sa:analyze-formal-args (cadr (sa:op-args expr))
                                          ns-binding
                                          (or default-elem-ns "")))
                 (return-type
                  (sa:analyze-ret-type (caddr (sa:op-args expr))
                                          ns-binding
                                          (or default-elem-ns ""))))
             (and
              fun-qname formal-args return-type
              (loop
               (cons   ; new prolog
                (list (sa:op-name expr)  ; ='declare-external-function
                      fun-qname
                      (cadr (sa:op-args expr))
                      (caddr (sa:op-args expr)))
                new-prlg)
               (let ((qname-parts (sa:proper-qname fun-qname)))
                 (cons   ; funcs
                  (list                   
                   (car qname-parts) (cadr qname-parts)  ; name
                   (length formal-args)
                   (length formal-args)  ; min and max args
                   (let ((arg-types (map cadr formal-args)))
                     (lambda (num-args) arg-types))
                   return-type
                   #f  ; to denote that it is an external function
                   )
                  funcs))
               triples
               ns-binding default-elem-ns default-func-ns
               (cdr prolog))))))
         ((declare-function)
          (and
           (sa:assert-num-args expr 4)
           (let ((fun-qname
                  (sa:resolve-qname
                   (car (sa:op-args expr))
                   ns-binding
                   (sa:default-func-ns->string default-func-ns)))
                 (formal-args
                  (sa:analyze-formal-args (cadr (sa:op-args expr))
                                          ns-binding
                                          (or default-elem-ns "")))
                 (return-type
                  (sa:analyze-ret-type (caddr (sa:op-args expr))
                                          ns-binding
                                          (or default-elem-ns "")))
                 (body
                  (sa:analyze-body (cadddr (sa:op-args expr)))))
             (and
              fun-qname formal-args return-type body
              (let ((qname-parts (sa:proper-qname fun-qname)))
                (let ((ns-uri (car qname-parts))
                      (local (cadr qname-parts))
                      (arity (length formal-args)))
                  (cond
                    ((equal? ns-uri "")
                     ; The function belongs to no namespace
                     (cl:signal-user-error XQST0060 local))
                    ((member
                      ns-uri
                      (map
                       cdr
                       (filter
                        (lambda (pair)
                          (member (car pair)
                                  '("xml" "xs" "xsi"
                                    ; TODO: "fn"
                                    "xdt")))
                        sa:predefined-ns-prefixes)))
                     (cl:signal-user-error
                      XQST0045
                      (string-append ns-uri ":" local)))
                    ((not
                      (null?
                       (filter
                        (lambda (entry)
                          (and
                           (equal? (car entry) ns-uri)
                           (string=? (cadr entry) local)
                           (<= (caddr entry)  ; minimal number of arguments
                               arity)
                           (>= (caddr entry)  ; maximal number of arguments
                               arity)))
                        funcs)))
                     ; Multiple declaration of the function with same number
                     ; of arguments
                     (cl:signal-user-error
                      XQST0034
                      (string-append ns-uri ":" local
                                     ", arity = " (number->string arity))))
                    (else
                     (loop
                      (cons #f new-prlg)  ; new prolog
                      (cons   ; funcs
                       (list                   
                        ns-uri local  ; name
                        arity arity ; min and max args
                        (let ((arg-types (map cadr formal-args)))
                          (lambda (num-args) arg-types))
                        return-type)
                       funcs)
                      (cons  ; triples
                       (list
                        (list (sa:op-name expr)  ; ='declare-function
                              fun-qname
                              ; Function arguments:
                              ; was: (cadr (sa:op-args expr))
                              (map
                               (lambda (before after)
                                 (list
                                  (caddr after)  ; rewritten type
                                  ; Keep the name unresolved - it is processed later
                                  (cadr before)
                                  ;(list (sa:op-name (cadr before))  ; == 'var
                                  ;      (car after)  ; resolved name
                                  ;      )
                                  ))
                               (cadr (sa:op-args expr))
                               formal-args)
                              (list
                               (sa:op-name  ; == 'result-type
                                (caddr (sa:op-args expr))  ; result type specification
                                )
                               (car return-type)  ; rewritten type
                               )
                              (cadddr (sa:op-args expr)))
                        formal-args return-type)
                       triples)
                      ns-binding default-elem-ns default-func-ns
                      (cdr prolog))))))))))
         (else
          (cl:signal-input-error SE5012 expr)))))))

; If the alist contains a pair of equal keys, returns that key
; Otherwise, returns #f
(define (sa:equal?-keys alist)
  (cond
    ((null? alist) #f)
    ((assoc (caar alist)
            (cdr alist))
     => car  ; key+value pair found
     )
    (else
     (sa:equal?-keys (cdr alist)))))

; Processes a declare-option prolog-clause
; reversed-prolog - already processed prolog clauses, in reverse order
; Returns the modified `reversed-prolog', by optionally cons'ing a new
; member to `reversed-prolog'
; The function may raise exceptions for an ill-formed declare-option clause
(define sa:prolog-declare-option
  (let ((option-string->key+value-pairs
         (lambda (str delim-char)
           (map
            (lambda (pair)
              (if
               ; There is exactly one #\= character in the substring
               (= (length (cdr pair)) 2)
               (cons (sa:remove-boundary-spaces (cadr pair))
                     (sa:remove-boundary-spaces (caddr pair)))
               (cl:signal-user-error SE5067 (car pair))))
            (map
             (lambda (sub)
               (cons sub  ; preserve initial substring
                     (sa:string-split sub '(#\=))))
             (filter
              (lambda (sub)  ; ignore adjacent delim-char
                (not (string=? sub "")))
              (sa:string-split str (list delim-char))))))))
    (lambda (expr reversed-prolog ns-binding)
      (and
       (sa:assert-num-args expr 2)
       (sa:proper-qname (cadr expr))  ; must be proper qname
       (sa:analyze-string-const (caddr expr) '() '() '() sa:default-ns)
       (let ((name
              (sa:resolve-qname (cadr expr) ns-binding sa:se-ns))
             (value (caddr (caddr expr))))
         (let ((qname-pair
                (cadr (sa:op-args name))))
           ; Cannot use `case' here, since case relies on a `eqv?' comparison
           (cond
             ((equal? qname-pair `(,sa:se-ns "output"))
              (let ((keys+values
                     (option-string->key+value-pairs value #\;)))
                (cond
                  ((null? keys+values)
                   (cl:signal-user-error
                    SE5069
                    (string-append (car qname-pair) ":" (cadr qname-pair))))
                  ((let ((unknown-keys
                          (map car
                               (filter
                                (lambda (pair)
                                  (not
                                   (member (car pair)
                                           '("method" "indent"))))
                                keys+values))))
                     (and
                      (not (null? unknown-keys))
                      unknown-keys))
                   => (lambda (unknown-keys)
                        ;(pp unknown-keys)
                        (cl:signal-user-error
                         SE5068
                         (apply
                          string-append
                          (cdr
                           (apply
                            append
                            (map
                             (lambda (key) (list ", " key))
                             unknown-keys)))))))
                  ((sa:equal?-keys keys+values)
                   => (lambda (key)
                        (cl:signal-user-error SE5070 key)))
                  ((let ((method-pair
                          (assoc "method" keys+values)))
                     (and method-pair  ; method key binding presented
                          (not  ; unknown option value
                           (member (cdr method-pair) '("xml" "html")))
                          (cdr method-pair)  ; always true
                          ))
                   => (lambda (value)
                        (cl:signal-user-error SE5071 value)))
                  ((let ((indent-pair
                          (assoc "indent" keys+values)))
                     (and indent-pair  ; method key binding presented
                          (not  ; unknown option value
                           (member (cdr indent-pair) '("yes" "no")))
                          (cdr indent-pair)  ; always true
                          ))
                   => (lambda (value)
                        (cl:signal-user-error SE5072 value)))
                  (else
                   (cons
                    (cons (car expr)  ; declare-option
                          (cons
                           name
                           (map
                            (lambda (pair)
                              `((const (type !xs!string) ,(car pair))
                                (const (type !xs!string) ,(cdr pair))))
                            keys+values)))
                    reversed-prolog)))))
             ((equal? qname-pair `(,sa:se-ns "character-map"))
              (let ((delim-char #\!))
                (cons
                 (cons (car expr)  ; declare-option
                       (cons
                        name
                        (map
                         (lambda (pair)
                           `((const (type !xs!string) ,(car pair))
                             (const (type !xs!string) ,(cdr pair))))
                         (option-string->key+value-pairs value delim-char))))
                 reversed-prolog)))
             (else
              ; Unknown option - silently ignoring it
              reversed-prolog))))))))

;-------------------------------------------------
; Namespaces-specific analysis

; If default-func-ns is #f, returns namespace URI that corresponds to
; predefined prefix "fn:"
(define (sa:default-func-ns->string default-func-ns)
  (if default-func-ns  ; default prefix declared
      default-func-ns
      (cdr (assoc "fn" sa:predefined-ns-prefixes))))

; Resolves QName
; Returns a QName representation with resolved prefix, or #f
(define (sa:resolve-qname qname-const ns-binding default-ns)
  (let ((name-parts (sa:proper-qname qname-const)))
    (and
     name-parts
     (if
      (memq name-parts '(* unspecified))  ; wildcard
      qname-const
      (let* ((prefix (car name-parts))
             (ns-uri
              (cond
                ((symbol? prefix)  ; wildcard
                 prefix)
                ((string=? prefix "")   ; no prefix supplied
                 default-ns)
                ((let ((entry
                        (assoc prefix ns-binding)))
                   (and entry  ; declaration found
                        (cdr entry)  ; not undeclaration
                        (not  ; not undeclaration - condition 2
                         (equal? (cdr entry) ""))
                        entry))
                 => cdr)
                (else
                 (cl:signal-user-error
                  XPST0081  ; was: SE5013
                  (string-append prefix ":"
                                 (if (symbol? (cadr name-parts))
                                     (symbol->string (cadr name-parts))
                                     (cadr name-parts))))))))
        (and
         ns-uri   ; namespace URI found successfully
         (list (sa:op-name qname-const)  ; ='const
               (car (sa:op-args qname-const))   ; type of constant
               (list ns-uri (cadr name-parts)))))))))

;-------------------------------------------------
; Helpers for function declaration analysis

; Given a const which represents a QName, returns (list prefix local-name)
; prefix, local-name - strings
; If errors detected, returns #f
(define (sa:proper-qname qname-const)
  (if
   (not (and (pair? qname-const) (eq? (car qname-const) 'const)))
   (cl:signal-input-error SE5014 qname-const)
   (let ((type (car (sa:op-args qname-const)))
         (value (cadr (sa:op-args qname-const))))
     (if
      (not (and (pair? type) (eq? (sa:op-name type) 'type)
                (memq (cadr type) '(!xs!QName !xs!qname))
                (or
                 (memq value '(* unspecified))
                 (and
                  (list? value) (= (length value) 2)  ; 2 members
                  (or (string? (car value)) (eq? (car value) '*))
                  (or (string? (cadr value)) (eq? (cadr value) '*)))
                )))
      (cl:signal-input-error SE5015 qname-const)
      value))))

; Whether we can cast from from-type to to-type
;  from, to ::= sa:type-atomic | sa:type-nodes | sa:type-any
(define (sa:can-be-cast? from to)
  (not (and (eq? from sa:type-atomic)
            (eq? to sa:type-nodes))))
  
; SequenceType
; default-ns - default element namespace
; Returns (cons new-type-spec type)
(define (sa:analyze-seq-type type-spec ns-binding default-ns)
  (if
   (and (pair? type-spec)
        (memq (car type-spec) '(optional zero-or-more one-or-more one)))
   (and 
    (sa:assert-num-args type-spec 1)
    (let ((new-type (sa:analyze-item-type
                     (cadr type-spec) ns-binding default-ns)))
      (and new-type
           (cons (list (car type-spec) (car new-type))
                 (cdr new-type)))))
   (sa:analyze-item-type type-spec ns-binding default-ns)))

; ItemType
; TODO: more sophisticated treatment for built-in types
(define (sa:analyze-item-type type-spec ns-binding default-ns)
  (cond
    ((symbol? type-spec)  ; XML Schema built-in type
     (if
      ; Detect parser bug, for XQTS tests like annex-1 - 5
      (memq type-spec '(optional zero-or-more one-or-more one))
      (cl:signal-input-error SE5016 type-spec)
      (cons type-spec
            (if (memq type-spec '(xs:anyType !xs!anyType))
                sa:type-nodes sa:type-atomic))))
    ((and (pair? type-spec) (= (length type-spec) 2)
          (string? (car type-spec)) (string? (cadr type-spec)))
     ; external atomic type
     (cond
       ; TODO: construct alist at program initialization
       ((assoc type-spec
               '((("xs" "int") . !xs!int)
                 (("xs" "long") . !xs!long)
                 (("xs" "short") . !xs!short)
                 (("xs" "byte") . !xs!byte)
                 (("xs" "nonPositiveInteger") . !xs!nonPositiveInteger)
                 (("xs" "negativeInteger") . !xs!negativeInteger)
                 (("xs" "nonNegativeInteger") . !xs!nonNegativeInteger)
                 (("xs" "positiveInteger") . !xs!positiveInteger)
                 (("xs" "unsignedLong") . !xs!unsignedLong)
                 (("xs" "unsignedInt") . !xs!unsignedInt)
                 (("xs" "unsignedShort") . !xs!unsignedShort)
                 (("xs" "unsignedByte") . !xs!unsignedByte)
                 (("xs" "normalizedString") . !xs!normalizedString)
                 (("xs" "token") . !xs!token)
                 (("xs" "language") . !xs!language)
                 (("xs" "Name") . !xs!Name)
                 (("xs" "NCName") . !xs!NCName)
                 (("xs" "NMTOKEN") . !xs!NMTOKEN)
                 ;(("xs" "NMTOKENS") . !xs!NMTOKENS)
                 (("xs" "ID") . !xs!ID)
                 (("xs" "IDREF") . !xs!IDREF)
                 ;(("xs" "IDREFS") . !xs!IDREFS)
                 (("xs" "ENTITY") . !xs!ENTITY)
                 ;(("xs" "ENTITIES") . !xs!ENTITIES)
                 ;----------
                 ; XQuery predefined schema types:
                 ; 2.5.1 Predefined Schema Types                
                 ;(("xs" "anyType") . !xs!anyType)  ; These must not be supported
                 ;(("xs" "untyped") . !xs!untyped)
                 ;(("xs" "anySimpleType") . !xs!anySimpleType)
                 (("xs" "anyAtomicType") . !xs!anyAtomicType)
                 (("xs" "untypedAtomic") . !xs!untypedAtomic)
                 (("xs" "dayTimeDuration") . !xs!dayTimeDuration)
                 (("xs" "yearMonthDuration") . !xs!yearMonthDuration)
                 ))
        => (lambda (pair)
             (cons (cdr pair)
                   (if
                    (memq (cdr pair) '(!xs!anyType !xs!untyped))
                    sa:type-any sa:type-atomic))))
       ((not (string=? (car type-spec) "xs"))
        ; TODO: QName resolution in type names
        (cl:signal-user-error
         XPST0051  ; was: XPST0081
         (car type-spec)))
       (else
        ; We do not know this type
        ; Was: (cons type-spec sa:type-atomic)
        (cl:signal-user-error
         XPST0051
         (string-append (car type-spec) ":" (cadr type-spec))))))
    ((and (pair? type-spec)
          (eq? (car type-spec) 'doc-test))
     (or
      (and (null? (cdr type-spec))  ; no more arguments
           (cons type-spec sa:type-nodes))
      (and
       (sa:assert-num-args type-spec 1)
       (let ((new-ename (sa:analyze-ename
                         (car (sa:op-args type-spec))
                         ns-binding default-ns)))
         (and new-ename
              (cons (list (car type-spec)
                          (car new-ename))
                    (cdr new-ename)))))))
    ((and (pair? type-spec)
          (memq (car type-spec) '(elem-test attr-test)))
     (and
      (sa:assert-num-args type-spec 1)
      (let ((new-ename (sa:analyze-ename
                        (car (sa:op-args type-spec)) ns-binding default-ns)))
        (and new-ename
             (cons (list (car type-spec)
                         (car new-ename))
                   (cdr new-ename))))))
    ((and (pair? type-spec)
          (memq (car type-spec) '(comment-test text-test node-test)))
     (and (sa:assert-num-args type-spec 0)
          (cons type-spec sa:type-nodes)))
    ((and (pair? type-spec)
          (eq? (car type-spec) 'pi-test))
     (if
      (null? (sa:op-args type-spec))  ; no arguments
      (cons type-spec sa:type-nodes)
      (and
       (sa:assert-num-args type-spec 1)
;       (begin
;         (display (list type-spec (sa:op-args type-spec)))
;         #t)
       (sa:analyze-const (car (sa:op-args type-spec))
                         '() '() ns-binding default-ns)
       (let* ((target-const (car (sa:op-args type-spec)))
              (const-value  (caddr target-const)))
         (if
          (not (or (symbol? const-value) (string? const-value)))
          (cl:signal-input-error SE5027 type-spec)
          (cons
           (list
            (car type-spec)  ; == 'pi-test
            (list (sa:op-name target-const)  ; == 'const
                  (car (sa:op-args target-const))  ; type specifier
                  (if (symbol? const-value)
                      (symbol->string const-value)
                      const-value)))
           sa:type-nodes))))))
    ((and (pair? type-spec)
          (eq? (car type-spec) 'item-test))
     (and (sa:assert-num-args type-spec 0)
          (cons type-spec sa:type-any)))
    (else
     (cl:signal-input-error SE5016 type-spec))))

(define (sa:analyze-ename expr ns-binding default-ns)
  (if
   (not (and (pair? expr) (not (null? expr))
             (eq? (sa:op-name expr) 'ename)))
   (cl:signal-input-error SE5017 expr)
   (let ((new-name
          (and
           (sa:proper-qname (car (sa:op-args expr)))
           ; Ensure that the qname can be correctly expanded
           ; Was: commented out
           (sa:resolve-qname (car (sa:op-args expr)) ns-binding default-ns)
           ; Do not actually expand it until dynamic evaluation phase
           (car (sa:op-args expr))))
         (new-type
          (if
           (not (and (pair? (cadr (sa:op-args expr)))
                     (not (null? (cadr (sa:op-args expr))))
                     (eq? (caadr (sa:op-args expr)) 'type)))
           (cl:signal-input-error SE5018 (cadr (sa:op-args expr)))
           (cadr (sa:op-args expr))))
         (new-nil
          (sa:analyze-string-const
           (caddr (sa:op-args expr))
           '() '() ns-binding (list default-ns ""))))
     (and
      new-name new-type new-nil
      (cons (list (sa:op-name expr)  ; ='ename
                  new-name
                  new-type
                  (car new-nil))
            sa:type-nodes)))))

; Is given (listof (list sequence-type var-name))
; Returns:  vars or #f
;  vars ::= (listof (list var-name var-type type-specifier))
;  var-name ::= Scheme symbol
;  var-type ::= 'sa:atomic | 'sa:nodes
;  type-specifier ::= !xs!int or stuff
(define (sa:analyze-formal-args arg-lst ns-binding default-ns)
  (letrec ((list-contains-equal-members
            ; If the list contains `equal?' members, returns a member that
            ; has a multiple occurrence in the list.
            ; Otherwise, returns #f
            (lambda (lst)
              (cond
                ((null? lst) #f)
                ((member (car lst) (cdr lst))
                 (car lst))
                (else
                 (list-contains-equal-members (cdr lst)))))))
    (if
     (or (not (list? arg-lst))
         ; not a proper arg-lst
         (memv #f (map pair? arg-lst)))
     (cl:signal-input-error SE5019 arg-lst)
     (let ((vars
            (map
             (lambda (pair)
               (let ((type (sa:analyze-seq-type
                            (car pair) ns-binding default-ns)))
                 ;(display (list (car pair) type))
                 (cond
                   ((not type)  ; error discovered
                    #f)
                   ;((sa:var? (cadr pair))
                   ; (cons (cadr pair) (cdr type)))
                   ((eq? (sa:op-name (cadr pair)) 'var)
                    (let ((var-name
                           (sa:expand-var-name 
                            (car (sa:op-args (cadr pair)))
                            ns-binding default-ns)))
                      (and
                       var-name
                       (list var-name (cdr type) (car type)))))
                   (else
                    (cl:signal-input-error SE5020 (cadr pair))))))
             arg-lst)))
       (and
        (not (memv #f vars))
        (cond  ; equal expanded variable names?
          ((list-contains-equal-members (map car vars))
           => (lambda (name-pair)
                (cl:signal-user-error
                 XQST0039   ; was: XQST0089
                 (if  ; no namespace in variable name
                  (string=? (car name-pair) "")
                  (cadr name-pair)
                  (string-append (car name-pair) ":" (cadr name-pair))))))
          (else
           vars)))))))

; Function return type
; Returns that type or #f
(define (sa:analyze-ret-type expr ns-binding default-ns)
  (cond
    ((not (and (pair? expr) (eq? (sa:op-name expr) 'result-type)))
     (cl:signal-input-error SE5021 expr))
    ((null? (cdr expr))  ; no return type
     sa:type-any)
    ((sa:assert-num-args expr 1)
     (sa:analyze-seq-type
      (car (sa:op-args expr)) ns-binding default-ns))
    (else  ; error already displayed
     #f)))

; Function body
(define (sa:analyze-body expr)
  (if
   (not (eq? (sa:op-name expr) 'body))
   (cl:signal-input-error SE5022 expr)
   (sa:assert-num-args expr 1)))
   

;==========================================================================
; Analysis for different operations

;; Variable reference
;(define (sa:analyze-variable expr vars funcs ns-binding default-ns)
;  (cond
;    ((assoc expr vars)   ; now have to use equal?
;     => (lambda (pair)   ; exactly the required return value
;          pair))
;    ; ATTENTION: STUB FOR BACKWARD COMPATIBILITY
;    ((and (pair? expr)
;          (assoc (car expr) vars))
;     => (lambda (pair) pair))
;    (else
;     (cl:signal-error "sa:analyze-variable: variable undeclared: " expr))))
;
;; Variable reference wrapped into (list var ...)
;; TODO: resolve Qname
;(define (sa:variable-wrapped expr vars funcs ns-binding default-ns)
;  (cond
;    ((and (sa:assert-num-args expr 1)
;          (sa:analyze-variable
;           (sa:op-args expr)    ; was: (car (sa:op-args expr))
;           vars funcs ns-binding default-ns))
;     => (lambda (pair)
;          (cons expr (cdr pair))))
;    (else #f)))

; var-name ::= (list prefix local-name)
; Returns: (list ns-uri local-name) or #f
; Analyzes variable name
(define (sa:expand-var-name var-name ns-binding default-elem-ns)
  (if
   (not (and (list? var-name)
             (= (length var-name) 2)
             (= (length (filter string? var-name)) 2)))
   (cl:signal-input-error SE5023 var-name)
   (cond
     ((string=? (car var-name) "")
      (list default-elem-ns (cadr var-name)))
     ((assoc (car var-name) ns-binding)
      => (lambda (pair)
           (list (cdr pair) (cadr var-name))))
     (else
      (cl:signal-user-error XPST0081 (car var-name))  ; was: SE5024
      ))))

(define (sa:variable-wrapped expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 1)
   (let ((var-name
          (sa:expand-var-name
           (car (sa:op-args expr)) ns-binding (car default-ns))))
     (and
      var-name
      (cond
        ((assoc var-name vars)
         => (lambda (pair)
              (cons (list (sa:op-name expr)  ; = 'var
                          var-name)
                    (cdr pair))))
        ((equal? var-name (cadr sa:context-item))
         (cl:signal-user-error XPDY0002  ; was: SE5053
                               ))
        (else
         (cl:signal-user-error
          XPST0008  ; was: SE5025
          (if
           (and (pair? var-name) (= (length var-name) 2))
           (if
            (string=? (car var-name) "")
            (cadr var-name)
            (string-append (car var-name) ":" (cadr var-name)))
           expr))))))))

;-------------------------------------------------
; 2.2 Constants

(define (sa:analyze-const expr vars funcs ns-binding default-ns)
  (if
   (not (sa:assert-num-args expr 2))
   #f
   (let ((type (car (sa:op-args expr)))
         (value (cadr (sa:op-args expr))))
     (if
      (not (and (pair? type) (eq? (sa:op-name type) 'type)))
      (cl:signal-input-error SE5026 expr)
      (case (cadr type)  ; type value
        ((!xs!integer !xs!decimal !xs!double)
         (if (or (string? value) (number? value))
             (cons expr sa:type-atomic)
             (cl:signal-input-error SE5027 expr)))
        ((!xs!string)
         (if (string? value)
             (cons expr sa:type-atomic)
             (cl:signal-input-error SE5027 expr)))
        ((!xs!boolean)
         (if (symbol? value)
             (cons expr sa:type-atomic)
             (cl:signal-input-error SE5027 expr)))
        ((!xs!QName)
         (if (and (list? value) (= (length value) 2)  ; a list of 2 members
                  (string? (car value)) (string? (cadr value)))
             (cons expr sa:type-atomic)
             (cl:signal-input-error SE5028 expr)))
        (else  ; a different constant type
         (cons expr sa:type-atomic)))))))

; Particular case: a string constant is assumed
(define (sa:analyze-string-const expr vars funcs ns-binding default-ns)
  (if
   (not (sa:assert-num-args expr 2))
   #f
   (let ((type (car (sa:op-args expr)))
         (value (cadr (sa:op-args expr))))
     (cond
       ((not (and (pair? type) (eq? (sa:op-name type) 'type)))
        (cl:signal-input-error SE5026 expr))
       ((and (eq? (cadr type)  ; type 
                  '!xs!string)
             (string? value))
        (cons expr sa:type-atomic))
       (else
        (cl:signal-input-error SE5027 expr))))))

; Whether the expr is the representation for QName constant
(define (sa:qname-const? expr)
  (and (list? expr)
       (= (length expr) 3)
       (eq? (car expr) 'const)
       (equal? (cadr expr) '(type !xs!QName))))

;-------------------------------------------------
; 2.3 Axes

; Axis
(define (sa:analyze-axis expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 2)
   (let*
       ; Type is to be analyzed _before_ analyzing the subexpr, since
       ; type analysis may produce XQuery static errors only, while
       ; subexpr analysis may result in an XQuery dynamic error, e.g.
       ; when context is not defined
       ((new-type (sa:analyze-type
                   (cadr (sa:op-args expr))
                   vars funcs ns-binding default-ns))
        (a (sa:analyze-expr (car (sa:op-args expr))
                            vars funcs ns-binding default-ns)))
     (and
      a new-type
      (if
       (eq? (cdr a) sa:type-atomic)
       (if (equal? (car (sa:op-args expr)) sa:context-item)
           (cl:signal-user-error  ; was: SE5029
            XPTY0020 expr)
           (cl:signal-user-error XPTY0019 expr))
       (cons (list (sa:op-name expr)
                   (car a)
                   (car new-type))
             sa:type-nodes))))))

; `(type ,something)
(define (sa:analyze-type expr vars funcs ns-binding default-ns)
  (cond
    ((not (and (pair? expr)  ; '() is not a pair
               (eq? (sa:op-name expr) 'type)))
     (cl:signal-input-error SE5030 expr))
    ((and (sa:assert-num-args expr 1)
          (sa:analyze-seq-type
           (car (sa:op-args expr)) ns-binding (car default-ns)))
     => (lambda (pair)
          (cons (list (sa:op-name expr)  ; ='type
                      (car pair))
                (cdr pair))))
    (else  ; error message already displayed
     #f)))

;-------------------------------------------------
; 2.4 Sequence operations

; Combines several types into one
(define (sa:combine-types type-lst)
  (if
   (or (null? type-lst)
       (memq sa:type-any type-lst)
       (and (memq sa:type-nodes type-lst)
            (memq sa:type-atomic type-lst)))
   sa:type-any
   ; type-lst now contains only all nodes or all atomic
   (car type-lst)))

; Processing sequences, space-sequences and unios
; Important note: the sequence may contain namespace declarations that
; affect the following parsing
(define (sa:analyze-sequence expr vars funcs ns-binding default-ns)
  (let ((args-res
         (map
          (lambda (subexpr)
            (sa:analyze-expr subexpr vars funcs ns-binding default-ns))
          (sa:op-args expr))))
    (if
     (memv #f args-res)  ; error detected    
     #f
     (cons (cons (sa:op-name expr)  ; ='sequence or 'unio or 'space-sequence
                 (map car args-res))
           (sa:combine-types (map cdr args-res))))))

;-------------------------------------------------
; 2.5 Arithmetic operations

(define (sa:binary-arithmetic expr vars funcs ns-binding default-ns)
  (if
   (sa:assert-num-args expr 2)
   (sa:propagate expr vars funcs ns-binding default-ns sa:type-atomic)
   #f))

(define (sa:unary-arithmetic expr vars funcs ns-binding default-ns)
  (if
   (sa:assert-num-args expr 1)
   (sa:propagate expr vars funcs ns-binding default-ns sa:type-atomic)
   #f))

(define (sa:analyze-union-intersect expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 2)
   (let ((args-res (map
                    (lambda (arg)
                      (sa:analyze-expr arg vars funcs ns-binding default-ns))
                    (sa:op-args expr))))
     (cond
       ((memv #f args-res)  ; error detected for args-res
        #f)
       ((memq sa:type-atomic (map cdr args-res))  ; atomic argument
        (cl:signal-user-error XPTY0004  ; was: SE5052
                              expr))
       (else  ; form new expr            
        (cons (cons (sa:op-name expr) (map car args-res))
              sa:type-nodes))))))

;-------------------------------------------------
; 2.6 Comparison operations

; Value comparison
(define (sa:analyze-comparison expr vars funcs ns-binding default-ns)
  (if
   (sa:assert-num-args expr 2)
   (sa:propagate expr vars funcs ns-binding default-ns sa:type-atomic)
   #f))

; Node comparison
(define (sa:analyze-node-comparison expr vars funcs ns-binding default-ns)
  (if
   (not (sa:assert-num-args expr 2))
   #f
   (let ((args-res
          (map
           (lambda (subexpr)
             (sa:analyze-expr subexpr vars funcs ns-binding default-ns))
           (sa:op-args expr))))
     (cond
       ((member #f args-res)  ; error detected
        #f)
       ((memq sa:type-atomic args-res)  ; at least one argument is atomic
        (cl:signal-user-error XPTY0004 expr)  ; was: SE5031
        )
       (else
        (cons (cons (sa:op-name expr)
                    (map car args-res))
              sa:type-atomic))))))

;-------------------------------------------------
; 2.7 Conditional operation

(define (sa:analyze-if expr vars funcs ns-binding default-ns)
  (if
   (not (sa:assert-num-args expr 3))
   #f
   (let ((args-res
         (map
          (lambda (subexpr) (sa:analyze-expr subexpr vars funcs ns-binding default-ns))
          (sa:op-args expr))))
     (if
      (memv #f args-res)  ; error detected      
      #f
      (cons (cons (sa:op-name expr)
                  (map car args-res))
            (sa:combine-types
             (cdr (map cdr args-res))))))))

;-------------------------------------------------
; 2.8 Logical operations

; QUESTION: is this operation called 'not or 'not@ ?
(define (sa:analyze-not@ expr vars funcs ns-binding default-ns)
  (if
   (sa:assert-num-args expr 1)
   (sa:propagate expr vars funcs ns-binding default-ns sa:type-atomic)
   #f))

(define (sa:analyze-and@-or@ expr vars funcs ns-binding default-ns)
  (if
   (sa:assert-num-args expr 2)
   (sa:propagate expr vars funcs ns-binding default-ns sa:type-atomic)
   #f))

;-------------------------------------------------
; 2.9 Constructors

; Is given the expr that represents the element constructor
; Scans whether any xmlns declarations are made within the given element
; Returns the modified ns-binding and default-ns
; Returns: (cons new-ns-binding new-default-ns)
; Never raises any error messages
; The element with namespace declarations looks as follows:
;   '(element
;     (const (type !xs!QName) ("a" "tag"))
;     (sequence
;       (namespace
;         (const (type !xs!NCName) "d")
;         (const (type !xs!string) "http://modis.com"))
;       ...))
; TODO: should raise error on default prefix declaration
(define (sa:xmlns-declarations-in-element expr ns-binding default-ns)
  (let ((sequence (and (= (length (sa:op-args expr)) 2)
                       (list? (cadr (sa:op-args expr)))
                       (eq? (caadr (sa:op-args expr)) 'sequence)
                       (cadr (sa:op-args expr)))))
    (if
     (not sequence)  ; element content is not a 'sequence
     (cons ns-binding default-ns)
     (let ((namespaces
            ; Returns a (listof (cons prefix ns-uri))
            (map
             (lambda (namespace)  ; operation with namespace declaration
               (cons
                (cadr (sa:op-args (car (sa:op-args namespace))))  ; prefix
                (cadr (sa:op-args (cadr (sa:op-args namespace))))))
             (filter
              (lambda (op)
                (and (list? op) (eq? (sa:op-name op) 'namespace)
                     (= (length (sa:op-args op)) 2)
                     (= (length
                         (filter
                          (lambda (c)
                            (and (list? c)
                                 (eq? (car c) 'const)
                                 (= (length (sa:op-args c)) 2)
                                 (string? (cadr (sa:op-args c)))))
                          (sa:op-args op)))
                        2)))
                (cdr sequence)))))
       ;(display namespaces)
       ;(newline)
       (cond
         ((assoc "" namespaces)  ; default prefix
          => (lambda (default-pair)
               (cons (append
                      (filter
                       (lambda (pair) (not (equal? (car pair) "")))
                       namespaces)
                      ns-binding)
                     (cons (cdr default-pair)  ; override default element ns
                           (cdr default-ns)))))
         (else
          (cons (append namespaces ns-binding)
                default-ns)))))))

; Post processing of element constructor
; Accepts: pair ::= (cons element-constructor var-type)
; 1. Checks that duplicate attributes are not declared
; 2. Moves namespace declarations in from of attribute declarations
; 3. Checks that duplicate namespace prefixes are not declared
; 4. Checks that built-in namespace prefixes are not re-declared
; Returns: (cons new-element-constructor var-type)
(define (sa:post-element pair ns-binding default-ns)
  (let* ((elem (car pair))
         (body (cadr (sa:op-args elem))))
    (let loop ((src ((if (and (pair? body)
                              (eq? (sa:op-name body) 'sequence))
                         cdr list)
                     body))
               (namespaces '())
               (others '())
               (prefixes '())
               (attr-names '()))
      (cond
        ((null? src)  ; content processed
         (let ((new-content
                (reverse (append others namespaces))))
           (cons
            (list
             (sa:op-name elem)  ; == 'element
             (car (sa:op-args elem))  ; element name
             (if (and (not (and (pair? body)
                                (eq? (sa:op-name body) 'sequence)))
                      (not (null? new-content))
                      (null? (cdr new-content)))
                 (car new-content)
                 (cons 'sequence new-content)))
            (cdr pair)  ; var type
            )))
        ((and (pair? (car src))
              (eq? (sa:op-name (car src)) 'attribute))
         (let ((name (car (sa:op-args (car src)))))
           (if
            (sa:qname-const? name)
            ; Constant attribute name
            (let ((name
                   (sa:resolve-qname name ns-binding (cadr default-ns))))
              (if
               (member (cadr (sa:op-args name)) attr-names)
               ; Duplicate attribute declared
               (cl:signal-user-error XQST0040 (sa:qname->string name))
               (loop (cdr src)
                     namespaces
                     (cons (car src) others)
                     prefixes
                     (cons (cadr (sa:op-args name)) attr-names))))
            (loop (cdr src)
                  namespaces (cons (car src) others)
                  prefixes attr-names))))
        ((and (pair? (car src))
              (eq? (sa:op-name (car src)) 'namespace))
         (let ((prefix (car (sa:op-args (car src)))))
           (if
            (and (pair? prefix)
                 (eq? (sa:op-name prefix) 'const)
                 (equal? (car (sa:op-args prefix))
                         '(type !xs!NCName)))
            ; Constant namespace prefix
            (let ((str (cadr (sa:op-args prefix))))
              (cond
                ((member str '("xml" "xmlns"))
                 ; Attempt to re-declare a predefined namespace prefix 
                 (cl:signal-user-error XQST0070 str))
                ((member str prefixes)
                 ; Duplicate prefix declared
                 (if (string=? str "")
                     ; Default namespace prefix              
                     (cl:signal-user-error XQST0071 "xmlns")
                     (cl:signal-user-error
                      XQST0071
                      (string-append "xmlns:" str))))
                ((not  ; not a constant namespace value
                  (let ((ns-value
                         (cadr (sa:op-args (car src)))))
                    (or
                     (and (pair? ns-value)
                          (eq? (sa:op-name ns-value) 'const))
                     (equal? ns-value '(sequence)))))
                 (if (string=? str "")
                     ; Default namespace prefix              
                     (cl:signal-user-error XQST0022 "xmlns")
                     (cl:signal-user-error
                      XQST0022
                      (string-append "xmlns:" str))))
                (else
                 (loop (cdr src)
                       (cons (car src) namespaces)
                       others
                       (cons str prefixes)
                       attr-names))))
            ; Computed namespace constructors are not allowed
            ; ATTENTION: should raise an exception here
            (loop (cdr src)
                  (cons (car src) namespaces) others
                  prefixes attr-names))))
        (else
         (loop (cdr src)
               namespaces (cons (car src) others)
               prefixes attr-names))))))

; Element constructor
(define (sa:analyze-element-constructor expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 2)
   (let* ((new-xmlns
           (sa:xmlns-declarations-in-element expr ns-binding default-ns))
          (ns-binding (car new-xmlns))
          (default-ns (cdr new-xmlns)))
     (and
      (or
       (not (sa:qname-const? (car (sa:op-args expr))))
       ; If name is constant, it must be correctly resolved
       (sa:resolve-qname
        (car (sa:op-args expr)) ns-binding (cadr default-ns)))
      (sa:post-element
       (sa:propagate expr vars funcs ns-binding default-ns sa:type-nodes)
       ns-binding
       default-ns)))))
     
; Constructors for attribute, pi and namespace
; TODO: more sophisticated treatment for pi and namespace names is desirable
; QUESTION: how many arguments constructors must have?
(define (sa:attribute-pi-namespace
         expr vars funcs ns-binding default-ns)
  (and
   (or
    (and (eq? (sa:op-name expr) 'pi)  ; PI constructor
         (= (length (sa:op-args expr)) 1)  ; no PI body
         )
    (sa:assert-num-args expr 2))
   (if
    (sa:qname-const? (car (sa:op-args expr)))  ; name is constant
    (and
     (sa:resolve-qname
      (car (sa:op-args expr)) ns-binding (cadr default-ns))
     (sa:propagate expr vars funcs ns-binding default-ns sa:type-nodes))
    (sa:propagate expr vars funcs ns-binding default-ns sa:type-nodes))))

; Constructors for document node, text node, comment node
(define (sa:document-text-comment expr vars funcs ns-binding default-ns)
  (if
   (sa:assert-num-args expr 1)
   (sa:propagate expr vars funcs ns-binding default-ns sa:type-nodes)
   #f))

;-------------------------------------------------
; 2.10 FLWOR Operations
; TODO: order-by

(define (sa:analyze-let@ expr vars funcs ns-binding default-ns)
  (if
   (not (sa:assert-num-args expr 2))
   #f
   (let ((new-value
          (sa:analyze-expr (car (sa:op-args expr)) vars funcs ns-binding default-ns))
         (new-fun
          (sa:analyze-fun-def (cadr (sa:op-args expr)) vars funcs ns-binding default-ns)))
     (and
      new-value new-fun
      (cons (list (sa:op-name expr)  ; ='let@
                  (car new-value)
                  (car new-fun))
            (cdr new-fun))))))

; Return
(define (sa:analyze-return expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 2)
   (let ((new-value
          (sa:analyze-expr (car (sa:op-args expr)) vars funcs ns-binding default-ns))
         (new-fun
          (sa:analyze-fun-def (cadr (sa:op-args expr))
                              vars funcs ns-binding default-ns)))
     (and
      new-value new-fun
      (cons (list (sa:op-name expr)  ; ='return
                  (car new-value)
                  (car new-fun))
            (cdr new-fun))))))

; Clone of Return except for return value
(define (sa:analyze-predicate expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 2)
   (let ((new-value
          (sa:analyze-expr (car (sa:op-args expr)) vars funcs ns-binding default-ns))
         (new-fun
          (sa:analyze-fun-def (cadr (sa:op-args expr)) vars funcs ns-binding default-ns)))
     (and
      new-value new-fun
      (cons (list (sa:op-name expr)  ; ='predicate
                  (car new-value)
                  (car new-fun))
            (cdr new-value))))))

; A call to fun-def.
; It's argument always has the type 'sa:nodes
; Returns the return type or #f
(define (sa:analyze-fun-def expr vars funcs ns-binding default-ns)
  (if
   (not (and (pair? expr) (eq? (sa:op-name expr) 'fun-def)))
   (cl:signal-input-error SE5032 expr)
   (and
    (sa:assert-num-args expr 2)
    (let ((formal-args
           (sa:analyze-formal-args
            (car (sa:op-args expr)) ns-binding (car default-ns))))
      (cond
        ((not formal-args) #f)
        ;((> (length formal-args) 3)
        ; (cl:signal-input-error SE5033 expr))
        (else
         (let ((expr-pair
                (sa:analyze-expr (cadr (sa:op-args expr))
                                 (append formal-args vars)
                                 funcs ns-binding default-ns)))
           (and
            expr-pair
            (cons
             (list (sa:op-name expr)  ; = 'fun-def
                   (map  ; args
                    (lambda (argument-pair analyzed-triple)
                      (list
                       (caddr analyzed-triple)  ; argument type
                       
                       (list  ; variable name
                        (caadr argument-pair)   ; = 'var
                        (car analyzed-triple)  ; expanded variable name
                        ; Was:
                        ;(sa:expand-var-name
                        ; (cadadr pair)  ; var-name
                        ; ns-binding (car default-ns))
                        )))
                    (car (sa:op-args expr))
                    formal-args)
                   (car expr-pair))
             (cdr expr-pair))))))))))

;-------------------------------------------------
; 2.11 Expressions on Sequence Types

; return-type-lambda ::= (lambda (args) ...)
; args - rewritten arguments of the operation
(define (sa:cast-helper return-type-lambda)
  (lambda (expr vars funcs ns-binding default-ns)
    (and
     (sa:assert-num-args expr 2)
     (let ((args
            (list
             (sa:analyze-expr (car (sa:op-args expr))
                              vars funcs ns-binding default-ns)
             (sa:analyze-type (cadr (sa:op-args expr))
                              vars funcs ns-binding default-ns))))
       (and
        (not (memv #f args))
        (let* ((type-spec (caadr args))  ; selects '(type ...)
               (seq-type (cadr type-spec))
               (item-type (if (and  ; occurrence indicator presented
                               (pair? seq-type)
                               (not (null? (cdr seq-type))))
                              (cadr seq-type)
                              seq-type)))
          (if
           (eq? item-type '!xs!anyAtomicType)
           (cl:signal-user-error XPST0080 "Cast as xs:anyAtomicType")
           (cons (cons (sa:op-name expr)
                       (map car args))
                 (return-type-lambda args)))))))))

(define sa:analyze-cast (sa:cast-helper
                         cdar  ; type of the subexpr
                         ))

; Castable
; TODO: analyze single type instead of item type
(define sa:analyze-castable
  (sa:cast-helper
   (lambda (args) 'sa:type-atomic)))

; Treat
(define (sa:analyze-treat expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 2)
   (let ((args (list
                (sa:analyze-expr
                 (car (sa:op-args expr)) vars funcs ns-binding default-ns)
                (sa:analyze-type
                 (cadr (sa:op-args expr)) vars funcs ns-binding default-ns))))
     (if (memv #f args)
         #f
         (cons (cons (sa:op-name expr)
                     (map car args))
               (cdadr args))))))
; DL: XPST0080 is not to be thrown for treat for xs:anyAtomicType
;(define sa:analyze-treat (sa:cast-helper
;                          cdadr  ; type of the treat-type
;                         ))

; Typeswitch
(define (sa:analyze-typeswitch expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 2)
   (let ((source-expr
          (sa:analyze-expr (car (sa:op-args expr))
                           vars funcs ns-binding default-ns)))
     (and
      source-expr
      (let ((cases (sa:ts-all-cases (cadr (sa:op-args expr))
                                    vars funcs ns-binding default-ns
                                    (cdr source-expr)  ; source expression type
                                    )))
        (and
         cases  ; no error detected
         (cons (list (sa:op-name expr)  ; = 'ts
                     (car source-expr)
                     (car cases))
               (cdr cases)  ; return type
               )))))))

(define (sa:ts-all-cases expr vars funcs ns-binding default-ns src-type)
  (cond
    ((not (and (pair? expr) (eq? (car expr) 'cases)))
     (cl:signal-input-error SE5056 expr))
    ((null? (sa:op-args expr))
     ; At least a default-case is required 
     (cl:signal-input-error SE5057 expr))
    (else
     (let loop ((args (sa:op-args expr))  ; non-null
                (res '()))
       (if
        (null? args)  ; all scanned
        (cons (cons (sa:op-name expr)  ; = 'cases
                    (map car (reverse res)))
              (sa:combine-types (map cdr
                                     res  ; no need for reverse, the effect is symmetric
                                     )))
        (let ((case ((if (null? (cdr args))  ; this is the last case
                         sa:ts-default sa:ts-case)
                     (car args)
                     vars funcs ns-binding default-ns src-type)))
          (and
           case
           (loop (cdr args)
                 (cons case res)))))))))

(define (sa:ts-case expr vars funcs ns-binding default-ns src-type)
  (cond
    ((not (and (pair? expr) (eq? (car expr) 'case)))
     (cl:signal-input-error SE5058 expr))
    (else
     (and
      (sa:assert-num-args expr 2)
      (let ((type-pair (sa:analyze-type (car (sa:op-args expr))
                                        vars funcs ns-binding default-ns))
            (func-pair (sa:analyze-fun-def (cadr (sa:op-args expr))
                                           vars funcs ns-binding default-ns)))
        (and
         type-pair func-pair
         (cons (list (sa:op-name expr)  ; = 'case
                     (car type-pair)
                     (car func-pair))
               (cdr func-pair))))))))       

(define (sa:ts-default expr vars funcs ns-binding default-ns src-type)
  (cond
    ((not (and (pair? expr) (eq? (car expr) 'default)))
     (cl:signal-input-error SE5059 expr))
    (else
     (and
      (sa:assert-num-args expr 1)
      (let ((new-fun (sa:analyze-fun-def (car (sa:op-args expr))
                                         vars funcs ns-binding default-ns)))
        (and
         new-fun
         (cons (list (sa:op-name expr)  ; = 'default
                     (car new-fun))
               (cdr new-fun))))))))

;-------------------------------------------------
; 2.14 Distinct doc order

(define (sa:analyze-ddo expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 1)
   (let ((seq-res (sa:analyze-expr (car (sa:op-args expr)) vars funcs ns-binding default-ns)))
     (and
      seq-res
      (if
       (eq? (cdr seq-res) sa:type-atomic)
       (cl:signal-input-error SE5034 expr)
       (cons (list (sa:op-name expr)             
                   (car seq-res))
             (cdr seq-res)))))))

;-------------------------------------------------
; 3.3 XPath

(define (sa:analyze-congen1 expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 2)
   (let ((seq-res (sa:analyze-expr
                   (car (sa:op-args expr)) vars funcs ns-binding default-ns))
         (secnd (cadr (sa:op-args expr))))
     (if
      (not (and (list? secnd)
                (eq? (car secnd) 'ivar)
                (not (null? (cdr secnd)))
                (list? (cadr secnd))
                (= (length (cadr secnd)) 2)
                (= (length (filter string? (cadr secnd))) 2)))
;      (or (null? secnd) (not (pair? secnd))
;          (not (eq? (car secnd) 'ivar))
;          (null? (cdr secnd)) (not (symbol? (cadr secnd))))
      (cl:signal-input-error SE5035 expr)
      (and
       seq-res
       (cons (list (sa:op-name expr)
                   (car seq-res)
                   secnd)
             (cdr seq-res)))))))

(define (sa:analyze-congen2 expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 1)
   (let ((pair
          (sa:analyze-expr
           (car (sa:op-args expr)) vars funcs ns-binding default-ns)))
     (and
      pair
      (cons (list (sa:op-name expr)
                  (car pair))
            (cdr pair))))))

;-------------------------------------------------
; 3.6. Quantified expressions

; Some, every
(define (sa:some-every expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 2)
   (let ((seq-res
          (sa:analyze-expr (car (sa:op-args expr)) vars funcs ns-binding default-ns))
         (fun-res
          (sa:analyze-fun-def (cadr (sa:op-args expr)) vars funcs ns-binding default-ns)))
     (and
      seq-res fun-res
      (cons (list (sa:op-name expr)
                  (car seq-res) (car fun-res))
            sa:type-any)))))

;-------------------------------------------------
; 3.7 XQuery 1.0 Functions

; fn:doc and fn:collection
(define (sa:document-collection expr vars funcs ns-binding default-ns)
  (if
   (< (length (sa:op-args expr)) 3)  ; enough arguments
   (sa:propagate expr vars funcs ns-binding default-ns sa:type-nodes)
   (sa:assert-num-args expr 1)  ; this would cause an error message
   ))

; fn:position, fn:last, fn:true, fn:false
(define (sa:position-last-true-false expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 0)
   (cons expr sa:type-atomic)))

; Functions with a single argument, return atomic value
(define (sa:basic-singlearg-atomic expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 1)
   (let ((pair
          (sa:analyze-expr (car (sa:op-args expr)) vars funcs ns-binding default-ns)))
     (and
      pair
      (cons (list (sa:op-name expr)
                  (car pair))
            sa:type-atomic)))))

; Functions with zero or more arguments, return atomic values
; fn:count, fn:sum, fn:avg, fn:min, fn:max, fn:distinct-values, fn:concat
(define (sa:basic-multiarg-atomic expr vars funcs ns-binding default-ns)
  (let ((args-res
         (map
          (lambda (subexpr)
            (sa:analyze-expr subexpr vars funcs ns-binding default-ns))
          (sa:op-args expr))))
    (if
     (memv #f args-res)  ; error detected
     #f
     (cons (cons (sa:op-name expr)
                 (map car args-res))           
           sa:type-atomic))))

(define (sa:basic-replace expr vars funcs ns-binding default-ns)
  (cond
    ((> (length (sa:op-args expr)) 4)  ; too many arguments
     (sa:assert-num-args expr 4)  ; will raise an error
     )
    ((< (length (sa:op-args expr)) 3)  ; too many arguments
     (sa:assert-num-args expr 3)  ; will raise an error
     )
    (else
     (sa:propagate expr vars funcs ns-binding default-ns sa:type-atomic))))

;-------------------------------------------------
; Not expressed in the new logical representation

; Function call
(define (sa:analyze-fun-call expr vars funcs ns-binding default-ns)
  (let ((args (sa:op-args expr)))
    (if
     (null? args)  ; no arguments
     (cl:signal-input-error SE5036 expr)
     (let ((fun-name (sa:resolve-qname
                      (car args) ns-binding (cadr default-ns))))
       (and
        fun-name
        (let ((name-parts (sa:proper-qname fun-name)))
          (let loop ((fs funcs))
            (cond
              ((null? fs)  ; all functions scanned
               (cl:signal-user-error XPST0017  ; was: SE5037
                                     (cadr (caddr  ; extract function name
                                            (car (sa:op-args expr))))
                                     ; expr
                                     ))
              ((and (string=? (caar fs) (car name-parts))
                    (string=? (cadar fs) (cadr name-parts)))
               (let ((fun-declaration (car fs))
                     (num-actual (length
                                  ; fun-call arguments minus fun-name
                                  (cdr args))))
                 (if
                  (or (< num-actual (list-ref fun-declaration 2))
                      (and (list-ref fun-declaration 3)  ; max-args
                           (> num-actual (list-ref fun-declaration 3))))
                  (cl:signal-user-error
                   XPST0017  ; was: SE5038
                   (string-append (car name-parts) ":" (cadr name-parts)))
                  (let ((formal-args
                         ((list-ref fun-declaration 4) num-actual))
                        ; Actual function arguments are to be analyzed ONLY
                        ; after the function name and the proper number of
                        ; arguments are matched
                        (actual-args
                         (map
                          (lambda (subexpr)
                            (sa:analyze-expr
                             subexpr vars funcs ns-binding default-ns))
                          (cdr args))))
                    (and
                     (not (memv #f actual-args))  ; error detected in arguments
                     (let rpt ((form formal-args)
                               (act actual-args))
                       (cond
                         ((null? form) ; all scanned successfully
                          (sa:fun-call-post-proc  ; post processing
                           (cons
                            (if
                             (= (length fun-declaration) 7)
                             (let ((extra (list-ref fun-declaration 6)))
                               (cond
                                 ((not extra)  ; call to external function
                                  (cons 'ext-fun-call
                                        (cons fun-name
                                              (map car actual-args))))
                                 ((and (pair? extra) (eq? (car extra) 'cast))
                                  `(cast
                                    ,@(map car actual-args)
                                    (type (one ,(cadr extra)))))
                                 (else  ; Basic name for function provided
                                  (cons
                                   (list-ref fun-declaration 6)  ; fun name
                                   (map car actual-args)))))
                             (cons (sa:op-name expr) ; ='fun-call
                                   (cons fun-name
                                         (map car actual-args))))
                            ; function return type
                            (list-ref fun-declaration 5))))
                         ((and (eq? (car form) sa:type-nodes)
                               (eq? (cdar act) sa:type-atomic))
                          (cl:signal-user-error XPTY0004 expr)  ; was: SE5039
                          )
                         (else
                          (rpt (cdr form) (cdr act))))))))))
              (else
               (loop (cdr fs)))))))))))


;==========================================================================
; Different kinds of queries
; This includes update operations, manage and retrieve-metadata

;-------------------------------------------------
; Update operations
; This is section 3.8 in the XQuery logical representation

(define (sa:analyze-update expr vars funcs ns-binding default-ns)
  (if
   (or (not (pair? expr)) (null? expr))
   (cl:signal-input-error SE5040 expr)
   (case (sa:op-name expr)  ; operation name       
     ((insert-into insert-preceding insert-following)
      (sa:analyze-insert expr vars funcs ns-binding default-ns))
     ((rename)
      (sa:analyze-rename expr vars funcs ns-binding default-ns))
     ((delete delete_undeep)
      (sa:analyze-delete expr vars funcs ns-binding default-ns))
     ((replace move-into move-preceding move-following)
      (sa:analyze-replace-move expr vars funcs ns-binding default-ns))
     (else
      (cl:signal-input-error SE5041 expr)))))

(define (sa:analyze-insert expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 2)
   (let ((frst-res
          (sa:analyze-expr
           (car (sa:op-args expr)) vars funcs ns-binding default-ns))
         (secnd-res
          (sa:analyze-expr
           (cadr (sa:op-args expr)) vars funcs ns-binding default-ns)))
     (and
      frst-res secnd-res
      (if
       (eq? (cdr secnd-res) sa:type-atomic)
       (cl:signal-user-error SE5042 expr)
       (list (sa:op-name expr)
             (car frst-res) (car secnd-res)))))))

(define (sa:analyze-rename expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 2)
   (let ((frst-res
          (sa:analyze-expr
           (car (sa:op-args expr)) vars funcs ns-binding default-ns)))
     (and
      frst-res 
      (if
       (eq? (cdr frst-res) sa:type-atomic)
       (cl:signal-user-error SE5043 expr)
       (list (sa:op-name expr)
             (car frst-res)
             (cadr (sa:op-args expr))   ; attention!!!
             ))))))

(define (sa:analyze-delete expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 1)
   (let ((arg-res
          (sa:analyze-expr
           (car (sa:op-args expr)) vars funcs ns-binding default-ns)))
     (if
      (eq? (cdr arg-res) sa:type-atomic)
      (cl:signal-user-error SE5044 expr)
      (list (sa:op-name expr)
            (car arg-res))))))

; TODO: probably, the first argument should always be a node()*
(define (sa:analyze-replace-move expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 2)
   (let ((ex-pair
          (sa:analyze-expr
           (car (sa:op-args expr)) vars funcs ns-binding default-ns))
         (fun-pair 
          (sa:analyze-fun-def
           (cadr (sa:op-args expr)) vars funcs ns-binding default-ns)))
     (and
      ex-pair fun-pair
      (list (sa:op-name expr)
            (car ex-pair) (car fun-pair))))))

;-------------------------------------------------
; Manage operations

(define (sa:analyze-manage expr vars funcs ns-binding default-ns)
  (cond
    ((or (not (pair? expr)) (null? expr))
     (cl:signal-input-error SE5045 expr))
    ((case (sa:op-name expr)  ; operation name
       ((create-collection drop-collection)
        (sa:analyze-manage-collection
         expr vars funcs ns-binding default-ns))
       ((create-document drop-document)
        (sa:analyze-manage-document
         expr vars funcs ns-binding default-ns))
       ((load)
        (sa:analyze-manage-load
         expr vars funcs ns-binding default-ns))
       ;-------------------
       ; Security operations
       ((create-role drop-role drop-user)
        ((sa:security-helper 1 #f) expr))
       ((grant-priv-on grant-priv-on-doc grant-priv-on-col
                       revoke-priv-from-doc revoke-priv-from-col)
        ((sa:security-helper 3 #t) expr))
       ((grant-priv revoke-priv)
        ((sa:security-helper 2 #t) expr))
       ((grant-role revoke-role create-user alter-user)
        ((sa:security-helper 2 #f) expr))
       ;-------------------
       ; Index operations
       ((create-index)
        (sa:analyze-index-create expr vars funcs ns-binding default-ns))
       ((drop-index)
        (sa:analyze-index-drop expr vars funcs ns-binding default-ns))
       ;-------------------
       ; Full-text
       ((create-fulltext-index)
        (sa:analyze-full-text-create expr vars funcs ns-binding default-ns))
       ((drop-fulltext-index)
        (sa:analyze-full-text-drop expr vars funcs ns-binding default-ns))
       (else
        (cl:signal-input-error SE5046 expr)))
     => (lambda (new-expr) new-expr))
    (else  ; error already detected
     #f)))

(define (sa:analyze-manage-collection
         expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 1)
   (let ((new
          (sa:propagate expr vars funcs ns-binding default-ns
                        'sa:atomic  ; dummy
                        )))
     (and new (car new)))))

(define (sa:analyze-manage-document
         expr vars funcs ns-binding default-ns)
  (and
   (or
    (let ((lng (length (sa:op-args expr))))
      (and (> lng 0) (< lng 3)))
    (sa:assert-num-args expr 2))
   (let ((new
          (sa:propagate expr vars funcs ns-binding default-ns
                        'sa:atomic  ; dummy
                        )))
     (and new (car new)))))

(define (sa:analyze-manage-load expr vars funcs ns-binding default-ns)
  (and
   (or
    (let ((lng (length (sa:op-args expr))))
      (and (>= lng 2) (<= lng 3)))
    (sa:assert-num-args expr 3))
   (let ((new
          (sa:propagate expr vars funcs ns-binding default-ns
                        'sa:atomic  ; dummy
                        )))
     (and new (car new)))))

(define (sa:nodeset? x)
  (or (and (pair? x) (not (symbol? (car x)))) (null? x)))

; Helper for analyzing security
; Consumes the number of arguments required for an operation
; first-argument? - whether the first argument of the operation is to be
;  analyze for predefined values
(define (sa:security-helper num-args first-argument?)
  (lambda (expr)
    (and
     (sa:assert-num-args expr num-args)
     (not
      (memv
       #f
       (map
        (lambda (arg)
          (if
           (sa:nodeset? arg)
           (not
            (memv
             #f
             (map
              (lambda (arg2)
                (sa:analyze-string-const arg2 '() '() '() sa:default-ns))
              arg)))
           (sa:analyze-string-const arg '() '() '() sa:default-ns)))
        (sa:op-args expr))))
     (if
      first-argument?
      (let* ((predefined-set
              (map
               (lambda (x) `(const (type !xs!string) ,x))
               '("CREATE-USER"
                 "CREATE-DOCUMENT"
                 "CREATE-COLLECTION"
                 "CREATE-INDEX"
                 "CREATE-TRIGGER"
                 "LOAD"
                 "DROP"
                 "QUERY"
                 "INSERT"
                 "DELETE"
                 "RENAME"
                 "REPLACE"
                 "RETRIEVE-METADATA"
                 "ALL")))
             (not-from-set
              (filter
               (lambda (arg) (not (member arg predefined-set)))
               ((if (sa:nodeset? (car (sa:op-args expr)))
                    (lambda (x) x) list)
                (car (sa:op-args expr))))))
        (if
         (null? not-from-set)
         #t
         (cl:signal-user-error
          SE3069
          (apply append
                 (map
                  (lambda (cnst) (list (caddr cnst) " "))
                  not-from-set)))))
      #t)
     expr)))

;-------------------------------------------------
; Retrieve metadata operations

(define (sa:analyze-retrieve-metadata expr vars funcs ns-binding default-ns)
  (if
   (or (not (pair? expr)) (null? expr))
   (cl:signal-input-error SE5047 expr)
   (case (sa:op-name expr)  ; operation name
     ((retrieve-metadata-documents)
      (sa:analyze-metadata-documents
       expr vars funcs ns-binding default-ns))
     ((retrieve-metadata-collections)
      (sa:analyze-metadata-collections
       expr vars funcs ns-binding default-ns))
     ((retrieve-descr-scheme)
      (sa:analyze-descr-scheme
       expr vars funcs ns-binding default-ns))
     (else
      (cl:signal-input-error SE5048 expr)))))

(define (sa:analyze-metadata-documents
         expr vars funcs ns-binding default-ns)
  (if
   (> (length (sa:op-args expr)) 2)
   (sa:assert-num-args expr 2)  ; this will raise the error message
   (let ((new
          (sa:propagate expr vars funcs ns-binding default-ns
                        'sa:atomic  ; dummy
                        )))
     (and new (car new)))))

(define (sa:analyze-metadata-collections
         expr vars funcs ns-binding default-ns)
  (or
   (and 
    (null? (sa:op-args expr))  ; no arguments
    expr)
   (and
    (sa:assert-num-args expr 1)
    (let ((new
          (sa:propagate expr vars funcs ns-binding default-ns
                        'sa:atomic  ; dummy
                        )))
      (and new (car new))))))

(define (sa:analyze-descr-scheme expr vars funcs ns-binding default-ns)
  (and
   (or
    (let ((lng (length (sa:op-args expr))))
      (and (> lng 0) (< lng 3)))
    (sa:assert-num-args expr 2))
   (let ((new
          (sa:propagate expr vars funcs ns-binding default-ns
                        'sa:atomic  ; dummy
                        )))
     (and new (car new)))))

;-------------------------------------------------
; Index managing operators

; Whether the `expr' represents a structural XPath
; The other semantic processing should be already performed
; Returns the logical representation for document or collection
(define (sa:structural-absolute-xpath? expr)
  (case (sa:op-name expr)
    ((!fn!document !fn!collection)
     expr)
    ((ddo attr-axis child descendant descendant-or-self self)
     (sa:structural-absolute-xpath? (car (sa:op-args expr))))
    (else
     (cl:signal-user-error SE5049 (sa:op-name expr)))))

; Returns the rewritten expr
; colldoc - logical representation for document or collection
(define (sa:structural-relative-xpath? expr colldoc)
  (case (sa:op-name expr)
    ((var)
     colldoc)
    ((ddo attr-axis child descendant descendant-or-self self)
     (cons (sa:op-name expr)
           (cons (sa:structural-relative-xpath? (car (sa:op-args expr))
                                                colldoc)
                 (cdr (sa:op-args expr)))))
    (else
     (cl:signal-user-error SE5049 (sa:op-name expr)))))

(define (sa:analyze-index-create expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 4)
   (let ((arg1 (sa:analyze-expr (car (sa:op-args expr))
                                vars funcs ns-binding default-ns))
         (arg2 (sa:analyze-expr (cadr (sa:op-args expr))
                                vars funcs ns-binding default-ns))
         (arg3 (sa:analyze-expr (caddr (sa:op-args expr))
                                (cons
                                 (cons (cadr sa:context-item)
                                       sa:type-nodes)
                                 vars)
                                funcs ns-binding default-ns))
         (arg4 (sa:analyze-seq-type (cadddr (sa:op-args expr))
                                    ns-binding default-ns)))
     (and
      arg1 arg2 arg3 arg4
      (list
       (sa:op-name expr)  ; operation name
       (car arg1)  ; without result type
       (car arg2)
       (sa:structural-relative-xpath?
        (car arg3)
        (sa:structural-absolute-xpath? (car arg2)))
       (car arg4))))))

(define (sa:analyze-index-drop expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 1)
   (let ((new
          (sa:propagate expr vars funcs ns-binding default-ns
                        'sa:atomic  ; dummy
                        )))
      (and new (car new)))))

; Post processing of the function call
(define (sa:fun-call-post-proc pair)
  (let ((expr (car pair)))
    (case (sa:op-name expr)
      ((!fn!index-scan)
       (let ((third (list-ref (sa:op-args expr) 2)))
       (if (member third            
                   '((const (type !xs!string) "GT")
                     (const (type !xs!string) "LT")
                     (const (type !xs!string) "GE")
                     (const (type !xs!string) "LE")
                     (const (type !xs!string) "EQ")))
           pair   ; everything is ok
           (cl:signal-user-error SE5050 third))))
      ((!fn!index-scan-between)
       (let ((fourth (list-ref (sa:op-args expr) 3)))
         (if (member fourth
                     '((const (type !xs!string) "INT")
                       (const (type !xs!string) "SEG")
                       (const (type !xs!string) "HINTL")
                       (const (type !xs!string) "HINTR")))
             pair   ; everything is ok
             (cl:signal-user-error SE5051 fourth))))
      ((!fn!name !fn!namespace-uri !fn!string-length !fn!string
                 !fn!local-name !fn!number)
       (if
        (null? (cdr expr))  ; no argument
        (cons (list (sa:op-name expr)  ; function name
                    sa:context-item  ; adding context item as argument
                    )
              (cdr pair)  ; return type
              )
        pair))
      (else  ; any other function call
       pair))))

;-------------------------------------------------
; Managing full-text index

(define (sa:analyze-full-text-create expr vars funcs ns-binding default-ns)
  (and
   (or
    (let ((lng (length (sa:op-args expr))))
      (and (>= lng 3) (<= lng 4)))
    (sa:assert-num-args expr 3))
   ; Index type is a string constant
   (sa:analyze-string-const (caddr (sa:op-args expr))
                            vars funcs ns-binding default-ns)
   (let ((new
          (sa:propagate expr vars funcs ns-binding default-ns
                        'sa:atomic  ; dummy
                        )))
     (and new (car new)))))

(define (sa:analyze-full-text-drop expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 1)
   (let ((new
          (sa:propagate expr vars funcs ns-binding default-ns
                        'sa:atomic  ; dummy
                        )))
      (and new (car new)))))


;==========================================================================
; Order by

;(ordermodifier 
; (const (type !xs!string) "asc")
; (const (type !xs!string) "empty-greatest"))
(define (sa:analyze-ordermodifier expr vars funcs ns-binding default-ns)
  (cond
    ((not (and (pair? expr) (eq? (sa:op-name expr) 'ordermodifier)))
     (cl:signal-input-error SE5064 expr))
    ((null? (sa:op-args expr))  ; everything by default
     (cons expr sa:type-any))
    (else
     (and
      (or (= (length (sa:op-args expr)) 1)
          (sa:assert-num-args expr 2))
      (let ((c1 (car (sa:op-args expr)))  ; first constant
            (c2 (if (null? (cdr (sa:op-args expr)))  ; a single argument
                    '(const (type !xs!string) "default")
                    (cadr (sa:op-args expr)))))
        (and
         (sa:analyze-string-const c1 '() '() '() sa:default-ns)
         (sa:analyze-string-const c2 '() '() '() sa:default-ns)
         (let ((v1 (caddr c1))  ; value of the first constant
               (v2 (caddr c2)))
           (cond
             ((not (member v1 '("asc" "desc")))
              (cl:signal-input-error SE5061 v1))
             ((not (member v2 '("empty-greatest" "empty-least" "default")))
              (cl:signal-input-error SE5062 v1))
             (else
              (cons expr sa:type-any))))))))))

;(orderspec
; (ordermodifier 
;  (const (type !xs!string) "asc")
;  (const (type !xs!string) "empty-greatest"))
; (*@ (var $x1) (var $x2) (var $x3) (var $x4)))
(define (sa:analyze-orderspec expr vars funcs ns-binding default-ns)
  (if
   (not (and (pair? expr) (eq? (sa:op-name expr) 'orderspec)))
   (cl:signal-input-error SE5065 expr)
   (and
    (sa:assert-num-args expr 2)
    (let ((new-modifier
           (sa:analyze-ordermodifier
            (car (sa:op-args expr)) vars funcs ns-binding default-ns))
          (new-subexpr
           (sa:analyze-expr
            (cadr (sa:op-args expr)) vars funcs ns-binding default-ns)))
      (and
       new-modifier new-subexpr
       (cons (list (sa:op-name expr)  ; ='orderspec
                   (car new-modifier)
                   (car new-subexpr))
             (cdr new-subexpr)))))))

;(orderspecs
; (const (type !xs!string) "non-stable")
; (orderspec ...)
; (orderspec ...))
(define (sa:analyze-multiple-orderspecs expr vars funcs ns-binding default-ns)
  (cond
    ((not (and (pair? expr) (eq? (sa:op-name expr) 'orderspecs)))
     (cl:signal-input-error SE5066 expr))
    ((null? (sa:op-args expr))  ; no stable/non-stable declaration
     (cl:signal-input-error SE5063 expr))
    ((not (and (sa:analyze-string-const
                (car (sa:op-args expr)) '() '() '() sa:default-ns)
               (member
                (caddr (car (sa:op-args expr)))  ; value of the constant
                '("stable" "non-stable"))))
     (cl:signal-input-error SE5063 (car (sa:op-args expr))))
    (else
     (let ((new-orderspec-lst
            (map
             (lambda (sub)
               (sa:analyze-orderspec sub vars funcs ns-binding default-ns))
             (cdr (sa:op-args expr)))))
       (and
        (not (memv #f new-orderspec-lst))
        (cons
         (cons (sa:op-name expr)  ; ='orderspecs
               (cons (car (sa:op-args expr))  ; stable / non-stable
                     (map car new-orderspec-lst)))
         sa:type-any  ; we don't care about the type
         ))))))

(define (sa:analyze-order-by expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 2)
   (let ((new-value
          (sa:analyze-expr (car (sa:op-args expr)) vars funcs ns-binding default-ns))
         (new-fun
          (sa:analyze-fun-def (cadr (sa:op-args expr)) vars funcs ns-binding default-ns)))
     (and
      new-value new-fun
      (let ((fun-body (caddr (car new-fun))))
        (if
         (not (and (pair? fun-body) (eq? (sa:op-name fun-body) 'orderspecs)))
         (cl:signal-input-error SE5066 fun-body)
         #t))
      (cons (list (sa:op-name expr)  ; ='order-by
                  (car new-value)
                  (car new-fun))
            (cdr new-fun))))))
