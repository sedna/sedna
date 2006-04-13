
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
; key = "indent" => result = "yes"
; If key not found, returns #f
(define (sa:extract-suboption str key)
  (cond
   ((assoc key
           (map
            (lambda (pair)
              (cons (sa:remove-boundary-spaces (car pair))
                    (sa:remove-boundary-spaces (cadr pair))))
            (filter
             (lambda (lst) (= (length lst) 2))  ; name-value pairs
             (map
              (lambda (sub) (sa:string-split sub '(#\=)))
              (sa:string-split str '(#\;))))))
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
     ((sequence)
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
     ((document text comment)
      (sa:document-text-comment expr vars funcs ns-binding default-ns))
     ;-------------------
     ; 2.10 FLWOR Operations
     ((let@)
      (sa:analyze-let@ expr vars funcs ns-binding default-ns))
     ((return)
      (sa:analyze-return expr vars funcs ns-binding default-ns))
     ((predicate)
      (sa:analyze-predicate expr vars funcs ns-binding default-ns))
     ;-------------------
     ; 2.11 Quantifiers
     ((some every)
      (sa:some-every expr vars funcs ns-binding default-ns))
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

(define sa:predefined-ns-prefixes
  `(("xml" . "http://www.w3.org/XML/1998/namespace")
    ("xs" . ,sa:xs-ns)
    ("xsi" . "http://www.w3.org/2001/XMLSchema-instance")
    ("fn" . ,sa:fn-ns)
    ("xdt" . "http://www.w3.org/2004/07/xpath-datatypes")
    ("local" . "http://www.w3.org/2004/07/xquery-local-functions")
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
    ; Single-arg atomic function
    (,sa:fn-ns "name" 0 1
     ,(lambda (num-args) (sa:make-list sa:type-nodes num-args))
     ,sa:type-atomic !fn!name)
    (,sa:fn-ns "document-uri" 0 1
     ,(lambda (num-args) (sa:make-list sa:type-nodes num-args))
     ,sa:type-atomic !fn!document-uri)
    (,sa:fn-ns "node-name" 0 1
     ,(lambda (num-args) (sa:make-list sa:type-nodes num-args))
     ,sa:type-atomic !fn!node-name)
    (,sa:fn-ns "node-kind" 0 1
     ,(lambda (num-args) (sa:make-list sa:type-nodes num-args))
     ,sa:type-atomic !fn!node-kind)
    (,sa:fn-ns "namespace-uri" 0 1
     ,(lambda (num-args) (sa:make-list sa:type-nodes num-args))
     ,sa:type-atomic !fn!namespace-uri)
    (,sa:fn-ns "not" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic !fn!not)
    (,sa:fn-ns "empty" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic !fn!empty)
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
    (,sa:fn-ns "string" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic !fn!string)
    (,sa:fn-ns "contains" 2 2
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic !fn!contains)
    (,sa:fn-ns "translate" 3 3
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic !fn!translate)
    (,sa:fn-ns "replace" 3 4
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic !fn!replace)
    (,sa:fn-ns "matches" 2 3
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic !fn!matches)
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
    ; Legacy
    (,sa:fn-ns "item-at" 2 2
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-atomic !fn!item-at)
    (,sa:fn-ns "test" 0 #f
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-any !fn!test)
    (,sa:fn-ns "exists" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))     
     ,sa:type-atomic)    
    (,sa:fn-ns "local-name" 0 1
     ,(lambda (num-args) (sa:make-list sa:type-nodes num-args))
     ,sa:type-atomic)
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
                 (cl:signal-user-error SE5007 body))))))))
     (let ((expr (car prolog)))
       (case (sa:op-name expr)
         ((boundary-space-decl)  ; Boundary space
          (and
           (sa:assert-num-args expr 1)
           (sa:analyze-string-const (cadr expr) '() '() '() sa:default-ns)
           (if
            (member (cadr expr)  ; predefined values
                    '((const (type !xs!string) "strip")
                      (const (type !xs!string) "preserve")))
            (loop (cons expr new-prlg)
                  funcs triples
                  ns-binding default-elem-ns default-func-ns
                  (cdr prolog))
            (cl:signal-user-error SE5054 (cadr expr)))))
         ((declare-option)
          (and
           (sa:assert-num-args expr 2)
           (sa:proper-qname (cadr expr))  ; must be proper qname
           (sa:analyze-string-const (caddr expr) '() '() '() sa:default-ns)
           (let ((name 
                  (sa:resolve-qname (cadr expr) ns-binding sa:se-ns))
                 (value (caddr (caddr expr))))
             (if
              (not
               (equal? name
                       `(const (type !xs!QName) (,sa:se-ns "output"))))
              (cl:signal-user-error
               SE5055
               (sa:qname->string (cadr expr)) ", expanded to "
               (sa:qname->string name))
              (loop
               (cons
                (cons
                 (car expr)  ; declare-option
                 (cons
                  name
                  (filter
                   (lambda (x) x)
                   (map
                    (lambda (sub)
                      (cond
                        ((sa:extract-suboption value sub)
                         => (lambda (v)
                              `((const (type !xs!string) ,sub)
                                (const (type !xs!string) ,v))))
                        (else #f)))
                    '("method" "indent")))))
                new-prlg)
               funcs triples
               ns-binding default-elem-ns default-func-ns
               (cdr prolog))))))
         ((declare-namespace)
          (and
           (sa:assert-num-args expr 2)
           (sa:analyze-string-const (caddr expr) '() '() '() sa:default-ns)
           (if
            (not (symbol? (cadr expr)))  ; prefix
            (cl:signal-user-error SE5008 (cadr expr))
            (let ((prefix (symbol->string (car (sa:op-args expr))))
                  (ns-uri (cadr (sa:op-args
                                 (cadr (sa:op-args expr))))))
              (if
               (or
                (member prefix   ; can redefine
                        '("xs" "xsi" "fn" "xdt" "local" "se"))
                (not (assoc prefix ns-binding)))
               (loop (cons expr new-prlg)
                     funcs triples
                     (cons (cons prefix ns-uri) ns-binding)
                     default-elem-ns default-func-ns
                     (cdr prolog))
               (cl:signal-user-error SE5009 prefix))))))
         ((declare-default-element-namespace)
          (and
           (sa:assert-num-args expr 1)
           (sa:analyze-string-const (cadr expr) '() '() '() sa:default-ns)
           (if
            default-elem-ns  ; default prefix already declared
            (cl:signal-user-error SE5010)
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
            (cl:signal-user-error SE5011)
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
                   (let ((arg-types (map cdr formal-args)))
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
              (loop
               (cons #f new-prlg)  ; new prolog
               (let ((qname-parts (sa:proper-qname fun-qname)))
                 (cons   ; funcs
                  (list                   
                   (car qname-parts) (cadr qname-parts)  ; name
                   (length formal-args) (length formal-args) ; min and max args
                   (let ((arg-types (map cdr formal-args)))
                     (lambda (num-args) arg-types))
                   return-type)
                  funcs))
               (cons  ; triples
                (list
                 (list (sa:op-name expr)  ; ='declare-function
                       fun-qname
                       (cadr (sa:op-args expr))
                       (caddr (sa:op-args expr))
                       (cadddr (sa:op-args expr)))
                 formal-args return-type)
                triples)
               ns-binding default-elem-ns default-func-ns
               (cdr prolog))))))
         (else
          (cl:signal-input-error SE5012 expr)))))))

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
                ((string=? prefix "")   ; no prefix supplied
                 default-ns)
                ((assoc prefix ns-binding)
                 => cdr)
                (else
                 (cl:signal-user-error
                  SE5013 prefix ":" (cadr name-parts))))))
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
     (cons type-spec
           (if (memq type-spec '(xs:anyType !xs!anyType))
               sa:type-nodes sa:type-atomic)))
    ((and (pair? type-spec) (= (length type-spec) 2)
          (string? (car type-spec)) (string? (cadr type-spec)))
     ; external atomic type
     (cons type-spec sa:type-atomic))
    ((eq? (car type-spec) 'doc-test)
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
    ((memq (car type-spec) '(elem-test attr-test))
     (and
      (sa:assert-num-args type-spec 1)
      (let ((new-ename (sa:analyze-ename
                        (car (sa:op-args type-spec)) ns-binding default-ns)))
        (and new-ename
             (cons (list (car type-spec)
                         (car new-ename))
                   (cdr new-ename))))))
    ((memq (car type-spec) '(comment-test text-test node-test))
     (and (sa:assert-num-args type-spec 0)
          (cons type-spec sa:type-nodes)))
    ((eq? (car type-spec) 'item-test)
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
           (car (sa:op-args expr)))
          ; DL: was - expansion:
          ;(sa:resolve-qname (car (sa:op-args expr)) ns-binding default-ns)
          )
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
;  vars ::= (listof (cons var-name var-type))
;  var-name ::= Scheme symbol
;  var-type ::= 'sa:atomic | 'sa:nodes
(define (sa:analyze-formal-args arg-lst ns-binding default-ns)
  (if
   (or (not (list? arg-lst))
       (memv #f (map pair? arg-lst)))  ; not a proper arg-lst
   (cl:signal-input-error SE5019 arg-lst)
   (let ((vars
          (map
           (lambda (pair)
             (let ((type (sa:analyze-seq-type
                          (car pair) ns-binding default-ns)))
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
                     (cons var-name (cdr type)))))
                 (else
                  (cl:signal-input-error SE5020 (cadr pair))))))
           arg-lst)))
     (if (memv #f vars) #f vars))))

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
      (cl:signal-user-error SE5024 (car var-name))))))

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
        ((equal? var-name '("" "$%v"))
         (cl:signal-user-error SE5053))
        (else
         (cl:signal-user-error
          SE5025
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
         (if (number? value)
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
   (let ((a (sa:analyze-expr (car (sa:op-args expr))
                             vars funcs ns-binding default-ns))
         (new-type (sa:analyze-type
                    (cadr (sa:op-args expr))
                    vars funcs ns-binding default-ns)))
     (and
      a new-type
      (if
       (eq? (cdr a) sa:type-atomic)        
       (cl:signal-user-error SE5029 expr)
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
     (cons (cons (sa:op-name expr)  ; ='sequence
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
        (cl:signal-user-error SE5052 expr))
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
           (lambda (subexpr) (sa:analyze-expr subexpr vars funcs ns-binding default-ns))
           (sa:op-args expr))))
     (cond
       ((member #f args-res)  ; error detected
        #f)
       ((memq sa:type-atomic args-res)  ; at least one argument is atomic
        (cl:signal-user-error SE5031 expr))
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

; Element constructor
(define (sa:analyze-element-constructor expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 2)
   (let* ((new-xmlns
           (sa:xmlns-declarations-in-element expr ns-binding default-ns))
          (ns-binding (car new-xmlns))
          (default-ns (cdr new-xmlns)))
     (if
      (sa:qname-const? (car (sa:op-args expr)))  ; name is constant
      (and
       (sa:resolve-qname
        (car (sa:op-args expr)) ns-binding (cadr default-ns))
       (sa:propagate expr vars funcs ns-binding default-ns sa:type-nodes))
      (sa:propagate expr vars funcs ns-binding default-ns sa:type-nodes)))))
     
; Constructors for attribute, pi and namespace
; TODO: more sophisticated treatment for pi and namespace names is desirable
; QUESTION: how many arguments constructors must have?
(define (sa:attribute-pi-namespace
         expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 2)
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
          (sa:analyze-fun-def (cadr (sa:op-args expr)) vars funcs ns-binding default-ns)))
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
        ((> (length formal-args) 3)
         (cl:signal-input-error SE5033 expr))
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
                    (lambda (pair)
                      (list
                       (car pair)  ; argument type
                       (list
                        (caadr pair)   ; = 'var
                        (sa:expand-var-name
                         (cadadr pair)  ; var-name
                         ns-binding (car default-ns)))))
                    (car (sa:op-args expr)))
                   (car expr-pair))
             (cdr expr-pair))))))))))

;-------------------------------------------------
; 2.11 Quantifiers

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
                      (car args) ns-binding (cadr default-ns)))
           (actual-args
            (map
             (lambda (subexpr)
               (sa:analyze-expr
                subexpr vars funcs ns-binding default-ns))
             (cdr args))))
       (and
        fun-name
        (not (memv #f actual-args))  ; error detected in arguments
        (let ((name-parts (sa:proper-qname fun-name)))
          (let loop ((fs funcs))
            (cond
              ((null? fs)  ; all functions scanned
               (cl:signal-user-error SE5037
                                     (cadr (caddr  ; extract function name
                                            (car (sa:op-args expr))))
                                     ; expr
                                     ))
              ((and (string=? (caar fs) (car name-parts))
                    (string=? (cadar fs) (cadr name-parts)))
               (let ((fun-declaration (car fs))
                     (num-actual (length actual-args)))
                 (if
                  (or (< num-actual (list-ref fun-declaration 2))
                      (and (list-ref fun-declaration 3)  ; max-args
                           (> num-actual (list-ref fun-declaration 3))))
                  (cl:signal-user-error SE5038
                                        (car name-parts) ":" (cadr name-parts))
                  (let ((formal-args
                         ((list-ref fun-declaration 4) num-actual)))
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
                         (cl:signal-user-error SE5039 expr))
                        (else
                         (rpt (cdr form) (cdr act)))))))))
              (else
               (loop (cdr fs)))))))))))

(define (sa:analyze-cast expr vars funcs ns-binding default-ns)
  (and
   (sa:assert-num-args expr 2)
   (let ((args (list (sa:analyze-expr (car (sa:op-args expr)) vars funcs ns-binding default-ns)
                     (sa:analyze-type (cadr (sa:op-args expr)) vars funcs ns-binding default-ns))))
     (if (memv #f args)
         #f
         (cons (cons (sa:op-name expr)
                     (map car args))
               (cdar args))))))


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
                                 `(("" "$%v") . ,sa:type-nodes)
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
