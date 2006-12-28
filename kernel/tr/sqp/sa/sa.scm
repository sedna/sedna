; File:  sa.scm
; Copyright (C) 2004 The Institute for System Programming
; of the Russian Academy of Sciences (ISP RAS)

;; Static Analysis for XQuery logical representation
; Primary actions:
;  - Semantic analysis of the query
;  - Resolution of qualified names
; Prefix for this module is "sa:"
(declare (unit sa) (uses common-lib scm-error-codes))

(declare (foreign-declare "const char* c_get_module(const char*);"))
(define get-module (foreign-callback-lambda c-string* "c_get_module" c-string))

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
       
; Whether a given string is a proper XML EncName
; [81]    EncName    ::=    [A-Za-z] ([A-Za-z0-9._] | '-')* 
(define (sa:proper-EncName? str)
  (and
   (> (string-length str) 0)
   (or
    (and
     (char>=? (string-ref str 0) #\A)
     (char<=? (string-ref str 0) #\Z))
    (and
     (char>=? (string-ref str 0) #\a)
     (char<=? (string-ref str 0) #\z)))
   (let loop ((i 1))
     (or
      (= i (string-length str))
      (and
       (or
        (and
         (char>=? (string-ref str i) #\A)
         (char<=? (string-ref str i) #\Z))
        (and
         (char>=? (string-ref str i) #\a)
         (char<=? (string-ref str i) #\z))
        (and
         (char>=? (string-ref str i) #\0)
         (char<=? (string-ref str i) #\9))
        (char=? (string-ref str i) #\.)
        (char=? (string-ref str i) #\_)
        (char=? (string-ref str i) #\-))
       (loop (+ i 1)))))))
        

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
    (let ((prolog-res (sa:analyze-prolog
                       (sa:get-query-prolog query) '() #f)))
      (and
       prolog-res  ; processed correctly
       (let* ((new-prolog (car prolog-res))
              (funcs      (cadr prolog-res))
              (ns-binding (caddr prolog-res))
              (default-ns (cadddr prolog-res))
              (vars       (list-ref prolog-res 4))
              (modules    (list-ref prolog-res 5)))
         (cond           
           ((eq? (sa:op-name query) 'query)
            (let ((pair   ; = (new-query . type)
                   (sa:analyze-expr
                    (sa:get-query-body query)
                    vars funcs ns-binding default-ns #f modules)))
              (and
               pair
               `(query
                 ,@(map
                    (lambda (quad)
                      (cons 'module
                            (sa:module->prolog (cadr quad))))
                    modules)
                 (prolog ,@new-prolog)
                 (query-body ,(car pair))))))
           ((assq (sa:op-name query)
                  `((update . ,sa:analyze-update)
                    (manage . ,sa:analyze-manage)
                    (retrieve-metadata . ,sa:analyze-retrieve-metadata)))
            => (lambda (pair)
                 (let ((new ((cdr pair)
                             (cadr (sa:op-args query))
                             vars funcs ns-binding default-ns #f modules)))
                   (and new
                        (cons (sa:op-name query)
                              (append
                               (map
                                (lambda (quad)
                                  (cons 'module
                                        (sa:module->prolog (cadr quad))))
                                modules)
                               (list
                                (cons 'prolog new-prolog)
                                new)))))))
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
;  uri ::= string | #f
;   For a main module, `uri' == #f.
;   For a library module, `uri' contains the URI of this module.
;
;  modules ::= (listof (list uri module vars funcs))
;              | (listof (list uri))
;   For a main module, `modules' contain the detailed information for each
;   imported module, including module body, declared vars and funcs.
;   For a library module, `modules' contain just imported modules' URIs.
;
; Returns
;  either (cons new-query var-type)
;  or #f - semantic error detected - the message is displayed as a side effect
(define (sa:analyze-expr expr vars funcs ns-binding default-ns uri modules)
  (if
   (not (pair? expr))
   (cl:signal-input-error SE5005 expr)
   ;(sa:var? expr)
   ; (sa:analyze-variable expr vars funcs ns-binding default-ns uri modules)
   (case (sa:op-name expr)  ; operation name
     ; new representation for variables     
     ((var)
      (sa:variable-wrapped expr vars funcs ns-binding default-ns uri modules))
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
      (sa:analyze-axis expr vars funcs ns-binding default-ns uri modules))
     ;-------------------
     ; 2.4 Sequence
     ((sequence space-sequence unio)
      (sa:analyze-sequence expr vars funcs ns-binding default-ns uri modules))
     ;-------------------
     ; 2.5 Arithmetic operations
     ((+@ -@ *@ div@ idiv@ mod@ /@ to@)
      (sa:binary-arithmetic expr vars funcs ns-binding default-ns uri modules))
     ((unary+@ unary-@)
      (sa:unary-arithmetic expr vars funcs ns-binding default-ns uri modules))
     ;-------------------
     ; 2.6 Comparison operations
     ((eq@ ne@ lt@ le@ gt@ ge@ =@ !=@ <@ <=@ >@ >=@)
      (sa:analyze-comparison expr vars funcs ns-binding default-ns uri modules))
     ((is@ <<@ >>@)
      (sa:analyze-node-comparison expr vars funcs ns-binding default-ns uri modules))
     ;-------------------
     ; 2.7 Conditional operation
     ((if if@)
      (sa:analyze-if expr vars funcs ns-binding default-ns uri modules))
     ;-------------------
     ; 2.8 Logical operations
     ;((not@)
     ; (sa:analyze-not@ expr vars funcs ns-binding default-ns uri modules))
     ((and@ or@)
      (sa:analyze-and@-or@ expr vars funcs ns-binding default-ns uri modules))
     ;-------------------
     ; 2.9 Constructors
     ((element)
      (sa:analyze-element-constructor expr vars funcs ns-binding default-ns uri modules))
     ((attribute namespace)
      (sa:attribute-namespace expr vars funcs ns-binding default-ns uri modules))
     ((pi)
      (sa:pi-constructor expr vars funcs ns-binding default-ns uri modules))
     ((document text comment)
      (sa:document-text-comment expr vars funcs ns-binding default-ns uri modules))
     ;-------------------
     ; 2.10 FLWOR Operations
     ((let@)
      (sa:analyze-let@ expr vars funcs ns-binding default-ns uri modules))
     ((return)
      (sa:analyze-return expr vars funcs ns-binding default-ns uri modules))
     ((predicate)
      (sa:analyze-predicate expr vars funcs ns-binding default-ns uri modules))
     ((order-by)
      (sa:analyze-order-by expr vars funcs ns-binding default-ns uri modules))
     ((orderspecs)
      (sa:analyze-multiple-orderspecs expr vars funcs ns-binding default-ns uri modules))
     ;-------------------
     ; 2.11 Expressions on Sequence Types
     ((ts)
      (sa:analyze-typeswitch expr vars funcs ns-binding default-ns uri modules))
     ((treat instance-of)
      (sa:analyze-treat expr vars funcs ns-binding default-ns uri modules))
     ((cast)
      (sa:analyze-cast expr vars funcs ns-binding default-ns uri modules))
     ((castable)
      (sa:analyze-castable expr vars funcs ns-binding default-ns uri modules))
     ;-------------------
     ; XQuery 3.14 Extension Expressions
     ((extension-expr)
      (sa:analyze-extension-expression expr vars funcs ns-binding default-ns uri modules))
     ;-------------------
     ; 2.14 Distinct document order
     ((ddo)
      (sa:analyze-ddo expr vars funcs ns-binding default-ns uri modules))
     ((ordered unordered)
      (sa:ordered-unordered expr vars funcs ns-binding default-ns uri modules))
     ;-------------------
     ; 3.3. XPath
     ((congen1)
      (sa:analyze-congen1 expr vars funcs ns-binding default-ns uri modules))
     ((congen2)
      (sa:analyze-congen2 expr vars funcs ns-binding default-ns uri modules))
     ;-------------------
     ; 3.6. Quantified expressions
     ((some every)
      (sa:some-every expr vars funcs ns-binding default-ns uri modules))
     ;-------------------
     ; 3.7 XQuery 1.0 Functions
     ((!fn!document !fn!collection)
      (sa:document-collection expr vars funcs ns-binding default-ns uri modules))
     ((!fn!position !fn!last !fn!true !fn!false)
      (sa:position-last-true-false expr vars funcs ns-binding default-ns uri modules))
     ((!fn!node-name !fn!node-kind !fn!name !fn!not !fn!empty !fn!count
                     !fn!error !fn!document-uri)
      (sa:basic-singlearg-atomic expr vars funcs ns-binding default-ns uri modules))
     ((!fn!sum !fn!avg !fn!max !fn!min !fn!contains !fn!translate
               !fn!distinct-values !fn!concat !fn!string-value !fn!typed-value)
      (sa:basic-multiarg-atomic expr vars funcs ns-binding default-ns uri modules))
     ((!fn!replace)
      (sa:basic-replace expr vars funcs ns-binding default-ns uri modules))
     ;-------------------
     ; Union operations
     ((union@ intersect@ except@)
      (sa:analyze-union-intersect expr vars funcs ns-binding default-ns uri modules))
     ;-------------------
     ; Not expressed in the new logical representation
     ((fun-call)
      (sa:analyze-fun-call expr vars funcs ns-binding default-ns uri modules))
     ((spaceseq)
      (sa:propagate expr vars funcs ns-binding default-ns uri modules 'sa:atomic))
     ;-------------------     
     (else  ; unknown operations
      (cl:signal-input-error SE5006 expr)))))

; Propagates semantic analysis to subexpressions
;  return - what type the operation should return if arguments are ok
(define (sa:propagate expr vars funcs ns-binding default-ns uri modules return)
  (let ((lst
         (map
          (lambda (subexpr)
            (sa:analyze-expr subexpr vars funcs ns-binding default-ns uri modules))
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

; Helpers for creating lists of 'sa:atomic or 'sa:nodes values
(define (sa:no-arguments num-args) '())
(define (sa:multiple-atomic num-args)
  (sa:make-list sa:type-atomic num-args))
(define (sa:multiple-nodes num-args)
  (sa:make-list sa:type-nodes num-args))
(define (sa:multiple-any num-args)
  (sa:make-list sa:type-any num-args))
(define (sa:atomic-and-node num-args)
  (if (= num-args 1)
      (list sa:type-atomic)
      (list sa:type-atomic sa:type-nodes)))

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
(define sa:xquery-functions
  (append
  `(;----------------------------------------
    ; 2 Accessors
    (,sa:fn-ns "node-name" 1 1
     ,sa:multiple-nodes
     ,sa:type-atomic !fn!node-name)
    (,sa:fn-ns "nilled" 1 1
     ,sa:multiple-nodes
     ,sa:type-atomic !fn!nilled)
    (,sa:fn-ns "string" 0 1
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!string)
    (,sa:fn-ns "data" 1 1
     ,sa:multiple-any
     ,sa:type-atomic !fn!data)
    (,sa:fn-ns "base-uri" 0 1
     ,sa:multiple-nodes
     ,sa:type-atomic !fn!base-uri)
    (,sa:fn-ns "document-uri" 1 1
     ,sa:multiple-nodes
     ,sa:type-atomic !fn!document-uri)
    ;----------------------------------------
    ; 3 The Error Function
    (,sa:fn-ns "error" 0 3
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!error)
    ;----------------------------------------
    ; 4 The Trace Function
    (,sa:fn-ns "trace" 2 2
     ,sa:multiple-any
     ,sa:type-any !fn!trace)
    ;----------------------------------------
    ; 5 Constructor Functions
    ; Entries for most of constructor functions are presented in the end
    ; of sa:xquery-functions definition
    (,sa:fn-ns "dateTime" 2 2
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!dateTime)
    ;----------------------------------------
    ; 6 Functions and Operators on Numerics
    ; 6.4 Functions on Numeric Values
    (,sa:fn-ns "abs" 1 1
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!abs)
    (,sa:fn-ns "ceiling" 1 1
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!ceiling)
    (,sa:fn-ns "floor" 1 1
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!floor)
    (,sa:fn-ns "round" 1 1
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!round)
    (,sa:fn-ns "round-half-to-even" 1 2
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!round-half-to-even)
    ;----------------------------------------
    ; 7 Functions on Strings
    ; *** 7.2 Functions to Assemble and Disassemble Strings
    (,sa:fn-ns "codepoints-to-string" 1 1
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!codepoints-to-string)
    (,sa:fn-ns "string-to-codepoints" 1 1
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!string-to-codepoints)
    ; *** 7.3 Equality and Comparison of Strings
    (,sa:fn-ns "compare" 2 3
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!compare)
    (,sa:fn-ns "codepoint-equal" 2 2
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!codepoint-equal)
    ; *** 7.4 Functions on String Values
    (,sa:fn-ns "concat" 2 #f
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!concat)
    (,sa:fn-ns "string-join" 2 2
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!string-join)
    (,sa:fn-ns "substring" 2 3
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!substring)
    (,sa:fn-ns "string-length" 0 1
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!string-length)
    (,sa:fn-ns "normalize-space" 0 1
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!normalize-space)
    (,sa:fn-ns "normalize-unicode" 1 2
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!normalize-unicode)
    (,sa:fn-ns "upper-case" 1 1
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!upper-case)
    (,sa:fn-ns "lower-case" 1 1
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!lower-case)
    (,sa:fn-ns "translate" 3 3
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!translate)
    (,sa:fn-ns "encode-for-uri" 1 1
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!encode-for-uri)
    (,sa:fn-ns "iri-to-uri" 1 1
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!iri-to-uri)
    (,sa:fn-ns "escape-html-uri" 1 1
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!escape-html-uri)
    ; *** 7.5 Functions Based on Substring Matching
    (,sa:fn-ns "contains" 2 3
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!contains)
    (,sa:fn-ns "starts-with" 2 3
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!starts-with)
    (,sa:fn-ns "ends-with" 2 3
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!ends-with)
    (,sa:fn-ns "substring-before" 2 3
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!substring-before)
    (,sa:fn-ns "substring-after" 2 3
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!substring-after)
    ; *** 7.6 String Functions that Use Pattern Matching
    (,sa:fn-ns "matches" 2 3
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!matches)
    (,sa:fn-ns "replace" 3 4
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!replace)
    (,sa:fn-ns "tokenize" 2 3
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!tokenize)
    ;----------------------------------------
    ; 8 Functions on anyURI
    (,sa:fn-ns "resolve-uri" 1 2
     ,sa:multiple-atomic
     ,sa:type-nodes !fn!resolve-uri)
    ;----------------------------------------    
    ; 9 Functions and Operators on Boolean Values
    (,sa:fn-ns "true" 0 0
     ,sa:no-arguments
     ,sa:type-atomic !fn!true)
    (,sa:fn-ns "false" 0 0
     ,sa:no-arguments
     ,sa:type-atomic !fn!false)
    (,sa:fn-ns "not" 1 1
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!not)
    ;----------------------------------------
    ; 10 Functions and Operators on Durations, Dates and Times
    ; 10.5 Component Extraction Functions on Durations, Dates and Times
    (,sa:fn-ns "years-from-duration" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!years-from-duration)
    (,sa:fn-ns "months-from-duration" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!months-from-duration)
    (,sa:fn-ns "days-from-duration" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!days-from-duration)
    (,sa:fn-ns "hours-from-duration" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!hours-from-duration)
    (,sa:fn-ns "minutes-from-duration" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!minutes-from-duration)
    (,sa:fn-ns "seconds-from-duration" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!seconds-from-duration)
    (,sa:fn-ns "year-from-dateTime" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!year-from-dateTime)
    (,sa:fn-ns "month-from-dateTime" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!month-from-dateTime)
    (,sa:fn-ns "day-from-dateTime" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!day-from-dateTime)
    (,sa:fn-ns "hours-from-dateTime" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!hours-from-dateTime)
    (,sa:fn-ns "minutes-from-dateTime" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!minutes-from-dateTime)
    (,sa:fn-ns "seconds-from-dateTime" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!seconds-from-dateTime)
    (,sa:fn-ns "timezone-from-dateTime" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!timezone-from-dateTime)
    (,sa:fn-ns "year-from-date" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!year-from-date)
    (,sa:fn-ns "month-from-date" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!month-from-date)
    (,sa:fn-ns "day-from-date" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!day-from-date)
    (,sa:fn-ns "timezone-from-date" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!timezone-from-date)
    (,sa:fn-ns "hours-from-time" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!hours-from-time)
    (,sa:fn-ns "minutes-from-time" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!minutes-from-time)
    (,sa:fn-ns "seconds-from-time" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!seconds-from-time)
    (,sa:fn-ns "timezone-from-time" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!timezone-from-time)
    ; *** 10.7 Timezone Adjustment Functions on Dates and Time Values
    (,sa:fn-ns "adjust-dateTime-to-timezone" 1 2
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!adjust-dateTime-to-timezone)
    (,sa:fn-ns "adjust-date-to-timezone" 1 2
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!adjust-date-to-timezone)
    (,sa:fn-ns "adjust-time-to-timezone" 1 2
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!adjust-time-to-timezone)
    ;----------------------------------------
    ; 11 Functions Related to QNames
    (,sa:fn-ns "resolve-QName" 2 2
     ,(lambda (num-args) (list sa:type-atomic sa:type-nodes))
     ,sa:type-atomic !fn!resolve-QName)
    (,sa:fn-ns "QName" 2 2
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!QName)
    (,sa:fn-ns "prefix-from-QName" 1 1
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!prefix-from-QName)
    (,sa:fn-ns "local-name-from-QName" 1 1
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!local-name-from-QName)
    (,sa:fn-ns "namespace-uri-from-QName" 1 1
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!namespace-uri-from-QName)
    (,sa:fn-ns "namespace-uri-for-prefix" 2 2
     ,sa:atomic-and-node
     ,sa:type-atomic !fn!namespace-uri-for-prefix)
    (,sa:fn-ns "in-scope-prefixes" 1 1
     ,sa:multiple-atomic
     ,sa:type-atomic !fn!in-scope-prefixes)
    ;----------------------------------------
    ; 14 Functions and Operators on Nodes
    (,sa:fn-ns "name" 0 1
      ,sa:multiple-nodes
      ,sa:type-atomic !fn!name)
    (,sa:fn-ns "local-name" 0 1
      ,sa:multiple-nodes
      ,sa:type-atomic !fn!local-name)
    (,sa:fn-ns "namespace-uri" 0 1
      ,sa:multiple-nodes
      ,sa:type-atomic !fn!namespace-uri)
    (,sa:fn-ns "number" 0 1
      ,sa:multiple-any
      ,sa:type-atomic !fn!number)
    (,sa:fn-ns "lang" 1 2
      ,sa:atomic-and-node
      ,sa:type-atomic !fn!lang)
    (,sa:fn-ns "root" 0 1
      ,sa:multiple-nodes
      ,sa:type-nodes !fn!root)
    ;----------------------------------------
    ; 15 Functions and Operators on Sequences
    ; *** 15.1 General Functions and Operators on Sequences
    (,sa:fn-ns "boolean" 1 1
      ,sa:multiple-any
      ,sa:type-atomic !fn!boolean)
    (,sa:fn-ns "index-of" 2 3
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!index-of)
    (,sa:fn-ns "empty" 1 1
      ,sa:multiple-any
      ,sa:type-atomic !fn!empty)
    (,sa:fn-ns "exists" 1 1
      ,sa:multiple-any
      ,sa:type-atomic !fn!exists)
    (,sa:fn-ns "distinct-values" 1 2
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!distinct-values)
    (,sa:fn-ns "insert-before" 3 3
      ,(lambda (num-args) (list sa:type-any sa:type-atomic sa:type-any))
      ,sa:type-any !fn!insert-before)
    (,sa:fn-ns "remove" 2 2
      ,(lambda (num-args) (list sa:type-any sa:type-atomic))
      ,sa:type-any !fn!remove)
    (,sa:fn-ns "reverse" 1 1
      ,sa:multiple-any
      ,sa:type-any !fn!reverse)
    (,sa:fn-ns "subsequence" 2 3
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!subsequence)
    (,sa:fn-ns "unordered" 1 1
      ,sa:multiple-any
      ,sa:type-any !fn!unordered)
    ; *** 15.2 Functions That Test the Cardinality of Sequences
    (,sa:fn-ns "zero-or-one" 1 1
     ,sa:multiple-any
     ,sa:type-any !fn!zero-or-one)
    (,sa:fn-ns "one-or-more" 1 1
     ,sa:multiple-any
     ,sa:type-any !fn!one-or-more)
    (,sa:fn-ns "exactly-one" 1 1
     ,sa:multiple-any
     ,sa:type-any !fn!exactly-one)
    ; *** 15.3 Equals, Union, Intersection and Except
    (,sa:fn-ns "deep-equal" 2 3
      ,sa:multiple-any
      ,sa:type-atomic !fn!deep-equal)
    ; *** 15.4 Aggregate Functions
    (,sa:fn-ns "count" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!count)
    (,sa:fn-ns "avg" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!avg)
    (,sa:fn-ns "max" 1 2
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!max)
    (,sa:fn-ns "min" 1 2
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!min)
    (,sa:fn-ns "sum" 1 2
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!sum)
    ; *** 15.5 Functions and Operators that Generate Sequences
    (,sa:fn-ns "id" 1 2
      ,sa:atomic-and-node
      ,sa:type-nodes !fn!id)
    (,sa:fn-ns "idref" 1 2
      ,sa:atomic-and-node
      ,sa:type-nodes !fn!idref)
    (,sa:fn-ns "doc" 1 2
      ,sa:multiple-atomic
      ,sa:type-nodes !fn!document)
    (,sa:fn-ns "doc-available" 1 1
      ,sa:multiple-atomic
      ,sa:type-atomic !fn!doc-available)
    (,sa:fn-ns "collection" 1 1
      ,sa:multiple-atomic
      ,sa:type-nodes !fn!collection)
    ;----------------------------------------
    ; 16 Context Functions
    (,sa:fn-ns "position" 0 0
      ,sa:no-arguments
      ,sa:type-atomic !fn!position)
    (,sa:fn-ns "last" 0 0
      ,sa:no-arguments
      ,sa:type-atomic !fn!last)
    (,sa:fn-ns "current-dateTime" 0 0
      ,sa:no-arguments
      ,sa:type-atomic !fn!current-dateTime)
    (,sa:fn-ns "current-date" 0 0
      ,sa:no-arguments
      ,sa:type-atomic !fn!current-date)
    (,sa:fn-ns "current-time" 0 0
      ,sa:no-arguments
      ,sa:type-atomic !fn!current-time)
    (,sa:fn-ns "implicit-timezone" 0 0
      ,sa:no-arguments
      ,sa:type-atomic !fn!implicit-timezone)
    (,sa:fn-ns "default-collation" 0 0
      ,sa:no-arguments
      ,sa:type-atomic !fn!default-collation)
    (,sa:fn-ns "static-base-uri" 0 0
      ,sa:no-arguments
      ,sa:type-atomic !fn!static-base-uri)
   

   ;----------------------------------------
   (,sa:xs-ns "yearMonthDuration" 1 1
     ,(lambda (num-args) (list sa:type-any))
     ,sa:type-atomic
     (cast !xs!yearMonthDuration))
   (,sa:xs-ns "dayTimeDuration" 1 1
     ,(lambda (num-args) (list sa:type-any))
     ,sa:type-atomic
     (cast !xs!dayTimeDuration))
    
    
    
    ;----------------------------------------
    ; Document and collection
    ; this one for backward compatiblity
    (,sa:fn-ns "document" 1 2
     ,(lambda (num-args) (sa:make-list sa:type-atomic num-args))
     ,sa:type-nodes !fn!document)

    ;----------------------------------------
    ; URI-related functions
    
    
    ;----------------------------------------

    ;----------------------------------------
    ; Single-arg atomic function
    
    (,sa:fn-ns "node-kind" 1 1
     ,(lambda (num-args) (sa:make-list sa:type-nodes num-args))
     ,sa:type-atomic !fn!node-kind)    
    
    ;----------------------------------------
    
    
    ;----------------------------------------
    ; Multiarg atomic functions
    
    ;----------------------------------------
    ; String functions
    
    (,sa:fn-ns "string-value" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic !fn!string-value)
    (,sa:fn-ns "typed-value" 1 1
     ,(lambda (num-args) `(,sa:type-atomic))
     ,sa:type-atomic !fn!typed-value)
    
    
    
    
    
    ;----------------------------------------
    ; XML Date/Time functions
    
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
     ; "NOTATION"  ; See "3.12.5 Constructor Functions"
     "normalizedString"
     "token"
     "language"
     "NMTOKEN"
     ;"NMTOKENS"
     "Name"
     "NCName"
     "ID"
     "IDREF"
     ;"IDREFS"
     "ENTITY"
     ;"ENTITIES"
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
; Otherwise, returns: (list new-prolog funcs ns-binding default-ns modules)
;  new-prolog - modified logical representation for prolog
;  ns-binding ::= (listof (cons prefix ns-URI))
;  default-ns ::= (list default-element-ns default-function-ns)
(define (sa:analyze-prolog prolog ns-binding uri)
  ; new-prlg contains #f members at the places of function declarationss
  ;; triples ::= (listof (fun-body formal-args return-type))
  ; triples ::= (listof (declare-function formal-args return-type))
  ; Stub
  (let loop ((new-prlg '())
             (funcs sa:xquery-functions)
             (triples '())
             (ns-binding (append ns-binding sa:predefined-ns-prefixes))
             (default-elem-ns #f)
             (default-func-ns #f)
             (modules '())
             (vars '())
             (prolog prolog))
    (if
     (null? prolog)  ; prolog viewed
     (let ((default-ns (list (or default-elem-ns "")
                             (sa:default-func-ns->string default-func-ns))))
       ; new-prlg and triples must be reversed, since global-vars are added
       ; to static context in the order of their declaration
       (let rpt ((new-prlg (reverse new-prlg))
                 (triples (reverse triples))
                 (prolog-res '())
                 (variables vars))
         (cond
           ((null? new-prlg)  ; all function bodies checked
            (and
             (sa:no-recursive-dependendencies?
              prolog-res  ; the order of prolog members is not important here
              )
             (list (reverse prolog-res)
                   funcs
                   ns-binding default-ns variables modules)))
           ((and (pair? (car new-prlg))
                 (eq? (sa:op-name (car new-prlg))
                      'declare-global-var))
            (let ((expr (car new-prlg)))
              (let ((var-wrapped (car (sa:op-args expr)))
                    (initial-expr-pair
                     (sa:analyze-expr (cadr (sa:op-args expr))
                                      variables funcs ns-binding default-ns
                                      uri modules))
                    (type-pair (caddr (sa:op-args expr))))
              (and
               initial-expr-pair
               (rpt (cdr new-prlg)
                    triples
                    (cons
                     (list (sa:op-name expr)  ; == 'declare-global-var
                           var-wrapped
                           (car initial-expr-pair)
                           (car type-pair))
                     prolog-res)
                    (cons (cons (car (sa:op-args var-wrapped))
                                (cdr type-pair))
                          variables))))))
           ; #f is added to `new-prlg' for 'declare-function
           ((car new-prlg)  ; doesn't correspond to function declaration
            (rpt (cdr new-prlg)
                 triples
                 (cons (car new-prlg) prolog-res)
                 variables))
           (else  ; corresponds to function declaration from (car triples)
            (let* ((declaration (caar triples))                 
                   (body (cadr (list-ref (sa:op-args (caar triples)) 3)))
                   (vars (append (cadar triples) variables))
                   (return-type (caddar triples))
                   (res (sa:analyze-expr
                         body vars funcs ns-binding default-ns uri modules)))
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
                                ""  ; DL: was: (or default-elem-ns "")
                                ))))
                           (cadr (sa:op-args func-decl)))
                          (caddr (sa:op-args func-decl))  ; return type
                          (list
                           (car (cadddr (sa:op-args func-decl)))  ; ='body
                           (car res))))
                       prolog-res)
                       variables))
                (else
                 (cl:signal-user-error XPTY0004  ; was: SE5007
                                       body))))))))
     (let ((expr (car prolog)))
       (case (sa:op-name expr)
         ((version-declaration)
          (and
           (or
            (= (length (sa:op-args expr)) 1)  ; no encoding supplied
            (sa:assert-num-args expr 2))
           (sa:analyze-string-const
            (car (sa:op-args expr))
            '() '() '() sa:default-ns)
           (or
            (equal? (caddr (car (sa:op-args expr))) "1.0")
            (cl:signal-user-error XQST0031
                                  (caddr (car (sa:op-args expr)))))
           (or
            (null? (cdr (sa:op-args expr)))  ; no encoding
            (and
             (sa:analyze-string-const
              (cadr (sa:op-args expr))
              '() '() '() sa:default-ns)
             (or
              (sa:proper-EncName? (caddr (cadr (sa:op-args expr))))
              (cl:signal-user-error XQST0087
                                    (caddr (cadr (sa:op-args expr)))))))
           (or
            (null? new-prlg)  ; the first member
            (cl:signal-user-error
             XPST0003
             (string-append
              "XQuery version declaration is not the first declaration in "
              "XQuery main module")))
           (loop new-prlg funcs triples
                 ns-binding default-elem-ns default-func-ns modules vars
                 (cdr prolog))))
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
                    ns-binding default-elem-ns default-func-ns modules vars
                    (cdr prolog))))))
         ((declare-default-collation)  ; Default collation
          (and
           (sa:assert-num-args expr 1)
           (sa:analyze-string-const (cadr expr) '() '() '() sa:default-ns)
           (cond
             ((assq (sa:op-name expr)  ; == 'declare-default-collation
                    (filter pair? new-prlg))
              ; Multiple default collation declarations
              => (lambda (entry)
                   (cl:signal-user-error
                    XQST0038
                    (string-append
                     (caddr  ; const value
                      (car (sa:op-args entry))  ; '(const ...)
                      )
                     " and "
                     (caddr 
                      (car (sa:op-args expr)))))))
;             ((not
;               (string=?
;                (caddr  ; collation value
;                 (car (sa:op-args expr)))                    
;                "http://www.w3.org/2005/xpath-functions/collation/codepoint"))
;              ; DL: temporary stub
;              ; "the value specified by a default collation declaration
;              ; is not present in statically known collations"
;              (cl:signal-user-error XQST0038
;                                    (caddr
;                                     (car (sa:op-args expr)))))
             (else
              (loop (cons expr new-prlg)
                    funcs triples
                    ns-binding default-elem-ns default-func-ns modules vars
                    (cdr prolog))))))
         ((declare-base-uri)
          (and
           (sa:assert-num-args expr 1)
           (sa:analyze-string-const (cadr expr) '() '() '() sa:default-ns)
           (cond
             ((assq (sa:op-name expr)  ; == 'declare-base-uri
                    (filter pair? new-prlg))
              ; Multiple base URI declarations
              => (lambda (entry)
                   (cl:signal-user-error
                    XQST0032
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
                    ns-binding default-elem-ns default-func-ns modules vars
                    (cdr prolog))))))
         ((declare-construction)
          ; Almost identical clone of boundary space declaration
          ; TODO: unite them
          (and
           (sa:assert-num-args expr 1)
           (sa:analyze-string-const (cadr expr) '() '() '() sa:default-ns)
           (cond
             ((not
               (member (cadr expr)  ; predefined values
                       '((const (type !xs!string) "strip")
                         (const (type !xs!string) "preserve"))))
              (cl:signal-user-error SE5054 (cadr expr)))
             ((assq (sa:op-name expr)  ; == 'declare-construction
                    (filter pair? new-prlg))
              => (lambda (entry)
                   (cl:signal-user-error
                    XQST0067
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
                    ns-binding default-elem-ns default-func-ns modules vars
                    (cdr prolog))))))
         ((declare-order)  ; Ordering mode
          (and
           (sa:assert-num-args expr 1)
           (sa:analyze-string-const (cadr expr) '() '() '() sa:default-ns)
           (cond
             ((not
               (member (cadr expr)  ; predefined values
                       '((const (type !xs!string) "ordered")
                         (const (type !xs!string) "unordered"))))
              (cl:signal-user-error SE5054 (cadr expr)))
             ((assq (sa:op-name expr)
                    (filter pair? new-prlg))
              ; Multiple ordering mode declarations
              => (lambda (entry)
                   (cl:signal-user-error
                    XQST0065
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
                    ns-binding default-elem-ns default-func-ns modules vars
                    (cdr prolog))))))
         ((declare-default-order)  ; Default order for empty sequences
          ; Clone of boundary-space-decl
          (and
           (sa:assert-num-args expr 1)
           (sa:analyze-string-const (cadr expr) '() '() '() sa:default-ns)
           (cond
             ((not
               (member (cadr expr)  ; predefined values
                       '((const (type !xs!string) "empty-greatest")
                         (const (type !xs!string) "empty-least"))))
              (cl:signal-user-error SE5054 (cadr expr)))
             ((assq 'declare-default-order
                    (filter pair? new-prlg))
              ; Multiple declare-default-order declarations
              ; Clone from boundary-space.
              ; TODO: think of uniting into a single function
              => (lambda (entry)
                   (cl:signal-user-error
                    XQST0069
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
                    ns-binding default-elem-ns default-func-ns modules vars
                    (cdr prolog))))))
         ((declare-copy-namespaces)
          (and
           (sa:assert-num-args expr 2)
           (sa:analyze-string-const (cadr expr) '() '() '() sa:default-ns)
           (sa:analyze-string-const (caddr expr) '() '() '() sa:default-ns)
           (cond
             ((not
               (and
                (member (cadr expr)  ; predefined values
                        '((const (type !xs!string) "preserve")
                          (const (type !xs!string) "no-preserve")))
                (member (caddr expr)
                        '((const (type !xs!string) "inherit")
                          (const (type !xs!string) "no-inherit")))))
              (cl:signal-user-error SE5054 (cadr expr)))
             ((assq (sa:op-name expr)
                    (filter pair? new-prlg))
              => (lambda (entry)
                   (cl:signal-user-error
                    XQST0055
                    (string-append
                     (caddr  ; const value
                      (car (sa:op-args entry)))
                     ", "
                     (caddr
                      (cadr (sa:op-args entry)))
                     " and "
                     (caddr 
                      (car (sa:op-args expr)))
                     ", "
                     (caddr 
                      (cadr (sa:op-args expr)))))))
             (else
              (loop (cons expr new-prlg)
                    funcs triples
                    ns-binding default-elem-ns default-func-ns modules vars
                    (cdr prolog))))))
         ((declare-option)
          (let ((new-prlg
                 ; Processing moved to a separate function
                 (sa:prolog-declare-option expr new-prlg ns-binding)))
            (and new-prlg  ; no error detected
                 (loop new-prlg funcs triples
                       ns-binding default-elem-ns default-func-ns modules vars
                       (cdr prolog)))))
         ((import-module)
          (let ((lst (sa:prolog-module-import expr ns-binding uri modules)))
            (and
             lst
             (let ((import-decl (car lst))
                   (ns-binding (cadr lst))
                   (modules   (caddr lst))
                   (vars      (append (list-ref lst 3) vars))
                   (funcs     (append (list-ref lst 4) funcs)))
               (loop (if uri
                         (cons import-decl new-prlg)
                         new-prlg)
                     funcs triples ns-binding
                     default-elem-ns default-func-ns modules vars
                     (cdr prolog))))))
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
                       default-elem-ns default-func-ns modules vars
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
                  default-func-ns modules vars
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
                  modules vars
                  (cdr prolog)))))
         ((declare-global-var)
          (and
           (or
            (memv (length (sa:op-args expr)) '(2 3))
            (sa:assert-num-args expr 3))
           (or
            (and  ; first argument has the form '(var ...)
             (pair? (car (sa:op-args expr)))
             (eq? (sa:op-name (car (sa:op-args expr)))
                  'var))
            (cl:signal-input-error SE5077 (car (sa:op-args expr))))
           (sa:assert-num-args (car (sa:op-args expr)) 1)
           (let ((var-name
                  (sa:expand-var-name
                   (car (sa:op-args
                         (car (sa:op-args expr))))
                   ns-binding
                   ""  ; DL: was: (or default-elem-ns "")
                   ))
                 (type-pair
                  (sa:analyze-seq-type
                   (if (null? (cddr (sa:op-args expr)))
                       ; no type supplied
                       '(zero-or-more (item-test))
                       (caddr (sa:op-args expr)))
                   ns-binding default-elem-ns)))
             ; Cannot analyze variable value now, since not all
             ; XQuery function declarations are generally processed
             (and
              var-name type-pair
              (or  ; for a library module, is variable declared in module namespace
               (not uri)  ; main module
               (string=? (car var-name) uri)  ; variable declared in this namespace
               (cl:signal-user-error
                XQST0048 
                (if
                 (string=? (car var-name) "")  ; no namespace URI
                 (cadr var-name)
                 (string-append (car var-name) ":" (cadr var-name)))))
              (or  ; no two equal variable names declared?
               (not
                (member
                 var-name
                 (map  ; extract variable names only
                  (lambda (entry)
                    (car (sa:op-args
                          (car (sa:op-args entry)))))
                  (filter 
                   (lambda (entry)
                     (and (pair? entry)
                          (eq? (sa:op-name entry)
                               (sa:op-name expr)  ; == 'declare-global-var
                               )))
                   new-prlg))))
               (cl:signal-user-error
                XQST0049 
                (if
                (string=? (car var-name) "")  ; no namespace URI
                (cadr var-name)
                (string-append (car var-name) ":" (cadr var-name)))))
              (loop
               (cons   ; new prolog
                (list (sa:op-name expr)  ; ='declare-global-var
                      (list (sa:op-name (car (sa:op-args expr)))  ; == 'var
                            var-name)
                      (cadr (sa:op-args expr))
                      ; ATTENTION: temporary spoilt type representation
                      type-pair
                      ; DL: was: (car type-pair)
                      )
                new-prlg)
               funcs triples
               ns-binding default-elem-ns default-func-ns modules vars
               (cdr prolog))))))
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
               ns-binding default-elem-ns default-func-ns modules vars
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
                                    ; DL: was commented out for
                                    ; backward compatibility
                                    "fn"
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
                    ((and uri
                          (not (string=? ns-uri uri)))
                     (cl:signal-user-error
                      XQST0048 
                      (string-append ns-uri ":" local)))
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
                      ns-binding default-elem-ns default-func-ns modules vars
                      (cdr prolog))))))))))
         (else
          (cl:signal-input-error SE5012 expr)))))))

; Processes module import declaration
; expr is a module import declaration:
;   (import-module
;     (const (type !xs!NCName) math)
;     (const (type !xs!string) "http://example.org/math-functions"))
; Returns (list import-decl new-ns-binding new-modules vars funcs) or #f
(define (sa:prolog-module-import expr ns-binding uri modules)
  ; expr analysis is largely borrowed from `sa:module-decl' 
  (and
   (or
    (= (length (sa:op-args expr)) 1)
    (sa:assert-num-args expr 2))
   (call-with-values
    (lambda ()
      (if
       (null? (cdr (sa:op-args expr)))  ; a single argument
       (values #f
               #t  ; dummy non-false value
               (sa:analyze-string-const
                (car (sa:op-args expr)) '() '() '() ""))
       (values #t
               (sa:analyze-const (car (sa:op-args expr)) '() '() '() "")
               (sa:analyze-string-const
                (cadr (sa:op-args expr)) '() '() '() ""))))
    (lambda (prefix-given? module-prefix module-uri)
      (and
       module-prefix module-uri
       (or        
        (not prefix-given?)
        (symbol? (caddr (car module-prefix)))  ; prefix
        (cl:signal-input-error SE5008 (caddr (car module-prefix))))
       (let ((prefix (if
                      prefix-given?
                      (symbol->string (caddr (car module-prefix)))
                      ""))
             (ns-uri (caddr (car module-uri))))
         (cond
           ((equal? ns-uri "")
            ; Sect. 4.11, 2nd paragraph
            (cl:signal-user-error XQST0088))
           ((or (member prefix '("xml" "xmlns"))
                (member ns-uri
                        '("http://www.w3.org/XML/1998/namespace")))
            => (lambda (reason)
                 ; Sect. 4.11, 1st paragraph
                (cl:signal-user-error XQST0070 (car reason))))
           ((assoc ns-uri modules)
            (cl:signal-user-error XQST0047 ns-uri))
           (else
            (let ((import-decl
                   (cons (sa:op-name expr)  ; == 'import-module
                         (if prefix-given?
                             (list (car module-prefix)
                                   (car module-uri))
                             (list (car module-uri)))))
                  (ns-binding (if prefix-given?
                                  (cons (cons prefix ns-uri) ns-binding)
                                  ns-binding)))
              (if
               uri  ; this is a library module
               (list import-decl ns-binding
                     (cons (list ns-uri) modules) '() '())
               ; Main module
               (let ((quad
                      ; Returns (list new-modules module vars funcs) or #f
                      (sa:obtain-module-recursive ns-uri modules '())))
                 (and
                  quad
                  (list import-decl ns-binding
                        (car quad) (caddr quad) (cadddr quad))))))))))))))

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
              (sa:resolve-qname (cadr expr) ns-binding
                                ""
                                ; Was: sa:se-ns
                                ; Sedna namespace is not the default namespace
                                ))
             (value (caddr (caddr expr))))
         (let ((qname-pair
                (cadr (sa:op-args name))))
           ; Cannot use `case' here, since case relies on a `eqv?' comparison
           (cond
             ((or (not (car qname-pair))
                  (string=? (car qname-pair) ""))
              ; See Sect. 4.16 in XQuery specification and
              ; XQTS test "K-OptionDeclarationProlog-1"
              (cl:signal-user-error XPST0081 (cadr qname-pair)))
             ((and (string=? (car qname-part) sa:se-ns)
                   (string=? (cadr qname-part) "output"))
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
             ((and (string=? (car qname-part) sa:se-ns)
                   (string=? (cadr qname-part) "character-map"))
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
      (or (memq name-parts '(* unspecified))  ; wildcard
          ; Already resolved
          (= (length name-parts) 3))
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
               (list ns-uri (cadr name-parts) prefix))))))))

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
     (and
      (or
       (and (pair? type) (eq? (sa:op-name type) 'type)
            (memq (cadr type) '(!xs!QName !xs!qname))
            (or
             (memq value '(* unspecified))
             (and
              (list? value) (memv (length value) '(2 3))
              (null?
               (filter
                (lambda (x)
                  (not (or (string? x) (eq? x '*))))
                value)))))
       (cl:signal-input-error SE5015 qname-const))
      value))))

; Whether we can cast from from-type to to-type
;  from, to ::= sa:type-atomic | sa:type-nodes | sa:type-any
(define (sa:can-be-cast? from to)
  (not (and (eq? from sa:type-atomic)
            (eq? to sa:type-nodes))))

; Signature: (lambda (type-pair abstract+complex-types-allowed?) ...)
; Example:  '("xs" "int") #t --> '!xs!int
; Example:  '("xs" "non-existing-type") #t --> #f
(define sa:map-xs-type-pair
  (let ((type-string->pair
         (lambda (str)
            (cons str (string->symbol (string-append "!xs!" str))))))
    (let ((concrete-simple-types-alist
           (map
            type-string->pair
            ; Depth-first traversal of type hierarchy in XQuery data model
            ; http://www.w3.org/TR/xpath-datamodel/#types-hierarchy
            '("anyAtomicType"
              "string" "normalizedString" "token" "language" "NMTOKEN"
              "Name" "NCName" "ID" "IDREF" "ENTITY"
              "untypedAtomic" "dateTime" "date" "time"
              "duration" "yearMonthDuration" "dayTimeDuration"
              "float" "double" "decimal"
              "integer" "nonPositiveInteger" "negativeInteger"
              "long" "int" "short" "byte"
              "nonNegativeInteger" "positiveInteger"
              "unsignedLong" "unsignedInt" "unsignedShort" "unsignedByte"
              "gYearMonth" "gMonthDay" "gDay" "gMonth"
              "boolean" "base64Binary" "hexBinary" "anyURI"
              "QName" "NOTATION")))
          (other-types-alist
           (map
            type-string->pair
            '("anyType" "anySimpleType"
              ;"IDREFS" "NMTOKENS" "ENTITIES"
              "untyped"))))
      (lambda (type-pair abstract+complex-types-allowed?)
        (cond
          ((not (string=? (car type-pair) "xs"))
           #f)
          ((assoc (cadr type-pair) concrete-simple-types-alist)
           => cdr)
          ((and abstract+complex-types-allowed?
                (assoc (cadr type-pair) other-types-alist))
           => cdr)
          (else #f))))))
  
; SequenceType
; default-ns - default element namespace
; Returns (cons new-type-spec type)
(define (sa:analyze-seq-type type-spec ns-binding default-ns)
  (cond
    ((and (pair? type-spec)
          (memq (car type-spec)
                '(optional zero-or-more one-or-more one)))
     (and 
      (sa:assert-num-args type-spec 1)
      (let ((new-type (sa:analyze-item-type
                       (cadr type-spec) ns-binding default-ns)))
        (and new-type
             (cons (list (car type-spec) (car new-type))
                   (cdr new-type))))))
    ((equal? type-spec '(empty-test))
     ; empty-sequence() sequence type
     (and 
      (sa:assert-num-args type-spec 0)
      (cons type-spec sa:type-any)))
    ((memq type-spec '(optional zero-or-more one-or-more one))
     ; Detect parser bug, for XQTS tests like annex-1 - 5
     ; ATTENTION: This solution might be dangerous, remove this ASAP
     (cons (list type-spec
                 '!xs!anyAtomicType   ; Was: '!xs!untypedAtomic
                 )
           sa:type-atomic))
    (else
     (sa:analyze-item-type type-spec ns-binding default-ns))))

; AtomicType
; Returns (cons rewritten-type sa:type-atomic)
(define (sa:analyze-atomic-type type-spec ns-binding default-ns)
  (cond
   ((symbol? type-spec)
    (cons type-spec sa:type-atomic))
   ((sa:map-xs-type-pair type-spec #f)
    => (lambda (x) (cons x sa:type-atomic)))
   ((and (pair? type-spec) (= (length type-spec) 2)
         (string? (car type-spec)) (string? (cadr type-spec)))
    (and
     ; Must be resolved correctly
     (sa:resolve-qname
      `(const (type !xs!QName) ,type-spec)
      ns-binding default-ns)
     ; We do not know this type
     (cl:signal-user-error
      XPST0051
      (string-append (car type-spec) ":" (cadr type-spec)))))
   (else
    (cl:signal-input-error SE5016 type-spec))))

; Returns (cons rewritten-type sa:type-atomic)
; The returned type always belongs to sa:type-atomic category
; ns-binding and default-ns are dummy arguments here and are provided for 
; mere unification purposes with sa:analyze-seq-type
(define (sa:analyze-single-type type-spec ns-binding default-ns)
  (if
   (and (pair? type-spec)
        (memq (car type-spec) '(optional one)))
   (and
    (sa:assert-num-args type-spec 1)
    (let ((new-type (sa:analyze-atomic-type
                     (cadr type-spec) ns-binding default-ns)))
      (and new-type
           (cons (list (car type-spec) (car new-type))
                 (cdr new-type)))))
  (sa:analyze-atomic-type type-spec ns-binding default-ns)))

; ItemType
; Is presented in SequenceType only
(define (sa:analyze-item-type type-spec ns-binding default-ns)
  (cond
    ((eq? type-spec 'doc-test)
     (cons (list type-spec) sa:type-nodes))
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
       ((sa:map-xs-type-pair type-spec #t)
        => (lambda (new-type)
             (cons new-type
                   (if
                    (memq new-type '(!xs!anyType !xs!untyped))
                    sa:type-any sa:type-atomic))))
       ; Older variant of the above branch:
;       ((assoc type-spec
;               '((("xs" "int") . !xs!int)
;                 (("xs" "long") . !xs!long)
;                 (("xs" "short") . !xs!short)
;                 (("xs" "byte") . !xs!byte)
;                 (("xs" "nonPositiveInteger") . !xs!nonPositiveInteger)
;                 (("xs" "negativeInteger") . !xs!negativeInteger)
;                 (("xs" "nonNegativeInteger") . !xs!nonNegativeInteger)
;                 (("xs" "positiveInteger") . !xs!positiveInteger)
;                 (("xs" "unsignedLong") . !xs!unsignedLong)
;                 (("xs" "unsignedInt") . !xs!unsignedInt)
;                 (("xs" "unsignedShort") . !xs!unsignedShort)
;                 (("xs" "unsignedByte") . !xs!unsignedByte)
;                 (("xs" "normalizedString") . !xs!normalizedString)
;                 (("xs" "token") . !xs!token)
;                 (("xs" "language") . !xs!language)
;                 (("xs" "Name") . !xs!Name)
;                 (("xs" "NCName") . !xs!NCName)
;                 (("xs" "NMTOKEN") . !xs!NMTOKEN)
;                 ;(("xs" "NMTOKENS") . !xs!NMTOKENS)
;                 (("xs" "ID") . !xs!ID)
;                 (("xs" "IDREF") . !xs!IDREF)
;                 ;(("xs" "IDREFS") . !xs!IDREFS)
;                 (("xs" "ENTITY") . !xs!ENTITY)
;                 ;(("xs" "ENTITIES") . !xs!ENTITIES)
;                 ;----------
;                 ; XQuery predefined schema types:
;                 ; 2.5.1 Predefined Schema Types                
;                 ;(("xs" "anyType") . !xs!anyType)  ; These must not be supported
;                 ;(("xs" "untyped") . !xs!untyped)
;                 ;(("xs" "anySimpleType") . !xs!anySimpleType)
;                 (("xs" "anyAtomicType") . !xs!anyAtomicType)
;                 (("xs" "untypedAtomic") . !xs!untypedAtomic)
;                 (("xs" "dayTimeDuration") . !xs!dayTimeDuration)
;                 (("xs" "yearMonthDuration") . !xs!yearMonthDuration)
;                 ))
;        => (lambda (pair)
;             (cons (cdr pair)
;                   (if
;                    (memq (cdr pair) '(!xs!anyType !xs!untyped))
;                    sa:type-any sa:type-atomic))))
       
;       ((not (string=? (car type-spec) "xs"))
;        ; TODO: QName resolution in type names
;        (cl:signal-user-error
;         XPST0051  ; was: XPST0081
;         (car type-spec)))
       (else
        (and
         ; Must be resolved correctly
         ; TODO: This should be moved towards the beginning of the function,
         ; namespace URI should be analyzed instead of prefix
         (sa:resolve-qname
          `(const (type !xs!QName) ,type-spec)
          ns-binding default-ns)
         ; We do not know this type
         ; Was: (cons type-spec sa:type-atomic)
         (cl:signal-user-error
          XPST0051
          (string-append (car type-spec) ":" (cadr type-spec)))))))
    ((and (pair? type-spec)
          (eq? (car type-spec) 'doc-test))
     (or
      (and (null? (cdr type-spec))  ; no more arguments
           (cons type-spec sa:type-nodes))
      (and
       (sa:assert-num-args type-spec 1)
       (let ((new-ename (sa:analyze-item-type
                         ;sa:analyze-ename
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
   ; Ad-hoc solution for parser bug that reveals on XQTS test
   ; "ForExprType002" and the like
   (let ((expr
          (if
           (= (length (sa:op-args expr)) 2)
           ; Nillable part missing
           (append expr '((const (type !xs!string) "non-nil")))
           expr)))
     (and
      (sa:assert-num-args expr 3)
      (let ((new-name
             (and
              (sa:proper-qname (car (sa:op-args expr)))
              ; Ensure that the qname can be correctly expanded
              ; Was: commented out
              (sa:resolve-qname (car (sa:op-args expr)) ns-binding default-ns
                                ; DL: should be?: (car default-ns)
                                )
              ; Do not actually expand it until dynamic evaluation phase
              ;(car (sa:op-args expr))
              ))
            (new-type
             (let ((type-node (cadr (sa:op-args expr))))
               (and
                (or
                 (and (pair? type-node)
                      (not (null? type-node))
                      (eq? (sa:op-name type-node) 'type))
                 (cl:signal-input-error SE5018 (cadr (sa:op-args expr))))
                (let ((type-pair
                       (sa:analyze-item-type (car (sa:op-args type-node))
                                             ns-binding default-ns)))
                  (and
                   type-pair
                   (list (sa:op-name type-node)  ; == 'type
                         (car type-pair)))))))
            (new-nil
             (if
              (equal? (caddr (sa:op-args expr))
                      '(const (type !xs!string) qmark))
              (cons '(const (type !xs!string) "qmark")
                    sa:type-atomic)
              (sa:analyze-string-const
               (caddr (sa:op-args expr))
               '() '() ns-binding (list default-ns "")))))
        (and
         new-name new-type new-nil
         (cons (list (sa:op-name expr)  ; ='ename
                     new-name
                     new-type
                     (car new-nil))
               sa:type-nodes)))))))

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
                            ns-binding
                            ""  ; DL: was: default-ns
                            )))
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
                (if
                 (null?
                  (filter
                   (lambda (triple)
                     ; triple ::= (list var-name sa:type var-type)
                     (and
                      (equal? (car triple) name-pair)
                      (memv (caddr triple)
                            '(!se!positional-var se:positional-var))))
                   vars))
                 ; Not a positional variable
                 (cl:signal-user-error
                  XQST0039
                  (if  ; no namespace in variable name
                   (string=? (car name-pair) "")
                   (cadr name-pair)
                   (string-append (car name-pair) ":" (cadr name-pair))))
                 (cl:signal-user-error
                  XQST0089
                  (if  ; no namespace in variable name
                   (string=? (car name-pair) "")
                   (cadr name-pair)
                   (string-append (car name-pair) ":" (cadr name-pair)))))))
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

;-------------------------------------------------
; Declare-variable helpers

; Performs apply append for lists and removes equal? duplicates
(define (sa:apply-append-remove-equal-duplicates lsts)
  (cond
    ((null? lsts) lsts)
    ((null? (cdr lsts)) (car lsts))
    (else
     (let loop ((first (car lsts))
                (second (cadr lsts)))
       (if
        (null? first)
        (sa:apply-append-remove-equal-duplicates
         (cons second (cddr lsts)))
        (loop (cdr first)
              (if
               (member (car first) second)
               second
               (cons (car first) second))))))))

; Returns: (list (listof var-name)
;                (listof (list function-name arity)))
; var-name, function-name ::= (list namespace-uri local-part)
; expr is considered properly-formed
(define (sa:free-variables-and-function-calls expr bound-vars)
  (letrec
      ((tree-walk
        (lambda (expr bound-vars)
          (if
           (not (and (pair? expr)
                     (symbol? (car expr))))
           '(() ())
           (case (sa:op-name expr)
             ((var)
              (let ((var-name
                     (car (sa:op-args expr))))
                (list (if (member var-name bound-vars)
                          '()
                          (list (car (sa:op-args expr))))
                      '())))
             ((fun-call)
              (let ((arg-results
                     (map
                      (lambda (kid) (tree-walk kid bound-vars))
                      (cdr  ; except for function name
                       (sa:op-args expr)))))
                (list
                 (sa:apply-append-remove-equal-duplicates
                  (map car arg-results))
                 (sa:apply-append-remove-equal-duplicates
                  (cons
                   (list
                    (list (cadr (sa:op-args  ; function name
                                 (car (sa:op-args expr))  ; '(const ...)
                                 ))
                          (length (cdr (sa:op-args expr)))))
                   (map cadr arg-results))))))
             ((fun-def)
              (tree-walk
               (cadr (sa:op-args expr))  ; fun-def body
               (append
                (map
                 (lambda (arg)
                   (car (sa:op-args
                         (cadr arg)  ; addresses '(var ...)
                         )))
                 (car (sa:op-args expr))  ; arguments
                 )
                bound-vars)))
             ((declare-function)
              (tree-walk
               (cadddr (sa:op-args expr))  ; function body
               (append
                (map
                 (lambda (arg)
                   (car (sa:op-args
                         (cadr arg)  ; addresses '(var ...)
                         )))
                 (cadr (sa:op-args expr))  ; arguments
                 )
                bound-vars)))
             (else  ; Recursive propagation
              (let ((kid-results
                     (map
                      (lambda (kid) (tree-walk kid bound-vars))
                      (sa:op-args expr))))
                (list
                 (sa:apply-append-remove-equal-duplicates
                  (map car kid-results))
                 (sa:apply-append-remove-equal-duplicates
                  (map cadr kid-results))))))))))
    (tree-walk expr bound-vars
;               (if (null? bound-vars)
;                   bound-vars
;                   (car bound-vars))
               )))

; Like `member', but uses a predicate condition
(define (sa:mem-pred lst pred?)
  (cond
    ((null? lst) #f)
    ((pred? (car lst)) lst)
    (else 
     (sa:mem-pred (cdr lst) pred?))))

; graph ::= (listof
;             (cons node (listof node)))
; Each list '(node1 node2 node3 ... nodek) denotes that the graph contains
; node1 and arcs node1 -> node2, node1 -> node3, etc.
; Even if the node does not contain any outgoing arcs, it still must be
; presented in the graph - in the form of (list node)
(define (sa:graph-contains-cycles? graph)
  (let ((sa:remove-node-from-graph
         (lambda (node graph)
           (map
            (lambda (single)
              (filter
               (lambda (item) (not (equal? item node)))
               single))
            (filter
             (lambda (single)
               (not (equal? (car single) node)))
             graph)))))
    (cond
      ((null? graph)  ; no cycles
       #f)
      ((sa:mem-pred graph
                    ; no outgoing arcs for that node
                    (lambda (single) (null? (cdr single))))
       => (lambda (sub)
            (sa:graph-contains-cycles?
             (sa:remove-node-from-graph (caar sub) graph))))
      (else  ; graph contains a cycle
       #t))))

; global-var-decl ::= `(declare-global-var ...)
(define (sa:build-dependency-graph global-var-decl prolog)
  ; TODO: can optimize processing of the first node
  (let loop ((nodes-to-view
              (list (car (sa:op-args  ; variable name
                          (car (sa:op-args global-var-decl))  ; `(var ...)
                          ))))
             (graph '()))
    (cond
      ((null? nodes-to-view)
       graph)
      ((assoc (car nodes-to-view) graph)
       ; Current node is already added to the graph
       (loop (cdr nodes-to-view)
             graph))
      (else
       (let* ((depends-on-vars-and-funcs
               (if
                (number? (cadar nodes-to-view))
                ; Current node is a function name
                (let ((fun-name (caar nodes-to-view))
                      (arity (cadar nodes-to-view)))
                  (let ((decl-lst
                         (filter
                          (lambda (entry)
                            (and
                             (pair? entry)
                             (eq? (sa:op-name entry) 'declare-function)
                             (= (length
                                 (cadr (sa:op-args entry))  ; argument list
                                 )
                                arity)
                             (equal?
                              (caddr  ; function name
                               (car (sa:op-args entry)))
                              fun-name)))
                          prolog)))
                    (if
                     (null? decl-lst)  ; this should not happen!
                     '(() ())
                     (sa:free-variables-and-function-calls
                      (car (sa:op-args
                            (cadddr (sa:op-args  ; addresses `(body ...)
                                     (car decl-lst)))))
                      (map
                       (lambda (arg)
                         (car (sa:op-args
                               (cadr arg)  ; addresses '(var ...)
                               )))
                       (cadr (sa:op-args (car decl-lst)))  ; arguments
                       )))))
                ; Otherwise - current node is a variable name
                (let ((var-name (car nodes-to-view)))
                  (let ((decl-lst
                         (filter
                          (lambda (entry)
                            (and
                             (pair? entry)
                             (eq? (sa:op-name entry) 'declare-global-var)
                             (equal?
                              (car (sa:op-args
                                    (car (sa:op-args entry))  ; `(var ...)
                                    ))
                              var-name)))
                          prolog)))
                    (if
                     (null? decl-lst)  ; this should not happen!
                     '(() ())
                     (sa:free-variables-and-function-calls
                      (cadr (sa:op-args
                             (car decl-lst)))
                      '()))))))
              (dependencies
               (append (car depends-on-vars-and-funcs)
                       (cadr depends-on-vars-and-funcs))))
         (loop (append dependencies (cdr nodes-to-view))
               (cons
                (cons (car nodes-to-view) dependencies)
                graph)))))))

; Checks whether there are no recursive dependencies in the
; initializing expressions for variables in prolog
(define (sa:no-recursive-dependendencies? prolog)
  (null?
   (filter
    (lambda (x) (not x))
    (map
     (lambda (var-decl)
       (or
        (not
         (sa:graph-contains-cycles?
          ((lambda (graph)
             ;(pp graph)
             graph)
           (sa:build-dependency-graph var-decl prolog))))
        (cl:signal-user-error
         XQST0054
         (let ((pair
                (car (sa:op-args  ; variable name
                      (car (sa:op-args var-decl))  ; `(var ...)
                      ))))
           (if (string=? (car pair) "")  ; no namespace URI
               (cadr pair)
               (string-append (car pair) ":" (cadr pair)))))))
     (filter  ; all global variables
      (lambda (entry)
        (and (pair? entry)
             (eq? (sa:op-name entry) 'declare-global-var)))
      prolog)))))
  

;==========================================================================
; Analysis for different operations

;; Variable reference
;(define (sa:analyze-variable expr vars funcs ns-binding default-ns uri modules)
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
;(define (sa:variable-wrapped expr vars funcs ns-binding default-ns uri modules)
;  (cond
;    ((and (sa:assert-num-args expr 1)
;          (sa:analyze-variable
;           (sa:op-args expr)    ; was: (car (sa:op-args expr))
;           vars funcs ns-binding default-ns uri modules))
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

; expr ::= `(var ...)
; Returns (cons `(var ...) var-type)
(define (sa:variable-wrapped expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 1)
   (let ((var-name
          (sa:expand-var-name
           (car (sa:op-args expr))
           ns-binding
           ; XQuery spec. 3.1.2:
           ; "An unprefixed variable reference is in no namespace"
           ""  ; DL: was: (car default-ns)
           )))
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
        ((and uri  ; this is library module
              (assoc (car var-name) modules))
         ; Variable is probably declared in the imported module
         (cons (list (sa:op-name expr)  ; = 'var
                     var-name)
               sa:type-any))
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
   (not (and (pair? expr)
             (eq? (sa:op-name expr) 'const)))
   (cl:signal-input-error SE5014 expr)
   (and
    (sa:assert-num-args expr 2)
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
         (cl:signal-input-error SE5027 expr)))))))

; Whether the expr is the representation for QName constant
(define (sa:qname-const? expr)
  (and (list? expr)
       (= (length expr) 3)
       (eq? (car expr) 'const)
       (equal? (cadr expr) '(type !xs!QName))))

;-------------------------------------------------
; 2.3 Axes

; Axis
(define (sa:analyze-axis expr vars funcs ns-binding default-ns uri modules)
  ;(pp (list expr vars default-ns))
  (and
   (sa:assert-num-args expr 2)
   (let*
       ; Type is to be analyzed _before_ analyzing the subexpr, since
       ; type analysis may produce XQuery static errors only, while
       ; subexpr analysis may result in an XQuery dynamic error, e.g.
       ; when context is not defined
       ((new-type (sa:analyse-wrapped-sequence-type
                   (cadr (sa:op-args expr))
                   vars funcs ns-binding default-ns))
        (a (sa:analyze-expr (car (sa:op-args expr))
                            vars funcs ns-binding default-ns uri modules)))
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

; Analysing `(type ,something)
(define (sa:wrapped-type-helper type-analyser)
  (lambda (expr vars funcs ns-binding default-ns)
    (cond
      ((not (and (pair? expr)  ; '() is not a pair
                 (eq? (sa:op-name expr) 'type)))
       (cl:signal-input-error SE5030 expr))
      ((and (sa:assert-num-args expr 1)
            (type-analyser
             (car (sa:op-args expr)) ns-binding (car default-ns)))
       => (lambda (pair)
            (cons (list (sa:op-name expr)  ; ='type
                        (car pair))
                  (cdr pair))))
      (else  ; error message already displayed
       #f))))
(define sa:analyse-wrapped-sequence-type
  (sa:wrapped-type-helper sa:analyze-seq-type))
(define sa:analyze-wrapped-single-type
  (sa:wrapped-type-helper sa:analyze-single-type))

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
(define (sa:analyze-sequence expr vars funcs ns-binding default-ns uri modules)
  (let ((args-res
         (map
          (lambda (subexpr)
            (sa:analyze-expr subexpr vars funcs ns-binding default-ns uri modules))
          (sa:op-args expr))))
    (if
     (memv #f args-res)  ; error detected    
     #f
     (cons (cons (sa:op-name expr)  ; ='sequence or 'unio or 'space-sequence
                 (map car args-res))
           (sa:combine-types (map cdr args-res))))))

;-------------------------------------------------
; 2.5 Arithmetic operations

(define (sa:binary-arithmetic expr vars funcs ns-binding default-ns uri modules)
  (if
   (sa:assert-num-args expr 2)
   (sa:propagate expr vars funcs ns-binding default-ns uri modules sa:type-atomic)
   #f))

(define (sa:unary-arithmetic expr vars funcs ns-binding default-ns uri modules)
  (if
   (sa:assert-num-args expr 1)
   (sa:propagate expr vars funcs ns-binding default-ns uri modules sa:type-atomic)
   #f))

(define (sa:analyze-union-intersect expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 2)
   (let ((args-res (map
                    (lambda (arg)
                      (sa:analyze-expr arg vars funcs ns-binding default-ns uri modules))
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
(define (sa:analyze-comparison expr vars funcs ns-binding default-ns uri modules)
  (if
   (sa:assert-num-args expr 2)
   (sa:propagate expr vars funcs ns-binding default-ns uri modules sa:type-atomic)
   #f))

; Node comparison
(define (sa:analyze-node-comparison expr vars funcs ns-binding default-ns uri modules)
  (if
   (not (sa:assert-num-args expr 2))
   #f
   (let ((args-res
          (map
           (lambda (subexpr)
             (sa:analyze-expr subexpr vars funcs ns-binding default-ns uri modules))
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

(define (sa:analyze-if expr vars funcs ns-binding default-ns uri modules)
  (if
   (not (sa:assert-num-args expr 3))
   #f
   (let ((args-res
         (map
          (lambda (subexpr) (sa:analyze-expr subexpr vars funcs ns-binding default-ns uri modules))
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
(define (sa:analyze-not@ expr vars funcs ns-binding default-ns uri modules)
  (if
   (sa:assert-num-args expr 1)
   (sa:propagate expr vars funcs ns-binding default-ns uri modules sa:type-atomic)
   #f))

(define (sa:analyze-and@-or@ expr vars funcs ns-binding default-ns uri modules)
  (if
   (sa:assert-num-args expr 2)
   (sa:propagate expr vars funcs ns-binding default-ns uri modules sa:type-atomic)
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
(define (sa:xmlns-declarations-in-element expr ns-binding default-ns uri modules)
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
  (and
   pair  ; error propagation
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
          (let* ((name (car (sa:op-args (car src))))
                 (url+local (let ((name-triple (cadr (sa:op-args name))))
                              (car name-triple) (cadr name-triple))))
            (if
             (sa:qname-const? name)
             ; Constant attribute name
             (let ((name
                    (sa:resolve-qname name ns-binding (cadr default-ns))))
               (if
                (member url+local attr-names)
                ; Duplicate attribute declared
                (cl:signal-user-error XQST0040 (sa:qname->string name))
                (loop (cdr src)
                      namespaces
                      (cons (car src) others)
                      prefixes
                      (cons url+local attr-names))))
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
                 (; Prefix undeclaration
                  (and
                   (not (string=? str ""))  ; not default namespace
                   (member (cadr (sa:op-args (car src)))  ; namespace value
                           '("" (sequence))))                  
                  ; Undeclaration is specified in XML Namespaces 1.1, an
                  ; XQuery implementation may not support undeclaration.
                  ; That is what we do here
                  (cl:signal-user-error
                   XQST0085
                   (string-append "xmlns:" str)))
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
                prefixes attr-names)))))))

; Element constructor
(define (sa:analyze-element-constructor expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 2)
   (let* ((new-xmlns
           (sa:xmlns-declarations-in-element expr ns-binding default-ns uri modules))
          (ns-binding (car new-xmlns))
          (default-ns (cdr new-xmlns)))
     (and
      (or
       (not (sa:qname-const? (car (sa:op-args expr))))
       ; If name is constant, it must be correctly resolved
       (sa:resolve-qname
        (car (sa:op-args expr)) ns-binding (cadr default-ns)))
      (sa:post-element
       (sa:propagate expr vars funcs ns-binding default-ns uri modules sa:type-nodes)
       ns-binding
       default-ns)))))
     
; Constructors for attribute, pi and namespace
; TODO: more sophisticated treatment for pi and namespace names is desirable
; QUESTION: how many arguments constructors must have?
(define (sa:attribute-namespace
         expr vars funcs ns-binding default-ns uri modules)
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
     (sa:propagate expr vars funcs ns-binding default-ns uri modules sa:type-nodes))
    (sa:propagate expr vars funcs ns-binding default-ns uri modules sa:type-nodes))))

(define (sa:pi-constructor expr vars funcs ns-binding default-ns uri modules)
  (and
   (or
    (= (length (sa:op-args expr)) 1)  ; no PI body
    (sa:assert-num-args expr 2))
   (or
    (not
     (let ((target
            (car (sa:op-args expr))))
       (and
        (pair? target)
        (eq? (car target) 'const)  ; constant PI target
        (or
         (null?  ; no PI content => direct PI constructor
          (cdr (sa:op-args expr)))
         (not
          (and  ; PI content is a space-seq => a computed PI constructor
           (pair? (cadr (sa:op-args expr)))
           (eq? (sa:op-name
                 (cadr (sa:op-args expr)))
                'space-sequence))))
        (= (length target) 3)
        (let ((value
               (cadr (sa:op-args target))))
          (string-ci=?
           (cond
             ((symbol? value) (symbol->string value))
             ((string? value) value)
             (else  ; this should not happen
              ""))
           "xml")))))
    (cl:signal-user-error
     XPST0003
     (string-append
      "The PITarget of a processing instruction may not "
      "consist of the characters \"XML\" in any combination "
      "of upper and lower case.")))
   (or
    (not
     (sa:qname-const? (car (sa:op-args expr)))  ; name is constant
     )
    (sa:resolve-qname
     (car (sa:op-args expr)) ns-binding (cadr default-ns)))
   (sa:propagate expr vars funcs ns-binding default-ns uri modules sa:type-nodes)))

; Constructors for document node, text node, comment node
(define (sa:document-text-comment expr vars funcs ns-binding default-ns uri modules)
  (if
   (sa:assert-num-args expr 1)
   (sa:propagate expr vars funcs ns-binding default-ns uri modules sa:type-nodes)
   #f))

;-------------------------------------------------
; 2.10 FLWOR Operations
; TODO: order-by

(define (sa:analyze-let@ expr vars funcs ns-binding default-ns uri modules)
  (if
   (not (sa:assert-num-args expr 2))
   #f
   (let ((new-value
          (sa:analyze-expr (car (sa:op-args expr)) vars funcs ns-binding default-ns uri modules))
         (new-fun
          (sa:analyze-fun-def (cadr (sa:op-args expr)) vars funcs ns-binding default-ns uri modules)))
     (and
      new-value new-fun
      (cons (list (sa:op-name expr)  ; ='let@
                  (car new-value)
                  (car new-fun))
            (cdr new-fun))))))

; Return
(define (sa:analyze-return expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 2)
   (let ((new-value
          (sa:analyze-expr (car (sa:op-args expr)) vars funcs ns-binding default-ns uri modules))
         (new-fun
          (sa:analyze-fun-def (cadr (sa:op-args expr))
                              vars funcs ns-binding default-ns uri modules)))
     (and
      new-value new-fun
      (cons (list (sa:op-name expr)  ; ='return
                  (car new-value)
                  (car new-fun))
            (cdr new-fun))))))

; Clone of Return except for return value
(define (sa:analyze-predicate expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 2)
   (let ((new-value
          (sa:analyze-expr (car (sa:op-args expr)) vars funcs ns-binding default-ns uri modules))
         (new-fun
          (sa:analyze-fun-def (cadr (sa:op-args expr)) vars funcs ns-binding default-ns uri modules)))
     (and
      new-value new-fun
      (cons (list (sa:op-name expr)  ; ='predicate
                  (car new-value)
                  (car new-fun))
            (cdr new-value))))))

; A call to fun-def.
; It's argument always has the type 'sa:nodes
; Returns the return type or #f
(define (sa:analyze-fun-def expr vars funcs ns-binding default-ns uri modules)
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
                                 funcs ns-binding default-ns uri modules)))
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
  (lambda (expr vars funcs ns-binding default-ns uri modules)
    (and
     (sa:assert-num-args expr 2)
     (let ((args
            (list
             (sa:analyze-expr (car (sa:op-args expr))
                              vars funcs ns-binding default-ns uri modules)
             (sa:analyze-wrapped-single-type
              (cadr (sa:op-args expr))
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
          (cond
            ((equal? expr '(cast (sequence) (type (optional !xs!QName))))
             (cons expr sa:type-atomic))
            ((assq item-type
                   '((!xs!anyAtomicType . "xs:anyAtomicType")
                     (!xs!NOTATION      . "xs:NOTATION")))
             => (lambda (pair)
                  (cl:signal-user-error
                   XPST0080
                   (string-append
                    (symbol->string (sa:op-name expr))
                    " as "
                    (cdr pair)))))
            ((assq item-type
                   '((!xs!anySimpleType . "xs:anySimpleType")
                     (!xs!anyType       . "xs:anyType")
                     (xs:anyType        . "xs:anyType")))
             => (lambda (pair)
                  (cl:signal-user-error
                   XPST0051
                   (string-append
                    (symbol->string (sa:op-name expr))
                    " as "
                    (cdr pair)))))
            (else
             (and
              (or
               (not
                (and
                 (eq? item-type '!xs!QName)
                 (pair? (car (sa:op-args expr)))
                 (not
                  (or
                   (eq? (sa:op-name (car (sa:op-args expr))) 'const)
                   (let ((after-analysis (caar args)))
                     (and
                      (eq? (sa:op-name after-analysis) 'cast)
                      (equal?
                       (cadr (sa:op-args after-analysis))
                       '(type (one !xs!QName)))
                      (pair? (car (sa:op-args after-analysis)))
                      (eq? (sa:op-name
                            (car (sa:op-args after-analysis)))
                           'const)))))))
               (begin
                 (cl:signal-user-error
                  XPTY0004
                  (string-append
                   (symbol->string (sa:op-name expr))
                   " as xs:QName for non-constant expression"))))
              (cons (cons (sa:op-name expr)
                          (map car args))
                    (return-type-lambda args)))))))))))

(define sa:analyze-cast (sa:cast-helper
                         cdar  ; type of the subexpr
                         ))

; Castable
; A lot of common code with sa:cast-helper
; TODO: analyze single type instead of item type
(define (sa:analyze-castable expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 2)
   (let ((args
          (list
           (sa:analyze-expr (car (sa:op-args expr))
                            vars funcs ns-binding default-ns uri modules)
           (sa:analyze-wrapped-single-type
            (cadr (sa:op-args expr))
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
        (cond
          ((equal? expr '(castable (sequence) (type (optional !xs!QName))))
           (cons expr sa:type-atomic))
          ((assq item-type
                   '((!xs!anyAtomicType . "xs:anyAtomicType")
                     (!xs!NOTATION      . "xs:NOTATION")))
             => (lambda (pair)
                  (cl:signal-user-error
                   XPST0080
                   (string-append "Castable as " (cdr pair)))))
          ((and
            (memq item-type '(!xs!QName !xs!NOTATION))
            (pair? (car (sa:op-args expr)))
            (not
             (or
              (eq? (sa:op-name (car (sa:op-args expr))) 'const)
              (let ((after-analysis (caar args)))
                (or
                 (and
                  (eq? (sa:op-name after-analysis) '!fn!QName)
                  (null?
                   (filter
                    (lambda (x)
                      (not (and (pair? x)
                                (eq? (sa:op-name x) 'const))))
                    (sa:op-args after-analysis))))
                 (and
                  (eq? (sa:op-name after-analysis) 'cast)
                  (equal?
                   (cadr (sa:op-args after-analysis))
                   '(type (one !xs!QName)))
                  (pair? (car (sa:op-args after-analysis)))
                  (eq? (sa:op-name
                        (car (sa:op-args after-analysis)))
                       'const)))))))
           ; Note in XQuery specification, Sect. 3.12.4
           (cons `(and@
                   (castable
                    ,(caar args)
                    (type (one !xs!string)))
                   (!fn!false))
                  sa:type-atomic))
          ((assq item-type
                 '((!xs!anyAtomicType . "xs:anyAtomicType")
                   (!xs!NOTATION      . "xs:NOTATION")))
           => (lambda (pair)
                (cl:signal-user-error
                 XPST0080
                 (string-append "Castable as " (cdr pair)))))
          ((assq item-type
                 '((!xs!anySimpleType . "xs:anySimpleType")
                   (!xs!anyType       . "xs:anyType")
                   (xs:anyType        . "xs:anyType")))
           => (lambda (pair)
                (cl:signal-user-error
                 XPST0051
                 (string-append "Castable as " (cdr pair)))))
          (else
           (cons (cons (sa:op-name expr)  ; == 'castable
                       (map car args))
                 sa:type-atomic))))))))

; Treat
(define (sa:analyze-treat expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 2)
   (let ((args (list
                (sa:analyze-expr
                 (car (sa:op-args expr)) vars funcs ns-binding default-ns uri modules)
                (sa:analyse-wrapped-sequence-type
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
(define (sa:analyze-typeswitch expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 2)
   (let ((source-expr
          (sa:analyze-expr (car (sa:op-args expr))
                           vars funcs ns-binding default-ns uri modules)))
     (and
      source-expr
      (let ((cases (sa:ts-all-cases (cadr (sa:op-args expr))
                                    vars funcs ns-binding default-ns
                                    uri modules
                                    (cdr source-expr)  ; source expression type
                                    )))
        (and
         cases  ; no error detected
         (cons (list (sa:op-name expr)  ; = 'ts
                     (car source-expr)
                     (car cases))
               (cdr cases)  ; return type
               )))))))

(define (sa:ts-all-cases
         expr vars funcs ns-binding default-ns uri modules src-type)
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
                     vars funcs ns-binding default-ns uri modules src-type)))
          (and
           case
           (loop (cdr args)
                 (cons case res)))))))))

(define (sa:ts-case expr vars funcs ns-binding default-ns uri modules src-type)
  (cond
    ((not (and (pair? expr) (eq? (car expr) 'case)))
     (cl:signal-input-error SE5058 expr))
    (else
     (and
      (sa:assert-num-args expr 2)
      (let ((type-pair (sa:analyse-wrapped-sequence-type
                        (car (sa:op-args expr))
                        vars funcs ns-binding default-ns))
            (func-pair (sa:analyze-fun-def (cadr (sa:op-args expr))
                                           vars funcs ns-binding default-ns uri modules)))
        (and
         type-pair func-pair
         (cons (list (sa:op-name expr)  ; = 'case
                     (car type-pair)
                     (car func-pair))
               (cdr func-pair))))))))       

(define (sa:ts-default expr vars funcs ns-binding default-ns uri modules src-type)
  (cond
    ((not (and (pair? expr) (eq? (car expr) 'default)))
     (cl:signal-input-error SE5059 expr))
    (else
     (and
      (sa:assert-num-args expr 1)
      (let ((new-fun (sa:analyze-fun-def (car (sa:op-args expr))
                                         vars funcs ns-binding default-ns uri modules)))
        (and
         new-fun
         (cons (list (sa:op-name expr)  ; = 'default
                     (car new-fun))
               (cdr new-fun))))))))

;-------------------------------------------------
; XQuery 3.14 Extension Expressions

(define (sa:analyze-extension-expression
         expr vars funcs ns-binding default-ns uri modules)
  (and
   (or
    (not (null? (sa:op-args expr)))  ; at least 1 argument
    (sa:assert-num-args expr 2))
   ; TODO: first argument must be 'pragmas, with one or more nested
   ; 'pragma, resolve namespace prefix for each pragma
   (or
    (not (null?  ; at least 2 arguments
          (cdr (sa:op-args expr))))
    (cl:signal-user-error XQST0079
                          ""  ; some details about pragmas can be added here
                          ))
   ; Fallback
   (sa:analyze-expr (cadr (sa:op-args expr))
                    vars funcs ns-binding default-ns uri modules)))
    
;-------------------------------------------------
; 2.14 Distinct doc order

(define (sa:analyze-ddo expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 1)
   (let ((seq-res (sa:analyze-expr (car (sa:op-args expr)) vars funcs ns-binding default-ns uri modules)))
     (and
      seq-res
      (if
       (eq? (cdr seq-res) sa:type-atomic)
       (cl:signal-input-error SE5034 expr)
       (cons (list (sa:op-name expr)             
                   (car seq-res))
             (cdr seq-res)))))))

; Ordered and unordered expressions
(define (sa:ordered-unordered expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 1)
   (let ((seq-res
          (sa:analyze-expr (car (sa:op-args expr))
                           vars funcs ns-binding default-ns uri modules)))
     (and
      seq-res
      (cons (list (sa:op-name expr)             
                  (car seq-res))
            (cdr seq-res))))))

;-------------------------------------------------
; 3.3 XPath

(define (sa:analyze-congen1 expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 2)
   (let ((seq-res (sa:analyze-expr
                   (car (sa:op-args expr)) vars funcs ns-binding default-ns uri modules))
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

(define (sa:analyze-congen2 expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 1)
   (let ((pair
          (sa:analyze-expr
           (car (sa:op-args expr)) vars funcs ns-binding default-ns uri modules)))
     (and
      pair
      (cons (list (sa:op-name expr)
                  (car pair))
            (cdr pair))))))

;-------------------------------------------------
; 3.6. Quantified expressions

; Some, every
(define (sa:some-every expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 2)
   (let ((seq-res
          (sa:analyze-expr (car (sa:op-args expr)) vars funcs ns-binding default-ns uri modules))
         (fun-res
          (sa:analyze-fun-def (cadr (sa:op-args expr)) vars funcs ns-binding default-ns uri modules)))
     (and
      seq-res fun-res
      (cons (list (sa:op-name expr)
                  (car seq-res) (car fun-res))
            sa:type-any)))))

;-------------------------------------------------
; 3.7 XQuery 1.0 Functions

; fn:doc and fn:collection
(define (sa:document-collection expr vars funcs ns-binding default-ns uri modules)
  (if
   (< (length (sa:op-args expr)) 3)  ; enough arguments
   (sa:propagate expr vars funcs ns-binding default-ns uri modules sa:type-nodes)
   (sa:assert-num-args expr 1)  ; this would cause an error message
   ))

; fn:position, fn:last, fn:true, fn:false
(define (sa:position-last-true-false expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 0)
   (cons expr sa:type-atomic)))

; Functions with a single argument, return atomic value
(define (sa:basic-singlearg-atomic expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 1)
   (let ((pair
          (sa:analyze-expr (car (sa:op-args expr)) vars funcs ns-binding default-ns uri modules)))
     (and
      pair
      (cons (list (sa:op-name expr)
                  (car pair))
            sa:type-atomic)))))

; Functions with zero or more arguments, return atomic values
; fn:count, fn:sum, fn:avg, fn:min, fn:max, fn:distinct-values, fn:concat
(define (sa:basic-multiarg-atomic expr vars funcs ns-binding default-ns uri modules)
  (let ((args-res
         (map
          (lambda (subexpr)
            (sa:analyze-expr subexpr vars funcs ns-binding default-ns uri modules))
          (sa:op-args expr))))
    (if
     (memv #f args-res)  ; error detected
     #f
     (cons (cons (sa:op-name expr)
                 (map car args-res))           
           sa:type-atomic))))

(define (sa:basic-replace expr vars funcs ns-binding default-ns uri modules)
  (cond
    ((> (length (sa:op-args expr)) 4)  ; too many arguments
     (sa:assert-num-args expr 4)  ; will raise an error
     )
    ((< (length (sa:op-args expr)) 3)  ; too many arguments
     (sa:assert-num-args expr 3)  ; will raise an error
     )
    (else
     (sa:propagate expr vars funcs ns-binding default-ns uri modules sa:type-atomic))))

;-------------------------------------------------
; Function call

; namespace-uri, local-name - function name
; arity - number of arguments in the function call
; Returns either the corresponding '(declare-function ...)
; S-expression or #f
(define (sa:find-function-declaration-by-name-and-arity
         namespace-uri local-name arity funcs)
  (let loop ((fs funcs))
    (cond
      ((null? fs)  ; all functions scanned
       #f)
      ((and (string=? (caar fs) namespace-uri)
            (string=? (cadar fs) local-name)
            (>= arity
                ; Minimal number of arguments
                (list-ref (car fs) 2))
            (or
             (not  ; infinite max number of arguments
              (list-ref (car fs) 3))
             (<= arity
                 (list-ref (car fs) 3))))
       (car fs))
      (else
       (loop (cdr fs))))))

; fun-call - something like  '(fun-call
;                               (const (type !xs!QName) ("local" "myName"))
;                               (const (type !xs!integer) "4"))
; Returns either the corresponding '(declare-function ...)
; S-expression or #f
; Function call expression is considered syntactically correct
; The function can raise the exception if the function name cannot be
; properly expanded
(define (sa:find-declaration-for-function-call
         fun-call funcs ns-binding default-ns uri modules)
  (let ((fun-name (sa:resolve-qname (car (sa:op-args fun-call))
                                    ns-binding (cadr default-ns))))
    (and
     fun-name
     (let ((name-parts (sa:proper-qname fun-name)))
       (sa:find-function-declaration-by-name-and-arity
        (car name-parts)
        (cadr name-parts)
        (length  ; arity
         (cdr (sa:op-args fun-call)))
        funcs)))))

; Function call
(define (sa:analyze-fun-call expr vars funcs ns-binding default-ns uri modules)
  (let ((args (sa:op-args expr)))
    (if
     (null? args)  ; no arguments
     (cl:signal-input-error SE5036 expr)
     (let ((fun-name (sa:resolve-qname
                      (car args) ns-binding (cadr default-ns))))
       (and
        fun-name
        (let ((name-parts (sa:proper-qname fun-name))
              (num-actual (length  ; arity
                           (cdr (sa:op-args expr)))))
          (cond
            ((sa:find-function-declaration-by-name-and-arity
              (car name-parts) (cadr name-parts)
              num-actual
              funcs)
             => (lambda (fun-declaration)
                  (let ((formal-args
                         ((list-ref fun-declaration 4) num-actual))
                        ; Actual function arguments are to be analyzed ONLY
                        ; after the function name and the proper number of
                        ; arguments are matched
                        (actual-args
                         (map
                          (lambda (subexpr)
                            (sa:analyze-expr
                             subexpr vars funcs ns-binding default-ns uri modules))
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
                            (list-ref fun-declaration 5))
                           vars funcs
                           ns-binding default-ns
                           uri modules))
                         ((and (eq? (car form) sa:type-nodes)
                               (eq? (cdar act) sa:type-atomic))
                          (cl:signal-user-error XPTY0004 expr)  ; was: SE5039
                          )
                         (else
                          (rpt (cdr form) (cdr act)))))))))
            ((and uri  ; this is a library module
                  (assoc (car name-parts) modules))
             ; Function probably declared in another module
             (let ((actual-args
                    (map
                     (lambda (subexpr)
                       (sa:analyze-expr
                        subexpr vars funcs ns-binding default-ns uri modules))
                     (cdr args))))
               (and
                (not (memv #f actual-args))  ; no errors in arguments
                (cons
                 (cons (sa:op-name expr) ; ='fun-call
                       (cons fun-name
                             (map car actual-args)))
                 sa:type-any))))
            (else  ; Function declaration not found
             (cl:signal-user-error
              XPST0017  ; was: SE5037
              (string-append (car name-parts) ":" (cadr name-parts))
              ;                (cadr (caddr  ; extract function name
              ;                       (car (sa:op-args expr))))
              ; expr
              )))))))))


;==========================================================================
; Different kinds of queries
; This includes update operations, manage and retrieve-metadata

;-------------------------------------------------
; Update operations
; This is section 3.8 in the XQuery logical representation

(define (sa:analyze-update expr vars funcs ns-binding default-ns uri modules)
  (if
   (or (not (pair? expr)) (null? expr))
   (cl:signal-input-error SE5040 expr)
   (case (sa:op-name expr)  ; operation name       
     ((insert-into insert-preceding insert-following)
      (sa:analyze-insert expr vars funcs ns-binding default-ns uri modules))
     ((rename)
      (sa:analyze-rename expr vars funcs ns-binding default-ns uri modules))
     ((delete delete_undeep)
      (sa:analyze-delete expr vars funcs ns-binding default-ns uri modules))
     ((replace move-into move-preceding move-following)
      (sa:analyze-replace-move expr vars funcs ns-binding default-ns uri modules))
     (else
      (cl:signal-input-error SE5041 expr)))))

(define (sa:analyze-insert expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 2)
   (let ((frst-res
          (sa:analyze-expr
           (car (sa:op-args expr)) vars funcs ns-binding default-ns uri modules))
         (secnd-res
          (sa:analyze-expr
           (cadr (sa:op-args expr)) vars funcs ns-binding default-ns uri modules)))
     (and
      frst-res secnd-res
      (if
       (eq? (cdr secnd-res) sa:type-atomic)
       (cl:signal-user-error SE5042 expr)
       (list (sa:op-name expr)
             (car frst-res) (car secnd-res)))))))

(define (sa:analyze-rename expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 2)
   (let ((frst-res
          (sa:analyze-expr
           (car (sa:op-args expr)) vars funcs ns-binding default-ns uri modules)))
     (and
      frst-res 
      (if
       (eq? (cdr frst-res) sa:type-atomic)
       (cl:signal-user-error SE5043 expr)
       (list (sa:op-name expr)
             (car frst-res)
             (cadr (sa:op-args expr))   ; attention!!!
             ))))))

(define (sa:analyze-delete expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 1)
   (let ((arg-res
          (sa:analyze-expr
           (car (sa:op-args expr)) vars funcs ns-binding default-ns uri modules)))
     (if
      (eq? (cdr arg-res) sa:type-atomic)
      (cl:signal-user-error SE5044 expr)
      (list (sa:op-name expr)
            (car arg-res))))))

; TODO: probably, the first argument should always be a node()*
(define (sa:analyze-replace-move expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 2)
   (let ((ex-pair
          (sa:analyze-expr
           (car (sa:op-args expr)) vars funcs ns-binding default-ns uri modules))
         (fun-pair 
          (sa:analyze-fun-def
           (cadr (sa:op-args expr)) vars funcs ns-binding default-ns uri modules)))
     (and
      ex-pair fun-pair
      (list (sa:op-name expr)
            (car ex-pair) (car fun-pair))))))

;-------------------------------------------------
; Manage operations

(define (sa:analyze-manage expr vars funcs ns-binding default-ns uri modules)
  (cond
    ((or (not (pair? expr)) (null? expr))
     (cl:signal-input-error SE5045 expr))
    ((case (sa:op-name expr)  ; operation name
       ((create-collection drop-collection)
        (sa:analyze-manage-collection
         expr vars funcs ns-binding default-ns uri modules))
       ((create-document drop-document)
        (sa:analyze-manage-document
         expr vars funcs ns-binding default-ns uri modules))
       ((load)
        (sa:analyze-manage-load
         expr vars funcs ns-binding default-ns uri modules))
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
        (sa:analyze-index-create expr vars funcs ns-binding default-ns uri modules))
       ((drop-index)
        (sa:analyze-index-drop expr vars funcs ns-binding default-ns uri modules))
       ;-------------------
       ; Full-text
       ((create-fulltext-index)
        (sa:analyze-full-text-create expr vars funcs ns-binding default-ns uri modules))
       ((drop-fulltext-index)
        (sa:analyze-full-text-drop expr vars funcs ns-binding default-ns uri modules))
       ;-------------------
       ; Trigger operations
       ((create-trigger)
        (sa:analyze-trigger-create expr vars funcs ns-binding default-ns uri modules))
       ((drop-trigger)
        (sa:analyze-trigger-drop expr vars funcs ns-binding default-ns uri modules))
       ;-------------------
       ; Module operations
       ((load-module load-or-replace-module)
        (sa:module-load expr))
       ((drop-module)
        (sa:module-drop expr))
       ;-------------------
       (else
        (cl:signal-input-error SE5046 expr)))
     => (lambda (new-expr) new-expr))
    (else  ; error already detected
     #f)))

(define (sa:analyze-manage-collection
         expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 1)
   (let ((new
          (sa:propagate expr vars funcs ns-binding default-ns uri modules
                        'sa:atomic  ; dummy
                        )))
     (and new (car new)))))

(define (sa:analyze-manage-document
         expr vars funcs ns-binding default-ns uri modules)
  (and
   (or
    (let ((lng (length (sa:op-args expr))))
      (and (> lng 0) (< lng 3)))
    (sa:assert-num-args expr 2))
   (let ((new
          (sa:propagate expr vars funcs ns-binding default-ns uri modules
                        'sa:atomic  ; dummy
                        )))
     (and new (car new)))))

(define (sa:analyze-manage-load expr vars funcs ns-binding default-ns uri modules)
  (and
   (or
    (let ((lng (length (sa:op-args expr))))
      (and (>= lng 2) (<= lng 3)))
    (sa:assert-num-args expr 3))
   (let ((new
          (sa:propagate expr vars funcs ns-binding default-ns uri modules
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

(define (sa:analyze-retrieve-metadata expr vars funcs ns-binding default-ns uri modules)
  (if
   (or (not (pair? expr)) (null? expr))
   (cl:signal-input-error SE5047 expr)
   (case (sa:op-name expr)  ; operation name
     ((retrieve-metadata-documents)
      (sa:analyze-metadata-documents
       expr vars funcs ns-binding default-ns uri modules))
     ((retrieve-metadata-collections)
      (sa:analyze-metadata-collections
       expr vars funcs ns-binding default-ns uri modules))
     ((retrieve-descr-scheme)
      (sa:analyze-descr-scheme
       expr vars funcs ns-binding default-ns uri modules))
     (else
      (cl:signal-input-error SE5048 expr)))))

(define (sa:analyze-metadata-documents
         expr vars funcs ns-binding default-ns uri modules)
  (if
   (> (length (sa:op-args expr)) 2)
   (sa:assert-num-args expr 2)  ; this will raise the error message
   (let ((new
          (sa:propagate expr vars funcs ns-binding default-ns uri modules
                        'sa:atomic  ; dummy
                        )))
     (and new (car new)))))

(define (sa:analyze-metadata-collections
         expr vars funcs ns-binding default-ns uri modules)
  (or
   (and 
    (null? (sa:op-args expr))  ; no arguments
    expr)
   (and
    (sa:assert-num-args expr 1)
    (let ((new
          (sa:propagate expr vars funcs ns-binding default-ns uri modules
                        'sa:atomic  ; dummy
                        )))
      (and new (car new))))))

(define (sa:analyze-descr-scheme expr vars funcs ns-binding default-ns uri modules)
  (and
   (or
    (let ((lng (length (sa:op-args expr))))
      (and (> lng 0) (< lng 3)))
    (sa:assert-num-args expr 2))
   (let ((new
          (sa:propagate expr vars funcs ns-binding default-ns uri modules
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

(define (sa:analyze-index-create expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 4)
   (let ((arg1 (sa:analyze-expr (car (sa:op-args expr))
                                vars funcs ns-binding default-ns uri modules))
         (arg2 (sa:analyze-expr (cadr (sa:op-args expr))
                                vars funcs ns-binding default-ns uri modules))
         (arg3 (sa:analyze-expr (caddr (sa:op-args expr))
                                (cons
                                 (cons (cadr sa:context-item)
                                       sa:type-nodes)
                                 vars)
                                funcs ns-binding default-ns uri modules))
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

(define (sa:analyze-index-drop expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 1)
   (let ((new
          (sa:propagate expr vars funcs ns-binding default-ns uri modules
                        'sa:atomic  ; dummy
                        )))
      (and new (car new)))))

; Post processing of the function call
(define (sa:fun-call-post-proc pair vars funcs ns-binding default-ns uri modules)
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
      ((!fn!name !fn!namespace-uri !fn!string-length !fn!normalize-space
                 !fn!string !fn!local-name !fn!number !fn!base-uri
                 !fn!root)
       (if
        (null? (cdr expr))  ; no argument
        (let ((context-pair
               (sa:analyze-expr
                sa:context-item  ; adding context item as argument
                vars funcs ns-binding default-ns uri modules)))
          (and
           context-pair
           (cons (list
                  (sa:op-name expr)  ; function name
                  (if
                   (eq? (sa:op-name expr) '!fn!normalize-space)
                   `(!fn!string ,(car context-pair))
                   (car context-pair)))
                 (cdr pair)  ; return type
                 )))
        pair))
      ((!fn!position !fn!last)
       ; See XQTS tests like "position-2"
       (and
        (sa:analyze-expr  ; context item defined
         sa:context-item
         vars funcs ns-binding default-ns uri modules)
        pair))
      ((!fn!contains)
       (if  ; collation supplied?
        (= (length (sa:op-args expr)) 3)
        (if
         (equal?  ; default collation?
          (caddr (sa:op-args expr))
          '(const
            (type !xs!string)
            "http://www.w3.org/2005/xpath-functions/collation/codepoint"))
         (cons  ; removing the 3rd argument from function call
          (reverse (cdr (reverse (car pair))))
          (cdr pair))
         (cl:signal-user-error
          FOCH0002  ; Was: FOCH0004
          (let ((collation (caddr (sa:op-args expr))))
            (if (and (pair? collation)
                     (eq? (sa:op-name collation) 'const))
                (string-append (caddr collation) " in fn:contains")
                "fn:contains"))))
        pair))
      ((!fn!lang)
       (let ((second-arg
              (if
               ; Second argument not supplied explicitly
               (null? (cdr (sa:op-args expr)))               
               (let ((context-pair
                      (sa:analyze-expr
                       sa:context-item  ; adding context item as argument
                       vars funcs ns-binding default-ns uri modules)))
                 (and
                  context-pair
                  (car context-pair)))
               (cadr (sa:op-args expr)))))
         (and
          second-arg
          ; Rewriting the function call to fn:lang into logical representation
          ; for the following XQuery expression:
          ;  let $testlang as xs:string? := fn:lower-case(fn:string("arg1")),
          ;      $node     as node()     := <arg2/>,
          ;      $attr     as node()?    := 
          ;         $node/ancestor-or-self::*[@xml:lang][1]/@xml:lang
          ;  return
          ;   (fn:not(fn:empty($attr))
          ;    and
          ;    (let $lang_value as xs:string := fn:lower-case(fn:string($attr))
          ;    return
          ;     (($lang_value eq $testlang) or
          ;      fn:starts-with(
          ;         $lang_value,
          ;         fn:concat($testlang, '-')
          ;      ))
          ;   ))
          (cons
           `(let@
                (!fn!lower-case (!fn!string ,(car (sa:op-args expr))))
              (fun-def
               (((optional !xs!string) (var ("" "testlang"))))
               (let@
                   ,second-arg
                 (fun-def
                  (((one (node-test)) (var ("" "node"))))
                  (let@
                      (ddo
                       (attr-axis
                        (ddo
                         (return
                          (var ("" "node"))
                          (fun-def
                           ((!xs!anyType (var ("" "$%v"))))
                           (predicate
                            (predicate
                             (ancestor-or-self
                              (var ("" "$%v"))
                              (type
                               (elem-test
                                (ename
                                 (const (type !xs!QName) *)
                                 (type *)
                                 (const (type !xs!string) "non-nil")))))
                             (fun-def
                              ((!xs!anyType (var ("" "$%v"))))
                              (ddo
                               (attr-axis
                                (var ("" "$%v"))
                                (type
                                 (attr-test
                                  (ename
                                   (const (type !xs!QName) ("xml" "lang"))
                                   (type *)
                                   (const (type !xs!string) "non-nil"))))))))
                            (fun-def
                             ((!xs!anyType (var ("" "$%v"))))
                             (const (type !xs!integer) "1"))))))
                        (type
                         (attr-test
                          (ename
                           (const (type !xs!QName) ("xml" "lang"))
                           (type *)
                           (const (type !xs!string) "non-nil"))))))
                    (fun-def
                     (((optional (node-test)) (var ("" "attr"))))
                     (and@
                      (!fn!not (!fn!empty (var ("" "attr"))))
                      (let@
                          (!fn!lower-case (!fn!string (var ("" "attr"))))
                        (fun-def
                         (((one !xs!string) (var ("" "lang_value"))))
                         (or@
                          (eq@ (var ("" "lang_value")) (var ("" "testlang")))
                          (!fn!starts-with
                           (var ("" "lang_value"))
                           (!fn!concat
                            (var ("" "testlang"))
                            (const (type !xs!string) "-")))))))))))))
           sa:type-atomic))))
      ((!fn!id)
       (let ((second-arg
              (if
               ; Second argument not supplied explicitly
               (null? (cdr (sa:op-args expr)))               
               (let ((context-pair
                      (sa:analyze-expr
                       sa:context-item  ; adding context item as argument
                       vars funcs ns-binding default-ns uri modules)))
                 (and
                  context-pair
                  (car context-pair)))
               (cadr (sa:op-args expr)))))
         (and
          second-arg
          ; Rewriting the function call to fn:id and !fn!idref into logical
          ; representation for the following XQuery expression:
          ;  let $arg      as xs:string* := "arg1",
          ;      $node     as node()     := <arg2/>,
          ;      $patterns as xs:string* :=
          ;        for $s in $arg
          ;        return
          ;         fn:concat(' ', fn:normalize-space($s), ' ')
          ;  return
          ;   $node/ancestor-or-self::node()[last()]/
          ;    descendant-or-self::*[@*
          ;     [self::xml:id or fn:ends-with(fn:lower-case(fn:local-name(.)), 'id')]
          ;     [let $attr_value as xs:string := fn:concat(' ', fn:string(.), ' ')
          ;      return
          ;       some $s as xs:string in $patterns
          ;       satisfies fn:contains($s, $attr_value)]]
          (cons
           `(let@
                ,(car (sa:op-args expr))
              (fun-def
               (((zero-or-more !xs!string) (var ("" "arg"))))
               (let@
                   ,second-arg
                 (fun-def
                  (((one (node-test)) (var ("" "node"))))
                  (let@
                      (return
                       (var ("" "arg"))
                       (fun-def
                        ((xs:anyType (var ("" "s"))))
                        (!fn!concat
                         (const (type !xs!string) " ")
                         (!fn!normalize-space (var ("" "s")))
                         (const (type !xs!string) " "))))
                    (fun-def
                     (((zero-or-more !xs!string) (var ("" "patterns"))))
                     (ddo
                      (return
                       (ddo
                        (return
                         (var ("" "node"))
                         (fun-def
                          ((!xs!anyType (var ("" "$%v"))))
                          (predicate
                           (ancestor-or-self (var ("" "$%v")) (type (node-test)))
                           (fun-def ((!xs!anyType (var ("" "$%v")))) (!fn!last))))))
                       (fun-def
                        ((!xs!anyType (var ("" "$%v"))))
                        (predicate
                         (descendant-or-self
                          (var ("" "$%v"))
                          (type
                           (elem-test
                            (ename
                             (const (type !xs!QName) *)
                             (type *)
                             (const (type !xs!string) "non-nil")))))
                         (fun-def
                          ((!xs!anyType (var ("" "$%v"))))
                          (ddo
                           (return
                            (var ("" "$%v"))
                            (fun-def
                             ((!xs!anyType (var ("" "$%v"))))
                             (predicate
                              (predicate
                               (attr-axis
                                (var ("" "$%v"))
                                (type
                                 (attr-test
                                  (ename
                                   (const (type !xs!QName) *)
                                   (type *)
                                   (const (type !xs!string) "non-nil")))))
                               (fun-def
                                ((!xs!anyType (var ("" "$%v"))))
                                (or@
                                 (ddo
                                  (self
                                   (var ("" "$%v"))
                                   (type
                                    (elem-test
                                     (ename
                                      (const (type !xs!QName) ("xml" "id"))
                                      (type *)
                                      (const (type !xs!string) "non-nil"))))))
                                 (!fn!ends-with
                                  (!fn!lower-case (!fn!local-name (var ("" "$%v"))))
                                  (const (type !xs!string) "id")))))
                              (fun-def
                               ((!xs!anyType (var ("" "$%v"))))
                               (let@
                                   (!fn!concat
                                    (const (type !xs!string) " ")
                                    (!fn!string (var ("" "$%v")))
                                    (const (type !xs!string) " "))
                                 (fun-def
                                  (((one !xs!string) (var ("" "attr_value"))))
                                  (some
                                   (var ("" "patterns"))
                                   (fun-def
                                    ((xs:anyType (var ("" "s"))))
                                    (!fn!contains
                                     (var ("" "s"))
                                     (var ("" "attr_value")))))))))))))))))))))))
           sa:type-nodes))))
      ((!fn!idref)
       (let ((second-arg
              (if
               ; Second argument not supplied explicitly
               (null? (cdr (sa:op-args expr)))               
               (let ((context-pair
                      (sa:analyze-expr
                       sa:context-item  ; adding context item as argument
                       vars funcs ns-binding default-ns uri modules)))
                 (and
                  context-pair
                  (car context-pair)))
               (cadr (sa:op-args expr)))))
         (and
          second-arg
          ; Rewriting the function call to fn:id and !fn!idref into logical
          ; representation for the following XQuery expression:
          ;  let $arg      as xs:string* := "arg1",
          ;      $node     as node()     := <arg2/>,
          ;      $patterns as xs:string* :=
          ;        for $s in $arg
          ;        return
          ;         fn:concat(' ', fn:normalize-space($s), ' ')
          ;  return
          ;   $node/ancestor-or-self::node()[last()]//
          ;    @*
          ;     [fn:contains(fn:lower-case(fn:local-name(.)), 'idref')]
          ;     [let $attr_value as xs:string := fn:concat(' ', fn:string(.), ' ')
          ;      return
          ;       some $s as xs:string in $patterns
          ;       satisfies fn:contains($s, $attr_value)]
          (cons
           `(let@
                ,(car (sa:op-args expr))
              (fun-def
               (((zero-or-more !xs!string) (var ("" "arg"))))
               (let@
                   ,second-arg
                 (fun-def
                  (((one (node-test)) (var ("" "node"))))
                  (let@
                      (return
                       (var ("" "arg"))
                       (fun-def
                        ((xs:anyType (var ("" "s"))))
                        (!fn!concat
                         (const (type !xs!string) " ")
                         (!fn!normalize-space (var ("" "s")))
                         (const (type !xs!string) " "))))
                    (fun-def
                     (((zero-or-more !xs!string) (var ("" "patterns"))))
                     (ddo
                      (return
                       (ddo
                        (descendant-or-self
                         (ddo
                          (return
                           (var ("" "node"))
                           (fun-def
                            ((!xs!anyType (var ("" "$%v"))))
                            (predicate
                             (ancestor-or-self (var ("" "$%v")) (type (node-test)))
                             (fun-def ((!xs!anyType (var ("" "$%v")))) (!fn!last))))))
                         (type (node-test))))
                       (fun-def
                        ((!xs!anyType (var ("" "$%v"))))
                        (predicate
                         (predicate
                          (attr-axis
                           (var ("" "$%v"))
                           (type
                            (attr-test
                             (ename
                              (const (type !xs!QName) *)
                              (type *)
                              (const (type !xs!string) "non-nil")))))
                          (fun-def
                           ((!xs!anyType (var ("" "$%v"))))
                           (!fn!contains
                            (!fn!lower-case (!fn!local-name (var ("" "$%v"))))
                            (const (type !xs!string) "idref"))))
                         (fun-def
                          ((!xs!anyType (var ("" "$%v"))))
                          (let@
                              (!fn!concat
                               (const (type !xs!string) " ")
                               (!fn!string (var ("" "$%v")))
                               (const (type !xs!string) " "))
                            (fun-def
                             (((one !xs!string) (var ("" "attr_value"))))
                             (some
                              (var ("" "patterns"))
                              (fun-def
                               ((xs:anyType (var ("" "s"))))
                               (!fn!contains
                                (var ("" "s"))
                                (var ("" "attr_value"))))))))))))))))))
           sa:type-nodes))))
      ((cast)
       ; Special check for xs:QName constructor function
       ; See "3.12.5 Constructor Functions" in XQuery specification
       (if
        (equal? (cadr (sa:op-args expr))
                 '(type (one !xs!QName)))
        (cond
          ((and
            (pair? (car (sa:op-args expr)))
            (not (eq? (sa:op-name (car (sa:op-args expr))) 'const)))
           (cl:signal-user-error
            XPTY0004
            "xs:QName constructor function for non-constant argument"))
          ((equal? (car (sa:op-args expr))
                   '(const (type !xs!string) ""))
           (cl:signal-user-error
            FORG0001
            "Attempting to cast empty string as xs:QName"))
          (else
           pair))
        pair))
      (else  ; any other function call
       pair))))

;-------------------------------------------------
; Managing full-text index

(define (sa:analyze-full-text-create expr vars funcs ns-binding default-ns uri modules)
  (and
   (or
    (let ((lng (length (sa:op-args expr))))
      (and (>= lng 3) (<= lng 4)))
    (sa:assert-num-args expr 3))
   ; Index type is a string constant
   (sa:analyze-string-const (caddr (sa:op-args expr))
                            vars funcs ns-binding default-ns)
   (let ((new
          (sa:propagate expr vars funcs ns-binding default-ns uri modules
                        'sa:atomic  ; dummy
                        )))
     (and new (car new)))))

(define (sa:analyze-full-text-drop expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 1)
   (let ((new
          (sa:propagate expr vars funcs ns-binding default-ns uri modules
                        'sa:atomic  ; dummy
                        )))
      (and new (car new)))))

;-------------------------------------------------
; Trigger manage operations

(define (sa:analyze-trigger-create expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 6)
   (if
    (not
     (list? (list-ref (sa:op-args expr) 5)))
    (cl:signal-input-error SE5075
                           (list-ref (sa:op-args expr) 5))
    (let ((first  (sa:analyze-string-const (car (sa:op-args expr))
                                           vars funcs ns-binding default-ns))
          (second (sa:analyze-string-const (cadr (sa:op-args expr))
                                           vars funcs ns-binding default-ns))
          (third  (sa:analyze-string-const (caddr (sa:op-args expr))
                                           vars funcs ns-binding default-ns))
          (fourth (sa:analyze-expr (cadddr (sa:op-args expr))
                                   vars funcs ns-binding default-ns uri modules))
          (fifth  (sa:analyze-string-const (list-ref (sa:op-args expr) 4)
                                           vars funcs ns-binding default-ns))
          (sixth  (map
                   (lambda (statement)
                     ((if
                       (and (pair? statement)
                            (memq (sa:op-name statement)
                                  '(insert-into insert-preceding insert-following rename
                                                delete delete_undeep
                                                replace move-into move-preceding move-following)))
                       (lambda x
                         (cons
                          (apply sa:analyze-update x)
                          sa:type-nodes))
                       sa:analyze-expr)
                      statement vars funcs ns-binding default-ns uri modules))
                   (list-ref (sa:op-args expr) 5))))
      (and
       first second third fourth fifth
       (null? (filter 
               (lambda (x) (not x))
               sixth))
       (cond
         ((not (member (car second)
                       '((const (type !xs!string) "BEFORE")
                         (const (type !xs!string) "AFTER"))))
          (cl:signal-user-error SE5073
                                (caddr (car second))))
         ((not (member (car third)
                       '((const (type !xs!string) "INSERT")
                         (const (type !xs!string) "DELETE")
                         (const (type !xs!string) "REPLACE"))))
          (cl:signal-user-error SE5074
                                (caddr (car third))))
         ((not (member (car fifth)
                       '((const (type !xs!string) "NODE")
                         (const (type !xs!string) "STATEMENT"))))
          (cl:signal-user-error SE5076
                                (caddr (car fifth))))
         (else
          (list
           (sa:op-name expr)  ; operation name
           (car first)  ; remove argument type
           (car second)
           (car third)
           (sa:structural-absolute-xpath? (car fourth))
           (car fifth)
           (map car sixth)
           ))))))))

; Clone from sa:analyze-manage-document
(define (sa:analyze-trigger-drop
         expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 1)
   (let ((new
          (sa:propagate expr vars funcs ns-binding default-ns uri modules
                        'sa:atomic  ; dummy
                        )))
     (and new (car new)))))

;-------------------------------------------------
; XQuery module management

; args-inverter - a function that may invert argument list
(define (sa:module-helper num-args args-inverter)
  (lambda (expr)
    (and
     (sa:assert-num-args expr num-args)
     (let ((args
            (map
             (lambda (arg)
               (sa:analyze-string-const arg '() '() '() ""))
             (sa:op-args expr))))
       (and
        (not (memv #f args))
        (cons
         (sa:op-name expr)
         (args-inverter (map car args))))))))
        
(define sa:module-drop (sa:module-helper 1 (lambda (x) x)))
(define sa:module-load (sa:module-helper 2 reverse))


;==========================================================================
; Order by

;(ordermodifier 
; (const (type !xs!string) "asc")
; (const (type !xs!string) "empty-greatest"))
(define (sa:analyze-ordermodifier expr vars funcs ns-binding default-ns uri modules)
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
         ; Temporary solution for collations in ordermodifier
         (let ((collations
                (filter
                 (lambda (x)
                   (and (pair? x)
                        (eq? (sa:op-name x) 'collation)))
                 (list c1 c2))))
           (or
            (null? collations)
            (letrec ((sa:flatten
                      (lambda (lst)
                        (if
                         (pair? lst)
                         (append (sa:flatten (car lst))
                                 (sa:flatten (cdr lst)))
                         (list lst)))))
              (cl:signal-user-error
               XQST0076
               (apply string-append
                      (cons "" 
                            (filter string?
                                    (sa:flatten (car collations)))))))))
         (sa:analyze-string-const c1 '() '() '() sa:default-ns)
         (sa:analyze-string-const c2 '() '() '() sa:default-ns)
         (let ((v1 (caddr c1))  ; value of the first constant
               (v2 (caddr c2)))
           (cond
             ((and 
               (= (length (sa:op-args expr)) 1)
               (member v1 '("empty-greatest" "empty-least" "default")))
              ; The first argument omitted
              (cons
               (list (sa:op-name expr)  ; == 'ordermodifier
                     '(const (type !xs!string) "asc")
                     c1)
               sa:type-any))
             ((not (member v1 '("asc" "desc")))
              (cl:signal-input-error SE5061 v1))
             ((not (member v2 '("empty-greatest" "empty-least" "default")))
              (cl:signal-input-error SE5062 v1))
             (else
              (cons expr sa:type-any))))))))))

;(orderspec
; (ordermodifier ...)
; (*@ (var $x1) (var $x2) (var $x3) (var $x4)))
(define (sa:analyze-orderspec expr vars funcs ns-binding default-ns uri modules)
  (if
   (not (and (pair? expr) (eq? (sa:op-name expr) 'orderspec)))
   (cl:signal-input-error SE5065 expr)
   (and
    (sa:assert-num-args expr 2)
    (let ((new-modifier
           (sa:analyze-ordermodifier
            (car (sa:op-args expr)) vars funcs ns-binding default-ns uri modules))
          (new-subexpr
           (sa:analyze-expr
            (cadr (sa:op-args expr)) vars funcs ns-binding default-ns uri modules)))
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
(define (sa:analyze-multiple-orderspecs expr vars funcs ns-binding default-ns uri modules)
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
               (sa:analyze-orderspec sub vars funcs ns-binding default-ns uri modules))
             (cdr (sa:op-args expr)))))
       (and
        (not (memv #f new-orderspec-lst))
        (cons
         (cons (sa:op-name expr)  ; ='orderspecs
               (cons (car (sa:op-args expr))  ; stable / non-stable
                     (map car new-orderspec-lst)))
         sa:type-any  ; we don't care about the type
         ))))))

(define (sa:analyze-order-by expr vars funcs ns-binding default-ns uri modules)
  (and
   (sa:assert-num-args expr 2)
   (let ((new-value
          (sa:analyze-expr (car (sa:op-args expr)) vars funcs ns-binding default-ns uri modules))
         (new-fun
          (sa:analyze-fun-def (cadr (sa:op-args expr)) vars funcs ns-binding default-ns uri modules)))
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


;==========================================================================
; XQuery module analysis 

;-------------------------------------------------
; Obtaining a single module

; Interface function
; Stub:
;(define (get-module uri)
;  (let ((string-port (open-output-string)))
;    (begin
;      (write       
;       (if
;        (< (string-length uri) 7)
;        `(lib-module
;          (module-decl (const (type !xs!NCName) foo)
;                       (const (type !xs!string) ,uri))
;          (prolog
;           (declare-function
;            (const (type !xs!QName) (,uri "fact"))
;            (((one !xs!integer) (var ("" "n"))))
;            (result-type (zero-or-more (item-test)))
;            (body
;             (if@
;              (eq@ (var ("" "n")) (const (type !xs!integer) "0"))
;              (const (type !xs!integer) "1")
;              (*@
;               (var ("" "n"))
;               (fun-call
;                (const (type !xs!QName) (,uri "fact"))
;                (-@ (var ("" "n")) (const (type !xs!integer) "1")))))))
;           (declare-global-var
;            (var (,uri "pi"))
;            (const (type !xs!decimal) "3.14")
;            (zero-or-more (item-test)))))
;        (let ((depend-on (substring uri 0 (- (string-length uri) 2))))
;        `(lib-module
;          (module-decl
;           (const (type !xs!NCName) foo)
;           (const (type !xs!string) ,uri))
;          (prolog
;           (import-module
;            (const (type !xs!NCName) math)
;            (const (type !xs!string) ,depend-on))
;           (declare-function
;            (const (type !xs!QName) (,uri "fact"))
;            (((one !xs!integer) (var ("" "n"))))
;            (result-type (zero-or-more (item-test)))
;            (body
;             (fun-call
;              (const (type !xs!QName) (,depend-on "fact"))
;              (var ("" "n")))))
;           (declare-global-var
;            (var (,uri "pi"))
;            (var (,depend-on "pi"))
;            (zero-or-more (item-test)))))))
;       string-port)
;      (get-output-string string-port))))

; Returns (list uri module vars funcs)
; ATTENTION: open-input-string is a non-R5RS function. However, it is
; supported in Chichen and in PLT, see
; http://www.ugcs.caltech.edu/manuals/lang/chicken-1.63/chicken_32.html
(define (sa:obtain-single-module uri)
  (let ((module-str (get-module uri)))
    (and
     module-str
     (let ((module (read (open-input-string module-str))))
       (and
        module
        (let ((prolog (sa:op-args
                     (cadr (sa:op-args module))  ; yields `(prolog ...)
                     )))
       (list
        uri
        module
        (map
         (lambda (expr)
           (let ((var-wrapped (car (sa:op-args expr)))
                 ;(type (caddr (sa:op-args expr)))
                 )
             (cons (car (sa:op-args var-wrapped))
                   ; TODO: more accurate typing
                   sa:type-any)))
         (filter
          (lambda (x)
            (and (pair? x) (eq? (sa:op-name x) 'declare-global-var)))
          prolog))
        (map
         (lambda (expr)
           (let ((fun-qname (car (sa:op-args expr)))
                 (formal-args (cadr (sa:op-args expr)))
                 ; TODO: more accurate typing
                 (return-type sa:type-any))
             (let ((qname-parts (sa:proper-qname fun-qname))
                   (arity (length formal-args)))
               (let ((ns-uri (car qname-parts))
                     (local (cadr qname-parts)))
                  (list                   
                   ns-uri local  ; name
                   arity arity ; min and max args
                   (let ((arg-types (map
                                     (lambda (x) sa:type-any)
                                     formal-args)))
                     (lambda (num-args) arg-types))
                   return-type)))))
         (filter
          (lambda (x)
            (and (pair? x) (eq? (sa:op-name x) 'declare-function)))
          prolog)))))))))
    
;-------------------------------------------------
; Working with multiple modules

(define (sa:all-identifiers-declared? expr vars funcs)
  (let ((dual
         ; Returns: (list (listof var-name)
         ;                (listof (list function-name arity)))
         (sa:free-variables-and-function-calls expr (map car vars))))
    (and
     (null?
      (map
       (lambda (var-name)
         (cl:signal-user-error
          XPST0008
          (if
           (string=? (car var-name) "")
           (cadr var-name)
           (string-append (car var-name) ":" (cadr var-name)))))
       (car dual)  ; undeclared variable names
       ))
      (null?
       (map
        (lambda (func-id)
          (let ((name-parts (car func-id))
                (arity (cadr func-id)))
            (cl:signal-user-error
             XPST0017
             (string-append
              "Function name == " (car name-parts) ":" (cadr name-parts)
              ", arity == " (number->string arity)))))
        (filter
         (lambda (func-id)
           (let ((func-name (car func-id))
                 (arity (cadr func-id)))
             (not
              (sa:find-function-declaration-by-name-and-arity
               (car func-name) (cadr func-name) arity funcs))))
         (cadr dual)  ; function names
         ))))))

; Returns (list new-modules module vars funcs) or #f
; new-modules contains the latter parameters
; NOTE: XQuery spec., 4.11, 4th paragraph:
; "Module imports are not transitive—that is, importing a module provides
; access only to function and variable declarations contained directly in
; the imported module."
(define (sa:obtain-module-recursive uri modules import-chain)
  (cond
    ((member uri import-chain)
     => (lambda (cycle)
          ; Recursive module dependency discovered
          (cl:signal-user-error
           XQST0093
           (string-append
            "Cycle in module import: " uri
            (apply string-append
                   (reverse (map
                             (lambda (str) (string-append ", " str))
                             cycle)))))))
    ((assoc uri modules)
     => (lambda (quad)
          ; Requested module already analyzed
          (cons modules (cdr quad))))
    (else
     (let ((requested (sa:obtain-single-module uri)))
       (and
        requested
        (let loop ((modules modules)
                   (to-import
                    (map
                     (lambda (import-decl)
                       (cadr (sa:op-args
                              ; Last argument is const with uri
                              (car (reverse (sa:op-args import-decl))))))
                     (filter
                      (lambda (x)
                        (and (pair? x)
                             (eq? (sa:op-name x) 'import-module)))
                      (sa:get-query-prolog (cadr requested)))))
                   (vars (caddr requested))
                   (funcs (cadddr requested)))
          (cond
            ((null? to-import)
             ; All recursive module imports processed
             (and
              (sa:all-identifiers-declared? (cadr requested) vars funcs)
              (cons
               (cons requested modules)
               (cdr requested))))
            ((sa:obtain-module-recursive
              (car to-import) modules (cons uri import-chain))
             => (lambda (nested)
                  (loop (append (car nested) modules)
                        (cdr to-import)
                        (append (caddr nested) vars)
                        (append (cadddr nested) funcs))))
            (else  ; error already displayed
             #f))))))))

; Extracts (listof prolog-declarations) from library module
; NOTE: module-decl implies a namespace prefix declaration
(define (sa:module->prolog lib-module)
  (let* ((module-decl (car (sa:op-args lib-module)))
         (prefix (cadr (sa:op-args
                        (car (sa:op-args module-decl))))))
    (cons
     `(declare-namespace ,prefix
                         ,(cadr (sa:op-args module-decl)))
     (filter
      (lambda (x)
        (not (and (pair? x)
                  (eq? (sa:op-name x) 'import-module))))
      (sa:get-query-prolog lib-module)))))

;-------------------------------------------------
; High-level library module analysis

; Returns: (list module-decl module-prefix module-uri) or #f
(define (sa:module-decl expr)
  (and
   (or (and (pair? expr)
            (eq? (sa:op-name expr) 'module-decl))
       (cl:signal-input-error SE5079 (sa:op-name expr)))
   (sa:assert-num-args expr 2)
   (let ((module-prefix
          (sa:analyze-const (car (sa:op-args expr))
                            '() '() '() ""))
         (module-uri
          (sa:analyze-string-const (cadr (sa:op-args expr))
                                   '() '() '() "")))
     (and
      module-prefix module-uri
      (or
       (symbol? (caddr (car module-prefix)))  ; prefix
       (cl:signal-input-error SE5008 (caddr (car module-prefix))))
      (let ((prefix (symbol->string (caddr (car module-prefix))))
            (ns-uri (caddr (car module-uri))))
        (cond
          ((equal? ns-uri "")
           (cl:signal-user-error XQST0088))
          ((or (member prefix '("xml" "xmlns"))
               (member ns-uri
                       '("http://www.w3.org/XML/1998/namespace")))
           => (lambda (reason)
                (cl:signal-user-error XQST0070 (car reason))))
          (else
           (list (list (sa:op-name expr)  ; == 'module-decl
                       (car module-prefix)
                       (car module-uri))
                 prefix ns-uri))))))))

; Example:
;(lib-module
;  (module-decl
;    (const (type !xs!NCName) math)
;    (const (type !xs!string) "http://example.org/math-functions"))
;  (prolog
;    (declare-function
;      (const (type !xs!QName) ("local" "f"))
;      ()
;      (result-type (zero-or-more (item-test)))
;      (body (const (type !xs!string) "petya")))))
(define (sa:analyze-module query)
  (and
   (or (pair? query)
       (cl:signal-input-error SE5003 query))
   (or (eq? (sa:op-name query) 'lib-module)
       (cl:signal-input-error SE5078 (sa:op-name query)))
   (sa:assert-num-args query 2)
   (let ((decl-pair (sa:module-decl (car (sa:op-args query)))))
     (and
      decl-pair
      (let ((prefix (cadr decl-pair))
            (ns-uri (caddr decl-pair)))
        (let ((prolog-res (sa:analyze-prolog
                           (sa:get-query-prolog query)
                           (list (cons prefix ns-uri))
                           ns-uri)))
          (and
           prolog-res  ; processed correctly
           (let ((new-prolog (car prolog-res)))
             (list (sa:op-name query)  ; == 'lib-module
                   (car decl-pair)
                   (cons 'prolog new-prolog))))))))))
