
; File:  por-cons.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

(declare (unit por-cons))

;; Optimization criteria for constructors
; For every constructor, adds either 1 or 0 as the additional argument.
; #t denotes that the constuctor is to be evaluated as the deep copy
; #f denotes that the constructor can be optimized
;
; Prefix for this module is porc:

;-------------------------------------------------
; Miscellaneous helpers

; Conventional filter
(define (filter pred lis)
  (let rpt ((l lis))		
    (if (null? l) 
      '()
       (if (pred (car l))
	 (cons (car l) (rpt (cdr l)))
	 (rpt (cdr l))))))

;; Implement 'or' as a function, so that we could 'apply' it
;(define (porc:or . args)
;  (if (null? args) #f (or (car args) (apply porc:or (cdr args)))))
;
; Boolean `or' for members of the list
(define (porc:lst-or lst)
  (cond
    ((null? lst) #f)
    ((car lst) #t)
    (else (porc:lst-or (cdr lst)))))

; Combination of map and append
(define (porc:map-append f lst)
  (if (null? lst)
      lst
      (append (f (car lst))
              (porc:map-append f (cdr lst)))))
  

;==========================================================================
; Higher-level functions

; Returns the new query
(define (porc:process-query query . debug-mode?)
  (let ((debug-mode?
         (and (not (null? debug-mode?))
              (car debug-mode?))))
    (if
     (not (and (list? query) (= (length query) 3)
               (eq? (car query) 'query)
               (pair? (cadr query))
               (eq? (caadr query) 'query-prolog)
               (pair? (caddr query))))             
     query  ; an error occured, query is kept as is
     (let ((query-prolog (cadr query))
           (query-essense (caddr query)))
       (list
        (car query)   ; = 'query
        (cons (car query-prolog)
              (porc:process-prolog (cdr query-prolog) debug-mode?))
        (porc:process-essense query-essense debug-mode?))))))

; Process the query prolog
; Query bodies may contain constructors
(define (porc:process-prolog prolog-decl-lst debug-mode?)
  (if
   (null? prolog-decl-lst)
   prolog-decl-lst
   (let ((curr-decl (car prolog-decl-lst)))
     (cons
      (if
       (and (pair? curr-decl) (not (null? curr-decl))
            (eq? (car curr-decl) 'PPFunDecl)  ; function declaration
            (= (length curr-decl) 5))
       (list (car curr-decl)  ; = 'PPFunDecl
             (cadr curr-decl)  ; int
             (caddr curr-decl)  ; args types
             (cadddr curr-decl)  ; result type
             (car
              (porc:process-phys-op
               (list-ref curr-decl 4) #t #f #f debug-mode?)))
       curr-decl)
      (porc:process-prolog (cdr prolog-decl-lst) debug-mode?)))))

; Processes query essense
(define (porc:process-essense query-essense debug-mode?)
  (cond
    ((not (and (pair? query-essense)
               (not (null? query-essense))))
     ; Error situation
     query-essense)
    ((memq (car query-essense)
           '(PPBulkLoad PPCreateIndex PPCreateMetadata
                        PPDropMetadata PPRetrieveDS PPRetrieveMetadata))
     ; These operations don't contain constructors
     query-essense)
    (else
     (car (porc:propagate
           porc:process-phys-op query-essense #t #f #f debug-mode?)))))


;==========================================================================
;  The following functions return
; (list  new-op  var-binding  prefix-redeclared?)
; var-binding ::= (listof var-num copy?)
; copy? - whether variable value is to be copied
; prefix-redeclared?

; Iterates a list of operations in the map style
(define (porc:map func op-lst copy? ns-prefix in-attr? debug-mode?)
  (let ((alist
         (map
          (lambda (op) (func op copy? ns-prefix in-attr? debug-mode?))
          op-lst)))
    (list (map car alist)
          (porc:map-append cadr alist)
          (porc:lst-or (map caddr alist)))))

; Propagates the operation
(define (porc:propagate func op copy? ns-prefix in-attr? debug-mode?)
  (let ((new
         (porc:map
          func (cdr op) copy? ns-prefix in-attr? debug-mode?)))
    (cons
     (cons (car op) (car new))
     (cdr new))))

;-------------------------------------------------
; The following functions accept 4 arguments
;  expr to be processed
;  copy? - whether the constructor is to be copied
;  ns-prefix - namespace prefix for a containing element.
; ns-prefix is either a string or #f(?)
;  in-attr? - whether the current expression is inside an
; attribute constructor

; PhysOp
(define (porc:process-phys-op
         expr copy? ns-prefix in-attr? debug-mode?)
  (cond
    ((or (not (pair? expr)) (null? expr))  ; atomic
     (list expr '() #f))
    ((and (number? (car expr))
          (= (length expr) 2))
     (let ((new
            (porc:process-op
             (cadr expr) copy? ns-prefix in-attr? debug-mode?)))
       (cond
         ((and (pair? (cadr expr))
                (eq? (caadr expr) 'PPDebug))
          (cons
           (if
            debug-mode?
            ; Avoid inserting a nested PPDebug
            (list (car expr)
                  (list (caadr expr)  ; == 'PPDebug
                        (cadadr expr)  ; original PPDebug arguments            
                        (caddr
                         (cadr  ; inner (PPDebug ...)
                          (caddr  ; `(tuple-size inner-PPDebug)
                           (car new)  ; outer (PPDebug ...)
                           )))))
            ; Removing this PPDebug
            (caddr (car new)))
           (cdr new)))
         ((and debug-mode?
               (pair? (car new))
               (not (eq? (caar new) 'PPDebug)))
          ; Wrapping into PPDebug
          (cons
           `(,(car expr)  ; tuple size
             (PPDebug
              (,(symbol->string (caar new))  ; physical operation name
               "")
              (,(car expr) ,(car new))))
           (cdr new)))
         (else  
          (cons (list (car expr)  ; number - tuple size
                      (car new))
                (cdr new))))))
    ((pair? (car expr))  ; a-la SXPath nodeset
     (porc:map
      porc:process-phys-op expr copy? ns-prefix in-attr? debug-mode?))
    (else  ; propagate
     (porc:propagate
      porc:process-phys-op expr copy? ns-prefix in-attr? debug-mode?))))
     
; Op
(define (porc:process-op op copy? ns-prefix in-attr? debug-mode?)
  (cond
    ((or (not (pair? op)) (null? op))
     (list op '() #f))
    ((eq? (car op) 'PPSequence)
     ; Sequence preserves the value of copy? and ns-prefix
     (porc:propagate
      porc:process-phys-op op copy? ns-prefix in-attr? debug-mode?))
    ((eq? (car op) 'PPIf)
     (let ((cnd  ; condition
            (porc:process-phys-op
             (cadr op) #t ns-prefix in-attr? debug-mode?))
           (altern
            (porc:map porc:process-phys-op
                      (cddr op) copy? ns-prefix in-attr? debug-mode?)))
       (list (cons (car op)  ;='PPIf
                   (cons (car cnd)
                         (car altern)))
             (append (cadr cnd) (cadr altern))
             (or (caddr cnd) (caddr altern)))))
    ; TODO: UNITE ELEMENT AND ATTRIBUTE IF THE 4-TH ARGUMENT
    ; IS ADDED TO ATTRIBUTE AS WELL
    ((and (eq? (car op) 'PPElement)
          (= (length op) 3))
     (let* ((this-ns-prefix
             (if (and (list? (cadr op))  ; not a null
                      (= (length (cadr op)) 2)
                      (string? (caadr op)))
                 (caadr op)  ; ns-prefix
                 #f  ; will match nothing
                 ))
            (name
             (porc:process-phys-op
              (cadr op) #t #f in-attr? debug-mode?))
            (value
             (porc:process-phys-op
              (caddr op) #f this-ns-prefix in-attr? debug-mode?)))
      (list
       (list (car op)  ; = 'PPElement or 'PPAttribute
             (car name)
             (car value)
             copy?   ; should copy or not
             (caddr value)  ; whether ns prefix redeclared
             )
       (append (cadr name) (cadr value))
       #f  ; we don't care
       )))
    ((and (eq? (car op) 'PPAttribute)
          (= (length op) 3))
     ; Attribute, pi, comment and computed text node constructors must
     ; have in-attr? == #t for their content
     (let ((name
            (porc:process-phys-op (cadr op) #t #f #t debug-mode?))
           (value
            ; Constructors inside attribute values have to be copied
            ; due to fn:string-value implicitly invoked
            (porc:process-phys-op (caddr op)
                                  #t  ; was: #f
                                  #f #t debug-mode?)))
      (list
       (list (car op)  ; = 'PPElement or 'PPAttribute
             (car name)
             (car value)
             copy?   ; should copy or not
             )
       (append (cadr name) (cadr value))
       #f  ; we don't care
       )))
    ((and (eq? (car op) 'PPPI)
          (= (length op) 3))
     (let ((name
            (porc:process-phys-op (cadr op) #t #f #t debug-mode?))
           (value
            ; Constructors inside PIs have to be copied
            ; due to fn:string-value implicitly invoked
            (porc:process-phys-op (caddr op) #t #f #t debug-mode?)))
      (list
       (list (car op)  ; = 'PPPI
             (car name)
             (car value)
             copy?   ; should copy or not
             )
       (append (cadr name) (cadr value))
       #f  ; we don't care
       )))
    ((and (eq? (car op) 'PPNamespace)
          (= (length op) 3))
     (list op '()
           (equal? ns-prefix (cadr op)))  ; redeclares the ns-prefix of interest
     )
    ((eq? (car op) 'PPComment)
     (let ((content
            (porc:process-phys-op (cadr op) #t #f #t debug-mode?)))
      (list
       (list (car op)  ; = 'PPComment
             (car content)
             copy?   ; should copy or not
             )
       (cadr content)
       #f  ; we don't care
       )))
    ((eq? (car op) 'PPDocument)
     (let ((content
            (porc:process-phys-op (cadr op)
                                  #f  ; don't copy inside document node
                                  #f #f debug-mode?)))
      (list
       (list (car op)  ; = 'PPDocument
             (car content))
       (cadr content)
       #f  ; we don't care
       )))
    ((eq? (car op) 'PPText)
     (let ((content
            (porc:process-phys-op (cadr op) #t #f #t debug-mode?)))
      (list
       (list (car op)  ; = 'PPText
             (car content)
             copy?   ; should copy or not
             )
       (cadr content)
       #f  ; we don't care
       )))
    ((and (eq? (car op) 'PPVariable)
          (= (length op) 2))
     (list op
           (list (cons (cadr op) copy?))
           #f))
    ((and (memq (car op) '(PPLet))  ; PPReturn
          (= (length op) 4)
          (list? (cadr op)))
     (let ((var-num (caadr op))
           (var-value (caddr op))
           (expr (cadddr op)))
       (let* ((new-expr
               (porc:process-phys-op expr copy? #f in-attr? debug-mode?))
              (new-value
               (porc:process-phys-op
                var-value
                (cond
                  ((assoc var-num (cadr new-expr))
                   => cdr)
                  (else #t))
                #f
                in-attr? debug-mode?)))
         (list
          (list (car op)  ; = 'PPLet
                (cadr op)  ; var num
                (car new-value)
                (car new-expr))
          (append (cadr new-value)
                  (filter
                   (lambda (pair)
                     (not (eq? (car pair) var-num)))
                   (cadr new-expr)))
          #f))))
    ((eq? (car op) 'PPSpaceSequence)
     ; The analogue of porc:propagate body
     (let ((new (porc:map porc:process-phys-op
                          (cdr op) copy? ns-prefix in-attr? debug-mode?)))
       (cons
        (cons (car op) ; == PPSpaceSequence
              (append (car new) (list in-attr?)))
        (cdr new))))
    (else  ; any other operation
     (porc:propagate porc:process-phys-op op #t #f in-attr? debug-mode?))))
