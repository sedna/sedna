
; File:  lr2por.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

(declare (unit lr2por) (uses lr2por-lib common-lib))

;-------------------------------------------------------------------------------
; lp2por

(define (l2p:trace x)
  (pp x)
  x)

;(define (fac n)
;  (if (zero? n) 1 (* n (fac (- n 1)))))
 
(define (l2p:lr2por query-in-lr)
;  (fac 100000000 )
  (let ((modules+prolog (filter
                         (lambda (x)
                           (and (pair? x)
                                (memq (car x) '(module prolog))))
                         query-in-lr)))
    (begin
      ; add all function names to the global func-list
      ; DL: moved here from l2p:lr-query-prolog2por
      (for-each
       ; DL: Every declare-function has a single arity value
       (lambda (y)
         (l2p:add-func-name!
          (list
           (l2p:qname->uri+local (cadr y))  ; function name
           (length (caddr y))  ; function arity
           ; DL: was: (caddr (cadr y))
           )))
       (filter
        (lambda (x) (eq? (car x) 'declare-function))
        (apply append (map cdr modules+prolog))))
      (let loop ((modules+prolog modules+prolog)
                 (PPquery-prolog '())  ; modules and prolog in PP
                 (var-binding '())
                 (next-var-num 0))
        (if
         (null? modules+prolog)
         (let ((PPquery-expr
                (l2p:lr-query-expr2por
                 ((lambda (query-expr)
                    (if (null? var-binding)
                        query-expr
                        (l2p:rename-vars var-binding query-expr)))
                  (if (eq? (car query-in-lr) 'query)
                      (assq 'query-body (cdr query-in-lr))
                      query-in-lr)))))
           (set! var-count 0)  ; DL: ad-hoc cleanup
           (set! funcs-map '())  ; DL: ad-hoc cleanup
           `(query ,@(reverse PPquery-prolog) ,PPquery-expr))
         (call-with-values
          (lambda ()
            (l2p:lr-query-prolog2por
             (car modules+prolog)
             next-var-num
             var-binding))
          (lambda (in-pp var-binding next-var-num)
            (loop (cdr modules+prolog)
                  (cons in-pp PPquery-prolog)
                  var-binding
                  next-var-num))))))))

; DL: was  
;  (if (eq? (car query-in-lr) 'query)
;      (let* ((PPquery-prolog (l2p:lr-query-prolog2por (cadr query-in-lr)))
;             (PPquery-expr (l2p:lr-query-expr2por (caddr query-in-lr)))
;            )
;        `(query ,PPquery-prolog ,PPquery-expr))
;      `(query (query-prolog) ,(l2p:lr-query-expr2por query-in-lr))
;  )

; qname-const:
; '(const (type !xs!QName) ("http://example.org/math-functions" "fact" "math"))
(define (l2p:qname->uri+local qname-const)
  (let ((name-triple
         (cadr (xlr:op-args qname-const))))
    (list (car name-triple) (cadr name-triple))))

; Helper for PPReturn for defining variable types
(define (l2p:make-por-arg-types arg-lst)
  (if
   (> (length arg-lst) 1)
   ; More than one variable - this case corresponds to the outermost
   ; generated PPReturn in order-by => types should not be specified
   '()
   (let ((por-arg-types
          (map
           (lambda (arg)
             (l2p:lr-sequenceType2por-sequenceType
              (if (symbol? (car arg))
                  `(one  ; occurrence indicator
                    ,(car arg))
                  (car arg))))
           arg-lst)))
     (if
      (null?  ; unknown types only
       (filter
        (lambda (arg)
          (not (memq (cadr arg)
                     '(xs:anyType !xs!anyType))))
        por-arg-types))
      '()
      por-arg-types))))

;-------------------------------------------------------------------------------

; lr-query-prolog2por
; DL: returns (values por-prolog var-binding next-var-num)
(define (l2p:lr-query-prolog2por query-prolog-in-lr next-var-num var-binding)
  (if
   (memq (car query-prolog-in-lr) '(prolog module))
   (begin
     (call-with-values
      (lambda ()
        (l2p:fold-prolog-decls 
         (filter
          (lambda (x)
            (not
             (eq? (car x) 'declare-external-function)))
          (cdr query-prolog-in-lr))
         next-var-num
         var-binding))
      (lambda (lr-prolog var-binding next-var-num)
        (values
         (cons (if (eq? (car query-prolog-in-lr) 'module)
                   'module 'query-prolog)
               (if
                (not (null?  ; there are option declarations
                      (filter
                       (lambda (x)
                         (and (pair? x) (eq? 'PPOptionDecl (car x))))
                       lr-prolog)))
                ; Unite them into one
                `((PPOptionDecl
                   ,@(map
                      cadr  ; single option declaration
                      (filter
                       (lambda (x)
                         (and (pair? x) (eq? 'PPOptionDecl (car x))))
                       lr-prolog)))
                  ,@(filter  ; the rest
                     (lambda (x)
                       (not (and (pair? x) (eq? 'PPOptionDecl (car x)))))
                     lr-prolog))
                lr-prolog))
         var-binding
         next-var-num))))
   (cl:signal-input-error SE4008 "argument is not query-prolog")))

; Returns (values por-prolog var-binding next-var-num)
(define (l2p:fold-prolog-decls prolog next-var-num var-binding)
  (let loop ((src prolog)
             (res '())
             (var-binding var-binding)
             (next-var-num next-var-num))
    (cond
      ((null? src)
       (values (reverse res) var-binding next-var-num))
      ((and (pair? (car src))
            (eq? (caar src) 'declare-global-var))
       (let ((expr (car src)))
         (let ((name  (cadr expr))
               (value (caddr expr))
               (type  (cadddr expr)))
         (loop
          (cdr src)
          (cons
           (begin
             (set! var-count 0)
             (let* ((por-value
                     (l2p:any-lr-node2por
                      (if (null? var-binding)
                          value
                          (l2p:rename-vars var-binding value))))
                    (context var-count))
               (begin
                 (set! var-count 0)
                 (list 'PPVarDecl
                       next-var-num
                       context
                       por-value
                       (l2p:lr-sequenceType2por-sequenceType type)))))
           res)
          (cons
           (list (cadr name)
                 (list  ; wrapped into a list to distinguish from local variable
                  next-var-num))
           var-binding)
          (+ next-var-num 1)))))
      (else
       (loop (cdr src)
             (cons
              (l2p:lr-prolog-decl2por (car src) var-binding)
              res)
             var-binding
             next-var-num)))))

(define (l2p:lr-prolog-decl2por prolog-decl var-binding)
  (cond
    ; TODO: rewrite as case
    ; *** 4.3 Boundary-space Declaration
    ((eq? (car prolog-decl) 'boundary-space-decl)
     `(PPBoundarySpaceDecl ,(caddr (cadr prolog-decl))))
    ; *** 4.4 - 4.7
    ((assq (car prolog-decl)
           '((declare-default-collation . PPDefaultCollationDecl)
             (declare-base-uri          . PPBaseURIDecl)
             (declare-construction      . PPConstructionDecl)
             (declare-order             . PPOrderingModeDecl)))
     => (lambda (pair)
          (list (cdr pair)
                (caddr (cadr prolog-decl)))))
    ; *** 4.8 Empty Order Declaration
    ((eq? (car prolog-decl) 'declare-default-order)
     `(PPEmptyOrderDecl
       ,(cdr (assoc (caddr (cadr prolog-decl))
                    '(("empty-greatest" . greatest)
                      ("empty-least" . least))))))
    ; *** 4.9 Copy-Namespaces Declaration
    ((eq? (car prolog-decl) 'declare-copy-namespaces)
     `(PPCopyNamespacesDecl ,(caddr (cadr prolog-decl))
                            ,(caddr (caddr prolog-decl))))
    ; *** 4.12 Namespace Declaration
    ((eq? (car prolog-decl) 'declare-namespace)
     (l2p:lr-decl-ns2por prolog-decl))
    ; *** 4.13 Default Namespace Declaration
    ((eq? (car prolog-decl) 'declare-default-element-namespace)
     (l2p:decl-def-elem-ns2por prolog-decl))
    ((eq? (car prolog-decl) 'declare-default-function-namespace)
     (l2p:decl-def-func-ns2por prolog-decl))
    ; *** 4.15 Function Declaration
    ((eq? (car prolog-decl) 'declare-function)
     (l2p:lr-named-fun-def2por prolog-decl var-binding))
    ; *** 4.16 Option Declaration
    ((eq? (car prolog-decl) 'declare-option)
     `(PPOptionDecl
       (,(let ((qname-triple
                (caddr (cadr prolog-decl))))
           (list (car qname-triple) (cadr qname-triple)))
        ,@(map
           (lambda (pair)
             (list (caddr (car pair)) (caddr (cadr pair))))
           (cddr prolog-decl)  ; name-value pairs
           ))))
    (else
     (cl:signal-input-error
      SE4008
      (string-append "unknown prolog declaration: "
                     (symbol->string (car prolog-decl)))))))

;-------------------------------------------------------------------------------
; l2p:lr-decl-ns2por - translates namespace declaration to the POR
(define (l2p:lr-decl-ns2por ns-decl)
  (if (eq? (car ns-decl) 'declare-namespace)
      `(PPNSDecl ,(symbol->string (cadr ns-decl)) ,(caddr (caddr ns-decl)))
      (cl:signal-input-error SE4008 (string-append "unknown namaspace declaration: "
                              (symbol->string (car ns-decl))))))

;-------------------------------------------------------------------------------
; l2p:decl-def-elem-ns2por - translates namespace declaration to the POR
(define (l2p:decl-def-elem-ns2por ns-decl)
  (if (eq? (car ns-decl) 'declare-default-element-namespace)
      `(PPDefNSDeclElem ,(caddr (cadr ns-decl)))
      (cl:signal-input-error SE4008 (string-append "unknown default element namaspace declaration: "
                              (symbol->string (car ns-decl))))))
  

; l2p:decl-def-func-ns2por - translates namespace declaration to the POR
(define (l2p:decl-def-func-ns2por ns-decl)
  (if (eq? (car ns-decl) 'declare-default-function-namespace)
      `(PPDefNSDeclFun ,(caddr (cadr ns-decl)))
      (cl:signal-input-error SE4008 (string-append "unknown default function namaspace declaration: "
                              (symbol->string (car ns-decl))))))

;-------------------------------------------------------------------------------
; lr-named-fun-def2por - translates named function definition to the POR one
(define (l2p:lr-named-fun-def2por fun-def-in-lr var-binding)
  (set! var-count '0)
  ;(l2p:add-func-name! (string->symbol (cadr (caddr (cadr fun-def-in-lr))))) ; update global function names list
  (if (not (eq? 'declare-function (car fun-def-in-lr)))
      (cl:signal-input-error SE4008 "wrong function definition")
      (let* ((args-types 
              (if (null? (caddr fun-def-in-lr))
                  '()
                   (map l2p:lr-sequenceType2por-sequenceType (map car (caddr fun-def-in-lr)))))
             (var-names (map cadadr (caddr fun-def-in-lr)))
             (result-type 
              (if (null? (cdr (cadddr fun-def-in-lr)))
                  '(zero_or_more (item))
              (l2p:lr-sequenceType2por-sequenceType (cadr (cadddr fun-def-in-lr)))))
             (body (cadr (cadddr (cdr fun-def-in-lr))))
             (args-num (length args-types))
             (vars-map (append (l2p:generate-map var-names)
                               var-binding))
             (new-expr1 (l2p:rename-vars vars-map body)) ;getting body with substituted parameters
             (new-expr2 (l2p:any-lr-node2por new-expr1))
             (context-size var-count))
        ;(display var-names)
        ;(display new-expr1)
        `(PPFunDecl ,context-size
                    ,args-types
                    ,result-type
                    ,new-expr2)
         )))

;-------------------------------------------------------------------------------
; lr-query-expr2por
(define (l2p:lr-query-expr2por query-in-lr)
 (if (null? (cdr query-in-lr))
     (cl:signal-input-error SE4008 "bad query body, it is empty")
     (begin
       (set! var-count '0)
       (cond  ((eq? 'query-body (car query-in-lr))
                (let ((query (l2p:any-lr-node2por (cadr query-in-lr)))
                      (context (if (eq? var-count 0) 0 var-count)))
                  (list 'PPQueryRoot context query)))
         
              ; DL: in the following 3 cases, cadr -> caddr
              ; because of the query prolog
               ((eq? 'update (car query-in-lr))
                 (l2p:any-lr-node2por (caddr query-in-lr)))
               ((eq? 'manage (car query-in-lr))
                 (l2p:any-lr-node2por (caddr query-in-lr)))
               ((eq? 'retrieve-metadata (car query-in-lr))
                 (l2p:any-lr-node2por (caddr query-in-lr)))

           
               (else (cl:signal-input-error SE4008 "argument is not query-body or update"))
  )
)))

;-------------------------------------------------------------------------------
; l2p:any-lr-node2por - translates any LR operation to the POR one
(define (l2p:any-lr-node2por node)

  (cond ;((symbol? node) `(1 (PPVariable ,node))) 
        ((list? node)
         (let ((op-name (car node))
               (node (cdr node))) 
           (cond
             ((eq? op-name 'var)
              ; The identifier for global variable is represented as
              ; (list identifier)
              `(1
                ,(if
                  (pair? (car node))
                  (begin
                    ;(pp node)
                    (if
                     (= (length (car node)) 2)
                     (cl:signal-input-error
                      SE4008
                      (string-append
                       "undeclared XQuery variable encountered: "
                       (caar node) (cadar node)))
                     `(PPGlobalVariable ,(caar node))))
                  `(PPVariable ,@node))))
             
             ; *** select ***
;             ((eq? op-name 'select)
;              (let* ((left-PhysOp (l2p:any-lr-node2por (cadr node)))
;                     (right-PhysOp (l2p:any-lr-node2por (caddr (caddr node))))
;                    )
;                `(,(l2p:tuple-size left-PhysOp)
;                   (PPSelect 
;                    ,(map  cadr (cadr (caddr node)))
;                    ,left-PhysOp
;                    ,right-PhysOp
;                   )
;                 )
;              )
;             )
              
             ; *** lselect ***
;             ((eq? op-name 'lselect)
;              (let* ((left-PhysOp (l2p:any-lr-node2por (cadr node)))
;                     (right-PhysOp (l2p:any-lr-node2por (caddr (caddr node))))
;                     (ts (l2p:tuple-size left-PhysOp))
;                    ) 
;                `(,ts
;                   (PPSelect 
;                    ,(map  cadr (cadr (caddr node)))
;                    (,ts (PPStore ,left-PhysOp))
;                    ,right-PhysOp
;                   )
;                 )
;               )
;             )
             ; *** return ***
             ((eq? op-name 'return)
              (let ((node (l2p:return-order-by node)))
              (let* ((new-fun-def (l2p:rename-vars2unique-numbers (cadr node)))
                     ;(right-operand
                     ; (substitute-var-value-in-fun-body context var-name value expr))
                     (left-PhysOp (l2p:any-lr-node2por (car node)))
                     (right-PhysOp (l2p:any-lr-node2por (caddr new-fun-def)))
                     (reverse-new-arg-lst (reverse (cadr new-fun-def))))
                (list
                 (l2p:tuple-size right-PhysOp)
                 (if
                  (or  ; no positional var
                   (null? (cdr reverse-new-arg-lst))  ; only a single argument
                   (not (memq
                         (caar reverse-new-arg-lst)  ; type of last argument
                         '(se:positional-var !se!positional-var)))
                   ; If first argument is an order-by, than the last variable
                   ; is not a positional-var, although it may have an
                   ; xs:positional-var type: when positional var of the for-clause
                   ; participates in the return-clause
                   (eq? (caar node) 'order-by))
                  `(PPReturn
                    ,(map cadr (cadr new-fun-def))
                    ,left-PhysOp
                    ,right-PhysOp
                    -1
                    ,@(l2p:make-por-arg-types (cadr new-fun-def)))
                  `(PPReturn
                    ,(map cadr (reverse (cdr reverse-new-arg-lst)))
                    ,left-PhysOp
                    ,right-PhysOp
                    ,(cadar reverse-new-arg-lst)  ; var-dsc for positional var argument
                    ,@(l2p:make-por-arg-types
                       (reverse (cdr reverse-new-arg-lst))))))
                )))
                                                  
             ; *** predicate ***
             ((eq? op-name 'predicate)
              (l2p:predicate2por node))
             
             ; *** order-by ***
             ((eq? op-name 'order-by)
              (l2p:order-by2por node))
             
             ((eq? op-name 'tmp-tuple)
              `(,(length node)
                (PPSTuple ,@(map l2p:any-lr-node2por node))))
             
             ; *** lreturn ***
             ((eq? op-name 'lreturn)
              (let ((node (l2p:return-order-by node)))
              (let* ((new-fun-def (l2p:rename-vars2unique-numbers (cadr node)))
                     ;(right-operand (substitute-var-value-in-fun-body context var-name value expr))
                     (left-PhysOp (l2p:any-lr-node2por (car node)))
                     (right-PhysOp (l2p:any-lr-node2por (caddr new-fun-def)))
                     (tsr (l2p:tuple-size right-PhysOp))
                     (tsl (l2p:tuple-size left-PhysOp))
                     (reverse-new-arg-lst (reverse (cadr new-fun-def))))
                (list
                 tsr
                 (if
                  (or  ; no positional var
                   (null? (cdr reverse-new-arg-lst))  ; only a single argument
                   (not (memq
                         (caar reverse-new-arg-lst)  ; type of last argument
                         '(se:positional-var !se!positional-var))))
                  `(PPReturn
                    ,(map cadr (cadr new-fun-def))
                    (,tsl (PPStore ,left-PhysOp))
                    ,right-PhysOp
                    -1
                    ,@(l2p:make-por-arg-types (cadr new-fun-def)))
                  `(PPReturn
                    ,(map cadr (reverse (cdr reverse-new-arg-lst)))
                    (,tsl (PPStore ,left-PhysOp))
                    ,right-PhysOp
                    ,(cadar reverse-new-arg-lst)
                    ,@(l2p:make-por-arg-types
                       (reverse (cdr reverse-new-arg-lst)))))))))
             
             ((memq op-name '(let@ slet@))
              (let* ((new-fun-def (l2p:rename-vars2unique-numbers (cadr node)))
                     ;(right-operand (substitute-var-value-in-fun-body context var-name value expr))
                     (left-PhysOp (l2p:any-lr-node2por (car node)))
                     (right-PhysOp (l2p:any-lr-node2por (caddr new-fun-def)))
                    )
                ;(pp new-fun-def)
                (let* ((lr-fun-arg-type
                        (car  ; argument type
                         (car  ; first argument
                          (cadr  ; argument list
                           (cadr node)  ; fun-def
                           ))))
                       (por-arg-type-list  ; argument type enclosed in list
                        (if
                         (memq lr-fun-arg-type '(xs:anyType !xs!anyType))
                         '()
                         (list
                          (l2p:lr-sequenceType2por-sequenceType
                           (if (symbol? lr-fun-arg-type)
                               `(one  ; occurrence indicator
                                 ,lr-fun-arg-type)
                               lr-fun-arg-type))))))
                       
                  `(,(l2p:tuple-size right-PhysOp)
                    (,(if (eq? op-name 'slet@) 'PPSLet 'PPLet)
                     ,(map  cadr (cadr new-fun-def))
                     ,left-PhysOp
                     ,right-PhysOp
                     ,@por-arg-type-list
                     )
                    )
              )
             ))

             ; *** axis ***
             ((or (eq? op-name 'child)
                  (eq? op-name 'attr-axis)
                  (eq? op-name 'parent)
                  (eq? op-name 'descendant)
                  (eq? op-name 'self)
                  (eq? op-name 'following)
                  (eq? op-name 'preceding)
                  (eq? op-name 'following-sibling)
                  (eq? op-name 'preceding-sibling)
                  (eq? op-name 'ancestor)
                  (eq? op-name 'ancestor-or-self)
                  (eq? op-name 'descendant-or-self)
                  (eq? op-name 'descendant-attr))
              (let ((AbsPath (l2p:findPPAbsPath `(,op-name ,@node))))
                              
                (if AbsPath
                  `(1 ,AbsPath)
                   (let* ((type (cadr node))
                          (test-type (caadr type))
                          (axis (l2p:lr-axis2por-axis op-name)))

                     (cond ((or (eq? test-type 'elem-test)
                             (eq? test-type 'attr-test))
                            (let* ((const (cadr (cadr (cadr type))))
                                   (const-value (caddr const)))
                              (if (list? const-value)
                                  (cond ((symbol? (car const-value))
                                         `(1 (,axis
                                              wildcard_star_ncname
                                              ,(cadr const-value) 
                                              ,(l2p:any-lr-node2por (car node)))))
                                        
                                        ((symbol? (cadr const-value))
                                         `(1 (,axis
                                              wildcard_ncname_star
                                              ,(car const-value) 
                                              ,(l2p:any-lr-node2por (car node)))))
                                        ((and (string? (car const-value))
                                              (string? (cadr const-value)))
                                         `(1 (,axis qname ,const-value, (l2p:any-lr-node2por (car node)))))
                                        (else (cl:signal-input-error SE4008 (string-append "bad const-value in path: "
                                                                    (l2p:list2string const-value)))))
                                  `(1 (,axis wildcard_star () ,(l2p:any-lr-node2por (car node))))
                              )
                            ))
                           
                           ((eq? test-type 'pi-test)
                            `(1
                              (,axis
                               ,(l2p:lr-test2por-test test-type)
                               ,(let ((pi (cadr type)))  ; pi == '(pi-test ...)
                                  (if
                                   (null? (cdr pi))  ; no target specified
                                   '()
                                   ;(list
                                    (caddr  ; constant value
                                     (cadr pi)  ; yields '(const (type ...) ...)
                                     );)
                                  ))
                               ,(l2p:any-lr-node2por (car node)))))

                           ((or (eq? test-type 'text-test)
                                (eq? test-type 'node-test)
                                (eq? test-type 'comment-test))
                            `(1 (,axis ,(l2p:lr-test2por-test test-type) () ,(l2p:any-lr-node2por (car node)))))
                           
                           (else (cl:signal-input-error SE4008 (string-append "unknown test-type: "
                                                         (symbol->string test-type)))))))
                ))

             ; *** element ***
             ((eq? op-name 'element)
              (if (and (eq? (caar node) `const)
                       (eq? (cadr (cadr (car node))) '!xs!QName))
                  `(1 (PPElement ,(caddr (car node))  ,(l2p:any-lr-node2por (cadr node))))
                  `(1 (PPElement ,(l2p:any-lr-node2por (car node)) ,(l2p:any-lr-node2por (cadr node))))
              )
             )
             ; *** attribute ***
             ((eq? op-name 'attribute) 
              (if (and (eq? (caar node) `const)
                       (eq? (cadr (cadr (car node))) '!xs!QName))
                  `(1 (PPAttribute ,(caddr (car node)) ,(l2p:any-lr-node2por (cadr node))))
                  `(1 (PPAttribute ,(l2p:any-lr-node2por (car node)) ,(l2p:any-lr-node2por (cadr node))))
              )
             )
             
             ; *** Processing-instruction (PI) constructor ***
             ; Analogue of attribute constructor
             ((eq? op-name 'pi)
              `(1 (PPPI
                    ,((if
                       (and (eq? (caar node) `const)
                            (eq? (cadr (cadr (car node))) '!xs!QName))
                       caddr l2p:any-lr-node2por)
                      (car node))                    
                    ,(l2p:any-lr-node2por
                      (if (null? (cdr node))  ; no PI body
                          '(sequence)
                          ; TODO: ensure that using '(sequence) is
                          ; semantically correct
                          (cadr node))))))
             
             ; *** namespace ***
             ((eq? op-name 'namespace)
              `(1 (PPNamespace
                   ,(caddr (car node))
                   ,(l2p:any-lr-node2por (cadr node)))))
             
             ; *** comment constructor ***
             ((eq? op-name 'comment)
              `(1 (PPComment
                   ,(l2p:any-lr-node2por (car node)))))
             
             ; *** document node constructor ***
             ((eq? op-name 'document)
              `(1 (PPDocument
                   ,(l2p:any-lr-node2por (car node)))))
             
             ; *** text node constructor ***
             ((eq? op-name 'text)
              `(1 (PPText
                   ,(l2p:any-lr-node2por (car node)))))

             ; *** some ***
             ((eq? op-name 'some)
              (let* ((select-left-PhysOp (l2p:any-lr-node2por (car node)))
                     (tsls (l2p:tuple-size select-left-PhysOp))
                     (new-fun-def (l2p:rename-vars2unique-numbers (cadr node)))
                     (select-right-PhysOp (l2p:any-lr-node2por (caddr new-fun-def)))
                    )
               `(1 (PPFnExists (,tsls (PPSelect ,(map  cadr (cadr new-fun-def))
                                                ,select-left-PhysOp
                                                ,select-right-PhysOp
                                      )
                               )
                   )
                )
              )
             )
   
             ; *** lsome ***
             ((eq? op-name 'lsome)
              (l2p:lsome->por node))
             
              ;*** every ***
              ((eq? op-name 'every)
                   (let* ((select-left-PhysOp (l2p:any-lr-node2por (car node)))
                          (tsls (l2p:tuple-size select-left-PhysOp))
                          (new-fun-def (l2p:rename-vars2unique-numbers (cadr node)))
                          (select-right-PhysOp (l2p:any-lr-node2por (caddr new-fun-def)))
                    )
               `(1 (PPFnEmpty (,tsls (PPSelect ,(map  cadr (cadr new-fun-def))
                                                ,select-left-PhysOp
                                                (1 (PPFnNot ,select-right-PhysOp))
                                      )
                               )
                   )
                )
              )
             )             
             

            
              ; *** levery ***
              ((eq? op-name 'levery)
               (l2p:levery->por node))

             ; *** exists ***
             ((eq? op-name 'exists)
              `(1 (PPFnExists  ,(l2p:any-lr-node2por (car node))))
             )


             
             ; *** const ***
             ((eq? op-name 'const) 
              (cond
                ((eq? (cadr node) 'true#) `(1 (PPFnTrue)))
                ((eq? (cadr node) 'false#) `(1 (PPFnFalse)))
                ((equal? (car node) '(type !xs!QName))
                 `(1 (PPxsQName ,@(cadr node))))
                (else
                 `(1
                   (PPConst
                    ,(cadr node)
                    ,(l2p:lr-atomic-type2por-atomic-type (cadar node)))))))
             ; *** if@ ***
             ((eq? op-name 'if@)
              (let* ((then-expr (l2p:any-lr-node2por (cadr node)))
                     (else-expr (l2p:any-lr-node2por (caddr node)))
                     (ts-then (l2p:tuple-size then-expr))
                     (ts-else (l2p:tuple-size else-expr))
                    )

                (cond
                  ((equal? else-expr '(1 (PPNil)))
                   `(,ts-then (PPIf
                               ,(l2p:any-lr-node2por (car node))
                               ,then-expr
                               (,ts-then (PPNil)))))                   
                  ((= ts-then ts-else)
                   `(,ts-then (PPIf
                               ,(l2p:any-lr-node2por (car node))
                               ,then-expr
                               ,else-expr)))
                  (else
                   (cl:signal-input-error
                    SE4008
                    "bad input logical plan: tuple-size of then expr not equal to tuple-size of else expr")))))
             
             ; *** General Comp ***
             ((eq? op-name '=@)
              `(1 (PPGeneralCompEQ ,(l2p:any-lr-node2por (car node)) ,(l2p:any-lr-node2por (cadr node)))))
             
             ((eq? op-name '!=@)
              `(1 (PPGeneralCompNE ,(l2p:any-lr-node2por (car node)) ,(l2p:any-lr-node2por (cadr node)))))
             
             ((eq? op-name '<=@)
              `(1 (PPGeneralCompLE ,(l2p:any-lr-node2por (car node)) ,(l2p:any-lr-node2por (cadr node)))))
             
             ((eq? op-name '>=@)
              `(1 (PPGeneralCompGE ,(l2p:any-lr-node2por (car node)) ,(l2p:any-lr-node2por (cadr node)))))
             
            
             ((eq? op-name '<@)
              `(1 (PPGeneralCompLT ,(l2p:any-lr-node2por (car node)) ,(l2p:any-lr-node2por (cadr node)))))
             
             ((eq? op-name '>@)
              `(1 (PPGeneralCompGT ,(l2p:any-lr-node2por (car node)) ,(l2p:any-lr-node2por (cadr node)))))
             
             ; *** NodeComp ***
             
             ((eq? op-name '<<@)
              `(1 (PPLTNodeComparison ,(l2p:any-lr-node2por (car node)) ,(l2p:any-lr-node2por (cadr node)))))

             ((eq? op-name '>>@)
              `(1 (PPGTNodeComparison ,(l2p:any-lr-node2por (car node)) ,(l2p:any-lr-node2por (cadr node)))))
             
             ((eq? op-name 'is@)
              `(1 (PPEQNodeComparison ,(l2p:any-lr-node2por (car node)) ,(l2p:any-lr-node2por (cadr node)))))
             
             ; DL: by analogue with 'is@
             ((eq? op-name 'to@)
              `(1 (PPRange ,(l2p:any-lr-node2por (car node))
                           ,(l2p:any-lr-node2por (cadr node)))))
             
             ; *** logical, ariphmetical, comparision operations ***
             ((or (eq? op-name 'and@)
                  (eq? op-name 'or@)
                  (eq? op-name 'not@)
                  (eq? op-name 'eq@)
                  (eq? op-name 'ne@)
                  (eq? op-name 'lt@)
                  (eq? op-name 'le@)
                  (eq? op-name 'gt@)
                  (eq? op-name 'ge@)
                  (eq? op-name '+@)
                  (eq? op-name 'unary+@)
                  (eq? op-name '-@)
                  (eq? op-name 'unary-@)
                  (eq? op-name '*@)
                  (eq? op-name '/@)
                  (eq? op-name 'mod@)
                  (eq? op-name 'idiv@)
              )
              (let ((calculate (l2p:build-PPCalculate `(,op-name ,@node))))
                `(1 (PPCalculate ,(cadr calculate) ,@(map l2p:any-lr-node2por (cddr calculate))))
              )
             )
             
             ; *** cast ***
             ((eq? op-name 'cast)
              (let ((expr (car node))
                    (type (cadr node)))
                `(1
                  (PPCast
                   ,(l2p:any-lr-node2por expr) 
                   ,@(if
                      (list? (cadr type))  ; sequence type
                      (list
                       (l2p:lr-atomic-type2por-atomic-type (cadadr type))
                       ; Can be a null sequence?
                       (and
                        (memq (caadr type) '(optional zero-or-more))
                        #t  ; produce boolean result
                        ))
                      ; atomic type
                      (list
                       (l2p:lr-atomic-type2por-atomic-type (cadr type))
                       #f))))))
             
             ; *** instance of ***             
             ((eq? op-name 'instance-of)
              `(1 (PPInstanceOf ,(l2p:any-lr-node2por (car node))
                                ,(l2p:lr-sequenceType2por-sequenceType (cadr (cadr node))))))
             
             ; *** treat as ***
             ((eq? op-name 'treat)
              `(1 (PPTreat ,(l2p:any-lr-node2por (car node))
                           ,(l2p:lr-sequenceType2por-sequenceType (cadr (cadr node))))))
             
             ; *** instance of ***
             ((eq? op-name 'instance-of)
              `(1 (PPInstanceOf ,(l2p:any-lr-node2por (car node))
                                ,(l2p:lr-sequenceType2por-sequenceType (cadr (cadr node))))))
             
             ; *** castable ***
             ((eq? op-name 'castable)
              (let ((single-type (cadr  ; addresses '(one type)
                                  (cadr node)  ; addresses '(type (one type))
                                  )))
              `(1 (PPCastable
                   ,(l2p:any-lr-node2por (car node))
                   ,(l2p:lr-atomic-type2por-atomic-type (cadr single-type))
                   ,(and (memq (car single-type) '(optional zero-or-more))
                         #t  ; boolean value
                         )))))
             
             ; *** typeswitch ***
             ((eq? op-name 'ts)
              (l2p:ts2por node))

             ; *** ddo ***
             ((eq? op-name 'ddo)
              `(1 (PPDDO ,(l2p:any-lr-node2por (car node)))))    


             ; *** sequence ***
             ((eq? op-name 'sequence)
              (if (null? node) 
                  '(1 (PPNil))
                  (let ((seq-operands (map l2p:any-lr-node2por node)))
                  `(,(l2p:tuple-size (car seq-operands)) (PPSequence ,@seq-operands)))))
             
             ; *** spaceseq ***
             ((or (eq? op-name 'space-sequence) (eq? op-name 'spaceseq))
              (if (null? node)
                  '(1 (PPNil))
                  (let ((seq-operands (map l2p:any-lr-node2por node)))
                  `(,(l2p:tuple-size (car seq-operands))
                    (PPSpaceSequence ,@seq-operands)))))
             
             ; *** union ***
             ((eq? op-name 'union@)
              `(1 (PPUnion ,(l2p:any-lr-node2por (car node)) ,(l2p:any-lr-node2por (cadr node))))
             )
             
             ; *** except ***
             ((eq? op-name 'except@)
              `(1 (PPExcept ,(l2p:any-lr-node2por (car node)) ,(l2p:any-lr-node2por (cadr node))))
             )
             
             ; *** intersect ***
             ((eq? op-name 'intersect@)
              `(1 (PPIntersect
                   ,(l2p:any-lr-node2por (car node))
                   ,(l2p:any-lr-node2por (cadr node))))
             )
                          
             ;----------------------------------------
             ; XQuery and XPath functions
             ((assq
               op-name
               '(;----------------------------------------
                 ; 2 Accessors
                 (!fn!node-name .    PPFnNodeName)
                 (!fn!nilled .       PPFnNilled)
                 (!fn!string .       PPFnString)
                 (!fn!data .         PPFnData)
                 (!fn!base-uri .     PPFnBaseURI)
                 (!fn!document-uri . PPFnDocumentURI)
                 ;----------------------------------------
                 ; 3 The Error Function
                 (!fn!error . PPFnError)
                 ;----------------------------------------
                 ; 4 The Trace Function
                 (!fn!trace . PPFnTrace)
                 ;----------------------------------------
                 ; 5 Constructor Functions
                 ; Most of the constructor functions are transformed to a cast
                 ; operation
                 (!fn!dateTime . PPFnDateTime)
                 ;----------------------------------------
                 ; 6 Functions and Operators on Numerics
                 ; *** 6.4 Functions on Numeric Values
                 (!fn!abs .                PPFnAbs)
                 (!fn!ceiling .            PPFnCeiling)
                 (!fn!floor .              PPFnFloor)
                 (!fn!round .              PPFnRound)
                 (!fn!round-half-to-even . PPFnRoundHalfToEven)
                 ;----------------------------------------
                 ; 7 Functions on Strings
                 ; *** 7.2 Functions to Assemble and Disassemble Strings
                 (!fn!codepoints-to-string . PPFnCodepointsToString)
                 (!fn!string-to-codepoints . PPFnStringToCodepoints)
                 ; *** 7.3 Equality and Comparison of Strings
                 (!fn!compare .         PPFnCompare)
                 (!fn!codepoint-equal . PPFnCodepointEqual)
                 ; *** 7.4 Functions on String Values
                 (!fn!concat .            PPFnConcat)
                 (!fn!string-join .       PPFnStringJoin)
                 (!fn!substring .         PPFnSubstring)
                 (!fn!string-length .     PPFnStringLength)
                 (!fn!normalize-space .   PPFnNormalizeSpace)
                 (!fn!normalize-unicode . PPFnNormalizeUnicode)
                 (!fn!upper-case .        PPFnUpperCase)
                 (!fn!lower-case .        PPFnLowerCase)
                 (!fn!translate .         PPFnTranslate)
                 (!fn!encode-for-uri .    PPFnEncodeForUri)
                 (!fn!iri-to-uri .        PPFnIriToUri)
                 (!fn!escape-html-uri .   PPFnEscapeHtmlUri)
                 ; *** 7.5 Functions Based on Substring Matching
                 (!fn!contains .          PPFnContains)
                 (!fn!starts-with .       PPFnStartsWith)
                 (!fn!ends-with .         PPFnEndsWith)
                 (!fn!substring-before .  PPFnSubstringBefore)
                 (!fn!substring-after .   PPFnSubstringAfter)
                 ; *** 7.6 String Functions that Use Pattern Matching
                 ; Functions !fn!matches !fn!replace have a different
                 ; physical representation and are mapped individually
                 (!fn!tokenize .          PPFnTokenize)
                 ;----------------------------------------
                 ; 8 Functions on anyURI
                 (!fn!resolve-uri . PPFnResolveUri)
                 ;----------------------------------------    
                 ; 9 Functions and Operators on Boolean Values
                 (!fn!true .  PPFnTrue)
                 (!fn!false . PPFnFalse)
                 (!fn!not .   PPFnNot)
                 ;----------------------------------------
                 ; 10 Functions and Operators on Durations, Dates and Times
                 ; *** 10.5 Component Extraction Functions on Durations, Dates and Times
                 (!fn!years-from-duration .    PPFnYearsFromDuration)
                 (!fn!months-from-duration .   PPFnMonthsFromDuration)
                 (!fn!days-from-duration .     PPFnDaysFromDuration)
                 (!fn!hours-from-duration .    PPFnHoursFromDuration)
                 (!fn!minutes-from-duration .  PPFnMinutesFromDuration)
                 (!fn!seconds-from-duration .  PPFnSecondsFromDuration)
                 (!fn!year-from-dateTime .     PPFnYearFromDateTime)
                 (!fn!month-from-dateTime .    PPFnMonthFromDateTime)
                 (!fn!day-from-dateTime .      PPFnDayFromDateTime)
                 (!fn!hours-from-dateTime .    PPFnHoursFromDateTime)
                 (!fn!minutes-from-dateTime .  PPFnMinutesFromDateTime)
                 (!fn!seconds-from-dateTime .  PPFnSecondsFromDateTime)
                 (!fn!timezone-from-dateTime . PPFnTimezoneFromDateTime)
                 (!fn!year-from-date .         PPFnYearFromDate)
                 (!fn!month-from-date .        PPFnMonthFromDate)
                 (!fn!day-from-date .          PPFnDayFromDate)
                 (!fn!timezone-from-date .     PPFnTimezoneFromDate)
                 (!fn!hours-from-time .        PPFnHoursFromTime)
                 (!fn!minutes-from-time .      PPFnMinutesFromTime)
                 (!fn!seconds-from-time .      PPFnSecondsFromTime)
                 (!fn!timezone-from-time .     PPFnTimezoneFromTime)
                 ; *** 10.7 Timezone Adjustment Functions on Dates and Time Values
                 (!fn!adjust-dateTime-to-timezone
                  . PPFnAdjustDateTimeToTimezone)
                 (!fn!adjust-date-to-timezone
                  . PPFnAdjustDateToTimezone)
                 (!fn!adjust-time-to-timezone
                  . PPFnAdjustTimeToTimezone)
                 ;----------------------------------------    
                 ; 11 Functions Related to QNames
                 (!fn!resolve-QName .            PPFnResolveQName)
                 (!fn!QName .                    PPFnQName)
                 (!fn!prefix-from-QName .        PPFnPrefixFromQName)
                 (!fn!local-name-from-QName .    PPFnLocalNameFromQName)
                 (!fn!namespace-uri-from-QName . PPFnNamespaceUriFromQName)
                 (!fn!namespace-uri-for-prefix . PPFnNamespaceUriForPrefix)
                 (!fn!in-scope-prefixes        . PPFnInScopePrefixes)
                 ;----------------------------------------
                 ; 14 Functions and Operators on Nodes
                 (!fn!name .          PPFnName)
                 (!fn!local-name .    PPFnLocalName)
                 (!fn!namespace-uri . PPFnNamespaceUri)
                 (!fn!number .        PPFnNumber)
                 (!fn!lang .          PPFnLang)
                 (!fn!root .          PPFnRoot)
                 ;----------------------------------------
                 ; 15 Functions and Operators on Sequences
                 (!fn!boolean .         PPFnBoolean)
                 (!fn!index-of .        PPFnIndexOf)
                 (!fn!empty .           PPFnEmpty)
                 (!fn!exists .          PPFnExists)
                 (!fn!distinct-values . PPFnDistinctValues)
                 (!fn!insert-before .   PPFnInsertBefore)
                 (!fn!remove .          PPFnRemove)
                 (!fn!reverse .         PPFnReverse)
                 (!fn!subsequence .     PPFnSubsequence)
                 (!fn!unordered .       PPFnUnordered)
                 ; *** 15.2 Functions That Test the Cardinality of Sequences
                 (!fn!zero-or-one .     PPFnZeroOrOne)
                 (!fn!one-or-more .     PPFnOneOrMore)
                 (!fn!exactly-one .     PPFnExactlyOne)
                 ; *** 15.3 Equals, Union, Intersection and Except
                 (!fn!deep-equal .      PPFnDeepEqual)
                 ; *** 15.4 Aggregate Functions
                 (!fn!count .           PPFnCount)
                 (!fn!avg .             PPFnAvg)
                 (!fn!max .             PPFnMax)
                 (!fn!min .             PPFnMin)
                 (!fn!sum .             PPFnSum)
                 ; *** 15.5 Functions and Operators that Generate Sequences
                 (!fn!id .              PPFnId)
                 (!fn!idref .           PPFnIdref)
                 (!fn!doc-available .   PPFnDocAvailable)
                 ;----------------------------------------
                 ; 16 Context Functions
                 (!fn!current-dateTime .  PPFnCurrentDateTime)
                 (!fn!current-date .      PPFnCurrentDate)
                 (!fn!current-time .      PPFnCurrentTime)
                 (!fn!implicit-timezone . PPFnImplicitTimezone)
                 (!fn!default-collation . PPFnDefaultCollation)
                 (!fn!static-base-uri .   PPFnStaticBaseUri)

                 
                 ))
              => (lambda (pair)
                   `(1 ,(cons (cdr pair)
                              (map l2p:any-lr-node2por node)))))
             
;             ((assq op-name '((!fn!max . PPFnMax)
;                              (!fn!min . PPFnMin)
;                              (!fn!sum . PPFnSum)))
;              ; Ignore the second argument
;              ; TODO: should be implemented in accordance with the
;              ; specification
;              => (lambda (pair)
;                   `(1 ,(list (cdr pair)
;                              (l2p:any-lr-node2por (car node))))))
             
             ;----------------------------------------
                   
             
             
                        
             ; *** string-value ***
             ((eq? op-name '!fn!string-value)
              `(1 (PPDmStringValue ,(l2p:any-lr-node2por (car node))))
             )

             ; *** typed-value ***
             ((eq? op-name '!fn!typed-value)
              `(1 (PPDmTypedValue ,(l2p:any-lr-node2por (car node))))
             )
                                      
             
             
             
             ; *** item-at ***
             ((eq? op-name '!fn!item-at)
              `(1 (PPFnItemAt ,(l2p:any-lr-node2por (car node)) ,(l2p:any-lr-node2por (cadr node))))
             )
            
                                            
            
         
             
             ; *** checkpoint ***
             ((eq? op-name '!fn!checkpoint) 
              `(1 (PPFnCheckpoint))
             )
             ((eq? op-name '!se!checkpoint) 
              `(1 (PPCheckpoint))
             )             
             
             ; *** !fn!replace ***
             ((eq? op-name '!fn!replace) 
              `(1 (PPPatMatch  pm_replace
                               ,(l2p:any-lr-node2por (car node))
                               ,(l2p:any-lr-node2por (cadr node))
                               ,(l2p:any-lr-node2por (caddr node))
                               ,@(if (null? (cdddr node))  ; no 4th argument
                                     '()
                                     (list (l2p:any-lr-node2por (cadddr node))))
                               )))
             
             ; *** matches ***
             ((eq? op-name '!fn!matches) 
              `(1 (PPPatMatch  pm_matches
                               ,(l2p:any-lr-node2por (car node))
                               ,(l2p:any-lr-node2por (cadr node))
                               ,@(if (null? (cddr node))  ; no 3rd argument
                                     '()
                                     (list (l2p:any-lr-node2por (caddr node))))
                               )))
             
             ; *** test ***
             ((eq? op-name '!fn!test) 
              `(1 (PPTest ,(l2p:any-lr-node2por (car node))))
             )             

             ; *** node-kind
             ((eq? op-name '!fn!node-kind) 
              `(1 (PPDmNodeKind ,(l2p:any-lr-node2por (car node))))
             )             
             
             ; *** !fn!document ***
             ((eq? op-name '!fn!document)
              (if (eq? (length node) 1)
                  `(1 (PPAbsPath (document ,(l2p:getDocorCollNamePor (car node))) ()))
                  `(1 (PPDocInCol ,(l2p:any-lr-node2por (cadr node))
                                  ,(l2p:any-lr-node2por (car node)))))
             )
             
             ; *** !fn!doc ***
             ((eq? op-name '!fn!doc)
              (if (eq? (length node) 1)
                  `(1 (PPAbsPath (document ,(l2p:getDocorCollNamePor (car node))) ()))
                  `(1 (PPDocInCol ,(l2p:any-lr-node2por (cadr node))
                                  ,(l2p:any-lr-node2por (car node)))))
             )             
             
             ; *** !fn!collection ***
             ((eq? op-name '!fn!collection)
              `(1 (PPAbsPath (collection ,(l2p:getDocorCollNamePor (car node))) ()))
             )



             ; *** !fn!sql-connect ***
             ((eq? op-name '!fn!sql-connect)
              `(1 (PPFnSQLConnect ,@(map l2p:any-lr-node2por node)))
             )       
             
             ; *** !fn!sql-prepare ***
             ((eq? op-name '!fn!sql-prepare)
              `(1 (PPFnSQLPrepare ,@(map l2p:any-lr-node2por node)))
             )             

             ; *** !fn!sql-execute ***
             ((eq? op-name '!fn!sql-execute)
              `(1 (PPFnSQLExecute ,@(map l2p:any-lr-node2por node)))
             )             

             ; *** !fn!sql-exec-update ***
             ((eq? op-name '!fn!sql-exec-update)
              `(1 (PPFnSQLExecUpdate ,@(map l2p:any-lr-node2por node)))
             )             

             ; *** !fn!sql-close ***
             ((eq? op-name '!fn!sql-close)
              `(1 (PPFnSQLClose ,@(map l2p:any-lr-node2por node)))
             )       
             
             ; *** !fn!sql-commit ***
             ((eq? op-name '!fn!sql-commit)
              `(1 (PPFnSQLCommit ,@(map l2p:any-lr-node2por node)))
             )                 

             ; *** !fn!sql-rollback ***
             ((eq? op-name '!fn!sql-rollback)
              `(1 (PPFnSQLRollback ,@(map l2p:any-lr-node2por node)))
             )               

             ; *** !fn!index-scan ***
             ((eq? op-name '!fn!index-scan)
              (let* ((ind-name (caddr (car node)))
                     (condition (l2p:lr-scan-cond2por-scan-cond (caddr (caddr node))))
                     )
              `(1 (PPIndexScan 
                   ,ind-name
                   ,(l2p:any-lr-node2por (cadr node))
                   ; DL: the 4th list member was: (1 (PPConst 0 !xs!integer))
                   (1 (PPConst "0" !xs!integer))
                   ,condition))))
             
             ; *** !fn!index-scan-between ***
             ((eq? op-name '!fn!index-scan-between)
              (let* ((ind-name (caddr (car node)))
                     (range (l2p:lr-range2por-range (caddr (cadddr node)))))
              `(1 (PPIndexScan 
                   ,ind-name
                   ,(l2p:any-lr-node2por (cadr node)) 
                   ,(l2p:any-lr-node2por (caddr node))
                   ,range))))
				   
			; *** !fn!ftindex-scan ***
             ((eq? op-name '!fn!ftindex-scan)
              `(1 (PPFtIndexScan ,@(map l2p:any-lr-node2por node)))
             )                 
			 
			; *** !fn!ftscan ***
             ((eq? op-name '!fn!ftscan)
              `(1 (PPFtScan ,@(map l2p:any-lr-node2por node)))
             )                 
             
			; *** !fn!fthighlight ***
             ((eq? op-name '!fn!fthighlight)
              `(1 (PPFtHighlight ,@(map l2p:any-lr-node2por node)))
             )                 
			 
			; *** !fn!fthighlight2 ***
             ((eq? op-name '!fn!fthighlight2)
              `(1 (PPFtHighlight2 ,@(map l2p:any-lr-node2por node)))
             )
             
             ; *** !fn!is_ancestor ***
             ((eq? op-name '!fn!is_ancestor)
              `(1 (PPANNodeComparison ,@(map l2p:any-lr-node2por node)))
             )
             
             ; *** !fn!filter_entry_level ***
             ((eq? op-name '!fn!filter_entry_level)
              `(1 (PPFEL ,(l2p:any-lr-node2por (car node))))
             )
			 
             ; *** scan ***
             ((eq? op-name 'scan)
              (let* ((entity (if (eq? (car (cadr node)) '!fn!document) 'document 'collection))
                     (ent-name (caddr (cadr (cadr node)))))
                     `(1 (PPScan ,(caddr (car node)) (,entity ,ent-name)))))
             
             ; *** filterad ***
             ((eq? op-name 'adfilter)
              `(1 (PPADFilter ,(l2p:any-lr-node2por (car node)) ,(l2p:any-lr-node2por (cadr node)))))
             
             ; *** filterda ***
             ((eq? op-name 'dafilter)
              `(1 (PPDAFilter ,(l2p:any-lr-node2por (car node)) ,(l2p:any-lr-node2por (cadr node)))))
             
             ; *** up ***
             ((eq? op-name 'up)
              `(1 (PPUp ,(l2p:any-lr-node2por (car node)) ,(caddr (cadr node)))))
           
             

             ; *** xjoin ***
;             ((eq? op-name 'xjoin)
;              (let* ((outer (l2p:any-lr-node2por (caddr (cddr node))))
;                     (inner (l2p:any-lr-node2por (cadddr (cddr node))))
;                     (ts-outer (l2p:tuple-size outer))
;                     (ts-inner (l2p:tuple-size inner))
;                    ) 
;                  
;              `(,(+ ts-outer ts-inner)
;                (PPAJoin ,outer ; outer
;                        (,ts-inner (PPStore ,inner)) ; inner
;                        ,(cadr (caddr (cadr node))) ; outer var-names
;                        ,(caddr (caddr (cadr node))) ; inner var-name
;                        ,(map cadr (xj:prepare-join-args-order (caddr (cadr node)) (caddr (cadddr node)))) ; order
;                        ,(l2p:any-lr-node2por (caddr (caddr node))) ; predicate
;                )
;               )
;              )
;             )
             ; *** semixjoin ***
;             ((eq? op-name 'semixjoin)
;              (let* ((outer (l2p:any-lr-node2por (cadr (cddr node))))
;                     (inner (l2p:any-lr-node2por (caddr (cddr node))))
;                     (ts-outer (l2p:tuple-size outer))
;                     (ts-inner (l2p:tuple-size inner))
;                    )
;                             
;               `(,ts-outer
;                 (PPASemiJoin ,outer ; outer
;                              (,ts-inner (PPStore ,inner)) ; inner
;                              ,(cadr (caddr (cadr node))) ; outer var-names
;                              ,(caddr (caddr (cadr node))) ; inner var-name
;                              ,(l2p:any-lr-node2por (caddr (caddr node))) ; predicate
;                 )
;                )
;              )
;             )
             
             ; *** congen1 *** The translation is not correct if congen1 multiplied be logical optimizer
             ((eq? op-name 'congen1)
              (let* ((seq (l2p:any-lr-node2por (car node)))
                     (ts-seq (+ (l2p:tuple-size seq) 1)))
              `(,ts-seq (PPConGen1 ,(l2p:gen-var) ,seq))))
             
             ; *** congen2 ***
             ((eq? op-name 'congen2)
              (let* ((seq (l2p:any-lr-node2por (car node)))
                     (ts-seq (+ (l2p:tuple-size seq) 2)))
              `(,ts-seq (PPConGen2 ,seq))))
              
             
             ; *** fun-call ***
             ((eq? op-name 'fun-call)
               (let ((func-index
                      (l2p:find-func-index
                       (list
                        (l2p:qname->uri+local (car node))  ; function name
                        (length (cdr node))  ; number of arguments
                        )
                       ; DL: was: (caddr (car node))
                       funcs-map)))
                 (if
                  (eq? func-index #f)
                  (cl:signal-input-error
                   SE4008
                   (string-append "unknown function call: "
                                  (cadr (caddr (car node)))
                                  ", arity == "
                                  (number->string (length (cdr node)))))
                  (let ((tuple-size 1)
                        (name-pair (caddr (car node))))
                    `(,tuple-size
                      (PPDebug
                       ("PPFunCall"
                        ,(if
                          (string=? (car name-pair) "")
                          (cadr name-pair)
                          (string-append
                           (car name-pair) ":" (cadr name-pair))))
                       (,tuple-size
                        (PPFunCall
                         ,func-index
                         ,@(map l2p:any-lr-node2por (cdr node))))))))))
             
             ; *** ext-fun-call ***
             ((eq? op-name 'ext-fun-call)
               (let* ((func-name (cadr (caddr (car node)))))
                  `(1 (PPExtFunCall  ,func-name ,@(map l2p:any-lr-node2por (cdr node))))))             
             
             ((eq? op-name 'insert-into)
              (let* ((left-operand (l2p:any-lr-node2por (car node)))
                     (left-context (if (eq? var-count 0) 0 (+ var-count 1)))
                     (right-operand (begin (set! var-count 0) (l2p:any-lr-node2por (cadr node)) ))
                     (right-context (if (eq? var-count 0) 0 (+ var-count 1))))
              `(PPInsertTo ,left-context ,left-operand ,right-context ,right-operand)
             ))

             ((eq? op-name 'insert-following)
              (let* ((left-operand (l2p:any-lr-node2por (car node)))
                     (left-context (if (eq? var-count 0) 0 (+ var-count 1)))
                     (right-operand (begin (set! var-count 0) (l2p:any-lr-node2por (cadr node)) ))
                     (right-context (if (eq? var-count 0) 0 (+ var-count 1))))
              `(PPInsertFollowing ,left-context ,left-operand ,right-context ,right-operand)
             ))
             
             ((eq? op-name 'insert-preceding)
              (let* ((left-operand (l2p:any-lr-node2por (car node)))
                     (left-context (if (eq? var-count 0) 0 (+ var-count 1)))
                     (right-operand (begin (set! var-count 0) (l2p:any-lr-node2por (cadr node)) ))
                     (right-context (if (eq? var-count 0) 0 (+ var-count 1))))
              `(PPInsertBefore ,left-context ,left-operand ,right-context ,right-operand)
             ))
             
             ((eq? op-name 'rename)
              (let* ((operand (begin (set! var-count 0) (l2p:any-lr-node2por (car node))))
                     (qname (cadr node))
                     (context (if (eq? var-count 0) 0 (+ var-count 1))))
              `(PPRename ,context ,operand ,qname)
             ))
             
             ((eq? op-name 'replace)
              (let* ((operand (l2p:any-lr-node2por (car node)))
                     (context (if (eq? var-count 0) 0 (+ var-count 1))))
              `(PPReplace ,context ,operand)))


             
             ((eq? op-name 'delete)
              (let* ((operand (l2p:any-lr-node2por (car node)))
                     (context (if (eq? var-count 0) 0 (+ var-count 1))))
              `(PPDeleteDeep ,context ,operand )
             ))

             ((eq? op-name 'delete_undeep)
              (let* ((operand (l2p:any-lr-node2por (car node)))
                     (context (if (eq? var-count 0) 0 (+ var-count 1))))
              `(PPDeleteUndeep ,context ,operand )
             ))
             
             ;---------------------
             ; Manage operations
             
             ((eq? op-name 'load)
              (let* ((client-file (l2p:any-lr-node2por (car node)))
                     (file-context (if (eq? var-count 0) 0 (+ var-count 1)))
                     (db-file (begin (set! var-count 0) (l2p:any-lr-node2por (cadr node))))
                     (db-file-context (if (eq? var-count 0) 0 (+ var-count 1))))
                (if (eq? (length node) 2)
                    `(PPBulkLoad ,file-context
                                 ,client-file
                                 ,db-file-context
                                 ,db-file)
                    (let* ((col (begin (set! var-count 0) (l2p:any-lr-node2por (caddr node))))
                           (col-context (if (eq? var-count 0) 0 (+ var-count 1))))
                    `(PPBulkLoad ,file-context
                                 ,client-file
                                 ,db-file-context
                                 ,db-file
                                 ,col-context
                                 ,col)))))

             
             
             ((eq? op-name 'create-document)
              (let* ((doc (l2p:any-lr-node2por (car node)))
                     (doc-context (if (eq? var-count 0) 0 (+ var-count 1))))
              (if (eq? (length node) 2)
                  (let* ((col (begin (set! var-count 0) (l2p:any-lr-node2por (cadr node))))
                         (col-context (if (eq? var-count 0) 0 (+ var-count 1))))
                  `(PPCreateDocumentInCollection ,doc-context
                                                 ,doc
                                                 ,col-context
                                                 ,col))
                  `(PPCreateDocument ,doc-context
                                     ,doc))))
             
             ; Module management
             ((assq op-name '((load-module . #f)
                              (load-or-replace-module . #t)))
              => (lambda (pair)
;                   (let*  ; order of evaluation is significant due to set!
;                       ((filename (l2p:any-lr-node2por (car node)))
;                        (file-context
;                         (if (eq? var-count 0) 0 (+ var-count 1)))
;                        (module-name-in-db
;                         (begin
;                           (set! var-count 0)
;                           (l2p:any-lr-node2por (cadr node))))
;                        (name-context
;                         (if (eq? var-count 0) 0 (+ var-count 1))))
                     `(PPLoadModule
                       ,(cdr pair)
                       ,@(apply
                          append
                          (map
                           (lambda (str-const)
                             (list
                              (if (eq? var-count 0) 0 (+ var-count 1))
                              (l2p:any-lr-node2por str-const)))
                           node))
;                           file-context
;                           filename
;                           name-context
;                           module-name-in-db
;                           (cdr pair)
                           )))

             ((eq? op-name 'drop-module)
              (let ((context (if (eq? var-count 0) 0 (+ var-count 1))))
              `(PPDropModule ,context
                             ,(l2p:any-lr-node2por (car node)))))
             
             ((eq? op-name 'create-collection)
              (let* ((col (l2p:any-lr-node2por (car node))))
              `(PPCreateCollection ,(if (eq? var-count 0) 0 (+ var-count 1))
                                   ,col)))

             ((eq? op-name 'create-index)
              (let* ((AbsPath (l2p:findPPAbsPath (cadr node)))
                    (AbsPath2 (l2p:findPPAbsPath (caddr node)))
                    (entity (cadr AbsPath))
                    (abs-path (caddr AbsPath))
                    (abs-path2 (caddr AbsPath2))
                    (type (cadr (l2p:lr-sequenceType2por-sequenceType (cadddr node))))
                    (ind-name (l2p:any-lr-node2por (car node))))
                `(PPCreateIndex ,entity ,abs-path ,abs-path2 ,type 
                                ,(if (eq? var-count 0) 0 (+ var-count 1))
                                ,ind-name)
              ))
             
             ((eq? op-name 'create-trigger)
              (let* ((time    (string->symbol
                               (caddr (cadr node))))
                     (event   (string->symbol
                               (caddr (caddr node))))
                     (AbsPath (l2p:findPPAbsPath (cadddr node)))
                     (entity (cadr AbsPath))
                     (abs-path (caddr AbsPath))
                     (granularity (string->symbol
                                   (caddr (list-ref node 4))))
                     (action (map
                              l2p:any-lr-node2por
                              (list-ref node 5)))
                     (name (l2p:any-lr-node2por (car node))))
                (if (= (length node) 9)
                     (let* ((insname (list-ref node 6))
                            (instype (list-ref node 7))
                            (ParentAbsPath (l2p:findPPAbsPath (list-ref node 8)))
                            (parent-path (caddr ParentAbsPath)))
                    `(PPCreateTrigger ,(if (eq? var-count 0) 0 (+ var-count 1))
                                  ,time
                                  ,event
                                  ,entity
                                  ,abs-path
                                  ,granularity
                                  ,action
                                  ,name
                                  ,insname
                                  ,instype
                                  ,parent-path))
                    `(PPCreateTrigger ,(if (eq? var-count 0) 0 (+ var-count 1))
                                  ,time
                                  ,event
                                  ,entity
                                  ,abs-path
                                  ,granularity
                                  ,action
                                  ,name))))
                               
             ((eq? op-name 'create-fulltext-index)
              ; ATTENTION: `node' is bound to the operation content, not the operation!
              (let ((ind-name (l2p:any-lr-node2por (car node)))
                    (AbsPath (l2p:findPPAbsPath (cadr node)))
                    (ind-type-str (caddr (caddr node))))
                (if
                 (not AbsPath)  ; not a proper absolute XPath supplied
                 (cl:signal-user-error
                  SE4008
                  (string-append
                   "Improper absolute location path supplied "
                   "for a CREATE FULL-TEXT INDEX statement"))
                 (let ((entity (cadr AbsPath))
                       (abs-path (caddr AbsPath)))
                  `(PPCreateFtIndex
                    ,(if (eq? var-count 0) 0 (+ var-count 1))
                    ,entity
                    ,abs-path
                    ,ind-type-str
                    ,ind-name
                    ,@(if (= (length node) 4)  ; optional parameters presented
                          (list (l2p:any-lr-node2por (list-ref node 3)))
                          '()))
                   ))))
             
             ((memv op-name '(drop-index drop-fulltext-index drop-collection))
              (let* ((ind-name (l2p:any-lr-node2por (car node))))
              `(,(cdr
                  (assq op-name '((drop-index . PPDropIndex)
                                  (drop-fulltext-index . PPDropFtIndex)
                                  (drop-collection . PPDropCollection))))
                ,(if (eq? var-count 0) 0 (+ var-count 1))
                ,ind-name)))
             
             
             ((eq? op-name 'drop-document)
              (let* ((doc (l2p:any-lr-node2por (car node)))
                     (doc-context (if (eq? var-count 0) 0 (+ var-count 1))))
              (if (eq? (length node) 2)
                  (let* ((col (begin (set! var-count 0) (l2p:any-lr-node2por (cadr node))))
                         (col-context (if (eq? var-count 0) 0 (+ var-count 1))))
                  `(PPDropDocumentInCollection ,doc-context
                                               ,doc
                                               ,col-context
                                               ,col))
                  `(PPDropDocument ,doc-context
                                   ,doc))))
             
             ((eq? op-name 'drop-trigger)
              (let* ((col (l2p:any-lr-node2por (car node))))
                `(PPDropTrigger ,(if (eq? var-count 0) 0 (+ var-count 1))
                                ,col)))
             
             ((eq? op-name 'retrieve-metadata-documents)
              (if (eq? (length node) 2)
                  (let* ((col (l2p:any-lr-node2por (car node))))
                  `(PPRetrieveMetadata ,(if (eq? var-count 0) 0 (+ var-count 1))
                                       ,col
                                       ,(if (eq? (caddr (cadr node)) 'true)
                                                             #t
                                                             #f)))
                  `(PPRetrieveMetadata document ,(if (eq? (caddr (car node)) 'true)
                                                             #t
                                                             #f))))

             ((eq? op-name 'retrieve-metadata-collections)
              `(PPRetrieveMetadata collection ,(if (eq? (caddr (car node)) 'true)
                                                             #t
                                                             #f)))
             
             ((eq? op-name 'retrieve-descr-scheme)
              (if (eq? (length (car node)) 2)
                  (let* ((col (l2p:any-lr-node2por (cadr node))))
                  `(PPRetrieveDSForCollection ,(if (eq? var-count 0) 0 (+ var-count 1))
                                              ,col))
                  (let* ((col (l2p:any-lr-node2por (car node))))
                  `(PPRetrieveDSForDocument ,(if (eq? var-count 0) 0 (+ var-count 1))
                                            ,col))))
             
            ((eq? op-name 'retrieve-descr-scheme-collection)
             (let* ((col (l2p:any-lr-node2por (car node))))
             `(PPRetrieveDSForCollection ,(if (eq? var-count 0) 0 (+ var-count 1))
                                         ,col)))
             

                          
             (else
              (cl:signal-input-error
               SE4008
               (string-append "unknown logical operation given: "
                              (symbol->string op-name)))))))
        (else
         (cl:signal-input-error
          SE4008 "unknown logical operation given: " (symbol->string node)))))

;--------------------------------------------------------------------------------
(define (l2p:tuple-size PhysOp)
  (car PhysOp)
)

(define (l2p:lr-axis2por-axis axis)
  (case axis
    ((child) 'PPAxisChild)
    ((attr-axis) 'PPAxisAttribute)
    ((parent) 'PPAxisParent)
    ((self) 'PPAxisSelf)
    ((descendant) 'PPAxisDescendant)
    ((descendant-or-self) 'PPAxisDescendantOrSelf)
    ((descendant-attr) 'PPAxisDescendantAttr)
    ; New axes implemented
    ((ancestor) 'PPAxisAncestor)
    ((ancestor-or-self) 'PPAxisAncestorOrSelf)
    ((following) 'PPAxisFollowing)
    ((following-sibling) 'PPAxisFollowingSibling)
    ((preceding) 'PPAxisPreceding)
    ((preceding-sibling) 'PPAxisPrecedingSibling)
    (else
     (cl:signal-input-error SE4008
                            (string-append "unknown axis given: " (symbol->string axis))))))

(define (l2p:lr-test2por-test test-type)
  (cond ((eq? test-type 'text-test) 'text)
        ((eq? test-type 'node-test) 'node)
        ((eq? test-type 'comment-test) 'comment)
        ((eq? test-type 'pi-test) 'processing_instruction)
        (else (cl:signal-input-error SE4008 (string-append "unknown test-type given: "
                                       (symbol->string test-type))))
        )
)


(define (l2p:lr-sequenceType2por-sequenceType SeqType)
  (let ((lr-occ-ind (car SeqType)))
    (cond 
      ((eq? lr-occ-ind 'empty-test)
       '(empty (item))
       ; DL: was: `(empty xdt_untypedAtomic)
       )
      ((or (eq? lr-occ-ind 'one)
           (eq? lr-occ-ind 'optional)
           (eq? lr-occ-ind 'zero-or-more)
           (eq? lr-occ-ind 'one-or-more))
       (let* ((item-type (cadr SeqType))
              (por-occ-ind (l2p:lr-oocur-ind2por-occur-ind lr-occ-ind)))
         (cond
           ((symbol? item-type)  ; built-in atomic type
            (list por-occ-ind
                  (l2p:lr-atomic-type2por-atomic-type item-type)))
           ((string? (car item-type)) ; not built-in atomic type
            (cl:signal-user-error
             SE4008
             (string-append " non built-in type is not supported: "
                            (apply string-append item-type))))
           ((eq? (car item-type) 'comment-test)
            `(,por-occ-ind (comment)))
           ((eq? (car item-type) 'text-test)
            `(,por-occ-ind (text)))
           ((eq? (car item-type) 'node-test)
            `(,por-occ-ind (node)))
           ((eq? (car item-type) 'item-test)
            `(,por-occ-ind (item)))
           ((eq? (car item-type) 'pi-test)
            ; DL: in expressions like instance-of
            `(,por-occ-ind
              (pi   ; Was: processing_instruction
               ,@(if
                  (null? (cdr item-type))  ; no target specified
                  '()
                  (list
                   (caddr  ; constant value
                    (cadr item-type)  ; selects '(const ...)
                    ))))))
           ((eq? (car item-type) 'elem-test)
            (list por-occ-ind
                  `(element ,(l2p:lr-elem-test2por-elem-test item-type))))
           ((eq? (car item-type) 'attr-test)
            (list por-occ-ind
                  `(attribute ,(l2p:lr-elem-test2por-elem-test
                                ; was: l2p:lr-attr-test2por-attr-test
                                item-type))))                      
           ((eq? (car item-type) 'doc-test)
            (list
             por-occ-ind
             (if
              (null? (cdr item-type))  ; no contents
              '(document)
              `(document ,(l2p:lr-elem-test2por-elem-test (cadr item-type))))))

                      
                      (else (cl:signal-input-error SE4008 (string-append "unknown item-type: "
                                       (symbol->string (car item-type))))))))
          (else
           (cl:signal-error
            SE4008
            (string-input-append "unknown occurence indicator: "
                                 (symbol->string lr-occ-ind)))))))



(define (l2p:lr-atomic-type2por-atomic-type atomic-type)
  (if (eq? atomic-type 'se:positional-var)
      'xs:anyType
      atomic-type))

;(define (l2p:lr-atomic-type2por-atomic-type atomic-type)
;  (cond ((eq? atomic-type '!xdt!untypedAtomic) 'xdt_untypedAtomic)
;        ((eq? atomic-type '!xs!anySimpleType) 'xs_anySimpleType)
;        ((eq? atomic-type '!xs!gYearMonth) 'xs_gYearMonth)
;        ((eq? atomic-type '!xs!gYear) 'xs_gYear)
;        ((eq? atomic-type '!xs!gMonthDay) 'xs_gMonthDay)
;        ((eq? atomic-type '!xs!gDay) 'xs_gDay)
;        ((eq? atomic-type '!xs!gMonth) 'xs_gMonth)
;        ((eq? atomic-type '!xs!dateTime) 'xs_dateTime)
;        ((eq? atomic-type '!xs!time) 'xs_time)
;        ((eq? atomic-type '!xs!date) 'xs_date)
;        ((eq? atomic-type '!xs!duration) 'xs_duration)
;        ((eq? atomic-type '!xs!boolean) 'xs_boolean)
;        ((eq? atomic-type '!xs!base64Binary) 'xs_base64Binary)
;        ((eq? atomic-type '!xs!hexBinary) 'xs_hexBinary)
;        ((eq? atomic-type '!xs!float) 'xs_float)
;        ((eq? atomic-type '!xs!double) 'xs_double)
;        ((eq? atomic-type '!xs!anyURI) 'xs_anyURI)
;        ((eq? atomic-type '!xs!QName) 'xs_QName)
;        ((eq? atomic-type '!xs!NOTATION) 'xs_NOTATION)
;        ((eq? atomic-type '!xs!string) 'xs_string)
;        ((eq? atomic-type '!xs!decimal) 'xs_decimal)
;        ((eq? atomic-type '!xs!integer) 'xs_integer)
;
;        (else (cl:signal-input-error SE4008 (string-append "unknown atomic-type: "
;                                       (symbol->string atomic-type))))
;        )
;)

(define (l2p:lr-scan-cond2por-scan-cond condit)
  (cond ((string=? condit "GT") 'isc_gt)
        ((string=? condit "LT") 'isc_lt)
        ((string=? condit "GE") 'isc_ge)
        ((string=? condit "LE") 'isc_le)
        ((string=? condit "EQ") 'isc_eq)))
  
(define (l2p:lr-range2por-range range)
  (cond ((string=? range "INT") 'isc_gt_lt)
        ((string=? range "SEG") 'isc_ge_le)
        ((string=? range "HINTL") 'isc_ge_lt)
        ((string=? range "HINTR") 'isc_gt_le)))  

  
(define (l2p:lr-oocur-ind2por-occur-ind occ-ind)
  (cond ((eq? occ-ind 'empty) 'empty)
        ((eq? occ-ind 'one) 'one)
        ((eq? occ-ind 'optional) 'optional)
        ((eq? occ-ind 'zero-or-more) 'zero_or_more)
        ((eq? occ-ind 'one-or-more) 'one_or_more)
        (else (cl:signal-input-error SE4008 (string-append "unknown occurance-indicator: "
                                       (symbol->string occ-ind))))))        
        
(define (l2p:lr-elem-test2por-elem-test elem-test)
  (let ((test-type (caadr elem-test)))
    (cond
      ((eq? test-type 'ename)
       (let* ((ename (cadr elem-test))
              (elem-name (cadr ename))
              (elem-type (caddr ename)))
         (let ((name-pair (caddr elem-name))
               (type-value (cadr elem-type)))
           (call-with-values
            (lambda ()
              (if
               (memq name-pair '(unspecified *))
               (values 'wildcard '())
;               (let ((parts
;                      (map
;                       (lambda (part)
;                         (if (eq? part '*)
;                             (cons "wildcard" "")
;                             (cons "name" part)))
;                       name-pair)))
;                 (values (string->symbol
;                          (string-append (caar parts) "-" (caadr parts)))
;                         (map cdr parts)))
               (values 'name name-pair)))
            (lambda (node-name-enum str-str)
              (call-with-values
               (lambda ()
                 (if
                  (memq type-value '(unspecified *))
                  (values 'nothing '!xs!anyType)
                  (values
                   (if
                    (equal? (cadddr ename)
                            '(const (type !xs!string) "qmark"))
                    'optional
                    'present)
                   type-value)))
               (lambda (type-name-enum single-type)
                 (list node-name-enum
                       type-name-enum
                       str-str
                       (l2p:lr-atomic-type2por-atomic-type single-type)))))))))
;      (cond
;           ((and (eq? (caddr elem-name) 'unspecified) ; element()
;                 (eq? (cadr elem-type) 'unspecified)) 
;            `())
;           
;           ((and (eq? (caddr elem-name) '*) ; element(*)
;                 (eq? (cadr elem-type) 'unspecified))
;            `(element_wildcard))
;           
;           ((and (list? (caddr elem-name))
;                 (eq? (cadr elem-type) 'unspecified)) ; element(fo:name)
;            `(element_name ,(caddr elem-name)))
;           
;           ((and (eq? (caddr elem-name) '*) ; element(*,*)
;                 (eq? (cadr elem-type) '*))
;            `(element_wildcard_wildcard))
;           
;           ((and (eq? (caddr elem-name) '*) ; element(*, fo:type-name)
;                 (list? (cadr elem-type)))
;            `(element_wildcard_name ,(cadr elem-type)))
;           
;           ((and (list? (caddr elem-name)) ; element(fo:name, *)
;                 (eq? (cadr elem-type) '*))
;            `(element_name_wildcard ,(caddr elem-name)))
;           
;           ((and (list? (caddr elem-name))
;                 (list? (cadr elem-type)))
;            `(element_name_name ,(caddr elem-name) ,(cadr elem-type)))
;           
;           (else (cl:signal-input-error SE4008 (string-append 
;                                                (string-append "unknown combination of element name and element test: "
;                                                               (l2p:list2string elem-name))
;                                                (l2p:list2string elem-type)))))
      ((eq? test-type 'sname)
       (cl:signal-user-error
        SE4008
        "sname is not supported in elem-test"))
      (else
       (cl:signal-input-error
        SE4008
        (string-append "unknown test-type: " (symbol->string test-type)))))))
       

(define (l2p:lr-attr-test2por-attr-test attr-test)
  (let ((test-type (caadr attr-test)))
    (cond ((eq? test-type 'ename)
              (let* ((attr-name (cadr (cadr attr-test)))
                     (attr-type (caddr (cadr attr-test))))
                (cond ((and (eq? (caddr attr-name) 'unspecified) ; attribute()
                            (eq? (cadr attr-type) 'unspecified)) 
                       `())
                      
                      ((and (eq? (caddr attr-name) '*) ; attribute(*)
                            (eq? (cadr attr-type) 'unspecified))
                       `(attribute_wildcard))
                      
                      ((and (list? (caddr attr-name))
                            (eq? (cadr attr-type) 'unspecified)) ; attribute(fo:name)
                       `(attribute_name ,(caddr attr-name)))
                      
                      ((and (eq? (caddr attr-name) '*) ; attribute(*,*)
                            (eq? (cadr attr-type) '*))
                       `(attribute_wildcard_wildcard))
                      
                      ((and (eq? (caddr attr-name) '*) ; attribute(*, fo:type-name)
                            (list? (cadr attr-type)))
                       `(attribute_wildcard_name ,(cadr attr-type)))
                      
                      ((and (list? (caddr attr-name)) ; attribute(fo:name, *)
                            (eq? (cadr attr-type) '*))
                       `(attribute_name_wildcard ,(caddr attr-name)))
                      
                      ((and (list? (caddr attr-name))
                            (list? (cadr attr-type)))
                       `(attribute_name_name ,(caddr attr-name) ,(cadr attr-type)))
                      
                      (else (cl:signal-input-error SE4008 (string-append 
                                    (string-append "unknown combination of attribute name and attribute test: "
                                                  (l2p:list2string attr-name))
                                    (l2p:list2string attr-type)))))))

          (else (cl:signal-input-error SE4008 (string-append "unknown test-type: " (symbol->string test-type)))))))

(define (l2p:list2string lst)
  (if (null? lst)
      ""
      (string-append
       (cond
         ((string? (car lst)) (car lst))
         ((symbol? (car lst)) (symbol->string (car lst)))
         (else (l2p:list2string (car lst))))
       (l2p:list2string (cdr lst)))))


(define var-count 0)
(define funcs-map '())

;Generates a var name that is not in the list l

(define (l2p:gen-var)
  (let ((var var-count))
    (set! var-count (+ var-count 1))
    var)
)

(define (l2p:rename-vars2unique-numbers fun-def)
  (let* ((vars-map (l2p:generate-map
                    (map cadr  ; yields variable names
                         (map cadr
                              (cadr fun-def)  ; argument list
                              ))))
         (new-var-decls
          (l2p:generate-new-fun-decl vars-map (xlr:var-defs fun-def))))
    ;(display new-var-decls)
    ;(display vars-map)
  `(fun-def ,new-var-decls ,(l2p:rename-vars vars-map (xlr:fun-body fun-def))))
)
  
(define (l2p:generate-map vars)
  (if (null? vars)
      '()
      (cons (list (car vars) (l2p:gen-var))
            (l2p:generate-map (cdr vars)))))

(define (l2p:generate-new-fun-decl vars var_decls)
  (if (null? vars)
      `()
      (cons (list (caar var_decls) (cadar vars))
            (l2p:generate-new-fun-decl (cdr vars) (cdr var_decls)))))

(define (l2p:rename-vars vars-map expr)
  (if (null? vars-map)
      expr
      (l2p:rename-vars 
       (cdr vars-map)
       (xlr:substitute-var-value `(var ,(caar vars-map)) `(var ,(cadar vars-map)) expr)))
)

(define (l2p:add-func-name! func-name)
  (if (null? funcs-map)
      (set! funcs-map `((,func-name 0)))
      (set! funcs-map (append funcs-map `((,func-name ,(length funcs-map)))))
  ))

; DL: func-name is now a list:
; func-name ::= (list function-name arity)
(define (l2p:find-func-index func-name funcs-list)
  (cond ((null? funcs-list) #f)
        ((equal? func-name (caar funcs-list))
         ; DL: was:
;         (and (string=? (caaar funcs-list) (car func-name))
;              (string=? (cadaar funcs-list) (cadr func-name)))
         (cadar funcs-list))
        (else (l2p:find-func-index func-name (cdr funcs-list)))))

;(define init-var-param 1)

;(define (l2p:new-var-name))

;=========================================================================
; Predicate

; Combines map and apply append
(define (l2p:map-append f lst)
  (if (null? lst)
      lst
      (append (f (car lst))
              (l2p:map-append f (cdr lst)))))

;; Applies `or' to arg-lst
;(define (l2p:apply-or arg-lst)
;  (cond
;    ((null? arg-lst) #f)
;    ((car arg-lst) #t)
;    (else (l2p:apply-or (cdr arg-lst)))))

; Returns (listof subexpr)
; where conjunction of all `subexpr's forms the original `expr'
(define (l2p:extract-conjunctors expr)
  (if (and (pair? expr)
           (eq? (car expr) 'and@))
      (l2p:map-append l2p:extract-conjunctors (cdr expr))
      (list expr)))

; For a comparison operation, returns the equivalent one with commuted arguments
(define (l2p:commute-comparison-op expr)
  (cond
    ((not (pair? expr))  ; ill-formed expr
     expr)
    ((memq (car expr) '(=@ !=@ eq@ ne@ is@))
     (list (car expr)  ; the same operation
           (caddr expr)
           (cadr expr)))
    ((assq (car expr)
           '((<@ >@) (<=@ >=@) (>@ <@) (>=@ <=@)
             (le@ ge@) (ge@ le@) (gt@ lt@) (lt@ gt@)
             (<<@ >>@) (>>@ <<@)))
     => (lambda (pair)
          (list (cadr pair)
                (caddr expr)
                (cadr expr))))
    (else  ; a different operation
     expr)))

; Returns: (values depends-on-context-node?
;                  depends-on-position?
;                  depends-on-last? )
(define (l2p:expr-depends-on expr context-node-var-name)
  (if
   (not (pair? expr))  ; leaf node
   (values #f #f #f)
   (case (car expr)
     ((var)
      (values (equal? (cadr expr)  ; variable name
                      context-node-var-name)
              #f #f))
     ((!fn!position)
      (values #f #t #f))
     ((!fn!last)
      (values #f #f #t))
     (else
      (let loop ((node? #f)
                 (pos? #f)
                 (last? #f)
                 (args expr))
        (if
         (null? args)  ; all arguments scanned
         (values node? pos? last?)
         (call-with-values
          (lambda ()
            (l2p:expr-depends-on (car args) context-node-var-name))
          (lambda (n? p? l?)
            (loop (or n? node?)
                  (or p? pos?)
                  (or l? last?)
                  (cdr args))))))))))
  
; For a comparison operation, returns its code for PPPred
(define (l2p:comparison-op->ppred-code op)
  (cond
    ((assq op '((eq@ eqv) (ne@ nev) (gt@ gtv) (lt@ ltv) (ge@ gev) (le@ lev)
                (=@ eqg) (!=@ neg) (>@ gtg) (<@ ltg) (>=@ geg) (<=@ leg)))
     => cadr)
    (else #f)))

; Replaces function calls to position() and last() with variable numbers
(define (l2p:replace-pos-last2numbers expr pos-num last-num)
  (cond
    ((not (pair? expr))  ; leaf node
     expr)
    ((eq? (car expr) '!fn!position)
     `(var ,pos-num))
    ((eq? (car expr) '!fn!last)
     `(var ,last-num))
    (else
     (map
      (lambda (kid)
        (l2p:replace-pos-last2numbers kid pos-num last-num))
      expr))))

; If `expr' has the form "position() cmp-op smth", returns
; (cons (list cmp-op for-smth) requires-last?)
; Otherwise, returns #f
(define (l2p:whether-pos-cmp-smth expr context-node-var-name)
  (let ((pos-on-left
         (lambda (expr)
           (let ((left (cadr expr))   ; left argument of comparison operation
                 (right (caddr expr)))
             (if
              (and (pair? left)
                   (eq? (car left) '!fn!position))
              (call-with-values
               (lambda ()
                 (l2p:expr-depends-on right context-node-var-name))
               (lambda (node? pos? last?)
                 (if (not (or node? pos?))  ; right part can be evaluated just once
                     (cons
                      (list (car expr)  ; operation name
                            right)
                      last?)
                     #f)))
              #f)))))
    (cond
      ((not (and (pair? expr)
                 (memv (car expr)
                       '(=@ >@ <@ >=@ <=@ !=@ eq@ ne@ gt@ ge@ lt@ le@ ne@))))
       #f)
      ((pos-on-left expr)
       => (lambda (x) x))
      (else
       (pos-on-left (l2p:commute-comparison-op expr))))))

(define (l2p:predicate2por arg-lst)
  (let ((new-fun-def (l2p:rename-vars2unique-numbers (cadr arg-lst)))
        (source-child (l2p:any-lr-node2por (car arg-lst))))
    (let ((context-node-var-name
           (cadr  ; argument unique number
            (car  ; 1st argument
             (cadr  ; function arguments
              new-fun-def)))))
      (let loop ((conjunctors (l2p:extract-conjunctors
                               (caddr new-fun-def)  ; function body
                               ))
                 (cmp-pos-lst '())  ; position() cmp-op smth
                 (others '())   ; the other conjunctors
                 (requires-node? #f)
                 (requires-pos? #f)
                 (requires-last? #f))
      (cond
        ((null? conjunctors)  ; all scanned
         (let*  ; Contains side effects!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
             ((pos-num (if requires-pos?
                           (l2p:gen-var)
                           666  ; we don't care
                           ))
              (last-num (if requires-last?
                            (l2p:gen-var)
                            667  ; we don't care
                            )))
           ((lambda (x)
             ;(pp x)
             x)
           `(,(l2p:tuple-size source-child)
             (,(if requires-last? 'PPPred2 'PPPred1)
              ,(map
                cadr               
                (cadr   ; function arguments
                 new-fun-def))
              ,source-child
              ,(map
                (lambda (pair)
                  (list
                   (l2p:comparison-op->ppred-code (car pair))
                   (l2p:any-lr-node2por
                    (l2p:replace-pos-last2numbers (cadr pair) pos-num last-num))))
                (reverse cmp-pos-lst))
              ,(cond
                 ((null? others)
                  '(1 (PPFnTrue)))
;                 ((null? (cdr others))  ; a single member
;                  (l2p:any-lr-node2por
;                   (l2p:replace-pos-last2numbers (car others)
;                                                 pos-num last-num)))
                 (else
                  (l2p:any-lr-node2por
                   (l2p:replace-pos-last2numbers
                    ; Was:
                    ;(cons 'and@ (reverse others))
                    (let bin ((src (cdr others))
                              (res (car others)))
                      (if
                       (null? src)
                       res
                       (bin (cdr src)
                            (list 'and@ (car src) res))))
                    pos-num last-num))))
              ,@((lambda (lst)
                   (cons (if (car lst) 1 0)
                         (cdr lst)))
                 (cond
                   (requires-last?
                    (if requires-pos?
                        (list #f  ; is to be evaluated multiple times, since depends on position()
                              last-num pos-num)
                        (list (not requires-node?) last-num)))
                   (requires-pos?
                    (list #f  ; is to be evaluated multiple times, since depends on position()
                          pos-num))
                   (else
                    (list (not requires-node?))))))))))
        ((l2p:whether-pos-cmp-smth (car conjunctors) context-node-var-name)
         => (lambda (pair)
              (loop (cdr conjunctors)
                    (cons (car pair) cmp-pos-lst)
                    others
                    requires-node?
                    requires-pos?
                    (or requires-last? (cdr pair)))))
        (else
         (call-with-values
          (lambda ()
            (l2p:expr-depends-on (car conjunctors) context-node-var-name))
          (lambda (node? pos? last?)
            (loop (cdr conjunctors)
                  cmp-pos-lst
                  (cons (car conjunctors) others)
                  (or requires-node? node?)
                  (or requires-pos? pos?)
                  (or requires-last? last?))))))))))

;=========================================================================
; Order-by

(define (l2p:list-partition lst pred?)
  (let loop ((lst lst)
             (satisfies '())
             (fails '()))
    (cond
      ((null? lst)
       (values (reverse satisfies)
               (reverse fails)))
      ((pred? (car lst))
       (loop (cdr lst)
             (cons (car lst) satisfies)
             fails))
      (else
       (loop (cdr lst)
             satisfies
             (cons (car lst) fails))))))

(define (l2p:union-remove-equal lst1 lst2)
  (cond
    ((null? lst1) lst2)
    ((member (car lst1) lst2)
     (l2p:union-remove-equal (cdr lst1) lst2))
    (else 
     (cons (car lst1)
           (l2p:union-remove-equal (cdr lst1) lst2)))))

; Replaces each 'unio with 'tmp-tuple and extends its arguments with ExprSingle-list
; from OrderSpecList
; expr-single-lst - over these expressions the ordering criteria are specified
; Returns: (values rewritten-expr for-variables let-variables)
(define (l2p:replace-unio2tmp-tuple expr expr-single-lst)
  (letrec ((tree-walk
            (lambda (expr for-variables let-variables capture-vars?)
              (cond
                ((not (pair? expr))  ; leaf node
                 (values expr for-variables let-variables))
                ((eq? (car expr) 'unio)
                 (call-with-values
                  (lambda ()
                    (l2p:list-partition
                     (cdr expr)
                     (lambda (x) (member x let-variables))))
                  (lambda (let-vars for-vars)
                    (values
                     (cons 'tmp-tuple   
                           (append for-vars
                                   let-vars
                                   expr-single-lst))
                     for-vars
                     let-vars))))
                ((memq (car expr) '(let@ return lreturn))
                 (call-with-values
                  (lambda ()
                    (let* ((fun-def (caddr expr))
                           (args (cadr fun-def))
                           ; the only argument
                           (var-wrapped (cadar args))
                           (var-name (cadr var-wrapped)))
                      (if
                       capture-vars?
                       (if
                        (eq? (car expr) 'let@)
                        (values for-variables (cons var-wrapped let-variables))
                        (values (cons var-wrapped for-variables) let-variables))
                       (values for-variables let-variables))))
                  (lambda (new-for-variables new-let-variables)
                    (call-with-values
                     (lambda ()
                       (tree-walk
                        (cadr expr) for-variables let-variables capture-vars?))
                     (lambda (sub1 for1 let1)
                       (call-with-values
                        (lambda ()
                          (tree-walk (caddr expr)
                                     new-for-variables
                                     new-let-variables
                                     capture-vars?))
                        (lambda (sub2 for2 let2)
                          (values
                           (list (car expr) sub1 sub2)
                           (l2p:union-remove-equal for1 for2)
                           (l2p:union-remove-equal let1 let2)))))))))
                (else
                 (let loop ((kids (cdr expr))
                            (res '())
                            (for-vars '())
                            (let-vars '()))
                   (if
                    (null? kids)
                    (values (cons (car expr) (reverse res))
                            for-vars let-vars)
                    (call-with-values
                     (lambda ()
                       (tree-walk (car kids)
                                  for-variables let-variables
                                  (if (eq? (car expr) 'fun-def)
                                      capture-vars?
                                      #f)))
                     (lambda (new-kid for3 let3)
                       (loop (cdr kids)
                             (cons new-kid res)
                             (l2p:union-remove-equal for-vars for3)
                             (l2p:union-remove-equal let-vars let3)))))))))))
    (tree-walk expr '() '() #t)))
; The older implementation that fails to avoid the
; SEDNA Message: ERROR SE1003
; Sedna XQuery processor error.
; Details: The tuple cell value is a sequence in PPTuple
; See e.g. xmp-queries-results-q4
;(define (l2p:replace-unio2tmp-tuple expr expr-single-lst)
;  (cond
;    ((not (pair? expr))  ; leaf node
;     expr)
;    ((eq? (car expr) 'unio)
;     ;(write expr)
;     ;(newline)
;     (cons 'tmp-tuple
;           (append (cdr expr) expr-single-lst)))    
;    (else
;     (map
;      (lambda (kid) (l2p:replace-unio2tmp-tuple kid expr-single-lst))
;      expr))))

; If return-expr contains order-by as its first subexpr, rewrites arg-lst
; accordingly. 
; Otherwise, returns arg-lst unchanged
(define (l2p:return-order-by arg-lst)
  ((lambda (x)
     ;(pp x)
     x)
  (letrec ((form-slets
            (lambda (var-lst last-body)
              (if
               (null? var-lst)
               last-body
               `(slet@
                 ,(car var-lst)
                 (fun-def
                  ((xs:anyType ,(car var-lst)))
                  ,(form-slets (cdr var-lst) last-body)))))))
    (if
     (not (and (pair? arg-lst)
               (pair? (car arg-lst))
               (eq? (caar arg-lst) 'order-by)))
     arg-lst
     (let* ((order-expr (car arg-lst))
            (subexpr (cadr order-expr))
            (fun-def (caddr order-expr))
            (orderspecs (caddr fun-def))
            (stable-const (cadr orderspecs))
            (spec-lst (cddr orderspecs)))
       (call-with-values
        (lambda ()
          (l2p:replace-unio2tmp-tuple subexpr (map caddr spec-lst)))
        (lambda (subexpr-with-tmp-tuple for-variables let-variables)
          (let ((second-fun (cadr arg-lst)))
            ((lambda (x)
               ;(pp (list arg-lst x))
               x)
             (list
              (list (car order-expr)  ; == 'order-by
                    subexpr-with-tmp-tuple
                    (call-with-values
                     (lambda ()
                       (l2p:list-partition
                        (cadr fun-def)
                        (lambda (x) (member (cadr x) let-variables))))
                     (lambda (let-vars for-vars)
                       (list (car fun-def)  ; == 'fun-def
                             (append for-vars let-vars)
                             (caddr fun-def)  ; function body
                             ))))
              (list (car second-fun)
                    ((lambda (args)
                       ;(pp (list "args: " args))
                       args)
                     (call-with-values
                      (lambda ()
                        (l2p:list-partition
                         (cadr second-fun)
                         (lambda (x) (member (cadr x) let-variables))))
                      (lambda (let-vars for-vars)
                        ;(pp let-vars)
                        (append for-vars
                                (map
                                 (lambda (pair)
                                   ; Removing type information for variables
                                   ; bound with let-clause
                                   (list '!xs!anyType (cadr pair)))
                                 let-vars)))))
                    (form-slets let-variables (caddr second-fun)))))))))))))

(define (l2p:order-by2por arg-lst)
  (let* ((subexpr (car arg-lst))
         (fun-def (cadr arg-lst))
         (orderspecs (caddr fun-def))
         (stable-const (cadr orderspecs))
         (spec-lst (cddr orderspecs)))
       `(,(length (cadr fun-def))  ; Tuple size
         (PPOrderBy
          ,(if (string=? (caddr stable-const)  ; constant value
                         "stable")
               #t #f)
          ,(l2p:any-lr-node2por subexpr)
          ,(map
            (lambda (modif)
              (if
               (null? (cdr modif))  ; no arguments at all
               '(default default)
               (cons
                (cond
                  ((null? (cddr modif))  ; no empty status
                   'default)
                  ((assoc (caddr (caddr modif))  ; value of the 1st const
                          '(("empty-greatest" . greatest)
                            ("empty-least" . least)))
                   => cdr)
                  (else
                   'default))
                (cons
                 (cdr (assoc (caddr (cadr modif))  ; value of the 1st const
                             '(("asc" . ascending)
                               ("desc" . descending))))
                 (if
                  (and (> (length (cdr modif)) 2)
                       (pair? (cadddr modif))
                       (eq? (car (cadddr modif)) 'collation))
                  (list 
                   (caddr  ; constant value
                    (cadr  ; (const (type !xs!string) ...)
                     (cadddr modif))))
                  '()  ; no collation specification
                  )))))
            (map cadr spec-lst)  ; list of ordermodifier
            )))))

;=========================================================================
; Typeswitch

(define (l2p:ts2por arg-lst)
  (let ((source-child (l2p:any-lr-node2por (car arg-lst)))
        (cases (cdr
                (cadr arg-lst)  ; addresses '(cases ...)
                ))
        (var-num (l2p:gen-var)  ; HAS A SIDE EFFECT!!!!         
                 ))
    (let ((case-pairs
           (map
            (lambda (case)
              (cons (let ((type (cadr  ; sequence type
                                 (cadr case)  ; addresses '(type ...)
                                 )))
                      ((if (pair? type)
                           l2p:lr-sequenceType2por-sequenceType
                           l2p:lr-atomic-type2por-atomic-type)
                       type))
                    (l2p:any-lr-node2por
                     (l2p:rename-fun-arg-to-given-num
                      (caddr case)  ; fun-def
                      var-num))))
            (reverse (cdr (reverse cases)))  ; this can be done more effectively, of course
            ))
          (default-body (l2p:any-lr-node2por
                         (l2p:rename-fun-arg-to-given-num
                          (cadr  ; fun-def for default case
                           (car (reverse cases))  ; default case
                           )
                          var-num
                          ))))
      `(,(l2p:tuple-size source-child)
        (PPTypeswitch
         (,var-num)
         ,source-child
         ,(map car case-pairs)  ; listof sequence types
         ,(map cdr case-pairs)
         ,default-body)))))

; Renames the fun-def argument with a given number
; Returns the modified function body
; The function is a simplified variant of `l2p:rename-vars2unique-numbers'
(define (l2p:rename-fun-arg-to-given-num fun-def num)
  (let ((vars-map (list (list
                         (cadr  ; variable name
                          (cadr  ; addresses '(var ("url" "name"))
                           (car  ; the first argument
                            (cadr fun-def)  ; argument list
                            )))
                         num))))
    (l2p:rename-vars vars-map (xlr:fun-body fun-def))))


;=========================================================================
; Lsome, Levery

(define (l2p:lsome->por content)
  (let* ((select-left-PhysOp (l2p:any-lr-node2por
                              (car content)  ; was: (cadr content)
                              ))
         (tsls (l2p:tuple-size select-left-PhysOp))
         (new-fun-def (l2p:rename-vars2unique-numbers (cadr content)))
         (select-right-PhysOp (l2p:any-lr-node2por (caddr new-fun-def))))
    `(1 (PPFnExists
         (,tsls
          (PPSelect
           ,(map cadr  ; argument names
                 (cadr new-fun-def))
           (,tsls (PPStore ,select-left-PhysOp))
           ,select-right-PhysOp
               ; Was:
               ;,(l2p:any-lr-node2por
               ;  (caddr  ; function body
               ;   (cadr content)  ; yields: (fun-def ...) ; was: (caddr content)
               ;   ))
           ))))))

(define (l2p:levery->por content)
  (let* ((select-left-PhysOp (l2p:any-lr-node2por
                              (car content)  ; was: (cadr content)
                              ))
         (tsls (l2p:tuple-size select-left-PhysOp))
         (new-fun-def (l2p:rename-vars2unique-numbers (cadr content)))
         (select-right-PhysOp (l2p:any-lr-node2por (caddr new-fun-def))))
    `(1 (PPFnEmpty
         (,tsls 
          (PPSelect
           ,(map cadr  ; argument names
                 (cadr new-fun-def))
           (,tsls (PPStore ,select-left-PhysOp))
           (1 (PPFnNot
               ,select-right-PhysOp
               ; Was:
               ;,(l2p:any-lr-node2por
               ;  (caddr  ; function body
               ;   (cadr content)  ; yields: (fun-def ...) ; was: (caddr content)
               ;   ))
               ))))))))
