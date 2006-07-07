
; File:  lr2por.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

(declare (unit lr2por) (uses lr2por-lib common-lib))

;-------------------------------------------------------------------------------
; lp2por
(define (l2p:lr2por query-in-lr)
  (let* ((PPquery-prolog (l2p:lr-query-prolog2por (cadr query-in-lr)))
         (PPquery-expr (l2p:lr-query-expr2por
                        (if (eq? (car query-in-lr) 'query)
                            (caddr query-in-lr) query-in-lr))))
    `(query ,PPquery-prolog ,PPquery-expr))
; DL: was  
;  (if (eq? (car query-in-lr) 'query)
;      (let* ((PPquery-prolog (l2p:lr-query-prolog2por (cadr query-in-lr)))
;             (PPquery-expr (l2p:lr-query-expr2por (caddr query-in-lr)))
;            )
;        `(query ,PPquery-prolog ,PPquery-expr))
;      `(query (query-prolog) ,(l2p:lr-query-expr2por query-in-lr))
;  )
)


;-------------------------------------------------------------------------------
; lr-query-prolog2por
(define (l2p:lr-query-prolog2por query-prolog-in-lr)
  (if
   (eq? 'prolog (car query-prolog-in-lr)) 
   (begin
     ; add all function names to the global func-list
     (map 
      (lambda (y) (l2p:add-func-name (caddr (cadr y))))
      (filter
       (lambda (x) (if (eq? (car x) 'declare-function) #t #f))
       (cdr query-prolog-in-lr)))
     (cons 'query-prolog
           (let ((lr-prolog
                  (map l2p:lr-prolog-decl2por 
                       (filter
                        (lambda (x)
                          (if
                           (eq? (car x) 'declare-external-function)
                           #f #t))
                        (cdr query-prolog-in-lr)))))
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
             lr-prolog))))
   (cl:signal-input-error SE4008 "argument is not query-prolog")))

(define (l2p:lr-prolog-decl2por prolog-decl)
  (cond
    ((eq? (car prolog-decl) 'declare-function)
     (l2p:lr-named-fun-def2por prolog-decl))
    ((eq? (car prolog-decl) 'declare-namespace)
     (l2p:lr-decl-ns2por prolog-decl))
    ((eq? (car prolog-decl) 'declare-default-element-namespace)
     (l2p:decl-def-elem-ns2por prolog-decl))
    ((eq? (car prolog-decl) 'declare-default-function-namespace)
     (l2p:decl-def-func-ns2por prolog-decl))
    ((eq? (car prolog-decl) 'boundary-space-decl)
     `(PPBoundarySpaceDecl ,(caddr (cadr prolog-decl))))
    ((eq? (car prolog-decl) 'declare-default-order)
     `(PPEmptyOrderDecl
       ,(cdr (assoc (caddr (cadr prolog-decl))
                    '(("empty-greatest" . greatest)
                      ("empty-least" . least))))))
    ((eq? (car prolog-decl) 'declare-option)
     `(PPOptionDecl
       (,(caddr (cadr prolog-decl))  ; option QName
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
(define (l2p:lr-named-fun-def2por fun-def-in-lr)
  (set! var-count '0)
  ;(l2p:add-func-name (string->symbol (cadr (caddr (cadr fun-def-in-lr))))) ; update global function names list
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
             (vars-map (l2p:generate-map var-names))
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
             ((eq? op-name 'var) `(1 (PPVariable ,@node)))
             
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
              (let* ((new-fun-def (l2p:rename-vars2unique-numbers (cadr node)))
                     ;(right-operand (substitute-var-value-in-fun-body context var-name value expr))
                     (left-PhysOp (l2p:any-lr-node2por (car node)))
                     (right-PhysOp (l2p:any-lr-node2por (caddr new-fun-def)))
                    )
                ;(pp new-fun-def)
                `(,(l2p:tuple-size right-PhysOp)
                   (PPReturn 
                    ,(map  cadr (cadr new-fun-def))
                    ,left-PhysOp
                    ,right-PhysOp
                   )
                 )
              )
             )
             
             ; *** predicate ***
             ((eq? op-name 'predicate)
              (l2p:predicate2por node))
             
             ; *** order-by ***
             ((eq? op-name 'order-by)
              (l2p:order-by2por node))
             
             ((eq? op-name 'tmp-tuple)
              `(,(length node)
                (PPTuple ,@(map l2p:any-lr-node2por node))))
             
             ; *** lreturn ***
             ((eq? op-name 'lreturn)
              (let* ((new-fun-def (l2p:rename-vars2unique-numbers (cadr node)))
                     ;(right-operand (substitute-var-value-in-fun-body context var-name value expr))
                     (left-PhysOp (l2p:any-lr-node2por (car node)))
                     (right-PhysOp (l2p:any-lr-node2por (caddr new-fun-def)))
                     (tsr (l2p:tuple-size right-PhysOp))
                     (tsl (l2p:tuple-size left-PhysOp))
                    )
                ;(pp new-fun-def)
                `(,tsr
                   (PPReturn 
                    ,(map  cadr (cadr new-fun-def))
                    (,tsl (PPStore ,left-PhysOp))
                    ,right-PhysOp
                   )
                 )
              )
             )
             
             ((eq? op-name 'let@)
              (let* ((new-fun-def (l2p:rename-vars2unique-numbers (cadr node)))
                     ;(right-operand (substitute-var-value-in-fun-body context var-name value expr))
                     (left-PhysOp (l2p:any-lr-node2por (car node)))
                     (right-PhysOp (l2p:any-lr-node2por (caddr new-fun-def)))
                    )
                ;(pp new-fun-def)
                `(,(l2p:tuple-size right-PhysOp)
                   (PPLet 
                    ,(map  cadr (cadr new-fun-def))
                    ,left-PhysOp
                    ,right-PhysOp
                   )
                 )
              )
             )

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
                           
                           ((or (eq? test-type 'text-test)
                                (eq? test-type 'node-test)
                                (eq? test-type 'comment-test)
                                (eq? test-type 'pi-test))
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
             
             ; *** namespace ***
             ((eq? op-name 'namespace)
              `(1 (PPNamespace ,(caddr (car node)) ,(l2p:any-lr-node2por (cadr node)))))
                           


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
;             ((eq? op-name 'lsome)
;              (let* ((select-left-PhysOp (l2p:any-lr-node2por (cadr node)))
;                     (tsls (l2p:tuple-size select-left-PhysOp))
;                    )
;              `(1 (PPFnExists (,tsls (PPSelect ,(map  cadr (cadr (caddr node)))
;                                               (,tsls (PPStore ,select-left-PhysOp))
;                                               ,(l2p:any-lr-node2por (caddr (caddr node)))
;                                     )
;                              )
;                  )
;               )
;              )
;             )
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
;             ((eq? op-name 'levery)
;              (let* ((select-left-PhysOp (l2p:any-lr-node2por (cadr node)))
;                     (tsls (l2p:tuple-size select-left-PhysOp))
;                    )              
;              `(1 (PPFnEmpty  (,tsls (PPSelect ,(map  cadr (cadr (caddr node)))
;                                               (,tsls (PPStore ,select-left-PhysOp))
;                                               (1 (PPFnNot,(l2p:any-lr-node2por (caddr (caddr node)))))
;                                     )
;                              )
;                  )
;               )
;              )
;             )

             ; *** exists ***
             ((eq? op-name 'exists)
              `(1 (PPFnExists  ,(l2p:any-lr-node2por (car node))))
             )


             
             ; *** const ***
             ((eq? op-name 'const) 
                  (cond ((eq? (cadr node) 'true#) `(1 (PPFnTrue)))
                        ((eq? (cadr node) 'false#) `(1 (PPFnFalse)))
                        (else `(1 (PPConst ,(cadr node) ,(l2p:lr-atomic-type2por-atomic-type (cadar node)))))
                  )
             )
             ; *** if@ ***
             ((eq? op-name 'if@)
              (let* ((then-expr (l2p:any-lr-node2por (cadr node)))
                     (else-expr (l2p:any-lr-node2por (caddr node)))
                     (ts-then (l2p:tuple-size then-expr))
                     (ts-else (l2p:tuple-size else-expr))
                    )

                (if
                 (or (eq? ts-then ts-else)
                     (equal? else-expr '(1 (PPNil))))
                 `(,ts-then (PPIf
                             ,(l2p:any-lr-node2por (car node))
                             ,then-expr
                             ,else-expr
                             )
                   )
                 (cl:signal-input-error
                  SE4008
                  "bad input logical plan: tuple-size of then expr not equal to tuple-size of else expr")
                )
              )
             )
             
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
              (let* ((expr (car node))
                     (type (cadr node))
                     )
  
                (if (list? (cadr type))
                   `(1 (PPCast ,(l2p:any-lr-node2por expr) ,(l2p:lr-atomic-type2por-atomic-type (cadadr type)) #t))
                   `(1 (PPCast ,(l2p:any-lr-node2por expr) ,(l2p:lr-atomic-type2por-atomic-type (cadr type)) #f))
                )               
              ) 
             )
             
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
             ((eq? op-name 'spaceseq)
              (if (null? node) 
                  '(1 (PPNil))
                  (let ((seq-operands (map l2p:any-lr-node2por node)))
                  `(,(l2p:tuple-size (car seq-operands)) (PPSpaceSequence ,@seq-operands)))))
             
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
              `(1 (PPIntersect ,(l2p:any-lr-node2por (car node)) ,(l2p:any-lr-node2por (cadr node))))
             )             
                                       
             
             ; *** not ***
             ((eq? op-name '!fn!not)
              `(1 (PPFnNot ,(l2p:any-lr-node2por (car node))))
             )
             ; *** string-value ***
             ((eq? op-name '!fn!string-value)
              `(1 (PPDmStringValue ,(l2p:any-lr-node2por (car node))))
             )

             ; *** typed-value ***
             ((eq? op-name '!fn!typed-value)
              `(1 (PPDmTypedValue ,(l2p:any-lr-node2por (car node))))
             )
             
             ; *** sum ***
             ((eq? op-name '!fn!sum)
              `(1 (PPFnSum ,(l2p:any-lr-node2por (car node))))
             )
             
             ; *** avg ***
             ((eq? op-name '!fn!avg)
              `(1 (PPFnAvg ,(l2p:any-lr-node2por (car node))))
             )
             
             ; *** max ***
             ((eq? op-name '!fn!max)
              `(1 (PPFnMax ,(l2p:any-lr-node2por (car node))))
             )
             
             ; *** min ***
             ((eq? op-name '!fn!min)
              `(1 (PPFnMin ,(l2p:any-lr-node2por (car node))))
             )
             
             ; *** count ***
             ((eq? op-name '!fn!count)
              `(1 (PPFnCount ,(l2p:any-lr-node2por (car node))))
             )
             ; *** distinct-values ***
             ((eq? op-name '!fn!distinct-values)
              `(1 (PPFnDistinctValues ,(l2p:any-lr-node2por (car node))))
             )
                          
             ; *** item-at ***
             ((eq? op-name '!fn!item-at)
              `(1 (PPFnItemAt ,(l2p:any-lr-node2por (car node)) ,(l2p:any-lr-node2por (cadr node))))
             )
             
             ; *** concat ***
             ((eq? op-name '!fn!concat)
              `(1 (PPFnConcat ,@(map l2p:any-lr-node2por node)))
             )

             ; *** contains ***
             ((eq? op-name '!fn!contains)
              `(1 (PPFnContains ,(l2p:any-lr-node2por (car node)) ,(l2p:any-lr-node2por (cadr node))))
             )
             
             ; *** translate ***
             ((eq? op-name '!fn!translate)
              `(1 (PPFnTranslate ,(l2p:any-lr-node2por (car node)) 
								 ,(l2p:any-lr-node2por (cadr node))
								 ,(l2p:any-lr-node2por (caddr node))))
             )
             
             ; *** string-length ***
             ((eq? op-name '!fn!string-length)
              `(1 (PPFnStringLength ,(l2p:any-lr-node2por (car node))))
             )
             
             ; *** string ***
             ((eq? op-name '!fn!string)
              `(1 (PPFnString ,(l2p:any-lr-node2por (car node))))
             )
             
             ; *** data ***
             ((eq? op-name '!fn!data)
              `(1 (PPFnData ,(l2p:any-lr-node2por (car node))))
             )
             
             ; *** empty ***
             ((eq? op-name '!fn!empty)
              `(1 (PPFnEmpty ,(l2p:any-lr-node2por (car node))))
             )

             
             
             ; *** node-name ***
             ((eq? op-name '!fn!node-name) 
              `(1 (PPDmNodeName ,(l2p:any-lr-node2por (car node))))
             )
             
             ; *** !fn!name ***
             ((eq? op-name '!fn!name) 
              `(1 (PPFnName ,(l2p:any-lr-node2por (car node))))
             )
             
             ; *** !fn!document-uri ***
             ((eq? op-name '!fn!document-uri) 
              `(1 (PPFnDocumentURI ,(l2p:any-lr-node2por (car node))))
             )
             
             ; *** error ***
             ((eq? op-name '!fn!error) 
              `(1 (PPFnError ,(l2p:any-lr-node2por (car node))))
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

             ; *** !fn!years-from-duration ***
             ((eq? op-name '!fn!years-from-duration)
              `(1 (PPFnYearsFromDuration ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!months-from-duration***
             ((eq? op-name '!fn!months-from-duration)
              `(1 (PPFnMonthsFromDuration ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!days-from-duration ***
             ((eq? op-name '!fn!days-from-duration)
              `(1 (PPFnDaysFromDuration ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!hours-from-duration ***
             ((eq? op-name '!fn!hours-from-duration)
              `(1 (PPFnHoursFromDuration ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!minutes-from-duration ***
             ((eq? op-name '!fn!minutes-from-duration)
              `(1 (PPFnMinutesFromDuration ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!seconds-from-duration ***
             ((eq? op-name '!fn!seconds-from-duration)
              `(1 (PPFnSecondsFromDuration ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!year-from-dateTime***
             ((eq? op-name '!fn!year-from-dateTime)
              `(1 (PPFnYearFromDateTime ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!month-from-dateTime***
             ((eq? op-name '!fn!month-from-dateTime)
              `(1 (PPFnMonthFromDateTime ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!day-from-dateTime***
             ((eq? op-name '!fn!day-from-dateTime)
              `(1 (PPFnDayFromDateTime ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!hours-from-dateTime***
             ((eq? op-name '!fn!hours-from-dateTime)
              `(1 (PPFnHoursFromDateTime ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!minutes-from-dateTime***
             ((eq? op-name '!fn!minutes-from-dateTime)
              `(1 (PPFnMinutesFromDateTime ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!seconds-from-dateTime***
             ((eq? op-name '!fn!seconds-from-dateTime)
              `(1 (PPFnSecondsFromDateTime ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!timezone-from-dateTime***
             ((eq? op-name '!fn!timezone-from-dateTime)
              `(1 (PPFnTimezoneFromDateTime ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!year-from-date***
             ((eq? op-name '!fn!year-from-date)
              `(1 (PPFnYearFromDate ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!month-from-date***
             ((eq? op-name '!fn!month-from-date)
              `(1 (PPFnMonthFromDate ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!day-from-date***
             ((eq? op-name '!fn!day-from-date)
              `(1 (PPFnDayFromDate ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!timezone-from-date***
             ((eq? op-name '!fn!timezone-from-date)
              `(1 (PPFnTimezoneFromDate ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!hours-from-time***
             ((eq? op-name '!fn!hours-from-time)
              `(1 (PPFnHoursFromTime ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!minutes-from-time***
             ((eq? op-name '!fn!minutes-from-time)
              `(1 (PPFnMinutesFromTime ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!seconds-from-time***
             ((eq? op-name '!fn!seconds-from-time)
              `(1 (PPFnSecondsFromTime ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!timezone-from-time***
             ((eq? op-name '!fn!timezone-from-time)
              `(1 (PPFnTimezoneFromTime ,(l2p:any-lr-node2por (car node))))
             )

             ; *** !fn!adjust-dateTime-to-timezone***
             ((eq? op-name '!fn!adjust-dateTime-to-timezone)
              `(1 (PPFnAdjustDateTimeToTimezone ,@(map l2p:any-lr-node2por node)))
             )

             ; *** !fn!adjust-date-to-timezone***
             ((eq? op-name '!fn!adjust-date-to-timezone)
              `(1 (PPFnAdjustDateToTimezone ,@(map l2p:any-lr-node2por node)))
             )

             ; *** !fn!adjust-time-to-timezone***
             ((eq? op-name '!fn!adjust-time-to-timezone)
              `(1 (PPFnAdjustTimeToTimezone ,@(map l2p:any-lr-node2por node)))
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
                   (1 (PPConst 0 !xs!integer))
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
               (let* ((func-index (l2p:find-func-index (caddr (car node)) funcs-map)))
                 (if (eq? func-index #f)
                     (cl:signal-input-error SE4008 (string-append "unknown function call: "
                                                     (cadr (caddr (car node)))))
                     `(1 (PPFunCall ,func-index ,@(map l2p:any-lr-node2por (cdr node)))))))
             
             
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
             
             ((eq? op-name 'create-fulltext-index)
              ; ATTENTION: `node' is bound to the operation content, not the operation!
              (let ((ind-name (l2p:any-lr-node2por (car node)))
                    (AbsPath (l2p:findPPAbsPath (cadr node)))
                    (ind-type-str (caddr (caddr node))))                
                (let ((entity (cadr AbsPath))
                      (abs-path (caddr AbsPath)))
                  `(PPCreateFtIndex ,(if (eq? var-count 0) 0 (+ var-count 1))
                                    ,entity
                                    ,abs-path
                                    ,ind-type-str
                                    ,ind-name
                                    ,@(if (= (length node) 4)  ; optional parameters presented
                                          (list (l2p:any-lr-node2por (list-ref node 3)))
                                          '()))
              )))
             
             ((memv op-name '(drop-index drop-fulltext-index))
              (let* ((ind-name (l2p:any-lr-node2por (car node))))
              `(,(cdr
                  (assq op-name '((drop-index . PPDropIndex)
                                  (drop-fulltext-index . PPDropFtIndex))))
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
             
             ((eq? op-name 'drop-collection)
              (let* ((col (l2p:any-lr-node2por (car node))))
                `(PPDropCollection ,(if (eq? var-count 0) 0 (+ var-count 1))
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
             

                          
             (else (cl:signal-input-error SE4008 (string-append "unknown logical operation given: "
                                                    (symbol->string op-name)))))))
        (else (cl:signal-input-error SE4008 "unknown logical operation given: "
                                     (symbol->string node)))))

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
          ((eq? lr-occ-ind 'empty-test) `(empty xdt_untypedAtomic))

             
          ((or (eq? lr-occ-ind 'one)
               (eq? lr-occ-ind 'optional)
               (eq? lr-occ-ind 'zero-or-more)
               (eq? lr-occ-ind 'one-or-more))
              (let* ((item-type (cadr SeqType))
                     (por-occ-ind (l2p:lr-oocur-ind2por-occur-ind lr-occ-ind)))
                (cond ((symbol? item-type) ;built-in atomic type
                          `(,por-occ-ind ,(l2p:lr-atomic-type2por-atomic-type item-type)))
                      ((string? (car item-type)) ;not built-in atomic type
                          (cl:signal-user-error SE4008 (string-append " non built-in type is not supported: "
                                                (apply string-append item-type))))
                      
                      ((eq? (car item-type) 'doc-test)
                          `(,por-occ-ind (document ,(l2p:lr-elem-test2por-elem-test (cadr item-type)))))
                      
                      ((eq? (car item-type) 'elem-test)
                          `(,por-occ-ind (element ,(l2p:lr-elem-test2por-elem-test item-type))))
                      
                      ((eq? (car item-type) 'attr-test)
                          `(,por-occ-ind (attribute ,(l2p:lr-attr-test2por-attr-test item-type))))
                      
                      ((eq? (car item-type) 'comment-test)
                          `(,por-occ-ind (comment)))
                      
                      ((eq? (car item-type) 'text-test)
                          `(,por-occ-ind (text)))
                      
                      ((eq? (car item-type) 'item-test)
                          `(,por-occ-ind (item)))
                      
                      ((eq? (car item-type) 'node-test)
                          `(,por-occ-ind (node)))
                      
                      (else (cl:signal-input-error SE4008 (string-append "unknown item-type: "
                                       (symbol->string (car item-type))))))))
          (else (cl:signal-error SE4008 (string-input-append "unknown occurance indicator: "
                                      (symbol->string lr-occ-ind)))))))



(define (l2p:lr-atomic-type2por-atomic-type atomic-type)
  atomic-type)

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
    (cond ((eq? test-type 'ename)
              (let* ((elem-name (cadr (cadr elem-test)))
                     (elem-type (caddr (cadr elem-test))))
                (cond ((and (eq? (caddr elem-name) 'unspecified) ; element()
                            (eq? (cadr elem-type) 'unspecified)) 
                       `())
                      
                      ((and (eq? (caddr elem-name) '*) ; element(*)
                            (eq? (cadr elem-type) 'unspecified))
                       `(element_wildcard))
                      
                      ((and (list? (caddr elem-name))
                            (eq? (cadr elem-type) 'unspecified)) ; element(fo:name)
                       `(element_name ,(caddr elem-name)))
                      
                      ((and (eq? (caddr elem-name) '*) ; element(*,*)
                            (eq? (cadr elem-type) '*))
                       `(element_wildcard_wildcard))
                      
                      ((and (eq? (caddr elem-name) '*) ; element(*, fo:type-name)
                            (list? (cadr elem-type)))
                       `(element_wildcard_name ,(cadr elem-type)))
                      
                      ((and (list? (caddr elem-name)) ; element(fo:name, *)
                            (eq? (cadr elem-type) '*))
                       `(element_name_wildcard ,(caddr elem-name)))
                      
                      ((and (list? (caddr elem-name))
                            (list? (cadr elem-type)))
                       `(element_name_name ,(caddr elem-name) ,(cadr elem-type)))
                      
                      (else (cl:signal-input-error SE4008 (string-append 
                                    (string-append "unknown combination of element name and element test: "
                                                  (l2p:list2string elem-name))
                                    (l2p:list2string elem-type)))))))
                                                                  
          ((eq? test-type 'sname)
              (cl:signal-user-error SE4008 "sname is not supported in elem-test"))
          (else (cl:signal-input-error SE4008 (string-append "unknown test-type: " (symbol->string test-type)))))))
       

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
  (let* ((vars-map (l2p:generate-map (map cadr (map cadr (cadr fun-def)))))
        (new-var-decls (l2p:generate-new-fun-decl vars-map (xlr:var-defs fun-def))))
    ;(display new-var-decls)
    ;(display vars-map)
  `(fun-def ,new-var-decls ,(l2p:rename-vars vars-map (xlr:fun-body fun-def))))
)
  
(define (l2p:generate-map vars)
  (if (null? vars)
      '()
      (append (list(list (car vars) (l2p:gen-var))) (l2p:generate-map (cdr vars))))
)

(define (l2p:generate-new-fun-decl vars var_decls)
  (if (null? vars)
      `()
      (append (list (list (caar var_decls) (cadar vars))) (l2p:generate-new-fun-decl (cdr vars) (cdr var_decls))))
)

(define (l2p:rename-vars vars-map expr)
  (if (null? vars-map)
      expr
      (l2p:rename-vars 
       (cdr vars-map)
       (xlr:substitute-var-value `(var ,(caar vars-map)) `(var ,(cadar vars-map)) expr)))
)

(define (l2p:add-func-name func-name)
  (if (null? funcs-map)
      (set! funcs-map `((,func-name 0)))
      (set! funcs-map (append funcs-map `((,func-name ,(length funcs-map)))))
  ))

(define (l2p:find-func-index func-name funcs-list)

  (cond ((null? funcs-list) #f)
        ((and (string=? (caaar funcs-list) (car func-name)) (string=? (cadaar funcs-list) (cadr func-name)))
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
           ;(pp source-child)
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
                 ((null? (cdr others))  ; a single member
                  (l2p:any-lr-node2por
                   (l2p:replace-pos-last2numbers (car others)
                                                 pos-num last-num)))
                 (else
                  (l2p:any-lr-node2por
                   (l2p:replace-pos-last2numbers
                    (cons 'and@ (reverse others))
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

; Replaces each 'unio with 'tmp-tuple and extends its arguments with ExprSingle-list
; from OrderSpecList
(define (l2p:replace-unio2tmp-tuple expr expr-single-lst)
  (cond
    ((not (pair? expr))  ; leaf node
     expr)
    ((eq? (car expr) 'unio)
     ;(write expr)
     ;(newline)
     (cons 'tmp-tuple
           (append (cdr expr) expr-single-lst)))    
    (else
     (map
      (lambda (kid) (l2p:replace-unio2tmp-tuple kid expr-single-lst))
      expr))))

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
       ,(l2p:any-lr-node2por
         (l2p:replace-unio2tmp-tuple subexpr (map caddr spec-lst)))
       ,(map
          (lambda (modif)
            (if
             (null? (cdr modif))  ; no arguments at all
             '(default default)
             (list
              (cond
                ((null? (cddr modif))  ; no empty status
                 'default)
                ((assoc (caddr (caddr modif))  ; value of the 1st const
                        '(("empty-greatest" . greatest)
                          ("empty-least" . least)))
                 => cdr)
                (else
                 'default))
              (cdr (assoc (caddr (cadr modif))  ; value of the 1st const
                          '(("asc" . ascending)
                            ("desc" . descending)))))))
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
