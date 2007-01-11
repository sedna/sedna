
; File:  lr2por-lib.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

(declare (unit lr2por-lib) (uses srfi-1 xquery-lr common-lib))

;(define l2p:subqueries '(no-subqueries))
;(define l2p:subquery-modes '((1 4)(2 5)(3 8)))

;(define (l2p:init-subquery-modes sq)
;  (set! l2p:subqueries sq)
;  (set! l2p:subquery-modes (map (lambda (s) `(,(eighth s) mode-unknown)) (cdr sq))))
  
;(define (l2p:update-subquery-modes sub-query-no mode)
;  (let ((new-subquery-modes
;         (map (lambda (x)
;                (if (eq? (car x) sub-query-no)
;                    `(,(car x) ,mode)
;                    x))
;              l2p:subquery-modes)))
;    (set! l2p:subquery-modes new-subquery-modes)
;    new-subquery-modes))

;(define (l2p:merge-subqueries-and-modes subqueries modes)
;  (map 
;   (lambda (s m)
;     (reverse `(,(cadr m) ,@(reverse s))))
;   subqueries
;   modes))

;(define (l2p:get-subqueries-with-modes-inserted) 
;  `(subqueries ,@(l2p:merge-subqueries-and-modes (cdr l2p:subqueries) l2p:subquery-modes)))

(define l2p:abs-xpath-axis '(child descendant attr-axis self descendant-or-self descendant-attr))
(define l2p:atom-unary-calc-ops '(unary+@ unary-@))
(define l2p:atom-binary-calc-ops '(+@ -@ eq@ ne@ ge@ gt@ le@ lt@ *@ /@ mod@ idiv@))

(define l2p:bool-unary-calc-ops '(not@))
(define l2p:bool-binary-calc-ops '(and@ or@))

(define l2p:lop2por-op-map 
  '((unary+@ UnaryOpPlus)
    (unary-@ UnaryOpMinus)
    (+@ BinaryOpAdd)
    (-@ BinaryOpSubtract)
    (eq@ BinaryValEQ)
    (ne@ BinaryValNE)
    (ge@ BinaryValGE)
    (gt@ BinaryValGT)
    (le@ BinaryValLE)
    (lt@ BinaryValLT)
    (*@ BinaryOpMultiply)
    (/@ BinaryOpDivide)
    (idiv@ BinaryOpIntegerDivide)
    (mod@ BinaryOpMod)
    (not@ UnaryOpNot)
    (and@ BinaryOpAnd)
    (or@ BinaryOpOr)))


(define (l2p:find-in-op-map lop)
  (let ((r (find (lambda (x) (eq? (car x) lop))
                 l2p:lop2por-op-map)))
           (if r (cadr r) (cl:signal-error "l2p:find-in-op-map error: op is not found in the map"))))

(define l2p:cur-arg-index 999)

(define (l2p:build-PPCalculate e)
  (set! l2p:cur-arg-index -1)
  (let ((result
         (let rpt ((expr e)) 
           (if (xlr:var? expr) 
               `((Leaf ,(begin 
                          (set! l2p:cur-arg-index (+ l2p:cur-arg-index 1))
                          l2p:cur-arg-index)) (,expr))
               (cond 
                 ;In case of atom unary calc ops
                 ((memq (xlr:op-name expr) l2p:atom-unary-calc-ops)
                  (let ((first-arg (rpt (car (xlr:op-args expr)))))
                    `((,(l2p:find-in-op-map (xlr:op-name expr)) 
                       ,(if (eq? (car (car first-arg)) 'Leaf)
                            `(LeafAtomOp ,(cadr (car first-arg)))
                            (car first-arg)))
                      ,(cadr first-arg))))
                 ;In case of atom binary calc ops
                 ((memq (xlr:op-name expr) l2p:atom-binary-calc-ops)
                  (let ((first-arg (rpt (car (xlr:op-args expr)))) 
                        (second-arg (rpt (cadr (xlr:op-args expr)))))
                    `((,(l2p:find-in-op-map (xlr:op-name expr)) 
                       ,(if (eq? (car (car first-arg)) 'Leaf)
                            `(LeafAtomOp ,(cadr (car first-arg)))
                            (car first-arg))
                       ,(if (eq? (car (car second-arg)) 'Leaf)
                            `(LeafAtomOp ,(cadr (car second-arg)))
                            (car second-arg)))
                      ,(append (cadr first-arg) (cadr second-arg)))))
                 ;In case of bool unary calc ops
                 ((memq (xlr:op-name expr) l2p:bool-unary-calc-ops)
                  (let ((first-arg (rpt (car (xlr:op-args expr)))))
                    `((,(l2p:find-in-op-map (xlr:op-name expr)) 
                       ,(if (eq? (car (car first-arg)) 'Leaf)
                            `(LeafEffectBoolOp ,(cadr (car first-arg)))
                            (car first-arg)))
                      ,(cadr first-arg))))
                 ;In case of bool binary calc ops
                 ((memq (xlr:op-name expr) l2p:bool-binary-calc-ops)
                  (let ((first-arg (rpt (car (xlr:op-args expr)))) 
                        (second-arg (rpt (cadr (xlr:op-args expr)))))
                    `((,(l2p:find-in-op-map (xlr:op-name expr)) 
                       ,(if (eq? (car (car first-arg)) 'Leaf)
                            `(LeafEffectBoolOp ,(cadr (car first-arg)))
                            (car first-arg))
                       ,(if (eq? (car (car second-arg)) 'Leaf)
                            `(LeafEffectBoolOp ,(cadr (car second-arg)))
                            (car second-arg)))
                      ,(append (cadr first-arg) (cadr second-arg)))))
                 ;In all other cases
                 (else 
                  `((Leaf 
                     ,(begin (set! l2p:cur-arg-index (+ l2p:cur-arg-index 1))
                             l2p:cur-arg-index))
                    (,expr))))))))
    (if (or (xlr:var? (car result)) (eq? (car (car result)) 'Leaf))
        (cl:signal-error "lr2por: build-PPCalculate is called with not calc-op as parameter") 
        `(PPCalculate ,(car result) ,@(cadr result)))))

(define (l2p:tran-lr-keyword2por-keyword lr-key-word)
  (let ((pair 
         (find
          (lambda (x) 
            (eq? lr-key-word (car x)))
          '((child PPAxisChild) 
            (attr-axis PPAxisAttribute)
            (descendant PPAxisDescendant)
            (self PPAxisSelf)
            (descendant-or-self PPAxisDescendantOrSelf)
            (descendant-attr PPAxisDescendantAttr)))))
    (if pair
        (cadr pair)
        (cl:signal-error "l2p:tran-lr-keyword2por-keyword: unknown keyword"))))
     

(define (l2p:getDocorCollNamePor lr-name)
  (if  (and (eq? 'const (car lr-name))
            (eq? '!xs!string (cadr (cadr lr-name))))
       (caddr lr-name)
       (l2p:any-lr-node2por lr-name)))
  
  
(define (l2p:findPPAbsPath e)
  (let rpt ((expr e) (first-call #t)) 
    (cond 
      ;In case of atom unary calc ops
      ((member (xlr:op-name expr) l2p:abs-xpath-axis)
       (let ((first-arg (rpt (car (xlr:op-args expr)) #f)))
         (if
          first-arg
          `(PPAbsPath 
            ,(cadr first-arg)
            ,(reverse
              (cons
               (list
                (let ((what (xlr:type-value (cadr (xlr:op-args expr)))))
                  (if
                   (eq? (xlr:op-name expr) 'attr-axis)
                   `(PPAxisAttribute
                     ,@(cond 
                         ((eq? (car what) 'attr-test)
                          (if
                           (eq? (car (cadr what)) 'ename)
                           (cond
                             ((equal? (cadr (cadr what)) '(const (type !xs!QName) *))
                              `(wildcard_star ()))
                             ((and
                               (eq? (car (cadr (cadr what))) 'const)
                               (equal? (cadr (cadr (cadr what)))
                                       '(type !xs!QName))
                               (pair? (caddr (cadr (cadr what))))
                               (eq? (car (caddr (cadr (cadr what)))) '*))
                              `(wildcard_star_ncname
                                ,(xlr:local-name (cadr (cadr what)))))
                             ((and (eq? (car (cadr (cadr what))) 'const)
                                   (equal? (cadr (cadr (cadr what)))
                                           '(type !xs!QName))
                                   (pair? (caddr (cadr (cadr what))))
                                   (eq? (cadr (caddr (cadr (cadr what)))) '*))
                              `(wildcard_ncname_star
                                ,(xlr:namespace-name (cadr (cadr what)))))
                             (else 
                              `(qname 
                                (,(xlr:namespace-name (cadr (cadr what)))
                                 ,(xlr:local-name (cadr (cadr what)))
                                 ,@(xlr:ns-prefix (cadr (cadr what))))))) 
                           (cl:signal-error
                            "l2p:findPPAbsPath: instruction is not supported - "
                            (car (cadr what)))))
                         ((eq? (car what) 'node-test)
                          '(wildcard_star ()))
                         (else
                          (cl:signal-error
                           "l2p:findPPAbsPath: unknown attibute axis KindTest"))))
                   `(,(l2p:tran-lr-keyword2por-keyword (xlr:op-name expr))
                     ,@(cond 
                         ((eq? (car what) 'doc-test)
                          (if
                           (null? (cdr what))
                           `(doc_node)
                           (cl:signal-error
                            "l2p:findPPAbsPath: KindTest document(element()) is not supported")))
                         ((eq? (car what) 'elem-test)
                          (if
                           (eq? (car (cadr what)) 'ename)
                           (cond
                             ((equal? (cadr (cadr what)) '(const (type !xs!QName) *))
                              `(wildcard_star ()))
                             ((and
                               (eq? (car (cadr (cadr what))) 'const)
                               (equal? (cadr (cadr (cadr what)))
                                       '(type !xs!QName))
                               (pair? (caddr (cadr (cadr what))))
                               (eq? (car (caddr (cadr (cadr what)))) '*))
                              `(wildcard_star_ncname
                                ,(xlr:local-name (cadr (cadr what)))))
                             ((and
                               (eq? (car (cadr (cadr what))) 'const)
                               (equal? (cadr (cadr (cadr what)))
                                       '(type !xs!QName))
                               (pair? (caddr (cadr (cadr what))))
                               (eq? (cadr (caddr (cadr (cadr what)))) '*))
                              `(wildcard_ncname_star
                                ,(xlr:namespace-name (cadr (cadr what)))))
                             (else 
                              `(qname 
                                (,(xlr:namespace-name (cadr (cadr what)))
                                 ,(xlr:local-name (cadr (cadr what)))
                                 ,@(xlr:ns-prefix (cadr (cadr what))))))) 
                           (cl:signal-error
                            "l2p:findPPAbsPath: instruction is not supported - "
                            (car (cadr what)))))
                         ((eq? (car what) 'attr-test)
                          (if
                           (eq? (car (cadr what)) 'ename)
                           (cond
                             ((equal? (cadr (cadr what))
                                      '(const (type !xs!QName) *))
                              `(wildcard_star ()))
                             ((and
                               (eq? (car (cadr (cadr what))) 'const)
                               (equal? (cadr (cadr (cadr what)))
                                       '(type !xs!QName))
                               (pair? (caddr (cadr (cadr what))))
                               (eq? (car (caddr (cadr (cadr what)))) '*))
                              `(wildcard_star_ncname
                                ,(xlr:local-name (cadr (cadr what)))))
                             ((and
                               (eq? (car (cadr (cadr what))) 'const)
                               (equal? (cadr (cadr (cadr what)))
                                       '(type !xs!QName))
                               (pair? (caddr (cadr (cadr what))))
                               (eq? (cadr (caddr (cadr (cadr what)))) '*))
                              `(wildcard_ncname_star
                                ,(xlr:namespace-name (cadr (cadr what)))))
                             (else 
                              `(qname 
                                (,(xlr:namespace-name (cadr (cadr what)))
                                 ,(xlr:local-name (cadr (cadr what)))
                                 ,@(xlr:ns-prefix (cadr (cadr what))))))) 
                           (cl:signal-error
                            "l2p:findPPAbsPath: instruction is not supported - "
                            (car (cadr what)))))
                         ((eq? (car what) 'comment-test)
                          `(comment ()))
                         ((eq? (car what) 'text-test)
                          `(text ()))
                         ((eq? (car what) 'node-test)
                          `(node ()))
                         ((eq? (car what) 'item-test)
                          `(item ()))
                         ((eq? (car what) 'pi-test)
                          ; DL: implemented by analogue with *** axis ***
                          ; processing in "lr2por.scm"
                          ;(pp what)
                          (list
                           (l2p:lr-test2por-test (car what))
                           (if
                            (null? (cdr what))  ; no target specified
                            '()
                            ;(list
                             (caddr  ; constant value
                              (cadr what)  ; yields '(const (type ...) ...)
                              );)
                           )))
                         (else
                          (cl:signal-error
                           "l2p:findPPAbsPath: unknown KindTest")))))))
               (reverse (caddr first-arg)))))
          #f)))
      ((and (eq? (xlr:op-name expr) '!fn!document) (eq? (length (xlr:op-args expr)) 1))
       ;           (if first-call
       ;               #f
       `(PPAbsPath (document ,(l2p:getDocorCollNamePor (car (xlr:op-args expr))))
                   ())
       ;           )
       )
      ((eq? (xlr:op-name expr) '!fn!collection)
       (if first-call
           #f
           `(PPAbsPath (collection ,(l2p:getDocorCollNamePor (car (xlr:op-args expr))))
                       ())))
      ((eq? (xlr:op-name expr) 'ddo)
       (rpt (car (xlr:op-args expr)) #t))
      (else #f))))
                       
