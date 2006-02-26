
; File:  test-lr2por.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

;;======================= Tests for mode management 
;(pp "Tests for mode")
;(l2p:init-subquery-modes 
; '(subqueries
;   (subquery source-name source-type source-brand doc-name dtd-path query 1)
;   (subquery source-name source-type source-brand doc-name dtd-path query 2)
;   (subquery source-name source-type source-brand doc-name dtd-path query 3)))
;
;(l2p:update-subquery-modes 1 771)
;(l2p:update-subquery-modes 2 772)
;(l2p:update-subquery-modes 3 773)
;
;(pp (l2p:get-subqueries-with-modes-inserted))
;
;;====================== Tests for calculate (any-lr-node2por)
;(pp "Tests for calculate")
;(pp
;(l2p:build-PPCalculate 
; '(+@ (type u) 
;   (-@ (type u) (const (type u) 1) (const (type u) 2))
;   (const (type u) 3))))
;
;(pp
;(l2p:build-PPCalculate 
; '(and@ (type u) 
;   (const (type u) 1) (const (type u) 2))))
;
;(pp
;(l2p:build-PPCalculate 
; '(and@ (type u) 
;   (=@ (type u) (return (type u) (const (type u) 2) (const (type u) 1)) (const (type u) 5))
;   (>@ (type u) (return (type u) (const (type u) 6) (const (type u) 3)) (const (type u) 4)))))
;
;(pp
;(l2p:build-PPCalculate 
; '(unary+@ (type u) 
;   (unary-@ (type u) (const (type u) 1)))))
;
;(pp
;(l2p:build-PPCalculate 
; '(unary+@ (type u) 
;   (not@ (type u) (const (type u) 1)))))
;
;(pp
;(l2p:build-PPCalculate 
; '(unary+@ (type u) 
;   (not@ (type u) (const (type u) 1)))))
;
;(pp
;(l2p:build-PPCalculate 
; '(unary+@ (type u) $v )))
;
;;ok (arises error)
;;(pp
;;(l2p:build-PPCalculate 
;; '(child (type u) (const (type u) 1) (const (type u) 2))))

;=================================== Tests for lr2por (any-lr-node2por)


;(pp
; (l2p:lr2por
;  '(query ( prolog )( query-body (instance-of (const (type !xs!integer)5 ) (type (one !xs!integer )))))))

;(pp
; (l2p:lr2por 
;'(update (replace (ddo (return (child (ddo (child (ddo (descendant-or-self (!fn!document (const (type !xs!string )"privileges_list" ))(type ( node-test ))))(type (elem-test (ename (const (type !xs!QName )("" "roles" ))(type * )(const (type !xs!string )"non-nil" ))))))(type (elem-test (ename (const (type !xs!QName )("" "role" ))(type * )(const (type !xs!string )"non-nil" )))))(fun-def (
;(!xs!anyType $v ))(if@ (=@ (ddo (child (var $v )(type (elem-test (ename (const (
;type !xs!QName )("" "role_name" ))(type * )(const (type !xs!string )"non-nil" ))
;))))(const (type !xs!string )"role1" ))(var $v )( sequence )))))(fun-def ( (type
; (one ("" "TypeSequence" ))$x ))(ddo (return (child (ddo (descendant-or-self (!fn!document (const (type !xs!string )"privileges_list" ))(type ( node-test ))))(type (elem-test (ename (const (type !xs!QName )("" "user" ))(type * )(const (type
; !xs!string )"non-nil" )))))(fun-def ( (!xs!anyType $v ))(if@ (=@ (ddo (child (var $v )(type (elem-test (ename (const (type !xs!QName )("" "user_name" ))(type * )(const (type !xs!string )"non-nil" ))))))(const (type !xs!string )"user1" ))(var $v )( sequence ))))))))))

;(pp
; (l2p:lr2por
;  '(query ( prolog )( query-body (!fn!document (element (const (type !xs!QName )("" "a" ))( sequence )))))
;  ))

(pp
(l2p:lr2por
 '(query (prolog) (query-body (up  (const (type !xs!string) "petya") (const (type !xs!integer) 50)))
)))
 

;(pp
; (l2p:lr2por '(query
; (prolog)
; (query-body
;   (return
;     (ddo
;      (child
;       (ddo
;        (child
;         (ddo
;          (child
;           (!fn!document (const (type !xs!string) "xmark"))
;           (type (elem-test (ename (const (type !xs!QName) ("" "site")) (type *) (const (type !xs!string) "non-nil"))))))
;         (type (elem-test (ename (const (type !xs!QName) ("" "people")) (type *) (const (type !xs!string) "non-nil"))))))
;       (type (elem-test (ename (const (type !xs!QName) ("" "person")) (type *) (const (type !xs!string) "non-nil"))))))
;     (fun-def
;       ((xs:anyType $x))
;       (return
;         (ddo
;          (child
;           (ddo
;            (child
;             (ddo
;              (child
;               (!fn!document (const (type !xs!string) "xmark"))
;               (type
;                (elem-test (ename (const (type !xs!QName) ("" "site")) (type *) (const (type !xs!string) "non-nil"))))))
;             (type
;              (elem-test
;                (ename (const (type !xs!QName) ("" "open_auctions")) (type *) (const (type !xs!string) "non-nil"))))))
;           (type
;            (elem-test (ename (const (type !xs!QName) ("" "open_auction")) (type *) (const (type !xs!string) "non-nil"))))))
;         (fun-def
;           ((xs:anyType $y))
;           (if@
;            (=@
;             (ddo
;              (attribute
;                (var $x)
;                (type (attr-test (ename (const (type !xs!QName) ("" "id")) (type *) (const (type !xs!string) "non-nil"))))))
;             (ddo
;              (attribute
;                (ddo
;                 (child
;                  (var $y)
;                  (type
;                   (elem-test
;                     (ename (const (type !xs!QName) ("" "seller")) (type *) (const (type !xs!string) "non-nil"))))))
;                (type
;                 (attr-test (ename (const (type !xs!QName) ("" "person")) (type *) (const (type !xs!string) "non-nil")))))))
;            (sequence (var $x) (var $y))
;            (sequence))))))))))

;(pp
; (l2p:lr2por '(update (delete (ddo (child (ddo (child (!fn!document (const (type !xs!string )"region" ))(type (elem-test (ename (const (type !xs!QName )* )(type * )(const (type !xs!string )"non-nil" ))))))(type (elem-test (ename (const (type !xs!QName )*
; )(type * )(const (type !xs!string )"non-nil" ))))))))))

;(substitute-var-value-in-fun-body '((unknown $v)) '$x '$y '(op $y))
;(l2p:generate-map '($x  $z))
;(l2p:generate-new-fun-decl '(($x 0) ($y 1) ($z 2)) '((unk $x) (unk $y) (unk $z)))
; 


;================================= Test for l2p:rename-vars2unique-numbers
;ok
;(pp
; (l2p:rename-vars2unique-numbers 
; '(fun-def
;       ((xs:anyType $a))
;       (return
;         (sequence (var $a) (var $b) (!fn!document (const (type !xs!string) "b.xml")))
;         (fun-def
;           ((xs:anyType $m))
;           (element 
;            (const (type !xs!QName) ("" "a"))
;            (sequence 
;              (const (type !xs!string) "Petya")
;              (var $m))))))))

;ok
;(pp
; (l2p:rename-vars2unique-numbers 
; '(fun-def
;       ((xs:anyType $a) (xs:anyType $b))
;       (return
;         (sequence (var $a) (var $b) (!fn!document (const (type !xs!string) "b.xml")))
;         (fun-def
;           ((xs:anyType $m))
;           (element 
;            (const (type !xs!QName) ("" "a"))
;            (sequence 
;              (const (type !xs!string) "Petya")
;              (var $m)
;              (var $b))))))))