
; File:  test-lr2por-max.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

;================= Tests for l2p:findPPAbsPath ========================
;ok
;result: 
;#f
(pp 
  (l2p:findPPAbsPath 
   '(ddo (!fn!document (const (type !xs!string) "a.xml")))))

;ok
;result:
;(PPAbsPath
;  (document "a.xml")
;  (((PPAxisChild qname ("" "a")))))
(pp 
  (l2p:findPPAbsPath 
   '(child
       (!fn!document (const (type !xs!string) "a.xml"))
       (type
        (elem-test
          (ename (const (type !xs!QName) ("" "a")) 
                 (type *)
                 (const (type !xs!string) "non-nil")))))))

;ok
;result:
;(PPAbsPath
;  (document "a.xml")
;  (((PPAxisChild qname ("" "a")))
;   ((PPAxisChild qname ("" "b")))))
(pp 
  (l2p:findPPAbsPath 
   '(ddo
    (child
     (ddo
      (child
       (!fn!document (const (type !xs!string) "a.xml"))
       (type
        (elem-test
          (ename (const (type !xs!QName) ("" "a")) 
                 (type *)
                 (const (type !xs!string) "non-nil"))))))
     (type
      (elem-test
        (ename (const (type !xs!QName) ("" "b"))
               (type *)
               (const (type !xs!string) "non-nil"))))))))

;ok
;result:
;(PPAbsPath
;  (document "a.xml")
;  (((PPAxisChild qname ("" "a")))
;   ((PPAxisAttribute qname ("" "b")))))
(pp 
  (l2p:findPPAbsPath 
   '(ddo
    (attr-axis
     (ddo
      (child
       (!fn!document (const (type !xs!string) "a.xml"))
       (type
        (elem-test
          (ename (const (type !xs!QName) ("" "a")) 
                 (type *)
                 (const (type !xs!string) "non-nil"))))))
     (type
      (attr-test
        (ename (const (type !xs!QName) ("" "b"))
               (type *))))))))

;ok
;result:
;#f
(pp 
  (l2p:findPPAbsPath 
   '(+@
   (ddo
    (child
     (ddo
      (child
       (!fn!document (const (type !xs!string) "a.xml"))
       (type
        (elem-test
          (ename (const (type !xs!QName) ("" "a")) 
                 (type *)
                 (const (type !xs!string) "non-nil"))))))
     (type
      (elem-test
        (ename (const (type !xs!QName) ("" "b"))
               (type *)
               (const (type !xs!string) "non-nil"))))))
   (const (type !xs!integer) 3))))

;ok
;result:
;(PPAbsPath
;  (collection "coll-name")
;  (((PPAxisChild qname ("" "a")))
;   ((PPAxisChild qname ("" "b")))))
(pp 
  (l2p:findPPAbsPath 
   '(ddo
    (child
     (ddo
      (child
       (!fn!document (const (type !xs!string) "a.xml"))
       (type
        (elem-test
          (ename (const (type !xs!QName) ("" "a")) 
                 (type *)
                 (const (type !xs!string) "non-nil"))))))
     (type
      (elem-test
        (ename (const (type !xs!QName) ("" "b"))
               (type *)
               (const (type !xs!string) "non-nil"))))))))

;ok
;result: 
;(PPAbsPath
;  (document "x")
;  (((PPAxisChild qname ("" "a")))
;   ((PPAxisDescendantAttr qname ("" "b")))))
(pp 
  (l2p:findPPAbsPath 
   '(ddo 
     (descendant-attr 
      (ddo (child (!fn!document (const (type !xs!string) "x")) (type (elem-test (ename (const (type !xs!QName) ("" "a")) (type *) (const (type !xs!string) "non-nil"))))))
      (type (attr-test (ename (const (type !xs!QName) ("" "b")) (type *) (const (type !xs!string) "non-nil")))))
     )))

;================= Tests for l2p:build-PPCalculate ========================
;ok
;result:
;(PPCalculate
;  (BinaryOpAdd (LeafAtomOp 0) (BinaryOpSubstract (LeafAtomOp 1) (LeafAtomOp 2)))
;  (const (type !xs!integer) 3)
;  (const (type !xs!integer) 2)
;  (const (type !xs!integer) 5))
(pp
(l2p:build-PPCalculate 
 '(+@
    (const (type !xs!integer) 3)
    (-@ (const (type !xs!integer) 2) (const (type !xs!integer) 5)))))
