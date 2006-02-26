
; File:  test-xquery-lr-lib.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

;========================= Tests for xlr:substitute-var-value =====================
;ok
;(pp (xlr:substitute-var-value 
; '(var $m) 
; '(var $new-m)
; '(return
;     (!fn!document (const (type !xs!string) "a.xml"))
;     (fun-def
;       ((xs:anyType $a))
;       (return
;         (!fn!document (const (type !xs!string) "b.xml"))
;         (fun-def
;           ((xs:anyType $b))
;           (element 
;            (const (type !xs!QName) ("" "a"))
;            (sequence 
;              (const (type !xs!string) "Petya")
;              (var $m)))))))))

;ok
;(pp (xlr:substitute-var-value 
; '(var $m) 
; '(var $new-m)
; '(return
;     (!fn!document (const (type !xs!string) "a.xml"))
;     (fun-def
;       ((xs:anyType $a))
;       (return
;         (sequence (var $m) (!fn!document (const (type !xs!string) "b.xml")))
;         (fun-def
;           ((xs:anyType $m))
;           (element 
;            (const (type !xs!QName) ("" "a"))
;            (sequence 
;              (const (type !xs!string) "Petya")
;              (var $m)))))))))