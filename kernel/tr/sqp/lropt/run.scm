(require (lib "defmacro.ss"))
(define-macro (declare . x) #t)

(require (lib "list.ss" "srfi" "1"))
(load "../common-lib.scm")
(load "../xquery-lr-lib.scm")
(load "lreturn.scm")

;------------------------

(lropt:rewrite-query
 '(query
   (prolog
    (declare-function
     (const
      (type !xs!QName)
      ("http://www.w3.org/2004/07/xquery-local-functions" "insert_elem"))
     (((one (node-test)) (var ("" "e"))) ((zero-or-more (node-test)) (var ("" "s"))))
     (result-type (zero-or-more (node-test)))
     (body
      (if@
       (!fn!empty (var ("" "s")))
       (var ("" "e"))
       (if@
        (le@
         (ddo (child (var ("" "e")) (type (text-test))))
         (ddo
          (child
           (predicate
            (var ("" "s"))
            (fun-def
             ((!xs!anyType (var ("" "$%v"))))
             (=@ (!fn!position) (const (type !xs!integer) "1"))))
           (type (text-test)))))
        (sequence (var ("" "e")) (var ("" "s")))
        (sequence
          (predicate
           (var ("" "s"))
           (fun-def
            ((!xs!anyType (var ("" "$%v"))))
            (=@ (!fn!position) (const (type !xs!integer) "1"))))
          (fun-call
           (const
            (type !xs!QName)
            ("http://www.w3.org/2004/07/xquery-local-functions" "insert_elem"))
           (var ("" "e"))
           (predicate
            (var ("" "s"))
            (fun-def
             ((!xs!anyType (var ("" "$%v"))))
             (>@ (!fn!position) (const (type !xs!integer) "1"))))))))))
    (declare-function
     (const
      (type !xs!QName)
      ("http://www.w3.org/2004/07/xquery-local-functions" "sort_by_name"))
     (((zero-or-more (node-test)) (var ("" "s"))))
     (result-type (zero-or-more (node-test)))
     (body
      (if@
       (!fn!empty (var ("" "s")))
       (var ("" "s"))
       (fun-call
        (const
         (type !xs!QName)
         ("http://www.w3.org/2004/07/xquery-local-functions" "insert_elem"))
        (predicate
         (var ("" "s"))
         (fun-def
          ((!xs!anyType (var ("" "$%v"))))
          (=@ (!fn!position) (const (type !xs!integer) "1"))))
        (fun-call
         (const
          (type !xs!QName)
          ("http://www.w3.org/2004/07/xquery-local-functions" "sort_by_name"))
         (predicate
          (var ("" "s"))
          (fun-def
           ((!xs!anyType (var ("" "$%v"))))
           (>@ (!fn!position) (const (type !xs!integer) "1"))))))))))
   (query-body
    (fun-call
     (const
      (type !xs!QName)
      ("http://www.w3.org/2004/07/xquery-local-functions" "sort_by_name"))
     (ddo
      (child
       (!fn!index-scan-between
        (const (type !xs!string) "zips")
        (const (type !xs!integer) "12")
        (const (type !xs!integer) "25")
        (const (type !xs!string) "SEG"))
       (type
        (elem-test
         (ename
          (const (type !xs!QName) ("" "name"))
          (type *)
          (const (type !xs!string) "non-nil"))))))))))