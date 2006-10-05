(require (lib "defmacro.ss"))
(require (rename (lib "pretty.ss") pp pretty-print))
(define-macro (declare . x) #t)

(require (lib "list.ss" "srfi" "1"))
(load "../common-lib.scm")
(load "../xquery-lr-lib.scm")
(load "lreturn.scm")

;------------------------

(lropt:rewrite-query
 '(query
   (prolog)
   (query-body
    (ddo
     (attr-axis
      (ddo
       (child
        (ddo
         (child
          (ddo
           (descendant-or-self
            (ddo
             (child
              (element (const (type !xs!QName) ("" "a")) (sequence))
              (type
               (elem-test
                (ename
                 (const (type !xs!QName) ("" "b"))
                 (type *)
                 (const (type !xs!string) "non-nil"))))))
            (type (node-test))))
          (type
           (elem-test
            (ename
             (const (type !xs!QName) ("" "c"))
             (type *)
             (const (type !xs!string) "non-nil"))))))
        (type
         (elem-test
          (ename
           (const (type !xs!QName) ("" "d"))
           (type *)
           (const (type !xs!string) "non-nil"))))))
      (type
       (attr-test
        (ename
         (const (type !xs!QName) ("" "e"))
         (type *)
         (const (type !xs!string) "non-nil")))))))))

;(lropt:rewrite-query
; '(query
;   (prolog
;    (declare-function
;     (const
;      (type !xs!QName)
;      ("http://www.w3.org/2004/07/xquery-local-functions" "insert_elem"))
;     (((one (node-test)) (var ("" "e"))) ((zero-or-more (node-test)) (var ("" "s"))))
;     (result-type (zero-or-more (node-test)))
;     (body
;      (if@
;       (!fn!empty (var ("" "s")))
;       (var ("" "e"))
;       (if@
;        (le@
;         (ddo (child (var ("" "e")) (type (text-test))))
;         (ddo
;          (child
;           (predicate
;            (var ("" "s"))
;            (fun-def
;             ((!xs!anyType (var ("" "$%v"))))
;             (=@ (!fn!position) (const (type !xs!integer) "1"))))
;           (type (text-test)))))
;        (sequence (var ("" "e")) (var ("" "s")))
;        (sequence
;          (predicate
;           (var ("" "s"))
;           (fun-def
;            ((!xs!anyType (var ("" "$%v"))))
;            (=@ (!fn!position) (const (type !xs!integer) "1"))))
;          (fun-call
;           (const
;            (type !xs!QName)
;            ("http://www.w3.org/2004/07/xquery-local-functions" "insert_elem"))
;           (var ("" "e"))
;           (predicate
;            (var ("" "s"))
;            (fun-def
;             ((!xs!anyType (var ("" "$%v"))))
;             (>@ (!fn!position) (const (type !xs!integer) "1"))))))))))
;    (declare-function
;     (const
;      (type !xs!QName)
;      ("http://www.w3.org/2004/07/xquery-local-functions" "sort_by_name"))
;     (((zero-or-more (node-test)) (var ("" "s"))))
;     (result-type (zero-or-more (node-test)))
;     (body
;      (if@
;       (!fn!empty (var ("" "s")))
;       (var ("" "s"))
;       (fun-call
;        (const
;         (type !xs!QName)
;         ("http://www.w3.org/2004/07/xquery-local-functions" "insert_elem"))
;        (predicate
;         (var ("" "s"))
;         (fun-def
;          ((!xs!anyType (var ("" "$%v"))))
;          (=@ (!fn!position) (const (type !xs!integer) "1"))))
;        (fun-call
;         (const
;          (type !xs!QName)
;          ("http://www.w3.org/2004/07/xquery-local-functions" "sort_by_name"))
;         (predicate
;          (var ("" "s"))
;          (fun-def
;           ((!xs!anyType (var ("" "$%v"))))
;           (>@ (!fn!position) (const (type !xs!integer) "1"))))))))))
;   (query-body
;    (fun-call
;     (const
;      (type !xs!QName)
;      ("http://www.w3.org/2004/07/xquery-local-functions" "sort_by_name"))
;     (ddo
;      (child
;       (!fn!index-scan-between
;        (const (type !xs!string) "zips")
;        (const (type !xs!integer) "12")
;        (const (type !xs!integer) "25")
;        (const (type !xs!string) "SEG"))
;       (type
;        (elem-test
;         (ename
;          (const (type !xs!QName) ("" "name"))
;          (type *)
;          (const (type !xs!string) "non-nil"))))))))))

(lropt:rewrite-query
 '(query
   (prolog)
   (query-body
    (return
     (ddo
      (attr-axis
       (ddo
        (child
         (ddo
          (child
           (ddo
            (descendant-or-self
             (ddo
              (child
               (element (const (type !xs!QName) ("" "a")) (sequence))
               (type
                (elem-test
                 (ename
                  (const (type !xs!QName) ("" "b"))
                  (type *)
                  (const (type !xs!string) "non-nil"))))))
             (type (node-test))))
           (type
            (elem-test
             (ename
              (const (type !xs!QName) ("" "c"))
              (type *)
              (const (type !xs!string) "non-nil"))))))
         (type
          (elem-test
           (ename
            (const (type !xs!QName) ("" "d"))
            (type *)
            (const (type !xs!string) "non-nil"))))))
       (type
        (attr-test
         (ename
          (const (type !xs!QName) ("" "e"))
          (type *)
          (const (type !xs!string) "non-nil"))))))
     (fun-def
      ((xs:anyType (var ("" "x"))))
      (return
       (ddo
        (child
         (ddo
          (child
           (ddo
            (descendant-or-self
             (element (const (type !xs!QName) ("" "b")) (sequence))
             (type (node-test))))
           (type
            (elem-test
             (ename
              (const (type !xs!QName) ("" "f"))
              (type *)
              (const (type !xs!string) "non-nil"))))))
         (type
          (elem-test
           (ename
            (const (type !xs!QName) ("" "g"))
            (type *)
            (const (type !xs!string) "non-nil"))))))
       (fun-def
        ((xs:anyType (var ("" "y"))))
        (sequence
          (ddo
           (child
            (var ("" "x"))
            (type
             (elem-test
              (ename
               (const (type !xs!QName) ("" "h"))
               (type *)
               (const (type !xs!string) "non-nil"))))))
          (var ("" "y"))))))))))

(lropt:rewrite-query
 '(query
   (prolog)
   (query-body
    (ddo
     (return
      (ddo
       (child
        (element (const (type !xs!QName) ("" "a")) (sequence))
        (type
         (elem-test
          (ename
           (const (type !xs!QName) ("" "sources"))
           (type *)
           (const (type !xs!string) "non-nil"))))))
      (fun-def
       ((!xs!anyType (var ("" "$%v"))))
       (predicate
        (predicate
         (ddo
          (child
           (var ("" "$%v"))
           (type
            (elem-test
             (ename
              (const (type !xs!QName) ("" "source"))
              (type *)
              (const (type !xs!string) "non-nil"))))))
         (fun-def
          ((!xs!anyType (var ("" "$%v"))))
          (=@
           (ddo
            (child
             (var ("" "$%v"))
             (type
              (elem-test
               (ename
                (const (type !xs!QName) ("" "id"))
                (type *)
                (const (type !xs!string) "non-nil"))))))
           (const (type !xs!string) "sql2"))))
        (fun-def
         ((!xs!anyType (var ("" "$%v"))))
         (>@ (!fn!position) (+@ (!fn!last) (const (type !xs!integer) "6")))))))))))

(lropt:rewrite-query
 '(query
   (prolog)
   (query-body
    (let@
        (ddo
         (child
          (ddo
           (descendant-or-self
            (ddo
             (child
              (ddo
               (descendant-or-self
                (sequence
                  (element (const (type !xs!QName) ("" "a")) (sequence))
                  (element (const (type !xs!QName) ("" "b")) (sequence))
                  (element (const (type !xs!QName) ("" "c")) (sequence)))
                (type (node-test))))
              (type
               (elem-test
                (ename
                 (const (type !xs!QName) ("" "n"))
                 (type *)
                 (const (type !xs!string) "non-nil"))))))
            (type (node-test))))
          (type
           (elem-test
            (ename
             (const (type !xs!QName) ("" "m"))
             (type *)
             (const (type !xs!string) "non-nil"))))))
      (fun-def ((xs:anyType (var ("" "x")))) (var ("" "x")))))))

(lropt:rewrite-query
 '(query
   (prolog)
   (query-body
    (some
     (ddo
      (child
       (ddo
        (descendant-or-self
         (ddo
          (child
           (ddo
            (descendant-or-self
             (sequence
               (element (const (type !xs!QName) ("" "a")) (sequence))
               (element (const (type !xs!QName) ("" "b")) (sequence))
               (element (const (type !xs!QName) ("" "c")) (sequence)))
             (type (node-test))))
           (type
            (elem-test
             (ename
              (const (type !xs!QName) ("" "n"))
              (type *)
              (const (type !xs!string) "non-nil"))))))
         (type (node-test))))
       (type
        (elem-test
         (ename
          (const (type !xs!QName) ("" "m"))
          (type *)
          (const (type !xs!string) "non-nil"))))))
     (fun-def
      ((xs:anyType (var ("" "x"))))
      (some
       (ddo
        (child
         (sequence
           (element (const (type !xs!QName) ("" "e")) (sequence))
           (element (const (type !xs!QName) ("" "f")) (sequence)))
         (type
          (elem-test
           (ename
            (const (type !xs!QName) ("" "p"))
            (type *)
            (const (type !xs!string) "non-nil"))))))
       (fun-def
        ((xs:anyType (var ("" "y"))))
        (ddo
         (attr-axis
          (sequence
            (var ("" "x"))
            (element (const (type !xs!QName) ("" "d")) (sequence))
            (var ("" "y")))
          (type
           (attr-test
            (ename
             (const (type !xs!QName) ("" "attr"))
             (type *)
             (const (type !xs!string) "non-nil")))))))))))))
