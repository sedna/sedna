(require (lib "defmacro.ss"))
(require (rename (lib "pretty.ss") pp pretty-print))
(define-macro (declare . x) #t)

(require (lib "list.ss" "srfi" "1"))
(load "../common-lib.scm")
(load "../xquery-lr-lib.scm")
(load "lreturn.scm")

(define go lropt:rewrite-query)

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

(lropt:rewrite-query
 '(query
   (prolog
    (declare-function
     (const (type !xs!QName) ("http://www.w3.org/2004/07/xpath-functions" "all_after"))
     (((one (node-test)) (var ("" "n"))) ((zero-or-more (node-test)) (var ("" "seq"))))
     (result-type (zero-or-more (node-test)))
     (body
      (if@
       (!fn!empty (var ("" "seq")))
       (var ("" "seq"))
       (if@
        (is@
         (var ("" "n"))
         (predicate
          (var ("" "seq"))
          (fun-def
           ((!xs!anyType (var ("" "$%v"))))
           (eq@ (!fn!position) (const (type !xs!integer) "1")))))
        (predicate
         (var ("" "seq"))
         (fun-def
          ((!xs!anyType (var ("" "$%v"))))
          (>@ (!fn!position) (const (type !xs!integer) "1"))))
        (fun-call
         (const
          (type !xs!QName)
          ("http://www.w3.org/2004/07/xpath-functions" "all_after"))
         (var ("" "n"))
         (predicate
          (var ("" "seq"))
          (fun-def
           ((!xs!anyType (var ("" "$%v"))))
           (>@ (!fn!position) (const (type !xs!integer) "1")))))))))
    (declare-function
     (const (type !xs!QName) ("http://www.w3.org/2004/07/xpath-functions" "foll_sibl"))
     (((zero-or-more (node-test)) (var ("" "nset"))))
     (result-type (zero-or-more (node-test)))
     (body
      (if@
       (!fn!empty (var ("" "nset")))
       (var ("" "nset"))
       (if@
        (eq@ (!fn!count (var ("" "nset"))) (const (type !xs!integer) "1"))
        (fun-call
         (const
          (type !xs!QName)
          ("http://www.w3.org/2004/07/xpath-functions" "all_after"))
         (var ("" "nset"))
         (ddo
          (child
           (ddo (parent (var ("" "nset")) (type (node-test))))
           (type (node-test)))))
        (return
         (var ("" "nset"))
         (fun-def
          ((xs:anyType (var ("" "m"))))
          (fun-call
           (const
            (type !xs!QName)
            ("http://www.w3.org/2004/07/xpath-functions" "foll_sibl"))
           (var ("" "m"))))))))))
   (query-body
    (fun-call
     (const (type !xs!QName) ("http://www.w3.org/2004/07/xpath-functions" "foll_sibl"))
     (ddo
      (child
       (ddo
        (descendant-or-self
         (!fn!document (const (type !xs!string) "regions"))
         (type (node-test))))
       (type
        (elem-test
         (ename
          (const (type !xs!QName) ("" "australia"))
          (type *)
          (const (type !xs!string) "non-nil"))))))))))

(lropt:rewrite-query
 '(query
   (prolog)
   (query-body
    (ts
     (ddo
      (child
       (ddo
        (descendant-or-self
         (ddo
          (child
           (ddo
            (descendant-or-self
             (element (const (type !xs!QName) ("" "x")) (sequence))
             (type (node-test))))
           (type
            (elem-test
             (ename
              (const (type !xs!QName) ("" "a"))
              (type *)
              (const (type !xs!string) "non-nil"))))))
         (type (node-test))))
       (type
        (elem-test
         (ename
          (const (type !xs!QName) ("" "b"))
          (type *)
          (const (type !xs!string) "non-nil"))))))
     (cases
         (case (type (zero-or-more (node-test)))
           (fun-def ((!xs!anytype (var ("" "n")))) (var ("" "n"))))
       (default
        (fun-def
         ((!xs!anytype (var ("" "%v"))))
         (element (const (type !xs!QName) ("" "y")) (sequence)))))))))

(go
 '(query
 (prolog
   (declare-function
     (const
      (type !xs!QName)
      ("http://www.w3.org/2005/xquery-local-functions" "get-ancestor-articles"))
     (((one (node-test)) (var ("" "a"))) ((one !xs!string) (var ("" "query"))))
     (result-type (zero-or-more (node-test)))
     (body
      (return
        (ddo
         (ancestor
           (var ("" "a"))
           (type
            (elem-test
              (ename
               (const (type !xs!QName) ("" "article"))
               (type *)
               (const (type !xs!string) "non-nil"))))))
        (fun-def
          ((xs:anyType (var ("" "anc"))))
          (element
            (const (type !xs!QName) ("" "ancestor"))
            (sequence
              (attribute
                (const (type !xs!QName) ("" "id"))
                (space-sequence
                  (ddo
                   (attr-axis
                     (var ("" "anc"))
                     (type
                      (attr-test
                        (ename
                         (const (type !xs!QName) ("" "id"))
                         (type *)
                         (const (type !xs!string) "non-nil"))))))))
              (attribute
                (const (type !xs!QName) ("" "title"))
                (space-sequence
                  (ddo
                   (child
                    (ddo
                     (child
                      (var ("" "anc"))
                      (type
                       (elem-test
                         (ename
                          (const (type !xs!QName) ("" "meta"))
                          (type *)
                          (const (type !xs!string) "non-nil"))))))
                    (type
                     (elem-test
                       (ename
                        (const (type !xs!QName) ("" "title"))
                        (type *)
                        (const (type !xs!string) "non-nil"))))))))))))))
   (declare-function
     (const (type !xs!QName) ("http://www.w3.org/2005/xquery-local-functions" "TO"))
     (((one !xs!integer) (var ("" "s"))) ((one !xs!integer) (var ("" "f"))))
     (result-type (zero-or-more !xs!integer))
     (body
      (if@
       (>=@ (var ("" "s")) (var ("" "f")))
       (sequence)
       (sequence
         (var ("" "s"))
         (fun-call
           (const
            (type !xs!QName)
            ("http://www.w3.org/2005/xquery-local-functions" "TO"))
           (+@ (var ("" "s")) (const (type !xs!integer) "1"))
           (var ("" "f"))))))))
 (query-body
   (let@
    (const (type !xs!string) "")
    (fun-def
      ((xs:anyType (var ("" "query"))))
      (let@
       (var ("" "query"))
       (fun-def
         ((xs:anyType (var ("" "query-search-unstemming"))))
         (let@
          (var ("" "query"))
          (fun-def
            ((xs:anyType (var ("" "query-search"))))
            (let@
             (const (type !xs!integer) "1")
             (fun-def
               ((xs:anyType (var ("" "is-bre"))))
               (let@
                (const (type !xs!integer) "0")
                (fun-def
                  ((xs:anyType (var ("" "is-bse"))))
                  (let@
                   (const (type !xs!integer) "1")
                   (fun-def
                     ((xs:anyType (var ("" "user-id"))))
                     (let@
                      (const (type !xs!integer) "10")
                      (fun-def
                        ((xs:anyType (var ("" "portion-size"))))
                        (let@
                         (cast (const (type !xs!string) "1") (type (one !xs!integer)))
                         (fun-def
                           ((xs:anyType (var ("" "cur-page"))))
                           (let@
                            (+@
                             (*@
                              (-@ (var ("" "cur-page")) (const (type !xs!integer) "1"))
                              (var ("" "portion-size")))
                             (const (type !xs!integer) "1"))
                            (fun-def
                              ((xs:anyType (var ("" "s"))))
                              (let@
                               (*@ (var ("" "cur-page")) (var ("" "portion-size")))
                               (fun-def
                                 ((xs:anyType (var ("" "f"))))
                                 (let@
                                  (ddo
                                   (child
                                    (!fn!document (const (type !xs!string) "alphabet"))
                                    (type
                                     (elem-test
                                       (ename
                                        (const (type !xs!QName) ("" "ltrs"))
                                        (type *)
                                        (const (type !xs!string) "non-nil"))))))
                                  (fun-def
                                    ((xs:anyType (var ("" "letters"))))
                                    (let@
                                     (!fn!filter_entry_level
                                       (ddo
                                        (return
                                          (!fn!ftindex-scan
                                            (const (type !xs!string) "fti-body")
                                            (var ("" "query-search")))
                                          (fun-def
                                            ((!xs!anyType (var ("" "$%v"))))
                                            (predicate
                                              (parent
                                                (var ("" "$%v"))
                                                (type (node-test)))
                                              (fun-def
                                                ((!xs!anyType (var ("" "$%v"))))
                                                (=@
                                                 (ddo
                                                  (attr-axis
                                                    (ddo
                                                     (ancestor
                                                       (var ("" "$%v"))
                                                       (type
                                                        (elem-test
                                                          (ename
                                                           (const
                                                            (type !xs!QName)
                                                            ("" "tome"))
                                                           (type *)
                                                           (const
                                                            (type !xs!string)
                                                            "non-nil"))))))
                                                    (type
                                                     (attr-test
                                                       (ename
                                                        (const
                                                         (type !xs!QName)
                                                         ("" "id"))
                                                        (type *)
                                                        (const
                                                         (type !xs!string)
                                                         "non-nil"))))))
                                                 (sequence
                                                   (const (type !xs!string) "0")
                                                   (const (type !xs!string) "1")
                                                   (const (type !xs!string) "2")
                                                   (const (type !xs!string) "3")
                                                   (const (type !xs!string) "4")
                                                   (const (type !xs!string) "5")
                                                   (const (type !xs!string) "6")))))))))
                                     (fun-def
                                       ((xs:anyType (var ("" "body-xml-res"))))
                                       (let@
                                        (var ("" "body-xml-res"))
                                        (fun-def
                                          ((xs:anyType (var ("" "xml-res"))))
                                          (let@
                                           (if@
                                            (>@
                                             (!fn!count (var ("" "xml-res")))
                                             (var ("" "portion-size")))
                                            (+@
                                             (var ("" "cur-page"))
                                             (const (type !xs!integer) "1"))
                                            (var ("" "cur-page")))
                                           (fun-def
                                             ((xs:anyType (var ("" "pages-num"))))
                                             (element
                                               (const (type !xs!QName) ("" "top"))
                                               (element
                                                 (const
                                                  (type !xs!QName)
                                                  ("" "sedna-result"))
                                                 (element
                                                   (const (type !xs!QName) ("" "items"))
                                                   (space-sequence
                                                     (return
                                                       (predicate
                                                         (var ("" "xml-res"))
                                                         (fun-def
                                                           ((!xs!anyType
                                                              (var ("" "$%v"))))
                                                           (<=@
                                                            (!fn!position)
                                                            (var ("" "portion-size")))))
                                                       (fun-def
                                                         ((xs:anyType (var ("" "a"))))
                                                         (element
                                                           (const
                                                            (type !xs!QName)
                                                            ("" "item"))
                                                           (sequence
                                                             (attribute
                                                               (const
                                                                (type !xs!QName)
                                                                ("" "tomeid"))
                                                               (space-sequence
                                                                 (ddo
                                                                  (attr-axis
                                                                    (ddo
                                                                     (ancestor
                                                                       (var ("" "a"))
                                                                       (type
                                                                        (elem-test
                                                                          (ename
                                                                           (const
                                                                            (type
                                                                             !xs!QName)
                                                                            ("" "tome"))
                                                                           (type *)
                                                                           (const
                                                                            (type
                                                                             !xs!string)
                                                                            "non-nil"))))))
                                                                    (type
                                                                     (attr-test
                                                                       (ename
                                                                        (const
                                                                         (type
                                                                          !xs!QName)
                                                                         ("" "id"))
                                                                        (type *)
                                                                        (const
                                                                         (type
                                                                          !xs!string)
                                                                         "non-nil"))))))))
                                                             (element
                                                               (const
                                                                (type !xs!QName)
                                                                ("" "ancestors"))
                                                               (space-sequence
                                                                 (fun-call
                                                                   (const
                                                                    (type !xs!QName)
                                                                    ("http://www.w3.org/2005/xquery-local-functions"
                                                                     "get-ancestor-articles"))
                                                                   (var ("" "a"))
                                                                   (var
                                                                    (""
                                                                     "query-search")))))
                                                             (element
                                                               (const
                                                                (type !xs!QName)
                                                                ("" "result"))
                                                               (sequence
                                                                 (attribute
                                                                   (const
                                                                    (type !xs!QName)
                                                                    ("" "id"))
                                                                   (space-sequence
                                                                     (ddo
                                                                      (attr-axis
                                                                        (var ("" "a"))
                                                                        (type
                                                                         (attr-test
                                                                           (ename
                                                                            (const
                                                                             (type
                                                                              !xs!QName)
                                                                             ("" "id"))
                                                                            (type *)
                                                                            (const
                                                                             (type
                                                                              !xs!string)
                                                                             "non-nil"))))))))
                                                                 (attribute
                                                                   (const
                                                                    (type !xs!QName)
                                                                    ("" "title"))
                                                                   (space-sequence
                                                                     (ddo
                                                                      (child
                                                                       (ddo
                                                                        (child
                                                                         (ddo
                                                                          (child
                                                                           (var
                                                                            ("" "a"))
                                                                           (type
                                                                            (elem-test
                                                                              (ename
                                                                               (const
                                                                                (type
                                                                                 !xs!QName)
                                                                                (""
                                                                                 "meta"))
                                                                               (type *)
                                                                               (const
                                                                                (type
                                                                                 !xs!string)
                                                                                "non-nil"))))))
                                                                         (type
                                                                          (elem-test
                                                                            (ename
                                                                             (const
                                                                              (type
                                                                               !xs!QName)
                                                                              (""
                                                                               "title"))
                                                                             (type *)
                                                                             (const
                                                                              (type
                                                                               !xs!string)
                                                                              "non-nil"))))))
                                                                       (type
                                                                        (text-test))))))
                                                                 (element
                                                                   (const
                                                                    (type !xs!QName)
                                                                    ("" "highlight"))
                                                                   (const
                                                                    (type !xs!string)
                                                                    "hl")))))))))))))))))))))))))))))))))))))))))))

(mlr:rewrite-module
 '(lib-module
   (module-decl
    (const (type !xs!NCName) math)
    (const (type !xs!string) "http://example.org/math-functions"))
   (prolog
    (declare-function
     (const (type !xs!QName) ("http://www.w3.org/2005/xquery-local-functions" "f"))
     ()
     (result-type (zero-or-more (item-test)))
     (body (const (type !xs!string) "petya"))))))

(go
 '(query
   (module
    (declare-namespace foo (const (type !xs!string) "http://xy.com"))
    (declare-function
      (const (type !xs!QName) ("http://xy.com" "fact"))
      (((one !xs!integer) (var ("" "n"))))
      (result-type (zero-or-more (item-test)))
      (body (fun-call (const (type !xs!QName) ("http://xy.c" "fact")) (var ("" "n")))))
     (declare-global-var
      (var ("http://xy.com" "pi"))
      (var ("http://xy.c" "pi"))
      (zero-or-more (item-test))))
   (module
    (declare-namespace foo (const (type !xs!string) "http://xy.c"))
    (declare-function
      (const (type !xs!QName) ("http://xy.c" "fact"))
      (((one !xs!integer) (var ("" "n"))))
      (result-type (zero-or-more (item-test)))
      (body (fun-call (const (type !xs!QName) ("http://xy" "fact")) (var ("" "n")))))
     (declare-global-var
      (var ("http://xy.c" "pi"))
      (var ("http://xy" "pi"))
      (zero-or-more (item-test))))
   (module
    (declare-namespace foo (const (type !xs!string) "http://xy"))
    (declare-function
      (const (type !xs!QName) ("http://xy" "fact"))
      (((one !xs!integer) (var ("" "n"))))
      (result-type (zero-or-more (item-test)))
      (body (fun-call (const (type !xs!QName) ("http://" "fact")) (var ("" "n")))))
    (declare-global-var
      (var ("http://xy" "pi"))
      (var ("http://" "pi"))
      (zero-or-more (item-test))))
   (module
    (declare-namespace foo (const (type !xs!string) "http://"))
    (declare-function
      (const (type !xs!QName) ("http://" "fact"))
      (((one !xs!integer) (var ("" "n"))))
      (result-type (zero-or-more (item-test)))
      (body (fun-call (const (type !xs!QName) ("http:" "fact")) (var ("" "n")))))
     (declare-global-var
      (var ("http://" "pi"))
      (var ("http:" "pi"))
      (zero-or-more (item-test))))
   (module (declare-namespace foo (const (type !xs!string) "http:")) (declare-function
      (const (type !xs!QName) ("http:" "fact"))
      (((one !xs!integer) (var ("" "n"))))
      (result-type (zero-or-more (item-test)))
      (body
       (if@
        (eq@ (var ("" "n")) (const (type !xs!integer) "0"))
        (const (type !xs!integer) "1")
        (*@
         (var ("" "n"))
         (fun-call
           (const (type !xs!QName) ("http:" "fact"))
           (-@ (var ("" "n")) (const (type !xs!integer) "1")))))))
     (declare-global-var
      (var ("http:" "pi"))
      (const (type !xs!decimal) "3.14")
      (zero-or-more (item-test))))
   (prolog)
   (query-body
     (*@
      (var ("http://xy.com" "pi"))
      (fun-call
        (const (type !xs!QName) ("http://xy.com" "fact"))
        (const (type !xs!integer) "5"))))))