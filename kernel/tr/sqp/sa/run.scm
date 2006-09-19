(require (lib "defmacro.ss"))
(define-macro (declare . x) #t)
(define-macro (cl:signal-input-error code . msg)
  `(begin
     (display "Input error ")
     (display ,(symbol->string code))
     (display ": ")
     (for-each display (list ,@msg))
     #f))
(define-macro (cl:signal-user-error code . msg)
  `(begin
     (display "User error ")
     (display ,(symbol->string code))
     (display ": ")
     (for-each write (list ,@msg))     
     #f))

(load "sa.scm")

;------------------------

(define query
  '(query
 (prolog (declare-namespace n (const (type !xs!string) "http://vodka.com")))
 (query-body
   (let@
    (const (type !xs!integer) 6)
    (fun-def
      ((xs:anyType (var ("" "x"))))
      (+@
       (+@ (+@ (const (type !xs!integer) 5) (var ("" "x"))) (var ("n" "y")))
       (var ("" "z"))))))))

(sa:analyze-query query)

(sa:analyze-query
 '(manage (prolog) (drop-index (const (type !xs!QName )("" "pet" )))))

(sa:analyze-query
 '(manage (prolog)
   (grant-priv-on-doc
           (const (type !xs!string ) "LOAD" )
           (const (type !xs!string ) "blya" )
           (const (type !xs!string ) "ass" ))))

(sa:analyze-query
 '(manage (prolog)
   (create-document (let@ (const (type !xs!string) "pt") (fun-def ((xs:anyType (var ("" "x")))) (var ("" "x")))))))

(sa:analyze-query
   '(manage (prolog)
            (create-collection (let@ (const (type !xs!string) "col") (fun-def ((xs:anyType (var ("" "x")))) (var ("" "x")))))))

(sa:analyze-query
 '(manage (prolog) (grant-priv (
                       (const (type !xs!string) "LOAD")
                       )
                      ((const (type !xs!string) "user")
                       (const (type !xs!string) "vasya")))))

(sa:analyze-query
 '(query
   (prolog)
   (query-body
    (fun-call (const (type !xs!QName) ("xs" "NOTATION"))
              (const (type !xs!string) "a")))))

(sa:analyze-query
 '(manage
   (prolog)
   (load
    (const (type !xs!string) "test.xml")
    (const (type !xs!string) "regions2"))))

(sa:analyze-query
   '(retrieve-metadata (prolog) (retrieve-descr-scheme (const (type !xs!string) "auc2")))
)

(sa:analyze-query
 '(query
   (prolog)
   (query-body
    (ts
     (const (type !xs!string) "test.xml")
     (cases
         (case (type !xs!string)
           (fun-def ((xs:anyType (var ("" "x"))))
                    (var ("" "x"))))
       (default
        (fun-def ((xs:anyType (var ("" "x"))))
                 (var ("" "x")))))))))

(sa:analyze-query
 '(query (prolog)
         (query-body (castable (const (type !xs!integer) 1) (type (optional !xs!string))))))
(sa:analyze-query
 '(query (prolog)
         (query-body (treat (const (type !xs!integer) 1) (type (one !xs!integer))))))

(sa:analyze-query
 '(query
   (prolog
    (declare-default-order (const (type !xs!string) "empty-least")))
   (query-body
    (return
     (order-by
      (return
       (const (type !xs!integer) 13)
       (fun-def
        ((xs:anyType (var ("" "$x1"))))
        (return
         (var ("" "$x1"))
         (fun-def
          ((xs:anyType (var ("" "$x2"))))
          (let@
              (+@ (var ("" "$x1")) (var ("" "$x2")))
            (fun-def
             ((xs:anyType (var ("" "$x3"))))
             (let@
                 (sequence (var ("" "$x1")) (var ("" "$x2")) (var ("" "$x3")))
               (fun-def
                ((xs:anyType (var ("" "$x4"))))
                (if@
                 (and@ (<@ (var ("" "$x1")) (var ("" "$x2")))
                     (=@ (var ("" "$x3")) (var ("" "$x4"))))
                 (unio (<@ (var ("" "$x1")) (var ("" "$x2")))
                       (=@ (var ("" "$x3")) (var ("" "$x4"))))
                 (sequence))))))))))
      (fun-def
       ((xs:anyType (var ("" "$x1"))) (!xs!anyType (var ("" "$x2")))
        (xs:anyType (var ("" "$x3"))) (xs:anyType (var ("" "$x4"))))
       (orderspecs
        (const (type !xs!string) "non-stable")
        (orderspec
         (ordermodifier 
          (const (type !xs!string) "asc")
          (const (type !xs!string) "empty-greatest"))
         (sequence (var ("" "$x1")) (var ("" "$x2"))
           (var ("" "$x3")) (var ("" "$x4"))))
        (orderspec
         (ordermodifier 
          (const (type !xs!string) "desc")
          ;(const (type !xs!string) "empty-least")
          )
         (sequence (var ("" "$x1")) (var ("" "$x2"))
           (var ("" "$x3")) (var ("" "$x4")))))))
     (fun-def
      ((xs:anyType (var ("" "$x1"))) (!xs!anyType (var ("" "$x2")))
       (xs:anyType (var ("" "$x3"))) (xs:anyType (var ("" "$x4"))))
      (sequence (var ("" "$x1")) (var ("" "$x2"))
           (var ("" "$x3")) (var ("" "$x4"))))))))

(sa:analyze-query
 '(query
   (prolog)
   (query-body
    (return
     (order-by
      (return
       (ddo
        (child
         (ddo
          (descendant-or-self
           (fun-call (const (type !xs!QName) ("" "document")) (const (type !xs!string) "a"))
           (type (node-test))))
         (type (elem-test (ename (const (type !xs!QName) ("" "book")) (type *) (const (type !xs!string) "non-nil"))))))
       (fun-def
        ((xs:anyType (var ("" "b"))))
        (return
         (ddo
          (child
           (ddo
            (descendant-or-self
             (fun-call (const (type !xs!QName) ("" "document")) (const (type !xs!string) "a"))
             (type (node-test))))
           (type
            (elem-test (ename (const (type !xs!QName) ("" "author")) (type *) (const (type !xs!string) "non-nil"))))))
         (fun-def ((xs:anyType (var ("" "a")))) (unio (var ("" "b")) (var ("" "a")))))))
      (fun-def
       ((xs:anyType (var ("" "b"))) (xs:anyType (var ("" "a"))))
       (orderspecs
        (const (type !xs!string) "non-stable")
        (orderspec
         (ordermodifier)
         (ddo
          (child
           (var ("" "b"))
           (type
            (elem-test (ename (const (type !xs!QName) ("" "title")) (type *) (const (type !xs!string) "non-nil")))))))
        (orderspec
         (ordermodifier (const (type !xs!string) "desc") (const (type !xs!string) "empty-least"))
         (ddo
          (child
           (var ("" "a"))
           (type
            (elem-test (ename (const (type !xs!QName) ("" "price")) (type *) (const (type !xs!string) "non-nil")))))))
        (orderspec
         (ordermodifier (const (type !xs!string) "desc"))
         (ddo
          (child
           (var ("" "b"))
           (type
            (elem-test
             (ename (const (type !xs!QName) ("" "salary")) (type *) (const (type !xs!string) "non-nil"))))))))))
     (fun-def ((xs:anyType (var ("" "b"))) (xs:anyType (var ("" "a")))) (var ("" "b")))))))

(sa:analyze-query
 '(query
   (prolog)
   (query-body
    (return
     (sequence (const (type !xs!integer) 1) (const (type !xs!integer) 2))
     (fun-def
      ((xs:anyType (var ("" "x")))
       (!se!positional-var (var ("" "n"))))
      (+@ (var ("" "x")) (const (type !xs!integer) 4)))))))

(sa:analyze-query
 '(query
   (prolog)
   (query-body
    (return
     (sequence (const (type !xs!integer) "6") (const (type !xs!integer) "7"))
     (fun-def
      ((xs:anyType (var ("" "i"))) (se:positional-var (var ("" "i"))))
      (if@
       (>@ (var ("" "i")) (const (type !xs!decimal) "6.5"))
       (var ("" "i"))
       (sequence)))))))
