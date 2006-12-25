(require (lib "defmacro.ss"))
(require (rename (lib "pretty.ss") pp pretty-print))
(define-macro (declare . x) #t)
(define-macro (cl:signal-input-error code . msg)
  `(begin
     (display "Input error ")
     (display ,(symbol->string code))
     (display ": ")
     (for-each display (list ,@msg))
     ;(/ 1 0)
     #f))
(define-macro (cl:signal-user-error code . msg)
  `(begin
     (display "User error ")
     (display (if (symbol? (quote ,code))
                  (symbol->string (quote ,code))
                  (quote ,code)))
     (display ": ")
     (for-each write (list ,@msg))
     ;(/ 1 0)
     #f))

(load "sa.scm")

(define go sa:analyze-query)

;------------------------

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

;(sa:analyze-query
; '(query
;   (prolog)
;   (query-body
;    (fun-call (const (type !xs!QName) ("xs" "NOTATION"))
;              (const (type !xs!string) "a")))))

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
   (prolog)
   (query-body
    (return
     (sequence (const (type !xs!integer) "1") (const (type !xs!integer) "2"))
     (fun-def (((one ("xs" "int")) (var ("" "x")))) (var ("" "x")))))))

(go
   '(manage
  (prolog)
  (create-trigger
    (const (type !xs!string) "trig")
    (const (type !xs!string) "AFTER")
    (const (type !xs!string) "REPLACE")
    (ddo
     (child
      (fun-call (const (type !xs!QName) ("" "doc")) (const (type !xs!string) "a.xml"))
      (type (elem-test (ename (const (type !xs!QName) ("" "people")) (type *) (const (type !xs!string) "non-nil"))))))
    (const (type !xs!string) "STATEMENT")
    ((delete
       (ddo
        (child
         (ddo
          (child
           (fun-call (const (type !xs!QName) ("" "doc")) (const (type !xs!string) "w.xml"))
           (type (elem-test (ename (const (type !xs!QName) ("" "site")) (type *) (const (type !xs!string) "non-nil"))))))
         (type (elem-test (ename (const (type !xs!QName) ("" "people")) (type *) (const (type !xs!string) "non-nil")))))))
     (ddo
      (child
       (ddo
        (child
         (fun-call (const (type !xs!QName) ("" "doc")) (const (type !xs!string) "a.xml"))
         (type (elem-test (ename (const (type !xs!QName) ("" "site")) (type *) (const (type !xs!string) "non-nil"))))))
       (type (elem-test (ename (const (type !xs!QName) ("" "people")) (type *) (const (type !xs!string) "non-nil"))))))))))

(go
 '(query
   (prolog
    (declare-global-var
     (var ("" "var"))
     (fun-call (const (type !xs!QName) ("local" "func1")))
     (one !xs!integer))
    (declare-function
     (const (type !xs!QName) ("local" "func1"))
     ()
     (result-type (zero-or-more (item-test)))
     (body (fun-call (const (type !xs!QName) ("local" "func2")))))
    (declare-function
     (const (type !xs!QName) ("local" "func2"))
     ()
     (result-type (zero-or-more (item-test)))
     (body (fun-call (const (type !xs!QName) ("local" "func3")))))
    (declare-global-var
     (var ("" "var2"))
     (fun-call (const (type !xs!QName) ("local" "func2"))))
    (declare-function
     (const (type !xs!QName) ("local" "func3"))
     ()
     (result-type (zero-or-more (item-test)))
     (body (var ("" "var2")))))
   (query-body (fun-call (const (type !xs!QName) ("" "boolean")) (var ("" "var"))))))

(go
 '(query
   (prolog
    (version-declaration
     (const (type !xs!string) "1.0")
     (const (type !xs!string) "iso-latin-1 ")))
   (query-body
    (return
     (sequence (const (type !xs!integer) "1") (const (type !xs!integer) "2"))
     (fun-def (((one ("xs" "int")) (var ("" "x")))) (var ("" "x")))))))

(go
 '(query
   (prolog)
   (query-body
    (return
     (element
      (const (type !xs!QName) ("" "parent2"))
      (sequence
        (namespace
         (const (type !xs!NCName) "foo")
         (const (type !xs!string) "http://www.example.com/parent2"))
        (attribute
         (const (type !xs!QName) ("foo" "attr2"))
         (const (type !xs!string) "attr2"))
        (element
         (const (type !xs!QName) ("" "child2"))
         (sequence
           (attribute
            (const (type !xs!QName) ("" "attr"))
            (const (type !xs!string) "child"))))))
     (fun-def
      ((xs:anyType (var ("" "x"))))
      (element
       (const (type !xs!QName) ("" "new"))
       (sequence
         (namespace
          (const (type !xs!NCName) "")
          (const (type !xs!string) "http://www.example.com"))
         (space-sequence
          (ddo
           (child
            (ddo (descendant-or-self (var ("" "x")) (type (node-test))))
            (type
             (elem-test
              (ename
               (const (type !xs!QName) (* "child2"))
               (type *)
               (const (type !xs!string) "non-nil"))))))))))))))

(sa:analyze-module
   '(lib-module
  (module-decl
    (const (type !xs!NCName) math)
    (const (type !xs!string) "http://example.org/math-functions"))
  (prolog
    (declare-function
      (const (type !xs!QName) ("local" "f"))
      ()
      (result-type (zero-or-more (item-test)))
      (body (const (type !xs!string) "petya"))))))
