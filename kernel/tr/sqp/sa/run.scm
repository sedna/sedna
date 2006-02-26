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
     (for-each display (list ,@msg))
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