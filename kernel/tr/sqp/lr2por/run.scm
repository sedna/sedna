
; File:  run.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

(require (lib "defmacro.ss"))
(require (lib "compat.ss"))
(require (lib "list.ss" "srfi" "1"))
(require (lib "errortrace.ss" "errortrace"))

(load "../stub.scm")
(load "../Tools/env/pp.scm")
;(load "../srfi-12.scm")
(define pp pretty-print)

;---------------------------------- Run lr2por
;for PLT
(load "../xquery-lr-lib.scm")

(load "../common-lib.scm")
(load "../../../common/error_codes_scm.scm")
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

(load "lr2por-lib.scm")
;(load "join-args-order-to-physical.scm") 
(load "lr2por.scm")
(load "por-cons.scm")

(define (cl:signal-error . msg)
  (let ((string-msg
         (apply
          string-append
          (map
           (lambda (single)
             (cond
               ((string? single) single)
               ((symbol? single) (symbol->string single))
               ((and (pair? single) (symbol? (car single)))
                (string-append "operation " (symbol->string (car single))))
               (else "")))
           msg))))
    (raise string-msg)))

;----------------------------------

(load "test/test-lr2por-max.scm")

(define go l2p:lr2por)

(l2p:lr2por
 '(manage
   (prolog)
   (create-fulltext-index
    (const (type !xs!string) "indexname")
    (ddo
     (child
      (!fn!document (const (type !xs!string) "reg"))
      (type (elem-test (ename (const (type !xs!QName) ("" "reg")) (type *) (const (type !xs!string) "non-nil"))))))
    (const (type !xs!string) "xml")
    (sequence
      (sequence (const (type !xs!string) "hui") (const (type !xs!string) "zhui"))
      (sequence (const (type !xs!string) "hui") (const (type !xs!string) "zhui"))))))

(porc:process-query
 (l2p:lr2por
  '(query
    (prolog)
    (query-body
     (ddo
      (return
       (ddo
        (child
         (ddo
          (child
           (!fn!document (const (type !xs!string) "s"))
           (type
            (elem-test
             (ename
              (const (type !xs!QName) ("" "site"))
              (type *)
              (const (type !xs!string) "non-nil"))))))
         (type
          (elem-test
           (ename
            (const (type !xs!QName) ("" "people"))
            (type *)
            (const (type !xs!string) "non-nil"))))))
       (fun-def
        ((!xs!anyType (var ("" "$%v"))))
        (predicate
         (child
          (var ("" "$%v"))
          (type
           (elem-test
            (ename
             (const (type !xs!QName) ("" "person"))
             (type *)
             (const (type !xs!string) "non-nil")))))
         (fun-def
          ((!xs!anyType (var ("" "$%v"))))
          (and@
           (=@
            (!fn!position)
            (element (const (type !xs!QName) ("" "a")) (sequence)))
           (le@ (!fn!last) (!fn!position))
           (return
            (var ("" "$%v"))
            (fun-def
             ((!xs!anyType (var ("" "$%v"))))
             (!fn!min (var ("" "$%v")) (!fn!position))))))))))))))

(porc:process-query
 (l2p:lr2por
  '(query
    (prolog)
    (query-body
     (return
      (const (type !xs!string) "test.xml")
      (fun-def
       ((!xs!anyType (var ("" "$%v"))))
       (ts
        (var ("" "$%v"))
        (cases
            (case (type (one !xs!string))
              (fun-def ((xs:anyType (var ("" "x"))))
                       (var ("" "x"))))
          (case (type (one (node-test)))
            (fun-def ((xs:anyType (var ("" "x"))))
                     (const (type !xs!string) "tesfdl")))
          (default
           (fun-def ((xs:anyType (var ("" "x"))))
                    (var ("" "$%v"))))))))))))
(l2p:lr2por
 '(query (prolog)
         (query-body
          (castable (const (type !xs!integer) 1) (type (optional !xs!string))))))

(porc:process-query
 (l2p:lr2por
  '(query
    (prolog (declare-default-order (const (type !xs!string) "empty-least")))
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
                  (and@
                   (<@ (var ("" "$x1")) (var ("" "$x2")))
                   (=@ (var ("" "$x3")) (var ("" "$x4"))))
                  (unio
                   (var ("" "$x1")) (var ("" "$x2")) (var ("" "$x3")) (var ("" "$x4")))
                  (sequence))))))))))
       (fun-def
        ((xs:anyType (var ("" "$x1")))
         (!xs!anyType (var ("" "$x2")))
         (xs:anyType (var ("" "$x3")))
         (xs:anyType (var ("" "$x4"))))
        (orderspecs
         (const (type !xs!string) "non-stable")
         (orderspec
          (ordermodifier (const (type !xs!string) "asc") (const (type !xs!string) "empty-greatest"))
          (sequence (var ("" "$x1")) (var ("" "$x2")) (var ("" "$x3")) (var ("" "$x4"))))
         (orderspec
          (ordermodifier (const (type !xs!string) "desc"))
          (sequence (var ("" "$x1")) (var ("" "$x2")) (var ("" "$x3")) (var ("" "$x4")))))))
      (fun-def
       ((xs:anyType (var ("" "$x1")))
        (!xs!anyType (var ("" "$x2")))
        (xs:anyType (var ("" "$x3")))
        (xs:anyType (var ("" "$x4"))))
       (sequence (var ("" "$x1")) (var ("" "$x2")) (var ("" "$x3")) (var ("" "$x4")))))))))


(porc:process-query
 (l2p:lr2por
  '(query
    (prolog)
    (query-body
     (return
      (order-by
       (return
        (ddo
         (child
          (ddo (descendant-or-self (!fn!document (const (type !xs!string) "a")) (type (node-test))))
          (type (elem-test (ename (const (type !xs!QName) ("" "book")) (type *) (const (type !xs!string) "non-nil"))))))
        (fun-def
         ((xs:anyType (var ("" "b"))))
         (return
          (ddo
           (child
            (ddo (descendant-or-self (!fn!document (const (type !xs!string) "a")) (type (node-test))))
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
      (fun-def ((xs:anyType (var ("" "b")))
                (xs:anyType (var ("" "a")))) (var ("" "b"))))))))

(porc:process-query
 (l2p:lr2por
  '(query
    (prolog)
    (query-body
     (return
      (sequence (const (type !xs!integer) 1) (const (type !xs!integer) 2))
      (fun-def
       ((xs:anyType (var ("" "x"))) (!se!positional-var (var ("" "n"))))
       (+@ (var ("" "x")) (const (type !xs!integer) 4))))))))

(porc:process-query
 (l2p:lr2por
  '(query
    (prolog)
    (query-body
     (return
      (sequence (const (type !xs!string) "Ford") (const (type !xs!string) "Chevy"))
      (fun-def
       ((xs:anyType (var ("" "car"))) (se:positional-var (var ("" "i"))))
       (return
        (sequence (const (type !xs!string) "Cat") (const (type !xs!string) "Dog"))
        (fun-def
         ((xs:anyType (var ("" "pet"))) (se:positional-var (var ("" "j"))))
         (sequence (var ("" "car")) (var ("" "pet")) (var ("" "i")) (var ("" "j")))))))))))

(porc:process-query
 (l2p:lr2por
  '(query
    (prolog)
    (query-body
     (element
      (const (type !xs!QName) ("" "tag"))
      (space-sequence
       (sequence
         (attribute
          (const (type !xs!string) "attr")
          (spaceseq
           (sequence
             (const (type !xs!integer) "4")
             (const (type !xs!integer) "5")
             (const (type !xs!integer) "6"))))
         (const (type !xs!integer) "7")
         (const (type !xs!integer) "8")
         (const (type !xs!integer) "9"))))))))

(porc:process-query
 (l2p:lr2por
  '(query
    (prolog)
    (query-body
     (return
      (order-by
       (lreturn
        (sequence
          (const (type !xs!integer) "1")
          (const (type !xs!integer) "2")
          (const (type !xs!integer) "3"))
        (fun-def
         (((one !xs!integer) (var ("" "i"))) (se:positional-var (var ("" "j"))))
         (lreturn
          (sequence (const (type !xs!integer) "8") (const (type !xs!integer) "10"))
          (fun-def
           ((xs:anyType (var ("" "u"))) (se:positional-var (var ("" "v"))))
           (unio (var ("" "i")) (var ("" "j")) (var ("" "u")) (var ("" "v")))))))
       (fun-def
        (((one !xs!integer) (var ("" "i")))
         (se:positional-var (var ("" "j")))
         (xs:anyType (var ("" "u")))
         (se:positional-var (var ("" "v"))))
        (orderspecs
         (const (type !xs!string) "non-stable")
         (orderspec (ordermodifier) (var ("" "j")))
         (orderspec (ordermodifier) (unary-@ (var ("" "v")))))))
      (fun-def
       (((one !xs!integer) (var ("" "i")))
        (se:positional-var (var ("" "j")))
        (xs:anyType (var ("" "u")))
        (se:positional-var (var ("" "v"))))
       (sequence (var ("" "i")) (var ("" "j")) (var ("" "u")) (var ("" "v")))))))))

(porc:process-query
 (l2p:lr2por
  '(query
    (prolog)
    (query-body
     (element
      (const (type !xs!string) "non-stable")
      (comment
       (sequence
         (const (type !xs!integer) "4")
         (const (type !xs!integer) "5")
         (const (type !xs!integer) "6"))))))))

(porc:process-query
 (l2p:lr2por
  '(query
    (prolog)
    (query-body
     (document
      (text
       (space-sequence
        (const (type !xs!string) "non-stable")
        (const (type !xs!integer) "4"))))))))

(go
 '(query
   (prolog)
   (query-body
    (return
     (order-by
      (let@
          (sequence
            (const (type !xs!integer) "1")
            (const (type !xs!integer) "2")
            (const (type !xs!integer) "3"))
        (fun-def
         ((xs:anyType (var ("" "a"))))
         (return
          (sequence
            (const (type !xs!integer) "4")
            (const (type !xs!integer) "6")
            (const (type !xs!integer) "5"))
          (fun-def
           ((xs:anyType (var ("" "last"))))
           (unio (var ("" "a")) (var ("" "last")))))))
      (fun-def
       ((xs:anyType (var ("" "a"))) (xs:anyType (var ("" "last"))))
       (orderspecs
        (const (type !xs!string) "non-stable")
        (orderspec (ordermodifier) (var ("" "last"))))))
     (fun-def
      ((xs:anyType (var ("" "a"))) (xs:anyType (var ("" "last"))))
      (element
       (const (type !xs!QName) ("" "res"))
       (sequence
         (element (const (type !xs!QName) ("" "a")) (space-sequence (var ("" "a"))))
         (element
          (const (type !xs!QName) ("" "last"))
          (space-sequence (var ("" "last")))))))))))

(porc:process-query
 (go
  '(query
    (prolog
     (declare-function
      (const (type !xs!QName) ("http://www.w3.org/2005/xquery-local-functions" "fn1"))
      (((one !xs!integer) (var ("" "n"))))
      (result-type (one !xs!integer))
      (body
       (fun-call
        (const (type !xs!QName) ("http://www.w3.org/2005/xquery-local-functions" "fn2"))
        (var ("" "n")))))
     (declare-function
      (const (type !xs!QName) ("http://www.w3.org/2005/xquery-local-functions" "fn2"))
      (((one !xs!integer) (var ("" "n"))))
      (result-type (one !xs!integer))
      (body
       (if@
        (=@ (var ("" "n")) (const (type !xs!integer) "1"))
        (const (type !xs!integer) "1")
        (+@
         (var ("" "n"))
         (fun-call
          (const
           (type !xs!QName)
           ("http://www.w3.org/2005/xquery-local-functions" "fn1"))
          (-@ (var ("" "n")) (const (type !xs!integer) "1"))))))))
    (query-body
     (fun-call
      (const (type !xs!QName) ("http://www.w3.org/2005/xquery-local-functions" "fn1"))
      (const (type !xs!integer) "4"))))))
