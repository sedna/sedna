
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

