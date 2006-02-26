;************************************************************************
; common.scm
; This file is the part of SSAX package (http://ssax.sourceforge.net),
; which is in public domain.

(define (command-line)
  (vector->list argv))

(define (call-with-input-string str fun)
  (fun (open-input-string str)))

(define pp pretty-print)
