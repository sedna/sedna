
; File:  impl-differences.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

(declare (unit impl-differences) (uses extras))
;(display "impl-differences") (newline)

;This module contains functions 
;implementation and interface of which are different in different impl of scheme
;The namespace prefix of this module is "id:" that stands for impl differences

;for PLT
(define (id:sort less-then-pred l)
  (sort less-then-pred l))

;for Chicken
;(define (id:sort less-then-pred l)
;  (sort l less-then-pred))

;(display "impl-differences") (newline)