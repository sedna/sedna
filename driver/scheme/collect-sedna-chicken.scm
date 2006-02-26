
; File:  collect-sedna-chicken.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

; General
(load "libs/chicken/common.scm")
(load "libs/chicken/myenv.scm")

; Chicken natively supports SRFI-12
;(load "libs/srfi-12.scm")
(define exc:signal signal)

; TCP library
(load "sedna-api/sedna-low.scm")

; Sedna API
(load "sedna-api/sedna-api.scm")
