changequote(%, %)

(declare (unit scm-debug))


ifelse(EL_DEBUG,1,
%


(declare (foreign-declare "extern void scm_error(int code, char* message, char* component);"))
(define scm-error (foreign-callback-lambda void "scm_error" int c-string c-string))
(declare (foreign-declare "extern void scm_debug(int code, char* message, char* component);"))
(define scm-debug (foreign-callback-lambda void "scm_debug" int c-string c-string))


%,EL_DEBUG,0,
%


(declare (foreign-declare "extern void scm_error(int code, char* message, char* component);"))
(define scm-error (foreign-callback-lambda void "scm_error" int c-string c-string))
(define (scm-debug n msg comp) '())


%,
%


(define (scm-error n msg comp) '())
(define (scm-debug n msg comp) '())


%)