
; File:  gen_error_codes.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

(declare (uses extras))

#>
// only for MSDEV 6.0
#if (_MSC_VER == 1200) && (WINVER < 0x0500)
long _ftol( double ); //defined by VC6 C libs
long _ftol2( double dblSource ) { return _ftol(dblSource ); } 
#endif
<#

(define (except-last-character str)
; for PLT
;  (if (> (string-length str) 0)
;      (substring str 0 (- (string-length str) 1))
;      str)
  
; for Chicken
  str
  )



(define port (current-output-port))
(define output-to-file #t)

(define (out s)
  (display s port))

(define (nl) (newline port))

(define input-port (open-input-file "errdbg/error.codes"))

(define (pick-out-code code)
  (define (f lst)
    (if (eq? (car lst) #\:)
        (cdr lst)
        (f (cdr lst))))
  (list->string (f (string->list code))))

(define (empty-string? str)
  (define (f lst)
    (cond ((null? lst) #t)
          ((or (eq? (car lst) #\space) (eq? (car lst) #\tab)) (f (cdr lst)))
          (else #f)))
  (f (string->list (except-last-character str))))

(define (comment? str)
  (eq? (car (string->list str)) #\#))

(define (read-record)
  (define (find-record)
    (let ((str (read-line input-port)))
      (cond ((eof-object? str) '())
            ((or (empty-string? str) (comment? str)) (find-record))
            (else str))))
  (define (certainly-read-record code)
    (if (not (null? code))
        (let ((param (read-line input-port))
              (descr (read-line input-port)))
          (if (not (eof-object? descr))
              (list (except-last-character code)
                    (except-last-character param)
                    (except-last-character descr))
              '()))
        '()))
  (certainly-read-record (find-record)))

(define (quote-string str)
  (define (quote-quotes lst)
    (if (null? lst) 
        '()
        (let ((fc (car lst))
              (lc (cdr lst)))
          (cond ((eq? fc #\") (cons #\\ (cons #\" (quote-quotes lc))))
                (else (cons fc (quote-quotes lc)))))))
  (define (trim lst)
    (if (null? lst)
        '()
        (let ((fc (car lst))
              (lc (cdr lst)))
          (cond ((eq? fc #\space) (trim lc))
                (else lst)))))
  (list->string (quote-quotes (trim (string->list str)))))

(define (process-header group-id)
  (let ((record (read-record)))
    (if (not (null? record))
        (let ((code  (car record))
              (param (cadr record))
              (descr (caddr record)))
          (out "#define ")
          (out (pick-out-code code))
          (out "           ")
          (out group-id)
          (out "  // ")
          (out (quote-string descr))
          (nl)
          (process-header (+ group-id 1))))))


; GENERATE HEADER FILE
;
(if output-to-file (set! port (open-output-file "error_codes.h")))
(out "#ifndef _USER_ERROR_CODES_H")
(nl)
(out "#define _USER_ERROR_CODES_H")
(nl)
(nl)
(out "// This file was generated. Don't edit it!!!")(nl)
(nl)
(out "#ifdef __cplusplus")(nl)
(out "extern \"C\" {")(nl)
(out "#endif")(nl)
(nl)
(out "enum user_error_code_act {ueca_NOTHING, ueca_ROLLBACK_TRN};")(nl)
(nl)
(out "struct user_error_code_entry")(nl)
(out "{")(nl)
(out "    const char* code;               /* error code */")(nl)
(out "    enum user_error_code_act act;   /* reaction on error */")(nl)
(out "    const char* descr;              /* error decrtiption */")(nl)
(out "};")(nl)
(nl)
(out "extern struct user_error_code_entry user_error_code_entries[];")(nl)
(out "extern const int user_error_code_entries_size;")(nl)
(nl)
(out "#ifdef __cplusplus")(nl)
(out "}")(nl)
(out "#endif")(nl)
(nl)
(process-header 0)
(nl)
(nl)
(out "#endif")
(nl)
(if output-to-file (close-output-port port))

(close-input-port input-port)

;;########################################################################################
;;########################################################################################
;;########################################################################################

(define input-port (open-input-file "errdbg/error.codes"))

(define (rollback? p) #t)

(define (process-body group-id)
  (let ((record (read-record)))
    (if (not (null? record))
        (let ((code  (car record))
              (param (cadr record))
              (descr (caddr record)))
          (if (not (eq? group-id 0)) (out ",") (out " "))
          (out "{\"")
          (out (pick-out-code code))
          (out "\", ")
          (cond ((rollback? param) (out "ueca_ROLLBACK_TRN"))
                (else (out "ueca_NOTHING")))
          (out ", \"")
          (out (quote-string descr))
          (out "\"}")
          (nl)
          (process-body (+ group-id 1))))))


;
; GENERATE SOURCE FILE
;
(if output-to-file (set! port (open-output-file "error_codes.c")))
(nl)
(out "// This file was generated. Don't edit it!!!")
(nl)
(nl)
(out "#include \"error_codes.h\"")
(nl)
(nl)
(out "struct user_error_code_entry user_error_code_entries[] = {")
(nl)
(process-body 0)
(nl)
(out "};")
(nl)
(nl)
(out "const int user_error_code_entries_size = sizeof user_error_code_entries;")
(nl)
(if output-to-file (close-output-port port))

(close-input-port input-port)

;;########################################################################################
;;########################################################################################
;;########################################################################################

(define input-port (open-input-file "errdbg/error.codes"))

(define (process-scm group-id)
  (let ((record (read-record)))
    (if (not (null? record))
        (let ((code  (car record))
              (param (cadr record))
              (descr (caddr record)))
          (out "(define ")
          (out (pick-out-code code))
          (out " ")
          (out group-id)
          (out ")  ;; ")
          (out (quote-string descr))
          (nl)
          (process-scm (+ group-id 1))))))


;
; GENERATE SCHEMA FILE
;
(if output-to-file (set! port (open-output-file "error_codes_scm.scm")))
(nl)
(out ";; This file was generated. Don't edit it!!!")
(nl)
(out "(declare (unit scm-error-codes))")
(nl)
(process-scm 0)
(nl)
(if output-to-file (close-output-port port))

(close-input-port input-port)

















