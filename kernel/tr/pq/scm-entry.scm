
; File:  scm-entry.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)


(declare (uses lr2por por-cons sa lreturn auth common-lib popt))

(case-sensitive #t)

;---------------------------------------------------------------------
; stubs for globals parameters
(declare (foreign-declare "extern int is_run_rewriter();"))
(define is-run-rewriter (foreign-safe-lambda int "is_run_rewriter"))
(declare (foreign-declare "extern int is_print_intermed();"))
(define is-print-intermed (foreign-safe-lambda int "is_print_intermed"))
(declare (foreign-declare "extern int is_server_mode();"))
(define is-server-mode (foreign-safe-lambda int "is_server_mode"))
(declare (foreign-declare "extern char* get_user_login();"))
(define user (foreign-safe-lambda c-string "get_user_login"))
(declare (foreign-declare "extern int is_auth();"))
(define is-auth (foreign-safe-lambda int "is_auth"))
(declare (foreign-declare "extern int is_run_popt();"))
(define is-run-popt (foreign-safe-lambda int "is_run_popt"))

(declare (foreign-declare "extern char* get_scm_input_string();"))
(define get-scm-input-string (foreign-safe-lambda c-string* "get_scm_input_string"))
(declare (foreign-declare "extern void set_scm_output_string(char* s);"))
(define set-scm-output-string (foreign-safe-lambda void "set_scm_output_string" c-string))



;---------------------------------------------------------------------
; entry function for scheme part of the kernel
(define (process-query-in-scheme step-id)
  (let* ((lr-query        (get-scm-input-string))
         (query           (let ((tmp `(#t ,(cl:string->scheme-list lr-query))))
                            (if (eq? 1 (is-print-intermed)) 
                                (cl:write-to-file tmp "intermed0_initial.scm"))
                            tmp))
         
         (query           (if (>= step-id 8)
                              (if (not (car query))
                                  query
                                  (let* ((tmp (if (eq? (car query) #t)
                                                  (handle-exceptions ex
                                                                     `(#f ,(cl:get-exception-message ex))
                                                                     `(#t ,(sa:analyze-query (cadr query))))
                                                  query)))
                                    (if (eq? 1 (is-print-intermed)) 
                                        (cl:write-to-file tmp "intermed1_analyzed.scm"))
                                    tmp))
                              query))
         
         (query           (if (>= step-id 7)
                              ;                              (if (eq? (is-auth) 0)
                              ;                                  query
                              (let* ((tmp (if (eq? (car query) #t)
                                              (handle-exceptions ex
                                                                 `(#f ,(cl:get-exception-message ex))
                                                                 `(#t ,@(sc:auth-query-rewriting 
                                                                         (cadr query) (user) (is-auth))))
                                              query)))
                                (if (eq? 1 (is-print-intermed))
                                    (cl:write-to-file tmp "intermed2_auth.scm"))
                                
                                tmp)
                              query))         
         
         
         ;         (query           (if (>= step-id 6)
         ;                              (let ((tmp (if (eq? 1 (is-run-rewriter))
         ;                                             (logical-optimizer query)
         ;                                             (logical-optimizer-rewrite-off query))))
         ;                                (if (eq? 1 (is-print-intermed))
         ;                                    (cl:write-to-file tmp "intermed1_rewr.scm"))
         ;                                tmp)
         ;                              query))
         ;         (query           (if (>= step-id 5)
         ;                              (let ((tmp (if (eq? 1 (is-run-normalizing-rewriter))
         ;                                             (norm-logical-optimizer query)
         ;                                             (norm-logical-optimizer-norm-rewrite-off query))))
         ;                                (if (eq? 1 (is-print-intermed))
         ;                                    (cl:write-to-file tmp "intermed2_norm_rewr.scm"))
         ;                                tmp)
         ;                              query))
         ; DL: uncommented lreturn
         (query           (if (and (>= step-id 4) (eq? (is-run-rewriter) 1))
                              (let ((tmp (if (eq? (car query) #t)
                                             (handle-exceptions ex
                                                                `(#f ,(cl:get-exception-message ex))
                                                                `(#t ,@(map 
                                                                        (lambda (q) (mlr:rewrite-query q))
                                                                        (cdr query))))
                                             query)))
                                (if (eq? 1 (is-print-intermed))
                                    (cl:write-to-file tmp "intermed3_mark_lreturn.scm"))
                                tmp)
                              query))
         
         (query           (if (and (>= step-id 3) (eq? (is-run-popt) 1))
                              (let ((tmp (if (eq? (car query) #t)
                                             (handle-exceptions ex
                                                                `(#f ,(cl:get-exception-message ex))
                                                                `(#t ,@(map 
                                                                        (lambda (q) (popt:optimize-query q))
                                                                        (cdr query))))
                                             query)))
                                (if (eq? 1 (is-print-intermed))
                                    (cl:write-to-file tmp "intermed4_popt.scm"))
                                tmp)
                              query))
         (query           (if (>= step-id 2)  
                              (let* ((tmp (if (eq? (car query) #t)
                                              (handle-exceptions ex
                                                                 `(#f ,(cl:get-exception-message ex))
                                                                 `(#t ,@(map
                                                                         (lambda (q) (porc:process-query (l2p:lr2por q)))
                                                                         (cdr query))))
                                              query)))
                                (if (eq? 1 (is-print-intermed))
                                    (cl:write-to-file tmp "intermed5_physical_plan.scm"))
                                (if (eq? (car tmp) #f) tmp
                                    `(#t ,@(cdr tmp))))
                              query))
         )
;           (cl:scheme-list->string query)
;           (display "!!!!!!!!!!!!!!!!!!!!!!")
;           (newline)
;           (display (cl:scheme-list->string query))
;           (newline)
;           (display "!!!!!!!!!!!!!!!!!!!!!!")
;           (newline)
    
    (set-scm-output-string (cl:scheme-list->string query))
    '()
    ))


(return-to-host)
