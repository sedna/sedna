
; File:  dschema.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)


(declare (unit dschema) (uses common-lib))

; Unit incapsulates functions, which obtain descriptive schemas from C-part of Sedna
; Namespace prefix is "dscm:". It stands for descriptive schema

; Functions of this library take the following parameters
; name - name of the entity (string)
; type - type of the entity (can be 'document or 'collection)


;-------------------------------------------------------------------------------
; Function returns descriptive schema for entity with name 'name' and type 'type'
; If descriptive schema for this entity was not previously queried then the function
; obtain it from C-part. Otherwise it takes the corresponding descriptive schema
; from list of queried descriptive schemas
(define (dscm:get-dschema name type)
  (define (find-dschema lst)
    (cond ((null? lst) lst)
          ((and (string=? name (caar lst)) (eq? type (cadar lst))) (caddar lst))
          (else (find-dschema (cdr lst)))))
  (let ((res (find-dschema dscm:queried-dschema-list)))
    (if (null? res)
        (let ((res (dscm:query-dschema name type)))
          (set! dscm:queried-dschema-list (cons (list name type res) dscm:queried-dschema-list))
          res)
        res)))

;-------------------------------------------------------------------------------
; Query descriptive schema from C-part
(define (dscm:query-dschema name type)
  (cl:string->scheme-list 
   (__c__-descriptive-schema->scheme-list 
    name 
    (if (eq? type 'collection) 1 0))))

;-------------------------------------------------------------------------------
; Function incapsulates original C-function
(declare (foreign-declare "extern char* __c__descriptive_schema_to_scheme_list(const char* name, int is_collection);"))
(define __c__-descriptive-schema->scheme-list 
  (foreign-callback-lambda c-string* "__c__descriptive_schema_to_scheme_list" c-string int))

;-------------------------------------------------------------------------------
; List of already queried descriptive schemas
(define dscm:queried-dschema-list '())


