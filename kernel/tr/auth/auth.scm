
; File:  auth.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

(declare (unit auth) (uses prec-qs xquery-lr common-lib))


(define (sc:auth-query-rewriting expr user is-auth)
  ; if e  - empty list then return empty list
   (if (eq? expr '())
         '()
         (begin
           (write expr)
         (if (eq? is-auth 0)
             (if (memq (caaddr expr) '(create-user drop-user alter-user create-role drop-role grant-role grant-priv-on-doc grant-priv-on-col grant-priv revoke-priv revoke-priv-from-doc revoke-priv-from-col revoke-role))
                 (cl:signal-input-error SE3068 "Compile Sedna with AUTH_SWITH=1 if you want to turn this feature on")
                 (list expr))
             (cond
               ( (eq? (car expr) 'retrieve-metadata)   
                       (begin
;                         (write 'retrieve-metadata)(newline)
                         (cond
                           ( (or (eq? (caadr expr) 'retrieve-metadata-documents) (eq? (caadr expr) 'retrieve-metadata-collections))
                             (list
                                (sc:q2 user "RETRIEVE-METADATA" "User doesn't have privilege RETRIEVE-METADATA")
                                expr))
                           (else 
                               (list expr))
                           ))
                       )
               ( (eq? (car expr) 'manage)   
                       (begin
;                       (write 'manage)(newline)
                       (cond
                       ( (eq? (caaddr expr) 'load) 
                            (cond
                              ( (eq? (length (caddr expr)) 4)                   ;load doc into collection
                                                (begin
;                                                 (write 'loadcol)
                                                 (list
                                                  (sc:q4 user 
                                                         "LOAD"
                                                         (caddr (cdaddr expr)) 
                                                         `collection 
                                                         "User doesn't have privilege LOAD on the collection, or collection does not exist.")                                                        
                                                   expr) )                               
                               )
                              ( (eq? (length (caddr expr)) 3)                   ;load doc                              
                                                 (begin
;                                                 (write 'loaddoc)
;                                                 (write (sc:q17 user (cadr(cdaddr expr)) `document))
                                                 (list
                                                 (sc:q2 user "LOAD" "User doesn't have privilege LOAD.")
                                                 
                                                 (sc:q17 user (cadr(cdaddr expr)) `document)
                                                 expr)
                                                  ))
                              ))
 
                       ( (eq? (caaddr expr) 'create-document) 
                         (cond
                           ( (eq? (length (caddr expr)) 3)
                                                 (begin
;                                                 (write 'create-doc-in-col)
                                                 (list
                                                  (sc:q4 user "CREATE-DOCUMENT" (cadr(cdaddr expr)) `collection "User doesn't have privilege to create documents in the collection, or collection does not exists.")                                                        
                                                   expr) )                               
                               )
                           ( (eq? (length (caddr expr)) 2)
                                                 (begin
;                                                 (write 'create-document)
;                                                 (write (caddar(cdadr expr)))
                                                 (list
                                                  (sc:q2 user "CREATE-DOCUMENT" "User doesn't have privilege CREATE-DOCUMENT.")
                                                  (sc:q17 user (car(cdaddr expr)) `document)
                                                  expr
                                                  ))
                              )
                         ))
                       ( (eq? (caaddr expr) 'create-collection) (begin
 ;                                                (write 'create-collection)
                                                 (list
                                                  (sc:q2 user "CREATE-COLLECTION" "User doesn't have privilege CREATE-COLLECTION.")
                                                  (sc:q17 user (car(cdaddr expr)) `collection)
                                                  expr
                                                  ) ))
                       ( (eq? (caaddr expr) 'create-index) (begin
                                                 (list
                                                  (sc:q4 user 
                                                         "CREATE-INDEX" 
                                                         (cadr (find_doc_col_name (cadr (cdaddr expr)))) 
                                                         (car (find_doc_col_name (cadr (cdaddr expr)))) 
                                                         "User doesn't have privilege CREATE-INDEX.")
                                                  (sc:q17 user (car(cdaddr expr)) `index)
                                                  expr
                                                  ) ))
                       ( (eq? (caaddr expr) 'create-trigger) (begin
                                                 (list
                                                  (sc:q2 user "CREATE-TRIGGER" "User doesn't have privilege CREATE-TRIGGER.")
                                                  (sc:q17 user (car(cdaddr expr)) `trigger)
                                                  expr
                                                  )))
                       ( (eq? (caaddr expr) 'drop-trigger) (begin
                                                 (list
                                                  (sc:q4 user "DROP" (car(cdaddr expr)) `trigger 
                                                         "User doesn't have privilege DROP on the trigger or the trigger does not exist.")              
                                                  (sc:q5 (car(cdaddr expr)) `trigger)
                                                  expr
                                                  ) ))                       
                       ( (eq? (caaddr expr) 'drop-document)
                         (cond
                           ( (eq? (length (caddr expr)) 3)                           
                                                 (list
                                                  (sc:q4 user "DROP" (cadr(cdaddr expr)) `collection "User doesn't have privilege to drop documents in the collection, or collection does not exist.")                                                        
                                                   expr)                               
                               )
                           ( (eq? (length (caddr expr)) 2)
                             (begin
                                                   (list
                                                  (sc:q4 user "DROP" (car(cdaddr expr)) `document
                                                         (string-append "User doesn't have privilege DROP on this document or the document does not exist. Document: " (caddr(car(cdaddr expr)))) )              
                                                  (sc:q5 (car(cdaddr expr)) `document)
                                                  expr
                                                  ) )
                              )
                         ))
                       ( (eq? (caaddr expr) 'drop-collection) (begin
;                                                 (write 'drop-collection)  
                                                 (list
                                                  (sc:q4 user "DROP" (car(cdaddr expr)) `collection
                                                         "User doesn't have privilege DROP on this collection or the collection does not exist.")              
                                                  (sc:q5 (car(cdaddr expr)) `collection)
                                                  expr
                                                  ) )) 
                       ( (eq? (caaddr expr) 'drop-index) (begin
                                                 (list
                                                  (sc:q4 user "DROP" (car(cdaddr expr)) `index 
                                                         "User doesn't have privilege DROP on the index or the index does not exist.")              
                                                  (sc:q5 (car(cdaddr expr)) `index)
                                                  expr
                                                  ) ))
                       ( (eq? (caaddr expr) 'load-module)
                                                (list
                                                 (sc:q2 user "LOAD-MODULE" "User doesn't have privilege LOAD-MODULE.")
                                   ;              (sc:q17 user (car(cdaddr expr)) `module)
                                                 expr)
                                                )
                       ( (eq? (caaddr expr) 'load-or-replace-module)
                                                (list
                                                 (sc:q2 user "LOAD-MODULE" "User doesn't have privilege LOAD-MODULE")
                                                 (sc:q4_1 user "DROP" (cadr(caddr expr)) `module
                                                          "User doesn't have privilege DROP on the module.")
                                                 (sc:q5 (cadr(caddr expr)) `module)
                                                 expr)
                                                  )
                       ( (eq? (caaddr expr) 'drop-module) 
                                                (list
                                                 (sc:q4 user "DROP" (cadr(caddr expr)) `module 
                                                         "User doesn't have privilege DROP on the module or the index does not exist.")
                                                 (sc:q5 (cadr(caddr expr)) `module)
                                                 expr)
                                                  )
                       ( (eq? (caaddr expr) 'create-user) (begin
 ;                                                (write 'create-user)
                                                 (list
                                                  (sc:q2 user "CREATE-USER" "User doesn't have privilege CREATE-USER.")
                                                  (sc:q27 (caddar(cdaddr expr)))  ; newuser
                                                  (sc:q18 user                   ; user
                                                          (caddar(cdaddr expr))   ; newuser
                                                          (car(cddadr(cdaddr expr)))) ; password
                                                  (sc:gag-query)
                                                  )
                                                  ) )     
                       ( (eq? (caaddr expr) 'drop-user) (begin
  ;                                               (write 'drop-user)
                                                 (list
                                                  (sc:q22 user (caddar(cdaddr expr)))   ; user drop_user
                                                  (sc:q23 (caddar(cdaddr expr)))  ; drop_user
                                                  (sc:gag-query)
                                                  )
                                                  ) )     
                       ( (eq? (caaddr expr) 'alter-user) (begin
 ;                                                (write 'alter-user)
                                                 (list
                                                  (sc:q19 user                     ; user  
                                                          (caddar(cdaddr expr)))    ; alter_user
                                                  (sc:q20 (caddar(cdaddr expr)))    ; alter_user
                                                  (sc:q21 (caddar(cdaddr expr))     ;alter_user
                                                          (car(cddadr(cdaddr expr))))   ;new_password
                                                  (sc:gag-query)
                                                  )
                                                  ) )
                       ( (eq? (caaddr expr) 'create-role) (begin
;                                                 (write 'create-role)
                                                 (list
                                                  (sc:q28 (caddar(cdaddr expr)))     ;role-name
                                                  (sc:q6 user (caddar(cdaddr expr))) ; user-id, role-name
                                                  (sc:gag-query)
                                                  )
                                                 ) )
                       ( (eq? (caaddr expr) 'drop-role) (begin
;                                                 (write 'drop-role)
                                                 (list
                                                 (sc:q8 user (caddar(cdaddr expr))) ;user-id,  role-name
                                                 (sc:q7 (caddar(cdaddr expr))); role-name
                                                 (sc:gag-query)
                                                 )  
                                                 ))
                       ( (eq? (caaddr expr) 'grant-role) (begin
                                                 (if (eq? (length (car(cdaddr expr))) 1)
                                                     (if (eq? (length (cadr(cdaddr expr))) 1)
                                                         (list
                                                          (sc:q9 user (car(cddaar(cdaddr expr))))   ;grantor,  role-name
                                                          (sc:q10 user                  ; grantor
                                                                (car(cddaar(cdaddr expr)))    ; role-name
                                                                (cadr(cdaadr(cdaddr expr))))  ;grantee
                                                          (sc:gag-query)
                                                                )
                                                         (cl:signal-input-error SE3070)
                                                         )
                                                     (cl:signal-input-error SE3070))
                                                 ))      
                       ( (eq? (caaddr expr) 'grant-priv-on-doc) (begin
;                                                 (write 'grant-priv-on-doc)
                                                 (if (eq? (length (car(cdaddr expr))) 1)
                                                     (if (eq? (length (caddr(cdaddr expr))) 1)
                                                         (list
                                                          (sc:q4 user                          ;grantor
                                                                 "OWNER"                       ;privilege
                                                                 (cadr(cdaddr expr))           ;doc
                                                                 `document                     ;type-obj
                                                                 "User is not allowed to grant the privilege."
                                                                 )
                                                          (sc:q11 user                         ; grantor
                                                                  (car(cddaar(cdaddr expr)))         ;privilege
                                                                  (car(cddadr(cdaddr expr)))       ;doc
                                                                  `document
                                                                  (caddr(caaddr(cdaddr expr)))    ;grantee
                                                                  )
                                                          (sc:gag-query))
                                                         (cl:signal-input-error SE3070)
                                                         )
                                                     (cl:signal-input-error SE3070))
                                                 ))    
                       ( (eq? (caaddr expr) 'grant-priv-on-col) (begin
;                                                 (write 'grant-priv-on-col)
                                                 (if (eq? (length (car(cdaddr expr))) 1)
                                                     (if (eq? (length (caddr(cdaddr expr))) 1)
                                                         (list
                                                          (sc:q4 user                          ;grantor
                                                                 "OWNER"                       ;privilege
                                                                 (cadr(cdaddr expr))           ;doc
                                                                 `collection                   ;type-obj
                                                                 "User is not allowed to grant the privilege."
                                                                 )       
                                                          (sc:q11 user                         ; grantor
                                                                  (car(cddaar(cdaddr expr)))         ;privilege
                                                                  (car(cddadr(cdaddr expr)))       ;doc
                                                                  `collection
                                                                  (caddr(caaddr(cdaddr expr))    ;grantee
                                                                       ))
                                                          (sc:gag-query))
                                                         (cl:signal-input-error SE3070)
                                                         )
                                                     (cl:signal-input-error SE3070))
                                                 ))                           
                       ( (eq? (caaddr expr) 'grant-priv) (begin
 ;                                                (write 'grant-priv)
                                                 (if (eq? (length (car(cdaddr expr))) 1)
                                                     (if (eq? (length (cadr(cdaddr expr))) 1)
                                                     (list
                                                      (sc:q24 user)                       ; grantor
                                                      (sc:q25 user                        ; grantor
                                                              (cadr(cdaar(cdaddr expr)))      ;privilege
                                                              (caddr(caadr(cdaddr expr)))       ;grantee        
                                                              )
                                                      (sc:gag-query))
                                                     (cl:signal-input-error SE3070))
                                                     (cl:signal-input-error SE3070)
                                                     )                                                     
                                                 ))                       
                       ( (eq? (caaddr expr) 'revoke-priv) (begin
 ;                                                (write 'revoke-priv)
                                                 (if (eq? (length (car(cdaddr expr))) 1)
                                                     (if (eq? (length (cadr(cdaddr expr))) 1)
                                                         (list
                                                          (sc:q24 user)                         ;grantor
                                                          (sc:q26 user                         ;grantor
                                                                  (cadr(cdaar(cdaddr expr)))        ;privilege
                                                                  (caddr(caadr(cdaddr expr))) ;grantee
                                                                  )
                                                          (sc:gag-query))
                                                         (cl:signal-input-error SE3070))
                                                     (cl:signal-input-error SE3070)
                                                     )
                                                 ))      
                       ( (eq? (caaddr expr) 'revoke-priv-from-doc) (begin
 ;                                                (write 'revoke-priv-from-doc)
                                                  (if (eq? (length (car(cdaddr expr))) 1)
                                                     (if (eq? (length (caddr(cdaddr expr))) 1)
                                                         (list
                                                          (sc:q13 user                         ;grantor
                                                                  (car(cddaar(cdaddr expr)))        ;privilege
                                                                  (car(cddadr(cdaddr expr)))       ;doc
                                                                  `document
                                                                  (caddar(caddr(cdaddr expr)))) ;grantee
                                                          (sc:q14 user                            ;grantor
                                                                  (car(cddaar(cdaddr expr)))        ;privilege
                                                                  (cadr(cdadr(cdaddr expr)))       ;doc
                                                                  `document
                                                                  (caddr(caaddr(cdaddr expr)))) ;grantee
                                                          (sc:gag-query))
                                                         (cl:signal-input-error SE3070))
                                                     (cl:signal-input-error SE3070)
                                                 )
                                                 ))  
                       ( (eq? (caaddr expr) 'revoke-priv-from-col) (begin
 ;                                                (write 'revoke-priv-from-col)
                                                  (if (eq? (length (car(cdaddr expr))) 1)
                                                     (if (eq? (length (caddr(cdaddr expr))) 1)
                                                         (list
                                                          (sc:q13 user                            ;grantor
                                                                  (car(cddaar(cdaddr expr)))        ;privilege
                                                                  (car(cddadr(cdaddr expr)))       ;col
                                                                  `collection
                                                                  (caddr(caaddr(cdaddr expr)))) ;grantee
                                                          (sc:q14 user                            ;grantor
                                                                  (car(cddaar(cdaddr expr)))        ;privilege
                                                                  (cadr(cdadr(cdaddr expr)))       ;doc
                                                                  `collection
                                                                  (caddr(caaddr(cdaddr expr)))) ;grantee
                                                          (sc:gag-query))
                                                         (cl:signal-input-error SE3070))
                                                     (cl:signal-input-error SE3070)
                                                     )
                                                 ))                        
                       ( (eq? (caaddr expr) 'revoke-role) (begin
 ;                                                (write 'revoke-role)
                                                  (if (eq? (length (car(cdaddr expr))) 1)
                                                     (if (eq? (length (cadr(cdaddr expr))) 1)
                                                         (list
                                                          (sc:q15 user                           ;grantor
                                                                  (car(cddaar(cdaddr expr)))        ;role-name
                                                                  (cadr(cdaadr(cdaddr expr))))     ;grantee
                                                          (sc:q16 user                           ;grantor
                                                                  (car(cddaar(cdaddr expr)))        ;role-name
                                                                  (cadr(cdaadr(cdaddr expr))))      ;grantee
                                                          (sc:gag-query))
                                                         (cl:signal-input-error SE3070))
                                                    (cl:signal-input-error SE3070) 
                                                 )     
                                                 ))
                       (else (begin
;                               (write 'else)
                               (list expr)
                               ))
                       )) )
          (else 
           (list expr)
           ))
        )))
    )

;(define (sc:parametrize user privilege expr)
;  (if (eq? expr '())
;  '()
;  (begin
;  (map (lambda (x) 
;       (if (list? x)
;          (if (eq? (car x) '!fn!document) 
;              (sc:build-in-auth-checking user privilege (cadr x))  
;              (sc:parametrize user privilege x))
;          x)
;         )
;       expr))
;  ) )
;(define (sc:build-in-auth-checking user privilege database-obj-expr)  
;   (begin
;   (sc:q1 user 
;          privilege 
;          (sc:parametrize user privilege database-obj-expr) 
;          (string-append "User "
;                         " doesn't have privilege " privilege
;                         ", or the object does not exist." 
;                         ))
;   )
; )

(define (sc:gag-query)
  `(update (prolog) (delete (sequence)))
 )

(define (find_doc_col_name expr)  
  (if (eq? expr '() )
      '()
      (if (eq? (car expr) '!fn!document)
          (list 'document (cadr expr))
          (if (eq? (car expr) '!fn!collection)
              (list 'collection (cadr expr))
              (find_doc_col_name (cadr expr)))
           )
      ))
