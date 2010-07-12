
; File:  client.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

; Load the necessary Sedna API files
(load "collect-sedna-plt.scm")  ; uncomment this line for PLT
;(load "collect-sedna-chicken.scm")   ; uncomment this line for Chicken
;(##include "collect-sedna-gambit.scm")  ; uncomment this line for Gambit


; Create a connection
(define conn
  (sedna:connect-to-database "localhost" "sample-db" "SYSTEM" "MANAGER"))

; Begin a transaction
(sedna:begin-transaction conn)

; Bulk load
(call/cc
 (lambda (k)
   (with-exception-handler  ; Exception handler
    (lambda (x)
      (display "File already loaded to the database")
      (newline)
      (k (sedna:begin-transaction conn)))
    (lambda ()
      (sedna:execute-query conn "LOAD '../data/region.xml' 'regions'")))))

; Execute a statement and represent it as an SXML nodeset
(pp
 (sedna:result->list
  (sedna:execute-query conn "doc('regions')/*/*")))

; Update statement
(pp
 (sedna:execute-query conn "UPDATE delete doc('regions')//africa"))

; Querying all regions once again
(pp
 (sedna:result->list
  (sedna:execute-query conn "doc('regions')/*/*")))

; Commit transaction
(sedna:end-transaction conn 'COMMIT)

; Close the connection
(sedna:disconnect-from-database conn)
