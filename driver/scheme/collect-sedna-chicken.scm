; File:  collect-sedna-chicken.scm
; Copyright (C) 2004 ISP RAS
; The Institute for System Programming of the Russian Academy of Sciences

; To use it in Scheme code, just evaluate
;
;     (import sedna)
;
; after loading.

(module sedna
  (
   ; Connection with a server
   sedna:connection?
   sedna:connect-to-database
   sedna:disconnect-from-database
   
   ; Transactions
   sedna:begin-transaction
   sedna:end-transaction
   
   ; Query execution
   sedna:next
   sedna:result->list
   sedna:execute-query-xml
   sedna:execute-query
   
   ; Obtaining query result items as XML stream
   sedna:input-stream?
   sedna:read-line
   sedna:execute-query-xml-stream
   
   ; Wrapper for bulk load from stream
   sedna:bulk-load-from-xml-stream
   
   ; Transaction operators
   sedna:transaction
  )

(import scheme chicken)
(use tcp posix extras ports)

(define-syntax let-values*
  (syntax-rules ()
    ((_ bindings . body)
     (let*-values bindings . body))))

(define exc:signal signal)

; TCP library
(include "sedna-api/sedna-low.scm")

; Sedna API
(include "sedna-api/sedna-api.scm")

)
