
; File:  sedna-api.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

;; SXML/Scheme API for Sedna

; Prefix for this module is sedna:

;==========================================================================
; Low-level operations

; Several constants for char, since Bigloo doesn't support sedna:char000  and such
(define sedna:char000 (integer->char 0))
(define sedna:char001 (integer->char 1))

; Raises an exception
(define (sedna:raise-exn . msg)
  (exc:signal
   (make-property-condition 'exn
                            'message (sedna:apply-string-append msg))))

;-------------------------------------------------
; Basic read and write operations on port

; Reads n characters from the input-port
(define (sedna:read-n-chars n input-port)
  (cond
    ((= n 0)  ; nothing to read
     '())
    ((eof-object? (peek-char input-port))
     (sedna:raise-exn "sedna:read-n-chars: Unexpected end of input port")
     #f)
    (else
     (cons (read-char input-port)
           (sedna:read-n-chars (- n 1) input-port)))))

; Reads n or less characters from the input-port
(define (sedna:read-n-or-less n input-port)
  (if
   (or (eof-object? (peek-char input-port)) (= n 0))
   '()       
   (cons (read-char input-port)
         (sedna:read-n-or-less (- n 1) input-port))))

; Returns the first n members of the list
(define (sedna:first-n n lst)
  (cond
    ((= n 0)  ; nothing to read
     '())
    ((null? lst)
     (sedna:raise-exn "sedna:first-n: Unexpected end of the list")
     #f)
    (else
     (cons (car lst)
           (sedna:first-n (- n 1) (cdr lst))))))

;-------------------------------------------------
; Convertions between the integer and its 4-byte representation in the
; Network byte order

; Converts an integer number to a list of 4 characters
(define (sedna:integer->chars num)
  (let* ((frst (quotient num 16777216))
         (num (- num (* frst 16777216)))
         (scnd (quotient num 65536))
         (num (- num (* scnd 65536)))
         (thrd (quotient num 256))
         (frth (- num (* thrd 256))))
    (list (integer->char frst)
          (integer->char scnd)
          (integer->char thrd)
          (integer->char frth))))

(define (sedna:chars->integer char-lst)
  (+ (* (char->integer (car char-lst)) 16777216)
     (* (char->integer (cadr char-lst)) 65536)
     (* (char->integer (caddr char-lst)) 256)
     (char->integer (cadddr char-lst))))

;-------------------------------------------------
; Conversion between a string and its protocol network representation

; Converts a string to its network representation
; Returns: (listof char)
(define (sedna:string->network str)
  (cons
   sedna:char000
   (append
    (sedna:integer->chars (string-length str))
    (string->list str))))

; Converts a list of chars to their network representation
; Returns: (listof char)
(define (sedna:chars->network chars)
  (cons
   sedna:char000
   (append
    (sedna:integer->chars (length chars))
    chars)))

; Extracts the network string from the list of chars
; Returns: (values string remaining-chars)
(define (sedna:extract-string chars)
  (if
   (< (length chars) 5)  ; at least 5 chars for format and length
   (begin
     (sedna:raise-exn "sedna:extract-string: No string found")
     #f)
   (let ((lng 
          (sedna:chars->integer
           (sedna:first-n 4 (cdr chars))))
         (rest (list-tail chars 5)))
     (values (list->string (sedna:first-n lng rest))
             (list-tail rest lng)))))
                    
;-------------------------------------------------
; Writing a package

;; Writes the package to the output-port
;;  header-code - the number that represents the code of the message
;;  body - the string that represents the message body
;(define (sedna:write-package header-code body output-port)
;  (display
;   (list->string (sedna:integer->chars header-code))
;   output-port)
;  (display
;   (list->string (sedna:integer->chars (string-length body)))
;   output-port)
;  (display body output-port))

; Writes the package to the output-port
;  header-code - the number that represents the code of the message
;  body - the list of bytes that represent the message body
(define (sedna:write-package-as-bytes header-code body output-port)
  (display
   (list->string (sedna:integer->chars header-code))
   output-port)
  (display
   (list->string (sedna:integer->chars (length body)))
   output-port)
  (display
   (list->string body)
   output-port)
  (sedna:flush-output-port output-port)
  ;(pp (list "Sent:" header-code))
  )
  
;-------------------------------------------------
; Reading a package

; Special package headers
(define sedna:ItemPart 360)
(define sedna:ItemEnd 370)

;; Reads the package from input port
;; Returns: (values header-code body-string)
;(define (sedna:read-package input-port)
;  (let*  ; the order of evaluation is significant
;      ((header-code
;        (sedna:chars->integer
;         (sedna:read-n-chars 4 input-port)))
;       (body-size
;        (sedna:chars->integer
;         (sedna:read-n-chars 4 input-port))))
;    (values header-code
;            (list->string (sedna:read-n-chars body-size input-port)))))

; Reads the package from input port
; Returns: (values header-code body-char-list)
; Returns the list the message body as the list of chars
(define (sedna:read-package-as-chars input-port)
  (let*  ; the order of evaluation is significant
      ((header-code
        (sedna:chars->integer
         (sedna:read-n-chars 4 input-port)))
       (body-size
        (sedna:chars->integer
         (sedna:read-n-chars 4 input-port))))
    ;(pp (list "Received:" header-code))
    (values header-code
            (sedna:read-n-chars body-size input-port))))

; Reads the first package after packages with sedna:ItemPart and sedna:ItemEnd
; header codes.
; Returns: (values header-code body-char-list)
(define (sedna:read-package-after-item-parts input-port)
  (call-with-values
   (lambda () (sedna:read-package-as-chars input-port))
   (lambda (header-code body-char-list)
     (if
      (or (= header-code sedna:ItemPart) (= header-code sedna:ItemEnd))
      (sedna:read-package-after-item-parts input-port)
      (values header-code body-char-list)))))


;==========================================================================
; Connection with a server

;-------------------------------------------------
; Connection datatype
; Is represented in SXML, as follows:
;  `(connection
;    (host ,host)
;    (db-name ,db-name)
;    (user ,user)
;    (password ,password)
;    (connection-input-port ,port)
;    (connection-output-port ,port))

; Predicate
(define (sedna:connection? obj)
  (and (pair? obj) (not (null? obj))
       (eq? (car obj) 'connection)
       (assq 'host (cdr obj)) (assq 'db-name (cdr obj))
       (assq 'user (cdr obj)) (assq 'passworn (cdr obj))
       (assq 'connection-input-port (cdr obj))
       (assq 'connection-output-port (cdr obj))))

; Constructor
(define (sedna:construct-connection host db-name user password
                                    tcp-input-port tcp-output-port)
  `(connection
    (host ,host)
    (db-name ,db-name)
    (user ,user)
    (password ,password)
    (connection-input-port ,tcp-input-port)
    (connection-output-port ,tcp-output-port)))

; Lower level function for parameterized accessors
(define (sedna:parameterized-connection-accessor entry)
  (lambda (connection)
    (cond
      ((assq entry (cdr connection))
       => cadr)
      (else #f))))
         
; Accessors
; In case of an error, #f is returned
(define sedna:connection-host
  (sedna:parameterized-connection-accessor 'host))
(define sedna:connection-db-name
  (sedna:parameterized-connection-accessor 'db-name))
(define sedna:connection-user
  (sedna:parameterized-connection-accessor 'user))
(define sedna:connection-password
  (sedna:parameterized-connection-accessor 'password))
(define sedna:connection-input
  (sedna:parameterized-connection-accessor 'connection-input-port))
(define sedna:connection-output
  (sedna:parameterized-connection-accessor 'connection-output-port))

;-------------------------------------------------
; Raise errors over messages

; Parses an error code and an info
; Returns a message string
(define (sedna:error-code+info body-chars)
  (if
   (null? body-chars)  ; no message body supplied
   "no error message transferred"
   (let ((code (sedna:chars->integer (sedna:first-n 4 body-chars))))
     (let-values*
      (((info dummy)
        (sedna:extract-string (list-tail body-chars 4))))
      (string-append info ".\n")))))

; Raises an exception with a given `msg' and the error info in the return
; package
;  msg - the constant message
;  body-chars - the body of the response message, represented as a list
; of chars
(define (sedna:raise-with-info msg body-chars)
  (if
   (null? body-chars)  ; no message body supplied
   (begin
     (sedna:raise-exn
      msg ": Server reported of an unknown error")
     #f)
   (let ((code
          (sedna:chars->integer (sedna:first-n 4 body-chars))))
     (let-values*
      (((info dummy)
        (sedna:extract-string (list-tail body-chars 4))))
      (sedna:raise-exn
       msg ": " info)
      #f))))

; Reaction on server ErrorResponse
(define (sedna:raise-server-error body-chars)
  (sedna:raise-with-info "Server reported of an error" body-chars))

;-------------------------------------------------
; Connection Start-Up

; Several constants for package header codes
(define sedna:Start-Up 110)
(define sedna:SessionParameters 120)
(define sedna:AuthentificationParameters 130)
(define sedna:SendSessionParameters 140)
(define sedna:SendAuthParameters 150)
(define sedna:AuthentificationOk 160)
(define sedna:AuthentificationFailed 170)
(define sedna:ErrorResponse 100)

; Establishing a connection
; In the normal case, connection object is returned.
; In case of an error, exception is raised and #f is returned
(define (sedna:connect-to-database host db-name user password)
  (let* ((host (if (string=? host "localhost")
                   "127.0.0.1" host))
         (ports
          (sedna:open-tcp-connection host 5050))
         (in (car ports))
         (out (cdr ports)))
    (sedna:write-package-as-bytes sedna:Start-Up '() out)
    (let-values*
     (((code body-chars)
       (sedna:read-package-as-chars in)))
     (cond
       ((= code sedna:SendSessionParameters)
        ; Asked for session parameters
        (sedna:write-package-as-bytes
         sedna:SessionParameters
         (cons
          sedna:char001  ; major protocol version number
          (cons
           sedna:char000  ; minor protocol version number
           (append
            (sedna:string->network user)
            (sedna:string->network db-name))))         
         out)
        (let-values*
         (((code body-chars)
           (sedna:read-package-as-chars in)))
         (cond
           ((= code sedna:SendAuthParameters)
            ; Asked for auth parameters
            (sedna:write-package-as-bytes
             sedna:AuthentificationParameters
             (sedna:string->network password)
             out)
            (let-values*
             (((code body-chars)
               (sedna:read-package-as-chars in)))
             (cond
               ((= code sedna:AuthentificationOk)
                (sedna:construct-connection
                 host db-name user password in out))
               ((= code sedna:AuthentificationFailed)
                (sedna:raise-exn
                 "sedna:connect-to-database: Authentification failed")
                #f)
               ((= code sedna:ErrorResponse)
                (sedna:raise-server-error body-chars))
               (else
                (sedna:raise-exn
                 "sedna:connect-to-database: Unexpected header code from server: "
                 (number->string code))))))                              
           ((= code sedna:ErrorResponse)
            (sedna:raise-server-error body-chars))
           (else
            (sedna:raise-exn
             "sedna:connect-to-database: Unexpected header code from server: "
             (number->string code))))))
     ((= code sedna:ErrorResponse)
        (sedna:raise-server-error body-chars))
       (else
        (sedna:raise-exn
         "sedna:connect-to-database: Unexpected header code from server: "
         (number->string code)))))))

;-------------------------------------------------
; Connection termination

; Several constants for package header codes
(define sedna:CloseConnection 500)
(define sedna:CloseConnectionOk 510)
(define sedna:TransactionRollbackBeforeClose 520)

; Disconnects from the database
; Returns #t in the correct case
(define (sedna:disconnect-from-database connection)
  (let ((in (sedna:connection-input connection))
        (out (sedna:connection-output connection)))
    (sedna:write-package-as-bytes sedna:CloseConnection '() out)
    (call-with-values
     (lambda () (sedna:read-package-after-item-parts in))
     (lambda (code body-chars)
       (sedna:close-tcp-connection out)  ; Close the connection
       (sedna:close-tcp-connection in)  ; ZNV: it might be wrong
       (cond
         ((or (= code sedna:CloseConnectionOk)
              (= code sedna:TransactionRollbackBeforeClose))        
          #t)
         ((= code sedna:ErrorResponse)
          (sedna:raise-server-error body-chars))
         (else
          (sedna:raise-exn
           "sedna:disconnect-from-database: Unexpected header code from server: "
           (number->string code))))))))


;==========================================================================
; Transactions

; Several constants for package header codes
(define sedna:BeginTransaction 210)
(define sedna:CommitTransaction 220)
(define sedna:RollbackTransaction 225)
(define sedna:BeginTransactionOk 230)
(define sedna:BeginTransactionFailed 240)
(define sedna:CommitTransactionOk 250)
(define sedna:RollbackTransactionOk 255)
(define sedna:CommitTransactionFailed 260)
(define sedna:RollbackTransactionFailed 265)

;-------------------------------------------------
; High-level API functions

; Begin transaction
; In the normal case, returns #t. Otherwise, exception is raised
(define (sedna:begin-transaction connection)
  (let ((in (sedna:connection-input connection))
        (out (sedna:connection-output connection)))
    (sedna:write-package-as-bytes sedna:BeginTransaction '() out)
    (call-with-values
     (lambda () (sedna:read-package-after-item-parts in))
     (lambda (code body-chars)
       (cond
         ((= code sedna:BeginTransactionOk)
          #t)
         ((= code sedna:BeginTransactionFailed)
          (sedna:raise-exn
           "sedna:begin-transaction: Begin transaction failed")
          #f)        
         ((= code sedna:ErrorResponse)
          (sedna:raise-server-error body-chars))       
         (else
          (sedna:raise-exn
           "sedna:begin-transaction: Unexpected header code from server: "
           (number->string code))))))))
    
; End transaction
;  action ::= 'COMMIT | 'ROLLBACK
; In the normal case, returns #t. Otherwise, exception is raised
(define (sedna:end-transaction connection action)
  (cond
    ((eq? action 'COMMIT)  
     (let ((in (sedna:connection-input connection))
           (out (sedna:connection-output connection)))
       (sedna:write-package-as-bytes sedna:CommitTransaction '() out)
       (call-with-values
        (lambda () (sedna:read-package-after-item-parts in))
        (lambda (code body-chars)
          (cond
            ((= code sedna:CommitTransactionOk)
             #t)
            ((= code sedna:CommitTransactionFailed)
             (sedna:raise-with-info "Transaction commit failed" body-chars))
            ((= code sedna:ErrorResponse)
             (sedna:raise-server-error body-chars))       
            (else
             (sedna:raise-exn
              "sedna:end-transaction: Unexpected header code from server: "
              (number->string code))))))))
    ((eq? action 'ROLLBACK)
     (let ((in (sedna:connection-input connection))
           (out (sedna:connection-output connection)))
       (sedna:write-package-as-bytes sedna:RollbackTransaction '() out)
       (call-with-values
        (lambda () (sedna:read-package-after-item-parts in))
        (lambda (code body-chars)
          (cond
            ((= code sedna:RollbackTransactionOk)
             #t)
            ((= code sedna:RollbackTransactionFailed)
             (sedna:raise-with-info "Transaction rollback failed" body-chars))
            ((= code sedna:ErrorResponse)
             (sedna:raise-server-error body-chars))
            (else
             (sedna:raise-exn
              "sedna:end-transaction: Unexpected header code from server: "
              (number->string code))))))))
    (else
     (sedna:raise-exn "sedna:end-transaction: unknown action specified")
     #f)))


;==========================================================================
; Query execution

; Several constants for query parts representation
(define sedna:Execute 300)
(define sedna:ExecuteLong 301)
(define sedna:LongQueryEnd 302)

; Writes a query to the output port
; query - a string that represents a query
; result-type-sxml? - whether the query result is to be output in SXML format
; out - output port
; If a query length exceeds 10Kbytes, the query is transmitted in several
; packets, otherwise, it is transmitted as a single packet
; The function always returns #t
(define sedna:write-query-out
  (let ((max-query-length 10000))
    (lambda (query result-type-sxml? out)
      (if
       (< (string-length query) max-query-length)  ; a relatively short query
       (sedna:write-package-as-bytes
        sedna:Execute
        (cons
         (if result-type-sxml? sedna:char001 sedna:char000)
         (sedna:string->network query))
        out)
       (let ((res-type-char
              (if result-type-sxml? sedna:char001 sedna:char000))
             (lng (string-length query)))
         (let loop ((i 0))
           (if
            ; remainder of a query contains less than 5K bytes
            (< (- lng i) max-query-length)
            (begin
              (sedna:write-package-as-bytes
               sedna:ExecuteLong
               (cons res-type-char
                     (sedna:string->network (substring query i lng)))
               out)
              (sedna:write-package-as-bytes
               sedna:LongQueryEnd '() out)
              #t)
            (begin
              (sedna:write-package-as-bytes
               sedna:ExecuteLong
               (cons res-type-char
                     (sedna:string->network
                      (substring query i (+ i max-query-length))))
               out)
              (loop (+ i max-query-length))))))))))       

;-------------------------------------------------
; Representation for a result
;  result ::= (cons item promise)
;  item - the current item in the sequence
;  promise - the promise to obtain the remaining items

(define (sedna:next result)
  (force (cdr result)))

; Reads the whole result, transforms it into the list
(define (sedna:result->list result)
  (if
   (not (pair? result))  ; result of an update operation
   result
   (let loop ((pair result)
              (lst '()))
     (cond
       ((null? pair)
        (reverse lst))
       ((null? (cdr pair))
        (reverse (cons (car pair) lst)))
       (else
        (loop (sedna:next pair)
              (cons (car pair) lst)))))))

;-------------------------------------------------
; Next item from the connection

; Several constants for package header codes
(define sedna:GetNextItem 310)
(define sedna:ResultEnd 375)

; Reads the next item from the connection.
; Returns either (cons item promise) or '()
(define (sedna:get-next-xml-item connection)
  (let ((in (sedna:connection-input connection))
        (out (sedna:connection-output connection)))
    (let loop ((res '())
               (frst #t))
      (let-values*
       (((code body-chars)
         (sedna:read-package-as-chars in)))
       (cond
         ((= code sedna:ItemPart)
          (loop (append
                 res
                 (let-values*                         
                  (((part dummy)
                    (sedna:extract-string body-chars)))
                  (list part)))
                #f))
         ((= code sedna:ResultEnd)
          (if frst  ; nothing yet in the res
              '()
              (list (sedna:apply-string-append res))))
         ((= code sedna:ItemEnd)
          (if
           frst  ; this item is empty (why?)
           (begin
             (sedna:write-package-as-bytes sedna:GetNextItem '() out)
             (sedna:get-next-xml-item connection))
           (let ((curr-position (sedna:port-position in)))
             (cons
              (sedna:apply-string-append res)
              (delay
                (cond
                  ((not (= (sedna:port-position in) curr-position))
                   (sedna:raise-exn
                    "sedna:get-next-xml-item: "
                    "Result invalid since the next query was executed")
                   '())
                  (else
                   (sedna:write-package-as-bytes sedna:GetNextItem '() out)
                   (sedna:get-next-xml-item connection))))))))
         ((= code sedna:ErrorResponse)
          (sedna:raise-server-error body-chars))
         (else
          (sedna:raise-exn
           "sedna:get-next-xml-item: Unexpected header code from server: "
           (number->string code))))))))

; Reads the next item from the connection.
; Returns either (cons item promise) or '()
(define (sedna:get-next-item connection)
  (let ((in (sedna:connection-input connection))
        (out (sedna:connection-output connection)))
    (let loop ((res '())
               (frst #t))
      (let-values*
       (((code body-chars)
         (sedna:read-package-as-chars in)))
       (cond
         ((= code sedna:ItemPart)
          (loop (append
                 res
                 (let-values*                         
                  (((part dummy)
                    (sedna:extract-string body-chars)))
                  (list part)))
                #f))
         ((= code sedna:ResultEnd)
          (if frst  ; nothing yet in the res
              '()
              (list (call-with-input-string
                     (sedna:apply-string-append res)
                     read))))
         ((= code sedna:ItemEnd)
          (if
           frst  ; this item is empty (why?)
           (begin
             (sedna:write-package-as-bytes sedna:GetNextItem '() out)
             (sedna:get-next-xml-item connection))
           (let ((curr-position (sedna:port-position in)))
             (cons
              ; DL: Uncomment this to get the result in SXML
              (call-with-input-string
               (sedna:apply-string-append res)
               read)
              (delay
                (cond
                  ((not (= (sedna:port-position in) curr-position))
                   (sedna:raise-exn
                    "sedna:get-next-item: "
                    "Result invalid since the next query was executed")
                   '())
                  (else
                   (sedna:write-package-as-bytes sedna:GetNextItem '() out)
                   (sedna:get-next-item connection))))))))
         ((= code sedna:ErrorResponse)
          (sedna:raise-server-error body-chars))
         (else
          (sedna:raise-exn
           "sedna:get-next-item: Unexpected header code from server: "
           (number->string code))))))))

;-------------------------------------------------
; Bulk load

; Several constants for package header codes
(define sedna:BulkLoadError 400)
(define sedna:BulkLoadPortion 410)
(define sedna:BulkLoadEnd 420)
(define sedna:BulkLoadFileName 430)
(define sedna:BulkLoadFromStream 431)
(define sedna:BulkLoadSucceeded 340)  ; =sedna:UpdateSucceeded
;(define sedna:BulkLoadSucceeded 440)
(define sedna:BulkLoadFailed 450)

; Bulk load from file
(define (sedna:bulk-load-file filename connection)
  (if
   (file-exists? filename)
   (sedna:bulk-load-port (open-input-file filename) connection)   
   (let ((in (sedna:connection-input connection))
         (out (sedna:connection-output connection)))
     (sedna:write-package-as-bytes
      sedna:BulkLoadError
      (append
       (sedna:integer->chars 666)
       (sedna:string->network "Requested file doesn't exist"))
      out)
     (sedna:read-package-as-chars in)
     (sedna:raise-exn
      "sedna:bulk-load-file: Requested file doesn't exist: " filename)
     #f)))

; Bulk load from input port
;  port - an input stream
(define (sedna:bulk-load-port port connection)
  (let ((in (sedna:connection-input connection))
        (out (sedna:connection-output connection)))  
     (let loop ((ch (peek-char port)))
       (cond
         ((eof-object? ch)
          (close-input-port port)
          (sedna:write-package-as-bytes sedna:BulkLoadEnd '() out)
          (sedna:flush-output-port out)
          ;(display "Bulk load end sent")
          ;(newline)
          (let-values*
           (((code body-chars)
             (sedna:read-package-as-chars in)))
           (cond
             ((= code sedna:BulkLoadSucceeded)
              #t)
             ((= code sedna:BulkLoadFailed)
              (sedna:raise-exn
               "sedna:bulk-load-stream: Bulk load failed: "
               (sedna:error-code+info body-chars))
              #f)
             ((= code sedna:ErrorResponse)
              (sedna:raise-server-error body-chars))
             (else
              (sedna:raise-exn
               "sedna:bulk-load-stream: Unexpected header code from server: "
               (number->string code))))))
         (else
          (sedna:write-package-as-bytes     
           sedna:BulkLoadPortion
           (sedna:chars->network (sedna:read-n-or-less 1000 port))
           out)
          (loop (peek-char port)))))))

;-------------------------------------------------
; High-level API functions

; Several constants for package header codes
(define sedna:QuerySucceeded 320)
(define sedna:QueryFailed 330)
(define sedna:UpdateSucceeded 340)
(define sedna:UpdateFailed 350)

; Execute a query and represent the result in the form of XML
(define (sedna:execute-query-xml connection query)
  (let ((in (sedna:connection-input connection))
        (out (sedna:connection-output connection)))
    (sedna:write-query-out query #f out)
    (call-with-values
     (lambda () (sedna:read-package-after-item-parts in))
     (lambda (code body-chars)
       (cond
         ((= code sedna:QuerySucceeded)
          (sedna:get-next-xml-item connection))
         ((= code sedna:QueryFailed)        
          (sedna:raise-exn
           "sedna:execute-query-xml: " (sedna:error-code+info body-chars))
          #f)
         ((= code sedna:UpdateSucceeded)
          #t)
         ((= code sedna:UpdateFailed)
          (sedna:raise-exn
           "sedna:execute-query: Update failed"
           (sedna:error-code+info body-chars))
          #f)
         ((= code sedna:BulkLoadFileName)
          (let-values*
           (((filename dummy)
             (sedna:extract-string body-chars)))
           (sedna:bulk-load-file filename connection)))
         ((= code sedna:BulkLoadFromStream)
          (sedna:bulk-load-port (current-input-port) connection))      
         ((= code sedna:ErrorResponse)
          (sedna:raise-server-error body-chars))       
         (else
          (sedna:raise-exn
           "sedna:execute-query-xml: Unexpected header code from server: "
           (number->string code))))))))

; Execute a query
(define (sedna:execute-query connection query)
  (let ((in (sedna:connection-input connection))
        (out (sedna:connection-output connection)))
    (sedna:write-query-out query #t out)
    (call-with-values
     (lambda () (sedna:read-package-after-item-parts in))
     (lambda (code body-chars)
       (cond
         ((= code sedna:QuerySucceeded)
          (sedna:get-next-item connection))
         ((= code sedna:QueryFailed)
          (sedna:raise-exn
           "sedna:execute-query: " (sedna:error-code+info body-chars))
          #f)
         ((= code sedna:UpdateSucceeded)
          #t)
         ((= code sedna:UpdateFailed)
          (sedna:raise-exn
           "sedna:execute-query: Update failed"
           (sedna:error-code+info body-chars))
          #f)
         ((= code sedna:BulkLoadFileName)
          (let-values*
           (((filename dummy)
             (sedna:extract-string body-chars)))         
           (sedna:bulk-load-file filename connection)))
         ((= code sedna:BulkLoadFromStream)
          (sedna:bulk-load-port (current-input-port) connection))       
         ((= code sedna:ErrorResponse)
          (sedna:raise-server-error body-chars))       
         (else
          (sedna:raise-exn
           "sedna:execute-query: Unexpected header code from server: "
           (number->string code))))))))


;==========================================================================
; Obtaining query result items as XML stream
; Sedna input stream for an XML item is represented in SXML as follows:
;  `(sedna:input-stream
;    (stream-input-port ,input-port)
;    (item-part-getter  ,lambda-with-no-arguments))
; When a character is not ready in the `input-port', calling
; `lambda-with-no-arguments' provides additional characters to the `input-port'
; as a side effect, until the whole current XML item is written.
; `lambda-with-no-arguments' returns #t iff anything was consumed from 
; tcp-connection-input.

; Predicate
(define (sedna:input-stream? obj)
  (and (pair? obj) (not (null? obj))
       (eq? (car obj) 'sedna:input-stream)
       (assq 'stream-input-port (cdr obj))
       (assq 'item-part-getter (cdr obj))))

; Constructor
(define (sedna:construct-input-stream input-port lambda-with-no-arguments)
  `(sedna:input-stream
    (stream-input-port ,input-port)
    (item-part-getter  ,lambda-with-no-arguments)))
         
; Accessors
; In case of an error, #f is returned
(define sedna:input-stream-port
  (sedna:parameterized-connection-accessor 'stream-input-port))
(define sedna:input-stream-item-part-getter
  (sedna:parameterized-connection-accessor 'item-part-getter))

;-------------------------------------------------
; Common functions for reading from sedna:input-stream

(define (sedna:char-reader-helper port-reader)
  (lambda (input-stream)
    (let ((port (sedna:input-stream-port input-stream)))
      (begin
        (if
         (char-ready? port)
         #t
         ; Otherwise: character not ready, need to supply more characters
         ((sedna:input-stream-item-part-getter input-stream)))
        (port-reader port)))))

; Analogue of R5RS `read-char'
(define sedna:read-char (sedna:char-reader-helper read-char))

; Analogue of R5RS `peek-char'
(define sedna:peek-char (sedna:char-reader-helper peek-char))
  
; Reads the string from Sedna input stream until the newline symbol
; Consumes the newline symbol from stream, returns the string without
; the ending newline
(define (sedna:read-line input-stream)
  (let ((port (sedna:input-stream-port input-stream)))
    (let loop ((char-lst '()))
      (let ((ch (begin
                  (if
                   (char-ready? port)
                   #t
                   ((sedna:input-stream-item-part-getter input-stream)))
                  (read-char port))))
        (cond
          ((eof-object? ch)  ; the file is over
           (if (null? char-lst)  ; nothing was read
               ch
               (list->string (reverse char-lst))))
          ((memv ch '(#\return #\newline))
           (begin             
             (if  ; followed by newline
              (and
               (not (and (char-ready? port)
                         (eof-object? (peek-char port))))
               (begin
                 (if
                   (char-ready? port)
                   #t
                   ((sedna:input-stream-item-part-getter input-stream)))
                 (char=? (peek-char port) #\newline)))
              (read-char port)  ; consume this newline as well
              #t  ; otherwise, do nothing
              )
             (list->string (reverse char-lst))))
          (else
           (loop (cons ch char-lst))))))))

;-------------------------------------------------
; Constructing XML stream object for the next item query result

; Reads the next item from the connection.
; Returns either (cons input-stream promise) or '()
(define (sedna:get-next-xml-stream-item connection)
  (let ((in (sedna:connection-input connection))
        (out (sedna:connection-output connection)))
    (call-with-values
     (lambda () (sedna:read-package-as-chars in))
     (lambda (code body-chars)
       (cond
         ((= code sedna:ItemPart)
          (let* ((ports-pair (sedna:make-pipe))
                 (pipe-from (car ports-pair))
                 (pipe-to (cdr ports-pair))
                 (curr-position (sedna:port-position out))
                 (item-part-getter
                  (lambda ()
                    (cond  ; stream is over?
                      ((and (char-ready? pipe-from)
                            (eof-object? (peek-char pipe-from)))
                       #f  ; nothing was read from in
                       )
                      ((not (= (sedna:port-position out) curr-position))
                       (sedna:raise-exn
                        "Sedna item-part-getter: "
                        "Result invalid since the next query was executed")
                       #f)
                      (else
                       (call-with-values
                        (lambda () (sedna:read-package-as-chars in))
                        (lambda (code body-chars)
                          (cond
                            ((= code sedna:ItemPart)
                             (call-with-values
                              (lambda () (sedna:extract-string body-chars))
                              (lambda (part dummy)
                                (display part pipe-to)))
                             #t)
                            ((= code sedna:ItemEnd)
                             (sedna:close-output-pipe pipe-to)
                             #f)
                            (else
                             (sedna:raise-exn
                              "Sedna item-part-getter: "
                              "Unexpected header code from server: "
                              (number->string code)))))))))))
            (call-with-values
             (lambda () (sedna:extract-string body-chars))
             (lambda (part dummy)
               (display part pipe-to)
               (cons
                (sedna:construct-input-stream pipe-from item-part-getter)
                (delay
                  (if
                   (not (= (sedna:port-position out) curr-position))
                   (sedna:raise-exn
                    "sedna:get-next-xml-stream-item: "
                    "Result invalid since the next query was executed")
                   (let loop ((consumed? item-part-getter))
                     ; Accomplish writing current item part to sedna stream
                     (if
                      consumed?
                      (loop (item-part-getter))
                      (begin
                        (sedna:write-package-as-bytes sedna:GetNextItem '() out)
                        (sedna:get-next-xml-stream-item connection)))))))))))
         ((= code sedna:ItemEnd)
          ; This item is empty (why?), request for the next item
          (sedna:write-package-as-bytes sedna:GetNextItem '() out)
          (sedna:get-next-xml-stream-item connection))
         ((= code sedna:ResultEnd)
          '())
         ((= code sedna:ErrorResponse)
          (sedna:raise-server-error body-chars))
         (else
          (sedna:raise-exn
           "sedna:get-next-xml-stream-item: Unexpected header code from server: "
           (number->string code))))))))

;-------------------------------------------------
; Higher-level API

; Execute a query and represent the result in the form of XML stream
(define (sedna:execute-query-xml-stream connection query)
  (let ((in (sedna:connection-input connection))
        (out (sedna:connection-output connection)))
    (sedna:write-query-out query #f out)
    (call-with-values
     (lambda () (sedna:read-package-after-item-parts in))
     (lambda (code body-chars)
       (cond
         ((= code sedna:QuerySucceeded)
          (sedna:get-next-xml-stream-item connection))
         ((= code sedna:QueryFailed)        
          (sedna:raise-exn
           "sedna:execute-query-xml: " (sedna:error-code+info body-chars))
          #f)
         ((= code sedna:UpdateSucceeded)
          #t)
         ((= code sedna:UpdateFailed)
          (sedna:raise-exn
           "sedna:execute-query: Update failed"
           (sedna:error-code+info body-chars))
          #f)
         ((= code sedna:BulkLoadFileName)
          (let-values*
           (((filename dummy)
             (sedna:extract-string body-chars)))
           (sedna:bulk-load-file filename connection)))
         ((= code sedna:BulkLoadFromStream)
          (sedna:bulk-load-port (current-input-port) connection))      
         ((= code sedna:ErrorResponse)
          (sedna:raise-server-error body-chars))       
         (else
          (sedna:raise-exn
           "sedna:execute-query-xml: Unexpected header code from server: "
           (number->string code))))))))


;==========================================================================
; Wrapper for bulk load from stream

; Is to be executed withing a transaction
;  port - an input port for a stream
;  document-name - the name of a new document in a database
;  collection-name - if supplied, specifies the name of the collection for a
; document in the database
(define (sedna:bulk-load-from-xml-stream
         connection port document-name . collection-name)
  (let ((in (sedna:connection-input connection))
        (out (sedna:connection-output connection)))
    (sedna:write-query-out
     (if
      (null? collection-name)  ; it is a standalone document
      (string-append "LOAD STDIN \"" document-name "\"")
      (string-append
       "LOAD STDIN \"" document-name "\" \"" (car collection-name) "\""))
     #t  ; result in the form of SXML
     out)
    (call-with-values
     (lambda () (sedna:read-package-after-item-parts in))
     (lambda (code body-chars)
       (cond       
         ((= code sedna:BulkLoadFromStream)
          (sedna:bulk-load-port port connection))
         ((= code sedna:ErrorResponse)
          (sedna:raise-server-error body-chars))      
         (else
          (sedna:raise-exn
           "sedna:bulk-load-from-xml-stream: "
           "Unexpected header code from server: " (number->string code))))))))


;==========================================================================
; Transaction operators

; queries ::= (listof query)
; Each query is a string
; Begins a transaction and executer all supplied queries in a sequence.
; If every query executes properly, the function commits the transaction and
; returns the result of the last query executed
; If an error occurs during some query processing, further queries are not
; executed and transaction rollback is performed. Exception is re-raised
(define (sedna:transaction connection . queries)
  (handle-exceptions
   exc
   (begin
     ; DL: should be rollback
     (sedna:end-transaction connection 'COMMIT)
     ; Re-raising the exception
     (exc:signal exc))
   (begin
     (sedna:begin-transaction connection)
     ;(display "Should be printed")
     (let loop ((queries queries)
                (last-res #t))
       (cond
         ((null? queries)  ; all queries executed
          (sedna:end-transaction connection 'COMMIT)
          last-res)
         (else
          (let* ((res (sedna:execute-query connection (car queries)))
                 (res (if (pair? res)  ; a promise
                          (sedna:result->list res)
                          res)))
            (loop (cdr queries)
                  res))))))))
