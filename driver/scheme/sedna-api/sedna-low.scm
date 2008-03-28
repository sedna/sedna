
; File:  sedna-low.scm
; Copyright (C) 2004 The Institute for System Programming
; of the Russian Academy of Sciences (ISP RAS)

;; Low-level Scheme-specific functions

;-------------------------------------------------
; Working with a TCP connection
; This part of code is much borrowed from Oleg Kiselyov's "http.scm"
;
;  1. (sedna:open-tcp-connection host port-number)
; establishes the TCP connection and creates a pipe for communication with
; the server.
; Returns: (cons input-port output-port)
;
;  2. (sedna:flush-output-port output-port)
; Flushes the output port
;
;  3. (sedna:close-tcp-connection output-port)
; Closes the TCP connection
;
;  4. (sedna:port-position input-port)
; Returns the current position in the input port

(cond-expand
 
 (plt  
  (define (sedna:open-tcp-connection host port-number)
    (let-values*
     (((input-port output-port)
       (tcp-connect host port-number)))
     (cons input-port output-port)))
  (define sedna:flush-output-port flush-output)
  ; used for both closing input and output ports 
  ; PLT does not allow to close input port with a call to close-output-port
  (define (sedna:close-tcp-connection port)   
    (or(and(output-port? port)(close-output-port port))(close-input-port port)))
  (define sedna:port-position file-position))
 
 ; Chicken implementation has much the same with a PLT one, since Chicken
 ; supports PLT API for TCP connections
 (chicken
  (define (sedna:open-tcp-connection host port-number)
    (let-values*
     (((input-port output-port)
       (tcp-connect host port-number)))
     (cons input-port output-port)))
  (define sedna:flush-output-port flush-output)
  ; used for both closing input and output ports 
  ; however chicken allows to close input port with close-output-port (and vice-versa)
  (define sedna:close-tcp-connection close-output-port)
  (define (sedna:port-position input-port)
    (receive (row col) (port-position input-port)
             (+ (* 80 row) col))))
 
 (gambit
  (define (sedna:open-tcp-connection host port-number)
    (let ((p (open-tcp-client
              (list server-address: host
                    port-number: port-number))))
      (cons p p)))    
  (define sedna:flush-output-port force-output)
  (define sedna:close-tcp-connection close-port)
  (define (sedna:port-position input-port)
    ;(input-port-byte-position input-port)
    (+ (* 80 (input-port-line input-port))
       (input-port-column input-port)))
  )
 
 (else
  #f) 
)

; Procedure sedna:apply-string-append borrowed from SXML Serializer
; ("serializer.ss" in ssax.sf.net)
;
; procedure srl:apply-string-append :: STR-LST -> STRING
; str-lst ::= (listof string)
; Concatenates `str-lst' members into a single string
; (sedna:apply-string-append str-lst) = (apply string-append str-lst)
(cond-expand
 (chicken
  ; In Chicken, procedures are generally limited to 126 arguments
  ; http://www.call-with-current-continuation.org/
  ; Due to this Chicken limitation, we cannot apply `string-append' directly
  ; for a potentially long `str-lst'
  
  ; Similar to R5RS 'list-tail' but returns the new list consisting of the
  ; first 'k' members of 'lst'
  (define (sedna:list-head lst k)
    (if (or (null? lst) (zero? k))
        '()
        (cons (car lst) (sedna:list-head (cdr lst) (- k 1)))))

  ; Because of Chicken 126-argument limitation, I do not care of intermediate
  ; garbage produced in the following solution:
  (define (sedna:apply-string-append str-lst)
    (cond
      ((null? str-lst) "")
      ((null? (cdr str-lst)) (car str-lst))
      (else  ; at least two members
       (let ((middle (inexact->exact (round (/ (length str-lst) 2)))))
         (string-append
          (sedna:apply-string-append (sedna:list-head str-lst middle))
          (sedna:apply-string-append (list-tail str-lst middle)))))))
  )
 (else
  (define (sedna:apply-string-append str-lst)
    (apply string-append str-lst))
  ))

;==========================================================================
; Pipes
; 
;  1. (sedna:make-pipe)
; Creates a pipe. Returns: (cons input-port output-port)
;
;  2. (sedna:close-output-pipe port)
; Closes an output pipe. The result returned is unspecified
(cond-expand
 (plt
  (define (sedna:make-pipe)
    (call-with-values make-pipe cons))
  (define sedna:close-output-pipe close-output-port)
  
  )
 (chicken
  (define (sedna:make-pipe)
    (call-with-values create-pipe cons))
  (define sedna:close-output-pipe close-output-port)
  ;(define sedna:close-output-pipe close-output-pipe)
  
  )
 (else
  
  ; A temporary stub
  (define (sedna:make-pipe)
    (cons #f #f))
  (define (sedna:close-output-pipe port) #f)
  ))

;==========================================================================
; Byte-level transfer operations
; Moved here from "sedna-api.scm"

; Raises an exception
(define (sedna:raise-exn . msg)
  (exc:signal
   (make-property-condition 'exn
                            'message (sedna:apply-string-append msg))))

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

(cond-expand
 ((and plt plt-bytes)
  ; Special byte operations for PLT with (non-R5RS) byte datatype support

  (define sedna:char000 0)
  (define sedna:char001 1)
  
  (define sedna:read-byte read-byte)
  (define sedna:peek-byte peek-byte)
  
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
      (list frst scnd thrd frth)))
  
  (define (sedna:chars->integer byte-lst)
    (+ (* (car byte-lst) 16777216)
       (* (cadr byte-lst) 65536)
       (* (caddr byte-lst) 256)
       (cadddr byte-lst)))
  
  ;-------------------------------------------------
  ; Conversion between a string and its protocol network representation
  
  ; Converts a string to its network representation
  ; Returns: (listof char)
  (define (sedna:string->network str)
    (let ((lst (bytes->list (string->bytes/utf-8 str))))
      (cons
       sedna:char000
       (append
        (sedna:integer->chars (length lst))
        lst))))
  
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
       (values (bytes->string/utf-8 (list->bytes (sedna:first-n lng rest)))
               (list-tail rest lng)))))
  
  ;-------------------------------------------------
  ; Writing a package
  
  ; Writes the package to the output-port
  ;  header-code - the number that represents the code of the message
  ;  body - the list of bytes that represent the message body
  (define (sedna:write-package-as-bytes header-code body output-port)
    (display
     (list->bytes (sedna:integer->chars header-code))
     output-port)
    (display
     (list->bytes (sedna:integer->chars (length body)))
     output-port)
    (display (list->bytes body) output-port)
    (sedna:flush-output-port output-port))
  
  )
 (else
  ; bytes datatype not defined
  
  ; Several constants for char, since Bigloo doesn't support sedna:char000  and such
  (define sedna:char000 (integer->char 0))
  (define sedna:char001 (integer->char 1))
  
  (define sedna:read-byte read-char)
  (define sedna:peek-byte peek-char)
  
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
    (sedna:flush-output-port output-port))
  
  ))
  