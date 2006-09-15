
; File:  sedna-low.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

;; Low-level Scheme-specific functions

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
  (define sedna:close-tcp-connection close-output-port)
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
