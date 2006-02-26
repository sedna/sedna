(require (lib "match.ss"))
(require (lib "list.ss" "srfi" "1"))

(load "../stub.scm")
(load "../xquery-lr-lib.scm")
(load "../dschema-utils.scm")
(load "popt.scm")



(define query
  '(query
    (prolog)
    (query-body
     (ddo
      (return
       (child
        (ddo
         (child
          (ddo
           (child
            (!fn!document (const (type !xs!string) "auction"))
            (type (elem-test (ename (const (type !xs!QName) ("" "site")) (type *) (const (type !xs!string) "non-nil"))))))
          (type
           (elem-test 
            (ename (const (type !xs!QName) ("" "open_auctions")) (type *) (const (type !xs!string) "non-nil"))))))
        (type (elem-test 
               (ename (const (type !xs!QName) ("" "open_auction")) (type *) (const (type !xs!string) "non-nil")))))
       (fun-def
        ((!xs!anyType (var ("" "$v"))))
        (if@
         (and@
          (=@
           (const (type !xs!integer) 153)
           (ddo
            (child
             (ddo
              (child
               (var ("" "$v"))
               (type
                (elem-test (ename (const (type !xs!QName) ("" "bidder")) (type *) (const (type !xs!string) "non-nil"))))))
             (type
              (elem-test (ename (const (type !xs!QName) ("" "increase")) (type *) (const (type !xs!string) "non-nil")))))))
          (and@
           (=@
            (const (type !xs!integer) 153)
            (ddo
             (child
              (ddo
               (child
                (var ("" "$v"))
                (type
                 (elem-test (ename (const (type !xs!QName) ("" "bidder")) (type *) (const (type !xs!string) "non-nil"))))))
              (type
               (elem-test (ename (const (type !xs!QName) ("" "increase")) (type *) (const (type !xs!string) "non-nil")))))))
           (=@
            (const (type !xs!integer) 153)
            (ddo
             (child
              (ddo
               (child
                (var ("" "$v"))
                (type
                 (elem-test (ename (const (type !xs!QName) ("" "bidder")) (type *) (const (type !xs!string) "non-nil"))))))
              (type
               (elem-test (ename (const (type !xs!QName) ("" "increase")) (type *) (const (type !xs!string) "non-nil")))))))))
         (var ("" "$v"))
         (sequence))))))))

;(define query
;  '(query
;    (prolog)
;    (query-body
;     (ddo
;      (return
;       (child
;        (ddo
;         (child
;          (ddo
;           (child
;            (!fn!document (const (type !xs!string) "auction"))
;            (type (elem-test (ename (const (type !xs!QName) ("" "site")) (type *) (const (type !xs!string) "non-nil"))))))
;          (type
;           (elem-test (ename (const (type !xs!QName) ("" "open_auctions")) (type *) (const (type !xs!string) "non-nil"))))))
;        (type (elem-test (ename (const (type !xs!QName) ("" "open_auction")) (type *) (const (type !xs!string) "non-nil")))))
;       (fun-def
;        ((!xs!anyType (var ("" "$v"))))
;        (if@
;         (=@
;          (ddo
;           (child
;            (var ("" "$v"))
;            (type
;             (elem-test (ename (const (type !xs!QName) ("" "initial")) (type *) (const (type !xs!string) "non-nil"))))))
;          (const (type !xs!decimal) 36.18))
;         (var ("" "$v"))
;         (sequence))))))))


;(define query
;  '(query
;    (prolog)
;    (query-body
;     (ddo
;      (return
;       (child
;        (ddo
;         (child
;          (ddo
;           (child
;            (!fn!document (const (type !xs!string) "auction"))
;            (type (elem-test (ename (const (type !xs!QName) ("" "site")) (type *) (const (type !xs!string) "non-nil"))))))
;          (type
;           (elem-test 
;            (ename (const (type !xs!QName) ("" "open_auctions")) (type *) (const (type !xs!string) "non-nil"))))))
;        (type (elem-test 
;               (ename (const (type !xs!QName) ("" "open_auction")) (type *) (const (type !xs!string) "non-nil")))))
;       (fun-def
;        ((!xs!anyType (var ("" "$v"))))
;        (if@
;         (=@
;          (const (type !xs!integer) 153)
;          (ddo
;           (child
;            (ddo
;             (child
;              (var ("" "$v"))
;              (type
;               (elem-test (ename (const (type !xs!QName) ("" "bidder")) (type *) (const (type !xs!string) "non-nil"))))))
;            (type
;             (elem-test (ename (const (type !xs!QName) ("" "increase")) (type *) (const (type !xs!string) "non-nil")))))))
;         (var ("" "$v"))
;         (sequence))))))))

;(define query
;  '(query
;    (prolog)
;    (query-body
;     (ddo
;      (child
;       (ddo
;        (child
;         (ddo
;          (child
;           (!fn!document (const (type !xs!string) "auction"))
;           (type (elem-test (ename (const (type !xs!QName) ("" "site")) (type *) (const (type !xs!string) "non-nil"))))))
;         (type
;          (elem-test 
;           (ename (const (type !xs!QName) ("" "open_auctions")) (type *) (const (type !xs!string) "non-nil"))))))
;       (type (elem-test 
;              (ename (const (type !xs!QName) ("" "open_auction")) (type *) (const (type !xs!string) "non-nil")))))))))

;(define query
;  '(query
;    (prolog)
;    (query-body
;     (child
;      (!fn!document (const (type !xs!string) "auction"))
;      (type (elem-test (ename (const (type !xs!QName) ("" "site")) (type *) (const (type !xs!string) "non-nil"))))))))

;(define query
;  '(query
;    (prolog)
;    (query-body
;     (!fn!document (const (type !xs!string) "auction")))))


(popt:optimize-query query)
