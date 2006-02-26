
; File:  dschema-utils.scm
; Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)


(declare (unit dschema-utils) (uses common-lib scm-error-codes))

; Unit incapsulates utilites for working with descriptive schema
; Namespace prefix is "dscm:". It stands for descriptive schema


;===============================================================================
; Descriptive schema type
; node-type ::= elem
;             | text
;             | attr
;             | doc
;             | ns
;             | comm
;             | pr-ins
;
; statistics ::= (int int int)
;
; schema ::= (node-type node-name[?] statistics schema[*])
;

;-------------------------------------------------------------------------------
; descriptive schema node type accessor
(define (dscm:node-type schema)
  (first schema))

;-------------------------------------------------------------------------------
; descriptive schema node name accessor
(define (dscm:node-name schema)
  (let ((node-type (dscm:node-type schema)))
    (if (or (eq? node-type 'elem)
            (eq? node-type 'attr))
        (second schema)
        '())))

;-------------------------------------------------------------------------------
; descriptive schema node statistics accessor
(define (dscm:node-stat schema)
  (let ((node-type (dscm:node-type schema)))
    (if (or (eq? node-type 'elem)
            (eq? node-type 'attr))
        (third schema)
        (second schema))))

;-------------------------------------------------------------------------------
; descriptive schema node children accessor
(define (dscm:node-children schema)
  (let ((node-type (dscm:node-type schema)))
    (if (or (eq? node-type 'elem)
            (eq? node-type 'attr))
        (cdddr schema)
        (cddr schema))))




;===============================================================================
; Utils

;-------------------------------------------------------------------------------
; is principle node kind "element"?
(define (dscm:is-pnk-element node-type)
  (or (eq? node-type 'elem)
      (eq? node-type 'text)
      (eq? node-type 'doc)
      (eq? node-type 'ns)))

;-------------------------------------------------------------------------------
; is principle node kind "attribute"?
(define (dscm:is-pnk-attribute node-type)
  (eq? node-type 'attr))

;-------------------------------------------------------------------------------
; is [schema] of [node-type] and has a QName [qname]?
(define (dscm:comp-qname-type schema qname node-type)
  (and (eq? (dscm:node-type schema) node-type)
       (equal? qname (dscm:node-name schema))))

;-------------------------------------------------------------------------------
; is [schema] of [node-type] and from namespace NCName [ncname]?
(define (dscm:comp-uri-type schema ncname node-type)
  (and (eq? (dscm:node-type schema) node-type)
       (equal? ncname (car (dscm:node-name schema)))))

;-------------------------------------------------------------------------------
; is [schema] of [node-type] and has local name NCName [ncname]?
(define (dscm:comp-local-type schema ncname node-type)
  (and (eq? (dscm:node-type schema) node-type)
       (equal? ncname (cadr (dscm:node-name schema)))))




;===============================================================================
; XPath on descriptive schema
;
; node-test-axis ::= axis-child
;                  | axis-descendant
;                  | axis-attribute
;                  | axis-self
;                  | axis-descendant-or-self
;                  | axis-descendant-attr
;                  | axis-parent
;                   
; node-test-type ::= node-test-processing-instruction
;                  | node-test-comment
;                  | node-test-text
;                  | node-test-node
;                  | node-test-string
;                  | node-test-qname
;                  | node-test-wildcard-star
;                  | node-test-wildcard-ncname-star
;                  | node-test-wildcard-star-ncname
;                  | node-test-function-call
;                  | node-test-var-name
;                   
; node-test-data ::= "string"                // NCName
;                  | ("string" "string")     // QName
;                  | ppnode                  // PPOpIn *ppnode
; 
; node-test ::= (node-test-axis node-test-type node-test-data)
;

;-------------------------------------------------------------------------------
; node-test's type accessor
(define (dscm:nt:type node-test)
  (second node-test))

;-------------------------------------------------------------------------------
; node-test's data accessor
(define (dscm:nt:data node-test)
  (third node-test))

;-------------------------------------------------------------------------------
; node-test's data::qname accessor
(define (dscm:nt:qname node-test)
  (dscm:nt:data node-test))

;-------------------------------------------------------------------------------
; node-test's data::ncname accessor
(define (dscm:nt:ncname node-test)
  (dscm:nt:data node-test))




;-------------------------------------------------------------------------------
; child axis on descriptive schema node
; return value: schema[list]
(define (execute-node-test-axis-child schema node-test)
  ; principle node kind for child axis
  (define AXIS-CHILD-PNK 'elem)
  (let ((node-test-type (dscm:nt:type node-test)))
    (cond 
      ((eq? node-test-type 'node-test-processing-instruction)
       '())
      ((eq? node-test-type 'node-test-comment)
       '())
      ((eq? node-test-type 'node-test-text)
       (filter (lambda (scm) (eq? (dscm:node-type scm) 'text))
               (dscm:node-children schema)))
      ((eq? node-test-type 'node-test-text)
       (filter (lambda (scm) (dscm:is-pnk-element (dscm:node-type scm)))
               (dscm:node-children schema)))
      ((eq? node-test-type 'node-test-string)
       (cl:signal-input-error SE1002 "XPath on Schema: node_test_string on axis child"))
      ((eq? node-test-type 'node-test-qname)
       (filter (lambda (scm) (dscm:comp-qname-type scm (dscm:nt:qname node-test) AXIS-CHILD-PNK))
               (dscm:node-children schema)))
      ((eq? node-test-type 'node-test-wildcard-star)
       (filter (lambda (scm) (eq? (dscm:node-type scm) AXIS-CHILD-PNK))
               (dscm:node-children schema)))
      ((eq? node-test-type 'node-test-wildcard-ncname-star)
       (filter (lambda (scm) (dscm:comp-uri-type scm (dscm:nt:ncname node-test) AXIS-CHILD-PNK))
               (dscm:node-children schema)))
      ((eq? node-test-type 'node-test-wildcard-star-ncname)
       (filter (lambda (scm) (dscm:comp-local-type scm (dscm:nt:ncname node-test) AXIS-CHILD-PNK))
               (dscm:node-children schema)))
      ((eq? node-test-type 'node-test-function-call)
       (cl:signal-input-error SE1002 "XPath on Schema: node_test_function_call on axis child"))
      ((eq? node-test-type 'node-test-var-name)
       (cl:signal-input-error SE1002 "XPath on Schema: node_test_var_name on axis child"))
      (else
       (cl:signal-input-error SE1003 "XPath on Schema: unknown node test")))))


;t_scmnodes descendant_or_self_nodes(schema_node *node);
;t_scmnodes descendant_nodes        (schema_node *node);
;
;t_scmnodes execute_node_test_axis_child             (schema_node *node, const NodeTest &nt);
;t_scmnodes execute_node_test_axis_descendant        (schema_node *node, const NodeTest &nt);
;t_scmnodes execute_node_test_axis_attribute         (schema_node *node, const NodeTest &nt);
;t_scmnodes execute_node_test_axis_self              (schema_node *node, const NodeTest &nt);
;t_scmnodes execute_node_test_axis_descendant_or_self(schema_node *node, const NodeTest &nt);
;t_scmnodes execute_node_test_axis_descendant_attr   (schema_node *node, const NodeTest &nt);
;t_scmnodes execute_node_test                        (schema_node *node, const NodeTest &nt);
;
;t_scmnodes execute_abs_path_expr_rec(const t_scmnodes &nodes, const PathExpr &pe);
;t_scmnodes execute_abs_path_expr(schema_node *root, const PathExpr *path_expr);
;



