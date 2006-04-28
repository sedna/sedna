/*
 * File:  att.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _ATT_H
#define _ATT_H

#include "sedna.h"
#include "XPath.h"
#include "PPBase.h"
#include "scheme_tree.h"

/// Prints lots of debugging information if defined
#define POPT_DEBUG

/*
;===============================================================================
; Attributed tree (ATT)
;===============================================================================
; ATT ::= (att-attr att-value)
;
; att-attr ::= xpath-attr
;            | xpath-pred-attr
; att-value is an LR operation
;
; === XPath attribute ===
; xpath-attr ::= ('xpath (xp-entity xp-ent-name) xp-preds-num xp-op)
; xp-entity ::= 'collection
;             | 'document
; xp-ent-name ::= "string"
; xp-preds-num ::= int
;
; === XPath predicate attribute ===
; xpath-pred-attr ::= ('xpath-pred var xp-op)
;
; === XPath operation ===
; xp-op ::= ()
;         | ('path abs-path xp-op)
;         | ('pred xp-preds-num var xp-op xp-op)
;         | ('and@ xp-op*)
;         | ('or@ xp-op*)
;         | ('const ...)   ; like in LR
;         | ('=@  xp-op xp-op)
;         | ('!=@ xp-op xp-op)
;         | ('<@  xp-op xp-op)
;         | ('>@  xp-op xp-op)
;         | ('<=@ xp-op xp-op)
;         | ('>=@ xp-op xp-op)
;         | ('eq@ xp-op xp-op)
;         | ('ne@ xp-op xp-op)
;         | ('lt@ xp-op xp-op)
;         | ('le@ xp-op xp-op)
;         | ('gt@ xp-op xp-op)
;         | ('ge@ xp-op xp-op)
;
; abs-path ::= (node-test-or . . . node-test-or)
; node-test-or ::= (node-test . . . node-test)
; node-test ::= (axis-op node-test-type node-test-data)
;
; axis-op ::= PPAxisChild
;           | PPAxisAttribute
;           | PPAxisParent
;           | PPAxisSelf
;           | PPAxisDescendant
;           | PPAxisDescendantOrSelf
;           | PPAxisDescendantAttr
; node-test-type ::= processing_instruction
;                  | comment
;                  | text
;                  | node
;                  | string
;                  | qname
;                  | wildcard_star
;                  | wildcard_ncname_star
;                  | wildcard_star_ncname
;                  | function_call
;                  | var_name
;
*/

struct xpath_attr;
struct xp_op;

enum att_attr_type
{
    att_xpath
};

struct att_attr
{
    att_attr_type type;
    xpath_attr *xpath;
};

struct xpath_attr
{
    counted_ptr<db_entity> db_ent;
    int xp_preds_num;
    xp_op *op;
};

enum xp_op_type 
{
      xp_op_path,
      xp_op_pred,
      xp_op_and,
      xp_op_or,
      xp_op_const,
      xp_op_gen_eq,
      xp_op_gen_ne,
      xp_op_gen_lt,
      xp_op_gen_gt,
      xp_op_gen_le,
      xp_op_gen_ge,
      xp_op_val_eq,
      xp_op_val_ne,
      xp_op_val_lt,
      xp_op_val_gt,
      xp_op_val_le,
      xp_op_val_ge
};

struct xp_op
{
    xp_op_type type;
    int pred_num;
    QName qname; // variable name for predicate
    PathExpr *path_expr;
    xp_op *op1; // for path, pred, general and value comparison
    xp_op *op2; // for pred, general and value comparison
    tuple_cell *tc; // for const
    xp_op **ops; // for and@, or@
    int ops_size; // for and@, or@

    void print();
};



att_attr *make_att_attr(scheme_list *lst);
void delete_att_attr(att_attr *att);


#endif

