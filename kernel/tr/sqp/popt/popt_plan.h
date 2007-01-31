/*
 * File: popt_plan.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _POPT_PLAN_H
#define _POPT_PLAN_H

#include "common/sedna.h"
#include "tr/sqp/popt/att.h"
#include "tr/executor/base/XPathOnSchema.h"


#define POPT_EOS					throw POPTEndOfSequence()
#define POPT_FAILED_TO_OPTIMIZE		throw POPTFailedToOptimize()


class POPTEndOfSequence {};
class POPTFailedToOptimize {};


/// selectivity type
typedef double tsel;
/// cost type
typedef double tcost;



///////////////////////////////////////////////////////////////////////////////
/// XPath predicate
///////////////////////////////////////////////////////////////////////////////
struct xp_pred
{
    int id;

    schema_node *scm_bottom; // bottom schema node
    schema_node *scm_middle; // middle schema node (predicate is applied to this node)
    schema_node *scm_upper;  // one level up schema node

    tsel sel_bottom;         // selectivity of bottom schema node (according to predicate)
    tsel sel_middle;         // selectivity of middle schema node (according to predicate)

    xp_op_type type;         // comparison type
    tuple_cell tc;           // the const to be compared with nodes

    xp_op *pred_op;          // predicate operation
    int level;               // level
};

int xp_pred_compare(const void *p1, const void *p2);

/* Table of symbols:
   ~~~~~~~~~~~~~~~~~

   xp_tree:

         ++ S1         For P3(predicate): scm_bottom -- P3
         ++                               scm_middle -- S2
        /| \                              scm_top    -- S1
       / |  \                             scm_sought -- S3
      P1 P2  \                            scm_upper  -- S1
              ++ S2
              ++                          sel_bottom -- Selectivity of P3
             /| \                         sel_middle -- Selectivity of S2 with respect to P3
            / |  \                        sel_top    -- Can be calculated with boolean_factor formula
xp_level:  P3 P4  \                       sel_sought -- Equals to sel_middle
                   ++ S3
                   ++
                  /|\
                 / | \
                P5 P6 P7

*/

///////////////////////////////////////////////////////////////////////////////
/// XPath predicate level
///////////////////////////////////////////////////////////////////////////////
struct xp_level
{
    int n;
    counted_ptr<xp_pred> preds;
    char *ncname_prefix;
    char *ncname_local;
};

///////////////////////////////////////////////////////////////////////////////
/// XPath predicate tree
///////////////////////////////////////////////////////////////////////////////

/* Filters:
   ~~~~~~~~
   ad:  -------- (param1) - upper level   ==> output

        -------- (param2) - lower level


   da:  -------- (param1) - upper level   

        -------- (param2) - lower level   ==> output

*/

// predicate evaluation methods
enum xp_eval_strategy
{
    xpes_top_down,
    xpes_bottom_up,
    xpes_ad_filter
};

// execution strategy for a predicate
struct xp_pred_strategy
{
    xp_pred *pred;          // predicate
    xp_eval_strategy es;    // evaluation method for the predicate
};

void print(xp_pred_strategy *group, int n, tcost cost);

// transition strategies including group of predicates
enum xp_trans_strategy
{
    xpts_group,
    xpts_da_filter,
    xpts_down,
    xpts_down_filter,
    xpts_up,
    xpts_up_filter
};

// execution strategy for a tree
struct xp_tree_strategy
{
    xp_trans_strategy ts;
    counted_ptr<xp_tree_strategy> up;
    counted_ptr<xp_tree_strategy> left;
    counted_ptr<xp_tree_strategy> right;
    int n;
    counted_ptr<xp_pred_strategy> group;

    /*
      var is used for xpts_group, xpts_down and xpts_down_filter only.
      For xpts_group var.idx not equal to 0 says that the intermediate
      results should be saved into the variable with index var.idx.
      For xpts_down and xpts_down_filter var.idx.n says with how many
      variables (which contain intermediate results) the current intermediate
      result should be mixed.
    */
    union
    {
        int idx;
        struct
        {
            int n;
            int *idxs;
        } idxs;
    } var;

    // to_level is valid only for xpts_down and only if xpts_down is a
    // last transition in the strategy
    int to_level;
};


/* Table of symbols:
   ~~~~~~~~~~~~~~~~~

                   pointer to strategy
                          ||
                          \/
                       +-------+
                       | group |
                       +-------+
                    prev/     \
                       /next   \
                 +-------+   +-------+
                 | down  |   |   |   |
                 +-------+  xp_pred_strategy (n)
              prev/
                 /next
           +-------+
           | group |
           +-------+
        prev/     \
           /       \
          NULL   +-------+
                 |   |   |
             xp_pred_strategy (n)
*/              


struct xp_tree
{
    tcost cost;                               // cost of the plan

    int n;                                    // number of levels
    int pn;                                   // total preds number
    counted_ptr<xp_level> levels;             // tree levels
    counted_ptr<xp_tree_strategy> strategy;   // execution strategy

    void rec_print(counted_ptr<xp_tree_strategy> cur, int indent);
    void print();
};

///////////////////////////////////////////////////////////////////////////////
/// XPath popt plan
///////////////////////////////////////////////////////////////////////////////
struct xpath_popt_plan
{
    int n;                         // number of trees
    counted_ptr<xp_tree> trees;    // optimized trees
    counted_ptr<db_entity> db_ent; // db entity

    void print();
};



///////////////////////////////////////////////////////////////////////////////
/// popt plan
///////////////////////////////////////////////////////////////////////////////
struct popt_plan
{
    xpath_popt_plan xp_plan;
    bool empty_sequence;
    bool failed_to_optimize;

    void print();
};


#endif
