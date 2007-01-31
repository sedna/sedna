/*
 * File:  att_xpath.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


// !!! Problems:
// 1. Such types of xp_op
//      xp_op_val_eq,
//      xp_op_val_ne,
//      xp_op_val_lt,
//      xp_op_val_gt,
//      xp_op_val_le,
//      xp_op_val_ge
//    are actually unsupported, but can appear...


#include "common/sedna.h"
#include <strstream>
#include <list>
#include <math.h>
#include "tr/sqp/popt/att_xpath.h"
#include "tr/executor/base/XPathOnSchema.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/pstr/pstrblk.h"
#include "common/errdbg/d_printf.h"


/// CPU Factor
#define CPU_FACTOR				0.01


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// SELECTIVITY
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
namespace sel
{

//tsel a1

//tsel s1_to_sm()

//inline tsel parent_transition(schema_node *source)
//{
//    double k = (double)(source->parent->nodecnt) / (double)(source->nodecnt);
//    return S_MIN((double)1, k);
//}

tsel pred(xp_op_type type, const schema_node *scm, const tuple_cell& tc)
{
    double s1;
    switch (type)
    {
        case xp_op_gen_eq:
            {
                s1 = (double)1 / (double)10;
                break;
            }
        case xp_op_gen_ne:
            {
                s1 = (double)9 / (double)10;
                break;
            }
        case xp_op_gen_lt:
        case xp_op_gen_gt:
        case xp_op_gen_le:
        case xp_op_gen_ge:
            {
                s1 = (double)1 / (double)3;
                break;
            }
        default: throw USER_EXCEPTION2(SE1051, "Unexpected case in pred_estimation");
    }

    return s1;
}

void simple_path_pred_const(xp_op_type type, 
                            const schema_node *top, 
                            const schema_node *bottom, 
                            const tuple_cell& tc,
                            tsel /*out*/*sel_top,
                            tsel /*out*/*sel_bottom)
{
    *sel_bottom = pred(type, bottom, tc);
    double a1 = (double)1 - *sel_bottom;

    // am = a1^(n1/nm);  sm = 1 - am;
    *sel_top = (double)1 - power(a1, (double)(bottom->nodecnt) / (double)(top->nodecnt));
}

void path_pred_const(const t_scmnodes &nodes, xp_pred /*in/out*/ *pred)
{
    if (nodes.size() == 1) 
    {
        pred->scm_bottom = (schema_node*)(nodes[0]);
        simple_path_pred_const(pred->type, 
                               pred->scm_middle, 
                               pred->scm_bottom, 
                               pred->tc,
                               &(pred->sel_middle),
                               &(pred->sel_bottom));
    }
    else POPT_FAILED_TO_OPTIMIZE;/// !!! not implemented
}

void boolean_factor(xp_pred /*in/out*/ *pred)
{
    switch (pred->pred_op->type)
    {
        case xp_op_or: 
            POPT_FAILED_TO_OPTIMIZE;/// !!! not implemented
        case xp_op_gen_eq:
        case xp_op_gen_ne:
        case xp_op_gen_lt:
        case xp_op_gen_gt:
        case xp_op_gen_le:
        case xp_op_gen_ge:
            {
                if (   pred->pred_op->op1->type != xp_op_path 
                    || pred->pred_op->op2->type != xp_op_const
                    || pred->pred_op->op1->op1) 
                    throw USER_EXCEPTION2(SE1051, "Not a well formed predicate");

                pred->type = pred->pred_op->type;
                pred->tc = *(pred->pred_op->op2->tc);

                t_scmnodes nodes = execute_abs_path_expr(pred->scm_middle, pred->pred_op->op1->path_expr); /// !!! optimize
                path_pred_const(nodes, pred);
                break;
            }
        default: throw USER_EXCEPTION2(SE1051, "Unexpected case in boolean_factor");
    }
}



} /// sel
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// COST ESTIMATION
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
namespace cost /// cost estimation
{

tcost blocks_simple(double m/*number of blocks*/, 
                    double n/*number of records*/,
                    double k/*number of satisfying records*/)
{
    return k / n * m;
}

tcost blocks_cardenas(double m/*number of blocks*/, 
                      double n/*number of records*/,
                      double k/*number of satisfying records*/)
{
    if (is_zero(k)) return 0.0;
    return m * (1.0 - power((1.0 - 1.0 / m), k));
}

tcost blocks_yao_approx_upper(double m/*number of blocks*/, 
                              double n/*number of records*/,
                              double k/*number of satisfying records*/)
{
    double B = n / m;
    return (n / B) * (1.0 - power((1.0 - B / n) * (1.0 - B / (n - k + 1)), k / 2.0));
}

tcost blocks_yao_approx_lower(double m/*number of blocks*/, 
                              double n/*number of records*/,
                              double k/*number of satisfying records*/)
{
    double B = n / m;
    return (n / B) * (1.0 - power(1.0 - B / (n - (k - 1) / 2), k));
}

tcost blocks(double m/*number of blocks*/, 
             double n/*number of records*/,
             double k/*number of satisfying records*/)
{
    return blocks_cardenas(m, n, k);
}

tcost pred_scan_bottom_level(const schema_node *scm, double k)
{
    double n = (double)(scm->nodecnt);

    tcost dsc_blocks = blocks((double)(scm->blockcnt), n, k);

    double str_m = ((double)(scm->textcnt) + sizeof(shft) * (double)(scm->nodecnt)) / 
                       (PAGE_SIZE - (sizeof(vmm_sm_blk_hdr) + 6 * sizeof(shft) + HHMAXSIZE * sizeof(hh_slot)));
    str_m = ceil(str_m);
    double str_blocks = blocks(str_m, n, k);

    return (dsc_blocks + str_blocks + CPU_FACTOR * k);
}

tcost pred_scan(xp_op_type type, 
                const schema_node *scm, 
                double k)
{
    if (scm->type == text || scm->type == attribute)
    {
        return pred_scan_bottom_level(scm, k);
    }
    else if (scm->type == element)
    {
        if (scm->first_child == scm->last_child && 
            (scm->first_child->snode->type == text || scm->first_child->snode->type == attribute))
        {
            tcost txt_cost = pred_scan_bottom_level(scm->first_child->snode, k);
            tcost elem_cost = blocks((double)(scm->blockcnt),
                                     (double)(scm->nodecnt),
                                     k) + 
                              CPU_FACTOR * k;
            return txt_cost + elem_cost;
        }
        else POPT_FAILED_TO_OPTIMIZE;
    }
    else POPT_FAILED_TO_OPTIMIZE;
}

tcost top_down_simple_path_pred_const(xp_op_type type, 
                                      const schema_node *top, 
                                      tsel upper_s,
                                      const schema_node *bottom, 
                                      const tuple_cell& tc)
{
    double s1_star = upper_s;
    double sm = sel::pred(type, bottom, tc);
    double n1 = (double)(top->nodecnt);
    double nm = (double)(bottom->nodecnt);

    double R = 0.0;
    double Am = 1.0 - sm;
    double Am_star = power(1.0 - s1_star, n1 / nm);

    double nk = nm, nk_1 = (double)(bottom->parent->nodecnt), nk_2 = 0.0;
    double Ak = Am, Ak_1 = power(Ak, nk / nk_1), Ak_2 = 0.0;
    double Ak_1_star = power(Am_star, nk / nk_1);

    const schema_node *cur = bottom;
    tcost up_cost = 0.0, R_bottom = 0.0, step_cost = 0.0;
    bool is_bottom = true;

    while (cur != top)
    {
        nk_2 = double(cur->parent->nodecnt);
        Ak_2 = power(Ak_1, nk_1 / nk_2);
        Ak_1_star = power(Ak_1_star, nk_1 / nk_2);
        Ak = Ak_1;
        Ak_1 = Ak_2;
        nk = nk_1;
        nk_1 = nk_2;

        R = ((1.0 - Ak_1) / (1.0 - Ak)) * nk_1 * (1.0 - Ak_1_star);
        step_cost = blocks((double)(cur->blockcnt), (double)(cur->nodecnt), R) + /// !!! check and optimize
                    CPU_FACTOR * R;

        if (is_bottom) 
        {
            is_bottom = false;
            R_bottom = R;
        }
        else up_cost += step_cost;

        cur = cur->parent;
    }

    tcost pred_cost = pred_scan(type, bottom, R_bottom);

    tcost top_cost = blocks((double)(top->blockcnt), n1, n1 * upper_s) +
                     CPU_FACTOR * n1 * upper_s;

    return pred_cost + up_cost + top_cost;
}

tcost top_down_path_pred_const(xp_op_type type, 
                               const schema_node *top, 
                               tsel upper_s,
                               const t_scmnodes_const &tested,
                               const tuple_cell& tc)
{
    if (tested.size() == 1) 
        return top_down_simple_path_pred_const(type, top, upper_s, tested[0], tc);
    else POPT_FAILED_TO_OPTIMIZE;/// !!! not implemented
}

tcost top_down_boolean_factor(const schema_node *top, tsel upper_s, const xp_op *op)
{
    switch (op->type)
    {
        case xp_op_or: 
            POPT_FAILED_TO_OPTIMIZE;/// !!! not implemented
        case xp_op_gen_eq:
        case xp_op_gen_ne:
        case xp_op_gen_lt:
        case xp_op_gen_gt:
        case xp_op_gen_le:
        case xp_op_gen_ge:
            {
                if (   op->op1->type != xp_op_path 
                    || op->op2->type != xp_op_const
                    || op->op1->op1) 
                    throw USER_EXCEPTION2(SE1051, "Not a well formed predicate");

                t_scmnodes_const tested = execute_abs_path_expr(top, op->op1->path_expr); /// !!! optimize
                return top_down_path_pred_const(op->type, top, upper_s, tested, *(op->op2->tc));
            }
        default: throw USER_EXCEPTION2(SE1051, "Unexpected case in top_down_boolean_factor");
    }
}

tcost ad_filter_extnids(double extnids)
{
    if (!is_zero(extnids))
    {
        return ceil(extnids / (double)(PAGE_SIZE / 16));
    }

    return 0.0;
}

tcost ad_filter_simple_path_pred_const(xp_op_type type, 
                                      const schema_node *bottom, 
                                      const tuple_cell& tc)
{
    tcost bottom_cost = pred_scan(type, bottom, (double)(bottom->nodecnt));
    bottom_cost += ad_filter_extnids((double)(bottom->extnids));

    return bottom_cost;
}

tcost ad_filter_path_pred_const(xp_op_type type, 
                               const schema_node *top, 
                               const t_scmnodes_const &tested,
                               const tuple_cell& tc)
{
    if (tested.size() == 1) 
        return ad_filter_simple_path_pred_const(type, tested[0], tc);
    else POPT_FAILED_TO_OPTIMIZE;/// !!! not implemented
}

tcost ad_filter_boolean_factor(const schema_node *top, const xp_op *op)
{
    switch (op->type)
    {
        case xp_op_or: 
            POPT_FAILED_TO_OPTIMIZE;/// !!! not implemented
        case xp_op_gen_eq:
        case xp_op_gen_ne:
        case xp_op_gen_lt:
        case xp_op_gen_gt:
        case xp_op_gen_le:
        case xp_op_gen_ge:
            {
                if (   op->op1->type != xp_op_path 
                    || op->op2->type != xp_op_const
                    || op->op1->op1) 
                    throw USER_EXCEPTION2(SE1051, "Not a well formed predicate");

                t_scmnodes_const tested = execute_abs_path_expr(top, op->op1->path_expr); /// !!! optimize
                return ad_filter_path_pred_const(op->type, top, tested, *(op->op2->tc));
            }
        default: throw USER_EXCEPTION2(SE1051, "Unexpected case in ad_filter_boolean_factor");
    }
}

tcost ad_filter_boolean_factors(const schema_node *top, const xp_pred *preds, int j, int pl, int pr)
{
    if (j == pl) return 0.0;

    int k = 0;
    tcost bottoms_cost = 0.0;
    for (k = pl; k < j; k++) bottoms_cost += ad_filter_boolean_factor(top, preds[k].pred_op);

    tcost top_cost = (double)(top->blockcnt) + CPU_FACTOR * (double)(top->nodecnt);
    top_cost += ad_filter_extnids((double)(top->extnids));

    return bottoms_cost + top_cost;
}

tcost bottom_up_simple_path_pred_const(xp_op_type type, 
                                       const schema_node *top, 
                                       const schema_node *bottom, 
                                       const tuple_cell& tc)
{
    double sm = sel::pred(type, bottom, tc);
    double am = 1.0 - sm;

    double n1 = (double)(top->nodecnt);
    double nm = (double)(bottom->nodecnt);
    double s1 = 1.0 - power(am, nm / n1);

    double R = 0.0;

    double nk = nm, nk_1 = (double)(bottom->parent->nodecnt), nk_2 = 0.0;
    double Ak = am, Ak_1 = 0.0;

    const schema_node *cur = bottom;
    tcost up_cost = 0.0, R_bottom = 0.0, step_cost = 0.0;
    bool is_bottom = true;

    while (cur != top)
    {
        nk_2 = double(cur->parent->nodecnt);
        Ak_1 = power(Ak, nk / nk_1);

        nk = nk_1;
        nk_1 = nk_2;
        Ak = Ak_1;

        R = (1.0 - Ak) * nk;

        step_cost = blocks((double)(cur->blockcnt), nk, R) +
                    blocks((double)(cur->indir_blk_cnt), nk, R) +
                    CPU_FACTOR * R;

        if (is_bottom) 
        {
            is_bottom = false;
            R_bottom = R;
        }
        else up_cost += step_cost;

        cur = cur->parent;
    }

    tcost pred_cost = pred_scan(type, bottom, nm) +
                      blocks((double)(bottom->indir_blk_cnt), nm, R_bottom);
    
    tcost top_cost = blocks((double)(top->blockcnt), n1, n1 * s1) +
                     CPU_FACTOR * n1 * s1;

    return pred_cost + up_cost + top_cost;
}

tcost bottom_up_path_pred_const(xp_op_type type, 
                                const schema_node *top, 
                                const t_scmnodes_const &tested,
                                const tuple_cell& tc)
{
    if (tested.size() == 1) 
        return bottom_up_simple_path_pred_const(type, top, tested[0], tc);
    else POPT_FAILED_TO_OPTIMIZE;/// !!! not implemented
}

tcost bottom_up_boolean_factor(const schema_node *top, const xp_op *op)
{
    switch (op->type)
    {
        case xp_op_or: 
            POPT_FAILED_TO_OPTIMIZE;/// !!! not implemented
        case xp_op_gen_eq:
        case xp_op_gen_ne:
        case xp_op_gen_lt:
        case xp_op_gen_gt:
        case xp_op_gen_le:
        case xp_op_gen_ge:
            {
                if (   op->op1->type != xp_op_path 
                    || op->op2->type != xp_op_const
                    || op->op1->op1) 
                    throw USER_EXCEPTION2(SE1051, "Not a well formed predicate");

                t_scmnodes_const tested = execute_abs_path_expr(top, op->op1->path_expr); /// !!! optimize
                return bottom_up_path_pred_const(op->type, top, tested, *(op->op2->tc));
            }
        default: throw USER_EXCEPTION2(SE1051, "Unexpected case in bottom_up_boolean_factor");
    }
}

tcost bottom_up_boolean_factors(const schema_node *top, const xp_pred *preds, int j, int pl, int pr)
{
    if (j == pl) return 0.0;

    int k = 0;
    tcost ups_cost = 0.0;
    for (k = pl; k < j; k++) ups_cost += bottom_up_boolean_factor(top, preds[k].pred_op);

    tcost top_cost = (double)(top->blockcnt) + CPU_FACTOR * (double)(top->nodecnt);
    top_cost += ad_filter_extnids((double)(top->extnids));

    return ups_cost + top_cost;
}


} /// cost
///////////////////////////////////////////////////////////////////////////////


struct uncovered_xp
{
    schema_node *scm;
    xp_op *op;
    counted_ptr<uncovered_xp> lower;
    schema_node *root;
};

typedef std::list< counted_ptr<uncovered_xp> > uncovered_xp_list;


/* Uncover query trees:
   ~~~~~~~~~~~~~~~~~~~~
   Because every path expression possibly evaluate into several desriptive
   schema nodes we have to uncover complex predicate (with more that on level)
   into several xp_tree's.

                    Tree of xp_op's
                    ---------------
                        S3
                         xp_op_pred
                            /   \
                           /     \
                      xp_op_path xp_and
                         /       |  |  |
                  S2    /        P5 P6 P7
                   xp_op_pred
                      /    \
                     /      \
                xp_op_path xp_and
                   /        |  |
            S1    /         P3 P4
             xp_op_pred
                /    \
               /      \
          xp_op_path xp_and
                      |  |
                      P1 P2

                  ____________________
                 |_________  _________|
                           ||
                          _||_
                          \  /
                           \/

                    Several xp_tree's
                    -----------------
         ++ S1(1)                        ++ S1(n)               level 0
         ++                              ++
        /| \                            /| \
       / |  \                          / |  \
      P1 P2  \                        P1 P2  \
              ++ S2(1)                        ++ S2(n)          level 1
              ++                              ++
             /| \                            /| \
            / |  \             ...          / |  \ 
           P3 P4  \                        P3 P4  \
                   ++ S3(1)                        ++ S3(n)     level 2
                   ++                              ++
                  /|\                             /|\
                 / | \                           / | \
                P5 P6 P7                        P5 P6 P7

*/

uncovered_xp_list popt_uncover_query_trees(counted_ptr<db_entity> db_ent, xp_op *op)
{
    if (op->type != xp_op_pred || op->op1->type != xp_op_path || op->op2->type != xp_op_and)
        POPT_FAILED_TO_OPTIMIZE;

    uncovered_xp_list res;
    if (op->op1->op1)
    {
        uncovered_xp_list src = popt_uncover_query_trees(db_ent, op->op1->op1);

        for (uncovered_xp_list::iterator it = src.begin(); it != src.end(); it++)
        {
            t_scmnodes nodes = execute_abs_path_expr((*it)->scm, op->op1->path_expr); /// !!! optimize

            for (int i = 0; i < nodes.size(); i++)
            {
                counted_ptr<uncovered_xp> tmp(new uncovered_xp);
                tmp->scm = nodes[i];
                tmp->op = op;
                tmp->lower = *it;
                tmp->root = (*it)->root;
                res.push_back(tmp);
            }
        }
    }
    else
    {
        schema_node *root = get_schema_node(db_ent, "Unknown entity passed to popt_uncover_query_trees");
        t_scmnodes nodes = execute_abs_path_expr(root, op->op1->path_expr); /// !!! optimize
        for (int i = 0; i < nodes.size(); i++)
        {
            counted_ptr<uncovered_xp> tmp(new uncovered_xp);
            tmp->scm = nodes[i];
            tmp->op = op;
            tmp->root = root;
            res.push_back(tmp);
        }
    }

    return res;
}

int popt_build_preds(counted_ptr<uncovered_xp> uncovered_tree, xp_tree *tree, int level)
{
    int n = uncovered_tree->op->op2->ops_size;  // number of predicates
    xp_op **ops = uncovered_tree->op->op2->ops; // predicates 0..n-1
    int j = 0;
    int start = 0;


    if (uncovered_tree->lower.get() == NULL)
    {
        start = 0;
    }
    else
    {
        start = popt_build_preds(uncovered_tree->lower, tree, level - 1);
    }

    tree->levels[level].n = n;
    tree->levels[level].preds = counted_ptr<xp_pred>(new xp_pred[n]);
    tree->levels[level].ncname_prefix = uncovered_tree->op->ncname_prefix;
    tree->levels[level].ncname_local = uncovered_tree->op->ncname_local;

    for (j = 0; j < n; j++)
    {
        xp_pred *pred    = &(tree->levels[level].preds[j]);
        pred->id         = j + start;
        pred->scm_middle = uncovered_tree->scm;
        pred->scm_upper  = uncovered_tree->lower.get() ? uncovered_tree->lower->scm : uncovered_tree->root;
        pred->pred_op    = ops[j];
        pred->level      = level;
        // The following fields
        // * scm_bottom
        // * sel_bottom
        // * sel_middle
        // * type
        // * tc
        // are filled by sel::boolean_factor
        sel::boolean_factor(pred);
    }

    return start + n;
}


/* Table of symbols:
   ~~~~~~~~~~~~~~~~~

   Optimize level (or part of it from pl to pr). 
   Parameter initial_sel means selectivity of top schema node
   (it can be less than 1 because of other predicates)

   Example:
   --------
   Select plan for execution of preds P2, P3 together.

   P0 P1 P2 P3 P4 P5   (level)
         ^     ^
         |     |
         pl    pr
*/

tcost popt_optimize_level(xp_tree *tree, int level, tsel initial_sel, int pl, int pr, xp_pred_strategy *group)
{
/// !!! Rewrite this function to take into account additional plans
    int j = 0, k = 0;

    tcost cost = DBL_MAX;
    schema_node *scm = tree->levels[level].preds[0].scm_middle; // all preds have the same scm
    xp_pred *preds = tree->levels[level].preds.get();

    tsel cur_sel = initial_sel;

#ifdef POPT_DEBUG
    d_printf1("\n*** Level optimization ***\n");
#endif

    for (j = pl; j <= pr; j++)
    {
        // cost(merge(0..j-1)) + ...
        tcost merge_cost = cost::ad_filter_boolean_factors(scm, preds, j, pl, pr);

        // cost(parent(0..j-1)) + ...
        tcost bottom_up_cost = cost::bottom_up_boolean_factors(scm, preds, j, pl, pr);


        for (k = pl; k < j; k++) cur_sel *= preds[k].sel_middle;

        // ... + cost(straightforward(j..n-1))
        tcost top_down_cost = 0.0;
        for (k = j; k < pr; k++)
        {
            top_down_cost += cost::top_down_boolean_factor(scm, cur_sel, preds[k].pred_op);
            cur_sel *= preds[k].sel_middle;
        }

        tcost ad_filter_cost_revised = merge_cost + top_down_cost;
        tcost bottom_up_cost_revised = bottom_up_cost + top_down_cost;

#ifdef POPT_DEBUG
        counted_ptr<xp_pred_strategy> group1(new xp_pred_strategy[pr - pl]), group2(new xp_pred_strategy[pr - pl]);
        /// group1
        for (k = pl; k < j; k++)
        {
            group1[k - pl].es = xpes_ad_filter;
            group1[k - pl].pred = preds + k;
        }

        for (; k < pr; k++)
        {
            group1[k - pl].es = xpes_top_down;
            group1[k - pl].pred = preds + k;
        }

        /// group2
        for (k = pl; k < j; k++)
        {
            group2[k - pl].es = xpes_bottom_up;
            group2[k - pl].pred = preds + k;
        }

        for (; k < pr; k++)
        {
            group2[k - pl].es = xpes_top_down;
            group2[k - pl].pred = preds + k;
        }

        if (pl == j)
        {
            print(group1.get(), pr - pl, ad_filter_cost_revised);
        }
        else
        {
            print(group1.get(), pr - pl, ad_filter_cost_revised);
            print(group2.get(), pr - pl, bottom_up_cost_revised);
        }
#endif

        if (cost > s_min(ad_filter_cost_revised, bottom_up_cost_revised))
        {
            for (k = pl; k < j; k++)
            {
                group[k - pl].es = (ad_filter_cost_revised < bottom_up_cost_revised) ? 
                                   xpes_ad_filter : xpes_bottom_up;
                group[k - pl].pred = preds + k;
            }

            for (; k < pr; k++)
            {
                group[k - pl].es = xpes_top_down;
                group[k - pl].pred = preds + k;
            }

            cost = s_min(ad_filter_cost_revised, bottom_up_cost_revised);
        }
    }

#ifdef POPT_DEBUG
    d_printf1("**************************\n");
#endif

    return cost;
}

void popt_optimize_tree(xp_tree *tree)
{
/// !!! Rewrite this function to take into account additional plans
    int i = 0, j = 0;

    for (i = 0; i < tree->n; i++)
        qsort(tree->levels[i].preds.get(), tree->levels[i].n, sizeof(xp_pred), xp_pred_compare);

    counted_ptr<xp_tree_strategy> strategy;
    tcost cost = 0.0;
    for (i = 0; i < tree->n; i++)
    {
        if (strategy.get())
        {
            counted_ptr<xp_tree_strategy> tmp_strategy(new xp_tree_strategy);
            tmp_strategy->ts = xpts_down_filter;
            tmp_strategy->n = 0;
            tmp_strategy->left = strategy;
            strategy->up = tmp_strategy;
            strategy = tmp_strategy;
        }

        counted_ptr<xp_tree_strategy> tmp_strategy(new xp_tree_strategy);
        tmp_strategy->ts = xpts_group;
        tmp_strategy->n = tree->levels[i].n;
        tmp_strategy->left = strategy;
        tmp_strategy->group = counted_ptr<xp_pred_strategy>(new xp_pred_strategy[tmp_strategy->n]);
        if (strategy.get()) strategy->up = tmp_strategy;
        strategy = tmp_strategy;

        cost += popt_optimize_level(tree, i, 1.0, 0, tree->levels[i].n, strategy->group.get());
        /// !!! cost += <cost of xpts_ad_filter_group>
    }

    tree->strategy = strategy;
    tree->cost = cost;
}

xpath_popt_plan popt_optimize_att_xpath_pred(counted_ptr<db_entity> db_ent, xp_op *op, int levels)
{
    uncovered_xp_list uncovered_trees = popt_uncover_query_trees(db_ent, op);
    int n = uncovered_trees.size();
    if (n == 0) POPT_EOS;

    counted_ptr<xp_tree> trees(new xp_tree[n]);

    int i = 0;
    for (uncovered_xp_list::iterator it = uncovered_trees.begin(); 
         it != uncovered_trees.end(); 
         ++i, ++it)
    {
        trees[i].cost = 0.0;
        trees[i].n = levels;
        trees[i].levels = counted_ptr<xp_level>(new xp_level[levels]);
        trees[i].pn = popt_build_preds(*it, &(trees[i]), levels - 1);

        popt_optimize_tree(&(trees[i]));
    }

    xpath_popt_plan res;
    res.n = n;
    res.trees = trees;
    res.db_ent = db_ent;

#ifdef POPT_DEBUG
    d_printf1("\n");
    d_printf1("++++++++++ BEST ++++++++++\n");
    res.print();
    d_printf1("++++++++++ BEST ++++++++++\n");
#endif

    return res;
}

popt_plan popt_att_xpath(xpath_attr *xpath)
{
    popt_plan plan;
    plan.empty_sequence = false;
    plan.failed_to_optimize = false;

    try {
        // !!! the root operation of a tree should be a pred
        if (xpath->op->type != xp_op_pred) POPT_FAILED_TO_OPTIMIZE;

        plan.xp_plan = popt_optimize_att_xpath_pred(xpath->db_ent, xpath->op, xpath->xp_preds_num);

    } catch (POPTFailedToOptimize &e) {
        plan.failed_to_optimize = true;
    } catch (POPTEndOfSequence &e) {
        plan.empty_sequence = true;
    }

    return plan;
}
