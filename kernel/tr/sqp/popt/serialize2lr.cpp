/*
 * File:  serialize2lr.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "serialize2lr.h"
#include "d_printf.h"

enum xp_op_path_end_type
{
    xp_et_strategy,
    xp_et_var
};

void serialize2lr_xp_pred_trans(std::ostream &s, 
                                counted_ptr<db_entity> db_ent, 
                                counted_ptr<xp_tree_strategy> strategy,
                                const xp_tree &tree);


void determine_transition_levels(counted_ptr<xp_tree_strategy> strategy, 
                                 int /*out*/ &from_level, 
                                 int /*out*/ &to_level)
{
    if (strategy->ts == xpts_down || strategy->ts == xpts_down_filter) ;
    else throw USER_EXCEPTION2(SE1051, "Wrong trans_strategy passed to determine_transition_levels");

    from_level = strategy->left->group[0].pred->level;

    if (strategy->up.get() != 0)
        to_level = strategy->up->group[0].pred->level;
    else
    {
        if (strategy->ts == xpts_down)
            to_level = strategy->to_level;
        else
            throw USER_EXCEPTION2(SE1051, "Error in physical plan");
    }
}

schema_node *level2scm_middle(int level, const xp_tree &tree)
{
    return tree.levels[level].preds[0].scm_middle;
}

void serialize2lr_xp_op_type(std::ostream &s, xp_op_type type)
{
    switch (type)
    {
        case xp_op_gen_eq: s << "=@"; break;
        case xp_op_gen_ne: s << "!=@"; break;
        case xp_op_gen_lt: s << "<@"; break;
        case xp_op_gen_gt: s << ">@"; break;
        case xp_op_gen_le: s << "<=@"; break;
        case xp_op_gen_ge: s << ">=@"; break;
// !!!
//        case xp_op_val_eq:
//        case xp_op_val_ne:
//        case xp_op_val_lt:
//        case xp_op_val_gt:
//        case xp_op_val_le:
//        case xp_op_val_ge:
//
        default: s << "UNKNOWN";
    }
}

void serialize2lr_tuple_cell(std::ostream &s, const tuple_cell &tc)
{
    if (tc.is_light_atomic())
    {
        s << "(const (type ";
        //(const (type !xs!integer) 153)
        switch (tc.get_atomic_type())
        {
            case xs_untypedAtomic		: s << "!xs!untypedAtomic) \"" << tc.get_str_mem() << "\""; break;
            case xs_dateTime			: s << "!xs!dateTime) ()"; break;
            case xs_date				: s << "!xs!date) ()"; break;
            case xs_time				: s << "!xs!time) ()"; break;
            case xs_duration			: s << "!xs!duration) ()"; break;
            case xs_yearMonthDuration	: s << "!xs!yearMonthDuration) ()"; break;
            case xs_dayTimeDuration		: s << "!xs!dayTimeDuration) ()"; break;
            case xs_float				: s << "!xs!float) " << tc.get_xs_float(); break;
            case xs_double				: s << "!xs!double) " << tc.get_xs_double(); break;
            case xs_string				: s << "!xs!string) \"" << tc.get_str_mem() << "\""; break;
            case xs_normalizedString	: s << "!xs!normalizedString) \"" << tc.get_str_mem() << "\""; break;
            case xs_token				: s << "!xs!token) ()"; break;
            case xs_language			: s << "!xs!language) ()"; break;
            case xs_NMTOKEN				: s << "!xs!NMTOKEN) ()"; break;
            case xs_Name				: s << "!xs!Name) \"" << tc.get_str_mem() << "\""; break;
            case xs_NCName				: s << "!xs!NCName) \"" << tc.get_str_mem() << "\""; break;
            case xs_ID					: s << "!xs!ID) ()"; break;
            case xs_IDREF				: s << "!xs!IDREF) ()"; break;
            case xs_ENTITY				: s << "!xs!ENTITY) ()"; break;
            case xs_decimal				: s << "!xs!decimal) ()"; break;
            case xs_integer				: s << "!xs!integer) " << tc.get_xs_integer(); break;
            case xs_gYearMonth			: s << "!xs!gYearMonth) ()"; break;
            case xs_gYear				: s << "!xs!gYear) ()"; break;
            case xs_gMonthDay			: s << "!xs!gMonthDay) ()"; break;
            case xs_gDay				: s << "!xs!gDay) ()"; break;
            case xs_gMonth				: s << "!xs!gMonth) ()"; break;
            case xs_boolean				: s << "!xs!boolean) " << (tc.get_xs_boolean() ? "true" : "false"); break;
            case xs_base64Binary		: s << "!xs!base64Binary) ()"; break;
            case xs_hexBinary			: s << "!xs!hexBinary) ()"; break;
            case xs_anyURI				: s << "!xs!anyURI) \"" << tc.get_str_mem() << "\""; break;
            case xs_QName				: s << "!xs!QName) \"" << tc.get_str_mem() << "\""; break;
            case xs_NOTATION			: s << "!xs!NOTATION) ()"; break;
            default						: s << "UNKNOWN";
        }
        s << ")";
    }
    else s << "UNKNOWN";
}

void serialize2lr_QName(std::ostream &s, const QName &qname)
{
    // (var ("" "$v"))
    s << "(var (\"";
    s << qname.Prefix.n;
    s << "\" \"";
    s << qname.LocalPart.n;
    s << "\"))";
}

void serialize2lr_save_var(std::ostream &s, int idx)
{
    QName save_var;
    char save_var_buf[64];

    sprintf(save_var_buf, "save_var_%d", idx);
    save_var.Prefix.n = NULL;
    save_var.LocalPart.n = save_var_buf;

    serialize2lr_QName(s, save_var);
}

void serialize2lr_db_entity(std::ostream &s, counted_ptr<db_entity> db_ent)
{
    switch (db_ent->type)
    {
        case dbe_document  : s << "(!fn!document (const (type !xs!string) "; break;
        case dbe_collection: s << "(!fn!collection (const (type !xs!string) "; break;
    }

    s << "\"" << db_ent->name << "\"))";
}

void serialize2lr_xp_op_path_nto(std::ostream &s, 
                                 NodeTestOr *nto, 
                                 int i,
                                 xp_op_path_end_type end_type,
                                 counted_ptr<db_entity> db_ent, 
                                 counted_ptr<xp_tree_strategy> strategy,
                                 const QName &var,
                                 const xp_tree &tree)
{
    if (i == -1)
    {
        switch (end_type)
        {
            case xp_et_strategy: if (strategy.get() == NULL) 
                                     serialize2lr_db_entity(s, db_ent);
                                 else
                                     serialize2lr_xp_pred_trans(s, db_ent, strategy, tree);
                                 break;
            case xp_et_var     : serialize2lr_QName(s, var);
                                 break;
            default            : s << "UNKNOWN";
        }
    }
    else
    {   // !!! I assume that nto[i].s is equal to 1
        bool is_attr_test = false;

        switch (nto[i].nt[0].axis)
        {
            case axis_child             : s << "(child "; break;
            case axis_descendant        : s << "(descendant "; break;
            case axis_attribute         : s << "(attribute "; is_attr_test = true; break;
            case axis_self              : s << "(self "; break;
            case axis_descendant_or_self: s << "(descendant-or-self "; break;
            case axis_descendant_attr   : s << "(descendant-attr "; is_attr_test = true; break;
            case axis_parent            : s << "(parent "; break;
            default                     : s << "(UNKNOWN ";
        }

        serialize2lr_xp_op_path_nto(s, nto, i - 1, end_type, db_ent, strategy, var, tree);

        s << " (type (";
        switch (nto[i].nt[0].type)
        {
            case node_test_processing_instruction: s << "pi-test"; break;
            case node_test_comment               : s << "comment-test"; break;
            case node_test_text                  : s << "text-test"; break;
            case node_test_node                  : s << "node-test"; break;
            case node_test_string                : s << "[string]"; break;
            case node_test_qname                 : 
            case node_test_wildcard_star         : 
            case node_test_wildcard_ncname_star  : 
            case node_test_wildcard_star_ncname  : if (is_attr_test) s << "attr-test";
                                                   else s << "elem-test";
                                                   s << " (ename (const (type !xs!QName) ";

                                                   if (nto[i].nt[0].type == node_test_qname)
                                                   {
                                                       s << "(\"" << nto[i].nt[0].data.qname.Prefix.n << "\" \"" 
                                                         << nto[i].nt[0].data.qname.LocalPart.n << "\")";
                                                   }
                                                   else if (nto[i].nt[0].type == node_test_wildcard_star)
                                                   {
                                                       s << "*";
                                                   }
                                                   else if (nto[i].nt[0].type == node_test_wildcard_ncname_star)
                                                   {
                                                       s << "(\"" << nto[i].nt[0].data.ncname.n << "\" *)";
                                                   }
                                                   else // node_test_wildcard_star_ncname
                                                   {
                                                       s << "(* \"" << nto[i].nt[0].data.ncname.n << "\")";
                                                   }

                                                   s << ") (type *) (const (type !xs!string) \"non-nil\")";
                                                   break;
            case node_test_function_call         : s << "[function]"; break;
            case node_test_var_name              : s << "[var]"; break;
            default                              : s << "UNKNOWN";
        }
        s << "))))";
    }
}

void serialize2lr_xp_op_path(std::ostream &s, 
                             const PathExpr *path_expr,
                             xp_op_path_end_type end_type,
                             counted_ptr<db_entity> db_ent, 
                             counted_ptr<xp_tree_strategy> strategy,
                             const QName &var,
                             const xp_tree &tree)
{
    serialize2lr_xp_op_path_nto(s, path_expr->nto, path_expr->s - 1, end_type, db_ent, strategy, var, tree);
/*
(child
      (child
       (child
        (!fn!document (const (type !xs!string) "auction"))
        (type (elem-test (ename (const (type !xs!QName) ("" "site")) (type *) (const (type !xs!string) "non-nil")))))
       (type
        (elem-test (ename (const (type !xs!QName) ("" "open_auctions")) (type *) (const (type !xs!string) "non-nil")))))
      (type (elem-test (ename (const (type !xs!QName) ("" "open_auction")) (type *) (const (type !xs!string) "non-nil")))))
*/
}


void serialize2lr_xp_pred_group(std::ostream &s, 
                                counted_ptr<db_entity> db_ent, 
                                counted_ptr<xp_tree_strategy> strategy,
                                int j, 
                                const QName &var,
                                const xp_tree &tree)
{
    int n = strategy->n;
    xp_pred_strategy *group = strategy->group.get();
    
    if (j == -1)
    {
        if (strategy->left.get())
        {
            serialize2lr_xp_pred_trans(s, db_ent, strategy->left, tree);
        }
        else
        {
            s << "(scan (const (type !xs!integer) ";
            s << (__uint32)(group[0].pred->scm_middle);
            s << ") ";
            serialize2lr_db_entity(s, db_ent);
            s << ")";
        }

        return;
    }



    if (group[j].es == xpes_top_down)
    {
        if (j > 0 && group[j - 1].es == xpes_top_down)
        {
            serialize2lr_xp_pred_group(s, db_ent, strategy, j - 1, var, tree);
        }
        else
        {
            s << "(return ";
            serialize2lr_xp_pred_group(s, db_ent, strategy, j - 1, var, tree);
            s << " (fun-def ((!xs!anyType ";
            serialize2lr_QName(s, var);
            s << ")) (if@ ";
            if (j == n - 1 || group[j + 1].es != xpes_top_down)
            { // do not need and@
                PathExpr *path_expr = build_PathExpr(group[j].pred->scm_middle,
                                                     group[j].pred->scm_bottom);

                s << "(";
                serialize2lr_xp_op_type(s, group[j].pred->type);
                s << " ";
                serialize2lr_xp_op_path(s, path_expr, xp_et_var, db_ent, strategy, var, tree);
                s << " ";
                serialize2lr_tuple_cell(s, group[j].pred->tc);
                s << ") ";

                //!!!delete_PathExpr(path_expr);
            }
            else
            { // need and@
                s << "(and@";
                for (int k = j; k < n && group[k].es == xpes_top_down; k++)
                {
                    s << " ";

                    PathExpr *path_expr = build_PathExpr(group[k].pred->scm_middle,
                                                         group[k].pred->scm_bottom);

                    s << "(";
                    serialize2lr_xp_op_type(s, group[k].pred->type);
                    s << " ";
                    serialize2lr_xp_op_path(s, path_expr, xp_et_var, db_ent, strategy, var, tree);
                    s << " ";
                    serialize2lr_tuple_cell(s, group[k].pred->tc);
                    s << ") ";

                    //!!!delete_PathExpr(path_expr);
                }
                s << ")";
            }
            s << " ";
            serialize2lr_QName(s, var);
            s << " (sequence))))";
        }
    }
    else if (group[j].es == xpes_bottom_up)
    {
        if (j != 0 || strategy->left.get() != NULL)
        { /// if there is something to intersect with... then intersect
            s << "(intersect@ ";
            serialize2lr_xp_pred_group(s, db_ent, strategy, j - 1, var, tree);
            s << " ";
        }
        // up
        s << "(up ";
        // return (predicate)
        s << "(return ";
        // scan
        s << " (scan (const (type !xs!integer) ";
        s << (__uint32)(group[j].pred->scm_bottom);
        s << ") ";
        serialize2lr_db_entity(s, db_ent);
        s << ")";
        // end of scan
        s << " (fun-def ((!xs!anyType ";
        serialize2lr_QName(s, var);
        s << ")) (if@ (";
        serialize2lr_xp_op_type(s, group[j].pred->type);
        s << " ";
        serialize2lr_tuple_cell(s, group[j].pred->tc);
        s << " ";
        serialize2lr_QName(s, var);
        s << ") ";
        serialize2lr_QName(s, var);
        s << " (sequence))))";
        // end of return (predicate)
        s << " (const (type !xs!integer) ";
        s << (__uint32)(group[0].pred->scm_middle);
        s << ")";
        s << ")";
        // end of up
        if (j != 0 || strategy->left.get() != NULL)
        {
            s << ")";
        }
    }
    else if (group[j].es == xpes_ad_filter)
    {
        s << "(adfilter ";
        serialize2lr_xp_pred_group(s, db_ent, strategy, j - 1, var, tree);
        s << " ";
        // return (predicate)
        s << "(return ";
        // scan
        s << " (scan (const (type !xs!integer) ";
        s << (__uint32)(group[j].pred->scm_bottom);
        s << ") ";
        serialize2lr_db_entity(s, db_ent);
        s << ")";
        // end of scan
        s << " (fun-def ((!xs!anyType ";
        serialize2lr_QName(s, var);
        s << ")) (if@ (";
        serialize2lr_xp_op_type(s, group[j].pred->type);
        s << " ";
        serialize2lr_tuple_cell(s, group[j].pred->tc);
        s << " ";
        serialize2lr_QName(s, var);
        s << ") ";
        serialize2lr_QName(s, var);
        s << " (sequence))))";
        // end of return (predicate)
        s << ")";
    }
    else throw USER_EXCEPTION2(SE1051, "Unexpected case in serialize2lr_xpes");
}

void serialize2lr_xp_pred_trans(std::ostream &s, 
                                counted_ptr<db_entity> db_ent, 
                                counted_ptr<xp_tree_strategy> strategy,
                                const xp_tree &tree)
{
    switch (strategy->ts)
    {
        case xpts_group      :
        {
            if (strategy->var.idx != 0)
            { /// stop and print variable
                serialize2lr_save_var(s, strategy->var.idx);
            }
            else
            {
                int level = strategy->group[0].pred->level;

                serialize2lr_xp_pred_group(s, 
                                           db_ent, 
                                           strategy, 
                                           strategy->n - 1, 
                                           tree.levels[level].qname,
                                           tree);
            }

            break;
        }
        case xpts_da_filter  : throw USER_EXCEPTION2(SE1051, "xpts_da_filter is unexpected in serialize2lr_xp_pred_trans");
        case xpts_down       :
        case xpts_down_filter:
        {
            int to_level = 0, from_level = 0, i = 0;
            bool touch_to_level = false;

            determine_transition_levels(strategy, from_level, to_level);
            for (i = 0; i < strategy->var.idxs.n; ++i)
            {
                if (strategy->var.idxs.idxs[i] == to_level)
                {
                    touch_to_level = true;
                    if (strategy->ts == xpts_down)
                        s << "(intersect@ ";
                }
                else
                {
                    s << "(dafilter ";
                    serialize2lr_save_var(s, strategy->var.idxs.idxs[i]);
                    s << " ";
                }
            }


            if (strategy->ts == xpts_down)
            {
                PathExpr *path_expr = build_PathExpr(level2scm_middle(from_level, tree),
                                                     level2scm_middle(to_level, tree));

                serialize2lr_xp_op_path(s, path_expr, xp_et_strategy, db_ent, strategy->left, QName(), tree);
                //!!!delete_PathExpr(path_expr);
            }
            else
            {
                s << "(dafilter ";

                serialize2lr_xp_pred_trans(s, db_ent, strategy->left, tree);

                if (touch_to_level) // if we have been on to_evel already...
                { // ... use variable
                    serialize2lr_save_var(s, to_level);
                }
                else
                { // ... use scan instead
                    // scan
                    s << "(scan (const (type !xs!integer) ";

                    if (strategy->up.get() == NULL || strategy->up->ts != xpts_group)
                        throw USER_EXCEPTION2(SE1051, "Error in physical plan");

                    s << (__uint32)(level2scm_middle(to_level, tree));
                    s << ") ";
                    serialize2lr_db_entity(s, db_ent);
                    s << ") ";
                    // end of scan
                }

                s << ")";
            }


            for (i = strategy->var.idxs.n - 1; i >= 0; --i)
            {
                if (strategy->var.idxs.idxs[i] == to_level)
                {
                    if (strategy->ts == xpts_down)
                    {
                        s << " ";
                        serialize2lr_save_var(s, strategy->var.idxs.idxs[i]);
                    }
                }

                s << ")";
            }

            break;
        }
        case xpts_up         :
        {
            // up
            s << " (up ";
            serialize2lr_xp_pred_trans(s, db_ent, strategy->left, tree);

            if (strategy->up.get() == NULL || strategy->up->ts != xpts_group)
                throw USER_EXCEPTION2(SE1051, "Error in physical plan");
            
            s << " (const (type !xs!integer) ";
            s << (__uint32)(strategy->up->group[0].pred->scm_middle);
            s << ")";
            // end of up
            s << ")";

            break;
        }
        case xpts_up_filter  :
        {
            s << "(adfilter ";

            if (strategy->var.idx != 0) // if we have been on upper level already...
            { // ... use variable
                serialize2lr_save_var(s, strategy->var.idx);
            }
            else
            { // ... use scan instead
                // scan
                s << "(scan (const (type !xs!integer) ";

                if (strategy->up.get() == NULL || strategy->up->ts != xpts_group)
                    throw USER_EXCEPTION2(SE1051, "Error in physical plan");

                s << (__uint32)(strategy->up->group[0].pred->scm_middle);
                s << ") ";
                serialize2lr_db_entity(s, db_ent);
                s << ") ";
                // end of scan
            }

            serialize2lr_xp_pred_trans(s, db_ent, strategy->left, tree);

            s << ")";

            break;
        }
        default              : throw USER_EXCEPTION2(SE1051, "Unexpected case in serialize2lr_xp_pred_trans");
    }

}

void serialize2lr_xp_tree_strategy_line(std::ostream &s, 
                                        counted_ptr<db_entity> db_ent, 
                                        counted_ptr<xp_tree_strategy> strategy,
                                        const xp_tree &tree)
{
    int res = 0, i = 0;

    counted_ptr<xp_tree_strategy> cur = strategy;
    while (cur->left.get()) cur = cur->left;

    while (true)
    {
        if (cur->ts == xpts_group && cur->var.idx != 0)
        { // save to var
            s << "(let@ ";
            serialize2lr_xp_pred_trans(s, db_ent, cur, tree);
            s << " (fun-def ((xs:anyType ";
            serialize2lr_save_var(s, cur->var.idx);
            s << ")) ";
            res += 2; // additional two brackets "))"
        }
                                   
        if (cur.get() == strategy.get()) 
        {
            serialize2lr_xp_pred_trans(s, db_ent, cur, tree);
            for (i = 0; i < res; ++i) s << ")";
            break;
        }
        cur = cur->up;
    }
}

void serialize2lr_xp_tree_strategy(std::ostream &s, 
                                   counted_ptr<db_entity> db_ent, 
                                   counted_ptr<xp_tree_strategy> strategy,
                                   const xp_tree &tree)
{
    if (strategy->ts == xpts_da_filter)
    {
        s << "(dafilter ";
        serialize2lr_xp_tree_strategy(s, db_ent, strategy->left, tree);
        s << " ";
        serialize2lr_xp_tree_strategy(s, db_ent, strategy->right, tree);
        s << ")";
    }
    else
    {
        serialize2lr_xp_tree_strategy_line(s, db_ent, strategy, tree);
    }
}

/*
Lets siplify the situation and assume that xpts_da_filter strategy can appear
only among top operations


                 da_filter
                 /       \
         da_filter       da_filter
        /       \        /       \
       other   other  da_filter  other
        |        |     /    \      |
       other   other other other other
        |        |     |     |
       other   other other other
                 |           | 
               other       other
                             |
                           other

*/

void specify_xp_tree_strategy_line(std::ostream &s, 
                                   counted_ptr<db_entity> db_ent, 
                                   counted_ptr<xp_tree_strategy> strategy,
                                   int levels /*number of levels*/,
                                   int /*in/out*/&var_idx)
{
    counted_ptr<xp_tree_strategy> cur = strategy;
    while (cur->left.get()) cur = cur->left;

    counted_ptr<xp_tree_strategy> *candidates = new counted_ptr<xp_tree_strategy>[levels];

    while (true)
    {
        switch (cur->ts)
        {
            case xpts_group      : 
                {
                    int level = cur->group[0].pred->level;
                    candidates[level] = cur;
                    break;
                }
            case xpts_da_filter  : throw USER_EXCEPTION2(SE1051, "Impossible case (xpts_da_filter) in specify_xp_tree_strategy_line");
            case xpts_down       : 
            case xpts_down_filter: 
                {
                    int from_level = 0, to_level = 0;
                    int i = 0, n = 0;

                    determine_transition_levels(cur, from_level, to_level);
                    for (i = to_level; i > from_level; --i)
                    {
                        if (candidates[i].get() != NULL) ++n;
                    }

                    cur->var.idxs.n = n;
                    if (n == 0) 
                    {
                        cur->var.idxs.idxs = NULL;
                    }
                    else
                    {
                        cur->var.idxs.idxs = new int[n];

                        for (i = to_level, n = 0; i > from_level; --i, ++n)
                        {
                            if (candidates[i].get() != NULL)
                            {
                                candidates[i]->var.idx = var_idx;
                                cur->var.idxs.idxs[n] = var_idx;
                                ++var_idx;
                            }
                        }
                    }
                    break;
                }
            case xpts_up         : break;
            case xpts_up_filter  : 
                {
                    int to_level = cur->up->group[0].pred->level;
                    if (candidates[to_level].get() != NULL && candidates[to_level]->var.idx != 0)
                        cur->var.idx = candidates[to_level]->var.idx;

                    break;
                }
            default              : d_printf1("UNKNOWN");
        }

        if (cur.get() == strategy.get()) break;
        cur = cur->up;
    }
}

void specify_xp_tree_strategy(std::ostream &s, 
                              counted_ptr<db_entity> db_ent, 
                              counted_ptr<xp_tree_strategy> strategy,
                              int levels /*number of levels*/,
                              int /*in/out*/&var_idx)
{
    if (strategy->ts == xpts_da_filter)
    {
        memset(&(strategy->var), 0, sizeof strategy->var);

        specify_xp_tree_strategy(s, db_ent, strategy->left, levels, var_idx);
        specify_xp_tree_strategy(s, db_ent, strategy->right, levels, var_idx);
    }
    else
    {
        counted_ptr<xp_tree_strategy> cur = strategy;
        while (cur->left.get())
        {
            memset(&(cur->var), 0, sizeof cur->var);
            cur = cur->left;
        }

        specify_xp_tree_strategy_line(s, db_ent, strategy, levels, var_idx);
    }
}

void serialize2lr_xp_tree(std::ostream &s, counted_ptr<db_entity> db_ent, const xp_tree &tree)
{
    int var_idx = 1; // should start from 1
    specify_xp_tree_strategy(s, db_ent, tree.strategy, tree.n, var_idx);

    serialize2lr_xp_tree_strategy(s, db_ent, tree.strategy, tree);
}

void serialize2lr_xpath_popt_plan(std::ostream &s, const xpath_popt_plan &xpath_plan)
{
    /// if plan contains several trees, than serialize each tree and unite the results
    if (xpath_plan.n != 1) s << "(union@ ";

    int i = 0;
    for (i = 0; i < xpath_plan.n; i++)
    {
        serialize2lr_xp_tree(s, xpath_plan.db_ent, xpath_plan.trees[i]);
    }

    if (xpath_plan.n != 1) s << ")";    
}

void serialize2lr_popt_plan(std::ostream &s, const popt_plan &plan)
{
    if (plan.empty_sequence)
    {
        s << "(sequence)";
    }
    else if (plan.failed_to_optimize)
    {
        // nothing to do
    }
    else
    {
        serialize2lr_xpath_popt_plan(s, plan.xp_plan);
    }
}


