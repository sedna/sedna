
#include "popt_plan.h"
#include "PPUtils.h"


int xp_pred_compare(const void *p1, const void *p2)
{
    tsel diff = ((xp_pred*)p1)->sel_middle - ((xp_pred*)p2)->sel_middle;
    if (diff < 0) return -1;
    else if (diff > 0) return 1;
    else return 0;
}

void print(xp_pred_strategy *group, int n, tcost cost)
{
    d_printf2("*** level's group *** (cost = %g)\n", cost);
    int i = 0;
    for (i = 0; i < n; i++)
    {
        d_printf2("  %d. ", i + 1);
        switch (group[i].es)
        {
            case xpes_top_down : d_printf1("xpes_top_down"); break;
            case xpes_bottom_up: d_printf1("xpes_bottom_up"); break;
            case xpes_ad_filter: d_printf1("xpes_ad_filter"); break;
            default            : d_printf1("UNKNOWN");
        }
        d_printf1(": ");
        group[i].pred->pred_op->print();
        d_printf1("\n");
    }
}

void xp_tree::rec_print(counted_ptr<xp_tree_strategy> cur, int indent)
{
    if (cur.get() == NULL) return;

    int i = 0;
    for (i = 0; i < indent; ++i) d_printf1(" ");

    switch (cur->ts)
    {
        case xpts_group      : 
            {
                int j = 0;
                d_printf2("xpts_group (level = %d)\n", cur->group[0].pred->level);
                for (j = 0; j < cur->n; ++j)
                {
                    for (i = 0; i < indent + 3; ++i) d_printf1(" ");
                    switch (cur->group[j].es)
                    {
                        case xpes_top_down : d_printf2("%d. xpes_top_down: ", j + 1); break;
                        case xpes_bottom_up: d_printf2("%d. xpes_bottom_up: ", j + 1); break;
                        case xpes_ad_filter: d_printf2("%d. xpes_ad_filter: ", j + 1); break;
                        default            : d_printf1("UNKNOWN");
                    }
                    cur->group[j].pred->pred_op->print();
                    d_printf1("\n");
                }
                break;
            }
        case xpts_da_filter  : d_printf1("xpts_da_filter\n"); break;
        case xpts_down       : d_printf1("xpts_down\n"); break;
        case xpts_down_filter: d_printf1("xpts_down_filter\n"); break;
        case xpts_up         : d_printf1("xpts_up\n"); break;
        case xpts_up_filter  : d_printf1("xpts_up_filter\n"); break;
        default              : d_printf1("UNKNOWN");
    }

    rec_print(cur->left, indent + 2);
    rec_print(cur->right, indent + 2);
}

void xp_tree::print()
{
    d_printf2("xp_tree (cost = %g):\n", cost);
    rec_print(strategy, 0);
}

void xpath_popt_plan::print()
{
    d_printf2("xpath_popt_plan (%d tree(s)):\n", n);
    int i = 0;
    for (i = 0; i < n; i++)
    {
        if (n > 1)
            d_printf2("     ++++ %d ++++\n", i);
        trees[i].print();
        if (n > 1 && i < n - 1)
            d_printf1("     +++++++++++\n");
    }
}

void popt_plan::print()
{
    d_printf1("popt_plan: ");
    if (empty_sequence) d_printf1("EMPTY SEQUENCE\n");
    else if (failed_to_optimize) d_printf1("FAILED TO OPTIMIZE\n");
    else 
    {
        d_printf1("OPTIMIZED XPATH\n");
        xp_plan.print();
    }
}
