
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
    printf("*** level's group *** (cost = %g)\n", cost);
    int i = 0;
    for (i = 0; i < n; i++)
    {
        printf("  %d. ", i + 1);
        switch (group[i].es)
        {
            case xpes_top_down : printf("xpes_top_down"); break;
            case xpes_bottom_up: printf("xpes_bottom_up"); break;
            case xpes_ad_filter: printf("xpes_ad_filter"); break;
            default            : printf("UNKNOWN");
        }
        printf(": ");
        group[i].pred->pred_op->print();
        printf("\n");
    }
}

void xp_tree::rec_print(counted_ptr<xp_tree_strategy> cur, int indent)
{
    if (cur.get() == NULL) return;

    int i = 0;
    for (i = 0; i < indent; ++i) printf(" ");

    switch (cur->ts)
    {
        case xpts_group      : 
            {
                int j = 0;
                printf("xpts_group (level = %d)\n", cur->group[0].pred->level);
                for (j = 0; j < cur->n; ++j)
                {
                    for (i = 0; i < indent + 3; ++i) printf(" ");
                    switch (cur->group[j].es)
                    {
                        case xpes_top_down : printf("%d. xpes_top_down: ", j + 1); break;
                        case xpes_bottom_up: printf("%d. xpes_bottom_up: ", j + 1); break;
                        case xpes_ad_filter: printf("%d. xpes_ad_filter: ", j + 1); break;
                        default            : printf("UNKNOWN");
                    }
                    cur->group[j].pred->pred_op->print();
                    printf("\n");
                }
                break;
            }
        case xpts_da_filter  : printf("xpts_da_filter\n"); break;
        case xpts_down       : printf("xpts_down\n"); break;
        case xpts_down_filter: printf("xpts_down_filter\n"); break;
        case xpts_up         : printf("xpts_up\n"); break;
        case xpts_up_filter  : printf("xpts_up_filter\n"); break;
        default              : printf("UNKNOWN");
    }

    rec_print(cur->left, indent + 2);
    rec_print(cur->right, indent + 2);
}

void xp_tree::print()
{
    printf("xp_tree (cost = %g):\n", cost);
    rec_print(strategy, 0);
}

void xpath_popt_plan::print()
{
    printf("xpath_popt_plan (%d tree(s)):\n", n);
    int i = 0;
    for (i = 0; i < n; i++)
    {
        if (n > 1)
            printf("     ++++ %d ++++\n", i);
        trees[i].print();
        if (n > 1 && i < n - 1)
            printf("     +++++++++++\n");
    }
}

void popt_plan::print()
{
    printf("popt_plan: ");
    if (empty_sequence) printf("EMPTY SEQUENCE\n");
    else if (failed_to_optimize) printf("FAILED TO OPTIMIZE\n");
    else 
    {
        printf("OPTIMIZED XPATH\n");
        xp_plan.print();
    }
}
