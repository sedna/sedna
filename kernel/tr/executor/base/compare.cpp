/*
 * File:  compare.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "compare.h"

int tuple_compare(const order_spec_list& osl, int cells_number, tuple_cell *cells1, tuple_cell *cells2)
{
    int res = 0;
    int i = 0;
    order_spec_list::const_iterator it;
    bool a_eos = false, b_eos = false;
    for (i = 0, it = osl.begin(); it != osl.end(); ++i, ++it)
    {
        if ((a_eos = cells1[i].is_eos()) || (b_eos = cells2[i].is_eos()))
        {
            if (a_eos == b_eos) continue;

            if (a_eos)
                if (it->empty_greatest) res = 1;
                else res = -1;
            else
                if (it->empty_greatest) res = -1;
                else res = 1;
            goto compare_order;
        }

        res = (it->f)(cells1[i], cells2[i]);
        if (res) goto compare_order;
    }

    return 0;

compare_order:
    if (it->ascending) return res;
    else return -res;
}

