/*
 * File:  ext_sort.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

/**
1. Перенести то, что касается merge в merge.h/cpp
2. Исправить ошибку с освобождением памяти в elim_disturb
3. Переписать elim_disturb более эффективно
4. Реализовать функции fix_blocks, unfix_blocks
5. Реализовать sort_merge
6. Исправить ошибку в sequence, когда освобождается память
   и copy_vmm_strings стоит в true

*/

#include "ext_sort.h"
#include "vmm.h"

struct merged_seq_top
{
    sequence* s;
    tuple t;
    int pos;
    const order_spec_list* osl;

    merged_seq_top() : s(NULL), pos(-1), osl(NULL) {}
    ~merged_seq_top() { clear(); }
    void clear()
    {
        delete s;
        s = NULL;
        pos = -1;
        osl = NULL;
    }
};

int merged_seq_top_cmp(const void *e1, const void *e2)
{
    merged_seq_top *el1 = (merged_seq_top*)e1;
    merged_seq_top *el2 = (merged_seq_top*)e2;

    if (el1->pos == -1 && el2->pos == -1) return 0;
    if (el1->pos == -1) return 1;
    if (el2->pos == -1) return -1;

    return tuple_compare(*(el1->osl), el1->t.cells_number, el1->t.cells, el2->t.cells);
}


void ext_sort(sequence *s, const order_spec_list& osl, bool stable)
{
    int number_of_potentially_allocated_blocks;
    vmm_enter_exclusive_mode(&number_of_potentially_allocated_blocks);
    number_of_potentially_allocated_blocks -= 10; //!!!
    if (number_of_potentially_allocated_blocks <= 0)
        throw USER_EXCEPTION2(SE1003, "Not enough memory to perform external sort");

    try {
        // separate sequence s into parts, which can be sorted in main memory and sort them
        std::vector<sequence*> _seqs;
        int tuple_size = s->get_tuple_size();
        sequence* cur = new sequence(tuple_size, 0, number_of_potentially_allocated_blocks, true);
        tuple t(s->get_tuple_size());

        int i = 0;
        for (i = 0; i < s->size(); ++i)
        {
            s->get(t, i);
            if (cur->add(t) != 0)
            {
                cur->fix_blocks();
                if (stable) cur->sort_merge(osl);
                else cur->qsort(osl);
                cur->unfix_blocks();

                _seqs.push_back(cur);

                cur = new sequence(tuple_size, 0, number_of_potentially_allocated_blocks, true);
                if (cur->add(t) != 0)
                    throw USER_EXCEPTION2(SE1003, "Not enough memory to perform external sort");
            }
        }

        _seqs.push_back(cur);

        // merge parts
        s->clear();
        
        int seqs_number = _seqs.size();
        merged_seq_top *seqs = new merged_seq_top[seqs_number];
        for (i = 0; i < seqs_number; ++i)
        {
            seqs[i].s = _seqs[i];
            seqs[i].osl = &osl;
            if (seqs[i].s->size() > 0)
            {
                seqs[i].s->get(seqs[i].t, 0);
                seqs[i].pos = 0;
            }
        }

        qsort(seqs, seqs_number, sizeof(merged_seq_top), merged_seq_top_cmp);

        while (true)
        {
            if (seqs[0].pos == -1) break;

            s->add(seqs[0].t);

            if (seqs[0].pos + 1 < seqs[0].s->size())
                seqs[0].s->get(seqs[0].t, ++(seqs[0].pos));
            else 
                seqs[0].pos = -1;

            elim_disturb(seqs, seqs_number, sizeof(merged_seq_top), merged_seq_top_cmp);
        }

        delete [] seqs;

        vmm_exit_exclusive_mode();

    } catch (SednaUserException &e) {
        vmm_exit_exclusive_mode();
        throw;
    }
}

