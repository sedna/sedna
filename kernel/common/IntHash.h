/*
 * File:  IntHash.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _INTHASH_H
#define _INTHASH_H

#include "common/sedna.h"

// T - value type
// 32-bit integer is hashed by the following template:
// 000000...001111................11110000.........0000
// \_________/\______________________/\_______________/
//             middle_significan_bits  right_zero_bits
template <class T, int middle_significan_bits, int right_zero_bits>
class IntHash
{
private:

    struct add_cell
    {
        int key;
        T val;
        add_cell *next;
    };

    struct cell 
    {
        int key;
        T val;
        add_cell *next;
        bool is_present;
    };

    int templ;

    cell tbl[1 << middle_significan_bits];

public:

    IntHash();
    ~IntHash();

    // functions return 0 if all OK    

    int  insert(int key, T val);
    int  find(int key, T &val);
    int  remove(int key);
    int  find_remove(int key, T &val);
    int  replace(int key, T &new_val, T &old_val);
    void clear();
};



template <class T, int middle_significan_bits, int right_zero_bits>
IntHash<T, middle_significan_bits, right_zero_bits>::IntHash() 
{
    int k = 32 - middle_significan_bits - right_zero_bits;
    templ = 0xFFFFFFFF << k >> k >> right_zero_bits << right_zero_bits;

    for (int i = 0; i < (1 << middle_significan_bits); i++)
    {
        tbl[i].next = NULL;
        tbl[i].key = 0;
        tbl[i].is_present = false;
    }
}


template <class T, int middle_significan_bits, int right_zero_bits>
IntHash<T, middle_significan_bits, right_zero_bits>::~IntHash() 
{
    for (int i = 0; i < (1 << middle_significan_bits); i++)
    {
        add_cell * p = tbl[i].next;

        for (; p != NULL; )
        {
            add_cell * tmp = p;
            p = p->next;
            delete tmp;
        }

        tbl[i].next = NULL;
    }
}


template <class T, int middle_significan_bits, int right_zero_bits>
int IntHash<T, middle_significan_bits, right_zero_bits>::insert(int key, T val)
{
    cell &start = tbl[(key & templ) >> right_zero_bits];
    if (start.is_present)
    {
        add_cell * new_cell = new add_cell;
        new_cell->key = key;
        new_cell->val = val;
        new_cell->next = start.next;
        start.next = new_cell;
    }
    else 
    {
        start.key = key;
        start.val = val;
        start.next = NULL;
        start.is_present = true;
    }

    return 0;
}


template <class T, int middle_significan_bits, int right_zero_bits>
int IntHash<T, middle_significan_bits, right_zero_bits>::find(int key, T &val)
{
    cell &start = tbl[(key & templ) >> right_zero_bits];
    if (start.is_present)
    {
        if (start.key == key)
        {
            val = start.val;
            return 0;
        }

        add_cell * p = start.next;
        for (; p != NULL; p = p->next)
        {
            if (p->key == key)
            {
                val = p->val;
                return 0;
            }
        }
        return 1;
    }
    else return 1;

    return 0;
}


template <class T, int middle_significan_bits, int right_zero_bits>
int IntHash<T, middle_significan_bits, right_zero_bits>::remove(int key)
{
    cell &start = tbl[(key & templ) >> right_zero_bits];
    if (start.is_present)
    {
        if (start.key == key)
        {
            if (start.next == NULL)
            {
                start.key = 0;
                start.is_present = false;
            }
            else
            {
                start.key = start.next->key;
                start.val = start.next->val;
                add_cell *p = start.next;
                start.next = start.next->next;
                delete p;
                start.is_present = true;
            }
            return 0;
        }
        else
        {
            if (start.next == NULL) return 1;

            if (start.next->key == key)
            {
                add_cell * p = start.next;
                start.next = start.next->next;
                delete p;
                return 0;
            }

            add_cell * pred = start.next;
            add_cell * cur = start.next->next;

            for (; cur != NULL; cur = cur->next, pred = pred->next)
            {
                if (cur->key == key)
                {
                    pred->next = cur->next;
                    delete cur;
                    return 0;
                }
            }
            return 1;
            
        }
    }
    else return 1;

    return 0;
}


template <class T, int middle_significan_bits, int right_zero_bits>
int IntHash<T, middle_significan_bits, right_zero_bits>::find_remove(int key, T &val)
{
    cell &start = tbl[(key & templ) >> right_zero_bits];
    if (start.is_present)
    {
        if (start.key == key)
        {
            if (start.next == NULL)
            {
                val = start.val;
                start.key = 0;
                start.is_present = false;
            }
            else
            {
                val = start.val;
                start.key = start.next->key;
                start.val = start.next->val;
                add_cell *p = start.next;
                start.next = start.next->next;
                delete p;
                start.is_present = true;
            }
            return 0;
        }
        else
        {
            if (start.next == NULL) return 1;

            if (start.next->key == key)
            {
                val = start.next->val;
                add_cell * p = start.next;
                start.next = start.next->next;
                delete p;
                return 0;
            }

            add_cell * pred = start.next;
            add_cell * cur = start.next->next;

            for (; cur != NULL; cur = cur->next, pred = pred->next)
            {
                if (cur->key == key)
                {
                    val = cur->val;
                    pred->next = cur->next;
                    delete cur;
                    return 0;
                }
            }
            return 1;
        }
    }
    else return 1;

    return 0;
}


template <class T, int middle_significan_bits, int right_zero_bits>
int IntHash<T, middle_significan_bits, right_zero_bits>::replace(int key, T &new_val, T &old_val)
{
    cell &start = tbl[(key & templ) >> right_zero_bits];
    if (start.is_present)
    {
        if (start.key == key)
        {
            old_val = start.val;
            start.val = new_val;
            return 0;
        }

        add_cell * p = start.next;
        for (; p != NULL; p = p->next)
        {
            if (p->key == key)
            {
                old_val = p->val;
                p->val = new_val;
                return 0;
            }
        }
        return 1;
    }
    else return 1;

    return 0;
}

template <class T, int middle_significan_bits, int right_zero_bits>
void IntHash<T, middle_significan_bits, right_zero_bits>::clear()
{
    for (int i = 0; i < (1 << middle_significan_bits); i++)
    {
        add_cell *cur = tbl[i].next, *tmp = NULL;
        while (cur)
        {
            tmp = cur;
            cur = cur->next;
            delete tmp;
        }

        tbl[i].next = NULL;
        tbl[i].key = 0;
        tbl[i].is_present = false;
    }
}

#endif

