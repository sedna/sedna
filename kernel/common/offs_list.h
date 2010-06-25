/*
 * File:  offs_list.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _SELIST_H
#define _SELIST_H

#include "common/sedna.h"
#include "common/se_hash.h"

/*
 * offs_list implements list of offsets with fast-find via hash
 *     'offset' is consistent with the architecture -- it can list-hash
 *     any offset in memory the architecture can handle
 *
 * middle_significan_bits, right_zero_bits - parameters of HashTable
 *
 */
template <unsigned middle_significan_bits, unsigned right_zero_bits>
class offs_list
{
public:
    typedef size_t value_type;

private:

    template <typename _Val>
    class offs_hash : public se_hash<value_type, _Val, middle_significan_bits, right_zero_bits>
    {
    protected:
        virtual typename se_hash<value_type, _Val, middle_significan_bits, right_zero_bits>::mask_type get_hashkey(const value_type &key)
        {
            return key;
        }
    };

    struct elem
    {
        value_type val;
        elem *next;
        elem *pred;
    };

    elem *start;
    elem *last;
    size_t num;

    // hash is used to facilitate fast find on the list
    offs_hash<elem *> tbl;

    class se_list_it
    {
    private:
        elem *p;

        se_list_it(elem *_p_) : p(_p_) {}

    public:
        static se_list_it begin(offs_list *_lst_) { return se_list_it(_lst_->start); }
        static se_list_it end(offs_list *_lst_)   { return se_list_it(NULL); }

        se_list_it() : p(NULL) {}
        se_list_it(const se_list_it& x) : p(x.p) {}
        se_list_it& operator=(const se_list_it &x) { p = x.p; return *this; }

        bool operator==(const se_list_it &x) { return p == x.p; }
        bool operator!=(const se_list_it &x) { return !operator==(x); }
    
        se_list_it &operator++() { if (p) p = p->next; return *this; }
        const value_type &operator*() { return p->val; }
    };


public:

    typedef se_list_it iterator;

    offs_list()
    {
        num = 0;
        start = NULL;
        last = NULL;
    }
    ~offs_list()
    {
        elem *p = start;
        while (p)
        {
            elem *tmp = p;
            p = p->next;
            delete tmp;
        }

        start = NULL;
        last = NULL;
        num = 0;
    }

    size_t size()
    {
        return num;
    }

    // functions return 0 if all OK

    int  push (const value_type &val)
    {
        elem *tmp = new elem;
        tmp->val = val;
        tmp->pred = NULL;
        num++;
        tbl.insert(val, tmp);

        if (start == NULL)
        {
            tmp->next = NULL;
            start = last = tmp;
        }
        else
        {
            tmp->next = start;
            start->pred = tmp;
            start = tmp;
        }

        return 0;
    }

    int  pop (value_type *val /* out */)
    {
        if (last == NULL) return 1;

        if (val) *val = last->val;
        num--;
        tbl.remove(last->val);

        if (start == last)
        {
            delete last;
            start = last = NULL;
        }
        else
        {
            last = last->pred;
            delete (last->next);
            last->next = NULL;
        }

        return 0;
    }
    int  find (const value_type &val)
    {
        return tbl.find(val, NULL);
    }

    int  find_remove(const value_type &val)
    {
        elem *tmp = NULL;

        if (tbl.find_remove(val, &tmp) == 0)
        {
            num--;

            if (tmp->next == NULL && tmp->pred == NULL) // the only elem in the list
            {
                start = last = NULL;
            }
            else if (tmp->next == NULL) // the last elem in the list
            {
                last = tmp->pred;
                last->next = NULL;
            }
            else if (tmp->pred == NULL) // the first elem in the list
            {
                start = tmp->next;
                start->pred = NULL;
            }
            else
            {
                tmp->pred->next = tmp->next;
                tmp->next->pred = tmp->pred;
            }

            delete tmp;
            return 0;
        }

        return 1;
    }
    void clear ()
    {
        elem *cur = start, *tmp = NULL;
        while (tmp)
        {
            tmp = cur;
            cur = cur->next;
            delete tmp;
        }

        tbl.clear();

        num = 0;
        start = NULL;
        last = NULL;
    }

    iterator begin() { return se_list_it::begin(this); }
    iterator end() { return se_list_it::end(this); }
};

#endif /* _SELIST_H */
