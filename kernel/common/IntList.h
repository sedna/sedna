/*
 * File:  IntList.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _INTLIST_H
#define _INTLIST_H

#include "sedna.h"
#include "IntHash.h"


// middle_significan_bits, right_zero_bits - parameters of HashTable
template <int middle_significan_bits, int right_zero_bits>
class IntList
{
private:

    struct elem
    {
        int val;
        elem *next;
        elem *pred;
    };

    elem * start;
    elem * last;
    int num;

    IntHash<elem*, middle_significan_bits, right_zero_bits> tbl;



    class ilit
    {
    private:
        elem *p;

        ilit(elem *_p_) : p(_p_) {}

    public:
        static ilit begin(IntList *_lst_) { return ilit(_lst_->start); }
        static ilit end(IntList *_lst_)   { return ilit(NULL); }

        ilit() : p(NULL) {}
        ilit(const ilit& x) : p(x.p) {}
        ilit& operator=(const ilit &x) { p = x.p; return *this; }

        bool operator==(const ilit &x) { return p == x.p; }
        bool operator!=(const ilit &x) { return !operator==(x); }
    
        ilit &operator++() { if (p) p = p->next; return *this; }
        const int &operator*() { return p->val; }
    };


public:

    friend class ilit;
    typedef ilit iterator;

    IntList();
    ~IntList();

    int size() { return num; }

    // functions return 0 if all OK

    int  push       (int  /*in*/  val);
    int  pop        (int& /*out*/ val);
    int  find       (int  /*in*/  val);
    int  find_remove(int  /*in*/  val);
    void clear      ();

    iterator begin() { return ilit::begin(this); }
    iterator end() { return ilit::end(this); }
};



template <int middle_significan_bits, int right_zero_bits>
IntList<middle_significan_bits, right_zero_bits>::IntList() 
{
    num = 0;
    start = NULL;
    last = NULL;
}


template <int middle_significan_bits, int right_zero_bits>
IntList<middle_significan_bits, right_zero_bits>::~IntList() 
{
    elem * p = start;
    for (; p != NULL; )
    {
        elem * tmp = p;
        p = p->next;
        delete tmp;
    }

    start = NULL;
    last = NULL;
    num = 0;
}


template <int middle_significan_bits, int right_zero_bits>
int IntList<middle_significan_bits, right_zero_bits>::push(int /*in*/ val)
{
    elem * tmp = new elem;
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


template <int middle_significan_bits, int right_zero_bits>
int IntList<middle_significan_bits, right_zero_bits>::pop(int& /*out*/ val)
{
    if (last == NULL) return 1;

    val = last->val;
    num--;
    tbl.remove(val);

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


template <int middle_significan_bits, int right_zero_bits>
int IntList<middle_significan_bits, right_zero_bits>::find(int /*in*/ val)
{
    elem * tmp = NULL;

    return tbl.find(val, tmp);
}


template <int middle_significan_bits, int right_zero_bits>
int IntList<middle_significan_bits, right_zero_bits>::find_remove(int /*in*/ val)
{
    elem * tmp = NULL;

    if (tbl.find_remove(val, tmp) == 0)
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

template <int middle_significan_bits, int right_zero_bits>
void IntList<middle_significan_bits, right_zero_bits>::clear()
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



#endif

