/*
 * File:  XptrHash.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _XPTRHASH_H
#define _XPTRHASH_H

#include "sedna.h"
#include "xptr.h"

// T - value type
// 32-bit part of xptr is hashed by the following template:
// 000000...001111................11110000.........0000
// \_________/\______________________/\_______________/
//             middle_significan_bits  right_zero_bits
template <class T, __uint32 middle_significan_bits, __uint32 right_zero_bits>
class XptrHash
{
private:

    struct add_cell
    {
        xptr key;
        T val;
        add_cell *next;
    };

    struct cell 
    {
        xptr key;
        T val;
        add_cell *next;
        bool is_present;
    };

    __uint32 templ;

    typedef cell t_tbl[1 << middle_significan_bits];

    t_tbl tbl;
    int num;



    class xhit
    {
        private:
        int pos;
        add_cell *p;
        XptrHash *hash;

        xhit &next() 
        {
            bool flag = true;
            if (p != NULL) 
            {
                p = p->next;
                if (p == NULL) flag = false;
            }

            if (p == NULL)
            {
                if (flag && hash->tbl[pos].next != NULL) 
                    p = hash->tbl[pos].next;
                else
                    while (++pos < (1 << middle_significan_bits)) 
                        if (hash->tbl[pos].is_present) break;
            }

            return *this;
        }

        xhit(int _pos_, add_cell *_p_, XptrHash *_hash_) : pos(_pos_), p(_p_), hash(_hash_) {}

    public:
        static xhit begin(XptrHash *_hash_) { xhit x(0, NULL, _hash_); return _hash_->tbl[0].is_present ? x : x.next(); }
        static xhit end(XptrHash *_hash_) { return xhit(1 << middle_significan_bits, NULL, _hash_); }

        xhit() : pos(0), p(NULL), hash(NULL) {}
        xhit(const xhit& x) : pos(x.pos), p(x.p), hash(x.hash) {}
        xhit& operator=(const xhit &x) { pos = x.pos; p = x.p; hash = x.hash; return *this; }

        bool operator==(const xhit &x) { return hash == x.hash && ((p == x.p && p != NULL) || (p == x.p && p == NULL && pos == x.pos)); }
        bool operator!=(const xhit &x) { return !operator==(x); }
    
        xhit &operator++() { return next(); }
        const T &operator*()
        {
            if (p == NULL)
            {
                if (pos >= (1 << middle_significan_bits)) throw SYSTEM_EXCEPTION("Out of bounds");
                else return hash->tbl[pos].val;
            }
            else return p->val;
        }
    };

public:

    friend class xhit;

    typedef xhit iterator;

    XptrHash();
    ~XptrHash();

    // functions return 0 if all OK    

    int insert(xptr key, T val);
    int find(xptr key, T &val);
    int remove(xptr key);
    int find_remove(xptr key, T &val);
    int replace(xptr key, const T &new_val, T &old_val);
    void clear();

    iterator begin() { return xhit::begin(this); }
    iterator end() { return xhit::end(this); }
};



template <class T, __uint32 middle_significan_bits, __uint32 right_zero_bits>
XptrHash<T, middle_significan_bits, right_zero_bits>::XptrHash() 
{
    __uint32 k = (__uint32)32 - middle_significan_bits - right_zero_bits;
    templ = (__uint32)0xFFFFFFFF << k >> k >> right_zero_bits << right_zero_bits;

    for (__uint32 i = 0; i < ((__uint32)1 << middle_significan_bits); i++)
    {
        tbl[i].next = NULL;
        tbl[i].key.clear();
        tbl[i].is_present = false;
    }

    num = 0;
}


template <class T, __uint32 middle_significan_bits, __uint32 right_zero_bits>
XptrHash<T, middle_significan_bits, right_zero_bits>::~XptrHash() 
{
    clear();
}


template <class T, __uint32 middle_significan_bits, __uint32 right_zero_bits>
int XptrHash<T, middle_significan_bits, right_zero_bits>::insert(xptr key, T val)
{
//    T val1;
//    if (find(key, val1) == 0) throw USER_ENV_EXCEPTION("xxx", false);
    cell &start = tbl[((__uint32)(XADDR(key)) & templ) >> right_zero_bits];
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
   
    num++;

    return 0;
}


template <class T, __uint32 middle_significan_bits, __uint32 right_zero_bits>
int XptrHash<T, middle_significan_bits, right_zero_bits>::find(xptr key, T &val)
{
    cell &start = tbl[((__uint32)(XADDR(key)) & templ) >> right_zero_bits];
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


template <class T, __uint32 middle_significan_bits, __uint32 right_zero_bits>
int XptrHash<T, middle_significan_bits, right_zero_bits>::remove(xptr key)
{
    cell &start = tbl[((__uint32)(XADDR(key)) & templ) >> right_zero_bits];

    if (start.is_present)
    {
        if (start.key == key)
        {
            if (start.next == NULL)
            {
                start.key.clear();
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
            --num;
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
                --num;
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
                    --num;
                    return 0;
                }
            }
            return 1;
            
        }
    }
    else return 1;

    --num;
    return 0;
}


template <class T, __uint32 middle_significan_bits, __uint32 right_zero_bits>
int XptrHash<T, middle_significan_bits, right_zero_bits>::find_remove(xptr key, T &val)
{
    cell &start = tbl[((__uint32)(XADDR(key)) & templ) >> right_zero_bits];

    if (start.is_present)
    {
        if (start.key == key)
        {
            if (start.next == NULL)
            {
                val = start.val;
                start.key.clear();
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
            --num;
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
                --num;
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
                    --num;
                    return 0;
                }
            }
            return 1;
        }
    }
    else return 1;

    --num;
    return 0;
}


template <class T, __uint32 middle_significan_bits, __uint32 right_zero_bits>
int XptrHash<T, middle_significan_bits, right_zero_bits>::replace(xptr key, const T &new_val, T &old_val)
{
    cell &start = tbl[((__uint32)(XADDR(key)) & templ) >> right_zero_bits];
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

template <class T, __uint32 middle_significan_bits, __uint32 right_zero_bits>
void XptrHash<T, middle_significan_bits, right_zero_bits>::clear()
{
    if (num != 0)
    {
        add_cell *p = NULL, *tmp = NULL;
    
        for (__uint32 i = 0; i < ((__uint32)1 << middle_significan_bits); i++)
        {
            p = tbl[i].next;
            while (p)
            {
                tmp = p;
                p = tmp->next;
                delete tmp;
            }
            tbl[i].key = XNULL;
            tbl[i].next = NULL;
            tbl[i].is_present = false;
        }
    
        num = 0;
    }
}

#endif

