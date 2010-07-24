/*
 * File:  se_hash.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef SEHASH_H_
#define SEHASH_H_

#include "common/sedna.h"

/*
 * _Key - key type
 * _Val - value type
 *
 * First, 32-bit hashkey is obtained from the key parameter.
 *
 * Then, 32-bit hashkey is used to find value using template:
 * 000000...001111................11110000.........0000
 * \_________/\______________________/\_______________/
 *             middle_significan_bits  right_zero_bits
 */
template <typename _Key, typename _Val, unsigned middle_significan_bits, unsigned right_zero_bits>
class se_hash
{
public:

    typedef _Key key_type;
    typedef _Val value_type;

protected:

    typedef unsigned int mask_type;

    // return index in table based on key value
    virtual mask_type get_hashkey(const key_type &key) = 0;

private:

    // collision cell
    struct add_cell
    {
        key_type key;
        value_type val;
        add_cell *next;
    };

    // main cell
    struct cell
    {
        key_type key;
        value_type val;
        add_cell *next;
        bool is_present;
    };

    mask_type templ;
    size_t cell_num;

    // basic table of cells
    cell *tbl;

    // iterator class
    class sehash_iterator
    {
    private:
        // (pos, p) determines exact location
        mask_type pos;
        add_cell *p;
        se_hash *hash;

        sehash_iterator &next()
        {
            bool use_next_cell = false;

            if (p == NULL)
            {
                if (hash->tbl[pos].next != NULL)
                    p = hash->tbl[pos].next;
                else
                    use_next_cell = true;
            }
            else
            {
                p = p->next;
                if (p == NULL) use_next_cell = true;
            }

            if (use_next_cell)
            {
                while (++pos < (1 << middle_significan_bits))
                    if (hash->tbl[pos].is_present) break;
            }

            return *this;
        }

        sehash_iterator(mask_type _pos_, add_cell *_p_, se_hash *_hash_) : pos(_pos_), p(_p_), hash(_hash_) {}

    public:
        static sehash_iterator begin(se_hash *_hash_)
        {
            sehash_iterator it(0, NULL, _hash_);
            return _hash_->tbl[0].is_present ? it : it.next();
        }
        static sehash_iterator end(se_hash *_hash_)
        {
            return sehash_iterator(1 << middle_significan_bits, NULL, _hash_);
        }

        sehash_iterator() : pos(0), p(NULL), hash(NULL) {}
        sehash_iterator(const sehash_iterator& x) : pos(x.pos), p(x.p), hash(x.hash) {}

        sehash_iterator& operator=(const sehash_iterator &x)
        {
            pos = x.pos;
            p = x.p;
            hash = x.hash;
            return *this;
        }

        bool operator==(const sehash_iterator &x)
        {
            return hash == x.hash && ((p == x.p && p != NULL) || (p == x.p && p == NULL && pos == x.pos));
        }
        bool operator!=(const sehash_iterator &x)
        {
            return !operator==(x);
        }

        const key_type &getKey()
        {
            if (p == NULL)
            {
                if (pos >= (1 << middle_significan_bits))
                    throw SYSTEM_EXCEPTION("Out of bounds");
                else
                    return hash->tbl[pos].key;
            }
            else
            {
                return p->key;
            }
        }

        sehash_iterator &operator++()
        {
            return next();
        }

        const value_type &operator*()
        {
            if (p == NULL)
            {
                if (pos >= (1 << middle_significan_bits))
                    throw SYSTEM_EXCEPTION("Out of bounds");
                else
                    return hash->tbl[pos].val;
            }
            else
            {
                return p->val;
            }
        }
    };

private:
    cell &get_cell(const key_type &key)
    {
        return tbl[(get_hashkey(key) & templ) >> right_zero_bits];
    }

public:

    typedef sehash_iterator iterator;

    se_hash()
    {
        unsigned k = 32 - middle_significan_bits - right_zero_bits;
        templ = 0xFFFFFFFF << k >> k >> right_zero_bits << right_zero_bits;

        tbl = new cell[1 << middle_significan_bits];

        for (mask_type i = 0; i < (1 << middle_significan_bits); i++)
        {
            tbl[i].next = NULL;
            tbl[i].is_present = false;
        }

        cell_num = 0;
    }

    void clear()
    {
        if (cell_num)
        {
            for (mask_type i = 0; i < (1 << middle_significan_bits); i++)
            {
                add_cell *cur = tbl[i].next, *tmp = NULL;

                while (cur)
                {
                    tmp = cur;
                    cur = cur->next;
                    delete tmp;
                }

                tbl[i].next = NULL;
                tbl[i].is_present = false;
            }

            cell_num = 0;
        }
    }

    virtual ~se_hash()
    {
        clear();
        delete[] tbl;
    }

    // functions return 0 if all OK
    int insert(const key_type &key, const value_type &val)
    {
        cell &start = get_cell(key);

        if (start.is_present)
        {
            add_cell *new_cell = new add_cell;
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

        cell_num++;

        return 0;
    }

    int find(const key_type &key, value_type *val)
    {
        cell &start = get_cell(key);

        if (start.is_present)
        {
            if (start.key == key)
            {
                if (val) *val = start.val;
                return 0;
            }

            add_cell *p = start.next;
            for (; p != NULL; p = p->next)
            {
                if (p->key == key)
                {
                    if (val) *val = p->val;
                    return 0;
                }
            }
            return 1;
        }
        else
        {
            return 1;
        }

        return 0;
    }

    /*
        Remove from se_hash invalidates all iterators!
        Consider the following standard example:

        Container container;
        for (Container::iterator it=container.begin(); it!=container.end(); false)
        {
            Container::iterator temp=it;
            ++it;                       // (1)
            Container.remove(temp);     // (2)
        }

        If you swap (1) and (2) you are in trouble, at least with STL.
        se_hash doesn't have a remove(Iterator) function, however it
        has remove-by-key function, which has the same issues.

        See code below for labels (A,B,C).
        (A) Removing an element that is stored in tbl[] invalidates an iterator
            pointing to the first element in the collision list attached to the same cell.
            An iterator pointing to the removed element is now pointing to the former first
            element of the list.
        (B,C) Removing an element from collision list invalidates iterators pointing
            to the removed element. Other iterators are not affected.

        The outlined inconsistency in iterator invalidation rules makes it impossible
        to write a safe code that does for-each-element-remove-those-that-meet-some-condition with
        se_hash.

        TODO:
            The resulted incosistency is mainly to the fact that both cell and add_cell
            store key-value pairs. It could be easily fixed (invalidate only removed element)
            if cell didn't contain key-value and was used only for meta-info (or
            was removed all together).
    */
    int find_remove(const key_type &key, value_type *val)
    {
        cell &start = get_cell(key);
        if (start.is_present)
        {
            if (start.key == key)
            {
                if (start.next == NULL)
                {
                    if (val) *val = start.val;
                    start.is_present = false;
                }
                else
                {
                    if (val) *val = start.val;
                    start.key = start.next->key;
                    start.val = start.next->val;   /* (A) */
                    add_cell *p = start.next;
                    start.next = start.next->next;
                    delete p;
                    start.is_present = true;
                }

                cell_num--;

                return 0;
            }
            else
            {
                if (start.next == NULL) return 1;

                if (start.next->key == key)
                {
                    if (val) *val = start.next->val;
                    add_cell *p = start.next;
                    start.next = start.next->next; /* (B) */
                    delete p;
                    cell_num--;

                    return 0;
                }

                add_cell *pred = start.next;
                add_cell *cur = start.next->next;

                for (; cur != NULL; cur = cur->next, pred = pred->next)
                {
                    if (cur->key == key)
                    {
                        if (val) *val = cur->val;
                        pred->next = cur->next; /* (C) */
                        delete cur;
                        cell_num--;

                        return 0;
                    }
                }
                return 1;
            }
        }
        else
        {
            return 1;
        }

        return 1;
    }

    int remove(const key_type &key)
    {
        return find_remove(key, NULL);
    }

    int replace(const key_type &key, const value_type &new_val, value_type *old_val)
    {
        cell &start = get_cell(key);

        if (start.is_present)
        {
            if (start.key == key)
            {
                if (old_val) *old_val = start.val;
                start.val = new_val;
                return 0;
            }

            add_cell * p = start.next;
            for (; p != NULL; p = p->next)
            {
                if (p->key == key)
                {
                    if (old_val) *old_val = p->val;
                    p->val = new_val;
                    return 0;
                }
            }
            return 1;
        }
        else
        {
            return 1;
        }

        return 0;
    }

    size_t size() const
    {
        return cell_num;
    }

    iterator begin()
    {
        return sehash_iterator::begin(this);
    }
    iterator end()
    {
        return sehash_iterator::end(this);
    }
};

#endif /* SEHASH_H_ */
