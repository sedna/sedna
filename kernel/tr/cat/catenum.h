/*
 * File: catenum.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef CATENUM_H
#define CATENUM_H

#include "tr/btree/btree.h"
#include "tr/btree/btstruct.h"
#include "tr/structures/rbtree.h"
#include "tr/cat/catalog.h"
#include "tr/cat/catmem.h"

/************************************
 * Catalog iterator
 */

struct catalog_custom_iterator {
public :
    virtual bool next() = 0;
    virtual xptr get_object() const = 0;
    virtual const char * get_name() const = 0;

    virtual ~catalog_custom_iterator() {};
};

struct catalog_simple_iterator : public catalog_custom_iterator {
private:
    bt_cursor c;
    xptr _object;
    bool locked;

public:
    catalog_simple_iterator(enum catalog_named_objects obj_type, bool _locking = true) :
      c(bt_lm(catalog_get_names(obj_type))), _object(XNULL), locked(false) {
        if (_locking) catalog_lock_metadata();
        locked = _locking;
    };

    ~catalog_simple_iterator() {
        if (locked) catalog_unlock_metadata();
    };

    xptr get_object() const {
        return _object;
    };

    const char * get_name() const {
        return (char *) c.get_key().data();
    };

    bool next() {
        if (!c.is_null()) {
            if (_object != XNULL && !c.bt_next_key()) { return false; }
            _object = c.bt_next_obj();
        }
        return (!c.is_null());
    };
};

struct catalog_complex_iterator : public catalog_custom_iterator {
private:
    bool locking;
    void * tmp_tree;
    void * it;

public:
    ~catalog_complex_iterator() {
        release_tree();
    };

    xptr get_object() const;
    const char * get_name() const;
    bool next();

    void build_tree(enum catalog_named_objects ot);
    void release_tree();

    catalog_complex_iterator(enum catalog_named_objects obj_type, bool _locking = true) :
       locking(_locking), tmp_tree(NULL), it(NULL) {
        build_tree(obj_type);
    };

};

struct catalog_iterator {
private :
    struct catalog_custom_iterator *it;
public :
    catalog_iterator(enum catalog_named_objects obj_type, bool _locking = true) : it(NULL) {
        it = new catalog_complex_iterator(obj_type, _locking);
    };
    ~catalog_iterator() { delete it; };

    inline xptr get_object() const { return it->get_object(); };
    inline const char * get_name() const { return it->get_name(); };
    inline bool next() {
        bool more;
        
        more = it->next();
        while (more && get_object() == XNULL) {
            more = it->next();
        }
        
        return more;
    };
};


#endif // CATENUM_H
