/*
 * File: catenum.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef CATENUM_H
#define CATENUM_H

#include "tr/idx/btree/btstruct.h"
#include "tr/cat/catalog.h"

/************************************
 * Catalog iterator
 */

struct catalog_iterator {
private:
    bt_cursor c;
    xptr _object;
    bool locking;

public:
    catalog_iterator(enum catalog_named_objects obj_type, bool _locking = true) :
      c(bt_lm(catalog_get_names(obj_type))), _object(XNULL), locking(false) {
        if (_locking) catalog_lock_metadata();
        locking = _locking;
    };

    ~catalog_iterator() {
        if (locking) catalog_unlock_metadata();
    };

    inline const xptr get_object() { return _object; };

    inline const char * name() { return (char *) c.get_key().data(); };

    inline bool next() {
        if (c.is_null()) return false;
        if (_object != XNULL) {
          if (!c.bt_next_key()) return false;
        }
        _object = c.bt_next_obj();
        return true;
    };
};

#endif // CATENUM_H
