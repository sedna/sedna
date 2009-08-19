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
    enum catalog_named_objects ot;
    void * jr;
    bt_cursor c;
    xptr _object;
    bool locking;

public:
    catalog_iterator(enum catalog_named_objects obj_type, bool _locking = true) :
      ot(obj_type), jr(NULL), c(bt_lm(catalog_get_names(obj_type))), _object(XNULL), locking(false) {
        if (_locking) catalog_lock_metadata();
        locking = _locking;
    };

    ~catalog_iterator() {
        if (locking) catalog_unlock_metadata();
    };

    inline const xptr get_object() {
        if (jr == NULL) {
            return _object;
        } else {
            return __catalog_name_enum_get_object(jr);
        }
    };

    inline const char * name() {
        if (jr == NULL) {
            return (char *) c.get_key().data();
        } else {
            return __catalog_name_enum_get_name(jr);
        }
    };

    inline bool next() {
        if (jr == NULL) {
            if (!c.is_null() && (_object == XNULL || c.bt_next_key())) {
                _object = c.bt_next_obj();
                return true;
            }

            jr = __catalog_name_enum_init(ot);
        } else {
            jr = __catalog_name_enum_next(jr, ot);
        }
        return jr != NULL;
    };
};

#endif // CATENUM_H
