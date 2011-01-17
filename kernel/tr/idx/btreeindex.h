/*
 * File:  btreeindex.h
 * BTree index backend
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _BTREE_INDEX_BACKEND_H
#define _BTREE_INDEX_BACKEND_H

#include "tr/idx/indeximpl.h"
#include "tr/btree/btree.h"
#include "tr/btree/btstruct.h"

/* Key-Value index implementation */

/* creates bt_key (fills key argument) from tuple_cell */
bt_key& tuple_cell2bt_key(const tuple_cell& /*in*/ tc, bt_key& /*out*/ key);

namespace idx {
    class BTreeIterator : public KeyValueIterator {
      private:
        bt_cursor cursor;

        tuple_cell tmp_key;
        xptr tmp_value;
      public:
        BTreeIterator(bt_cursor _cursor);

        virtual ~BTreeIterator();
        virtual tuple_cell getKey();
        virtual tuple_cell getValue();

        virtual bool nextKey();
        virtual bool nextValue();
        virtual bool nextPair();

        virtual bool isnull() const;
    };

    class BTreeMultimap : public KeyValueMultimap {
      private:
        xptr btree_root;
      public:
        BTreeMultimap(xptr root);
        virtual ~BTreeMultimap();

        /* insert key-value pair */
        virtual bool insertPair(tuple_cell key, tuple_cell value);
        virtual bool deletePair(tuple_cell key, tuple_cell value);
        virtual bool deleteKey(tuple_cell key);

        virtual xptr getEntryPoint();
        virtual void dropTree();

        virtual KeyValueIterator * find(tuple_cell key);
        virtual KeyValueIterator * begin();
        virtual KeyValueIterator * end();

        virtual KeyValueIterator * find_equal(tuple_cell key);

        static BTreeMultimap * openIndex(xptr entryPoint);
        static BTreeMultimap * createIndex(xmlscm_type t);
    };
};

#endif /* _BTREE_INDEX_BACKEND_H */
