/*
 * File:  mapindex.h
 * STL map index backend
 * Copyright (C) 2012 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _MAP_INDEX_BACKEND_H
#define _ MAP_INDEX_BACKEND_H

#include "tr/idx/indeximpl.h"
#include "tr/bstrie/btrie.h"
#include "tr/strings/utf8.h"
#include "tr/executor/fo/op_map.h"

#include "tr/btree/btstruct.h"

/* Key-Value index implementation using stl map and BTree key class*/

class CollationHandler;

#include <map>

struct tuple_cell_comp {
    CollationHandler *ch;
    
    tuple_cell_comp() {
        //CollationManager cm = CollationManager();
        //ch = cm.get_default_collation_handler();
        //ch = charset_handler->get_unicode_codepoint_collation();
    }
    
    ~tuple_cell_comp() {
        //delete ch;
    }
    
    bool operator() (const tuple_cell& left, const tuple_cell& right) const
    {
        return op_lt(left, right, charset_handler->get_unicode_codepoint_collation()).get_xs_boolean();
    }
};

typedef std::multimap<tuple_cell, xptr, tuple_cell_comp> index_map;
typedef std::pair<tuple_cell, xptr> index_obj;
typedef index_map::iterator index_iter;
typedef std::pair<index_iter, index_iter> index_range;

namespace idx {
    class MapIndex;

    class MapIndexIterator : public KeyValueIterator {
    private:
        index_iter iter;
        index_map *mapping;
        
        tuple_cell tmp_key;
        xptr tmp_value;
        
        CollationHandler *ch;
    public:
        MapIndexIterator(index_iter iter, index_map *mapping, CollationHandler *ch);

        //virtual ~MapIndexIterator();
        
        virtual tuple_cell getKey();
        virtual tuple_cell getValue();

        virtual bool nextKey();
        virtual bool nextValue();
        virtual bool nextPair();

        virtual bool isnull() const;
    };

    class MapIndex : public KeyValueMultimap {
    private:
        index_map *mapping;
        CollationHandler *ch;
    public:
        MapIndex(index_map* mapping_ptr);
        MapIndex();
        ~MapIndex();

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

        static MapIndex * openIndex(xptr entryPoint);
        static MapIndex * createIndex();
    };
};

#endif /* _MAP_INDEX_BACKEND_H */
