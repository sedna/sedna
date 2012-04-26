/*
 * File:  mapindex.cpp
 * Map index backend
 * Copyright (C) 2012 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/idx/mapindex.h"
#include <cstring>

using namespace idx;

bool MapIndex::insertPair(tuple_cell key, tuple_cell value)
{
    mapping->insert(index_obj(key, value.get_node_inderection()));
    return true;
}

bool MapIndex::deletePair(tuple_cell key, tuple_cell value)
{
    index_range range = mapping->equal_range(key);
    for (index_iter it = range.first; it != range.second; it++) {
        if (op_eq(it->second, value, charset_handler->get_unicode_codepoint_collation()).is_boolean_true()) {
            mapping->erase(it);
            return true;
        }
    }
    U_ASSERT("false"); //No such element in index
    return true;
}

xptr MapIndex::getEntryPoint()
{
    xptr res;
    memcpy(&res, &mapping, sizeof(mapping));
    return res;
}

bool MapIndex::deleteKey(tuple_cell key)
{
    U_ASSERT(false); // unimplemented!
    return true;
}

void MapIndex::dropTree()
{
    delete mapping;
}

MapIndex* MapIndex::openIndex(xptr entryPoint)
{
    index_map *map_ptr;
    memcpy(&map_ptr, &entryPoint, sizeof(map_ptr));
    
    MapIndex* result = new MapIndex(map_ptr);
    return result;
}

MapIndex* MapIndex::createIndex()
{
    index_map *map = new index_map();
    MapIndex* result = new MapIndex(map);
    return result;
}

idx::MapIndex::MapIndex(index_map* mapping_ptr)
{
    mapping = mapping_ptr;
}

idx::MapIndex::MapIndex()
{
    mapping = NULL;
}

KeyValueIterator* MapIndex::find(tuple_cell key)
{
    MapIndexIterator *result = new MapIndexIterator(mapping->find(key), mapping, ch);
    return result;
}

KeyValueIterator* MapIndex::begin()
{
    MapIndexIterator *result = new MapIndexIterator(mapping->begin(), mapping, ch);
    return result;
}

KeyValueIterator* MapIndex::end()
{
    U_ASSERT(false); // Unimplemented yet;
    return NULL;
}

MapIndexIterator::MapIndexIterator(index_iter iter, index_map* mapping, CollationHandler* ch)
{
    this->ch = ch;
    this->iter = iter;
    this->mapping = mapping;
    if (iter != mapping->end()) {
        tmp_key = iter->first;
        tmp_value = iter->second;
    } else {
        tmp_key = tuple_cell::eos();
        tmp_value = XNULL;
    }
}

bool MapIndexIterator::nextKey()
{
    if (iter == mapping->end()) {
        return false;
    } else {
        index_iter next_key_iter = mapping->upper_bound(iter->first);
        if (next_key_iter != mapping->end()) {
            iter = next_key_iter;
            tmp_key = iter->first;
            tmp_value = iter->second;
            return true;
        } else {
            tmp_key = tuple_cell::eos();
            tmp_value = XNULL;
            return false;
        }
    }
}

bool MapIndexIterator::nextValue() 
{
    //Shifting to the next value for the current key, if have
    if (iter == mapping->end()) {
        return false;
    } else {
        tuple_cell cur_key = iter->first;

        index_iter new_iter = iter;
        new_iter++;
        if (new_iter != mapping->end() && op_eq(new_iter->first, cur_key, charset_handler->get_unicode_codepoint_collation()).is_boolean_true()) {
            iter = new_iter;
            tmp_value = iter->second;
            return true;
        } else {
            tmp_value = XNULL;
            return false;
        }
    }
}

bool MapIndexIterator::nextPair() 
{
    if (iter != mapping->end()) {
        iter++;
        if (iter != mapping->end()) {
            tmp_key = iter->first;
            tmp_value = iter->second;
            return true;
        } else {
            tmp_key = tuple_cell::eos();
            tmp_value = XNULL;
            return false;
        }
    } else {
        return false;
    }
}

tuple_cell MapIndexIterator::getKey()
{
    return tmp_key;
}

tuple_cell MapIndexIterator::getValue()
{
    if (XNULL != tmp_value) {
        return tuple_cell::safenode_indir(tmp_value);
    } else {
        return tuple_cell::eos();
    }
}

bool MapIndexIterator::isnull() const
{
    return tmp_value == XNULL && tmp_key.is_eos();
}

KeyValueIterator* MapIndex::find_equal(tuple_cell key)
{
    MapIndexIterator *result = new MapIndexIterator(mapping->find(key), mapping, charset_handler->get_unicode_codepoint_collation());
    return result;
}
