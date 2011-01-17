/*
 * File:  indeximpl.h
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _INDEX_IMPLEMENTATION_H
#define _INDEX_IMPLEMENTATION_H

#include "common/xptr.h"
#include "tr/executor/base/tuple.h"

/* Key-Value index implementation */
namespace idx {
  class KeyValueIterator {
    public:
      virtual ~KeyValueIterator() {};
      virtual tuple_cell getKey() = 0;
      virtual tuple_cell getValue() = 0;

      /* nextKey iterates to next key skipping remaining values
         returns true if next logical key is found */
      virtual bool nextKey() = 0;
      /* nextValue iterates to next value
         returns true if next logical key is found */
      virtual bool nextValue() = 0;
      /* nextPair iterates to next value and if values are through,
           iterates to next key
         returns true if next logical key or value is found */
      virtual bool nextPair() = 0;

      virtual bool isnull() const = 0;
  };

  class KeyValueMultimap {
    protected:
      bool sortedInsertionHint;
    public:
      KeyValueMultimap() : sortedInsertionHint(false) {};
      virtual ~KeyValueMultimap() {};

      /* insert key-value pair */
      virtual bool insertPair(tuple_cell key, tuple_cell value) = 0;
      virtual bool deletePair(tuple_cell key, tuple_cell value) = 0;
      virtual bool deleteKey(tuple_cell key) = 0;

      virtual xptr getEntryPoint() = 0;
      virtual void dropTree() = 0;

      virtual KeyValueIterator * begin() = 0;
      virtual KeyValueIterator * find(tuple_cell key) = 0; /* Find nearest */
      virtual KeyValueIterator * end() = 0;

      virtual KeyValueIterator * find_equal(tuple_cell key) = 0;

      void setSortedInsertionHint(bool val) { sortedInsertionHint = val; };
  };
};

#endif /* _INDEX_IMPLEMENTATION_H */
