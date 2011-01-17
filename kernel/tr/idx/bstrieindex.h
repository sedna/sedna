/*
 * File:  bstrieindex.h
 * BSTrie index backend
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _BSTRIE_INDEX_BACKEND_H
#define _BSTRIE_INDEX_BACKEND_H

#include "tr/idx/indeximpl.h"
#include "tr/bstrie/btrie.h"

/* Key-Value index implementation on BS-trie */

class CollationHandler;

namespace idx {
  class BSTrieMultimap;

  class BSTrieIterator : public KeyValueIterator {
    friend class BSTrieMultimap;
    private:
      btrie_enum_t items;

      // TODO: optimize previous key comparison!  char * key_clone;
      mutable tuple_cell tmp_key;
      mutable xptr tmp_value;
      mutable bool tmp_valid;
      CollationHandler * collationHandler;
      bool no_next_key;
      bool first_match_was_equal;

      void deserializePair();

      BSTrieIterator(bool always_empty);
    public:
      BSTrieIterator(btrie_enum_t path);

      virtual ~BSTrieIterator();
      virtual tuple_cell getKey();
      virtual tuple_cell getValue();

      virtual bool nextKey();
      virtual bool nextValue();
      virtual bool nextPair();

      virtual bool isnull() const;

      bool is_first_match_equal() const { return first_match_was_equal; }
  };

  class BSTrieMultimap : public KeyValueMultimap {
    private:
      btrie_t trie;
    public:
      BSTrieMultimap(btrie_t _trie);
      virtual ~BSTrieMultimap();

      virtual bool insertPair(tuple_cell key, tuple_cell value);
      virtual bool deletePair(tuple_cell key, tuple_cell value);
      virtual bool deleteKey(tuple_cell key);

      virtual xptr getEntryPoint();
      virtual void dropTree();

      virtual KeyValueIterator * find(tuple_cell key);
      virtual KeyValueIterator * begin();
      virtual KeyValueIterator * end();

      virtual KeyValueIterator * find_equal(tuple_cell key);

      static BSTrieMultimap * createIndex();
      static BSTrieMultimap * openIndex(xptr entryPoint);
  };
};

#endif /* _BSTRIE_INDEX_BACKEND_H */
