/*
 * File:  bstrieindex.cpp
 * BSTrie index backend
 * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/idx/bstrieindex.h"
#include "tr/executor/fo/op_map.h"

using namespace idx;

BSTrieMultimap::BSTrieMultimap(btrie_t _trie) : trie(_trie)
{

}

BSTrieMultimap* BSTrieMultimap::openIndex(xptr entryPoint)
{
    BSTrieMultimap* result = new BSTrieMultimap(btrie_open(entryPoint));
    return result;
}

BSTrieMultimap* BSTrieMultimap::createIndex()
{
    BSTrieMultimap* result = new BSTrieMultimap(btrie_open(XNULL));
    return result;
}

/*
static
void null_safe_encode(const char * c, size_t len, std::stringbuf * buf) {
    const char * i = c;
    const char * s;
    size_t sector;

    while (len > 0) {
        i++;
        len--;
    }
};
*/

/* Currently we want to be able to save only strings here.
  Notice, that fixed atomics are ignored assuming, they are filtered on some earlier steps!
*/

class CharBuffer {
  private:
    size_t capacity, size;
    char * buf;
    char * it;
  public:
    ~CharBuffer() { it = NULL; free(buf); };
    CharBuffer(size_t _cap = ((1<<7)-1)) : capacity(_cap), size(0), buf(NULL), it(NULL) { it = buf = (char *) malloc(capacity); }

    void write(const char * src, size_t len) {
        size += len;
        if (size > capacity) {
            capacity = (((size >> 7) + 1) << 7) - 1; U_ASSERT(size <= capacity);
            intptr_t d = it - buf;
            it = (buf = (char *) realloc(buf, capacity)) + d;
        }

        memcpy(it, src, len); it += len;
    };

    void putchar(char c) { write(&c, 1); };

    size_t get_size() const { return size; };
    const char * get_buf() const { return buf; };
};

inline
static void serialize_key(tuple_cell key, CharBuffer * buf)
{
    key = key.make_sure_light_atomic(key);

    U_ASSERT(key.is_light_atomic());

    xmlscm_type tp = key.get_atomic_type();
//    buf->sputn((char *) &tp, sizeof(tp));
    if (key.is_fixed_size_atomic()) {
        U_ASSERT(false);
        // Not yet implemented
    } else {
        buf->write(key.get_str_mem(), key.get_strlen_mem());
    }
}


inline
static void serialize_pair(tuple_cell key, tuple_cell value, CharBuffer * buf)
{
    serialize_key(key, buf);

    buf->putchar('\0');

    U_ASSERT(value.is_node());
    xptr p = value.get_node_inderection();
    buf->write((char *) &p, sizeof(xptr));
}

bool BSTrieMultimap::insertPair(tuple_cell key, tuple_cell value)
{
    CharBuffer key_buf;
    int16_t counter = 1; /* Currently we use 16-bit counter, but we SHOULD use some unlimited byte-level integer encoding */
    serialize_pair(key, value, &key_buf);
    btrie_record_t item = btrie_find(trie, key_buf.get_buf(), key_buf.get_size());

    if (item == BTRNULL) {
        btrie_insert(trie, key_buf.get_buf(), key_buf.get_size(), (char *) &counter, sizeof(counter), true);
//        btrie_insert(trie, key_buf.get_buf(), key_buf.get_size(), NULL, 0, true);
    } else {
        btrie_get_object(item, (char *) &counter, sizeof(counter));
        ++counter;
        btrie_replace_object(item, (char *) &counter);
    }

    return true;
}

bool BSTrieMultimap::deletePair(tuple_cell key, tuple_cell value)
{
    CharBuffer key_buf;
    int16_t counter = 1; /* Currently we use 16-bit counter, but we SHOULD use some unlimited byte-level integer encoding */
    serialize_pair(key, value, &key_buf);
    btrie_record_t item = btrie_find(trie, key_buf.get_buf(), key_buf.get_size());

    if (item == BTRNULL) {
        return false;
    } else {
        btrie_get_object(item, (char *) &counter, sizeof(counter));
        --counter;
        if (counter == 0) {
            btrie_delete(trie, key_buf.get_buf(), key_buf.get_size());
        } else {
            btrie_replace_object(item, (char *) &counter);
        }
    }

    return true;
}

/* Currently not implemented */
bool BSTrieMultimap::deleteKey(tuple_cell key)
{
    U_ASSERT(false);
    return false;
}

BSTrieIterator::BSTrieIterator(bool always_empty) : items(NULL_ENUM),
    tmp_key(tuple_cell::eos()),
    tmp_value(XNULL),
    tmp_valid(true),
    no_next_key(true), first_match_was_equal(false)
{

}

BSTrieIterator::BSTrieIterator(btrie_enum_t path) : items(path), tmp_valid(false), no_next_key(false), first_match_was_equal(false)
{
    deserializePair();
    collationHandler = charset_handler->get_unicode_codepoint_collation();
}

KeyValueIterator* BSTrieMultimap::find(tuple_cell key)
{
    CharBuffer key_buf;

    serialize_key(key, &key_buf);

    bool equal;
    btrie_enum_t bte = btrie_find_prefix(trie, key_buf.get_buf(), key_buf.get_size(), &equal);

    if (NULL_ENUM == bte) {
        return new BSTrieIterator(true);
    } else {
        BSTrieIterator * result = new BSTrieIterator(bte);
        result->first_match_was_equal = equal;
        return result;
    }
}

/* Next implementations can be and need to be optimized much better. */

/* Here we assume, that only standard logical pointer-value break is used,
  that means that the last [sizeof(xptr)] bytes of physical key are logical value,
  the char before them is always '\0', and the rest before it is a logical key */

void BSTrieIterator::deserializePair()
{
    size_t len = btrie_get_key_len(items);
    const char * key = btrie_get_key(items);
    static const size_t value_len = sizeof(xptr);
    const char * value = key + len - value_len;

    U_ASSERT('\0' == value[-1]);

    memcpy(&tmp_value, value, value_len);

   /* Note, that null-terminated string is required for atomic_deep tuple_cell constructor,
      and as far as null character is a logical key terminator, that is ok just to pass the whole key */

    tmp_key = tuple_cell::atomic_deep(xs_string, key);

    tmp_valid = true;
}


tuple_cell BSTrieIterator::getKey()
{
    if (!tmp_valid) { deserializePair(); }

    /* If after deserialization attemp either key or value are not still set,
      that means that it is the end of sequence */
    if (!tmp_valid) {
        return tuple_cell::eos();
    } else {
        return tmp_key;
    };
};

tuple_cell BSTrieIterator::getValue()
{
    if (!tmp_valid) { deserializePair(); }

    if (!tmp_valid || XNULL == tmp_value) { /* See note for getKey() */
        return tuple_cell::eos();
    } else {
        return tuple_cell::node_indir(tmp_value);
    };
}

bool BSTrieIterator::nextPair()
{
    if (btrie_enum_next(items) == false) {
        tmp_value = XNULL;
        tmp_key = tuple_cell::eos();
        tmp_valid = true;
        return false;
    }

    deserializePair();
    return true;
}

bool BSTrieIterator::nextKey()
{
    if (no_next_key) {
        no_next_key = false;
        deserializePair();
        return true;
    }

    tuple_cell current_key = getKey();

    while (!current_key.is_eos()) {
        if (!nextPair()) {
            return false;
        };

        tuple_cell next_key = getKey();
        if (!next_key.is_eos() && !op_eq(current_key, next_key, collationHandler).is_boolean_true()) {
            return true;
        }
        current_key = next_key;
    }

    return false;
}

bool BSTrieIterator::nextValue()
{
    if (no_next_key) {
        return false;
    }

    tuple_cell current_key = getKey();
    if (!nextPair()) { return false; };
    tuple_cell next_key = getKey();

    bool result = op_eq(current_key, next_key, collationHandler).is_boolean_true();

    if (!result) {
        no_next_key = true;

        tmp_value = XNULL;
        tmp_key = current_key;
        tmp_valid = true;
    }

    return result;
}

BSTrieIterator::~BSTrieIterator()
{
//    free(key_clone);
    btrie_enum_close(items);
}

BSTrieMultimap::~BSTrieMultimap()
{
    btrie_close(trie);
}

void BSTrieMultimap::dropTree()
{
    btrie_drop(trie);
}

xptr BSTrieMultimap::getEntryPoint()
{
    return btrie_get_root(trie);
}

KeyValueIterator* BSTrieMultimap::begin()
{
    btrie_enum_t bte = btrie_find_prefix(trie, "", 0);

    if (NULL_ENUM == bte) {
        return new BSTrieIterator(true);
    } else {
        BSTrieIterator * result = new BSTrieIterator(bte);
        result->first_match_was_equal = false;
        return result;
    }
}

KeyValueIterator* BSTrieMultimap::end()
{
    U_ASSERT(false); // Unimplemented yet;
    return NULL;
}

bool BSTrieIterator::isnull() const
{
    return XNULL == tmp_value && tmp_key.is_eos();
}

KeyValueIterator* BSTrieMultimap::find_equal(tuple_cell key)
{
    U_ASSERT(false);

    BSTrieIterator * result = static_cast<BSTrieIterator *>(this->find(key));

    if (!result->is_first_match_equal()) {
        delete result;
        return NULL;
    } else {
        return result;
    }
}
