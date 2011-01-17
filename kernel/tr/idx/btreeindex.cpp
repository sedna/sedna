#include "tr/idx/btreeindex.h"

using namespace idx;

bt_key& tuple_cell2bt_key(const tuple_cell& /*in*/ tc, bt_key& /*out*/ key)
{
    tuple_cell ltc = tuple_cell::make_sure_light_atomic(tc);

    switch (ltc.get_atomic_type())
    {
        case xs_integer          : key.setnew((int64_t)(ltc.get_xs_integer())); break; //!!! FIX THIS
        case xs_float            : key.setnew(ltc.get_xs_float());  break;
        case xs_double           : key.setnew(ltc.get_xs_double()); break;
        case xs_string           : key.setnew(ltc.get_str_mem());   break;
        case xs_date             :
        case xs_dateTime         :
        case xs_time             : key.setnew_dateTime(ltc.get_xs_dateTime(), ltc.get_atomic_type()); break;
        case xs_yearMonthDuration:
        case xs_dayTimeDuration  : key.setnew_duration(ltc.get_xs_duration(), ltc.get_atomic_type()); break;

        default                  : throw USER_EXCEPTION2(SE1003, "Unexpected index type");
    }

    return key;
}

tuple_cell bt_key2tuple_cell(const bt_key& key)
{
    switch (key.get_type()) {
      case xs_integer : return tuple_cell::atomic(key.get_int()); break;
      case xs_float : return tuple_cell::atomic(key.get_float());  break;
      case xs_double : return tuple_cell::atomic(key.get_double()); break;
      case xs_string : return tuple_cell::atomic_deep(xs_string, key.get_string()); break;
      case xs_date : case xs_dateTime :
      case xs_time : return tuple_cell::atomic(key.get_datetime(), key.get_type()); break;
      case xs_yearMonthDuration:
      case xs_dayTimeDuration : return tuple_cell::atomic(key.get_duration(), key.get_type()); break;
      default : throw USER_EXCEPTION2(SE1003, "Unexpected index type");
    }

    return tuple_cell::eos();
}


bool BTreeMultimap::insertPair(tuple_cell key, tuple_cell value)
{
    bt_key btkey;
    bt_insert(this->btree_root, tuple_cell2bt_key(key, btkey), value.get_node_inderection(), !sortedInsertionHint);
    return true;
}

bool BTreeMultimap::deletePair(tuple_cell key, tuple_cell value)
{
    bt_key btkey;
    bt_delete(this->btree_root, tuple_cell2bt_key(key, btkey), value.get_node_inderection());
    return true;
}

xptr BTreeMultimap::getEntryPoint()
{
    return btree_root;
}

bool BTreeMultimap::deleteKey(tuple_cell key)
{
    U_ASSERT(false); // unimplemented!
    return true;
}

void BTreeMultimap::dropTree()
{
    bt_drop(btree_root);
}

BTreeMultimap* BTreeMultimap::openIndex(xptr entryPoint)
{
    BTreeMultimap* result = new BTreeMultimap(entryPoint);
    return result;
}

BTreeMultimap* BTreeMultimap::createIndex(xmlscm_type t)
{
    BTreeMultimap* result = new BTreeMultimap(bt_create(t));
    return result;
}

BTreeMultimap::BTreeMultimap(xptr root): btree_root(root)
{

}

KeyValueIterator* BTreeMultimap::find(tuple_cell key)
{
    bt_key k;
    BTreeIterator * result = new BTreeIterator(bt_find_ge(btree_root, tuple_cell2bt_key(key, k)));
    return result;
}

KeyValueIterator* BTreeMultimap::begin()
{
    U_ASSERT(false); // Unimplemented yet;
    return NULL;
}

KeyValueIterator* BTreeMultimap::end()
{
    U_ASSERT(false); // Unimplemented yet;
    return NULL;
}

BTreeIterator::BTreeIterator(bt_cursor _cursor): cursor(_cursor), tmp_key(tuple_cell::eos()), tmp_value(XNULL)
{
    if (!cursor.is_null()) {
        tmp_key = bt_key2tuple_cell(cursor.get_key());
        tmp_value = cursor.bt_next_obj();
    }
};

bool BTreeIterator::nextKey()
{
    if (cursor.bt_next_key()) {
        tmp_key = bt_key2tuple_cell(cursor.get_key());
        tmp_value = cursor.bt_next_obj();
        return true;
    } else {
        tmp_key = tuple_cell::eos();
        tmp_value = XNULL;
        return false;
    }
}

bool BTreeIterator::nextValue()
{
    tmp_value = cursor.bt_next_obj();
    return tmp_value != XNULL;
}

bool BTreeIterator::nextPair()
{
    if (XNULL != (tmp_value = cursor.bt_next_obj())) {
        return true;
    } else {
        return cursor.bt_next_key();
    }
}

tuple_cell BTreeIterator::getKey()
{
    return tmp_key;
}

tuple_cell BTreeIterator::getValue()
{
    if (XNULL != tmp_value) {
        return tuple_cell::safenode_indir(tmp_value);
    } else {
        return tuple_cell::eos();
    }
}

BTreeIterator::~BTreeIterator()
{
}

BTreeMultimap::~BTreeMultimap()
{
}

bool BTreeIterator::isnull() const
{
    return tmp_value == XNULL && tmp_key.is_eos();
}

KeyValueIterator* BTreeMultimap::find_equal(tuple_cell key)
{
    bt_key k;
    BTreeIterator * result = new BTreeIterator(bt_find(btree_root, tuple_cell2bt_key(key, k)));
    return result;
}
