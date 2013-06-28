/*
 * File:  PPOrderBy_serializer.cpp
 * Copyright (C) 2011 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "PPOrderBy_serializer.h"
#include "common/bit_set.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/fo/string_operations.h"

TupleSerializer::TupleSerializer(int _bit_set_offset_, arr_of_orb_modifier* _modifiers_, arr_of_common_type* _header_, bool _stable_, sequence *_sort_, int64_t *_pos_)
{
    bit_set_offset = _bit_set_offset_;
    header = _header_;
    modifiers = _modifiers_;
    stable = _stable_;
    sort = _sort_;
    init_pos = _pos_;
}

/// Works correctly only if dest is pointer within dynamic memory!
static inline void serialize_string(const tuple_cell& tc, void* dest)
{
    int64_t length_all = tc.get_strlen();
    /* We pretty sure that cast is valid since we check string length */
    size_t length_ser = (size_t)(length_all < ORB_STRING_PREFIX_SIZE ? length_all : ORB_STRING_PREFIX_SIZE);
    bool flag = (length_all <= ORB_STRING_PREFIX_SIZE);
    memcpy(dest, &flag, sizeof(bool));
    tc.copy_string((char*)dest + sizeof(bool), length_ser);
    *((char*)dest + sizeof(bool) + length_ser) = '\0';
}

size_t TupleSerializer::serialize(const xqp_tuple& t, void* buf)
{
    bit_set bs(header -> size());
    memcpy((char *)buf, init_pos, sizeof(int64_t));
    size_t offset = sizeof(int64_t); //Offset to write to buffer
    for (int i = 0; i < t.cells_number; i++)
    {
        orb_common_type &ct = header -> at(i);
        if (!ct.initialized) continue;              //if ct.initialized == false we have a column of eos values
        xmlscm_type type = ct.xtype;                //thus we don't need to serialize this column and sort by it
        int type_size = ct.size;

        if (t.cells[i].is_eos())
        {
            bs.setAt(i);
            offset += ct.size;
        }
        else
        {
            if (t.cells[i].get_atomic_type() != type)
            {
                t.cells[i] = cast(t.cells[i], type);
            }
            U_ASSERT(t.cells[i].is_atomic());
            xmlscm_type s_type = t.cells[i].get_atomic_type();
            int s_type_size = ORB_SERIALIZED_SIZE(s_type);

            switch (s_type)
            {
            case xs_float                : {
                float value = t.cells[i].get_xs_float();
                memcpy((char *)buf + offset, &value, s_type_size);
                break;
            }
            case xs_double               : {
                double value = t.cells[i].get_xs_double();
                memcpy((char *)buf + offset, &value, s_type_size);
                break;
            }
            case xs_decimal              : {
                xs_decimal_t value = t.cells[i].get_xs_decimal();
                memcpy((char *)buf + offset, &value, s_type_size);
                break;
            }
            case xs_integer              : {
                int64_t value = t.cells[i].get_xs_integer();
                memcpy((char *)buf + offset, &value, s_type_size);
                break;
            }
            case xs_boolean              : {
                bool value = t.cells[i].get_xs_boolean();
                memcpy((char *)buf + offset, &value, s_type_size);
                break;
            }
            case xs_string               : {
                serialize_string(t.cells[i], (char *)buf + offset);
                break;
            }
            case xs_time                 :
            case xs_date                 :
            case xs_dateTime             : {
                xs_packed_datetime value = t.cells[i].get_xs_dateTime();
                memcpy((char *)buf + offset, &value, type_size);
                break;
            }
            case xs_yearMonthDuration    :
            case xs_dayTimeDuration      : {
                xs_packed_duration value = t.cells[i].get_xs_duration();
                memcpy((char *)buf + offset, &value, type_size);
                break;
            }
            default                      :
                throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type.");
            }
            offset += s_type_size;
        }
    }
    memcpy((char *)buf + offset, bs.get_ptr_to_bytes(), bs.get_size_in_bytes());
    offset += bs.get_size_in_bytes();
    return offset;
}

void TupleSerializer::deserialize(xqp_tuple& t, void* buf, size_t size)
{
    int64_t pos;
    memcpy(&pos, buf, sizeof(int64_t));

    t.copy(tuple_cell::atomic((int64_t)pos));
}

static inline void get_deserialized_value(void* value, const void* addr, xmlscm_type type)
{
    memcpy(value, addr, ORB_SERIALIZED_SIZE(type));
}

static inline int compare_doubles(double value1, double value2, orb_modifier::orb_empty_status o)
{
    if (value2 == value1) return 0;

    bool is_nan1 = (u_is_nan(value1) != 0);
    bool is_nan2 = (u_is_nan(value2) != 0);

    if (is_nan1 && !is_nan2) return (o == orb_modifier::ORB_EMPTY_GREATEST ? -1 : 1);
    if (is_nan2 && !is_nan1) return (o == orb_modifier::ORB_EMPTY_GREATEST ? 1 : -1);

    /* Still should check if both are NaN since comparisons involved NaN are always false */
    if (is_nan1 && is_nan2) return 0;

    return (value2 > value1 ? 1 : -1);
}

int TupleSerializer::compare(void* buf1, size_t size1, void* buf2, size_t size2)
{
    if (buf1 == buf2) return 0;

    int length = header -> size();

    bit_set bs1((char *)buf1 + bit_set_offset, length);
    bit_set bs2((char *)buf2 + bit_set_offset, length);

    int offset = sizeof(int64_t);
    int result = 0;

    for (int i=0; i < length; i++)
    {
        orb_common_type  &ct = header -> at(i);
        if (!ct.initialized) continue;
        orb_modifier &m  = modifiers -> at(i);
        xmlscm_type type = ct.xtype;
        int type_size = ct.size;

        int order = m.order == orb_modifier::ORB_ASCENDING ? -1 : 1;
        bool is_eos1 = bs1.testAt(i);
        bool is_eos2 = bs2.testAt(i);

        if     (is_eos1 && !is_eos2)                   /// there we have (j)-th is eos and (j-1)-th is not eos
            result = (m.status == orb_modifier::ORB_EMPTY_GREATEST ? -1 : 1) * order;
        else if (is_eos2 && !is_eos1)                  /// there we have (j-1)-th is eos and (j)-th is not eos
            result = (m.status == orb_modifier::ORB_EMPTY_GREATEST ? 1 : -1) * order;

        else if (!is_eos2 && !is_eos1)
        {
            switch (type)
            {
            case xs_float                :
            {
                float value1, value2;
                get_deserialized_value(&value1, (char *)buf1 + offset, xs_float);
                get_deserialized_value(&value2, (char *)buf2 + offset, xs_float);
                result = compare_doubles((double)value1, (double)value2, m.status) * order;
                break;
            }
            case xs_double               :
            {
                double value1, value2;
                get_deserialized_value(&value1, (char *)buf1 + offset, xs_double);
                get_deserialized_value(&value2, (char *)buf2 + offset, xs_double);
                result = compare_doubles(value1, value2, m.status) * order;
                break;
            }
            case xs_decimal              :
            {
                xs_decimal_t value1, value2;
                get_deserialized_value(&value1, (char *)buf1 + offset, xs_decimal);
                get_deserialized_value(&value2, (char *)buf2 + offset, xs_decimal);
                if (value2 == value1) result = 0;
                else result = (value2 > value1 ? 1 : -1) * order;
                break;
            }
            case xs_integer              :
            {
                int64_t value1, value2;
                get_deserialized_value(&value1, (char *)buf1 + offset, xs_integer);
                get_deserialized_value(&value2, (char *)buf2 + offset, xs_integer);
                if (value2 == value1) result = 0;
                else result = (value2 > value1 ? 1 : -1) * order;
                break;
            }
            case xs_boolean              :
            {
                bool value1, value2;
                get_deserialized_value(&value1, (char *)buf1 + offset, xs_boolean);
                get_deserialized_value(&value2, (char *)buf2 + offset, xs_boolean);
                if (value2 && !value1) result = 1 * order;
                if (value1 && !value2) result = -1 * order;
                break;
            }
            case xs_string               :
            {
                bool flag1, flag2;
                get_deserialized_value(&flag1, (char *)buf1 + offset, xs_boolean);
                get_deserialized_value(&flag2, (char *)buf2 + offset, xs_boolean);
                result = sign(strcmp((char*)buf2 + offset + sizeof(bool), (char*)buf1 + offset + sizeof(bool)) * order);
                if (result == 0 && (!flag1 || !flag2))
                {
                    if      (!flag1 && flag2) result = -1 * order;
                    else if (!flag2 && flag1) result =  1 * order;
                    else /// both strings are not fully serialized !
                    {
                        int64_t position1, position2;
                        get_deserialized_value(&position1, (char *)buf1, xs_integer);
                        get_deserialized_value(&position2, (char *)buf2, xs_integer);
                        xqp_tuple t(length);
                        sort -> get(t, position1);
                        tuple_cell tc = t.cells[i];
                        sort -> get(t, position2);
                        result = fn_compare(t.cells[i], tc, m.collation) * order;
                    }
                }
                break;
            }
            case xs_time                 :
            case xs_date                 :
            case xs_dateTime             :
            case xs_yearMonthDuration    :
            case xs_dayTimeDuration      :
            {
                if (type == xs_yearMonthDuration || type == xs_dayTimeDuration)
                {
                    XMLDateTime value1(*(xs_packed_duration*)((char*)buf1 + offset), type);
                    XMLDateTime value2(*(xs_packed_duration*)((char*)buf2 + offset), type);
                    result = XMLDateTime::compare(value2, value1) * order;
                }
                else
                {
                    XMLDateTime value1(*(xs_packed_datetime*)((char*)buf1 + offset), type);
                    XMLDateTime value2(*(xs_packed_datetime*)((char*)buf2 + offset), type);
                    result = XMLDateTime::compare(value2, value1)*order;
                }
                break;
            }
            default                      :
                throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type.");
            }
        }
        if (result != 0) break;
        offset += type_size;
    }

    if (result == 0 && stable)
    {
        int64_t position1, position2;
        get_deserialized_value(&position1, (char *)buf1, xs_integer);
        get_deserialized_value(&position2, (char *)buf2, xs_integer);
        if (position1 == position2) return 0;
        result = position1 > position2 ? 1 : -1;
    }

    return result;
}
