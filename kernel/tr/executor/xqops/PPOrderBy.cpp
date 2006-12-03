/*
 * File:  PPOrderBy.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <iostream>
#include "sedna.h"
#include "PPOrderBy.h"

using namespace std;


/// Returns the least common type that has a gt operator.
/// Throws XPTY0004 if least common type doesn't have a gt operator.

xmlscm_type get_least_common_type_with_gt(xmlscm_type t1, xmlscm_type t2)
{
    xmlscm_type t = evaluate_common_type(t1, t2);    
    
    if(t == xs_string  || is_derived_from_xs_string(t) || t == xs_anyURI)  
        return xs_string;
    if(t == xs_integer || is_derived_from_xs_integer(t)) 
        return xs_integer; 
    
    switch(t)    
    {   
        case xs_time                  :
        case xs_date                  :
        case xs_dateTime              :
        case xs_yearMonthDuration     : 
        case xs_dayTimeDuration       : 
        case xs_float                 : 
        case xs_double                : 
        case xs_decimal               : 
        case xs_boolean               : return t;
        default                       : throw USER_EXCEPTION2(XPTY0004, "Least common type doesn't have a gt operator (PPOrderBy).");
    }    
}



///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPOrderBy
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPOrderBy::PPOrderBy(variable_context *_cxt_,
                     bool _stable_,
                     PPOpIn _child_,
                     arr_of_orb_modifier _modifiers_,
                     int _data_size_) : PPIterator(_cxt_),
                                        stable(_stable_),
                                        child(_child_),
                                        modifiers(_modifiers_),
                                        data_size(_data_size_)
{
    if(modifiers.size() != child.ts - data_size) 
        throw USER_EXCEPTION2(SE1003, "Number of modifiers must be equal to the expressions number (PPOrderBy).");
    
    sort_size = child.ts - data_size;
    types.resize(sort_size);
}

PPOrderBy::~PPOrderBy()
{
    delete child.op;
    child.op = NULL;
}

void PPOrderBy::open  ()
{
    child.op->open();
    first_time  = true;
    need_reinit = false;
    need_to_sort= false;
    pos = 0;
    
    data_cells  = new sequence_tmp(data_size);
    sort_cells  = new sequence_tmp(sort_size); 

    udata.sort      = sort_cells;
    udata.pos       = 0;
    udata.header    = &types;
    udata.modifiers = &modifiers;
    udata.size      = sizeof(__int64);            
    udata.buffer    = NULL;

    ss = new sorted_sequence(compare,get_size,serialize,serialize_2_blks,deserialize,deserialize_2_blks,&udata);
}

void PPOrderBy::reopen()
{
    child.op->reopen();
    first_time  = true;
    need_reinit = true;
}

void PPOrderBy::close ()
{
    child.op->close();
    delete data_cells;
    data_cells = NULL;
    delete sort_cells;
    sort_cells = NULL;
    udata.sort = NULL;
    delete ss;
    ss = NULL;
    if(udata.buffer != NULL) 
    {
        delete udata.buffer;
        udata.buffer = NULL;
    }
}

void PPOrderBy::next  (tuple &t)
{
    if(first_time)
    {
        if(need_reinit)
        {
            data_cells -> clear();
            sort_cells -> clear();
            ss         -> clear();
            pos = 0;
            udata.pos   = 0;
            udata.size  = sizeof(__int64);
            need_reinit = false;
            need_to_sort= false;
        }

        int i;
        tuple data_tuple(data_size);
        tuple sort_tuple(sort_size);
        tuple source(child.ts);

        for(i = 0; i < sort_size; i++) types.at(i).initialized = false;

        while (true)
        {
            child.op -> next(source);
            if (source.is_eos()) break;

            for(i = 0; i < source.cells_number; i++)
            {    
                if(i < data_size)  data_tuple.cells[i] = source.cells[i];
                else                
                {
                    if(source.cells[i].is_eos()) 
                        sort_tuple.cells[i - data_size].set_eos();
                    else
                    {
                        tuple_cell tc = atomize(source.cells[i]);
                        sort_tuple.cells[i - data_size] = tc.get_atomic_type() == xs_untypedAtomic ? 
                                                          cast_primitive_to_xs_string(tc) : tc ;
                        
                        common_type* ct = &types.at(i - data_size);
                        xmlscm_type t = sort_tuple.cells[i - data_size].get_atomic_type();
                        
                        if(ct->initialized)
                            ct->xtype = get_least_common_type_with_gt(ct->xtype, t);
                        else
                        {
                            ct->xtype = t;
                            ct->initialized = true;
                        }
                    }
                }
            }
            
            data_cells -> add(data_tuple);
            sort_cells -> add(sort_tuple);
        }
        
        for(i = 0; i < sort_size; i++)
        {
            common_type* ct = &types.at(i);
            if(!ct->initialized) continue;
            ct->size = ORB_SERIALIZED_SIZE(ct->xtype);
            udata.size += ct->size;
            need_to_sort = true;
        }

        if(need_to_sort)
        {
            udata.bit_set_offset = udata.size;         // offset to the begining of the eos map in each serialized tuple
            udata.size += sort_size / 8;               // additional bytes for serialized bit_set which contains eos bitmap
            if(sort_size % 8 != 0) udata.size++;

            if(udata.size > DATA_BLK_SIZE) 
                throw USER_EXCEPTION2(SE1003, "Too long order by specification (PPOrderBy).");

            if(udata.buffer != NULL) delete udata.buffer;
            udata.buffer = new temp_buffer(udata.size);

            for(i = 0; i < sort_cells->size(); i++)
            {
                sort_cells -> get(sort_tuple, i);
                ss -> add(sort_tuple);
                udata.pos ++;
            }

            ss -> sort();
        }
        first_time = false;
    }
    
    if (pos < (need_to_sort ? ss->size() : data_cells->size())) 
    {
        if(need_to_sort) {
            ss->get(t,pos++);
            if(t.cells[0].get_atomic_type() != xs_integer) 
                throw USER_EXCEPTION2(SE1003, "Incorrect serialization/deserialization (PPOrderBy).");
        }
        data_cells -> get(t, need_to_sort ? t.cells[0].get_xs_integer() : pos++);
    }
    else 
    {
        t.set_eos();
        first_time  = true;
        need_reinit = true;
    }
}

PPIterator* PPOrderBy::copy(variable_context *_cxt_)
{
    PPOrderBy *res = new PPOrderBy(_cxt_, 
                                   stable, 
                                   child, 
                                   modifiers, 
                                   data_size);

    res->child.op = child.op->copy(_cxt_);
    return res;
}

bool PPOrderBy::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPOrderBy::result");
}





///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPOrderBy static methods needed by sorted sequence
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

void serialize_string(const tuple_cell& tc, void* dest)
{
    __int64 length_all = tc.get_strlen();
    int length_ser = length_all < ORB_STRING_PREFIX_SIZE ? length_all : ORB_STRING_PREFIX_SIZE;
    bool flag = (length_all <= ORB_STRING_PREFIX_SIZE);
    memcpy(dest, &flag, sizeof(bool));
    tc.copy_string((char*)dest + sizeof(bool), length_ser);
    *((char*)dest + sizeof(bool) + length_ser) = '\0';
}

void* get_ptr_to_complete_serialized_data(xptr v, char** temp,  const void * Udata)
{
    orb_user_data* ud = (orb_user_data*)Udata;
    CHECKP(v);
    int sz = GET_FREE_SPACE(v);
    if(sz < ud->size)
    {
        *temp = new char[ud->size];
        memcpy(*temp, XADDR(v), sz);
        xptr nblk=((seq_blk_hdr*)XADDR(BLOCKXPTR(v)))->nblk+sizeof(seq_blk_hdr);
        CHECKP(nblk);
        memcpy((*temp)+sz, XADDR(nblk), (ud->size) - sz);
        return *temp;
    }
    else return XADDR(v);    
}

void get_deserialized_value(void* value, const void* addr, xmlscm_type type)
{
    #ifdef ALIGNMENT_REQUIRED
        memcpy(value, addr, ORB_SERIALIZED_SIZE(type));
    #else
        switch(type)
        {
            case xs_float                  : *((float*)value) = *((float*)addr); break;
            case xs_double                 : *((double*)value) = *((double*)addr); break;
            case xs_decimal                : *((xs_decimal_t*)value) = *((xs_decimal_t*)addr); break;
            case xs_integer                : *((__int64*)value) = *((__int64*)addr); break;
            case xs_boolean                : *((bool*)value) = *((bool*)addr); break;
            default                        : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type or deserialization is not implemented (PPOrderBy).");
        }    
    #endif
}


static inline int compare_doubles(double value1, double value2, orb_empty_status o)
{
    if (value2 == value1) return 0;
    
    bool is_nan1 = u_is_nan(value1);
    bool is_nan2 = u_is_nan(value2);

    if(is_nan1 && !is_nan2) return (o == ORB_EMPTY_GREATEST ? -1 : 1);
    if(is_nan2 && !is_nan1) return (o == ORB_EMPTY_GREATEST ? 1 : -1);
    
    return (value2 > value1 ? 1 : -1);
} 

//////////////////////////////////////////////////////////////
/// v2 points to (j-1)-th element
/// v1 points to (j)-th element
/// if compare(j-1, j) < 0 then they are swapped
//////////////////////////////////////////////////////////////
int PPOrderBy::compare (xptr v1, xptr v2, const void * Udata)
{
    orb_user_data* ud = (orb_user_data*)Udata;
    int length = (ud->header)->size();
    
    char* temp1 = NULL;
    char* temp2 = NULL; 
    void* addr1;
    void* addr2;
    
    addr1 = get_ptr_to_complete_serialized_data(v1, &temp1, Udata);
    addr2 = get_ptr_to_complete_serialized_data(v2, &temp2, Udata);
    
    if(temp1 == NULL) CHECKP(v1);
    bit_set bs1((char *)addr1+ud->bit_set_offset, length);
    if(temp2 == NULL) CHECKP(v2);
    bit_set bs2((char *)addr2+ud->bit_set_offset, length);

    int offset = sizeof(__int64);
    int result = 0;

    for(int i=0; i < length; i++)
    {
        common_type  &ct = (ud -> header) -> at(i);
        if (!ct.initialized) continue;
        orb_modifier &m  = (ud -> modifiers) -> at(i);
        xmlscm_type type = ct.xtype;
        int type_size = ct.size;        

        int order = m.order == ORB_ASCENDING ? -1 : 1;
        if(temp1 == NULL) CHECKP(v1);
        bool is_eos1 = bs1.testAt(i);
        if(temp2 == NULL) CHECKP(v2);
        bool is_eos2 = bs2.testAt(i);

        if     (is_eos1 && !is_eos2)                   /// there we have (j)-th is eos and (j-1)-th is not eos
            result = (m.status == ORB_EMPTY_GREATEST ? -1 : 1) * order;
        else if(is_eos2 && !is_eos1)                   /// there we have (j-1)-th is eos and (j)-th is not eos    
            result = (m.status == ORB_EMPTY_GREATEST ? 1 : -1) * order;
        
        else if(!is_eos2 && !is_eos1)        
        {
            switch (type)
            {
                case xs_float                : 
                {
                    float value1, value2;
                    if(temp1 == NULL) CHECKP(v1);
                    get_deserialized_value(&value1, (char*)addr1+offset, xs_float);
                    if(temp2 == NULL) CHECKP(v2);
                    get_deserialized_value(&value2, (char*)addr2+offset, xs_float);
                    result = compare_doubles((double)value1, (double)value2, m.status) * order;
                    break;
                }
                case xs_double               : 
                {
                    double value1, value2;
                    if(temp1 == NULL) CHECKP(v1);
                    get_deserialized_value(&value1, (char*)addr1+offset, xs_double);
                    if(temp2 == NULL) CHECKP(v2);
                    get_deserialized_value(&value2, (char*)addr2+offset, xs_double);
                    result = compare_doubles(value1, value2, m.status) * order;
                    break;
                }
                case xs_decimal              : 
                {
                    xs_decimal_t value1, value2;
                    if(temp1 == NULL) CHECKP(v1);
                    get_deserialized_value(&value1, (char*)addr1+offset, xs_decimal);
                    if(temp2 == NULL) CHECKP(v2);
                    get_deserialized_value(&value2, (char*)addr2+offset, xs_decimal);
                    if (value2 == value1) result = 0;
                    else result = (value2 > value1 ? 1 : -1) * order;
                    break;
                }
                case xs_integer              : 
                {
                    __int64 value1, value2;
                    if(temp1 == NULL) CHECKP(v1);
                    get_deserialized_value(&value1, (char*)addr1+offset, xs_integer);
                    if(temp2 == NULL) CHECKP(v2);
                    get_deserialized_value(&value2, (char*)addr2+offset, xs_integer);
                    if (value2 == value1) result = 0;
                    else result = (value2 > value1 ? 1 : -1) * order;
                    break;
                }
                case xs_boolean              : 
                {
                    bool value1, value2;
                    if(temp1 == NULL) CHECKP(v1);
                    get_deserialized_value(&value1, (char*)addr1+offset, xs_boolean);
                    if(temp2 == NULL) CHECKP(v2);
                    get_deserialized_value(&value2, (char*)addr2+offset, xs_boolean);
                    if(value2 && !value1) result = 1 * order;
                    if(value1 && !value2) result = -1 * order;
                    break;
                }
                case xs_string               :
                {
                    bool flag1, flag2;
                    if(temp1 != NULL || temp2 !=NULL)
                    {
                        if(temp1 != NULL) CHECKP(v1);
                        if(temp2 != NULL) CHECKP(v2);
                        get_deserialized_value(&flag1, (char*)addr1+offset, xs_boolean);
                        get_deserialized_value(&flag2, (char*)addr2+offset, xs_boolean);
                        result = strcmp((char*)addr2+offset+sizeof(bool), (char*)addr1+offset+sizeof(bool))*order;
                    }
                    else 
                    {
                        char* prefix = new char[ORB_STRING_PREFIX_SIZE + 1]; 
                        CHECKP(v1);
                        get_deserialized_value(&flag1, (char*)addr1+offset, xs_boolean);
                        strcpy(prefix, (char*)addr1+offset+sizeof(bool));
                        CHECKP(v2);
                        get_deserialized_value(&flag2, (char*)addr2+offset, xs_boolean);
                        result = strcmp((char*)addr2+offset+sizeof(bool), prefix)*order;
                        delete prefix;
                    }
                    if (result == 0 && (!flag1 || !flag2))
                    {
                        __int64 position1;
                        __int64 position2;
                        if(temp1 != NULL) CHECKP(v1);
                        get_deserialized_value(&position1, addr1, xs_integer);
                        if(temp2 != NULL) CHECKP(v2);
                        get_deserialized_value(&position2, addr2, xs_integer);
                        tuple t1(length);
                        tuple t2(length);
                        ud->sort->get(t1, position1);
                        ud->sort->get(t2, position2);
                        result = fn_compare(t2.cells[i], t1.cells[i])*order;
                    }
                    break;
                }
                case xs_time                 :
                case xs_date                 :
                case xs_dateTime             :
                case xs_yearMonthDuration    : 
                case xs_dayTimeDuration      : 
                {
                    if(temp1 != NULL || temp2 != NULL)
                    {
                        if(temp1 != NULL) CHECKP(v1);
                        if(temp2 != NULL) CHECKP(v2);
                        XMLDateTime value1(*(xs_packed_duration*)((char*)addr1+offset), type);
                        XMLDateTime value2(*(xs_packed_duration*)((char*)addr2+offset), type);
                        result = XMLDateTime::compare(value2, value1)*order;
                    }
                    else
                    {
                        char* buffer = new char[type_size];    
                        CHECKP(v1);
                        memcpy(buffer, (char*)addr1+offset, type_size);
                        XMLDateTime value1(*(xs_packed_duration*)buffer, type);
                        CHECKP(v2);
                        XMLDateTime value2(*(xs_packed_duration*)((char*)addr2+offset), type);
                        result = XMLDateTime::compare(value2, value1)*order;
                        delete buffer;
                    }
                    break;
                }
                default                      : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type or serialization is not implemented (PPOrderBy).");
            }
        }
        if(result != 0) break;
        offset += type_size;
    }
    if(temp1 != NULL) {    delete temp1; temp1 = NULL;    }
    if(temp2 != NULL) {    delete temp2; temp2 = NULL;    }
	
	return result;
}

int PPOrderBy::get_size (tuple& t, const void * Udata)
{
    return ((orb_user_data*)Udata) -> size;
}

void PPOrderBy::serialize (tuple& t, xptr v1, const void * Udata)
{
    orb_user_data* ud = (orb_user_data*)Udata;
    __int64 pos = ud  -> pos;
    bit_set bs((ud -> header) -> size());
        
    #ifdef ALIGNMENT_REQUIRED

        temp_buffer* buffer = ud -> buffer;
        buffer->clear();
        buffer->copy_to_buffer(&pos, sizeof(__int64));
        for(int i = 0; i < t.cells_number; i++)
        {
            common_type &ct = (ud -> header) -> at(i);
            if (!ct.initialized) continue;              //if ct.initialized == false we have a column of eos values
            xmlscm_type type = ct.xtype;                //thus we don't need to serialize this column and sort by it
            int type_size = ct.size;

            if(t.cells[i].is_eos()) bs.setAt(i);
            else 
            {
                if(t.cells[i].get_atomic_type() != type) t.cells[i] = cast(t.cells[i], type);
                buffer->serialize_to_buffer(t.cells[i]);
            }
        }
        buffer->copy_to_buffer(bs.get_ptr_to_bytes(), bs.get_size_in_bytes());
        buffer->copy_from_buffer(v1);
    
    #else
    
        CHECKP(v1);
        VMM_SIGNAL_MODIFICATION(v1);
        void* p = XADDR(v1);
        *((__int64 *)p) = pos;
        int offset = sizeof(__int64);
        for(int i = 0; i < t.cells_number; i++)
        {
            common_type &ct = (ud -> header) -> at(i);
            if (!ct.initialized) continue;
            xmlscm_type type = ct.xtype;
            int type_size = ct.size;

            if(t.cells[i].is_eos()) bs.setAt(i);
            else 
            {
                if(t.cells[i].get_atomic_type() != type) t.cells[i] = cast(t.cells[i], type);
                
                switch (type)
                {
                    case xs_float                : *((float*)((char*)p+offset)) = t.cells[i].get_xs_float(); break;
                    case xs_double               : *((double*)((char*)p+offset)) = t.cells[i].get_xs_double(); break;
                    case xs_decimal              : *((xs_decimal_t*)((char*)p+offset)) = t.cells[i].get_xs_decimal(); break;
                    case xs_integer              : *((__int64*)((char*)p+offset)) = t.cells[i].get_xs_integer(); break;
                    case xs_boolean              : *((bool*)((char*)p+offset)) = t.cells[i].get_xs_boolean(); break;
                    case xs_string               : 
                    {
                        serialize_string(t.cells[i], (char*)p+offset); 
                        if(!t.cells[i].is_light_atomic())
                        {
                            CHECKP(v1);
                            VMM_SIGNAL_MODIFICATION(v1);
                        }
                        break;
                    }
                    case xs_time                 :
                    case xs_date                 :
                    case xs_dateTime             :
                    case xs_yearMonthDuration    : 
                    case xs_dayTimeDuration      : memcpy((char*)p+offset, &(t.cells[i].get_xs_duration()), type_size); break;
                    default                      : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type or serialization is not implemented (PPOrderBy).");
                }
            }
            offset += type_size;
            memcpy((char*)p + offset, bs.get_ptr_to_bytes(), bs.get_size_in_bytes());
        }

    #endif
}

void PPOrderBy::serialize_2_blks (tuple& t, xptr& v1, shft size1, xptr& v2, const void * Udata)
{
    orb_user_data* ud = (orb_user_data*)Udata;
    __int64 pos = ud  -> pos;
    bit_set bs((ud -> header) -> size());
    
    temp_buffer* buffer = ud -> buffer;
    buffer->clear();
    buffer->copy_to_buffer(&pos, sizeof(__int64));
    for(int i = 0; i < t.cells_number; i++)
    {
        common_type &ct = (ud -> header) -> at(i);
        if (!ct.initialized) continue;
        xmlscm_type type = ct.xtype;
        int type_size = ct.size;

        if(t.cells[i].is_eos()) bs.setAt(i);
        else 
        {
            if(t.cells[i].get_atomic_type() != type) t.cells[i] = cast(t.cells[i], type);
            buffer->serialize_to_buffer(t.cells[i]);
        }
    }
    buffer->copy_to_buffer(bs.get_ptr_to_bytes(), bs.get_size_in_bytes());
    buffer->copy_from_buffer(0, size1, v1);
    buffer->copy_from_buffer(size1, v2);
}

void PPOrderBy::deserialize (tuple& t, xptr& v1, const void * Udata)
{
    CHECKP(v1);
    void* p = XADDR(v1);
    __int64 pos;

    #ifdef ALIGNMENT_REQUIRED
        memcpy(&pos, p, sizeof(__int64));
    #else
        pos = *((__int64*)p);
    #endif

    t.cells[0] = tuple_cell::atomic((__int64)pos);
}

void PPOrderBy::deserialize_2_blks (tuple& t, xptr& v1, shft size1, xptr& v2, const void * Udata)
{
    if(size1 < sizeof(__int64))
    {
        __int64 pos;
        temp_buffer* buffer = ((orb_user_data*)Udata) -> buffer;
        buffer->clear();
        buffer->copy_to_buffer(v1, size1);
        buffer->copy_to_buffer(v2, sizeof(__int64)-size1);
        buffer->copy_from_buffer(&pos);
        t.cells[0] = tuple_cell::atomic((__int64)pos);
    }
    else
        deserialize(t, v1, Udata);
}






///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// temp_buffer
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

temp_buffer::temp_buffer (int _size_): size(_size_),
                                       pos(0)
{
    if(size <= 0) 
        throw USER_EXCEPTION2(SE1003, "Buffer size must be positive (PPOrderBy::temp_buffer).");            
    buffer = new char[size]; 
}
    
temp_buffer::~temp_buffer ()
{
    delete buffer;
    buffer = NULL;
}

void temp_buffer::clear ()
{
    pos = 0;    
}

void temp_buffer::serialize_to_buffer (const tuple_cell& tc)
{
    if(tc.is_eos()) return;
    U_ASSERT(tc.is_atomic());
    xmlscm_type type = tc.get_atomic_type();

    int type_size = ORB_SERIALIZED_SIZE(type);

    switch (type)
    {
        case xs_float                : {float value = tc.get_xs_float(); memcpy(buffer + pos, &value, type_size); break;}
        case xs_double               : {double value = tc.get_xs_double(); memcpy(buffer + pos, &value, type_size);  break;}
        case xs_decimal              : {xs_decimal_t value = tc.get_xs_decimal(); memcpy(buffer + pos, &value, type_size); break;}
        case xs_integer              : {__int64 value = tc.get_xs_integer(); memcpy(buffer + pos, &value, type_size); break;}
        case xs_boolean              : {bool value = tc.get_xs_boolean(); memcpy(buffer + pos, &value, type_size); break;}
        case xs_string               : {serialize_string(tc, buffer+pos); break; }        
        case xs_time                 :
        case xs_date                 :
        case xs_dateTime             :
        case xs_yearMonthDuration    : 
        case xs_dayTimeDuration      : {memcpy(buffer + pos, &(tc.get_xs_duration()), type_size); break;}
        default                      : throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type or serialization is not implemented (PPOrderBy).");
    }

    pos += type_size;
}


///////////////////////////////////////////////////////////////////////////////

void temp_buffer::copy_to_buffer      (const void* addr, int size)        
{
    memcpy(buffer + pos, addr, size);
    pos += size;
}

void temp_buffer::copy_to_buffer      (xptr addr, int size)
{
    CHECKP(addr);
    copy_to_buffer(XADDR(addr), size);
}

///////////////////////////////////////////////////////////////////////////////

void temp_buffer::copy_from_buffer    (int start, int size, xptr addr)
{
    CHECKP(addr);
    VMM_SIGNAL_MODIFICATION(addr);
    memcpy(XADDR(addr), buffer+start, size);    
}

void temp_buffer::copy_from_buffer    (int start, xptr addr)
{
    copy_from_buffer(start, pos-start, addr);
}

void temp_buffer::copy_from_buffer    (xptr addr)
{
    copy_from_buffer(0, pos, addr);
}

///////////////////////////////////////////////////////////////////////////////

void temp_buffer::copy_from_buffer      (int start, int size, void* addr)        
{
    memcpy(addr, buffer+start, size);    
}

void temp_buffer::copy_from_buffer      (int start, void* addr)
{
    copy_from_buffer(start, pos-start, addr);
}

void temp_buffer::copy_from_buffer      (void* addr)
{
    copy_from_buffer(0, pos, addr);
}


///////////////////////////////////////////////////////////////////////////////

void temp_buffer::create_empty_block  (int size)
{
    create_empty_block(pos, size);
}

void temp_buffer::create_empty_block  (int start, int size)
{
    memset(buffer + start, '0', size);    
}
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
