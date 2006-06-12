/*
 * File:  PPOrderBy.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <iostream>
#include "sedna.h"
#include "PPOrderBy.h"

using namespace std;


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
		throw USER_EXCEPTION2(SE1003, "Number of modifiers must be equal to the expressions number in PPOrderBy.");
	
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
    pos = 0;
    
    data_cells  = new sequence(data_size);
    sort_cells  = new sequence(sort_size); 

    udata.sort      = sort_cells;
    udata.pos       = 0;
    udata.header    = &types;
    udata.modifiers = &modifiers;
    udata.size      = sizeof(int);			
    udata.buffer    = NULL;

    ss = new sorted_sequence(compare_less,get_size,serialize,serialize_2_blks,deserialize,deserialize_2_blks,&udata);
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
   		    udata.pos = 0;
		    udata.size = sizeof(int);
		    need_reinit = false;
    	}

	   	int i;
	   	tuple data_tuple(data_size);
        tuple sort_tuple(sort_size);

        for(i = 0; i < sort_size; i++) types.at(i).initialized = false;

    	while (true)
        {
            child.op -> next(t);
            if (t.is_eos()) break;
            if (child.ts != t.cells_number) 
            	throw USER_EXCEPTION2(SE1003, "Incorrect size of the arrived tuple in PPOrderBy.");
            
            for(i = 0; i < t.cells_number; i++)
            {	
            	if(i < data_size)	data_tuple.cells[i] = t.cells[i];
            	else                
            	{
            		if(t.cells[i].is_eos()) 
            			sort_tuple.cells[i - data_size].set_eos();
            		else
            		{
            			tuple_cell tc = atomize(t.cells[i]);
            			sort_tuple.cells[i - data_size] = tc.get_atomic_type() == xdt_untypedAtomic ? 
	            	 									  cast_to_xs_string(tc) : tc ;
        	    		
        	    		common_type* ct = &types.at(i - data_size);
	            		xmlscm_type t = sort_tuple.cells[i - data_size].get_atomic_type();
            			
            			if(ct->initialized)
	            			ct->xtype = evaluate_common_type(ct->xtype, t);
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
        	ct->size = ORB_SERIALIZED_SIZE(ct->xtype);
        	udata.size += ct->size;
        	//ct->gt = get_binary_op(xqbop_gt, ct->xtype, ct->xtype);
        }

        udata.bit_set_offset = udata.size;		   // offset to the begin of the eos map in each serialized tuple
        udata.size += sort_size / 8;               // additional bytes for serialized bit_set which contains eos bitmap
        if(sort_size % 8 != 0) udata.size++;

        if(udata.size > DATA_BLK_SIZE) 
	        throw USER_EXCEPTION2(SE1003, "Too long order by specification.");

      	if(udata.buffer != NULL) delete udata.buffer;
	    udata.buffer = new temp_buffer(udata.size);

        for(i = 0; i < sort_cells->size(); i++)
        {
        	sort_cells -> get(sort_tuple, i);
        	ss -> add(sort_tuple);
        	udata.pos ++;
        }
        
        first_time = false;
    }
    
    if (pos < ss->size()) 
    {
    	ss->get(t,pos++);
    	if(t.cells[0].get_atomic_type() != xs_integer) 
    		throw USER_EXCEPTION2(SE1003, "Incorrect serialization/deserialization in PPOrderBy.");
    	data_cells -> get(t, t.cells[0].get_xs_integer());
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
			case xs_float				: *((float*)value) = *((float*)addr); break;
   		 	case xs_double				: 
   	    	case xs_decimal				: *((double*)value) = *((double*)addr); break;
		    case xs_integer				: *((int*)value) = *((int*)addr); break;
		    case xs_boolean				: *((bool*)value) = *((bool*)addr); break;
			default						: throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type or deserialization is not implemented in order by.");
		}	
	#endif
}

int PPOrderBy::compare_less (xptr v1, xptr v2, const void * Udata)
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

	int offset = sizeof(int);
	int result = 0;

	for(int i=0; i< length; i++)
	{
		common_type  &ct = (ud -> header) -> at(i);
		orb_modifier &m  = (ud -> modifiers) -> at(i);
		xmlscm_type type = ct.xtype;
		int type_size = ct.size;		

		int order = m.order == ORB_ASCENDING ? 1 : -1;
		if(temp1 == NULL) CHECKP(v1);
		bool is_eos1 = bs1.testAt(i);
		if(temp2 == NULL) CHECKP(v2);
		bool is_eos2 = bs2.testAt(i);

		if(is_eos1 && !is_eos2)
		{
			if(m.status == ORB_EMPTY_GREATEST) result = -1 * order;
			else result = 1 * order;
		}
		else if(is_eos2 && !is_eos1)		
		{
		    if(m.status == ORB_EMPTY_GREATEST) result = 1 * order;
			else result = -1 * order;
		}
		else if(!is_eos2 && !is_eos1)		
		{
			switch (type)
	  		{
		        case xs_float				: 
	    	    {
		        	float value1, value2;
	    	    	if(temp1 == NULL) CHECKP(v1);
	        		get_deserialized_value(&value1, (char*)addr1+offset, xs_float);
	        		if(temp2 == NULL) CHECKP(v2);
		        	get_deserialized_value(&value2, (char*)addr2+offset, xs_float);
		        	result = (value2-value1)*order;
		        	break;
	        	}
	    	    case xs_double				: 
		        case xs_decimal				: 
		        {
   		    	    double value1, value2;
	    	    	if(temp1 == NULL) CHECKP(v1);
	        		get_deserialized_value(&value1, (char*)addr1+offset, xs_double);
	        		if(temp2 == NULL) CHECKP(v2);
		        	get_deserialized_value(&value2, (char*)addr2+offset, xs_double);
		        	result = (value2-value1)*order;
		        	break;
		        }
	    	    case xs_integer				: 
		        {
   		    	    int value1, value2;
	    	    	if(temp1 == NULL) CHECKP(v1);
	        		get_deserialized_value(&value1, (char*)addr1+offset, xs_integer);
	        		if(temp2 == NULL) CHECKP(v2);
		        	get_deserialized_value(&value2, (char*)addr2+offset, xs_integer);
		        	result = (value2-value1)*order;
		        	break;
		        }
	        	case xs_boolean				: 
		        {
   		    	    bool value1, value2;
	    	    	if(temp1 == NULL) CHECKP(v1);
	        		get_deserialized_value(&value1, (char*)addr1+offset, xs_boolean);
	        		if(temp2 == NULL) CHECKP(v2);
		        	get_deserialized_value(&value2, (char*)addr2+offset, xs_boolean);
		        	if(value2 && !value1) result = 1*order;
		        	if(value1 && !value2) result = -1*order;
		        	break;
		        }
				default						: throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type or serialization is not implemented in order by.");
			}
		}
		if(result != 0) break;
		offset += type_size;
	}
	if(temp1 != NULL) {	delete temp1; temp1 = NULL;	}
	if(temp2 != NULL) {	delete temp2; temp2 = NULL;	}
	return result;
}

int PPOrderBy::get_size (tuple& t, const void * Udata)
{
	return ((orb_user_data*)Udata) -> size;
}

void PPOrderBy::serialize (tuple& t, xptr v1, const void * Udata)
{
	orb_user_data* ud = (orb_user_data*)Udata;
	int pos = ud  -> pos;
	bit_set bs((ud -> header) -> size());
		
	#ifdef ALIGNMENT_REQUIRED

		temp_buffer* buffer = ud -> buffer;
		buffer->clear();
		buffer->copy_to_buffer(&pos, sizeof(int));
		for(int i = 0; i < t.cells_number; i++)
		{
			common_type &ct = (ud -> header) -> at(i);
			xmlscm_type type = ct.xtype;
			int type_size = ct.size;

			if(t.cells[i].is_eos()) 
			{
				bs.setAt(i);
				buffer->create_empty_block(type_size);
			}
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
		*((int *)p) = pos;
		int offset = sizeof(int);
		for(int i = 0; i < t.cells_number; i++)
		{
			common_type &ct = (ud -> header) -> at(i);
			xmlscm_type type = ct.xtype;
			int type_size = ct.size;


			if(t.cells[i].is_eos()) 
			{
				bs.setAt(i);
				memset((char*)p+offset, '0', type_size);
			}
			else 
			{
				if(t.cells[i].get_atomic_type() != type) t.cells[i] = cast(t.cells[i], type);
				
				switch (type)
	    		{
    	    		
			        case xs_float				: *((float*)((char*)p+offset)) = t.cells[i].get_xs_float(); break;
		    	    case xs_double				: *((double*)((char*)p+offset)) = t.cells[i].get_xs_double(); break;
			        case xs_decimal				: *((double*)((char*)p+offset)) = t.cells[i].get_xs_decimal().to_double(); break;
			        case xs_integer				: *((int*)((char*)p+offset)) = t.cells[i].get_xs_integer(); break;
			        case xs_boolean				: *((bool*)((char*)p+offset)) = t.cells[i].get_xs_boolean(); break;
					default						: throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type or serialization is not implemented in order by.");
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
	int pos = ud  -> pos;
	bit_set bs((ud -> header) -> size());
	
	temp_buffer* buffer = ud -> buffer;
	buffer->clear();
	buffer->copy_to_buffer(&pos, sizeof(int));
	for(int i = 0; i < t.cells_number; i++)
	{
		common_type &ct = (ud -> header) -> at(i);
		xmlscm_type type = ct.xtype;
		int type_size = ct.size;

		if(t.cells[i].is_eos()) 
		{
			bs.setAt(i);
			buffer->create_empty_block(type_size);
		}
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
	int pos;

	#ifdef ALIGNMENT_REQUIRED
		memcpy(&pos, p, sizeof(int));
	#else
		pos = *((int*)p);
	#endif

	t.copy(tuple_cell::atomic(pos));
}

void PPOrderBy::deserialize_2_blks (tuple& t, xptr& v1, shft size1, xptr& v2, const void * Udata)
{
	if(size1 < sizeof(int))
	{
		int pos;
		temp_buffer* buffer = ((orb_user_data*)Udata) -> buffer;
		buffer->clear();
        buffer->copy_to_buffer(v1, size1);
        buffer->copy_to_buffer(v2, sizeof(int)-size1);
        buffer->copy_from_buffer(&pos);
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
		throw USER_EXCEPTION2(SE1003, "Buffer size must be positive in temp_buffer in PPOrderBy.");			
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

void temp_buffer::serialize_to_buffer (tuple_cell tc)
{
    if(tc.is_eos()) return;
    U_ASSERT(tc.is_atomic());
    xmlscm_type type = tc.get_atomic_type();

    switch (type)
    {
        case xs_float				: {float value = tc.get_xs_float(); memcpy(buffer + pos, &value, sizeof(float)); break;}
        case xs_double				: {double value = tc.get_xs_double(); memcpy(buffer + pos, &value, sizeof(double));  break;}
        case xs_decimal				: {double value = tc.get_xs_decimal().to_double(); memcpy(buffer + pos, &value, sizeof(double)); break;}
        case xs_integer				: {int value = tc.get_xs_integer(); memcpy(buffer + pos, &value, sizeof(int)); break;}
        case xs_boolean				: {bool value = tc.get_xs_boolean(); memcpy(buffer + pos, &value, sizeof(bool)); break;}
		default						: throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type or serialization is not implemented in order by.");
	}

	pos += ORB_SERIALIZED_SIZE(type);
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

void temp_buffer::copy_from_buffer	  (int start, int size, void* addr)        
{
	memcpy(addr, buffer+start, size);	
}

void temp_buffer::copy_from_buffer	  (int start, void* addr)
{
	copy_from_buffer(start, pos-start, addr);
}

void temp_buffer::copy_from_buffer	  (void* addr)
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
