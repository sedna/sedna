/*
 * File:  PPOrderBy.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <iostream>
#include "sedna.h"
#include "PPOrderBy.h"

using namespace std;

static char* temp_buffer = NULL;  
static int buf_lgth = 0;          


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
}

PPOrderBy::~PPOrderBy()
{
	delete child.op;
    child.op = NULL;
}

void PPOrderBy::open  ()
{
    child.op->open();
    first_time = true;
    pos = 0;
    
    data_cells  = new sequence(data_size);
    sort_cells  = new sequence(sort_size); 

    udata.sort      = sort_cells;
    udata.pos       = 0;
    udata.header    = &types;
    udata.modifiers = &modifiers;
    udata.size      = sizeof(int);

    ss = new sorted_sequence(compare_less,get_size,serialize,serialize_2_blks,deserialize,deserialize_2_blks,&udata);
}

void PPOrderBy::reopen()
{
    child.op->reopen();
    pos = 0;
    first_time = true;
    udata.pos = 0;
    udata.size = sizeof(int);
    data_cells  -> clear();
    sort_cells  -> clear();
    ss          -> clear();
}

void PPOrderBy::close ()
{
    child.op->close();
    delete data_cells;
    delete sort_cells;
    delete ss;
    udata.sort = NULL;
}

void PPOrderBy::next  (tuple &t)
{
    if(first_time)
    {
    	data_cells -> clear();
    	sort_cells -> clear();
    	ss         -> clear();
    	
    	int i;
    	tuple data_tuple(data_size);
        tuple sort_tuple(sort_size);
    	bool first_tuple = true;

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
            		sort_tuple.cells[i - data_size] = t.cells[i].get_atomic_type() == xdt_untypedAtomic ? 
            	 									  cast_to_xs_string(atomize(t.cells[i])) : 
            	 									  atomize(t.cells[i]) ;
            		common_type ct;
            		xmlscm_type t = sort_tuple.cells[i - data_size].get_atomic_type();
            		
            		if(first_tuple) 
            		{
            			ct.xtype = t;
            			types.push_back(ct);
            		}
            		else
            		{
            			ct = types.at(i - data_size);
            			ct.xtype = evaluate_common_type(ct.xtype, t);
            		}
            	}
            }
            
            data_cells -> add(data_tuple);
            sort_cells -> add(sort_tuple);
            first_tuple = false;
        }

        for(i = 0; i < sort_size; i++)
        {
        	common_type ct = types.at(i);
        	ct.size = xmlscm_type_size(ct.xtype);
        	udata.size += ct.size == 0 ? ORB_SERIALIZED_STRING_SIZE : ct.size;
        	//ct.gt = get_gt(ct.xtype);
        }

        if(udata.size > DATA_BLK_SIZE) udata.size = sizeof(int);

        for(i = 0; i < sort_cells->size(); i++)
        {
        	sort_cells -> get(sort_tuple, i);
        	ss -> add(sort_tuple);
        	udata.pos ++;
        }
        //////////////////////////////////////////////
        //TODO:
        //1. initialize gt function
        //2. cast to common type all cells (or 
        //   this process can be done in serialize)?;
        //////////////////////////////////////////////
        
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
        first_time = true;
        pos = 0;
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





int PPOrderBy::get_size_ser(xptr& v1)
{
	CHECKP(v1);
	xptr ptr=(GET_FREE_SPACE(v1)<=sizeof(xptr))?
		((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk+(sizeof(seq_blk_hdr)+sizeof(xptr)-GET_FREE_SPACE(v1)):v1+sizeof(xptr);
	if (GET_FREE_SPACE(ptr)<sizeof(shft))
	{
		copy_to_buffer(XADDR(ptr),GET_FREE_SPACE(ptr));
		xptr nblk=((seq_blk_hdr*)XADDR(BLOCKXPTR(ptr)))->nblk+sizeof(seq_blk_hdr);
		CHECKP(nblk);
		copy_to_buffer(XADDR(nblk),GET_FREE_SPACE(ptr),sizeof(shft)-GET_FREE_SPACE(ptr));
		return *((shft*)temp_buffer);
	}
	else
	{
#ifdef ALIGNMENT_REQUIRED
			copy_to_buffer(XADDR(ptr),sizeof(shft));
			return *((shft*)temp_buffer);
#else
			return *((shft*)XADDR(ptr));		
#endif		
	}
}
xptr PPOrderBy::get_ptr_ser(xptr& v1,int sz)
{
	CHECKP(v1);
	xptr ptr=(GET_FREE_SPACE(v1)<=sizeof(xptr)+sizeof(shft))?
		((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk+(sizeof(seq_blk_hdr)+sizeof(xptr)+sizeof(shft)-GET_FREE_SPACE(v1)):v1+(sizeof(xptr)+sizeof(shft));
	bool nc=(sz+sizeof(xptr)+sizeof(shft))>DATA_BLK_SIZE;
	if (nc)
	{
		CHECKP(ptr);
		if (GET_FREE_SPACE(ptr)<sizeof(xptr))
		{
			copy_to_buffer(XADDR(ptr),GET_FREE_SPACE(ptr));
			xptr nblk=((seq_blk_hdr*)XADDR(BLOCKXPTR(ptr)))->nblk+sizeof(seq_blk_hdr);
			CHECKP(nblk);
			copy_to_buffer(XADDR(nblk),GET_FREE_SPACE(ptr),sizeof(xptr)-GET_FREE_SPACE(ptr));
			return *((xptr*)temp_buffer);

		}
		else
		{
#ifdef ALIGNMENT_REQUIRED
			copy_to_buffer(XADDR(ptr),sizeof(xptr));
			return *((xptr*)temp_buffer);
#else
			return *((xptr*)XADDR(ptr));		
#endif
		}
	}
	else
		return ptr;	
}
void PPOrderBy::copy_data_ser_to_buffer(xptr v1,int sz)
{
	if (sz>GET_FREE_SPACE(v1))
	{
		copy_to_buffer(v1,GET_FREE_SPACE(v1));
		xptr nblk=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk+sizeof(seq_blk_hdr);
		CHECKP(nblk);
		copy_to_buffer(XADDR(nblk),GET_FREE_SPACE(v1),sz-GET_FREE_SPACE(v1));
	}
	else
	{
		copy_to_buffer(v1,sz);
	}	
}
void PPOrderBy::copy_data_ser_to_buffer(xptr v1,shft shift,int sz)
{
	if (sz>GET_FREE_SPACE(v1))
	{
		copy_to_buffer(v1,shift,GET_FREE_SPACE(v1));
		xptr nblk=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk+sizeof(seq_blk_hdr);
		CHECKP(nblk);
		copy_to_buffer(XADDR(nblk),shift+GET_FREE_SPACE(v1),sz-GET_FREE_SPACE(v1));
	}
	else
	{
		copy_to_buffer(v1,shift,sz);
	}	
}
int PPOrderBy::compare_less (xptr v1,xptr v2, const void * Udata)
{
	int s1=get_size_ser(v1);
	int s2=get_size_ser(v2);
	/*copy_data_ser_to_buffer(get_ptr_ser(v1,s1),s1);
	copy_data_ser_to_buffer(get_ptr_ser(v2,s2),s1,s2);
	int res=memcmp(temp_buffer,temp_buffer+s1,min(s1,s2));
	if (res) return res;
	else
	{
		return (s1-s2);
	}*/

	if (s1<s2)
	{
		copy_data_ser_to_buffer(get_ptr_ser(v1,s1),s1);
		xptr data=get_ptr_ser(v2,s2);
		int s2_p1=min(GET_FREE_SPACE(data),s2);
		CHECKP(data);
		int res=memcmp(temp_buffer,XADDR(data),min(s1,s2_p1));
		if (res) return -res;
		else
		{
			if (s1<s2_p1) return 1;
			else
			{
				if (s2_p1==s2) return (s2-s1);
				else
				{
					xptr nblk=((seq_blk_hdr*)XADDR(BLOCKXPTR(v2)))->nblk+sizeof(seq_blk_hdr);
					CHECKP(nblk);
					memcmp(temp_buffer+s2_p1,XADDR(data),min(s1,s2)-s2_p1);
					if (res) return res;
					else
						return (s2-s1);
				}

			}
		}
	}
	else
	{
		copy_data_ser_to_buffer(get_ptr_ser(v2,s2),s2);
		xptr data=get_ptr_ser(v1,s1);
		int s1_p1=min(GET_FREE_SPACE(data),s1);
		CHECKP(data);
		int res=memcmp(temp_buffer,XADDR(data),min(s2,s1_p1));
		if (res) return -res;
		else
		{
			if (s2<s1_p1) return -1;
			else
			{
				if (s1_p1==s1) return (s1-s2);
				else
				{
					xptr nblk=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk+sizeof(seq_blk_hdr);
					CHECKP(nblk);
					memcmp(temp_buffer+s1_p1,XADDR(data),min(s1,s2)-s1_p1);
					if (res) return -res;
					else
						return (s2-s1);
				}
			}
		}
	}
}

int PPOrderBy::get_size (tuple& t, const void * Udata)
{
	return ((orb_user_data*)Udata) -> size;
}

void PPOrderBy::serialize (tuple& t,xptr v1, const void * Udata)
{
	xptr node=t.cells[0].get_node();
	CHECKP(node);
	int sz=((n_dsc*)XADDR(node))->nid.size;
	xptr addr=(sz)? ADDR2XPTR(((n_dsc*)XADDR(node))->nid.prefix):*(xptr*)(((n_dsc*)XADDR(node))->nid.prefix);
	if (!sz)sz=*(shft*)(((n_dsc*)XADDR(node))->nid.prefix+sizeof(xptr));
	CHECKP(v1);
	*((xptr*)XADDR(v1))=node;	
	*((shft*)((char*)XADDR(v1)+sizeof(xptr)))=sz;
	CHECKP(node);	
	if ((sz+(sizeof(xptr)+sizeof(shft)))>DATA_BLK_SIZE)
	{		
		CHECKP(v1);
		VMM_SIGNAL_MODIFICATION(v1);
		*((xptr*)((char*)XADDR(v1)+sizeof(xptr)+sizeof(shft)))=addr;		
	}
	else
	{
		CHECKP(node);		
		copy_to_buffer(addr,sz);
		copy_from_buffer(v1+sizeof(xptr)+sizeof(shft), 0,sz);
	}

}

void PPOrderBy::serialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata)
{
	xptr node=t.cells[0].get_node();
	CHECKP(node);
	int sz=((n_dsc*)XADDR(node))->nid.size;
	xptr addr=(sz)? ADDR2XPTR(((n_dsc*)XADDR(node))->nid.prefix):*(xptr*)(((n_dsc*)XADDR(node))->nid.prefix);
	if (!sz)sz=*(shft*)(((n_dsc*)XADDR(node))->nid.prefix+sizeof(xptr));	
	copy_to_buffer(&node,sizeof(xptr));
	copy_to_buffer(&sz,sizeof(xptr),sizeof(shft));	
	if ((sz+(sizeof(xptr)+sizeof(shft)))>DATA_BLK_SIZE)
	{		
		copy_to_buffer(&addr,sizeof(xptr)+sizeof(shft),sizeof(xptr));
		copy_from_buffer(v1,0,size1);
		copy_from_buffer(v2,size1,2*sizeof(xptr)+sizeof(shft)-size1);

	}
	else
	{			
		copy_to_buffer(addr,sizeof(xptr)+sizeof(shft),sz);
		copy_from_buffer(v1, 0,size1);
		copy_from_buffer(v2,size1,sz+sizeof(xptr)+sizeof(shft)-size1);
	}
}

void PPOrderBy::deserialize (tuple& t,xptr& v1, const void * Udata)
{
	if (GET_FREE_SPACE(v1)<sizeof(xptr))
	{
		copy_to_buffer(v1,GET_FREE_SPACE(v1));
		xptr v2=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk;
		copy_to_buffer(v2+sizeof(seq_blk_hdr),GET_FREE_SPACE(v1),sizeof(xptr)-GET_FREE_SPACE(v1));		
		t.copy(tuple_cell::node(*((xptr*)temp_buffer)));
	}
	else
	{
		CHECKP(v1);		
#ifdef ALIGNMENT_REQUIRED
		copy_to_buffer(XADDR(v1),sizeof(xptr));
		t.copy(tuple_cell::node(*((xptr*)temp_buffer)));
#else
		t.copy(tuple_cell::node(*((xptr*)XADDR(v1))));
#endif				
	}
}

void PPOrderBy::deserialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata)
{
	deserialize (t,v1, Udata);
}

void PPOrderBy::copy_to_buffer(const void* addr, shft size)
{
	if (size>buf_lgth)
	{
		if (buf_lgth)
		{
			delete [] temp_buffer;
		}
		temp_buffer=new char[size];
		buf_lgth=size;
	}	
	memcpy(temp_buffer,addr,size);
}

void PPOrderBy::copy_from_buffer(xptr addr, shft shift,shft size)
{
	CHECKP(addr);
	VMM_SIGNAL_MODIFICATION(addr);
	memcpy(XADDR(addr),temp_buffer+shift,size);
}

void PPOrderBy::copy_to_buffer(const void* addr, shft shift,shft size)
{
	if (size+shift>buf_lgth)
	{
		if (buf_lgth)
		{
			char* buf=new char[size+shift];
			memcpy(buf,temp_buffer,shift);
			delete [] temp_buffer;
			temp_buffer=buf;
		}		
		else
			temp_buffer=new char[size+shift];
		buf_lgth=size+shift;
	}
	memcpy(temp_buffer+shift,addr,size);
}