/*
 * File:  PPDDO.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <iostream>

#include "common/sedna.h"

#include "tr/executor/xqops/PPDDO.h"
#include "tr/nid/numb_scheme.h"

using namespace std;

char* PPDDO::temp_buffer=NULL;//se_new char[MAXINTERNALPREFIX];
int PPDDO::buf_lgth=0;//MAXINTERNALPREFIX;


PPDDO::PPDDO(dynamic_context *_cxt_,
             PPOpIn _child_) : PPIterator(_cxt_),
                               child(_child_)
{
#ifdef TURN_ON_DDO
	ret_val=XNULL;
#endif
}

PPDDO::~PPDDO()
{
#ifdef TURN_ON_DDO
#else
    
#endif
	delete child.op;
    child.op = NULL;
}

void PPDDO::open  ()
{
#ifdef TURN_ON_DDO
    s = se_new sorted_sequence(compare_less,get_size,serialize,serialize_2_blks,deserialize,deserialize_2_blks,NULL);
    child.op->open();
    pos = 0;
	ret_val=XNULL;
#else
    child.op->open();
#endif
}

void PPDDO::reopen()
{
#ifdef TURN_ON_DDO
    child.op->reopen();
    pos = 0;
    s->clear();
	ret_val=XNULL;
#else
    child.op->reopen();
#endif
}

void PPDDO::close ()
{
#ifdef TURN_ON_DDO
    child.op->close();
    pos = 0;
    delete s;
	ret_val=XNULL;
#else
    child.op->close();
#endif
}

void PPDDO::next  (tuple &t)
{
    SET_CURRENT_PP(this);

#ifdef TURN_ON_DDO
    if (!pos)
    {
        // accumulate nodes and sort them
        while (true)
        {
            child.op->next(t);
            if (t.is_eos()) break;
            else
            {
                tuple_cell tc = child.get(t);
                if (!tc.is_node()) throw USER_EXCEPTION2(SE1003, "Argument of PPDDO is not a node");
                s->add(t);
            }
        }

       /* u_timeb t_sort1, t_sort2;
        d_printf1("Before sorting: \n");
        u_ftime(&t_sort1);
        s->sort();
        s->sort();*/
        s->lazy_sort();
        /*u_ftime(&t_sort2);
        d_printf3("After sorting: time = %s size= %d\n", to_string(t_sort2 - t_sort1).c_str(),s->size());*/
        pos=1;
        ret_val=XNULL;
    }

while (true)
{
	s->next(t);
	if (t.is_eos())
	{
		pos = 0;
		s->clear();
		{RESTORE_CURRENT_PP; return;}
	}
	else
	{
		if (t.cells[0].get_node()!=ret_val)
		{
			ret_val=t.cells[0].get_node();
			{RESTORE_CURRENT_PP; return;}
		}
	}
}
#else
    child.op->next(t);
#endif

    RESTORE_CURRENT_PP;
}

PPIterator* PPDDO::copy(dynamic_context *_cxt_)
{
    PPDDO *res = se_new PPDDO(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}

bool PPDDO::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
#ifdef TURN_ON_DDO
    return true;
#else
    PPOpIn child;
    ((PPDDO*)cur)->children(child);

    void *child_r;
    bool child_s = (child.op->res_fun())(child.op, cxt, child_r);

    if (!child_s) // if expression is not strict
    { // create PPDDO and transmit state
        child.op = (PPIterator*)child_r;
        PPDDO *res_op = se_new PPDDO(cxt, child);

        r = res_op;
        return false;
    }

    return strict_op_result(cur, (sequence*)child_r, cxt, r);
#endif
}
int PPDDO::get_size_ser(xptr& v1)
{
	CHECKP(v1);
	xptr ptr=(GET_FREE_SPACE(v1)<=sizeof(xptr))?
		((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk+(sizeof(seq_blk_hdr)+sizeof(xptr)-GET_FREE_SPACE(v1)):v1+sizeof(xptr);
	CHECKP(ptr);
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
xptr PPDDO::get_ptr_ser(xptr& v1,int sz)
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
void PPDDO::copy_data_ser_to_buffer(xptr v1,int sz)
{
	CHECKP(v1);
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
void PPDDO::copy_data_ser_to_buffer(xptr v1,shft shift,int sz)
{
	CHECKP(v1);
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

/*static char bufA[100000];
static char bufB[100000];*/

int PPDDO::compare_less (xptr v1,xptr v2, const void * Udata)
{
	CHECK_TIMER_FLAG;
	
	int s1=get_size_ser(v1);
	int s2=get_size_ser(v2);
	/*copy_data_ser_to_buffer(get_ptr_ser(v1,s1),s1);
	copy_data_ser_to_buffer(get_ptr_ser(v2,s2),s1,s2);
	int res=memcmp(temp_buffer,temp_buffer+s1,MIN(s1,s2));
	if (res) return res;
	else
	{
		return (s1-s2);
	}*/

/*  /// Straightforward implementation for debug:

    copy_data_ser_to_buffer(get_ptr_ser(v1,s1),s1);
    memcpy(bufA, temp_buffer, s1);
    copy_data_ser_to_buffer(get_ptr_ser(v2,s2),s2);
	memcpy(bufB, temp_buffer, s2);
	int res = sign(memcmp(bufA, bufB, MIN(s1, s2)));
    if( 0 == res )
	{
	     if(s1 > s2) return 1; if(s2 > s1) return -1;
		 return 0;
	} return res;*/

	if (s1<s2)
	{
		copy_data_ser_to_buffer(get_ptr_ser(v1,s1),s1);
		xptr data=get_ptr_ser(v2,s2);
		int s2_p1=MIN(GET_FREE_SPACE(data),s2);
		CHECKP(data);
		int res=memcmp(temp_buffer,XADDR(data),MIN(s1,s2_p1));
		if (res) return sign(res);
		else
		{
			if (s1<=s2_p1) return -1;
			else
			{
				if (s2_p1==s2) return (s1-s2);
				else
				{
					data=((seq_blk_hdr*)XADDR(BLOCKXPTR(v2)))->nblk+sizeof(seq_blk_hdr);
					CHECKP(data);
					res=memcmp(temp_buffer+s2_p1,XADDR(data),MIN(s1,s2)-s2_p1);
					if (res) return sign(res);
					else
						return (s1-s2);
				}

			}
		}
	}
	else  /* s1 >= s2 */
	{
		copy_data_ser_to_buffer(get_ptr_ser(v2,s2),s2);
		xptr data=get_ptr_ser(v1,s1);
		int s1_p1=MIN(GET_FREE_SPACE(data),s1);
		CHECKP(data);
		int res=memcmp(temp_buffer,XADDR(data),MIN(s2,s1_p1));
		if (res) return -sign(res);
		else
		{
			if (s2 == s1_p1 && s1_p1 < s1) return 1;  
			if (s2 < s1_p1) return 1;
			else
			{
				if (s1_p1==s1) return (s1-s2);
				else
				{
					data=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk+sizeof(seq_blk_hdr);
					CHECKP(data);
					res=memcmp(temp_buffer+s1_p1,XADDR(data),MIN(s1,s2)-s1_p1);
					if (res) return -sign(res);
					else
						return (s1-s2);
				}
			}
		}
	}
}
int PPDDO::get_size (tuple& t, const void * Udata)
{
	xptr node=t.cells[0].get_node();
	CHECKP(node);
	shft sz=((n_dsc*)XADDR(node))->nid.size;
	if (!sz)sz=*(shft*)(((n_dsc*)XADDR(node))->nid.prefix+sizeof(xptr));
	sz+=(sizeof(xptr)+sizeof(shft));
	return (sz>DATA_BLK_SIZE)?2*sizeof(xptr)+sizeof(shft):sz;	
}
void PPDDO::serialize (tuple& t,xptr v1, const void * Udata)
{
	CHECK_TIMER_FLAG;
	
	xptr node=t.cells[0].get_node();
	CHECKP(node);
	shft sz=((n_dsc*)XADDR(node))->nid.size;
	xptr addr=(sz)? ADDR2XPTR(((n_dsc*)XADDR(node))->nid.prefix):*(xptr*)(((n_dsc*)XADDR(node))->nid.prefix);
	if (!sz)
	{
	    CHECKP(addr);
		addr = PSTRDEREF(addr);
		CHECKP(node);
	}
	if (!sz)sz=*(shft*)(((n_dsc*)XADDR(node))->nid.prefix+sizeof(xptr));
	CHECKP(v1);
	VMM_SIGNAL_MODIFICATION(v1);
	*((xptr*)XADDR(v1))=node;	
	*((shft*)((char*)XADDR(v1)+sizeof(xptr)))=sz;

	if ((sz+(sizeof(xptr)+sizeof(shft)))>DATA_BLK_SIZE)
	{		
		*((xptr*)((char*)XADDR(v1)+sizeof(xptr)+sizeof(shft)))=addr;		
	}
	else
	{
		CHECKP(node);		
		copy_to_buffer(addr,sz);
		copy_from_buffer(v1+sizeof(xptr)+sizeof(shft), 0,sz);
	}

}
void PPDDO::serialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata)
{
	xptr node=t.cells[0].get_node();
	CHECKP(node);
	shft sz=((n_dsc*)XADDR(node))->nid.size;
	xptr addr=(sz)? ADDR2XPTR(((n_dsc*)XADDR(node))->nid.prefix):*(xptr*)(((n_dsc*)XADDR(node))->nid.prefix);
	if (!sz)
	{
	    CHECKP(addr);
		addr = PSTRDEREF(addr);
		CHECKP(node);
	}
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
void PPDDO::deserialize (tuple& t,xptr& v1, const void * Udata)
{
	CHECK_TIMER_FLAG;
	
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

void PPDDO::deserialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata)
{
	deserialize (t,v1, Udata);
}
void PPDDO::copy_to_buffer(const void* addr, shft size)
{
	if (size>buf_lgth)
	{
		if (buf_lgth)
		{
			delete [] temp_buffer;
		}
		temp_buffer=se_new char[size];
		buf_lgth=size;
	}	
	memcpy(temp_buffer,addr,size);
}
void PPDDO::copy_from_buffer(xptr addr, shft shift,shft size)
{
	CHECKP(addr);
	VMM_SIGNAL_MODIFICATION(addr);
	memcpy(XADDR(addr),temp_buffer+shift,size);
}
void PPDDO::copy_to_buffer(const void* addr, shft shift,shft size)
{
	if (size+shift>buf_lgth)
	{
		if (buf_lgth)
		{
			char* buf=se_new char[size+shift];
			memcpy(buf,temp_buffer,shift);
			delete [] temp_buffer;
			temp_buffer=buf;
		}		
		else
			temp_buffer=se_new char[size+shift];
		buf_lgth=size+shift;
	}
	memcpy(temp_buffer+shift,addr,size);
}
