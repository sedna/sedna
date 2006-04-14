/*
 * File:  PPDDO.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "PPDDO.h"
#include "numb_scheme.h"
#include <iostream>

using namespace std;
//#ifdef TURN_ON_DDO
static char* temp_buffer=NULL;//new char[MAXINTERNALPREFIX];
static int buf_lgth=0;//MAXINTERNALPREFIX;
//#endif

PPDDO::PPDDO(variable_context *_cxt_,
             PPOpIn _child_) : PPIterator(_cxt_),
                               child(_child_)
{

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
    child.op->open();
    pos = 0;
    s = new sorted_sequence(compare_less,get_size,serialize,serialize_2_blks,deserialize);
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
#else
    child.op->close();
#endif
}

void PPDDO::next  (tuple &t)
{
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

        u_timeb t_sort1, t_sort2;
        d_printf2("Before sorting: size = %d\n", s->size());
        u_ftime(&t_sort1);
        //s->sort();
        s->sort();
        u_ftime(&t_sort2);
        d_printf2("After sorting: time = %s\n", to_string(t_sort2 - t_sort1).c_str());
    }

    if (pos < s->size()) t.copy(s->get(pos++));
    else 
    {
        t.set_eos();
        pos = 0;
        s->clear();
    }
#else
    child.op->next(t);
#endif
}

PPIterator* PPDDO::copy(variable_context *_cxt_)
{
    PPDDO *res = new PPDDO(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPDDO::result(PPIterator* cur, variable_context *cxt, void*& r)
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
        PPDDO *res_op = new PPDDO(cxt, child);

        r = res_op;
        return false;
    }

    return strict_op_result(cur, (sequence*)child_r, cxt, r);
#endif
}
int PPDDO::get_size_ser(xptr& v1)
{
	CHECKP(v1);
	if (GET_FREE_SPACE(v1)<(sizeof(xptr)+sizeof(shft)))
	{
		copy_to_buffer(XADDR(v1),GET_FREE_SPACE(v1));
		xptr nblk=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk+sizeof(seq_blk_hdr);
		CHECKP(nblk);
		copy_to_buffer(XADDR(nblk),GET_FREE_SPACE(v1),sizeof(xptr)+sizeof(shft)-GET_FREE_SPACE(v1));
		return *((int*)(temp_buffer+sizeof(xptr)));
	}
	else
	{
		return *((int*)XADDR((v1+sizeof(xptr))));
	}
}
xptr PPDDO::get_ptr_ser(xptr& v1,int sz)
{
	CHECKP(v1);
	bool nc=(sz+sizeof(xptr)+sizeof(shft))>DATA_BLK_SIZE;	
	if ((GET_FREE_SPACE(v1)<(2*sizeof(xptr)+sizeof(shft))) ||! (!nc && GET_FREE_SPACE(v1)>(sizeof(xptr)+sizeof(shft)) ) )
	{
		copy_to_buffer(XADDR(v1),GET_FREE_SPACE(v1));
		xptr nblk=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk+sizeof(seq_blk_hdr);
		CHECKP(nblk);
		copy_to_buffer(XADDR(nblk),GET_FREE_SPACE(v1),2*sizeof(xptr)+sizeof(shft)-GET_FREE_SPACE(v1));
		if (nc) 
			return *((xptr*)(temp_buffer+sizeof(shft)+sizeof(xptr)));
		else
			return nblk+(sizeof(xptr)+sizeof(shft)-GET_FREE_SPACE(v1));
	}
	else
	{
		return (nc)?*((xptr*)XADDR((v1+sizeof(shft)+sizeof(xptr)))):v1+(sizeof(shft)+sizeof(xptr));
	}
}
void PPDDO::copy_data_ser_to_buffer(xptr v1,int sz)
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
int PPDDO::compare_less (xptr v1,xptr v2)
{
	int s1=get_size_ser(v1);
	int s2=get_size_ser(v2);
	if (s1<s2)
	{
		copy_data_ser_to_buffer(get_ptr_ser(v1,s1),s1);
		xptr data=get_ptr_ser(v2,s2);
		int s2_p1=min(GET_FREE_SPACE(data),s2);
		CHECKP(data);
		int res=memcmp(temp_buffer,XADDR(data),min(s1,s2_p1));
		if (res) return res;
		else
		{
			if (s1<=s2_p1) return -1;
			else
			{
				if (s2_p1==s2) return 1;
				else
				{
					xptr nblk=((seq_blk_hdr*)XADDR(BLOCKXPTR(v2)))->nblk+sizeof(seq_blk_hdr);
					CHECKP(nblk);
					memcmp(temp_buffer+s2_p1,XADDR(data),min(s1,s2)-s2_p1);
					if (res) return res;
					else
						return (s1-s2);
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
		if (res) return res;
		else
		{
			if (s2<=s1_p1) return 1;
			else
			{
				if (s1_p1==s1) return -1;
				else
				{
					xptr nblk=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk+sizeof(seq_blk_hdr);
					CHECKP(nblk);
					memcmp(temp_buffer+s1_p1,XADDR(data),min(s1,s2)-s1_p1);
					if (res) return res;
					else
						return (s1-s2);
				}

			}
		}
	}
}
int PPDDO::get_size (tuple& t)
{
	xptr node=t.cells[0].get_node();
	CHECKP(node);
	int sz=((n_dsc*)XADDR(node))->nid.size;
	if (!sz)sz=*(shft*)(((n_dsc*)XADDR(node))->nid.prefix+sizeof(xptr));
	sz+=(sizeof(xptr)+sizeof(shft));
	return (sz>DATA_BLK_SIZE)?2*sizeof(xptr)+sizeof(shft):sz;	
}
void PPDDO::serialize (tuple& t,xptr v1)
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
		*((xptr*)((char*)XADDR(v1)+sizeof(xptr)+sizeof(shft)))=addr;		
	}
	else
	{
		CHECKP(node);		
		copy_to_buffer(addr,sz);
		copy_from_buffer(v1+sizeof(xptr)+sizeof(shft), 0,sz);
	}

}
void PPDDO::serialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2)
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
tuple PPDDO::deserialize (xptr& v1)
{
	if (GET_FREE_SPACE(v1)<sizeof(xptr))
	{
		copy_to_buffer(v1,GET_FREE_SPACE(v1));
		xptr v2=((seq_blk_hdr*)XADDR(BLOCKXPTR(v1)))->nblk;
		copy_to_buffer(v2+sizeof(seq_blk_hdr),GET_FREE_SPACE(v1),sizeof(xptr)-GET_FREE_SPACE(v1));
		tuple t(1);
		t.copy(tuple_cell::node(*((xptr*)temp_buffer)));
		return t;

	}
	else
	{
		CHECKP(v1);
		tuple t(1);
		t.copy(tuple_cell::node(*((xptr*)XADDR(v1))));
		return t;
	}
}

void PPDDO::copy_to_buffer(const void* addr, shft size)
{
	if (size>buf_lgth)
	{
		if (buf_lgth)
		{
			delete [] temp_buffer;
		}
		temp_buffer=new char[size];
	}	
	memcpy(temp_buffer,addr,size);
}
void PPDDO::copy_from_buffer(xptr addr, shft shift,shft size)
{
	CHECKP(addr);
	memcpy(XADDR(addr),temp_buffer+shift,size);
}
void PPDDO::copy_to_buffer(const void* addr, shft shift,shft size)
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
	}
	memcpy(temp_buffer+shift,addr,size);
}