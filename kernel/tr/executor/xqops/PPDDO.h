/*
 * File:  PPDDO.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPDDO_H
#define _PPDDO_H

#include "sedna.h"

#include "PPBase.h"
#include "sorted_sequence.h"

//#define TURN_ON_DDO

class PPDDO : public PPIterator
{
private:
	static char* temp_buffer;
	static int buf_lgth;	
	static int get_size_ser(xptr& v1);
	static xptr get_ptr_ser(xptr& v1,int sz);
	static void copy_data_ser_to_buffer(xptr v1,int sz);
	static void copy_data_ser_to_buffer(xptr v1,shft shift,int sz);

	inline static void copy_to_buffer(xptr addr, shft size)
	{
		CHECKP(addr);
		copy_to_buffer(XADDR(addr),size);
	}
	static void copy_to_buffer(xptr addr, shft shift,shft size)
	{
		CHECKP(addr);
		copy_to_buffer(XADDR(addr),shift,size);
	}
	static void copy_to_buffer(const void* addr, shft size);
	static void copy_to_buffer(const void* addr, shft shift,shft size);
	static void copy_from_buffer(xptr addr, shft shift,shft size);
protected:
    // Inhereted through PPIterator
    // query_prolog_type *qp;
    // PPOpOut out;

#ifdef TURN_ON_DDO
    int pos;
    sorted_sequence *s;
#endif

    // given parameters
    PPOpIn child;

    void children(PPOpIn& _child_) { _child_ = child; }

public:

    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    PPDDO(variable_context *_cxt_,
          PPOpIn _child_);
    virtual ~PPDDO();

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);
	static int compare_less (xptr v1,xptr v2, const void * Udata);
	static int get_size (tuple& t, const void * Udata);
	static void serialize (tuple& t,xptr v1, const void * Udata);
	static void serialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata);
	static void deserialize (tuple &t, xptr& v1, const void * Udata);
	static void deserialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata);
};


#endif
