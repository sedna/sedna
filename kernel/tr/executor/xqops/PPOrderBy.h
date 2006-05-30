/*
 * File:  PPOrderBy.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPORDERBY_H
#define _PPORDERBY_H

#include "sedna.h"
#include "PPBase.h"
#include "PPUtils.h"
#include "sorted_sequence.h"
#include "numb_scheme.h"
#include "casting_operations.h"


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPOrderBy Types and Modifiers
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

#define ORB_SERIALIZED_STRING_SIZE 8			    //All xs:string based types have ORB_SERIALIZED_STRING_SIZE
                                                    //bytes after serialization, whic can store string itself or
                                                    //fixed size prefix + xptr.
enum orb_empty_status {
	ORB_EMPTY_GREATEST,
	ORB_EMPTY_LEAST,
	ORB_ES_DEFAULT
};

enum orb_sort_order {
	ORB_ASCENDING,
	ORB_DESCENDING,
	ORB_SO_DEFAULT
};

struct orb_modifier {
	orb_sort_order order;
	orb_empty_status status;

	orb_modifier(orb_sort_order _order_,
	             orb_empty_status _status_): order(_order_),
	                                         status(_status_) 
	{
	}
};

struct common_type
{
	int size;										//Size of type in bytes. 0 for string based types.
	xmlscm_type xtype;                              //One of the atomic built-in types.
	int (* gt)(const void* arg1, const void* arg2); //Pointer to in-memory comparison function for given type.
};

typedef std::vector<orb_modifier>		arr_of_orb_modifier;
typedef std::vector<common_type>        arr_of_common_type; 

//Udata* used in serialization/deserialization in sorted sequence
struct orb_user_data
{
	sequence *sort;									
	int pos;                                        
	int size; 										//Serialized size in bytes. 
	arr_of_common_type* header;	
	arr_of_orb_modifier* modifiers;
};

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPOrderBy
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

class PPOrderBy : public PPIterator
{
private:
    bool stable;
    PPOpIn child;
    arr_of_orb_modifier modifiers;

    int data_size;		              				//Tuple cells number of actual data in the tuple.
    								  				//Other tuple cells are used for sorting and filled
 					 								//by order by expressions evaluation results.
    int sort_size; 					  				//Number of these tuple cells. This value is automaticaly
    								  				//evaluated form the 'data_size' and 'child.ts' values

    sequence *data_cells;			  				//Accumulates the first 'data_size' tuple cells.
    sequence *sort_cells;             				//Accumulates other 'sort_size' tuple cells. 
    
    bool first_time;
    sorted_sequence *ss;
    int pos;
    
    arr_of_common_type types;
    orb_user_data udata;
    
    static int compare_less (xptr v1,xptr v2, const void * Udata);
	static int get_size (tuple& t, const void * Udata);
	static void serialize (tuple& t,xptr v1, const void * Udata);
	static void serialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata);
	static void deserialize (tuple &t, xptr& v1, const void * Udata);
	static void deserialize_2_blks (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata);
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
	void children(PPOpIn& _child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    PPOrderBy(variable_context *_cxt_,
    		  bool _stable_,
              PPOpIn _child_,
              arr_of_orb_modifier _modifiers_,
              int _data_size_);
    virtual ~PPOrderBy();

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);
};


#endif
