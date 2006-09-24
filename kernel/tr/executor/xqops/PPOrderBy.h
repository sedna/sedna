/*
 * File:  PPOrderBy.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPORDERBY_H
#define _PPORDERBY_H

#include "sedna.h"
#include "PPBase.h"
#include "PPUtils.h"
#include "bit_set.h"
#include "sorted_sequence.h"
#include "e_string.h"
#include "op_map.h"
#include "string_operations.h"
#include "casting_operations.h"


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPOrderBy Types and Modifiers
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

#define ORB_STRING_PREFIX_SIZE  8                   


#define ORB_SERIALIZED_STRING_SIZE sizeof(bool) + ORB_STRING_PREFIX_SIZE + sizeof(char)      
                                                                        //All xs:string based types have ORB_SERIALIZED_STRING_SIZE
                                                                        //bytes after serialization:
                                                                        //sizeof(bool) + prefix size + sizeof(char) 
                                                                        //[true ] + [prefix] + ['\0'] or
                                                                        //[false] + [prefix] + ['\0'] 
                                                                        //'true' means that the whole string was serialized (size <= prefix size).
                                                                        //'false' means we are able to get the whole string using not 
                                                                        //serialize tuple cell with this string.


#define ORB_SERIALIZED_SIZE(t)  xmlscm_type_size(t) == 0 ? ORB_SERIALIZED_STRING_SIZE : xmlscm_type_size(t);
                                                                                                                
///////////////////////////////////////////////////////////////////////////////
/// NOTE!
/// This buffer is internal class to use only within OrderBy operation!
/// It doesn't have many important security checks for memory borders and others!
///////////////////////////////////////////////////////////////////////////////
class temp_buffer
{
private:
    int   size;              //Size in bytes is fixed.
    char* buffer;            //Pointer to actual data.
    int   pos;               //Points to the data end in the buffer.
    
public:
    temp_buffer (int _size_);
    ~temp_buffer ();

    void clear ();
    void serialize_to_buffer (const tuple_cell& tc);
    void copy_to_buffer      (xptr addr, int size);
    void copy_to_buffer      (const void* addr, int size);
    void copy_from_buffer    (int start, int size, xptr addr);
    void copy_from_buffer    (xptr addr);
    void copy_from_buffer    (int start, xptr addr);
    void copy_from_buffer    (int start, int size, void* addr);
    void copy_from_buffer    (void* addr);
    void copy_from_buffer    (int start, void* addr);
    void create_empty_block  (int size);
    void create_empty_block  (int start, int size);
};
///////////////////////////////////////////////////////////////////////////////

enum orb_empty_status {
    ORB_EMPTY_GREATEST,
    ORB_EMPTY_LEAST,
};

enum orb_sort_order {
    ORB_ASCENDING,
    ORB_DESCENDING,
};

struct orb_modifier {
    orb_sort_order   order;                         
    orb_empty_status status;
};

struct common_type
{
    int size;                                       //Size of type in bytes.
    xmlscm_type xtype;                              //One of the atomic built-in types.
    bool initialized;                               //'true' if xtype is defined, else must be 'false'
};

typedef std::vector<orb_modifier>       arr_of_orb_modifier;
typedef std::vector<common_type>        arr_of_common_type; 

//Udata* is used in serialization/deserialization in sorted sequence
struct orb_user_data
{
    sequence_tmp *sort;                             //Initial sequence which must be sorted.    
    __int64 pos;                                    
    int size;                                       //Serialized size of in bytes (fixed for each tuple): 
                                                    //[position] + [tuple_cell(1) | tuple_cell(2) | .... tuple_cell(N)] + [bit_set - eos map].
    int bit_set_offset;                             //Offset to bit_set (i.e. size of position + size of tuple cells in serialized presentation).
    arr_of_common_type* header;                     //Array of common types structures.
    arr_of_orb_modifier* modifiers;                 //Array of standart order by modifiers - [empty (greatest | least)] and [ascending | descending].
    temp_buffer* buffer;                            //Buffer for memory copy operations (when ALIGNMENT_REQUIRED or two blocks are used).
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

    int data_size;                                  //Tuple cells number of actual data in the tuple.
                                                    //Other tuple cells are used for sorting and filled
                                                    //by order by expressions evaluation results.
    int sort_size;                                  //Number of these tuple cells. This value is automaticaly
                                                    //evaluated form the 'data_size' and 'child.ts' values

    sequence_tmp *data_cells;                           //Accumulates the first 'data_size' tuple cells. 
    sequence_tmp *sort_cells;                           //Accumulates other 'sort_size' tuple cells.  
    
    bool first_time;
    bool need_reinit;
    bool need_to_sort;                              //If we have of eos values then we don't need to sort by them
    sorted_sequence *ss;
    __int64 pos;
    
    arr_of_common_type types;
    orb_user_data udata;
    
    static int  compare             (xptr v1,xptr v2, const void * Udata);
    static int  get_size            (tuple& t, const void * Udata);
    static void serialize           (tuple& t,xptr v1, const void * Udata);
    static void serialize_2_blks    (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata);
    static void deserialize         (tuple &t, xptr& v1, const void * Udata);
    static void deserialize_2_blks  (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata);
        
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
