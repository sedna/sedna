/*
 * File:  PPOrderBy.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPORDERBY_H
#define _PPORDERBY_H

#include <vector>
#include <string>

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/sorted_sequence.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPOrderBy Types and Modifiers
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////



#define ORB_STRING_PREFIX_SIZE  29                                      //Prefix size is selected to make serialized 
                                                                        //representation of the tuple with xs:string 
                                                                        //to be 8-byte aligned. It can improve performance.


#define ORB_SERIALIZED_STRING_SIZE (sizeof(bool) + ORB_STRING_PREFIX_SIZE + sizeof(char))
                                                                        //All xs:string based types have ORB_SERIALIZED_STRING_SIZE
                                                                        //bytes after serialization:
                                                                        //sizeof(bool) + prefix size + sizeof(char) 
                                                                        //[true ] + [prefix] + ['\0'] or
                                                                        //[false] + [prefix] + ['\0'] 
                                                                        //'true' means that the whole string was serialized (size <= prefix size).
                                                                        //'false' means we are able to get the whole string using not 
                                                                        //serialize tuple cell with this string.


#define ORB_SERIALIZED_SIZE(t)  (xmlscm_type_size(t) == 0 ? ORB_SERIALIZED_STRING_SIZE : xmlscm_type_size(t));

///////////////////////////////////////////////////////////////////////////////
/// Note! Don't use this buffer!
/// This buffer is internal class to use only within OrderBy operation!
/// It doesn't have many important security checks: memory bounds, etc...!
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
	void advance_buffer      (int len);
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

struct orb_modifier {
    enum orb_empty_status {
        ORB_EMPTY_GREATEST,
        ORB_EMPTY_LEAST,
    };

    enum orb_sort_order {
        ORB_ASCENDING,
        ORB_DESCENDING,
    };

    orb_sort_order   order;                         
    orb_empty_status status;
    CollationHandler* collation;

    inline std::string to_string() const
    {
        std::string res;
        if(order == ORB_ASCENDING) res += "ascending";
        else res += "descending";
        if(status == ORB_EMPTY_GREATEST) res += " empty greatest";
        else res += " empty least";
        return res;
    }
};

struct orb_common_type
{
    int size;                                       //Size of type in bytes.
    xmlscm_type xtype;                              //One of the atomic built-in types.
    bool initialized;                               //'true' if xtype is defined, else must be 'false'
};

typedef std::vector<orb_modifier>       arr_of_orb_modifier;
typedef std::vector<orb_common_type>    arr_of_common_type; 

//Udata* is used in serialization/deserialization in sorted sequence
struct orb_user_data
{
    sequence *sort;                                 //Initial sequence which must be sorted.
    int64_t pos;                                    
    int size;                                       //Serialized size of in bytes (fixed for each tuple): 
                                                    //[position] + [tuple_cell(1) | tuple_cell(2) | .... tuple_cell(N)] + [bit_set - eos map].
    int bit_set_offset;                             //Offset to bit_set (i.e. size of position + size of tuple cells in serialized presentation).
    arr_of_common_type* header;                     //Array of common types structures.
    arr_of_orb_modifier* modifiers;                 //Array of standart order by modifiers - [empty (greatest | least)] and [ascending | descending].
    temp_buffer* buffer;                            //Buffer for memory copy operations (when ALIGNMENT_REQUIRED or two blocks are used).
    bool stable;                                    //'true' if 'stable order by' version used.
    char* temps[2];                                 //Buffer memory used in comparator.
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

    sequence *data_cells;                           //Accumulates the first 'data_size' tuple cells.
    sequence *sort_cells;                           //Accumulates other 'sort_size' tuple cells.
    
    bool first_time;
    bool need_reinit;
    bool need_to_sort;                              //If we have only eos values then we don't need to sort by them
    sorted_sequence *ss;
    int64_t pos;
    
    arr_of_common_type types;
    orb_user_data udata;

    static int  compare             (xptr v1,xptr v2, const void * Udata);
    static int  get_size            (tuple& t, const void * Udata);
    static void serialize           (tuple& t,xptr v1, const void * Udata);
    static void serialize_2_blks    (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata);
    static void deserialize         (tuple &t, xptr& v1, const void * Udata);
    static void deserialize_2_blks  (tuple& t,xptr& v1,shft size1,xptr& v2, const void * Udata);
        
private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPOrderBy(dynamic_context *_cxt_,
              operation_info _info_,
              bool _stable_,
              PPOpIn _child_,
              arr_of_orb_modifier _modifiers_,
              int _data_size_);

    virtual ~PPOrderBy();
    
    inline bool is_stable() const { return stable; }
    inline int get_tuple_size() const { return data_size; }
    inline const arr_of_orb_modifier& get_modifiers() const { return modifiers; }
};



///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// Special versions of PPLet and PPTuple operations
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

typedef std::vector<sequence*> arr_of_seq_ptr;

class PPSTuple : public PPIterator
{
protected:
    arr_of_PPOpIn ch_arr;
    unsigned int i;
    tuple lt; // local tuple

    arr_of_seq_ptr seq_ptrs;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPSTuple(dynamic_context *_cxt_,
             operation_info _info_,
             const arr_of_PPOpIn &_children_);
    virtual ~PPSTuple();
};

class PPSLet : public PPVarIterator
{
private:
    arr_of_var_dsc var_dscs;

    PPOpIn source_child;
    tuple source;

    PPOpIn data_child;

    bool need_reopen;
    bool first_time;
    int64_t size;
    sequence *s;

    inline void reinit_consumer_table();

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);
    
    virtual var_c_id do_register_consumer(var_dsc dsc);
    virtual void do_next  (tuple &t, var_dsc dsc, var_c_id id);
    virtual void do_reopen(var_dsc dsc, var_c_id id);
    virtual void do_close (var_dsc dsc, var_c_id id);

public:
    PPSLet(dynamic_context *_cxt_,
           operation_info _info_,
           arr_of_var_dsc _var_dscs_, 
           PPOpIn _source_child_, 
           PPOpIn _data_child_);

    virtual ~PPSLet();

    inline const arr_of_var_dsc& get_variable_descriptors() { return var_dscs; }
};


#endif
