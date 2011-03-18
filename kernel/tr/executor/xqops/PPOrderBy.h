/*
 * File:  PPOrderBy.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPORDERBY_H
#define _PPORDERBY_H

#include <vector>
#include <string>

#include "common/sedna.h"

#include "PPOrderBy_serializer.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/SortedSequence.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPOrderBy Types and Modifiers
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

struct orb_user_data
{
    sequence *sort;                                 //Initial sequence which must be sorted.
    int64_t pos;                                    
    int size;                                       //Serialized size of in bytes (fixed for each tuple): 
    //[position] + [tuple_cell(1) | tuple_cell(2) | .... tuple_cell(N)] + [bit_set - eos map].
    int bit_set_offset;                             //Offset to bit_set (i.e. size of position + size of tuple cells in serialized presentation).
    arr_of_common_type* header;                     //Array of common types structures.
    arr_of_orb_modifier* modifiers;                 //Array of standart order by modifiers - [empty (greatest | least)] and [ascending | descending].
    bool stable;                                    //'true' if 'stable order by' version used.
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
    SortedSequence *ss;
    TupleSerializer *serializer;
    int64_t pos;
    
    arr_of_common_type types;
    orb_user_data udata;

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
