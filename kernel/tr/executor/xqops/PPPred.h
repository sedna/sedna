/*
 * File:  PPPred.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPPRED_H
#define __PPPRED_H

#include "PPBase.h"
#include <list>



enum operation_compare_condition
{
	OCC_VALUE_EQUAL,
	OCC_VALUE_NOT_EQUAL,
	OCC_VALUE_LESS,
	OCC_VALUE_GREATER,
	OCC_VALUE_LESS_EQUAL,
	OCC_VALUE_GREATER_EQUAL,
	OCC_GENERAL_EQUAL,
	OCC_GENERAL_NOT_EQUAL,
	OCC_GENERAL_LESS,
	OCC_GENERAL_GREATER,
	OCC_GENERAL_LESS_EQUAL,
	OCC_GENERAL_GREATER_EQUAL
};                          

typedef std::vector<operation_compare_condition>	arr_of_comp_cond;



///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPPredRange
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

struct PPPredRange 
{

enum range_state
{
	RS_INITIAL,
	RS_EMPTY,
	RS_RANGE,
	RS_POINTS
};

private:

	int upper_bound;
	int lower_bound;		
	range_state state;
	std::list<int> except_points;
	std::list<int> points;

	inline bool is_occ_value(operation_compare_condition occ){
		return occ == OCC_VALUE_EQUAL || occ == OCC_VALUE_NOT_EQUAL || occ == OCC_VALUE_LESS || 
		       occ == OCC_VALUE_GREATER || occ == OCC_VALUE_LESS_EQUAL || occ == OCC_VALUE_GREATER_EQUAL;

	}
	inline bool is_occ_general(operation_compare_condition occ)
	{
		return occ == OCC_GENERAL_EQUAL || occ == OCC_GENERAL_NOT_EQUAL || occ == OCC_GENERAL_LESS || 
		       occ == OCC_GENERAL_GREATER || occ == OCC_GENERAL_LESS_EQUAL || occ == OCC_GENERAL_GREATER_EQUAL;
	}
	
	void PPPredRange::position_less_than(double double_value);
	void PPPredRange::position_greater_than(double double_value);
	void PPPredRange::position_less_equal_than(double double_value);
	void PPPredRange::position_greater_equal_than(double double_value);
	
	void PPPredRange::print_state();

public:
	
	int add_new_constraint(operation_compare_condition occ, const PPOpIn &conjunct);
	bool is_position_in_range(int pos);
	int get_max_posible_position();
	int get_min_posible_position();

	bool is_empty();
	bool is_any();
	
	PPPredRange();
	
	int reinit_with_position(double position);
	void reinit();
};






///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPPred1
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
class PPPred1 : public PPVarIterator
{
private:
    arr_of_var_dsc var_dscs;

    PPOpIn source_child;
    arr_of_PPOpIn conjuncts;
    PPOpIn data_child;

    arr_of_comp_cond conditions;
    tuple *cur_tuple;
    tuple data;
    PPPredRange range;

    bool first_time;
    bool once;
    bool result_ready;
    bool eos_reached;
    bool need_reopen;
    bool any;

    int pos;
    int upper_bound;
    int lower_bound;
    var_dsc pos_dsc;

    void children(PPOpIn& _source_child_,
                  PPOpIn& _data_child_)
    {
        _source_child_ = source_child;
        _data_child_ = data_child;
    }
    
    inline void reinit_consumer_table();
    
public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

	PPPred1(variable_context *_cxt_,
            arr_of_var_dsc _var_dscs_, 
            PPOpIn _source_child_, 
            arr_of_PPOpIn _conjuncts_,
	        arr_of_comp_cond _conditions_,
            PPOpIn _data_child_,
	        bool _once_,
            var_dsc _pos_dsc_ = -1);
    
    virtual ~PPPred1();

    virtual var_c_id register_consumer(var_dsc dsc);
    virtual void next(tuple &t, var_dsc dsc, var_c_id id);
    virtual void reopen(var_dsc dsc, var_c_id id);
};




///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPPred2
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
class PPPred2 : public PPVarIterator
{
private:
    arr_of_var_dsc var_dscs;

    PPOpIn source_child;
    arr_of_PPOpIn conjuncts;
    PPOpIn data_child;

    arr_of_comp_cond conditions;
    tuple data;
    tuple *cur_tuple;
    int pos;
    var_dsc pos_dsc;
    var_dsc lst_dsc;
    sequence *s;

    PPPredRange range;

    bool first_time;
    bool once;
    bool result_ready;
    bool any;
    bool eos_reached;
    bool need_reopen;

    int upper_bound;
    int lower_bound;


    inline void reinit_consumer_table();

    void children(PPOpIn& _source_child_,
                  PPOpIn& _data_child_)
    {
        _source_child_ = source_child;
        _data_child_ = data_child;
    }

/*
    PPPred2(variable_context *_cxt_,
             arr_of_var_dsc _var_dscs_, 
             PPOpIn _source_child_, 
             PPOpIn _data_child_,
             tuple _source_);
*/
public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    
    PPPred2(variable_context *_cxt_,
            arr_of_var_dsc _var_dscs_, 
            PPOpIn _source_child_, 
            arr_of_PPOpIn _conjuncts_,
	        arr_of_comp_cond _conditions_,
            PPOpIn _data_child_,
   	        bool _once_,
            var_dsc _lst_dsc_,
            var_dsc _pos_dsc_ = -1);

    virtual ~PPPred2();

    virtual var_c_id register_consumer(var_dsc dsc);
    virtual void next(tuple &t, var_dsc dsc, var_c_id id);
    virtual void reopen(var_dsc dsc, var_c_id id);
};




#endif
