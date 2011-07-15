/*
 * File:  PPPred.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PPPRED_H
#define __PPPRED_H

#include <list>

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"



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

typedef std::vector<operation_compare_condition>    arr_of_comp_cond;

inline const char* 
operation_compare_condition2string(operation_compare_condition cond)
{
    switch(cond)
    {
    case OCC_VALUE_EQUAL: return "eq";
    case OCC_VALUE_NOT_EQUAL: return "ne";
    case OCC_VALUE_LESS: return "lt";
    case OCC_VALUE_GREATER: return "gt";
    case OCC_VALUE_LESS_EQUAL: return "le";
    case OCC_VALUE_GREATER_EQUAL: return "ge";
    case OCC_GENERAL_EQUAL: return "=";
    case OCC_GENERAL_NOT_EQUAL: return "!=";
    case OCC_GENERAL_LESS: return "<";
    case OCC_GENERAL_GREATER: return ">";
    case OCC_GENERAL_LESS_EQUAL: return "<=";
    case OCC_GENERAL_GREATER_EQUAL: return ">=";
    default: throw USER_EXCEPTION2(SE1003, "Impossible conjunt comparison type in operation compare condition conversion to string");
    }
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPPredRange -
/// Just helper to evaluate ranges on line
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
    
    void position_less_than(double double_value);
    void position_greater_than(double double_value);
    void position_less_equal_than(double double_value);
    void position_greater_equal_than(double double_value);
    
    void print_state();

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
/// PPPred1 - encapulates evaluation of the predicate in XPath expressions.
///
/// Each predicate can be considered like:
///
/// [(position() cmp N) and (position() cmp K)       ... 
///        ...      (position() cmp M) and TailExpr]
///
/// Each position() cmp N subexpression is referred later as 'conjunct'
/// 'TailExpr' is referred as 'data child' - represents 'tail' of the 
/// predicate which can't be presented in form of conjuncts. 
///
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

class PPPred1 : public PPVarIterator
{
private:
    arr_of_var_dsc var_dscs;

    PPOpIn source_child;          //returns result of the axis::filter part of
                                  //path expression step this predicate is applied to

    arr_of_PPOpIn conjuncts;      //right part of the conjuncts (some number usually)
                                  //e.g. for {position() < 1} conjunct we'll have 
                                  //PPConst(xs:integer(1))

    PPOpIn data_child;            //tail of the predicate which can't be presented
                                  //as conjunct

    arr_of_comp_cond conditions;  //list of comparison types of conjuncts,
                                  //e.g. for {position() < 1} conjunct we'll have 
                                  //OCC_GENERAL_LESS condition

    tuple *cur_tuple;
    tuple data;
    PPPredRange range;

    bool first_time;              //as-is, we call next() for the first time

    bool once;                    //if data child doesn't depend on source child
                                  //we can evaluate all conjuncts and tail of 
                                  //the predicate only once - when next() is called
                                  //for the first time
    bool result_ready;

    bool eos_reached;             //determines if we need to reopen operation after
                                  //effective boolean (or numeric) expression
                                  //evaluation (for tail non-conjunct expression,
                                  //aka data_child)

    bool need_reopen;             //we need reopen tail non-conjunct expression sometimes
                                  //(aka data_child) when the next() is called

    bool any;                     //if after evaluation of all conjuncts and inserting
                                  //of theirs results into PPPredRange instance, it
                                  //doesn't restricts position, e.g:
                                  //position() > -1 and position() ne 1.5

    int pos;
    int upper_bound;              //just caches bound after the predevaluation
    int lower_bound;              //just caches bound after the predevaluation

    var_dsc pos_dsc;              //returns current position (int64_t) of the tuple
                                  //source child

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
    PPPred1(dynamic_context *_cxt_,
            operation_info _info_,
            arr_of_var_dsc _var_dscs_, 
            PPOpIn _source_child_, 
            arr_of_PPOpIn _conjuncts_,
            arr_of_comp_cond _conditions_,
            PPOpIn _data_child_,
            bool _once_,
            var_dsc _pos_dsc_ = -1);
    
    virtual ~PPPred1();

    inline bool is_once()                         { return once; }
    inline arr_of_comp_cond::size_type get_conjuncts_number()    { return conjuncts.size(); }
    inline operation_compare_condition get_conjunt_comparison_type(arr_of_comp_cond::size_type i)
    {
        U_ASSERT(i < conjuncts.size());
        return conditions.at(i);
    }
    /* Returns -1 if position var is not used */
    inline var_dsc get_position_var_dsc() { return pos_dsc; }
    inline const arr_of_var_dsc& get_variable_descriptors() { return var_dscs; }
};




///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPPred2 -as PPPred1, but predicate contains last() function call,
///          so that we need to use sequence* s to evaluate last position
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
class PPPred2 : public PPVarIterator
{
private:
    arr_of_var_dsc var_dscs;

    PPOpIn source_child;          //expression which returns result of the
                                  //axis::filter expression of the XPath step
                                  //which contains this predicate

    arr_of_PPOpIn conjuncts;      //conjuncts like position() < 2, position() > 1
    PPOpIn data_child;            //represents 'tail' of the predicate which
                                  //can't be considered as [position() smt. N]

    arr_of_comp_cond conditions;  //types of comparison operations in conjunts
                                  //e.g. position() < 1 corresponds to OCC_VALUE_LESS
    tuple data;
    tuple *cur_tuple;
    int pos;
    var_dsc pos_dsc;              //returns current position (int64_t) of the source child
    var_dsc lst_dsc;              //returns last source child tuple through this variable
    sequence *s;

    PPPredRange range;

    bool first_time;        //as-is - next is called for the first time

    bool once;              //data child doesn't depend on source child - so that
                            //we can predevaluate its numeric or boolean value
                            //when next is called for the first time
    bool result_ready;

    bool any;               //after evaluation of all conjuncts and inserting
                            //theirs values into PPPredRange instance - 
                            //range doesn't restrict position at all,
                            //for example [position() > -1]

    bool eos_reached;       //determines if we need to reopen operation after
                            //effective boolean (or numeric) expression
                            //evaluation (for tail non-conjunct expression,
                            //aka data_child)

    bool need_reopen;       //we need reopen tail non-conjunct expression sometimes
                            //(aka data_child) when the next() is called


    int upper_bound;        //just caches preevaluated bound
    int lower_bound;        //just caches preevaluated bound

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
    PPPred2(dynamic_context *_cxt_,
            operation_info _info_,
            arr_of_var_dsc _var_dscs_, 
            PPOpIn _source_child_, 
            arr_of_PPOpIn _conjuncts_,
            arr_of_comp_cond _conditions_,
            PPOpIn _data_child_,
            bool _once_,
            var_dsc _lst_dsc_,
            var_dsc _pos_dsc_ = -1);

    virtual ~PPPred2();
    
    inline bool is_once()                          { return once; }
    inline arr_of_comp_cond::size_type get_conjuncts_number()    { return conjuncts.size(); }
    inline operation_compare_condition get_conjunt_comparison_type(arr_of_comp_cond::size_type i)
    {
        U_ASSERT(i < conjuncts.size());
        return conditions.at(i);
    }
    /* Returns -1 if position var is not used */
    inline var_dsc get_position_var_dsc() { return pos_dsc; }
    inline var_dsc get_last_var_dsc()     { return lst_dsc; }
    inline const arr_of_var_dsc& get_variable_descriptors() { return var_dscs; }
};

#endif /* __PPPRED_H */
