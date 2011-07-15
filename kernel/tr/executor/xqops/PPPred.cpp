/*
 * File:  PPPred.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <math.h>
#include <algorithm>
#include <functional>

#include "common/sedna.h"

#include "tr/executor/xqops/PPPred.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/visitor/PPVisitor.h"


using namespace std;

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPPredRange
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

class int_less : public std::unary_function<int, bool> 
{
    int v;
public:
    explicit int_less(const int& _v_): v(_v_) {}
    bool operator( ) ( int& val ) { return val < v; }
};

class int_greater : public std::unary_function<int, bool> 
{
    int v;
public:
    explicit int_greater(const int& _v_): v(_v_) {}
    bool operator( ) ( int& val ) { return val > v; }
};

class int_less_equal : public std::unary_function<int, bool> 
{
    int v;
public:
    explicit int_less_equal(const int& _v_): v(_v_) {}
    bool operator( ) ( int& val ) { return val <= v; }
};

class int_greater_equal : public std::unary_function<int, bool> 
{
    int v;
public:
    explicit int_greater_equal(const int& _v_): v(_v_) {}
    bool operator( ) ( int& val ) { return val >= v; }
};


  
PPPredRange::PPPredRange()
{
    upper_bound = 0;
    lower_bound = 1;
    state = RS_INITIAL;
}

int PPPredRange::reinit_with_position(double position)
{
    reinit();
    int integer_value = (position == ceil(position)) ? (int) position : -1; 
    if(is_position_in_range(integer_value)) 
    {
        state = RS_POINTS;
        points.push_back(integer_value);
    }
    else
        state = RS_EMPTY;
    return state;
}


void PPPredRange::reinit()
{
    upper_bound = 0;
    lower_bound = 1;
    state = RS_INITIAL;
    points.clear();
    except_points.clear();
}

int PPPredRange::get_max_posible_position()
{
    switch(state)
    {
        case RS_INITIAL: return 0; 
        case RS_EMPTY: return -1;
        case RS_RANGE: 
        {
            if(upper_bound == 0) return 0;
            int temp = upper_bound;
            while(temp >= lower_bound)
            {
                if(is_position_in_range(temp)) return temp;
                temp--;
            }
            return -1;
        }
        case RS_POINTS: return points.size() > 0 ? *max_element(points.begin(), points.end()) : -1; 
        default:
            throw USER_EXCEPTION2(SE1003, "Unexpected state of the PPPredRange instance");
    }                                                                                                                                    
}

int PPPredRange::get_min_posible_position()
{
    switch(state)
    {
        case RS_INITIAL: return 1; 
        case RS_EMPTY: return -1;
        case RS_RANGE: 
        {
            int temp = lower_bound;
            while(upper_bound == 0 ? true : temp <= upper_bound )
            {
                if(is_position_in_range(temp)) return temp;
                temp++;
            }
            return -1;
        }
        case RS_POINTS: return points.size() > 0 ? *min_element(points.begin(), points.end()) : -1;
        default:
            throw USER_EXCEPTION2(SE1003, "Unexpected state of the PPPredRange instance");
    }                                                                                                                                    
}


int PPPredRange::add_new_constraint(operation_compare_condition occ, const PPOpIn &conjunct)
{
    if(state == RS_EMPTY) return state;
    
    tuple t(conjunct.ts);
    
    if(is_occ_value(occ))
    {
        conjunct.op->next(t);
        
        if(t.is_eos()) state = RS_EMPTY;
        else 
        {
            conjunct.op->reopen();
            
            double double_value;
            int integer_value;

            tuple_cell tc = t.cells[0];
            tc = atomize(tc);
            
            if(is_numeric_type(tc.get_atomic_type()))
                double_value = get_numeric_value(tc);
            else
                throw XQUERY_EXCEPTION2(XPTY0004, "There is a not valid combination of types in value comparison");
            
            switch(occ)
            {
                
                case OCC_VALUE_EQUAL:

                    integer_value = (double_value == ceil(double_value)) ? (int) double_value : -1;
                    if(!is_position_in_range(integer_value))
                        state = RS_EMPTY;
                    else
                    {
                        points.clear();
                        points.push_back(integer_value);
                        state = RS_POINTS;
                    }
                    break;

                case OCC_VALUE_NOT_EQUAL:

                    integer_value = (double_value == ceil(double_value)) ? (int) double_value : -1;
                    if(!is_position_in_range(integer_value)) break;
                    else
                    {
                        if(state != RS_POINTS) 
                        {
                            except_points.push_back(integer_value);
                            state = RS_RANGE;
                        }
                        else
                            points.remove(integer_value);
                    }
                    break;

                case OCC_VALUE_LESS:

                    position_less_than(double_value);
                    break;

                case OCC_VALUE_GREATER:

                    position_greater_than(double_value);
                    break;
                
                case OCC_VALUE_LESS_EQUAL:
                    
                    position_less_equal_than(double_value);
                    break;

                case OCC_VALUE_GREATER_EQUAL:
                    
                    position_greater_equal_than(double_value);
                    break;

                default: 
                    throw USER_EXCEPTION2(SE1003, "Unexpected value comparison type in PPPredRange");
            }
        }
    }
    else if(is_occ_general(occ))
    {
        std::vector<double> double_values;
        std::list<int> integer_values;
        
        int integer_value;
        double double_value;

        while(true)
        {
            conjunct.op->next(t);
            if(t.is_eos()) break;
            tuple_cell tc = t.cells[0];
            tc = atomize(tc);
            
            if(is_numeric_type(tc.get_atomic_type()))
            {
                double_values.push_back(get_numeric_value(tc));
            }
            else if(tc.get_atomic_type() == xs_untypedAtomic)
            {
                tc = cast_primitive_to_xs_double(tc);
                double_values.push_back(tc.get_xs_double());
            }
        }   
        
        if(double_values.size() == 0) state = RS_EMPTY;
        else
        {
            unsigned int i;
            switch(occ)
            {
                case OCC_GENERAL_EQUAL:
                
                    for(i = 0; i < double_values.size(); i++)
                    {
                        double_value = double_values[i];
                        integer_value = (double_value == ceil(double_value)) ? (int) double_value : -1;
                        if(is_position_in_range(integer_value)) integer_values.push_back(integer_value);
                    }
                    if(integer_values.size() == 0) state = RS_EMPTY;
                    else
                    {
                        state = RS_POINTS;
                        integer_values.sort();
                        integer_values.unique();
                        points = integer_values;
                    }
                    break;

                case OCC_GENERAL_NOT_EQUAL:

                    for(i = 0; i < double_values.size(); i++)
                    {
                        double_value = double_values[i];
                        integer_value = (double_value == ceil(double_value)) ? (int) double_value : -1;
                        if(!is_position_in_range(integer_value)) break;
                        else integer_values.push_back(integer_value);
                    }
                    if(integer_values.size() != double_values.size()) break;
                    integer_values.sort();
                    integer_values.unique();
                    if(integer_values.size() > 1) break;
                    else  
                    {
                        if(state != RS_POINTS) 
                        {
                            except_points.push_back(*integer_values.begin());
                            state = RS_RANGE;
                        }
                        else
                            points.remove(*integer_values.begin());
                    }
                    break;  
                    
                case OCC_GENERAL_LESS:

                    double_value = *max_element(double_values.begin(), double_values.end());
                    position_less_than(double_value);
                    break;

                case OCC_GENERAL_GREATER:

                    double_value = *min_element(double_values.begin(), double_values.end());
                    position_greater_than(double_value);
                    break;

                case OCC_GENERAL_LESS_EQUAL:

                    double_value = *max_element(double_values.begin(), double_values.end());
                    position_less_equal_than(double_value);
                    break;

                case OCC_GENERAL_GREATER_EQUAL:

                    double_value = *min_element(double_values.begin(), double_values.end());
                    position_greater_equal_than(double_value);
                    break;

                default:
                    throw USER_EXCEPTION2(SE1003, "Unexpected general comparison type in PPPredRange");

            }
        }
    }
    else 
        throw USER_EXCEPTION2(SE1003, "Unexpected operation compare condition in PPPredRange");

    if(is_empty()) state = RS_EMPTY;    
    //print_state();   //if you want to try to debug this hell :))
    return state;
}


void PPPredRange::position_less_than(double double_value)
{
    if(double_value <= 1) state = RS_EMPTY;
    else
    {
        int integer_value = (int) ceil(double_value);
        int max_position = get_max_posible_position();
        int min_position = get_min_posible_position();
        if( max_position != 0 && max_position < integer_value) return;
        if( integer_value <= min_position ) { state = RS_EMPTY; return; }  
        
        if(state == RS_POINTS)
            points.remove_if(int_greater_equal(integer_value));     
        else
        {
            if(state == RS_RANGE) except_points.remove_if(int_greater_equal(integer_value));
            state = RS_RANGE;
            upper_bound = integer_value - 1;
        }
    }
}

void PPPredRange::position_greater_than(double double_value)
{
    if(double_value < 1) return;
    else
    {
        int integer_value = (int) floor(double_value);
        int max_position = get_max_posible_position();
        int min_position = get_min_posible_position();
        if( max_position != 0 && max_position <= integer_value) { state = RS_EMPTY; return; }  
        if( integer_value < min_position ) return;
        
        if(state == RS_POINTS)
            points.remove_if(int_less_equal(integer_value));        
        else
        {
            if(state == RS_RANGE) except_points.remove_if(int_less_equal(integer_value));
            state = RS_RANGE;
            lower_bound = integer_value + 1;
        }
    }
}

void PPPredRange::position_less_equal_than(double double_value)
{
    if(double_value < 1) state = RS_EMPTY;
    else
    {
        int integer_value = (int) floor(double_value);
        int max_position = get_max_posible_position();
        int min_position = get_min_posible_position();
        if( max_position != 0 && max_position <= integer_value) return;
        if( integer_value < min_position ) { state = RS_EMPTY; return; }  
    
        if(state == RS_POINTS)
        points.remove_if(int_greater(integer_value));       
            else
        {
            if(state == RS_RANGE) except_points.remove_if(int_greater(integer_value));
            state = RS_RANGE;
            upper_bound = integer_value;
        }
    }
}

void PPPredRange::position_greater_equal_than(double double_value)
{
    if(double_value <= 1) return;
    else
    {
        int integer_value = (int) ceil(double_value);
        int max_position = get_max_posible_position();
        int min_position = get_min_posible_position();
        if( max_position != 0 && max_position < integer_value) { state = RS_EMPTY; return; }  
        if( integer_value <= min_position ) return;

        if(state == RS_POINTS)
            points.remove_if(int_less(integer_value));      
        else
        {
            if(state == RS_RANGE) except_points.remove_if(int_less(integer_value));
            state = RS_RANGE;
            lower_bound = integer_value;
        }
    }
}

bool PPPredRange::is_position_in_range(int pos)
{
    if(pos <= 0) return false;

    switch(state)
    {
        case RS_INITIAL: return true; 
        case RS_EMPTY: return false;
        case RS_RANGE: return (pos >= lower_bound) && 
                              (upper_bound == 0 ? true : pos <= upper_bound) && 
                              find(except_points.begin(), except_points.end(), pos) == except_points.end();
        case RS_POINTS: return points.size() > 0 ? find(points.begin(), points.end(), pos) != points.end()
                                                 : false;
        default:
            throw USER_EXCEPTION2(SE1003, "Unexpected state of the PPPredRange instance");
    }                                                                                                                                    
}

bool PPPredRange::is_empty()
{
    switch(state)
    {
        case RS_INITIAL: return false; 
        case RS_EMPTY: return true;
        case RS_RANGE: 
        {
            if(upper_bound == 0) return false;
            int power = upper_bound - lower_bound + 1;
            if(power > (signed)except_points.size()) return false;
            else return true;
        }
        case RS_POINTS: return points.size() > 0 ? false : true;
        default:
            throw USER_EXCEPTION2(SE1003, "Unexpected state of the PPPredRange instance");
    }                                                                                                                                       
}

bool PPPredRange::is_any()
{
    switch(state)
    {
        case RS_INITIAL: return true; 
        case RS_EMPTY: return false;
        case RS_RANGE: 
            if(upper_bound == 0 && 
               lower_bound == 1 && 
               except_points.size() == 0) return true;
            else return false;
        case RS_POINTS: return false;
        default:
            throw USER_EXCEPTION2(SE1003, "Unexpected state of the PPPredRange instance");
    }                                                                                                                                       
}

void PPPredRange::print_state()
{
    cout << "\n";
    std::list<int>::iterator itr;
    cout << "======================================================\n";
    switch(state)
    {
        case RS_INITIAL: 
            cout << "State is RS_INITIAL\n";
            break;
        case RS_EMPTY: 
            cout << "State is RS_EMPTY\n";
            break;
        case RS_RANGE: 
            cout << "State is RS_RANGE\n";
            cout << "lower bound   >> " << lower_bound << "\n";
            cout << "upper bound   >> " << upper_bound << "\n";
            cout << "except points >> ";
            itr = except_points.begin();
            while(itr != except_points.end())
            {
                cout << *itr << "  ";
                itr++;
            }
            cout << "\n";
            break;

        case RS_POINTS:
            cout << "State is RS_POINTS\n";
            cout << "points >> ";
            itr = points.begin();
            while(itr != points.end())
            {
                cout << *itr << "  ";
                itr++;
            }
            cout << "\n";
            break;

        default:
            throw USER_EXCEPTION2(SE1003, "Unexpected state of the PPPredRange instance");
    }
    cout << "======================================================\n";
    cout << "\n";
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPPred1
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPPred1::PPPred1(dynamic_context *_cxt_,
                 operation_info _info_,
                 arr_of_var_dsc _var_dscs_, 
                 PPOpIn _source_child_, 
                 arr_of_PPOpIn _conjuncts_,
                 arr_of_comp_cond _conditions_,
                 PPOpIn _data_child_,
                 bool _once_,
                 var_dsc _pos_dsc_ ): PPVarIterator(_cxt_, _info_, "PPPred1"),
                                      var_dscs(_var_dscs_),
                                      source_child(_source_child_),
                                      conjuncts(_conjuncts_),
                                      data_child(_data_child_),
                                      conditions(_conditions_),
                                      data(_data_child_.ts),
                                      once(_once_),
                                      pos_dsc(_pos_dsc_)
                                      
{
    if(conjuncts.size() != conditions.size()) 
        throw USER_EXCEPTION2(SE1003, "Quantities of conjuncts and conditions are not equal in PPPred1");
}

PPPred1::~PPPred1()
{
    delete source_child.op;
    source_child.op = NULL;
    
    for(unsigned int i = 0; i < conjuncts.size(); i++)
    {
        delete (conjuncts[i].op);
        conjuncts[i].op = NULL;
    }
    
    delete data_child.op;            
    data_child.op = NULL;
}


void PPPred1::do_open ()
{
    source_child.op->open();

    pos = 0;
    cur_tuple = NULL;
    first_time = true;
    result_ready = false;
    eos_reached = true;
    need_reopen = false;

    for (unsigned int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->get_var_producer(var_dscs[i], var_cxt);
        p.type = pt_lazy_simple;
        p.op = this;
        p.svc = se_new simple_var_consumption;
        p.tuple_pos = i;
    }

    if (pos_dsc != INVALID_VAR_DSC)
    {
        producer &p = cxt->get_var_producer(pos_dsc, var_cxt);
        p.type = pt_lazy_simple;
        p.op = this;
        p.svc = se_new simple_var_consumption;
        p.tuple_pos = 0;
    }

    for(unsigned int i = 0; i < conjuncts.size(); i++)
        (conjuncts[i].op) -> open();
    
    data_child.op->open();
    
}

void PPPred1::do_reopen()                                             
{
    pos = 0;
    first_time = true;
    result_ready = false;
    source_child.op->reopen();
}

void PPPred1::do_close()
{
    source_child.op->close();
    
    for(unsigned int i = 0; i < conjuncts.size(); i++)
        (conjuncts[i].op) -> close();
     
    data_child.op->close();
}
                                                                          
void PPPred1::do_next(tuple &t)
{
    if(need_reopen)
    {
        need_reopen = false;
        if(!eos_reached) data_child.op->reopen();
        reinit_consumer_table();
    }
    
    if(first_time)
    {
        /* If tail expresssion (data child not representable as conjunct)
         * doesn't depend on source expression we can evaluate data child once
         * and buffer its value */
        if(once)
        {
            need_reopen = true;
            if(conjuncts.size() != 0) 
            {
                tuple_cell tc = effective_boolean_value(data_child, data, eos_reached);
                if( tc.get_xs_boolean() )
                {
                    range.reinit();
                    for(unsigned int i = 0; i < conjuncts.size(); i++)
                        range.add_new_constraint(conditions[i], conjuncts[i]);  
                    if(range.is_empty()) { t.set_eos(); return;}
                    else if(range.is_any()) result_ready = true; 
                }
                else { t.set_eos(); return;}
            }
            else
            {
                bool is_numeric;
                double value;
                tuple_cell tc = predicate_boolean_and_numeric_value(data_child, data, eos_reached, is_numeric, value);
                if(is_numeric)
                {
                    range.reinit_with_position(value);
                    if(range.is_empty()) { t.set_eos(); return;}
                }                                       
                else if( tc.get_xs_boolean() ) result_ready = true;
                else { t.set_eos(); return;}
            }
        }
        else 
        {
            range.reinit();
            for(unsigned int i = 0; i < conjuncts.size(); i++)
                range.add_new_constraint(conditions[i], conjuncts[i]);  
            if(range.is_empty()) { t.set_eos(); return;} 
        }
        
        first_time = false;
        
        if(!result_ready)
        {
            any = range.is_any();
            if( !any )
            {
                lower_bound = range.get_min_posible_position();
                upper_bound = range.get_max_posible_position();
            }
        }
    }

    if(result_ready)    
    {
        source_child.op->next(t);
        ++pos;
        cur_tuple = &t;
        if(!t.is_eos()) return;
    }   
    else    
    {
        while (true)
        {
            source_child.op->next(t);
            ++pos;
            cur_tuple = &t;
    
            if(t.is_eos()) break;
    
            if( !any )
            {
                if(upper_bound != 0 && pos > upper_bound)
                { 
                    source_child.op->reopen();
                    break;
                }
                if(pos < lower_bound || !range.is_position_in_range(pos)) continue;
            }
            
            if( !once )
            {
                if(need_reopen)
                {
                    if(!eos_reached) data_child.op->reopen();
                    reinit_consumer_table();
                }
                
                tuple_cell tc = (conjuncts.size() == 0) ? predicate_boolean_value(data_child, data, eos_reached, pos) :
                                                          effective_boolean_value(data_child, data, eos_reached);
                need_reopen = true;
                if(tc.get_xs_boolean()) return;
            }
            else return;
        }
    }
    
    pos = 0;
    result_ready = false;
    first_time = true; 
    t.set_eos();
}

PPIterator* PPPred1::do_copy(dynamic_context *_cxt_)                      
{
    
    PPPred1 *res = se_new PPPred1(_cxt_,
                               info,
                               var_dscs, 
                               source_child, 
                               conjuncts, 
                               conditions,
                               data_child,
                               once,
                               pos_dsc);
    
    for (unsigned int i = 0; i < conjuncts.size(); i++)
        res->conjuncts[i].op = conjuncts[i].op->copy(_cxt_);

    
    res->source_child.op = source_child.op->copy(_cxt_);     
    res->data_child.op = data_child.op->copy(_cxt_);
    return res;
}

void PPPred1::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    for (unsigned int i = 0; i < conjuncts.size(); i++)
        conjuncts[i].op->accept(v);
    source_child.op->accept(v);
    data_child.op->accept(v);
    v.pop();
}


var_c_id PPPred1::do_register_consumer(var_dsc dsc)
{
    cxt->get_var_producer(dsc, var_cxt).svc->push_back(true);
    return cxt->get_var_producer(dsc, var_cxt).svc->size() - 1;
}

void PPPred1::do_next(tuple &t, var_dsc dsc, var_c_id id)                     
{
    producer &p = cxt->get_var_producer(dsc, var_cxt);

    if (p.svc->at(id))
    {
        p.svc->at(id) = false;
        t.copy(dsc == pos_dsc ? tuple_cell::atomic((int64_t)pos)
                              : cur_tuple->cells[p.tuple_pos]);
    }
    else 
    {
        p.svc->at(id) = true;
        t.set_eos();
    }
}

void PPPred1::do_reopen(var_dsc dsc, var_c_id id)
{
    cxt->get_var_producer(dsc, var_cxt).svc->at(id) = true;
}

void PPPred1::do_close(var_dsc dsc, var_c_id id)
{
}

inline void PPPred1::reinit_consumer_table()
{
    unsigned int j;
    for (unsigned int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->get_var_producer(var_dscs[i], var_cxt);
        for (j = 0; j < p.svc->size(); j++) p.svc->at(j) = true;
    }

    if (pos_dsc != INVALID_VAR_DSC)
    {
        producer &p = cxt->get_var_producer(pos_dsc, var_cxt);
        for (j = 0; j < p.svc->size(); j++) p.svc->at(j) = true;
    }
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPPred2
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPPred2::PPPred2(dynamic_context *_cxt_,
                 operation_info _info_,
                 arr_of_var_dsc _var_dscs_, 
                 PPOpIn _source_child_,
                 arr_of_PPOpIn _conjuncts_, 
                 arr_of_comp_cond _conditions_,
                 PPOpIn _data_child_,
                 bool _once_,
                 var_dsc _lst_dsc_,
                 var_dsc _pos_dsc_) : PPVarIterator(_cxt_, _info_, "PPPred2"),
                                      var_dscs(_var_dscs_),
                                      source_child(_source_child_),
                                      conjuncts(_conjuncts_),
                                      data_child(_data_child_),
                                      conditions(_conditions_),
                                      data(_data_child_.ts),
                                      pos_dsc(_pos_dsc_),
                                      lst_dsc(_lst_dsc_),
                                      s(NULL),
                                      once(_once_)
{
    if(conjuncts.size() != conditions.size()) 
        throw USER_EXCEPTION2(SE1003, "Quantities of conjuncts and conditions are not equal in PPPred2");
}

PPPred2::~PPPred2()
{
    delete source_child.op;
    source_child.op = NULL;

    for(unsigned int i = 0; i < conjuncts.size(); i++)
    {
        delete (conjuncts[i].op);
        conjuncts[i].op = NULL;
    }
        
    delete data_child.op;
    data_child.op = NULL;
}

void PPPred2::do_open ()
{
    source_child.op->open();

    s = se_new sequence(source_child.ts);
    first_time = true;
    result_ready = false;
    cur_tuple = NULL;
    eos_reached = false;
    need_reopen = false;
    pos = 0;


    for (unsigned int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->get_var_producer(var_dscs[i], var_cxt);
        p.type = pt_lazy_simple;
        p.op = this;
        p.svc = se_new simple_var_consumption;
        p.tuple_pos = i;
    }
    if(pos_dsc != INVALID_VAR_DSC)
    {
        producer &p = cxt->get_var_producer(pos_dsc, var_cxt);
        p.type = pt_lazy_simple;
        p.op = this;
        p.svc = se_new simple_var_consumption;
        p.tuple_pos = 0;
    }
    {
        producer &p = cxt->get_var_producer(lst_dsc, var_cxt);
        p.type = pt_lazy_simple;
        p.op = this;
        p.svc = se_new simple_var_consumption;
        p.tuple_pos = 0;
    }

    for(unsigned int i = 0; i < conjuncts.size(); i++)
        (conjuncts[i].op) -> open();
   
    data_child.op->open();
}

void PPPred2::do_reopen()
{
    reinit_consumer_table();    //In PPPred2 conjucts can use lst_dsc to get last().
                                //Every conjunct is reopend in the PPPredRange::add_new_constraint(), but
                                //we must also reinit table.
    source_child.op->reopen();
    first_time = true;
    result_ready = false;
    s->clear();
    pos = 0;
}

void PPPred2::do_close()
{
    source_child.op->close();

    for(unsigned int i = 0; i < conjuncts.size(); i++)
        (conjuncts[i].op) -> close();

    data_child.op->close();

    delete s;
    s = NULL;
}

void PPPred2::do_next(tuple &t)
{
    if(need_reopen)
    {
        need_reopen = false;
        if(!eos_reached) data_child.op->reopen();
        reinit_consumer_table();
    }
    
    if(first_time)
    {
        if(s->size() != 0) s->clear();
        while (true)
        {
            source_child.op->next(t);
            if (t.is_eos()) break;
            else s->add(t);
        }
        if(s->size() == 0) return;

        /* If tail expresssion (data child not representable as conjunct)
         * doesn't depend on source expression we can evaluate data child once
         * and buffer its value */
        if(once)
        {
            need_reopen = true;
            if(conjuncts.size() != 0) 
            {
                tuple_cell tc = effective_boolean_value(data_child, data, eos_reached);
                if( tc.get_xs_boolean() )
                {
                    range.reinit();
                    for(unsigned int i = 0; i < conjuncts.size(); i++)
                        range.add_new_constraint(conditions[i], conjuncts[i]);  
                    if(range.is_empty()) { t.set_eos(); return; }
                    else if(range.is_any()) result_ready = true; 
                }
                else { t.set_eos(); return; }
            }
            else
            {
                bool is_numeric;
                double value;
                tuple_cell tc = predicate_boolean_and_numeric_value(data_child, data, eos_reached, is_numeric, value);
                if(is_numeric)
                {
                    range.reinit_with_position(value);
                    if(range.is_empty()) { t.set_eos(); return; }
                }
                else if( tc.get_xs_boolean() ) result_ready = true;
                else { t.set_eos(); return; }
            }
        }
        else
        {
            range.reinit();
            for(unsigned int i = 0; i < conjuncts.size(); i++)
                range.add_new_constraint(conditions[i], conjuncts[i]);  
            if(range.is_empty()) { t.set_eos(); return; } 
        }
        
        first_time = false;

        if(!result_ready)
        {
            any = range.is_any();
            if( !any )
            {
                lower_bound = range.get_min_posible_position();
                upper_bound = range.get_max_posible_position();
            }
        }
    }

    if(result_ready)    
    {
        if(pos < s->size()) 
        {
            s->get(t, pos++);
            return;
        }
    }   
    else
    {    
        while (pos < s->size())
        {
            s->get(t, pos++);
            cur_tuple = &t;

            if( !any )
            {
                if(upper_bound != 0 && pos > upper_bound) break;
                if(pos < lower_bound || !range.is_position_in_range(pos)) continue;
            }
            
            if( !once )
            {
                if(need_reopen)
                {
                    if(!eos_reached) data_child.op->reopen();
                    reinit_consumer_table();
                }
                tuple_cell tc = (conjuncts.size() == 0) ? predicate_boolean_value(data_child, data, eos_reached, pos) :
                                                          effective_boolean_value(data_child, data, eos_reached);
                need_reopen = true;
                if(tc.get_xs_boolean()) return;
            }
            else return;
        }
    }
    
    result_ready = false;
    first_time = true;
    t.set_eos();
    pos = 0;
}

PPIterator* PPPred2::do_copy(dynamic_context *_cxt_)
{
    PPPred2 *res = se_new PPPred2(_cxt_, 
                               info,
                               var_dscs, 
                               source_child, 
                               conjuncts, 
                               conditions,
                               data_child,
                               once,
                               lst_dsc,
                               pos_dsc);
    
    for (unsigned int i = 0; i < conjuncts.size(); i++)
        res->conjuncts[i].op = conjuncts[i].op->copy(_cxt_);
    
    res->source_child.op = source_child.op->copy(_cxt_);     
    res->data_child.op = data_child.op->copy(_cxt_);
    return res;
}

void PPPred2::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    for (unsigned int i = 0; i < conjuncts.size(); i++)
        conjuncts[i].op->accept(v);
    source_child.op->accept(v);
    data_child.op->accept(v);
    v.pop();
}


var_c_id PPPred2::do_register_consumer(var_dsc dsc)
{
    cxt->get_var_producer(dsc, var_cxt).svc->push_back(true);
    return cxt->get_var_producer(dsc, var_cxt).svc->size() - 1;
}

void PPPred2::do_next(tuple &t, var_dsc dsc, var_c_id id)
{
    producer &p = cxt->get_var_producer(dsc, var_cxt);

    if (p.svc->at(id))
    {
        p.svc->at(id) = false;
        if (dsc == pos_dsc)         t.copy(tuple_cell::atomic((int64_t)pos));
        else if (dsc == lst_dsc)    t.copy(tuple_cell::atomic((int64_t)(s->size())));
        else                        t.copy(cur_tuple->cells[p.tuple_pos]);
    }
    else 
    {
        p.svc->at(id) = true;
        t.set_eos();
    }
}

void PPPred2::do_reopen(var_dsc dsc, var_c_id id)
{
    cxt->get_var_producer(dsc, var_cxt).svc->at(id) = true;
}

void PPPred2::do_close(var_dsc dsc, var_c_id id)
{
}

inline void PPPred2::reinit_consumer_table()
{
    unsigned int j;
    for (unsigned int i = 0; i < var_dscs.size(); i++)
    {
        producer &p = cxt->get_var_producer(var_dscs[i], var_cxt);
        for (j = 0; j < p.svc->size(); j++) p.svc->at(j) = true;
    }
    if (pos_dsc != INVALID_VAR_DSC)
    {
        producer &p = cxt->get_var_producer(pos_dsc, var_cxt);
        for (j = 0; j < p.svc->size(); j++) p.svc->at(j) = true;
    }
    {
        producer &p = cxt->get_var_producer(lst_dsc, var_cxt);
        for (j = 0; j < p.svc->size(); j++) p.svc->at(j) = true;
    }
}
