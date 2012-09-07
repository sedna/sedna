/*
 * File:  PPGeneralComparison.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPGENERALCOMP_H
#define _PPGENERALCOMP_H

#include "common/sedna.h"

#include "tr/executor/base/tuple.h"
#include "tr/executor/base/PPUtils.h"

inline tuple_cell getAtomizedCell(tuple& tup)
{
    if (tup.size() != 1) {
        throw XQUERY_EXCEPTION2(XPTY0004, "Name argument of Constructor is not a single atomic value");
    };

    return atomize(tup.cells[0]);
}

///////////////////////////////////////////////////////////////////////////////
/// PPGeneralComparison
///////////////////////////////////////////////////////////////////////////////
class PPGeneralComparison : public PPIterator
{
protected:
    PPOpIn seq1;
	PPOpIn seq2;
    bool first_time;
    bool eos_reached1;
	bool eos_reached2;
    CollationHandler *handler;

    PPGeneralComparison(dynamic_context *_cxt_,
                        operation_info _info_,
                        PPOpIn _seq1_,
                        PPOpIn _seq2_);

    void generalNodePrepare(tuple_cell& cell1, tuple_cell& cell2);

private:   
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:

    virtual ~PPGeneralComparison();
	
    /* Factories for General Comparisons */
    static PPGeneralComparison* PPGTGeneralComparison(dynamic_context *_cxt_,
                                                      operation_info _info_, 
                                                      PPOpIn _seq1_,
                                                      PPOpIn _seq2_); 

	static PPGeneralComparison* PPLTGeneralComparison(dynamic_context *_cxt_,
                                                      operation_info _info_, 
                                                      PPOpIn _seq1_,
                                                      PPOpIn _seq2_); 

	static PPGeneralComparison* PPGEGeneralComparison(dynamic_context *_cxt_, 
                                                      operation_info _info_, 
                                                      PPOpIn _seq1_,
                                                      PPOpIn _seq2_); 

	static PPGeneralComparison* PPLEGeneralComparison(dynamic_context *_cxt_, 
                                                      operation_info _info_, 
                                                      PPOpIn _seq1_,
                                                      PPOpIn _seq2_); 

	static PPGeneralComparison* PPNEGeneralComparison(dynamic_context *_cxt_, 
                                                      operation_info _info_, 
                                                      PPOpIn _seq1_,
                                                      PPOpIn _seq2_); 

	static PPGeneralComparison* PPEQGeneralComparison(dynamic_context *_cxt_, 
                                                      operation_info _info_, 
                                                      PPOpIn _seq1_,
                                                      PPOpIn _seq2_);
};

///////////////////////////////////////////////////////////////////////////////
/// PPLMGeneralComparison
///////////////////////////////////////////////////////////////////////////////
class PPLMGeneralComparison : public PPGeneralComparison
{
protected:
	bool more;
	bool strict;
	bool min_ut_num;
	tuple_cell min_ut_num_cell;
	bool min_ut_dat;
	tuple_cell min_ut_dat_cell;

	bool min_num;
	tuple_cell min_num_cell;
	bool min_dat;
	tuple_cell min_dat_cell;
	bool min_str;
	tuple_cell min_str_cell;

	bool max_ut_num;
	tuple_cell max_ut_num_cell;
	bool max_ut_dat;
	tuple_cell max_ut_dat_cell;

	bool max_num;
	tuple_cell max_num_cell;
	bool max_dat;
	tuple_cell max_dat_cell;
	bool max_str;
	tuple_cell max_str_cell;

	bool min_dat_cast;
	bool min_num_cast;
	bool max_dat_cast;
	bool max_num_cast;

	xmlscm_type fill_minimums(tuple_cell value);
	xmlscm_type fill_maximums(tuple_cell value);
	bool compare_minmax(xmlscm_type type_info,bool min_changed);
	bool compare_minmax_le(xmlscm_type type_info,bool min_changed);
public:
	PPLMGeneralComparison(dynamic_context *_cxt_, 
                          operation_info _info_, 
                          PPOpIn _seq1_,
                          PPOpIn _seq2_,
                          bool _more_);

	PPLMGeneralComparison(dynamic_context *_cxt_,
                          operation_info _info_,
                          PPOpIn _seq1_,
                          PPOpIn _seq2_,
                          bool _more_,
                          bool strict);

    inline const char* get_operation_comparison_type() 
    { 
        if(more && strict) return "gt";
        else if(more) return "ge";
        else if(strict) return "lt";
        return "le";
    }

private:
    virtual void do_next (tuple &t);
    virtual void do_accept (PPVisitor &v);
	virtual PPIterator* do_copy(dynamic_context *_cxt_);
};

///////////////////////////////////////////////////////////////////////////////
/// PPNEQGeneralComparison
///////////////////////////////////////////////////////////////////////////////
class PPNEQGeneralComparison : public PPGeneralComparison
{
public:
	PPNEQGeneralComparison(dynamic_context *_cxt_, 
                           operation_info _info_, 
                           PPOpIn _seq1_,
                           PPOpIn _seq2_); 

private:
    virtual void do_next (tuple &t);
    virtual void do_accept (PPVisitor &v);
	virtual PPIterator* do_copy(dynamic_context *_cxt_);
};

///////////////////////////////////////////////////////////////////////////////
/// PPEQLGeneralComparison
///////////////////////////////////////////////////////////////////////////////
class PPEQLGeneralComparison : public PPGeneralComparison
{
public:
	PPEQLGeneralComparison(dynamic_context *_cxt_,
                           operation_info _info_,     
                           PPOpIn _seq1_,
                           PPOpIn _seq2_);
private:
    virtual void do_next (tuple &t);
    virtual void do_accept (PPVisitor &v);
	virtual PPIterator* do_copy(dynamic_context *_cxt_);
};

#endif
