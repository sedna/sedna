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
	if (!(tup.cells_number==1 )) throw XQUERY_EXCEPTION2(XPTY0004, "Name argument of Constructor is not a single atomic value");
	return atomize(tup.cells[0]);
}
///////////////////////////////////////////////////////////////////////////////
/// PPGeneralComparison
///////////////////////////////////////////////////////////////////////////////
class PPGeneralComparison : public PPIterator
{
protected:
    // obtained parameters and local data
    PPOpIn seq1;
	PPOpIn seq2;
    bool first_time;
    bool eos_reached1;
	bool eos_reached2;
    CollationHandler *handler;
    void children(PPOpIn &_seq1_,PPOpIn &_seq2_) { _seq1_ = seq1; _seq2_ = seq2;}

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);
	virtual void generalNodePrepare(tuple_cell& cell1, tuple_cell& cell2);
    PPGeneralComparison(dynamic_context *_cxt_, 
            PPOpIn _seq1_, PPOpIn _seq2_);
    virtual ~PPGeneralComparison();
	////////////////////////////////////////////////////////////////////////////
    /// FACTORIES FOR General Comparisons
    ////////////////////////////////////////////////////////////////////////////
    static PPGeneralComparison* PPGTGeneralComparison(dynamic_context *_cxt_, 
            PPOpIn _seq1_, PPOpIn _seq2_); 
	static PPGeneralComparison* PPLTGeneralComparison(dynamic_context *_cxt_, 
		PPOpIn _seq1_, PPOpIn _seq2_); 
	static PPGeneralComparison* PPGEGeneralComparison(dynamic_context *_cxt_, 
		PPOpIn _seq1_, PPOpIn _seq2_); 
	static PPGeneralComparison* PPLEGeneralComparison(dynamic_context *_cxt_, 
		PPOpIn _seq1_, PPOpIn _seq2_); 
	static PPGeneralComparison* PPNEGeneralComparison(dynamic_context *_cxt_, 
		PPOpIn _seq1_, PPOpIn _seq2_); 
	static PPGeneralComparison* PPEQGeneralComparison(dynamic_context *_cxt_, 
		PPOpIn _seq1_, PPOpIn _seq2_); 

};

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
            PPOpIn _seq1_, PPOpIn _seq2_, bool _more_);
		PPLMGeneralComparison(dynamic_context *_cxt_, 
            PPOpIn _seq1_, PPOpIn _seq2_, bool _more_,bool strict);
		virtual PPIterator* copy(dynamic_context *_cxt_);
		virtual void next   (tuple &t);
};
class PPNEQGeneralComparison : public PPGeneralComparison
{
public:
	virtual void next   (tuple &t);
	PPNEQGeneralComparison(dynamic_context *_cxt_, 
            PPOpIn _seq1_, PPOpIn _seq2_);
    virtual PPIterator* copy(dynamic_context *_cxt_);

};

class PPEQLGeneralComparison : public PPGeneralComparison
{
public:
	virtual void next   (tuple &t);
	PPEQLGeneralComparison(dynamic_context *_cxt_, 
            PPOpIn _seq1_, PPOpIn _seq2_);
	virtual PPIterator* copy(dynamic_context *_cxt_);

};

#endif
