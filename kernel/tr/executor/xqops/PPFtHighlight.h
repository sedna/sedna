#ifndef _PPFTHIGHLIGHT_H
#define _PPFTHIGHLIGHT_H

#include "PPBase.h"
#include "FTsearch.h"

class PPFtHighlight : public PPIterator
{
protected:
    // given parameters
    PPOpIn query, seq;

    // obtained parameters and local data
	bool first_time;
	SednaSearchJob *sj;
	pers_sset<ft_custom_cell,unsigned short>* ptr;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);

    PPFtHighlight(variable_context *_cxt_,
                PPOpIn _seq_,
                PPOpIn _query_);

    virtual ~PPFtHighlight();

	static bool result(PPIterator* cur, variable_context *cxt, void*& r);
};


#endif