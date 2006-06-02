/*
 * File:  PPDateTimeFuncs.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPFNDATETIMEFUNCS_H
#define _PPFNDATETIMEFUNCS_H

#include "PPBase.h"


///////////////////////////////////////////////////////////////////////////////
/// PPFnDateTimeFunc1Param
///////////////////////////////////////////////////////////////////////////////
class PPFnDateTimeFunc: public PPIterator
{
protected:
    // obtained parameters and local data
    PPOpIn child;
    bool first_time;
    int dateTimeFunc;

    void children(PPOpIn &_child_) { _child_ = child; }

public:

    enum dateTimeFuncs
    {
	yearsFromDuration=0,
	monthsFromDuration,
	daysFromDuration,
	hoursFromDuration,
	minutesFromDuration,
	secondsFromDuration,
	yearFromDateTime,
	monthFromDateTime,
	dayFromDateTime,
	hoursFromDateTime,
	minutesFromDateTime,
	secondsFromDateTime,
	timezoneFromDateTime,
	yearFromDate,
	monthFromDate,
	dayFromDate,
	timezoneFromDate,
	hoursFromTime,
	minutesFromTime,
	secondsFromTime,
	timezoneFromTime,
	adjustDateTimeToTimezone,
	adjustDateToTimezone,
	adjustTimeToTimezone
    };
   
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnDateTimeFunc(variable_context *_cxt_, 
                     PPOpIn _child_, int dateTimeFunc);
    virtual ~PPFnDateTimeFunc();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnDateTimeFunc2Params
///////////////////////////////////////////////////////////////////////////////
class PPFnDateTimeFunc2Params: public PPIterator
{
protected:
    // obtained parameters and local data
    PPOpIn child1;
    PPOpIn child2;
    bool first_time;
    int dateTimeFunc;

    void children(PPOpIn &_child1_, PPOpIn &_child2_) { _child1_ = child1; 
							_child2_ = child2;}

public:

    enum dateTimeFuncs
    {
	adjustDateTimeToTimezone=0,
	adjustDateToTimezone,
	adjustTimeToTimezone
    };
   
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnDateTimeFunc2Params(variable_context *_cxt_, 
                     PPOpIn _child1_, PPOpIn _child2_, int dateTimeFunc);
    virtual ~PPFnDateTimeFunc2Params();
};

#endif
