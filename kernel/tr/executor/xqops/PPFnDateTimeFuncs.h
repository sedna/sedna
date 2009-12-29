/*
 * File:  PPDateTimeFuncs.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPFNDATETIMEFUNCS_H
#define _PPFNDATETIMEFUNCS_H

#include "tr/executor/base/PPBase.h"

///////////////////////////////////////////////////////////////////////////////
/// PPFnDateTimeFuncNoParam
///////////////////////////////////////////////////////////////////////////////
class PPFnDateTimeFuncNoParam: public PPIterator
{
protected:
    bool first_time;
    int dateTimeFunc;

public:
    enum dateTimeFuncs
    {
	currentDateTime=0,
	currentDate,
	currentTime,
	implicitTimezone
    };

private:   
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnDateTimeFuncNoParam(dynamic_context *_cxt_,
                            operation_info _info_,
                            int dateTimeFunc);
    virtual ~PPFnDateTimeFuncNoParam();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnDateTimeFunc1Param
///////////////////////////////////////////////////////////////////////////////
class PPFnDateTimeFunc: public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;
    int dateTimeFunc;
    xmlscm_type expected_type;

public:

    enum dateTimeFuncs
    { yearsFromDuration=0,
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

private:   
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnDateTimeFunc(dynamic_context *_cxt_, 
                     operation_info _info_,
                     PPOpIn _child_, 
                     int dateTimeFunc,
                     xmlscm_type _expected_type_);
    virtual ~PPFnDateTimeFunc();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnDateTimeFunc2Params
///////////////////////////////////////////////////////////////////////////////
class PPFnDateTimeFunc2Params: public PPIterator
{
protected:
    PPOpIn child1;
    PPOpIn child2;
    bool first_time;
    int dateTimeFunc;

public:
    enum dateTimeFuncs
    {
	adjustDateTimeToTimezone=0,
	adjustDateToTimezone,
	adjustTimeToTimezone,
	dateTime
    };
   
private:   
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnDateTimeFunc2Params(dynamic_context *_cxt_,
                            operation_info _info_,
                            PPOpIn _child1_,
                            PPOpIn _child2_,
                            int dateTimeFunc);

    virtual ~PPFnDateTimeFunc2Params();
};

#endif
