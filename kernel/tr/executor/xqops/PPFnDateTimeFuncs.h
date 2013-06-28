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
public:
    enum dateTimeFuncs
    {
	currentDateTime=0,
	currentDate,
	currentTime,
	implicitTimezone
    };
    static const char* dateTimeFunc2string(dateTimeFuncs f);

protected:
    bool first_time;
    dateTimeFuncs dateTimeFunc;
    
private:   
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnDateTimeFuncNoParam(dynamic_context *_cxt_,
                            operation_info _info_,
                            dateTimeFuncs dateTimeFunc);
    virtual ~PPFnDateTimeFuncNoParam();
    inline dateTimeFuncs get_function_type() { return dateTimeFunc; }
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnDateTimeFunc1Param
///////////////////////////////////////////////////////////////////////////////
class PPFnDateTimeFunc: public PPIterator
{
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
    static const char* dateTimeFunc2string(dateTimeFuncs f);

protected:
    PPOpIn child;
    bool first_time;
    dateTimeFuncs dateTimeFunc;
    xmlscm_type expected_type;

private:   
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnDateTimeFunc(dynamic_context *_cxt_, 
                     operation_info _info_,
                     PPOpIn _child_, 
                     dateTimeFuncs dateTimeFunc,
                     xmlscm_type _expected_type_);
    virtual ~PPFnDateTimeFunc();
    inline dateTimeFuncs get_function_type() { return dateTimeFunc; }
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnDateTimeFunc2Params
///////////////////////////////////////////////////////////////////////////////
class PPFnDateTimeFunc2Params: public PPIterator
{
public:
    enum dateTimeFuncs
    {
	adjustDateTimeToTimezone=0,
	adjustDateToTimezone,
	adjustTimeToTimezone,
	dateTime
    };
    static const char* dateTimeFunc2string(dateTimeFuncs f);

protected:
    PPOpIn child1;
    PPOpIn child2;
    bool first_time;
    dateTimeFuncs dateTimeFunc;

       
private:   
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:    
    PPFnDateTimeFunc2Params(dynamic_context *_cxt_,
                            operation_info _info_,
                            PPOpIn _child1_,
                            PPOpIn _child2_,
                            dateTimeFuncs dateTimeFunc);

    virtual ~PPFnDateTimeFunc2Params();
    inline dateTimeFuncs get_function_type() { return dateTimeFunc; }
};

#endif
