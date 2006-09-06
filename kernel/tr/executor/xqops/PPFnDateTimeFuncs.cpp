/*
 * File:  PPDateTimeFuncs.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "PPFnDateTimeFuncs.h"
#include "casting_operations.h"
#include "PPUtils.h"
#include "e_string.h"
#include "strings.h"

///////////////////////////////////////////////////////////////////////////////
/// PPFnDateTimeFunc
///////////////////////////////////////////////////////////////////////////////
PPFnDateTimeFunc::PPFnDateTimeFunc(variable_context *_cxt_,
                                   PPOpIn _child_, int _dateTimeFunc_) : PPIterator(_cxt_),
                                                     child(_child_), dateTimeFunc(_dateTimeFunc_)
{
}

PPFnDateTimeFunc::~PPFnDateTimeFunc()
{
    delete child.op;
    child.op = NULL;
}

void PPFnDateTimeFunc::open  ()
{
    child.op->open();
    first_time = true;
}

void PPFnDateTimeFunc::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnDateTimeFunc::close ()
{
    child.op->close();
}

void PPFnDateTimeFunc::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);
        if (!t.is_eos())
        {
            tuple_cell tc = child.get(t);
            tc = atomize(tc);
 	    xmlscm_type tc_type = tc.get_atomic_type();

            child.op->next(t);
              if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Length of sequence passed to fn:dateTime function is more than 1");

	    switch (dateTimeFunc)
	    {
		case yearsFromDuration:
		case yearFromDateTime:
		case yearFromDate:
					if (tc_type != xs_duration &&
						tc_type != xs_yearMonthDuration &&
						tc_type != xs_dateTime &&
						tc_type != xs_date )
					throw USER_EXCEPTION2(XPTY0004, "Invalid type passed to fn:dateTime function");
						
					t.copy(tuple_cell::atomic((__int64)(tc.get_xs_dateTime().getYears())));
					break;
		case monthsFromDuration:
		case monthFromDateTime:
		case monthFromDate:
					if (tc_type != xs_duration &&
						tc_type != xs_yearMonthDuration &&
						tc_type != xs_dateTime &&
						tc_type != xs_date )
					throw USER_EXCEPTION2(XPTY0004, "Invalid type passed to fn:dateTime function");
					
					t.copy(tuple_cell::atomic((__int64)(tc.get_xs_dateTime().getMonths())));
					break;
		case daysFromDuration:
		case dayFromDateTime:
		case dayFromDate:
					if (tc_type != xs_duration &&
						tc_type != xs_dayTimeDuration &&
						tc_type != xs_dateTime &&
						tc_type != xs_date )
					throw USER_EXCEPTION2(XPTY0004, "Invalid type passed to fn:dateTime function");
					
					t.copy(tuple_cell::atomic((__int64)(tc.get_xs_dateTime().getDays())));
					break;
		case hoursFromDuration:
		case hoursFromDateTime:
		case hoursFromTime:
					if (tc_type != xs_duration &&
						tc_type != xs_dayTimeDuration &&
						tc_type != xs_dateTime &&
						tc_type != xs_time)
					throw USER_EXCEPTION2(XPTY0004, "Invalid type passed to fn:dateTime function");
					
					t.copy(tuple_cell::atomic((__int64)(tc.get_xs_dateTime().getHours())));
					break;
		case minutesFromDuration:
		case minutesFromDateTime:
		case minutesFromTime:
					if (tc_type != xs_duration &&
						tc_type != xs_dayTimeDuration &&
						tc_type != xs_dateTime &&
						tc_type != xs_time)
					throw USER_EXCEPTION2(XPTY0004, "Invalid type passed to fn:dateTime function");
					
					t.copy(tuple_cell::atomic((__int64)(tc.get_xs_dateTime().getMinutes())));
					break;
		case secondsFromDuration:
		case secondsFromDateTime:
		case secondsFromTime:
					if (tc_type != xs_duration &&
						tc_type != xs_dayTimeDuration &&
						tc_type != xs_dateTime &&
						tc_type != xs_time)
					throw USER_EXCEPTION2(XPTY0004, "Invalid type passed to fn:dateTime function");
					
					t.copy(tuple_cell::atomic( tc.get_xs_dateTime().getSeconds()));
					break;
		case timezoneFromDateTime:
		case timezoneFromDate:
		case timezoneFromTime:
					if (tc_type != xs_date &&
						tc_type != xs_dateTime &&
						tc_type != xs_time)
					throw USER_EXCEPTION2(XPTY0004, "Invalid type passed to fn:dateTime function");
					
					t.copy(tuple_cell::atomic(xs_dayTimeDuration,
							tc.get_xs_dateTime().getTimezone().getRawData()));
					break;
		case adjustDateTimeToTimezone:
		case adjustDateToTimezone:
		case adjustTimeToTimezone:
					if (tc_type != xs_date &&
						tc_type != xs_dateTime &&
						tc_type != xs_time)
					throw USER_EXCEPTION2(XPTY0004, "Invalid type passed to fn:dateTime function");
					
					t.copy(tuple_cell::atomic(tc.get_atomic_type(), 
							adjustToTimezone(tc.get_xs_dateTime()).getRawData()));
					break;
		default:		throw USER_EXCEPTION2(XPTY0004, "Invalid date/time function");
		}
         }		
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnDateTimeFunc::copy(variable_context *_cxt_)
{
    PPFnDateTimeFunc *res = new PPFnDateTimeFunc(_cxt_, child,dateTimeFunc);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnDateTimeFunc::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnDateTimeFunc::result");
}

///////////////////////////////////////////////////////////////////////////////
/// PPFnDateTimeFunc2Params
///////////////////////////////////////////////////////////////////////////////
PPFnDateTimeFunc2Params::PPFnDateTimeFunc2Params(variable_context *_cxt_,
                                   PPOpIn _child1_, PPOpIn _child2_, int _dateTimeFunc_) : PPIterator(_cxt_),
                                                     child1(_child1_), child2(_child2_),
							dateTimeFunc(_dateTimeFunc_)
{
}

PPFnDateTimeFunc2Params::~PPFnDateTimeFunc2Params()
{
    delete child1.op;
    child1.op = NULL;
    delete child2.op;
    child2.op = NULL;
}

void PPFnDateTimeFunc2Params::open  ()
{
    child1.op->open();
    child2.op->open();
    first_time = true;
}

void PPFnDateTimeFunc2Params::reopen()
{
    child1.op->reopen();
    child2.op->reopen();
    first_time = true;
}

void PPFnDateTimeFunc2Params::close ()
{
    child1.op->close();
    child2.op->close();
}

void PPFnDateTimeFunc2Params::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child1.op->next(t);
        if (!t.is_eos())
        {
            tuple_cell firstArg = child1.get(t);
            firstArg = atomize(firstArg);
 	    xmlscm_type firstArgType = firstArg.get_atomic_type();

	    child2.op->next(t);
	    if (!t.is_eos())
	    {
	    	tuple_cell secondArg = child2.get(t);
		secondArg = atomize(secondArg);
		xmlscm_type secondArgType = secondArg.get_atomic_type();

                child2.op->next(t);
            	  if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Length of sequence passed to fn:dateTime function is more than 1");

	    	switch (dateTimeFunc)
	    	{
			case adjustDateTimeToTimezone:
			case adjustDateToTimezone:
			case adjustTimeToTimezone:
					if (firstArgType != xs_date &&
						firstArgType != xs_dateTime &&
						firstArgType != xs_time)
					throw USER_EXCEPTION2(XPTY0004, "Invalid type passed as first argument to fn:dateTime function");

					if (secondArgType != xs_dayTimeDuration)
					throw USER_EXCEPTION2(XPTY0004, "Invalid type passed as second argument to fn:dateTime function");
					
					t.copy(tuple_cell::atomic(firstArgType,
							adjustToTimezone(firstArg.get_xs_dateTime(), 
								secondArg.get_xs_dateTime()).getRawData()));
					break;
		}
	    }
	    else
	    {	
            	child1.op->next(t);
            	  if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Length of sequence passed to fn:dateTime function is more than 1");

	    	switch (dateTimeFunc)
	    	{
			case adjustDateTimeToTimezone:
			case adjustDateToTimezone:
			case adjustTimeToTimezone:
					if (firstArgType != xs_date &&
						firstArgType != xs_dateTime &&
						firstArgType != xs_time)
					throw USER_EXCEPTION2(XPTY0004, "Invalid type passed to fn:dateTime function");

					t.copy(tuple_cell::atomic(firstArgType,
							adjustToTimezone(firstArg.get_xs_dateTime()).getRawData()));
					break;
		}
	    }

        }
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnDateTimeFunc2Params::copy(variable_context *_cxt_)
{
    PPFnDateTimeFunc2Params *res = new PPFnDateTimeFunc2Params(_cxt_, child1, child2,dateTimeFunc);
    res->child1.op = child1.op->copy(_cxt_);
    res->child2.op = child2.op->copy(_cxt_);

    return res;
}

bool PPFnDateTimeFunc2Params::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnDateTimeFunc::result");
}
