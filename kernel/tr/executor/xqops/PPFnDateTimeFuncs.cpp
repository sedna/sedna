/*
* File:  PPDateTimeFuncs.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include <string>

#include "tr/executor/xqops/PPFnDateTimeFuncs.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/strings/e_string.h"
#include "tr/strings/strings.h"

using namespace std;

///////////////////////////////////////////////////////////////////////////////
/// PPFnDateTimeFuncNoParam
///////////////////////////////////////////////////////////////////////////////
PPFnDateTimeFuncNoParam::PPFnDateTimeFuncNoParam(dynamic_context *_cxt_,
                                                 operation_info _info_,
                                                 dateTimeFuncs _dateTimeFunc_) : PPIterator(_cxt_, _info_, "PPFnDateTimeFuncNoParam"),
                                                 dateTimeFunc(_dateTimeFunc_)
{
}

PPFnDateTimeFuncNoParam::~PPFnDateTimeFuncNoParam()
{
}

void PPFnDateTimeFuncNoParam::do_open ()
{
    first_time = true;
}

void PPFnDateTimeFuncNoParam::do_reopen()
{
    first_time = true;
}

void PPFnDateTimeFuncNoParam::do_close()
{
}

void PPFnDateTimeFuncNoParam::do_next (xqp_tuple &t)
{
    if (first_time)
    {
        first_time = false;

        if (!cxt->is_datetime_inited())
            cxt->set_datetime();

        switch (dateTimeFunc)
        {
        case currentDateTime:   t.copy(tuple_cell::atomic(cxt->get_datetime().getPackedDateTime(), xs_dateTime)); break;
        case currentDate:       t.copy(tuple_cell::atomic(cxt->get_date().getPackedDateTime(), xs_date)); break;
        case currentTime:       t.copy(tuple_cell::atomic(cxt->get_time().getPackedDateTime(), xs_time)); break;
        case implicitTimezone:  t.copy(tuple_cell::atomic(cxt->get_timezone().getPackedDuration(), xs_dayTimeDuration)); break;
        default:                throw XQUERY_EXCEPTION2(SE1003, "Impossible parameterless date/time function");        
        }
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

const char* 
PPFnDateTimeFuncNoParam::dateTimeFunc2string(dateTimeFuncs type)
{
    switch(type)
    {
    case currentDateTime: return "fn:current-dateTime";
    case currentDate: return "fn:current-date";
    case currentTime: return "fn:current-time";
    case implicitTimezone: return "fn:implicit-timezone";
    default: throw USER_EXCEPTION2(SE1003, "Impossible case in parameterless date time function type conversion to string");
    }
}

PPIterator* PPFnDateTimeFuncNoParam::do_copy(dynamic_context *_cxt_)
{
    PPFnDateTimeFuncNoParam *res = se_new PPFnDateTimeFuncNoParam(_cxt_, info, dateTimeFunc);
    return res;
}

void PPFnDateTimeFuncNoParam::do_accept(PPVisitor &v)
{
    v.visit (this);
}


///////////////////////////////////////////////////////////////////////////////
/// PPFnDateTimeFunc
///////////////////////////////////////////////////////////////////////////////
PPFnDateTimeFunc::PPFnDateTimeFunc(dynamic_context *_cxt_,
                                   operation_info _info_,
                                   PPOpIn _child_, 
                                   dateTimeFuncs _dateTimeFunc_,
                                   xmlscm_type _expected_type_) : PPIterator(_cxt_, _info_, "PPFnDateTimeFunc"),
                                                                  child(_child_), 
                                                                  dateTimeFunc(_dateTimeFunc_),
                                                                  expected_type(_expected_type_)
{
}

PPFnDateTimeFunc::~PPFnDateTimeFunc()
{
    delete child.op;
    child.op = NULL;
}

void PPFnDateTimeFunc::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnDateTimeFunc::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnDateTimeFunc::do_close()
{
    child.op->close();
}

void PPFnDateTimeFunc::do_next (xqp_tuple &t)
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

            ///Each item in the atomic sequence that is of type xs:untypedAtomic is cast to the expected atomic type.
            if(tc_type == xs_untypedAtomic) { tc = cast(tc, expected_type); tc_type = expected_type; }

            child.op->next(t);
            if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, (string("Length of sequence passed to ")  + dateTimeFunc2string(dateTimeFunc) + string(" function is more than 1")).c_str());

            switch (dateTimeFunc)
            {
            case yearsFromDuration:
                if (tc_type != xs_duration &&
                    tc_type != xs_yearMonthDuration )
                    throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type passed to ") + dateTimeFunc2string(dateTimeFunc) + string(" function")).c_str());

                t.copy(tuple_cell::atomic((int64_t)(XMLDateTime(tc.get_xs_duration(), tc_type).getYears())));
                break;
            case yearFromDateTime:
            case yearFromDate:
                if (tc_type != xs_dateTime &&
                    tc_type != xs_date )
                    throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type passed to ") + dateTimeFunc2string(dateTimeFunc) + string(" function")).c_str());

                t.copy(tuple_cell::atomic((int64_t)(XMLDateTime(tc.get_xs_dateTime(), tc_type).getYears())));
                break;
            case monthsFromDuration:
                if (tc_type != xs_duration &&
                    tc_type != xs_yearMonthDuration )
                    throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type passed to ") + dateTimeFunc2string(dateTimeFunc) + string(" function")).c_str());

                t.copy(tuple_cell::atomic((int64_t)(XMLDateTime(tc.get_xs_duration(), tc_type).getMonths())));
                break;
            case monthFromDateTime:
            case monthFromDate:
                if (tc_type != xs_dateTime &&
                    tc_type != xs_date )
                    throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type passed to ") + dateTimeFunc2string(dateTimeFunc) + string(" function")).c_str());

                t.copy(tuple_cell::atomic((int64_t)(XMLDateTime(tc.get_xs_dateTime(), tc_type).getMonths())));
                break;
            case daysFromDuration:
                if (tc_type != xs_duration &&
                    tc_type != xs_dayTimeDuration)
                    throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type passed to ") + dateTimeFunc2string(dateTimeFunc) + string(" function")).c_str());

                t.copy(tuple_cell::atomic((int64_t)(XMLDateTime(tc.get_xs_duration(), tc_type).getDays())));
                break;
            case dayFromDateTime:
            case dayFromDate:
                if (tc_type != xs_dateTime &&
                    tc_type != xs_date)
                    throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type passed to ") + dateTimeFunc2string(dateTimeFunc) + string(" function")).c_str());

                t.copy(tuple_cell::atomic((int64_t)(XMLDateTime(tc.get_xs_dateTime(), tc_type).getDays())));
                break;
            case hoursFromDuration:
                if (tc_type != xs_duration &&
                    tc_type != xs_dayTimeDuration)
                    throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type passed to ") + dateTimeFunc2string(dateTimeFunc) + string(" function")).c_str());

                t.copy(tuple_cell::atomic((int64_t)(XMLDateTime(tc.get_xs_duration(), tc_type).getHours())));
                break;
            case hoursFromDateTime:
            case hoursFromTime:
                if (tc_type != xs_dateTime &&
                    tc_type != xs_time)
                    throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type passed to ") + dateTimeFunc2string(dateTimeFunc) + string(" function")).c_str());

                t.copy(tuple_cell::atomic((int64_t)(XMLDateTime(tc.get_xs_dateTime(), tc_type).getHours())));
                break;
            case minutesFromDuration:
                if (tc_type != xs_duration &&
                    tc_type != xs_dayTimeDuration)
                    throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type passed to ") + dateTimeFunc2string(dateTimeFunc) + string(" function")).c_str());

                t.copy(tuple_cell::atomic((int64_t)(XMLDateTime(tc.get_xs_duration(), tc_type).getMinutes())));
                break;
            case minutesFromDateTime:
            case minutesFromTime:
                if (tc_type != xs_dateTime &&
                    tc_type != xs_time)
                    throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type passed to ") + dateTimeFunc2string(dateTimeFunc) + string(" function")).c_str());

                t.copy(tuple_cell::atomic((int64_t)(XMLDateTime(tc.get_xs_dateTime(), tc_type).getMinutes())));
                break;
            case secondsFromDuration:
                {
                    if (tc_type != xs_duration &&
                        tc_type != xs_dayTimeDuration)
                        throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type passed to ") + dateTimeFunc2string(dateTimeFunc) + string(" function")).c_str());

                    XMLDateTime dur(tc.get_xs_duration(), tc_type);
                    double seconds = dur.getSeconds();

                    if (dur.getValue(XMLDateTime::MiliSecond) != 0)
                        t.copy(tuple_cell::atomic(seconds));
                    else
                        t.copy(tuple_cell::atomic((int64_t)seconds));

                    break;
                }
            case secondsFromDateTime:
            case secondsFromTime:
                {
                    if (tc_type != xs_dateTime &&
                        tc_type != xs_time)
                        throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type passed to ") + dateTimeFunc2string(dateTimeFunc) + string(" function")).c_str());

                    XMLDateTime dt(tc.get_xs_dateTime(), tc_type);
                    double seconds = dt.getSeconds();
                    if (dt.getValue(XMLDateTime::MiliSecond) != 0)
                        t.copy(tuple_cell::atomic(seconds));
                    else
                        t.copy(tuple_cell::atomic((int64_t)seconds));

                    break;
                }
            case timezoneFromDateTime:
            case timezoneFromDate:
            case timezoneFromTime:
                {
                    if (tc_type != xs_date &&
                        tc_type != xs_dateTime &&
                        tc_type != xs_time)
                        throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type passed to ") + dateTimeFunc2string(dateTimeFunc) + string(" function")).c_str());

                    XMLDateTime dt(tc.get_xs_dateTime(), tc_type);

                    if (dt.getValue(XMLDateTime::utc) != XMLDateTime::UTC_UNKNOWN)
                        t.copy(tuple_cell::atomic(dt.getTimezone().getPackedDuration(), xs_dayTimeDuration));
                    else
                        t.set_eos();

                    break;
                }
            case adjustDateTimeToTimezone:
            case adjustDateToTimezone:
            case adjustTimeToTimezone:
                // extract implicit timezone from static context and use it to adjust the datetime
                if (!cxt->is_datetime_inited())
                    cxt->set_datetime();

                if (tc_type != xs_date &&
                    tc_type != xs_dateTime &&
                    tc_type != xs_time)
                    throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type passed to ") + dateTimeFunc2string(dateTimeFunc) + string(" function")).c_str());

                t.copy(tuple_cell::atomic(adjustToTimezone(XMLDateTime(tc.get_xs_dateTime(), tc_type),
                    cxt->get_timezone()).getPackedDateTime(), tc_type));
                break;
            default: throw XQUERY_EXCEPTION2(SE1003, "Impossible date/time function");
            }
        }		
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

const char* 
PPFnDateTimeFunc::dateTimeFunc2string(dateTimeFuncs type)
{
    switch(type)
    {
    case yearsFromDuration: return "fn:years-from-duration";
    case yearFromDateTime: return "fn:year-from-dateTime";
    case yearFromDate: return "fn:year-from-date";
    case monthsFromDuration: return "fn:months-from-duration";
    case monthFromDateTime: return "fn:month-from-dateTime";
    case monthFromDate: return "fn:month-from-date";
    case daysFromDuration: return "fn:days-from-duration";
    case dayFromDateTime: return "fn:day-from-dateTime";
    case dayFromDate: return "fn:day-from-date";
    case hoursFromDuration: return "fn:hours-from-duration";
    case hoursFromDateTime: return "fn:hours-from-dateTime";
    case hoursFromTime: return "fn:hours-from-time";
    case minutesFromDuration: return "fn:minutes-from-duration";
    case minutesFromDateTime: return "fn:minutes-from-dateTime";
    case minutesFromTime: return "fn:minutes-from-time";
    case secondsFromDuration: return "fn:seconds-from-duration";
    case secondsFromDateTime: return "fn:seconds-from-dateTime";
    case secondsFromTime: return "fn:seconds-from-time";
    case timezoneFromDateTime: return "fn:timezone-from-dateTime";
    case timezoneFromDate: return "fn:timezone-from-date";
    case timezoneFromTime: return "fn:timezone-from-time";
    case adjustDateTimeToTimezone: return "fn:adjust-dateTime-to-timezone";
    case adjustDateToTimezone: return "fn:adjust-date-to-timezone";
    case adjustTimeToTimezone: return "fn:adjust-time-to-timezone";
    default: throw USER_EXCEPTION2(SE1003, "Impossible case in one-parameter date time function type conversion to string");
    }
}


PPIterator* PPFnDateTimeFunc::do_copy(dynamic_context *_cxt_)
{
    PPFnDateTimeFunc *res = se_new PPFnDateTimeFunc(_cxt_, info, child, dateTimeFunc, expected_type);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnDateTimeFunc::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
/// PPFnDateTimeFunc2Params
///////////////////////////////////////////////////////////////////////////////
PPFnDateTimeFunc2Params::PPFnDateTimeFunc2Params(dynamic_context *_cxt_,
                                                 operation_info _info_,
                                                 PPOpIn _child1_,
                                                 PPOpIn _child2_,
                                                 dateTimeFuncs _dateTimeFunc_) : PPIterator(_cxt_, _info_, "PPFnDateTimeFunc2Params"),
                                                 child1(_child1_),
                                                 child2(_child2_),
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

void PPFnDateTimeFunc2Params::do_open ()
{
    child1.op->open();
    child2.op->open();
    first_time = true;
}

void PPFnDateTimeFunc2Params::do_reopen()
{
    child1.op->reopen();
    child2.op->reopen();
    first_time = true;
}

void PPFnDateTimeFunc2Params::do_close()
{
    child1.op->close();
    child2.op->close();
}

void PPFnDateTimeFunc2Params::do_next (xqp_tuple &t)
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
                if (!(t.is_eos())) 
                    throw XQUERY_EXCEPTION2(XPTY0004, (string("Length of sequence passed as second argument to ")  + dateTimeFunc2string(dateTimeFunc) + string(" function is more than 1")).c_str());

                switch (dateTimeFunc)
                {
                case adjustDateTimeToTimezone:
                case adjustDateToTimezone:
                case adjustTimeToTimezone:
                    if (firstArgType != xs_date &&
                        firstArgType != xs_dateTime &&
                        firstArgType != xs_time)
                        throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type passed as first argument to ")  + dateTimeFunc2string(dateTimeFunc) + string(" function")).c_str());
                    if (secondArgType != xs_dayTimeDuration)
                        throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type passed as second argument to ")  + dateTimeFunc2string(dateTimeFunc) + string(" function")).c_str());

                    t.copy(tuple_cell::atomic(
                        adjustToTimezone( XMLDateTime(firstArg.get_xs_dateTime(), firstArgType), 
                        XMLDateTime(secondArg.get_xs_duration(), xs_dayTimeDuration)).getPackedDateTime(), firstArgType));
                    break;
                case dateTime:
                    if (firstArgType != xs_date)
                        throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type passed as first argument to ")  + dateTimeFunc2string(dateTimeFunc) + string(" function")).c_str());
                    if (secondArgType != xs_time)
                        throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type passed as second argument to ")  + dateTimeFunc2string(dateTimeFunc) + string(" function")).c_str());

                    t.copy(tuple_cell::atomic(
                        fnDateTime( XMLDateTime(firstArg.get_xs_dateTime(), firstArgType),
                        XMLDateTime(secondArg.get_xs_dateTime(), secondArgType)).getPackedDateTime(), xs_dateTime));
                    break;
                default: throw XQUERY_EXCEPTION2(SE1003, "Impossible two-parameters date/time function");
                }
            }
            else
            {	
                child1.op->next(t);
                if (!(t.is_eos()))
                    throw XQUERY_EXCEPTION2(XPTY0004, (string("Length of sequence passed as first argument to ")  + dateTimeFunc2string(dateTimeFunc) + string(" function is more than 1")).c_str());

                switch (dateTimeFunc)
                {
                case adjustDateTimeToTimezone:
                case adjustDateToTimezone:
                case adjustTimeToTimezone:
                    if (firstArgType != xs_date &&
                        firstArgType != xs_dateTime &&
                        firstArgType != xs_time)
                        throw XQUERY_EXCEPTION2(XPTY0004, (string("Invalid type passed to ") + dateTimeFunc2string(dateTimeFunc) + string(" function")).c_str());

                    t.copy(tuple_cell::atomic(adjustToTimezone(XMLDateTime(firstArg.get_xs_dateTime(), firstArgType)).getPackedDateTime(), firstArgType));
                    break;
                case dateTime: break; //The result is the empty sequence if either of the parameters is the empty sequence.
                default: throw XQUERY_EXCEPTION2(SE1003, "Impossible two-parameters date/time function");
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

const char* 
PPFnDateTimeFunc2Params::dateTimeFunc2string(dateTimeFuncs type)
{
    switch(type)
    {
    case adjustDateTimeToTimezone: return "fn:adjust-dateTime-to-timezone";
    case adjustDateToTimezone: return "fn:adjust-date-to-timezone";
    case adjustTimeToTimezone: return "fn:adjust-time-to-timezone";
    case dateTime: return "fn:dateTime";
    default: throw USER_EXCEPTION2(SE1003, "Impossible case in two-parameter date time function type conversion to string");
    }
}


PPIterator* PPFnDateTimeFunc2Params::do_copy(dynamic_context *_cxt_)
{
    PPFnDateTimeFunc2Params *res = se_new PPFnDateTimeFunc2Params(_cxt_, info, child1, child2, dateTimeFunc);
    res->child1.op = child1.op->copy(_cxt_);
    res->child2.op = child2.op->copy(_cxt_);
    return res;
}

void PPFnDateTimeFunc2Params::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child1.op->accept(v);
    child2.op->accept(v);
    v.pop();
}
