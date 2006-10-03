/*
 * File:  dateTime_operations.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "sedna.h"
#include "dateTime_operations.h"
#include "casting_operations.h"
#include "XMLDateTime.h"

tuple_cell op_add_yearMonthDuration_to_date(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_date ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_yearMonthDuration)
	throw USER_EXCEPTION2(SE1003, "Calling op:add-yearMonthDuration-to-date on invalid arguments");

    return tuple_cell::atomic(addDurationToDateTime(
				XMLDateTime(a1.get_xs_dateTime(), xs_date),
				XMLDateTime(a2.get_xs_duration(), xs_yearMonthDuration)).getPackedDateTime(), xs_date);
}

tuple_cell op_add_date_to_yearMonthDuration(const tuple_cell &a1, const tuple_cell &a2)
{
    return op_add_yearMonthDuration_to_date(a2,a1);
}

tuple_cell op_add_dayTimeDuration_to_date(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_date ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dayTimeDuration)
	throw USER_EXCEPTION2(SE1003, "Calling op:add-dayTimeDuration-to-date on invalid arguments");

    return tuple_cell::atomic(addDurationToDateTime(
				XMLDateTime(a1.get_xs_dateTime(), xs_date),
				XMLDateTime(a2.get_xs_duration(), xs_dayTimeDuration)).getPackedDateTime(), xs_date);
}

tuple_cell op_add_date_to_dayTimeDuration(const tuple_cell &a1, const tuple_cell &a2)
{
    return op_add_dayTimeDuration_to_time(a2,a1);
}

tuple_cell op_add_dayTimeDuration_to_time(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_time ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dayTimeDuration)
	throw USER_EXCEPTION2(SE1003, "Calling op:add-dayTimeDuration-to-time on invalid arguments");

    return tuple_cell::atomic(addDurationToDateTime(
				XMLDateTime(a1.get_xs_dateTime(), xs_time),
				XMLDateTime(a2.get_xs_duration(), xs_dayTimeDuration)).getPackedDateTime(), xs_time);
}

tuple_cell op_add_time_to_dayTimeDuration(const tuple_cell &a1, const tuple_cell &a2)
{
    return op_add_dayTimeDuration_to_time(a2,a1);
}

tuple_cell op_add_yearMonthDuration_to_dateTime(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dateTime ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_yearMonthDuration)
	throw USER_EXCEPTION2(SE1003, "Calling op:add-yearMonthDuration-to-dateTime on invalid arguments");

    return tuple_cell::atomic(addDurationToDateTime(
				XMLDateTime(a1.get_xs_dateTime(), xs_dateTime),
				XMLDateTime(a2.get_xs_duration(), xs_yearMonthDuration)).getPackedDateTime(), xs_dateTime);
}

tuple_cell op_add_dateTime_to_yearMonthDuration(const tuple_cell &a1, const tuple_cell &a2)
{
    return op_add_yearMonthDuration_to_dateTime(a2,a1);
}

tuple_cell op_add_dayTimeDuration_to_dateTime(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dateTime ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dayTimeDuration)
	throw USER_EXCEPTION2(SE1003, "Calling op:add-dayTimeDuration-to-time on invalid arguments");

    return tuple_cell::atomic(addDurationToDateTime(
				XMLDateTime(a1.get_xs_dateTime(), xs_dateTime),
				XMLDateTime(a2.get_xs_duration(), xs_dayTimeDuration)).getPackedDateTime(), xs_dateTime);
}

tuple_cell op_add_dateTime_to_dayTimeDuration(const tuple_cell &a1, const tuple_cell &a2)
{
    return op_add_dayTimeDuration_to_dateTime(a2,a1);
}

tuple_cell op_add_yearMonthDurations(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_yearMonthDuration ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_yearMonthDuration)
	throw USER_EXCEPTION2(SE1003, "Calling op:add-yearMonthDurations on invalid arguments");

    return tuple_cell::atomic(addDurations(
				XMLDateTime(a1.get_xs_duration(), xs_yearMonthDuration),
				XMLDateTime(a2.get_xs_duration(), xs_yearMonthDuration)).getPackedDuration(), xs_yearMonthDuration);
}

tuple_cell op_add_dayTimeDurations(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dayTimeDuration ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dayTimeDuration)
	throw USER_EXCEPTION2(SE1003, "Calling op:add-dayTimeDurations on invalid arguments");

    return tuple_cell::atomic(addDurations(
				XMLDateTime(a1.get_xs_duration(), xs_dayTimeDuration),
				XMLDateTime(a2.get_xs_duration(), xs_dayTimeDuration)).getPackedDuration(), xs_dayTimeDuration);
}

tuple_cell op_subtract_dates(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_date ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_date )
	throw USER_EXCEPTION2(SE1003, "Calling op:subtract-dates on invalid arguments");

    return tuple_cell::atomic(subtractDateTimes(
				XMLDateTime(a1.get_xs_dateTime(), xs_date),
				XMLDateTime(a2.get_xs_dateTime(), xs_date)).getPackedDuration(), xs_dayTimeDuration);
}

tuple_cell op_subtract_yearMonthDuration_from_date(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_date ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_yearMonthDuration)
	throw USER_EXCEPTION2(SE1003, "Calling op:subtract-yearMonthDuration-from-date on invalid arguments");

    return tuple_cell::atomic(subtractDurationFromDateTime(
				XMLDateTime(a1.get_xs_dateTime(), xs_date),
				XMLDateTime(a2.get_xs_duration(), xs_yearMonthDuration)).getPackedDateTime(), xs_date);
}

tuple_cell op_subtract_dayTimeDuration_from_date(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_date ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dayTimeDuration)
	throw USER_EXCEPTION2(SE1003, "Calling op:subtract-dayTimeDuration-from-date on invalid arguments");

    return tuple_cell::atomic(subtractDurationFromDateTime(
				XMLDateTime(a1.get_xs_dateTime(), xs_date),
				XMLDateTime(a2.get_xs_duration(), xs_dayTimeDuration)).getPackedDateTime(), xs_date);
}

tuple_cell op_subtract_times(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_time ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_time )
	throw USER_EXCEPTION2(SE1003, "Calling op:subtract-times on invalid arguments");

    return tuple_cell::atomic(subtractDateTimes(
				XMLDateTime(a1.get_xs_dateTime(), xs_time),
				XMLDateTime(a2.get_xs_dateTime(), xs_time)).getPackedDuration(), xs_dayTimeDuration);
}

tuple_cell op_subtract_dayTimeDuration_from_time(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_time ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dayTimeDuration)
	throw USER_EXCEPTION2(SE1003, "Calling op:subtract-dayTimeDuration-from-time on invalid arguments");

    return tuple_cell::atomic(subtractDurationFromDateTime(
				XMLDateTime(a1.get_xs_dateTime(), xs_time),
				XMLDateTime(a2.get_xs_duration(), xs_dayTimeDuration)).getPackedDateTime(), xs_time);
}

tuple_cell op_subtract_dateTimes(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dateTime ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dateTime )
	throw USER_EXCEPTION2(SE1003, "Calling op:subtract-dateTimes on invalid arguments");

    return tuple_cell::atomic(subtractDateTimes(
				XMLDateTime(a1.get_xs_dateTime(), xs_dateTime),
				XMLDateTime(a2.get_xs_dateTime(), xs_dateTime)).getPackedDuration(), xs_dayTimeDuration);
}

tuple_cell op_subtract_yearMonthDuration_from_dateTime(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dateTime ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_yearMonthDuration )
	throw USER_EXCEPTION2(SE1003, "Calling op:subtract-yearMonthDuration-from-dateTime on invalid arguments");

    return tuple_cell::atomic(subtractDurationFromDateTime(
				XMLDateTime(a1.get_xs_dateTime(), xs_dateTime),
				XMLDateTime(a2.get_xs_duration(), xs_yearMonthDuration)).getPackedDateTime(), xs_dateTime);
}

tuple_cell op_subtract_dayTimeDuration_from_dateTime(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dateTime ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dayTimeDuration )
	throw USER_EXCEPTION2(SE1003, "Calling op:subtract-dayTimeDuration-from-dateTime on invalid arguments");

    return tuple_cell::atomic(subtractDurationFromDateTime(
				XMLDateTime(a1.get_xs_dateTime(), xs_dateTime),
				XMLDateTime(a2.get_xs_duration(), xs_dayTimeDuration)).getPackedDateTime(), xs_dateTime);
}

tuple_cell op_subtract_yearMonthDurations(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_yearMonthDuration ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_yearMonthDuration )
	throw USER_EXCEPTION2(SE1003, "Calling op:subtract-yearMonthDurations on invalid arguments");

    return tuple_cell::atomic(subtractDurations(
				XMLDateTime(a1.get_xs_duration(), xs_yearMonthDuration),
				XMLDateTime(a2.get_xs_duration(), xs_yearMonthDuration)).getPackedDuration(), xs_yearMonthDuration);
}

tuple_cell op_subtract_dayTimeDurations(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dayTimeDuration ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dayTimeDuration )
	throw USER_EXCEPTION2(SE1003, "Calling op:subtract-dayTimeDurations on invalid arguments");

    return tuple_cell::atomic(subtractDurations(
				XMLDateTime(a1.get_xs_duration(), xs_dayTimeDuration),
				XMLDateTime(a2.get_xs_duration(), xs_dayTimeDuration)).getPackedDuration(), xs_dayTimeDuration);
}
tuple_cell op_multiply_yearMonthDuration_by_numeric(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_yearMonthDuration ||
	!a2.is_atomic() || !a2.is_numeric_type())
	throw USER_EXCEPTION2(SE1003, "Calling op:multiply-yearMonthDuration-by-numeric on invalid arguments");

    return tuple_cell::atomic(multiplyDuration(
				XMLDateTime(a1.get_xs_duration(), xs_yearMonthDuration),
				cast(a2, xs_double).get_xs_double()).getPackedDuration(), xs_yearMonthDuration);
}

tuple_cell op_multiply_numeric_by_yearMonthDuration(const tuple_cell &a1, const tuple_cell &a2)
{
	return op_multiply_yearMonthDuration_by_numeric(a2,a1);
}

tuple_cell op_multiply_dayTimeDuration_by_numeric(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dayTimeDuration ||
	!a2.is_atomic() || !a2.is_numeric_type())
	throw USER_EXCEPTION2(SE1003, "Calling op:multiply-dayTimeDuration-by-numeric on invalid arguments");

    return tuple_cell::atomic(multiplyDuration(
				XMLDateTime(a1.get_xs_duration(), xs_dayTimeDuration),
				cast(a2, xs_double).get_xs_double()).getPackedDuration(), xs_dayTimeDuration);
}

tuple_cell op_multiply_numeric_by_dayTimeDuration(const tuple_cell &a1, const tuple_cell &a2)
{
	return op_multiply_dayTimeDuration_by_numeric(a2,a1);
}

tuple_cell op_divide_yearMonthDuration_by_numeric(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_yearMonthDuration ||
	!a2.is_atomic() || !a2.is_numeric_type())
	throw USER_EXCEPTION2(SE1003, "Calling op:divide-yearMonthDuration-by-numeric on invalid arguments");

    return tuple_cell::atomic(divideDuration(
				XMLDateTime(a1.get_xs_duration(), xs_yearMonthDuration),
				cast(a2, xs_double).get_xs_double()).getPackedDuration(), xs_yearMonthDuration);
}

tuple_cell op_divide_dayTimeDuration_by_numeric(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dayTimeDuration ||
	!a2.is_atomic() || !a2.is_numeric_type())
	throw USER_EXCEPTION2(SE1003, "Calling op:divide-dayTimeDuration-by-numeric on invalid arguments");

    return tuple_cell::atomic(divideDuration(
				XMLDateTime(a1.get_xs_duration(), xs_dayTimeDuration),
				cast(a2, xs_double).get_xs_double()).getPackedDuration(), xs_dayTimeDuration);
}

tuple_cell op_divide_yearMonthDuration_by_yearMonthDuration(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_yearMonthDuration ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_yearMonthDuration )
	throw USER_EXCEPTION2(SE1003, "Calling op:divide-yearMonthDuration-by-yearMonthDuration on invalid arguments");

    return tuple_cell::atomic(divideDurationByDuration(
				XMLDateTime(a1.get_xs_duration(), xs_yearMonthDuration),
				XMLDateTime(a2.get_xs_duration(), xs_yearMonthDuration)));
}

tuple_cell op_divide_dayTimeDuration_by_dayTimeDuration(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dayTimeDuration ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dayTimeDuration )
	throw USER_EXCEPTION2(SE1003, "Calling op:divide-dayTimeDuration-by-dayTimeDuration on invalid arguments");

    return tuple_cell::atomic(divideDurationByDuration(
				XMLDateTime(a1.get_xs_duration(), xs_dayTimeDuration),
				XMLDateTime(a2.get_xs_duration(), xs_dayTimeDuration)));
}

//================================================================================================================

tuple_cell op_date_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_date ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_date )
	throw USER_EXCEPTION2(SE1003, "Calling op:date-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_date),
				XMLDateTime(a2.get_xs_dateTime(), xs_date)));
}

tuple_cell op_time_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_time||
	!a2.is_atomic() || a2.get_atomic_type() != xs_time)
	throw USER_EXCEPTION2(SE1003, "Calling op:time-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_time),
				XMLDateTime(a2.get_xs_dateTime(), xs_time)));
}

tuple_cell op_dateTime_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dateTime||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dateTime)
	throw USER_EXCEPTION2(SE1003, "Calling op:dateTime-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_dateTime),
				XMLDateTime(a2.get_xs_dateTime(), xs_dateTime)));
}

tuple_cell op_yearMonthDuration_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_yearMonthDuration ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_yearMonthDuration )
	throw USER_EXCEPTION2(SE1003, "Calling op:yearMonthDuration-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_equal(
				XMLDateTime(a1.get_xs_duration(), xs_yearMonthDuration),
				XMLDateTime(a2.get_xs_duration(), xs_yearMonthDuration)));
}

tuple_cell op_dayTimeDuration_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dayTimeDuration ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dayTimeDuration )
	throw USER_EXCEPTION2(SE1003, "Calling op:dayTimeDuration-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_equal(
				XMLDateTime(a1.get_xs_duration(), xs_dayTimeDuration),
				XMLDateTime(a2.get_xs_duration(), xs_dayTimeDuration)));
}

tuple_cell op_duration_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || (a1.get_atomic_type() != xs_duration && a1.get_atomic_type() != xs_dayTimeDuration && a1.get_atomic_type() != xs_yearMonthDuration ) ||
	!a2.is_atomic() || (a2.get_atomic_type() != xs_duration && a1.get_atomic_type() != xs_dayTimeDuration && a1.get_atomic_type() != xs_yearMonthDuration ))
	throw USER_EXCEPTION2(SE1003, "Calling op:duration-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_equal(
				XMLDateTime(a1.get_xs_duration(), xs_duration),
				XMLDateTime(a2.get_xs_duration(), xs_duration)));
}

tuple_cell op_gYear_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_gYear ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_gYear )
	throw USER_EXCEPTION2(SE1003, "Calling op:gYear-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_gYear),
				XMLDateTime(a2.get_xs_dateTime(), xs_gYear)));
}

tuple_cell op_gYearMonth_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_gYearMonth ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_gYearMonth )
	throw USER_EXCEPTION2(SE1003, "Calling op:gYearMonth-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_gYearMonth),
				XMLDateTime(a2.get_xs_dateTime(), xs_gYearMonth)));
}

tuple_cell op_gMonth_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_gMonth ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_gMonth )
	throw USER_EXCEPTION2(SE1003, "Calling op:gMonth-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_gMonth),
				XMLDateTime(a2.get_xs_dateTime(), xs_gMonth)));
}

tuple_cell op_gMonthDay_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_gMonthDay ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_gMonthDay )
	throw USER_EXCEPTION2(SE1003, "Calling op:gMonthDay-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_gMonthDay),
				XMLDateTime(a2.get_xs_dateTime(), xs_gMonthDay)));
}

tuple_cell op_gDay_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_gDay ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_gDay )
	throw USER_EXCEPTION2(SE1003, "Calling op:gDay-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_gDay),
				XMLDateTime(a2.get_xs_dateTime(), xs_gDay)));
}

//==============================================================================================

tuple_cell op_date_not_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_date ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_date )
	throw USER_EXCEPTION2(SE1003, "Calling op:date-not-equal on invalid arguments");

    return tuple_cell::atomic(!xs_dateTime_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_date),
				XMLDateTime(a2.get_xs_dateTime(), xs_date)));
}

tuple_cell op_time_not_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_time||
	!a2.is_atomic() || a2.get_atomic_type() != xs_time)
	throw USER_EXCEPTION2(SE1003, "Calling op:time-not-equal on invalid arguments");

    return tuple_cell::atomic(!xs_dateTime_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_time),
				XMLDateTime(a2.get_xs_dateTime(), xs_time)));
}

tuple_cell op_dateTime_not_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dateTime||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dateTime)
	throw USER_EXCEPTION2(SE1003, "Calling op:dateTime-not-equal on invalid arguments");

    return tuple_cell::atomic(!xs_dateTime_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_dateTime),
				XMLDateTime(a2.get_xs_dateTime(), xs_dateTime)));
}

tuple_cell op_yearMonthDuration_not_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_yearMonthDuration ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_yearMonthDuration )
	throw USER_EXCEPTION2(SE1003, "Calling op:yearMonthDuration-not-equal on invalid arguments");

    return tuple_cell::atomic(!xs_dateTime_equal(
				XMLDateTime(a1.get_xs_duration(), xs_yearMonthDuration),
				XMLDateTime(a2.get_xs_duration(), xs_yearMonthDuration)));
}

tuple_cell op_dayTimeDuration_not_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dayTimeDuration ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dayTimeDuration )
	throw USER_EXCEPTION2(SE1003, "Calling op:dayTimeDuration-not-equal on invalid arguments");

    return tuple_cell::atomic(!xs_dateTime_equal(
				XMLDateTime(a1.get_xs_duration(), xs_dayTimeDuration),
				XMLDateTime(a2.get_xs_duration(), xs_dayTimeDuration)));
}

tuple_cell op_duration_not_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || (a1.get_atomic_type() != xs_duration && a1.get_atomic_type() != xs_dayTimeDuration && a1.get_atomic_type() != xs_yearMonthDuration ) ||
	!a2.is_atomic() || (a2.get_atomic_type() != xs_duration && a1.get_atomic_type() != xs_dayTimeDuration && a1.get_atomic_type() != xs_yearMonthDuration ))
	throw USER_EXCEPTION2(SE1003, "Calling op:duration-not-equal on invalid arguments");

    return tuple_cell::atomic(!xs_dateTime_equal(
				XMLDateTime(a1.get_xs_duration(), xs_duration),
				XMLDateTime(a2.get_xs_duration(), xs_duration)));
}

tuple_cell op_gYear_not_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_gYear ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_gYear )
	throw USER_EXCEPTION2(SE1003, "Calling op:gYear-not-equal on invalid arguments");

    return tuple_cell::atomic(!xs_dateTime_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_gYear),
				XMLDateTime(a2.get_xs_dateTime(), xs_gYear)));
}

tuple_cell op_gYearMonth_not_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_gYearMonth ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_gYearMonth )
	throw USER_EXCEPTION2(SE1003, "Calling op:gYearMonth-not-equal on invalid arguments");

    return tuple_cell::atomic(!xs_dateTime_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_gYearMonth),
				XMLDateTime(a2.get_xs_dateTime(), xs_gYearMonth)));
}

tuple_cell op_gMonth_not_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_gMonth ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_gMonth )
	throw USER_EXCEPTION2(SE1003, "Calling op:gMonth-not-equal on invalid arguments");

    return tuple_cell::atomic(!xs_dateTime_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_gMonth),
				XMLDateTime(a2.get_xs_dateTime(), xs_gMonth)));
}

tuple_cell op_gMonthDay_not_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_gMonthDay ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_gMonthDay )
	throw USER_EXCEPTION2(SE1003, "Calling op:gMonthDay-not-equal on invalid arguments");

    return tuple_cell::atomic(!xs_dateTime_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_gMonthDay),
				XMLDateTime(a2.get_xs_dateTime(), xs_gMonthDay)));
}

tuple_cell op_gDay_not_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_gDay ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_gDay )
	throw USER_EXCEPTION2(SE1003, "Calling op:gDay-not-equal on invalid arguments");

    return tuple_cell::atomic(!xs_dateTime_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_gDay),
				XMLDateTime(a2.get_xs_dateTime(), xs_gDay)));
}

//===================================================================================================

tuple_cell op_date_less_than(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_date ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_date )
	throw USER_EXCEPTION2(SE1003, "Calling op:date-less-than on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_less_than(
				XMLDateTime(a1.get_xs_dateTime(), xs_date),
				XMLDateTime(a2.get_xs_dateTime(), xs_date)));
}

tuple_cell op_time_less_than(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_time ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_time )
	throw USER_EXCEPTION2(SE1003, "Calling op:time-less-than on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_less_than(
				XMLDateTime(a1.get_xs_dateTime(), xs_time),
				XMLDateTime(a2.get_xs_dateTime(), xs_time)));
}

tuple_cell op_dateTime_less_than(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dateTime ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dateTime )
	throw USER_EXCEPTION2(SE1003, "Calling op:dateTime-less-than on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_less_than(
				XMLDateTime(a1.get_xs_dateTime(), xs_dateTime),
				XMLDateTime(a2.get_xs_dateTime(), xs_dateTime)));
}

tuple_cell op_yearMonthDuration_less_than(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_yearMonthDuration ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_yearMonthDuration )
	throw USER_EXCEPTION2(SE1003, "Calling op:yearMonthDuration-less-than on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_less_than(
				XMLDateTime(a1.get_xs_duration(), xs_yearMonthDuration),
				XMLDateTime(a2.get_xs_duration(), xs_yearMonthDuration)));
}

tuple_cell op_dayTimeDuration_less_than(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dayTimeDuration ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dayTimeDuration )
	throw USER_EXCEPTION2(SE1003, "Calling op:dayTimeDuration-less-than on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_less_than(
				XMLDateTime(a1.get_xs_duration(), xs_dayTimeDuration),
				XMLDateTime(a2.get_xs_duration(), xs_dayTimeDuration)));
}

//================================================================================================

tuple_cell op_date_greater_than(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_date ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_date )
	throw USER_EXCEPTION2(SE1003, "Calling op:date-greater-than on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_greater_than(
				XMLDateTime(a1.get_xs_dateTime(), xs_date),
				XMLDateTime(a2.get_xs_dateTime(), xs_date)));
}

tuple_cell op_time_greater_than(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_time ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_time )
	throw USER_EXCEPTION2(SE1003, "Calling op:time-greater-than on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_greater_than(
				XMLDateTime(a1.get_xs_dateTime(), xs_time),
				XMLDateTime(a2.get_xs_dateTime(), xs_time)));
}

tuple_cell op_dateTime_greater_than(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dateTime ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dateTime )
	throw USER_EXCEPTION2(SE1003, "Calling op:dateTime-greater-than on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_greater_than(
				XMLDateTime(a1.get_xs_dateTime(), xs_dateTime),
				XMLDateTime(a2.get_xs_dateTime(), xs_dateTime)));
}

tuple_cell op_yearMonthDuration_greater_than(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_yearMonthDuration ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_yearMonthDuration )
	throw USER_EXCEPTION2(SE1003, "Calling op:yearMonthDuration-greater-than on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_greater_than(
				XMLDateTime(a1.get_xs_duration(), xs_yearMonthDuration),
				XMLDateTime(a2.get_xs_duration(), xs_yearMonthDuration)));
}

tuple_cell op_dayTimeDuration_greater_than(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dayTimeDuration ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dayTimeDuration )
	throw USER_EXCEPTION2(SE1003, "Calling op:dayTimeDuration-greater-than on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_greater_than(
				XMLDateTime(a1.get_xs_duration(), xs_dayTimeDuration),
				XMLDateTime(a2.get_xs_duration(), xs_dayTimeDuration)));
}

//==================================================================================================

tuple_cell op_date_greater_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_date ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_date )
	throw USER_EXCEPTION2(SE1003, "Calling op:date-greater-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_greater_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_date),
				XMLDateTime(a2.get_xs_dateTime(), xs_date)));
}

tuple_cell op_time_greater_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_time ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_time )
	throw USER_EXCEPTION2(SE1003, "Calling op:time-greater-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_greater_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_time),
				XMLDateTime(a2.get_xs_dateTime(), xs_time)));
}

tuple_cell op_dateTime_greater_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dateTime ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dateTime )
	throw USER_EXCEPTION2(SE1003, "Calling op:dateTime-greater-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_greater_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_dateTime),
				XMLDateTime(a2.get_xs_dateTime(), xs_dateTime)));
}

tuple_cell op_yearMonthDuration_greater_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_yearMonthDuration ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_yearMonthDuration )
	throw USER_EXCEPTION2(SE1003, "Calling op:yearMonthDuration-greater-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_greater_equal(
				XMLDateTime(a1.get_xs_duration(), xs_yearMonthDuration),
				XMLDateTime(a2.get_xs_duration(), xs_yearMonthDuration)));
}

tuple_cell op_dayTimeDuration_greater_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dayTimeDuration ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dayTimeDuration )
	throw USER_EXCEPTION2(SE1003, "Calling op:dayTimeDuration-greater-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_greater_equal(
				XMLDateTime(a1.get_xs_duration(), xs_dayTimeDuration),
				XMLDateTime(a2.get_xs_duration(), xs_dayTimeDuration)));
}

//=====================================================================================================

tuple_cell op_date_less_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_date ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_date )
	throw USER_EXCEPTION2(SE1003, "Calling op:date-less-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_less_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_date),
				XMLDateTime(a2.get_xs_dateTime(), xs_date)));
}

tuple_cell op_time_less_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_time ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_time )
	throw USER_EXCEPTION2(SE1003, "Calling op:time-less-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_less_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_time),
				XMLDateTime(a2.get_xs_dateTime(), xs_time)));
}

tuple_cell op_dateTime_less_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dateTime ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dateTime )
	throw USER_EXCEPTION2(SE1003, "Calling op:dateTime-less-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_less_equal(
				XMLDateTime(a1.get_xs_dateTime(), xs_dateTime),
				XMLDateTime(a2.get_xs_dateTime(), xs_dateTime)));
}

tuple_cell op_yearMonthDuration_less_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_yearMonthDuration ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_yearMonthDuration )
	throw USER_EXCEPTION2(SE1003, "Calling op:yearMonthDuration-less-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_less_equal(
				XMLDateTime(a1.get_xs_duration(), xs_yearMonthDuration),
				XMLDateTime(a2.get_xs_duration(), xs_yearMonthDuration)));
}

tuple_cell op_dayTimeDuration_less_equal(const tuple_cell &a1, const tuple_cell &a2)
{
    if (!a1.is_atomic() || a1.get_atomic_type() != xs_dayTimeDuration ||
	!a2.is_atomic() || a2.get_atomic_type() != xs_dayTimeDuration )
	throw USER_EXCEPTION2(SE1003, "Calling op:dayTimeDuration-less-equal on invalid arguments");

    return tuple_cell::atomic(xs_dateTime_less_equal(
				XMLDateTime(a1.get_xs_duration(), xs_dayTimeDuration),
				XMLDateTime(a2.get_xs_duration(), xs_dayTimeDuration)));
}

