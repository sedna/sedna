/*
 * File:  dateTime_operations.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _DATETIME_OPERATIONS_H
#define _DATETIME_OPERATIONS_H

#include "sedna.h"
#include "tuple.h"

/*******************************************************************************
 * OPERATORS ON DATETIME VALUES: BEGIN
 ******************************************************************************/

tuple_cell op_add_yearMonthDuration_to_date(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_add_date_to_yearMonthDuration(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_add_dayTimeDuration_to_date(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_add_date_to_dayTimeDuration(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_add_dayTimeDuration_to_time(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_add_time_to_dayTimeDuration(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_add_yearMonthDuration_to_dateTime(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_add_dateTime_to_yearMonthDuration(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_add_dayTimeDuration_to_dateTime(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_add_dateTime_to_dayTimeDuration(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_add_yearMonthDurations(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_add_dayTimeDurations(const tuple_cell &a1, const tuple_cell &a2);

tuple_cell op_subtract_dates(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_subtract_yearMonthDuration_from_date(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_subtract_dayTimeDuration_from_date(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_subtract_times(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_subtract_dayTimeDuration_from_time(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_subtract_dateTimes(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_subtract_yearMonthDuration_from_dateTime(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_subtract_dayTimeDuration_from_dateTime(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_subtract_yearMonthDurations(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_subtract_dayTimeDurations(const tuple_cell &a1, const tuple_cell &a2);

tuple_cell op_multiply_yearMonthDuration_by_numeric(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_multiply_numeric_by_yearMonthDuration(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_multiply_dayTimeDuration_by_numeric(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_multiply_numeric_by_dayTimeDuration(const tuple_cell &a1, const tuple_cell &a2);

tuple_cell op_divide_yearMonthDuration_by_numeric(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_divide_dayTimeDuration_by_numeric(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_divide_yearMonthDuration_by_yearMonthDuration(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_divide_dayTimeDuration_by_dayTimeDuration(const tuple_cell &a1, const tuple_cell &a2);

tuple_cell op_date_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_time_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_dateTime_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_yearMonthDuration_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_dayTimeDuration_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_duration_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_gYear_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_gYearMonth_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_gMonth_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_gMonthDay_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_gDay_equal(const tuple_cell &a1, const tuple_cell &a2);

tuple_cell op_date_not_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_time_not_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_dateTime_not_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_yearMonthDuration_not_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_dayTimeDuration_not_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_duration_not_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_gYear_not_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_gYearMonth_not_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_gMonth_not_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_gMonthDay_not_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_gDay_not_equal(const tuple_cell &a1, const tuple_cell &a2);

tuple_cell op_date_less_than(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_time_less_than(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_dateTime_less_than(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_yearMonthDuration_less_than(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_dayTimeDuration_less_than(const tuple_cell &a1, const tuple_cell &a2);

tuple_cell op_date_greater_than(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_time_greater_than(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_dateTime_greater_than(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_yearMonthDuration_greater_than(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_dayTimeDuration_greater_than(const tuple_cell &a1, const tuple_cell &a2);

tuple_cell op_date_greater_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_time_greater_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_dateTime_greater_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_yearMonthDuration_greater_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_dayTimeDuration_greater_equal(const tuple_cell &a1, const tuple_cell &a2);

tuple_cell op_date_less_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_time_less_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_dateTime_less_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_yearMonthDuration_less_equal(const tuple_cell &a1, const tuple_cell &a2);
tuple_cell op_dayTimeDuration_less_equal(const tuple_cell &a1, const tuple_cell &a2);

#endif

