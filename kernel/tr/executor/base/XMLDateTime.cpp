/*
 * File:  XMLDateTime.cpp
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <stdlib.h>
#include <errno.h>
#include <math.h>

#include "common/sedna.h"
#include "tr/executor/base/XMLDateTime.h"
#include "tr/executor/base/PPBase.h"


//
// constants used to process raw data (fBuffer)
//
// [-]{CCYY-MM-DD}'T'{HH:MM:SS.MS}['Z']
//                                [{+|-}hh:mm']
//

static const char DURATION_STARTER     = 'P';              // chLatin_P
static const char DURATION_Y           = 'Y';              // chLatin_Y
static const char DURATION_M           = 'M';              // chLatin_M
static const char DURATION_D           = 'D';              // chLatin_D
static const char DURATION_H           = 'H';            // chLatin_H
static const char DURATION_S           = 'S';              // chLatin_S

static const char DATE_SEPARATOR       = '-';                 // '-'
static const char TIME_SEPARATOR       = ':';                // ':'
static const char TIMEZONE_SEPARATOR   = ':';                // ':'
static const char DATETIME_SEPARATOR   = 'T';              // 'T'
static const char MILISECOND_SEPARATOR = '.';               // '.'

static const char UTC_STD_CHAR         = 'Z';              // 'Z'
static const char UTC_POS_CHAR         = '+';                 // '+'
static const char UTC_NEG_CHAR         = '-';                 // '-'

static const char UTC_SET[]            = {UTC_STD_CHAR           //"Z+-"
, UTC_POS_CHAR
, UTC_NEG_CHAR
, 0};


static const size_t YMD_MIN_SIZE    = 10;   // CCYY-MM-DD
static const size_t YMONTH_MIN_SIZE = 7;    // CCYY_MM
static const size_t TIME_MIN_SIZE   = 8;    // hh:mm:ss
static const size_t TIMEZONE_SIZE   = 5;    // hh:mm
static const size_t DAY_SIZE        = 5;    // ---DD
//static const size_t MONTH_SIZE      = 6;    // --MM--
static const size_t MONTHDAY_SIZE   = 7;    // --MM-DD

#define NOT_FOUND NULL

//define constants to be used in assigning default values for
//all date/time excluding duration
static const int YEAR_DEFAULT  = 1972;
static const int MONTH_DEFAULT = 12;
static const int DAY_DEFAULT   = 31;

static const unsigned short DUR_MILISECOND_DIGITS = 3;
static const unsigned short DT_MILISECOND_DIGITS  = 5;
static const int DUR_MILISECOND_MAX_VALUE = (int)pow((double)10, (double)DUR_MILISECOND_DIGITS);
static const int DT_MILISECOND_MAX_VALUE = (int)pow((double)10, (double)DT_MILISECOND_DIGITS);

// order-relation on duration is a partial order. The dates below are used to
// for comparison of 2 durations, based on the fact that
// duration x and y is x<=y iff s+x<=s+y
// see 3.2.6 duration W3C schema datatype specs
//
// the dates are in format: {CCYY,MM,DD, H, S, M, MS, timezone}
static const int DATETIMES[][XMLDateTime::TOTAL_FIELDS] =
{
    {1696, 9, 1, 0, 0, 0, 0, XMLDateTime::UTC_STD},
    {1697, 2, 1, 0, 0, 0, 0, XMLDateTime::UTC_STD},
    {1903, 3, 1, 0, 0, 0, 0, XMLDateTime::UTC_STD},
    {1903, 7, 1, 0, 0, 0, 0, XMLDateTime::UTC_STD}
};

static inline int fQuotient(int a, int b)
{
    div_t div_result = div(a, b);
    return div_result.quot;
}

/**
* XQuery rounding routine for floating point numbers
*/
static inline int xq_round( double v )
{
    int sign = v >= 0 ? 1 : -1;
    double remainder = sign*(v - ((int)v));

    if (remainder >= 0.5 )
    {
        if (remainder == 0.5 && sign == -1)
            return ((int)v);
        return ((int)v) + sign;
    }
    return ((int)v);
}

static inline int fQuotient(int temp, int low, int high)
{
    return fQuotient(temp - low, high - low);
}

static inline int mod(int a, int b, int quotient)
{
    return (a - quotient*b) ;
}

static inline int modulo (int temp, int low, int high)
{
    //modulo(a - low, high - low) + low
    int a = temp - low;
    int b = high - low;
    return (mod (a, b, fQuotient(a, b)) + low) ;
}

static inline bool isLeapYear(int year)
{
    return((year%4 == 0) && ((year%100 != 0) || (year%400 == 0)));
}

static int maxDayInMonthFor(int year, int month)
{

    if ( month == 4 || month == 6 || month == 9 || month == 11 )
    {
        return 30;
    }
    else if ( month==2 )
    {
        if ( isLeapYear(year) )
            return 29;
        else
            return 28;
    }
    else
    {
        return 31;
    }

}

static int adjustMiliseconds(int value, size_t actualLen, size_t maxLen)
{
    for (size_t i=actualLen; i<maxLen; i++)
        value = value * 10;
    for (size_t i=maxLen; i<actualLen; i++)
        value = value / 10;
    return value;
}

static void handleWhitespace(const char * buf, size_t& start, size_t& end)
{
    size_t i;
    for (i=start; i<end; i++)
        if (! isspace(buf[i]))
            break;

    start = i;

    if (start == end) return;

    for (i=end-1; i>=start; i--)
        if (! isspace(buf[i]))
            break;

    end = i+1;
}

void normalizeMilisAndSeconds( double& milis, int& seconds )
{
    int milisNeg = milis >= 0 ? 1 : -1;
    int secondsNeg = seconds >= 0 ? 1 : -1;

    int carry = 0;

    if (milis * milisNeg >= 1.0)
    {
        milis = milis - 1.0 * milisNeg;
        carry = 1 * milisNeg;
    }

    if (secondsNeg != milisNeg && seconds != 0 && milis != 0.0)
    {
        seconds = seconds + carry - secondsNeg*1;
        milis = secondsNeg * (1.0 - milisNeg * milis);
    }
    else
        seconds += carry;
}

//-----------------------------------------------------------------------------
// Constructors
//-----------------------------------------------------------------------------
XMLDateTime::XMLDateTime()
{
    reset();
}

XMLDateTime::XMLDateTime(const utm& t)
{
    reset();
    setValue(Type, xs_dateTime);

    setValue(CentYear, t.utm_year);
    setValue(Month, t.utm_mon);
    setValue(Day, t.utm_mday);
    setValue(Hour, t.utm_hour);
    setValue(Minute, t.utm_min);
    setValue(Second, t.utm_sec);
    setValue(MiliSecond, xq_round(t.utm_millis/1000.0 * DT_MILISECOND_MAX_VALUE));

    int tz_neg = t.utm_gmtoff >= 0 ? 1 : -1;

    setValue(utc, t.utm_gmtoff == 0 ? UTC_STD :  (t.utm_gmtoff > 0 ? UTC_POS : UTC_NEG ));
    setValue(tz_hh, tz_neg * t.utm_gmtoff / 3600 );
    setValue(tz_mm, tz_neg * ( t.utm_gmtoff % 3600 )/ 60 );
}

XMLDateTime::XMLDateTime(const xs_packed_datetime& dt, xmlscm_type type)
{
    reset();
    setValue(Type, type);

    setValue(CentYear, dt.year);
    setValue(Month, dt.month);
    setValue(Day, dt.day);
    setValue(Hour, dt.hour);
    setValue(Minute, dt.minute);
    setValue(Second, dt.second);
    setValue(MiliSecond, dt.mili);
    setValue(utc, dt.utc);
    setValue(tz_hh, dt.tz_hh);
    setValue(tz_mm, dt.tz_mm);
}

XMLDateTime::XMLDateTime(const xs_packed_duration& dur, xmlscm_type type)
{
    reset();
    setValue(Type, type);

    int neg = dur.neg == 0 ? 1 : -1;

    setValue(CentYear, neg * dur.years);
    setValue(Month, neg * dur.months );
    setValue(Day, neg * dur.days );
    setValue(Hour, neg * dur.hours );
    setValue(Minute, neg * dur.minutes );
    setValue(Second, neg * dur.seconds );
    setValue(MiliSecond, neg * dur.milis );
    setValue(utc, dur.neg == 0 ? UTC_POS : UTC_NEG );
}

xs_packed_datetime XMLDateTime::getPackedDateTime() const
{
    xs_packed_datetime retval;

    retval.year = getValue(CentYear);
    retval.month = getValue(Month);
    retval.day = getValue(Day);
    retval.hour = getValue(Hour);
    retval.minute = getValue(Minute);
    retval.second = getValue(Second);
    retval.mili = getValue(MiliSecond);
    retval.utc = getValue(utc);
    retval.tz_hh = getValue(tz_hh);
    retval.tz_mm = getValue(tz_mm);

    return retval;
}

xs_packed_duration XMLDateTime::getPackedDuration() const
{
    xs_packed_duration retval;

    int neg = getValue(utc) == UTC_POS ? 1 : -1;
    retval.neg = getValue(utc) == UTC_POS ? 0 : 1;
    retval.years = neg * getValue(CentYear);
    retval.months = neg * getValue(Month);
    retval.days = neg * getValue(Day);
    retval.hours = neg * getValue(Hour);
    retval.minutes = neg * getValue(Minute);
    retval.seconds = neg * getValue(Second);
    retval.milis = neg * getValue(MiliSecond);
    return retval;
}

//---------------------------------------------------------------------------------------
// Conversion function
//---------------------------------------------------------------------------------------
XMLDateTime XMLDateTime::convertTo(xmlscm_type type)
{
    XMLDateTime newValue;
    newValue.setValue(Type, type);

    switch (type)
    {
    case xs_gYear: 		newValue.setValue(CentYear, getValue(CentYear));
        break;
    case xs_gYearMonth:	newValue.setValue(CentYear, getValue(CentYear));
        newValue.setValue(Month, getValue(Month));
        break;
    case xs_gMonth:		newValue.setValue(Month, getValue(Month));
        break;
    case xs_gDay:		newValue.setValue(Day, getValue(Day));
        break;
    case xs_gMonthDay:	newValue.setValue(Month, getValue(Month));
        newValue.setValue(Day, getValue(Day));
        break;
    case xs_date:		newValue.setValue(CentYear, getValue(CentYear));
        newValue.setValue(Month, getValue(Month));
        newValue.setValue(Day, getValue(Day));
        break;
    case xs_time:		newValue.setValue(Hour, getValue(Hour));
        newValue.setValue(Minute, getValue(Minute));
        newValue.setValue(Second, getValue(Second));
        newValue.setValue(MiliSecond, getValue(MiliSecond));
        break;
    case xs_dateTime:	newValue.setValue(CentYear, getValue(CentYear));
        newValue.setValue(Month, getValue(Month));
        newValue.setValue(Day, getValue(Day));
        newValue.setValue(Hour, getValue(Hour));
        newValue.setValue(Minute, getValue(Minute));
        newValue.setValue(Second, getValue(Second));
        newValue.setValue(MiliSecond, getValue(MiliSecond));
        break;
    case xs_duration:	newValue.setValue(CentYear, getValue(CentYear));
        newValue.setValue(Month, getValue(Month));
        newValue.setValue(Day, getValue(Day));
        newValue.setValue(Hour, getValue(Hour));
        newValue.setValue(Minute, getValue(Minute));
        newValue.setValue(Second, getValue(Second));
        newValue.setValue(MiliSecond, getValue(MiliSecond));
        break;
    case xs_yearMonthDuration: 	newValue.setValue(CentYear, getValue(CentYear));
        newValue.setValue(Month, getValue(Month));
        break;
    case xs_dayTimeDuration:	newValue.setValue(Day, getValue(Day));
        newValue.setValue(Hour, getValue(Hour));
        newValue.setValue(Minute, getValue(Minute));
        newValue.setValue(Second, getValue(Second));
        newValue.setValue(MiliSecond, getValue(MiliSecond));
        break;
    }

    ////////////////////////////////////////////////////////////////
    /// if ST is xs:yearMonthDuration and TT is xs:dayTimeDuration, the cast is permitted and returns a xs:dayTimeDuration with value 0.0 seconds.
    /// if ST is xs:dayTimeDuration and TT is xs:yearMonthDuration, the cast is permitted and returns a xs:yearMonthDuration with value 0 months.
    /// Target value should have UTC_POS.
    if((type == xs_yearMonthDuration && getValue(Type) == xs_dayTimeDuration) ||
        (type == xs_dayTimeDuration && getValue(Type) == xs_yearMonthDuration))
    {
        newValue.setValue(utc, UTC_POS);
        return newValue;
    }
    ////////////////////////////////////////////////////////////////

    newValue.setValue(utc, getValue(utc));
    newValue.setValue(tz_hh, getValue(tz_hh));
    newValue.setValue(tz_mm, getValue(tz_mm));
    return newValue;
}

//---------------------------------------------------------------------------------------
// Component extraction functions
//---------------------------------------------------------------------------------------
int XMLDateTime::getYears() const
{ 
    XMLDateTime dt = *this;
    if (dt.isDuration())
        dt.normalize();
    return dt.getValue(CentYear);
}

int XMLDateTime::getMonths() const
{ 
    XMLDateTime dt = *this;
    if (dt.isDuration())
        dt.normalize();
    return dt.getValue(Month);
}

int XMLDateTime::getDays() const
{ 
    XMLDateTime dt = *this;
    if (dt.isDuration())
        dt.normalize();
    return dt.getValue(Day);
}

int XMLDateTime::getHours() const
{ 
    XMLDateTime dt = *this;
    if (dt.isDuration())
        dt.normalize();
    return dt.getValue(Hour);
}

int XMLDateTime::getMinutes() const
{ 
    XMLDateTime dt = *this;
    if (dt.isDuration())
        dt.normalize();
    return dt.getValue(Minute);
}

double XMLDateTime::getSeconds() const
{ 
    XMLDateTime dt = *this;
    double milis;
    if (dt.isDuration())
    {
        dt.normalize();
        milis = ( dt.getValue(MiliSecond) == 0 ) ? 0.0 : dt.getValue(MiliSecond)/(double)DUR_MILISECOND_MAX_VALUE;
    }
    else
        milis = ( dt.getValue(MiliSecond) == 0 ) ? 0.0 : dt.getValue(MiliSecond)/(double)DT_MILISECOND_MAX_VALUE;

    return (double)dt.getValue(Second) + milis;
} 

XMLDateTime XMLDateTime::getTimezone() const
{ 
    XMLDateTime tz;
    tz.setValue(Type, xs_dayTimeDuration);
    int neg = (getValue(utc) == UTC_NEG )? -1: 1;
    tz.setValue(utc, getValue(utc) == UTC_NEG?UTC_NEG:UTC_POS );
    tz.setValue(Hour, neg * getValue(tz_hh));
    tz.setValue(Minute, neg * getValue(tz_mm));
    tz.normalizeDuration();
    return tz;
}

//----------------------------------------------------------------------------------------
// Arithmetic operations on dateTimes and durations
//----------------------------------------------------------------------------------------

XMLDateTime addDurations(const XMLDateTime& d1, const XMLDateTime& d2)
{
    XMLDateTime newDuration;
    newDuration.setValue(XMLDateTime::Type, d1.getValue(XMLDateTime::Type));


    if (d1.getValue(XMLDateTime::Type) == xs_yearMonthDuration)
    {
        int month = d1.getValue(XMLDateTime::CentYear)*12 + d1.getValue(XMLDateTime::Month) + 
            d2.getValue(XMLDateTime::CentYear)*12 + d2.getValue(XMLDateTime::Month);

        newDuration.setValue(XMLDateTime::Month, month );
        newDuration.setValue(XMLDateTime::utc, month >= 0 ? XMLDateTime::UTC_POS : XMLDateTime::UTC_NEG );	
        newDuration.normalize();
        return newDuration;
    }
    else //must be xs_dayTimeDuration
    {
        //add miliseconds
        int carry = 0;
        double milis1 = d1.getValue(XMLDateTime::MiliSecond) == 0 ? 0.0 :
            d1.getValue(XMLDateTime::MiliSecond)/(double)DUR_MILISECOND_MAX_VALUE;
        double milis2 = d2.getValue(XMLDateTime::MiliSecond) == 0 ? 0.0 :
            d2.getValue(XMLDateTime::MiliSecond)/(double)DUR_MILISECOND_MAX_VALUE;
        double milis = milis1 + milis2;
        int seconds = 
            d1.getValue(XMLDateTime::Second) + d2.getValue(XMLDateTime::Second) + carry +
            d1.getValue(XMLDateTime::Minute)*60 + d2.getValue(XMLDateTime::Minute)*60 +
            d1.getValue(XMLDateTime::Hour)*60*60 + d2.getValue(XMLDateTime::Hour)*60*60 +
            d1.getValue(XMLDateTime::Day)*60*60*24 + d2.getValue(XMLDateTime::Day)*60*60*24;

        normalizeMilisAndSeconds( milis, seconds );
        int neg = seconds >= 0 && milis >= 0.0 ? 1 : -1;

        newDuration.setValue(XMLDateTime::MiliSecond, xq_round(milis * DUR_MILISECOND_MAX_VALUE));
        newDuration.setValue(XMLDateTime::Second, seconds);
        newDuration.setValue(XMLDateTime::utc, neg == 1 ? XMLDateTime::UTC_POS : XMLDateTime::UTC_NEG );	
        newDuration.normalize();
        return newDuration;
    }
}

XMLDateTime subtractDurations(const XMLDateTime& d1, const XMLDateTime& d2)
{
    return addDurations(d1, multiplyDuration(d2, -1.0));
}

XMLDateTime multiplyDuration(const XMLDateTime& d, double v)
{
    XMLDateTime newDuration;
    newDuration.setValue(XMLDateTime::Type, d.getValue(XMLDateTime::Type));

    if (d.getValue(XMLDateTime::Type) == xs_yearMonthDuration)
    {
        int months = d.getValue(XMLDateTime::CentYear)*12 + d.getValue(XMLDateTime::Month);
        double multMonths = months * v;
        int neg = multMonths >= 0 ? 1 : -1;

        months = xq_round( multMonths );
        newDuration.setValue(XMLDateTime::Month, months);
        newDuration.setValue(XMLDateTime::utc, neg == 1? XMLDateTime::UTC_POS : XMLDateTime::UTC_NEG);

        newDuration.normalize();
        return newDuration;
    }
    else // must be xs_dayTimeDuration
    {
        int miliSecond = d.getValue(XMLDateTime::MiliSecond);
        double milis = (miliSecond == 0)?0.0:miliSecond/(double)DUR_MILISECOND_MAX_VALUE;
        long seconds = d.getValue(XMLDateTime::Second) + d.getValue(XMLDateTime::Minute)*60 
            + d.getValue(XMLDateTime::Hour)*60*60 + d.getValue(XMLDateTime::Day)*60*60*24;
        double seconds_milis = (seconds + milis) * v;
        int neg = seconds_milis >= 0 ? 1 : -1;
        newDuration.setValue(XMLDateTime::Second, (int)seconds_milis);
        if (seconds_milis - (int)seconds_milis != 0.0)
            newDuration.setValue(XMLDateTime::MiliSecond, xq_round((seconds_milis - (int)seconds_milis)*DUR_MILISECOND_MAX_VALUE));

        newDuration.setValue(XMLDateTime::utc, neg == 1? XMLDateTime::UTC_POS : XMLDateTime::UTC_NEG );

        newDuration.normalize();
        return newDuration;
    }
}

xs_decimal_t divideDurationByDuration(const XMLDateTime& d1, const XMLDateTime& d2)
{
    if (d1.getValue(XMLDateTime::Type) == xs_yearMonthDuration)
    {
        int64_t m1 = d1.getValue(XMLDateTime::CentYear)*12 + d1.getValue(XMLDateTime::Month);
        int64_t m2 = d2.getValue(XMLDateTime::CentYear)*12 + d2.getValue(XMLDateTime::Month);
        return (xs_decimal_t(m1)/ xs_decimal_t(m2));
    }
    else // must be xs_dayTimeDuration
    {
        double milis1 = d1.getValue(XMLDateTime::MiliSecond) == 0 ? 0.0 :
            d1.getValue(XMLDateTime::MiliSecond)/(double)DUR_MILISECOND_MAX_VALUE;
        long seconds1 = d1.getValue(XMLDateTime::Second) + d1.getValue(XMLDateTime::Minute)*60 + d1.getValue(XMLDateTime::Hour)*60*60 +
            d1.getValue(XMLDateTime::Day)*60*60*24;
        double milis2 = d2.getValue(XMLDateTime::MiliSecond) == 0 ? 0.0 :
            d2.getValue(XMLDateTime::MiliSecond)/(double)DUR_MILISECOND_MAX_VALUE;
        long seconds2 = d2.getValue(XMLDateTime::Second) + d2.getValue(XMLDateTime::Minute)*60 + d2.getValue(XMLDateTime::Hour)*60*60 +
            d2.getValue(XMLDateTime::Day)*60*60*24;
        return ( xs_decimal_t(seconds1 + milis1) / xs_decimal_t(seconds2 + milis2));
    }
}

XMLDateTime divideDuration(const XMLDateTime& d, double v)
{
    return multiplyDuration(d, 1.0/v );
}	

XMLDateTime addDurationToDateTime(const XMLDateTime& dt, const XMLDateTime& fDuration)
{
    XMLDateTime fNewDate;
    int carry;

    fNewDate.setValue(XMLDateTime::Type, dt.getValue(XMLDateTime::Type));

    //add months
    int temp = dt.getValue(XMLDateTime::Month) + fDuration.getValue(XMLDateTime::Month);
    carry = fQuotient(temp, 1, 13);
    fNewDate.setValue(XMLDateTime::Month, modulo(temp, 1, 13));
    if (fNewDate.getValue(XMLDateTime::Month) <= 0) {
        fNewDate.setValue(XMLDateTime::Month, fNewDate.getValue(XMLDateTime::Month) + 12);
        carry--;
    }

    //add years (may be modified additionaly below)
    fNewDate.setValue(XMLDateTime::CentYear, dt.getValue(XMLDateTime::CentYear) + fDuration.getValue(XMLDateTime::CentYear) + carry);
    //copy timezone

    fNewDate.setValue(XMLDateTime::utc, dt.getValue(XMLDateTime::utc));
    fNewDate.setValue(XMLDateTime::tz_hh, dt.getValue(XMLDateTime::tz_hh));
    fNewDate.setValue(XMLDateTime::tz_mm, dt.getValue(XMLDateTime::tz_mm));

    //add miliseconds
    carry = 0;
    double milis = dt.getValue(XMLDateTime::MiliSecond)/(double)DT_MILISECOND_MAX_VALUE + 
        fDuration.getValue(XMLDateTime::MiliSecond)/(double)DUR_MILISECOND_MAX_VALUE;

    if (milis>=1.0 || milis <0.0)
    {
        if (milis>=1.0)
        {
            milis -= 1.0;
            carry = 1;
        }
        else if (milis < 0.0)
        {
            milis += 1.0;
            carry = -1;
        }
    }

    fNewDate.setValue(XMLDateTime::MiliSecond, xq_round(milis * DT_MILISECOND_MAX_VALUE));

    //add seconds
    temp = dt.getValue(XMLDateTime::Second) + fDuration.getValue(XMLDateTime::Second) + carry;
    carry = fQuotient (temp, 60);
    fNewDate.setValue(XMLDateTime::Second,  mod(temp, 60, carry));
    if (fNewDate.getValue(XMLDateTime::Second) < 0) {
        fNewDate.setValue(XMLDateTime::Second, fNewDate.getValue(XMLDateTime::Second) + 60);
        carry--;
    }

    //add minutes
    temp = dt.getValue(XMLDateTime::Minute) + fDuration.getValue(XMLDateTime::Minute) + carry;
    carry = fQuotient(temp, 60);
    fNewDate.setValue(XMLDateTime::Minute, mod(temp, 60, carry));
    if (fNewDate.getValue(XMLDateTime::Minute) < 0) {
        fNewDate.setValue(XMLDateTime::Minute, fNewDate.getValue(XMLDateTime::Minute) + 60);
        carry--;
    }

    //add hours
    temp = dt.getValue(XMLDateTime::Hour) + fDuration.getValue(XMLDateTime::Hour) + carry;
    carry = fQuotient(temp, 24);
    fNewDate.setValue(XMLDateTime::Hour, mod(temp, 24, carry));
    if (fNewDate.getValue(XMLDateTime::Hour) < 0) {
        fNewDate.setValue( XMLDateTime::Hour, fNewDate.getValue(XMLDateTime::Hour) + 24 );
        carry--;
    }

    /*
    *  Add days according to the W3C algorithm
    *
    *	if S[day] > maximumDayInMonthFor(E[year], E[month])
    *		tempDays := maximumDayInMonthFor(E[year], E[month])
    *	else if S[day] < 1
    *		tempDays := 1
    *	else
    *		tempDays := S[day]
    *	E[day] := tempDays + D[day] + carry
    */
    if ( dt.getValue(XMLDateTime::Day) > maxDayInMonthFor(fNewDate.getValue(XMLDateTime::CentYear), fNewDate.getValue(XMLDateTime::Month)))
        temp = maxDayInMonthFor(fNewDate.getValue(XMLDateTime::CentYear), fNewDate.getValue(XMLDateTime::Month));
    else 
        temp = dt.getValue(XMLDateTime::Day);

    fNewDate.setValue(XMLDateTime::Day, temp + fDuration.getValue(XMLDateTime::Day) + carry);

    while ( true )
    {
        int temp = maxDayInMonthFor(fNewDate.getValue(XMLDateTime::CentYear), fNewDate.getValue(XMLDateTime::Month));
        if ( fNewDate.getValue(XMLDateTime::Day) < 1 )
        { //original fNewDate was negative
            fNewDate.setValue(XMLDateTime::Day, fNewDate.getValue(XMLDateTime::Day) + maxDayInMonthFor(
                fNewDate.getValue(XMLDateTime::CentYear), fNewDate.getValue(XMLDateTime::Month)-1));
            carry = -1;
        }
        else if ( fNewDate.getValue(XMLDateTime::Day) > temp )
        {
            fNewDate.setValue(XMLDateTime::Day, fNewDate.getValue(XMLDateTime::Day) - temp);
            carry = 1;
        }
        else
        {
            break;
        }

        temp = fNewDate.getValue(XMLDateTime::Month) + carry;
        fNewDate.setValue(XMLDateTime::Month, modulo(temp, 1, 13));
        if (fNewDate.getValue(XMLDateTime::Month) <= 0) {
            fNewDate.setValue(XMLDateTime::Month, fNewDate.getValue(XMLDateTime::Month) + 12);
            fNewDate.setValue(XMLDateTime::CentYear, fNewDate.getValue(XMLDateTime::CentYear)-1);
        }
        fNewDate.setValue(XMLDateTime::CentYear, fNewDate.getValue(XMLDateTime::CentYear) +  fQuotient(temp, 1, 13));
    }

    //Handle the transition of CentYear through 0. 0 is not a valid year
    if (dt.getValue(XMLDateTime::CentYear) < 0 && fNewDate.getValue(XMLDateTime::CentYear) >= 0 )
        fNewDate.setValue(XMLDateTime::CentYear, fNewDate.getValue(XMLDateTime::CentYear) + 1);

    else if (dt.getValue(XMLDateTime::CentYear) > 0 && fNewDate.getValue(XMLDateTime::CentYear) <= 0 )
        fNewDate.setValue(XMLDateTime::CentYear, fNewDate.getValue(XMLDateTime::CentYear) - 1);


    if (fNewDate.getValue(XMLDateTime::Type) == xs_date)
    {
        fNewDate.setValue(XMLDateTime::Hour, 0);
        fNewDate.setValue(XMLDateTime::Minute, 0);
        fNewDate.setValue(XMLDateTime::Second, 0);
        fNewDate.setValue(XMLDateTime::MiliSecond, 0);
    }

    return fNewDate;
}

XMLDateTime subtractDurationFromDateTime(const XMLDateTime& d1, const XMLDateTime& d)
{
    return addDurationToDateTime(d1, multiplyDuration(d, -1.0));
}

int rollMonth(int month)
{
    if (month < 0)
        return month+12;
    if (month >12)
        return month-12;
    return month;
}

XMLDateTime subtractDateTimes(const XMLDateTime& d1, const XMLDateTime& d2)
{
    XMLDateTime result;
    result.setValue(XMLDateTime::Type, xs_dayTimeDuration);

    // We always subtract the smaller value from the bigger one, therefore the final duration value
    // will be positive during the calculation and we will negate it in the end, if necessary
    int neg=1;

    XMLDateTime lTemp = d1, rTemp = d2;

    if (XMLDateTime::compare(d1,d2) == XMLDateTime::LESS_THAN)
    {
        neg = -1;
        lTemp = d2;
        rTemp = d1;
    }

    // Normalize the values, if the values are times, add the default date components
    if (lTemp.getValue(XMLDateTime::Type) == xs_time)
    {

        lTemp.setValue(XMLDateTime::CentYear, YEAR_DEFAULT);
        rTemp.setValue(XMLDateTime::CentYear, YEAR_DEFAULT);

        lTemp.setValue(XMLDateTime::Month, MONTH_DEFAULT);
        rTemp.setValue(XMLDateTime::Month, MONTH_DEFAULT);

        lTemp.setValue(XMLDateTime::Day, DAY_DEFAULT);
        rTemp.setValue(XMLDateTime::Day, DAY_DEFAULT);

        if (lTemp.getValue(XMLDateTime::Hour) == 24)
            lTemp.setValue(XMLDateTime::Hour, 0);
        if (rTemp.getValue(XMLDateTime::Hour) == 24)
            rTemp.setValue(XMLDateTime::Hour, 0);
    }	

    lTemp.setValue(XMLDateTime::Type, xs_dateTime);
    rTemp.setValue(XMLDateTime::Type, xs_dateTime);

    if (lTemp.getValue(XMLDateTime::utc) == XMLDateTime::UTC_UNKNOWN)
    {	
	    lTemp = adjustToTimezone(lTemp, XMLDateTime(getLocalTime()).getTimezone());
    }	

    if (rTemp.getValue(XMLDateTime::utc) == XMLDateTime::UTC_UNKNOWN)
    {	
        rTemp = adjustToTimezone(rTemp, XMLDateTime(getLocalTime()).getTimezone());
    }	

    lTemp.normalize();
    rTemp.normalize();

    int days = 0;

    for (int i=lTemp.getValue(XMLDateTime::CentYear)-1; i>rTemp.getValue(XMLDateTime::CentYear); i--)
    {
        if (isLeapYear(i))
            days += 366;
        else
            days += 365;
    }

    if (lTemp.getValue(XMLDateTime::CentYear) == rTemp.getValue(XMLDateTime::CentYear))
    {
        for (int month=lTemp.getValue(XMLDateTime::Month); month < rTemp.getValue(XMLDateTime::Month); month++)
            days += maxDayInMonthFor( lTemp.getValue(XMLDateTime::CentYear), month );
    }
    else
    {
        for (int month=1; month < lTemp.getValue(XMLDateTime::Month); month++)
            days += maxDayInMonthFor( lTemp.getValue(XMLDateTime::CentYear), month );
        for (int month=rTemp.getValue(XMLDateTime::Month); month <= 12; month++)
            days += maxDayInMonthFor( rTemp.getValue(XMLDateTime::CentYear), month );

    }

    days += lTemp.getValue(XMLDateTime::Day) - rTemp.getValue(XMLDateTime::Day);

    // Process times
    int seconds = 0;
    seconds += ( lTemp.getValue(XMLDateTime::Hour) - rTemp.getValue(XMLDateTime::Hour) ) * 60 * 60 ;
    seconds += ( lTemp.getValue(XMLDateTime::Minute) - rTemp.getValue(XMLDateTime::Minute) ) * 60 ;
    seconds +=  lTemp.getValue(XMLDateTime::Second) - rTemp.getValue(XMLDateTime::Second) ;

    double milis = ( lTemp.getValue(XMLDateTime::MiliSecond) - rTemp.getValue(XMLDateTime::MiliSecond)) / (double)DT_MILISECOND_MAX_VALUE;
    normalizeMilisAndSeconds( milis, seconds );

    if (seconds < 0 || milis < 0 )
    {
        days --;
        seconds += 60*60*24;
        normalizeMilisAndSeconds( milis, seconds );
    }		

    result.setValue(XMLDateTime::utc, neg==1 ? XMLDateTime::UTC_POS : XMLDateTime::UTC_NEG );
    result.setValue(XMLDateTime::Day, neg*days);
    result.setValue(XMLDateTime::Second, neg*seconds);
    result.setValue(XMLDateTime::MiliSecond, xq_round(milis*DUR_MILISECOND_MAX_VALUE));
    result.normalizeDuration();
    return result;
}

XMLDateTime adjustToTimezone(const XMLDateTime& dt)
{
    XMLDateTime fNewDate = dt;
    fNewDate.setValue(XMLDateTime::utc, 0);
    fNewDate.setValue(XMLDateTime::tz_hh, 0);
    fNewDate.setValue(XMLDateTime::tz_mm, 0);
    return fNewDate;
}

XMLDateTime adjustToTimezone(const XMLDateTime& dt, const XMLDateTime& tz)
{
    XMLDateTime fNewDate = dt;


    // compute the difference between the two timezones
    XMLDateTime dtTz = dt.getTimezone();
    XMLDateTime diff = subtractDurations(tz, dtTz);

    int utc_type = ( tz.getValue(XMLDateTime::Hour) < 0 )?XMLDateTime::UTC_NEG:XMLDateTime::UTC_POS;
    int neg = utc_type==XMLDateTime::UTC_NEG?-1:1;

    // check the timezone
    if (neg*tz.getValue(XMLDateTime::Hour) > 14 || (neg*tz.getValue(XMLDateTime::Hour) == 14 && tz.getValue(XMLDateTime::Minute) != 0))
        throw XQUERY_EXCEPTION2(FODT0003, "invalid timezone hours, values must be between -14 and 14");
    if (tz.getValue(XMLDateTime::Second) != 0 || tz.getValue(XMLDateTime::MiliSecond) !=0 )
        throw XQUERY_EXCEPTION2(FODT0003, "invalid timezone, non-integral number of minutes");

    fNewDate.setValue(XMLDateTime::utc, utc_type);
    fNewDate.setValue(XMLDateTime::tz_hh, neg*tz.getValue(XMLDateTime::Hour));
    fNewDate.setValue(XMLDateTime::tz_mm, neg*tz.getValue(XMLDateTime::Minute));

    //If the dateTime has an unknown timezone, simply set the timezone. Otherwise adjust
    // the dateTime to the se_new timezone.
    if (dt.getValue(XMLDateTime::utc) != XMLDateTime::UTC_UNKNOWN)
        fNewDate = addDurationToDateTime(fNewDate, diff);

    return fNewDate;
}

XMLDateTime fnDateTime(const XMLDateTime& d, const XMLDateTime& t)
{
    XMLDateTime fNewDateTime = d;

    fNewDateTime.setValue(XMLDateTime::Type, xs_dateTime);

    // Special case for hour, 24 is a lexical form of 0 and we can't normalize time (we'll lose the timezone).
    // Hence we handle it here

    int hours = t.getValue(XMLDateTime::Hour);
    hours = hours == 24 ? 0 : hours;
    fNewDateTime.setValue(XMLDateTime::Hour, hours);

    fNewDateTime.setValue(XMLDateTime::Minute, t.getValue(XMLDateTime::Minute));
    fNewDateTime.setValue(XMLDateTime::Second, t.getValue(XMLDateTime::Second));
    fNewDateTime.setValue(XMLDateTime::MiliSecond, t.getValue(XMLDateTime::MiliSecond));

    if (fNewDateTime.getValue(XMLDateTime::utc) == XMLDateTime::UTC_UNKNOWN)
    {	
        fNewDateTime.setValue(XMLDateTime::utc, t.getValue(XMLDateTime::utc));
        fNewDateTime.setValue(XMLDateTime::tz_hh, t.getValue(XMLDateTime::tz_hh));
        fNewDateTime.setValue(XMLDateTime::tz_mm, t.getValue(XMLDateTime::tz_mm));
    }

    else if (t.getValue(XMLDateTime::utc) != XMLDateTime::UTC_UNKNOWN)
    {
        if (XMLDateTime::compare(d.getTimezone(), t.getTimezone()) != 0)
            throw XQUERY_EXCEPTION2(FORG0008, "Different timezones in the components of date time, passed to fn:dateTime");
    }

    return fNewDateTime;
} 

int XMLDateTime::compare(const XMLDateTime& lValue
                         ,const XMLDateTime& rValue)
                         //, MemoryManager* const memMgr)
{
    //
    // If any of the them is not normalized() yet,
    // we need to do something here.
    //

    XMLDateTime lTemp = lValue;
    XMLDateTime rTemp = rValue;

    // Special case for xs:time, we need to use the comparison for dateTime
    // with an arbitrary dateTime value and also we need to normalize the hours

    if (lTemp.getValue(XMLDateTime::Type) == xs_time)
    {
        lTemp.setValue(XMLDateTime::Type, xs_dateTime);
        rTemp.setValue(XMLDateTime::Type, xs_dateTime);

        lTemp.setValue(XMLDateTime::CentYear, YEAR_DEFAULT);
        rTemp.setValue(XMLDateTime::CentYear, YEAR_DEFAULT);

        lTemp.setValue(XMLDateTime::Month, MONTH_DEFAULT);
        rTemp.setValue(XMLDateTime::Month, MONTH_DEFAULT);

        lTemp.setValue(XMLDateTime::Day, DAY_DEFAULT);
        rTemp.setValue(XMLDateTime::Day, DAY_DEFAULT);

        if (lTemp.getValue(XMLDateTime::Hour) == 24)
            lTemp.setValue(XMLDateTime::Hour, 0);
        if (rTemp.getValue(XMLDateTime::Hour) == 24)
            rTemp.setValue(XMLDateTime::Hour, 0);
    }	

    lTemp.normalize();
    rTemp.normalize();

    for ( int i = 0 ; i < XMLDateTime::MiliSecond; i++ )
    {
        if ( lTemp.getValue(i) < rTemp.getValue(i) )
        {
            return LESS_THAN;
        }
        else if ( lTemp.getValue(i) > rTemp.getValue(i) )
        {
            return GREATER_THAN;
        }
    }

    if ( lTemp.getValue(MiliSecond))
    {
        double lMilis, rMilis;
        if (lTemp.isDuration())
        {
            lMilis = lTemp.getValue(MiliSecond) == 0 ? 0.0 :
                lTemp.getValue(MiliSecond)/(double)DUR_MILISECOND_MAX_VALUE;

        rMilis = rTemp.getValue(MiliSecond) == 0 ? 0.0 :
            rTemp.getValue(MiliSecond)/(double)DUR_MILISECOND_MAX_VALUE;
        }	
        else
        {
            lMilis = lTemp.getValue(MiliSecond) == 0 ? 0.0 :
                lTemp.getValue(MiliSecond)/(double)DT_MILISECOND_MAX_VALUE;

        rMilis = rTemp.getValue(MiliSecond) == 0 ? 0.0 :
            rTemp.getValue(MiliSecond)/(double)DT_MILISECOND_MAX_VALUE;
        }	

        if ( lMilis < rMilis )
        {
            return LESS_THAN;
        }
        else if ( lMilis > rMilis )
        {
            return GREATER_THAN;
        }
    }

    /* This is for xs:date datatype, where the normalized value still contains a timezone!
    Doesn't happen in any other date/time types
    */
    if (lTemp.getValue(Type) ==xs_date && rTemp.getValue(Type) == xs_date && (   lTemp.getValue(utc) != UTC_STD || rTemp.getValue(utc) != UTC_STD))
    {
        // For timezones, the negative values are bigger than the positive ones
        int lNeg = lTemp.getValue(utc) == UTC_POS ? - 1 : 1;
        int rNeg = rTemp.getValue(utc) == UTC_POS ? - 1 : 1;

        if (lTemp.getValue(tz_hh) * lNeg < rTemp.getValue(tz_hh) * rNeg)
            return LESS_THAN;

        else if (lTemp.getValue(tz_hh) * lNeg > rTemp.getValue(tz_hh) * rNeg)
            return GREATER_THAN;

        if (lTemp.getValue(tz_mm) * lNeg < rTemp.getValue(tz_mm) * rNeg)
            return LESS_THAN;

        else if (lTemp.getValue(tz_mm) * lNeg > rTemp.getValue(tz_mm) * rNeg)
            return GREATER_THAN;
    }

    return EQUAL;
}


XMLDateTime::XMLDateTime(const XMLDateTime &toCopy)
{
    copy(toCopy);
}

XMLDateTime& XMLDateTime::operator=(const XMLDateTime& rhs)
{
    if (this == &rhs)
        return *this;

    copy(rhs);
    return *this;
}

void XMLDateTime::copy(const XMLDateTime &toCopy)
{
    for (int i=0; i<TOTAL_FIELDS; i++)
        fields[i] = toCopy.fields[i];
}

//
// [-]{CCYY-MM-DD}[TimeZone]
//
void XMLDateTime::parseDate(const char* buf)
{
    setValue(Type, xs_date);
    size_t start = 0, end = strlen(buf);
    handleWhitespace(buf, start, end);
    getDate(buf, start, end);
    parseTimeZone(buf, start, end);
    validateDateTime();
}

void XMLDateTime::parseTime(const char* buf)
{

    setValue(Type, xs_time);
    size_t start=0, end = strlen(buf);
    handleWhitespace(buf, start, end);
    // time initialize to default values
    getTime(buf, start, end);
    validateDateTime();
}

// 
// [-]{CCYY-MM-DD}'T'{HH:MM:SS.MS}[TimeZone]
// 
void XMLDateTime::parseDateTime(const char* buf)
{
    setValue(Type, xs_dateTime);
    size_t fStart = 0, fEnd = strlen(buf);
    handleWhitespace(buf, fStart, fEnd);
    getDate(buf,fStart, fEnd);

    //fStart is supposed to point to 'T'
    if (buf[fStart++] != DATETIME_SEPARATOR)
        throw XQUERY_EXCEPTION2(FORG0001, "Missing a time separator in dateTime value");

    getTime(buf, fStart, fEnd);
    validateDateTime();
    normalizeDateTimeWeak();
}

//
// {---DD}[TimeZone]
//  01234
//
void XMLDateTime::parseDay(const char* buf)
{
    setValue(Type, xs_gDay);
    size_t start=0, end=strlen(buf);
    handleWhitespace(buf, start, end);

    if (buf[start] != DATE_SEPARATOR ||
        buf[start+1] != DATE_SEPARATOR ||
        buf[start+2] != DATE_SEPARATOR  )
        throw XQUERY_EXCEPTION2(FORG0001, "invalid gDay value");

    //initialize values
    setValue(Day, parseInt(buf, start+3, start+5));

    if ( start+DAY_SIZE < end )
    {        
        size_t pos;
        char* res = indexOf(UTC_SET, buf[start+DAY_SIZE], pos);
        if ( res == NOT_FOUND )
            throw XQUERY_EXCEPTION2(FORG0001, "invalid gDay value");
        else
        {
            setValue(utc, (int)(pos+1));
            getTimeZone(buf, start+DAY_SIZE, end);
        }
    }

    validateDateTime();
}

//
// {--MM--}[TimeZone]
// {--MM}[TimeZone]
//
void XMLDateTime::parseMonth(const char* fBuffer)
{
    setValue(Type, xs_gMonth);
    size_t fStart=0, fEnd=strlen(fBuffer);
    handleWhitespace(fBuffer, fStart, fEnd);

    if (fBuffer[fStart] != DATE_SEPARATOR ||
        fBuffer[fStart+1] != DATE_SEPARATOR ||
        fEnd < fStart+4 )
        throw XQUERY_EXCEPTION2(FORG0001, "invalid gMonth value");

    // REVISIT: allow both --MM and --MM-- now. 
    // need to remove the following lines to disallow --MM-- 
    // when the errata is officially in the rec. 
    setValue(Month, parseInt(fBuffer, fStart+2, fStart+4));

    fStart = fStart+4;
    if ( fEnd >= fStart+2 &&
         fBuffer[fStart] == DATE_SEPARATOR &&
         fBuffer[fStart+1] == DATE_SEPARATOR ) 
    { 
        fStart += 2; 
    } 

    //
    // parse TimeZone if any
    //
    if ( fStart < fEnd )
    {      
        size_t pos;
        char* res = indexOf(UTC_SET, fBuffer[fStart], pos);
        if ( res == NOT_FOUND )
            throw XQUERY_EXCEPTION2(FORG0001, "invalid gMonth value");
        else
        {
            setValue(utc, (int)(pos+1));
            getTimeZone(fBuffer, fStart, fEnd);
        }
    }

    validateDateTime();
}

//
//[-]{CCYY}[TimeZone]
// 0  1234
//
void XMLDateTime::parseYear(const char* fBuffer)
{

    setValue(Type, xs_gYear);
    size_t fStart=0, fEnd=strlen(fBuffer);
    handleWhitespace(fBuffer, fStart, fEnd);
    // skip the first '-' and search for timezone
    //
    size_t sign;
    char * res = findUTCSign(fBuffer, (fBuffer[fStart] == '-') ? fStart+1 : fStart, fEnd, sign);

    if (res == NOT_FOUND)
    {
        setValue(CentYear, parseIntYear(fBuffer,fStart,fEnd));
    }
    else
    {
        setValue(CentYear, parseIntYear(fBuffer, fStart, sign));
        getTimeZone(fBuffer, sign, fEnd);
    }

    //initialize values
    validateDateTime();
}

//
//{--MM-DD}[TimeZone]
// 0123456
//
void XMLDateTime::parseMonthDay(const char* fBuffer)
{

    setValue(Type, xs_gMonthDay);
    size_t fStart=0, fEnd = strlen(fBuffer);
    handleWhitespace(fBuffer, fStart, fEnd);

    if (fBuffer[fStart] != DATE_SEPARATOR ||
        fBuffer[fStart+1] != DATE_SEPARATOR ||
        fBuffer[fStart+4] != DATE_SEPARATOR )
        throw XQUERY_EXCEPTION2(FORG0001, "invalid gMonthDay value");


    //initialize
    setValue(Month, parseInt(fBuffer, fStart+2, fStart+4));	
    setValue(Day, parseInt(fBuffer, fStart+5, fStart+7));

    if ( fStart+MONTHDAY_SIZE < fEnd )
    {        
        size_t pos;
        char* res = indexOf(UTC_SET, fBuffer[fStart+MONTHDAY_SIZE], pos);
        if ( res == NOT_FOUND )
            throw XQUERY_EXCEPTION2(FORG0001, "invalid gMonthDay value");
        else
        {
            setValue(utc, (int)(pos+1));
            getTimeZone(fBuffer,fStart+MONTHDAY_SIZE,fEnd);
        }
    }

    validateDateTime();
}

void XMLDateTime::parseYearMonth(const char* fBuffer)
{
    setValue(Type, xs_gYearMonth);
    size_t fStart=0, fEnd=strlen(fBuffer);
    handleWhitespace(fBuffer, fStart, fEnd);

    // get date
    getYearMonth(fBuffer, fStart, fEnd);
    parseTimeZone(fBuffer, fStart, fEnd);

    validateDateTime();
}

//
//PnYn MnDTnH nMnS: -P1Y2M3DT10H30M
//
// [-]{'P'{[n'Y'][n'M'][n'D']['T'][n'H'][n'M'][n'S']}}
//
//  Note: the n above shall be >= 0
//        if no time element found, 'T' shall be absent
//
void XMLDateTime::parseDuration(const char* fBuffer)
{
    setValue(Type, xs_duration);
    size_t fStart=0, fEnd = strlen(fBuffer);
    handleWhitespace(fBuffer, fStart, fEnd);
    // must start with '-' or 'P'
    //
    char c = fBuffer[fStart++];
    if ( (c != DURATION_STARTER) &&
        (c != '-')            )
        throw XQUERY_EXCEPTION2(FORG0001, "invalid duration start");

    // 'P' must ALWAYS be present in either case
    if ( (c == '-') &&
        (fBuffer[fStart++]!= DURATION_STARTER ))
        throw XQUERY_EXCEPTION2(FORG0001, "invalid duration");

    // java code
    //date[utc]=(c=='-')?'-':0;
    //fValue[utc] = UTC_STD;
    setValue(utc, (fBuffer[0] == '-'? UTC_NEG : UTC_POS));

    int negate = ( fBuffer[0] == '-'? -1 : 1);

    //
    // No negative value is allowed after 'P'
    //
    // eg P-1234, invalid
    //
    size_t temp;
    if (indexOf(fBuffer, fStart, fEnd, '-', temp) != NOT_FOUND)
        throw XQUERY_EXCEPTION2(FORG0001, "invalid duration");

    //at least one number and designator must be seen after P
    bool designator = false;

    size_t endDate;
    char* res = indexOf(fBuffer, fStart, fEnd, DATETIME_SEPARATOR, endDate);
    if ( res == NOT_FOUND )
    {
        endDate = fEnd;  // 'T' absent
    }

    //find 'Y'
    size_t end;
    res= indexOf(fBuffer, fStart, endDate, DURATION_Y, end);
    if ( res != NOT_FOUND )
    {
        //scan year
        if (fStart == end)
            throw XQUERY_EXCEPTION2(FORG0001, "invalid duration");
        setValue(CentYear, negate * parseInt(fBuffer, fStart, end));
        fStart = end+1;
        designator = true;
    }

    res = indexOf(fBuffer, fStart, endDate, DURATION_M, end);
    if ( res != NOT_FOUND )
    {
        //scan month
        if (fStart == end)
            throw XQUERY_EXCEPTION2(FORG0001, "invalid duration");
        setValue(Month, negate * parseInt(fBuffer, fStart, end));
        fStart = end+1;
        designator = true;
    }

    res = indexOf(fBuffer, fStart, endDate, DURATION_D, end);
    if ( res != NOT_FOUND )
    {
        //scan day
        if (fStart == end)
            throw XQUERY_EXCEPTION2(FORG0001, "invalid duration");
        setValue(Day, negate * parseInt(fBuffer, fStart,end));
        fStart = end+1;
        designator = true;
    }

    if ( (fEnd == endDate) &&   // 'T' absent
        (fStart != fEnd)   )   // something after Day
        throw XQUERY_EXCEPTION2(FORG0001, "invalid duration");

    if ( fEnd != endDate ) // 'T' present
    {
        //scan hours, minutes, seconds
        // skip 'T' first
        res = indexOf(fBuffer, ++fStart, fEnd, DURATION_H, end);
        if ( res != NOT_FOUND )
        {
            //scan hours
            if (fStart == end)
                throw XQUERY_EXCEPTION2(FORG0001, "invalid duration");
            setValue(Hour, negate * parseInt(fBuffer, fStart, end));
            fStart = end+1;
            designator = true;
        }

        res = indexOf(fBuffer, fStart, fEnd, DURATION_M, end);
        if ( res != NOT_FOUND )
        {
            //scan min
            if (fStart == end)
                throw XQUERY_EXCEPTION2(FORG0001, "invalid duration");
            setValue(Minute, negate * parseInt(fBuffer, fStart, end));
            fStart = end+1;
            designator = true;
        }

        res = indexOf(fBuffer, fStart, fEnd, DURATION_S, end);
        if ( res != NOT_FOUND )
        {
            //scan seconds
            size_t mlsec;
            res = indexOf (fBuffer, fStart, end, MILISECOND_SEPARATOR, mlsec);

            /***
            * Schema Errata: E2-23
            * at least one digit must follow the decimal point if it appears. 
            * That is, the value of the seconds component must conform 
            * to the following pattern: [0-9]+(.[0-9]+)? 
            */
            if ( res != NOT_FOUND )
            {
                /***
                * make usure there is something after the '.' and before the end.
                */
                if ( mlsec+1 >= end )
                    throw XQUERY_EXCEPTION2(FORG0001, "invalid milisecond value in duration");

                setValue(Second, negate * parseInt(fBuffer, fStart, mlsec));
                int rawMilis = parseInt(fBuffer, mlsec+1, end);
                size_t miliSecondLen = end-mlsec-1;
                setValue(MiliSecond, negate * adjustMiliseconds(rawMilis, miliSecondLen, (size_t)DUR_MILISECOND_DIGITS));
            }
            else
            {
                if (fStart==end) throw XQUERY_EXCEPTION2(FORG0001, "invalid duration");
                setValue(Second, negate * parseInt(fBuffer, fStart,end));
            }

            fStart = end+1;
            designator = true;
        }

        // no additional data should appear after last item
        // P1Y1M1DT is illigal value as well
        if ( (fStart != fEnd) ||
            fBuffer[--fStart] == DATETIME_SEPARATOR )
            throw XQUERY_EXCEPTION2(FORG0001, "no time after time separator in duration");
    }

    if ( !designator )
        throw XQUERY_EXCEPTION2(FORG0001, "invalid duration");

    normalize();

}

//
// Parse xs_yearMonth duration
// PnYnM: -P1Y2M
//
// [-]{'P'{[n'Y'][n'M']}}
//
//
void XMLDateTime::parseYearMonthDuration(const char* fBuffer)
{
    setValue(Type, xs_yearMonthDuration);
    size_t fStart=0, fEnd = strlen(fBuffer);
    handleWhitespace(fBuffer, fStart, fEnd);
    // must start with '-' or 'P'
    //
    char c = fBuffer[fStart++];
    if ( (c != DURATION_STARTER) &&
        (c != '-')            )
        throw XQUERY_EXCEPTION2(FORG0001, "invalid xs_yearMonthDuration start");

    // 'P' must ALWAYS be present in either case
    if ( (c == '-') &&
        (fBuffer[fStart++]!= DURATION_STARTER ))
        throw XQUERY_EXCEPTION2(FORG0001, "invalid xs_yearMonthDuration");

    // java code
    //date[utc]=(c=='-')?'-':0;
    //fValue[utc] = UTC_STD;
    setValue(utc, (fBuffer[0] == '-'? UTC_NEG : UTC_POS));

    int negate = ( fBuffer[0] == '-'? -1 : 1);

    //
    // No negative value is allowed after 'P'
    //
    // eg P-1234, invalid
    //
    size_t temp;
    if (indexOf(fBuffer, fStart, fEnd, '-', temp) != NOT_FOUND)
        throw XQUERY_EXCEPTION2(FORG0001, "invalid xs_yearMonthDuration");

    //at least one number and designator must be seen after P
    bool designator = false;

    //find 'Y'
    size_t end;
    char* res = indexOf(fBuffer, fStart, fEnd, DURATION_Y, end);
    if ( res != NOT_FOUND )
    {
        //scan year
        if (fStart == fEnd)
            throw XQUERY_EXCEPTION2(FORG0001, "invalid xs_yearMonthDuration");
        setValue(CentYear, negate * parseInt(fBuffer, fStart, end));
        fStart = end+1;
        designator = true;
    }

    res = indexOf(fBuffer, fStart, fEnd, DURATION_M, end);
    if ( res != NOT_FOUND )
    {
        //scan month
        if (fStart == fEnd)
            throw XQUERY_EXCEPTION2(FORG0001, "invalid xs_yearMonthDuration");
        setValue(Month, negate * parseInt(fBuffer, fStart, end));
        fStart = end+1;
        designator = true;
    }

    if ( !designator )
        throw XQUERY_EXCEPTION2(FORG0001, "invalid xs_yearMonthDuration");

    if (fStart != fEnd)
        throw XQUERY_EXCEPTION2(FORG0001, "invalid xs_yearMonthDuration");

    normalize();
}

// xs_dayTimeDuration
// PnDTnHnMnS: -P3DT10H30M
//
// [-]{'P'{[n'D']['T'][n'H'][n'M'][n'S']}}
//
//  Note: the n above shall be >= 0
//        if no time element found, 'T' shall be absent
//
void XMLDateTime::parseDayTimeDuration(const char* fBuffer)
{
    setValue(Type, xs_dayTimeDuration);
    size_t fStart=0, fEnd = strlen(fBuffer);
    handleWhitespace(fBuffer, fStart, fEnd);
    // must start with '-' or 'P'
    //
    char c = fBuffer[fStart++];
    if ( (c != DURATION_STARTER) &&
        (c != '-')            )
        throw XQUERY_EXCEPTION2(FORG0001, "invalid xs_dayTimeDuration start");

    // 'P' must ALWAYS be present in either case
    if ( (c == '-') &&
        (fBuffer[fStart++]!= DURATION_STARTER ))
        throw XQUERY_EXCEPTION2(FORG0001, "invalid xs_dayTimeDuration");

    setValue(utc, (fBuffer[0] == '-'? UTC_NEG : UTC_POS));

    int negate = ( fBuffer[0] == '-'? -1 : 1);

    //
    // No negative value is allowed after 'P'
    //
    // eg P-1234, invalid
    //
    size_t temp;
    if (indexOf(fBuffer, fStart, fEnd, '-', temp) != NOT_FOUND)
        throw XQUERY_EXCEPTION2(FORG0001, "invalid duration");

    //at least one number and designator must be seen after P
    bool designator = false;

    size_t endDate;
    char* res = indexOf(fBuffer, fStart, fEnd, DATETIME_SEPARATOR, endDate);
    if ( res == NOT_FOUND )
    {
        endDate = fEnd;  // 'T' absent
    }

    size_t end;
    res = indexOf(fBuffer, fStart, endDate, DURATION_D, end);
    if ( res != NOT_FOUND )
    {
        //scan day
        if (fStart == end)
            throw XQUERY_EXCEPTION2(FORG0001, "invalid xs_dayTimeDuration");
        setValue(Day, negate * parseInt(fBuffer, fStart,end));
        fStart = end+1;
        designator = true;
    }

    if ( (fEnd == endDate) &&   // 'T' absent
         (fStart != fEnd) )     // something after Day
        throw XQUERY_EXCEPTION2(FORG0001, "invalid xs_dayTimeDuration");

    if ( fEnd != endDate ) // 'T' present
    {
        //scan hours, minutes, seconds
        //

        // skip 'T' first
        res = indexOf(fBuffer, ++fStart, fEnd, DURATION_H, end);
        if ( res != NOT_FOUND )
        {
            //scan hours
            if (fStart == end)
                throw XQUERY_EXCEPTION2(FORG0001, "invalid xs_dayTimeDuration");
            setValue(Hour, negate * parseInt(fBuffer, fStart, end));
            fStart = end+1;
            designator = true;
        }

        res = indexOf(fBuffer, fStart, fEnd, DURATION_M, end);
        if ( res != NOT_FOUND )
        {
            //scan min
            if (fStart == end)
                throw XQUERY_EXCEPTION2(FORG0001, "invalid xs_dayTimeDuration");
            setValue(Minute, negate * parseInt(fBuffer, fStart, end));
            fStart = end+1;
            designator = true;
        }

        res = indexOf(fBuffer, fStart, fEnd, DURATION_S, end);
        if ( res != NOT_FOUND )
        {
            //scan seconds
            size_t mlsec;
            res = indexOf (fBuffer, fStart, end, MILISECOND_SEPARATOR, mlsec);

            /***
            * Schema Errata: E2-23
            * at least one digit must follow the decimal point if it appears. 
            * That is, the value of the seconds component must conform 
            * to the following pattern: [0-9]+(.[0-9]+)? 
            */
            if ( res != NOT_FOUND )
            {
                /***
                * make usure there is something after the '.' and before the end.
                */
                if ( mlsec+1 >= end )
                    throw XQUERY_EXCEPTION2(FORG0001, "invalid milisecond value in xs_dayTimeDuration");

                setValue(Second, negate * parseInt(fBuffer, fStart, mlsec));
                int rawMilis = parseInt(fBuffer, mlsec+1, end);
                size_t miliSecondLen = end-mlsec-1;
                setValue(MiliSecond, negate * adjustMiliseconds(rawMilis, miliSecondLen, (size_t)DUR_MILISECOND_DIGITS));
            }
            else
            {
                if (fStart==end) throw XQUERY_EXCEPTION2(FORG0001, "invalid xs_dayTimeDuration");
                setValue(Second, negate * parseInt(fBuffer, fStart,end));
            }

            fStart = end+1;
            designator = true;
        }

        // no additional data should appear after last item
        // P1Y1M1DT is illigal value as well
        if ( (fStart != fEnd) ||
            fBuffer[--fStart] == DATETIME_SEPARATOR )
            throw XQUERY_EXCEPTION2(FORG0001, "no time after time separator in xs_dayTimeDuration");
    }

    if ( !designator )
        throw XQUERY_EXCEPTION2(FORG0001, "invalid xs_dayTimeDuration");

    normalize();
}


// ---------------------------------------------------------------------------
//  Scanners
// ---------------------------------------------------------------------------

//
// [-]{CCYY-MM-DD}
//
// Note: CCYY could be more than 4 digits
//       Assuming fStart point to the beginning of the Date Section
//       fStart updated to point to the position right AFTER the second 'D'
//       Since the lenght of CCYY might be variable, we can't check format upfront
//
void XMLDateTime::getDate(const char* fBuffer, size_t& fStart, size_t& fEnd)
{
    // Ensure enough chars in buffer
    if ( (fStart+YMD_MIN_SIZE) > fEnd)
        throw XQUERY_EXCEPTION2(FORG0001, "incomplete date");

    getYearMonth(fBuffer, fStart, fEnd);    // Scan YearMonth and
    // fStart point to the next '-'
    if (fBuffer[fStart++] != DATE_SEPARATOR)
        throw XQUERY_EXCEPTION2(FORG0001, "invalid date, CCYY-MM must be followed by a '-' sign");

    setValue(Day, parseInt(fBuffer, fStart, fStart+2));
    fStart += 2 ;  //fStart points right after the Day
}

//
// hh:mm:ss[.msssss]['Z']
// hh:mm:ss[.msssss][['+'|'-']hh:mm]
// 012345678
//
// Note: Assuming fStart point to the beginning of the Time Section
//       fStart updated to point to the position right AFTER the second 's'
//                                                  or ms if any
//
void XMLDateTime::getTime(const char* fBuffer, size_t& fStart, size_t& fEnd)
{
    // Ensure enough chars in buffer
    if ( (fStart+TIME_MIN_SIZE) > fEnd)
        throw XQUERY_EXCEPTION2(FORG0001, "incomplete time");
    //"Imcomplete Time Format"

    // check (fixed) format first
    if ((fBuffer[fStart + 2] != TIME_SEPARATOR) ||
        (fBuffer[fStart + 5] != TIME_SEPARATOR)  )
        throw XQUERY_EXCEPTION2(FORG0001, "invalid time format");
    //("Error in parsing time" );

    //
    // get hours, minute and second
    //
    setValue(Hour, parseInt(fBuffer, fStart + 0, fStart + 2));
    setValue(Minute, parseInt(fBuffer, fStart + 3, fStart + 5));
    setValue(Second, parseInt(fBuffer, fStart + 6, fStart + 8));
    fStart += 8;

    // to see if any ms and/or utc part after that
    if (fStart >= fEnd)
        return;

    //find UTC sign if any
    size_t sign;
    char* res = findUTCSign(fBuffer, fStart, fEnd, sign);

    //parse miliseconds
    if ( fBuffer[fStart] == MILISECOND_SEPARATOR )
    {
        size_t milisec = fStart;
        fStart++;   // skip the '.'
        // make sure we have some thing between the '.' and fEnd
        if (fStart >= fEnd)
            throw XQUERY_EXCEPTION2(FORG0001, "invalid time: no digits after the '.'");
        //("ms shall be present once '.' is present" );

        int rawMilis;
        size_t miliSecondLen;

        if ( res == NOT_FOUND )
        {
            rawMilis = parseInt(fBuffer, fStart, fEnd);  //get ms between '.' and fEnd
            miliSecondLen = fEnd-fStart;
            fStart = fEnd;
        }
        else
        {
            rawMilis = parseInt(fBuffer, fStart, sign);  //get ms between UTC sign and fEnd
            miliSecondLen = sign-fStart;
        }

        rawMilis = adjustMiliseconds(rawMilis, miliSecondLen, (size_t)DT_MILISECOND_DIGITS);
        setValue(MiliSecond, rawMilis);
    }
    else if(res == NOT_FOUND || sign == 0 || sign != fStart)
        throw XQUERY_EXCEPTION2(FORG0001, "seconds have more than 2 digits");
    
    //parse UTC time zone (hh:mm)
    if ( res != NOT_FOUND && sign > 0 ) {
        getTimeZone(fBuffer,sign,fEnd);
    }
}

//
// [-]{CCYY-MM}
//
// Note: CCYY could be more than 4 digits
//       fStart updated to point AFTER the second 'M' (probably meet the fEnd)
//
void XMLDateTime::getYearMonth(const char* fBuffer, size_t& fStart, size_t& fEnd)
{

    // Ensure enough chars in buffer
    if ( (fStart+YMONTH_MIN_SIZE) > fEnd)
        throw XQUERY_EXCEPTION2(FORG0001, "incomplete year month format");
    //"Imcomplete YearMonth Format";

    // skip the first leading '-'
    size_t start = ( fBuffer[fStart] == '-') ? fStart + 1 : fStart;

    //
    // search for year separator '-'
    //
    size_t yearSeparator;
    char* res = indexOf(fBuffer, start, fEnd, DATE_SEPARATOR, yearSeparator);
    if ( res == NOT_FOUND)
        throw XQUERY_EXCEPTION2(FORG0001, "year separator is missing or misplaced");
    //("Year separator is missing or misplaced");

    setValue(CentYear, parseIntYear(fBuffer,fStart,yearSeparator));
    fStart = yearSeparator + 1;  //skip the '-' and point to the first M

    //
    //gonna check we have enough byte for month
    //
    if ((fStart + 2) > fEnd )
        throw XQUERY_EXCEPTION2(FORG0001, "no month specified");
    //"no month in buffer"

    setValue(Month, parseInt(fBuffer,fStart, yearSeparator + 3));

    fStart += 2;  //fStart points right after the MONTH
}

void XMLDateTime::parseTimeZone(const char* fBuffer, size_t& fStart, size_t& fEnd)
{
    //fStart points right after the date   	 
    if ( fStart < fEnd ) {
        size_t pos;
        char* res = indexOf(UTC_SET, fBuffer[fStart], pos);
        if ( res == NOT_FOUND ) 
            throw XQUERY_EXCEPTION2(FORG0001, "no UTC sign in the timezone");
        else { 
            setValue(utc, (int)(pos+1));
            getTimeZone(fBuffer,fStart,fEnd);   		
        }
    }
}

//
// 'Z'
// ['+'|'-']hh:mm
//
// Note: Assuming fStart points to the beginning of TimeZone section
//       fStart updated to meet fEnd
//
void XMLDateTime::getTimeZone(const char* fBuffer, const size_t& sign, size_t& fEnd)
{

    if ( fBuffer[sign] == UTC_STD_CHAR )
    {
        if ((sign + 1) != fEnd )
            throw XQUERY_EXCEPTION2(FORG0001, "extra characters after Z in timezone");
        //"Error in parsing time zone");

        return;	
    }

    //
    // otherwise, it has to be this format
    // '[+|-]'hh:mm
    //    1   23456 7
    //   sign      fEnd
    //
    if ( ( ( sign + TIMEZONE_SIZE + 1) != fEnd )      ||
        ( fBuffer[sign + 3] != TIMEZONE_SEPARATOR ) )
        throw XQUERY_EXCEPTION2(FORG0001, "error parsing time zone");
    //("Error in parsing time zone");

    setValue(tz_hh, parseInt(fBuffer,sign+1, sign+3));
    setValue(tz_mm, parseInt(fBuffer,sign+4, fEnd));

    return;
}

// ---------------------------------------------------------------------------
//  Validator and normalizer
// ---------------------------------------------------------------------------

void XMLDateTime::normalize()
{
    if (getValue(Type) == xs_duration ||
        getValue(Type) == xs_yearMonthDuration ||
        getValue(Type) == xs_dayTimeDuration)
        normalizeDuration();
    else
        normalizeDateTime();
}

/**
* If timezone present - normalize dateTime  [E Adding durations to dateTimes]
*
* @param date   CCYY-MM-DDThh:mm:ss+03
* @return CCYY-MM-DDThh:mm:ssZ
*/
void XMLDateTime::normalizeDateTime()
{
    int negate = (getValue(utc) == UTC_POS)? -1: 1;
    int temp;
    int carry;

    // we normalize a duration so could have 200M...
    //update months (may be modified additionaly below)
    temp = getValue(Month);
    setValue(Month, modulo(temp, 1, 13));
    carry = fQuotient(temp, 1, 13);
    if (getValue(Month) <= 0) {
        setValue(Month, getValue(Month) + 12);
        carry--;
    }

    //add years (may be modified additionaly below)
    setValue(CentYear, getValue(CentYear) + carry);

    // add mins
    temp = getValue(Minute) + negate * getValue(tz_mm);
    carry = fQuotient(temp, 60);
    setValue(Minute, mod(temp, 60, carry));
    if (getValue(Minute) < 0) {
        setValue(Minute, getValue(Minute)+ 60);
        carry--;
    }

    //add hours
    temp = getValue(Hour) + negate * getValue(tz_hh) + carry;
    carry = fQuotient(temp, 24);
    setValue(Hour, mod(temp, 24, carry));
    if (getValue(Hour) < 0) {
        setValue(Hour, getValue(Hour) + 24);
        carry--;
    }

    // Special case for xs:date
    if (getValue(Type) == xs_date && (getValue(Hour) != 0 || getValue(Minute) != 0 ))
    {
        negate = (getValue(Hour)<0 || getValue(Minute)<0) ? -1 : 1;

        setValue(utc, negate==1 ? UTC_NEG : UTC_POS );
        setValue(tz_hh, negate*getValue(Hour));
        setValue(tz_mm, negate*getValue(Minute));
        setValue(Hour, 0);
        setValue(Minute, 0);
    }

    if (getValue(Type) != xs_time)
        setValue(Day, getValue(Day) + carry);   

    if (getValue(Type) != xs_time)
        while (1)
        {
            temp = maxDayInMonthFor(getValue(CentYear), getValue(Month));
            if (getValue(Day) < 1)
            {
                setValue(Day, getValue(Day) + maxDayInMonthFor(getValue(CentYear), getValue(Month) - 1));
                carry = -1;
            }
            else if ( getValue(Day) > temp )
            {
                setValue(Day, getValue(Day) - temp);
                carry = 1;
            }
            else
            {
                break;
            }

            temp = getValue(Month) + carry;
            setValue(Month, modulo(temp, 1, 13));
            if (getValue(Month) <=0) {
                setValue(Month, getValue(Month)+ 12);
                setValue(CentYear, getValue(CentYear) - 1);
            }
            setValue(CentYear, getValue(CentYear) + fQuotient(temp, 1, 13));
        }

        if (getValue(Type) != xs_date)
        {
            setValue(utc, UTC_STD);
            setValue(tz_hh, 0);
            setValue(tz_mm, 0);
        }

        return;
}


/**
* Weak normalization of dateTimes, always performed after parsing
*
*/
void XMLDateTime::normalizeDateTimeWeak()
{
    int temp;
    int carry;

    //normalize hours: the case of 24 hours
    if (getValue(Hour) == 24)
    {
        setValue(Hour, 0);
        setValue(Day, getValue(Day) + 1);   
    }

    while (1)
    {
        temp = maxDayInMonthFor(getValue(CentYear), getValue(Month));
        if (getValue(Day) < 1)
        {
            setValue(Day, getValue(Day) + maxDayInMonthFor(getValue(CentYear), getValue(Month) - 1));
            carry = -1;
        }
        else if ( getValue(Day) > temp )
        {
            setValue(Day, getValue(Day) - temp);
            carry = 1;
        }
        else
        {
            break;
        }

        temp = getValue(Month) + carry;
        setValue(Month, modulo(temp, 1, 13));
        if (getValue(Month) <=0) {
            setValue(Month, getValue(Month)+ 12);
            setValue(CentYear, getValue(CentYear) - 1);
        }
        setValue(CentYear, getValue(CentYear) + fQuotient(temp, 1, 13));
    }

    return;
}

/**
* If timezone present - normalize dateTime  [E Adding durations to dateTimes]
*
* @param date   CCYY-MM-DDThh:mm:ss+03
* @return CCYY-MM-DDThh:mm:ssZ
*/
void XMLDateTime::normalizeDuration()
{

    int negate = (getValue(utc) == UTC_NEG)? -1: 1;
    int temp;
    int carry;

    // add seconds
    temp = negate * getValue(Second);
    carry = fQuotient(temp, 60);
    setValue(Second, negate * mod(temp, 60, carry));
    if (negate * getValue(Second) < 0) {
        setValue(Second, getValue(Second)+ 60*negate);
        carry--;
    }

    // add mins
    temp = negate * getValue(Minute) + carry;
    carry = fQuotient(temp, 60);
    setValue(Minute, negate * mod(temp, 60, carry));
    if (negate * getValue(Minute) < 0) {
        setValue(Minute, getValue(Minute)+ 60*negate);
        carry--;
    }

    //add hours
    temp = negate * getValue(Hour) + carry;
    carry = fQuotient(temp, 24);
    setValue(Hour, negate * mod(temp, 24, carry));
    if (negate * getValue(Hour) < 0) {
        setValue(Hour, getValue(Hour) + 24*negate);
        carry--;
    }

    setValue(Day, getValue(Day) + carry*negate);

    //add months
    temp = negate * getValue(Month);
    carry = fQuotient(temp, 12);
    setValue(Month, negate * mod(temp, 12, carry));

    if (negate * getValue(Month)< 0){
        setValue(Month, getValue(Month) + 12*negate);
        carry--;
    }

    //add years
    setValue(CentYear, getValue(CentYear) + carry*negate);

    // For the special case, where all values are zero, but the sign is negative, adjust the sign to positive
    if (getValue(MiliSecond) == 0 && getValue(Second) == 0 && getValue(Minute) == 0 && getValue(Hour) == 0 &&	
        getValue(Day) == 0 && getValue(Month) == 0 && getValue(CentYear) == 0 )
        setValue(utc, UTC_POS);

    return;
}

void XMLDateTime::validateDateTime() const
{

    //REVISIT: should we throw an exception for not valid dates
    //          or reporting an error message should be sufficient?
    if (( getValue(Type)==xs_date  || getValue(Type)==xs_dateTime ||
          getValue(Type)==xs_gYear || getValue(Type)==xs_gYearMonth )
          && getValue(CentYear) == 0 )
        throw XQUERY_EXCEPTION2(FORG0001, "0000 is an illegal value for year");
    //"The year \"0000\" is an illegal year value");

    if ((getValue(Type)==xs_date || getValue(Type)==xs_dateTime ||
        getValue(Type)==xs_gMonth || getValue(Type)==xs_gYearMonth || getValue(Type) == xs_gMonthDay)
        && ( getValue(Month) < 1  || getValue(Month) > 12  ))
        throw XQUERY_EXCEPTION2(FORG0001, "invalid month value, must be between 1 and 12");
    //"The month must have values 1 to 12");

    //validate days
    if ((getValue(Type)==xs_date || getValue(Type)==xs_dateTime ||
        getValue(Type)==xs_gMonthDay || getValue(Type)==xs_gDay)
        && ( getValue(Day) > maxDayInMonthFor( getValue(CentYear), getValue(Month))
        || getValue(Day) == 0 ))
        throw XQUERY_EXCEPTION2(FORG0001, "invalid day value");
    //"The day must have values 1 to 31");

    //validate hours
    if ((getValue(Hour) < 0)  ||
        (getValue(Hour) > 24) ||
        ((getValue(Hour) == 24) && ((getValue(Minute) !=0) ||
        (getValue(Second) !=0) ||
        (getValue(MiliSecond) !=0))))
        throw XQUERY_EXCEPTION2(FORG0001, "invalid hour, values must be between 0 and 23");
    //("Hour must have values 0-23");

    //validate minutes
    if ( getValue(Minute) < 0 ||
         getValue(Minute) > 59 )
        throw XQUERY_EXCEPTION2(FORG0001,"invalid minutes, values must be between 0 and 59");
    //"Minute must have values 0-59");

    //validate seconds
    if ( getValue(Second) < 0 ||
         getValue(Second) >= 60 )
        throw XQUERY_EXCEPTION2(FORG0001,"invalid seconds, values must be between 0 and 59");
    //"Second must have values 0-59");

    //validate time-zone hours
    if ( (abs(getValue(tz_hh)) > 14) ||
        ((abs(getValue(tz_hh)) == 14) && (getValue(tz_mm)!= 0)) )
        throw XQUERY_EXCEPTION2(FORG0001, "invalid timezone hours, values must be between -14 and 14");
    //"Time zone should have range -14..+14");

    //validate time-zone minutes
    if ( abs(getValue(tz_mm)) > 59 )
        throw XQUERY_EXCEPTION2(FORG0001, "invalid timezone minutes, values must be between 0 and 59");
    //("Minute must have values 0-59");
}

// -----------------------------------------------------------------------
// locator and converter
// -----------------------------------------------------------------------
char* XMLDateTime::indexOf(const char* fBuffer,
                           const size_t start,
                           const size_t end,
                           const char ch,
                           size_t& pos) const
{
    for ( size_t i = start; i < end; i++ )
    {
        if ( fBuffer[i] == ch ) {
            pos = i;
            return (char*)(fBuffer+pos);
        }
    }
    return NOT_FOUND;
}

char* XMLDateTime::indexOf(const char* array,
                           const char ch,
                           size_t& pos) const
{
    for (size_t i=0; array[i] != 0; i++)
    {
        if (array[i] == ch) {
            pos = i;
            return (char*)(array+pos);
        }
    }
    return NOT_FOUND;
}

char* XMLDateTime::findUTCSign (const char* fBuffer,
                                const size_t start,
                                const size_t fEnd,
                                size_t& pos)
{
    char* res;
    size_t subpos;
    for (size_t index = start; index < fEnd; index++ )
    {
        res = indexOf(UTC_SET, fBuffer[index], subpos);
        if ( res != NOT_FOUND )
        {
            /* UTC_SET is small enough for subpos can be stored in int */
            setValue(utc, (int)(subpos+1));
            pos = index;
            return (char*)(fBuffer+pos);
        }
    }
    return NOT_FOUND;
}

//
// Note:
//    start: starting point in fBuffer
//    end:   ending point in fBuffer (exclusive)
//    fStart NOT updated
//
int XMLDateTime::parseInt(const char* fBuffer, const size_t start, const size_t end) const
{
    int retVal = 0;
    for (size_t i=start; i < end; i++) {


        if (fBuffer[i] < '0' || fBuffer[i] > '9')
        {
            char errString[100];
            sprintf(errString, "invalid numeric character '%c' in dateTime or duration", fBuffer[i]);
            throw XQUERY_EXCEPTION2(FORG0001, errString);
        }

        retVal = (retVal * 10) + (unsigned int) (fBuffer[i] - '0');
    }

    return retVal;
}

//
// [-]CCYY
//
// Note: start from fStart
//       end (exclusive)
//       fStart NOT updated
//
int XMLDateTime::parseIntYear(const char* fBuffer, const size_t fStart, const size_t end) const
{
    // skip the first leading '-'
    size_t start = ( fBuffer[fStart] == '-') ? fStart + 1 : fStart;

    if (end <= start)
        throw XQUERY_EXCEPTION2(FORG0001, "invalid year, year must be in 'CCYY' format");
    
    size_t length = end - start;
    
    if (length < 4)
        throw XQUERY_EXCEPTION2(FORG0001, "invalid year, year must be in 'CCYY' format");
    //"Year must have 'CCYY' format");
    else if (length > 4 && fBuffer[start] == '0')
        throw XQUERY_EXCEPTION2(FORG0001, "leading zeros in year are only allowed if otherwise the year would have fewer than four digits");
    //"Leading zeros are required if the year value would otherwise have fewer than four digits;
    // otherwise they are forbidden");

    bool negative = (fBuffer[fStart] == '-');
    int  yearVal = parseInt(fBuffer, (negative ? fStart+1 : fStart), end);
    return ( negative ? (-1) * yearVal : yearVal );
}

/***
* E2-41
*
*  3.2.7.2 Canonical representation
* 
*  Except for trailing fractional zero digits in the seconds representation, 
*  '24:00:00' time representations, and timezone (for timezoned values), 
*  the mapping from literals to values is one-to-one. Where there is more 
*  than one possible representation, the canonical representation is as follows: 
*  redundant trailing zero digits in fractional-second literals are prohibited. 
*  An hour representation of '24' is prohibited. Timezoned values are canonically
*  represented by appending 'Z' to the nontimezoned representation. (All 
*  timezoned dateTime values are UTC.) 
*
*  .'24:00:00' -> '00:00:00'
*  .milisecond: trailing zeros removed
*  .'Z'
*
***/
void XMLDateTime::printDateTime(char* buf) const
{
    int utcSize = (getValue(utc) == UTC_UNKNOWN) ? 0 : 1;

    char* retPtr = buf;

    // (-?) cc+yy-mm-dd'T'hh:mm:ss'Z'    ('.'s+)?
    //      2+  8       1      8   1
    //
    fillYearString(retPtr, getValue(CentYear));
    *retPtr++ = DATE_SEPARATOR;
    fillString(retPtr, getValue(Month), 2);
    *retPtr++ = DATE_SEPARATOR;
    fillString(retPtr, getValue(Day), 2);
    *retPtr++ = DATETIME_SEPARATOR;

    fillString(retPtr, getValue(Hour), 2);
    *retPtr++ = TIME_SEPARATOR;
    fillString(retPtr, getValue(Minute), 2);
    *retPtr++ = TIME_SEPARATOR;
    fillString(retPtr, getValue(Second), 2);

    if (getValue(MiliSecond) != 0)
    {
        *retPtr++ = '.';
        fillMilisString(retPtr, getValue(MiliSecond), DT_MILISECOND_DIGITS);
    }

    if (utcSize) {
        if (getValue(utc) != UTC_STD && (getValue(tz_hh) != 0 || getValue(tz_mm) != 0))
        {
            *retPtr++ = UTC_SET[getValue(utc)-1];
            fillString(retPtr, getValue(tz_hh), 2);
            *retPtr++ = TIME_SEPARATOR;
            fillString(retPtr, getValue(tz_mm), 2);
        }
        else {
            *retPtr++ = UTC_STD_CHAR;
        }
    }
    *retPtr = 0;
}

/***
* E2-41
*
*  3.2.9.2 Canonical representation
* 
* Given a member of the date value space, the date 
* portion of the canonical representation (the entire 
* representation for nontimezoned values, and all but
* the timezone representation for timezoned values) 
* is always the date portion of the dateTime canonical
* representation of the interval midpoint (the 
* dateTime representation, truncated on the right
* to eliminate 'T' and all following characters). 
* For timezoned values, append the canonical 
* representation of the recoverable timezone. 
*
***/
void XMLDateTime::printDate(char* buf) const
{    
    /*
    * Case Date               Actual Value    Canonical Value
    *    1 yyyy-mm-dd         yyyy-mm-dd          yyyy-mm-dd
    *    2 yyyy-mm-ddZ        yyyy-mm-ddT00:00Z   yyyy-mm-ddZ
    *    3 yyyy-mm-dd+00:00   yyyy-mm-ddT00:00Z   yyyy-mm-ddZ
    *    4 yyyy-mm-dd+00:01   YYYY-MM-DCT23:59Z   yyyy-mm-dd+00:01              
    *    5 yyyy-mm-dd+12:00   YYYY-MM-DCT12:00Z   yyyy-mm-dd+12:00    
    *    6 yyyy-mm-dd+12:01   YYYY-MM-DCT11:59Z   YYYY-MM-DC-11:59 
    *    7 yyyy-mm-dd+14:00   YYYY-MM-DCT10:00Z   YYYY-MM-DC-10:00 
    *    8 yyyy-mm-dd-00:00   yyyy-mm-ddT00:00Z   yyyy-mm-ddZ
    *    9 yyyy-mm-dd-00:01   yyyy-mm-ddT00:01Z   yyyy-mm-dd-00:01 
    *   11 yyyy-mm-dd-11:59   yyyy-mm-ddT11:59Z   YYYY-MM-DD-11:59
    *   10 yyyy-mm-dd-12:00   yyyy-mm-ddT12:00Z   YYYY-MM-DD+12:00      
    *   12 yyyy-mm-dd-14:00   yyyy-mm-ddT14:00Z   YYYY-MM-DD+10:00
    */
    int utcSize = (getValue(utc) == UTC_UNKNOWN) ? 0 : 1;
    // YYYY-MM-DD  + chNull 
    // 1234567890  + 1
    int memLength = 10 + 1 + utcSize;

    if (getValue(tz_hh) != 0 || getValue(tz_mm) != 0) {
        // YYYY-MM-DD+HH:MM  (utcSize will be 1 so drop that)
        // 1234567890123456
        memLength += 5; // 6 - 1 for utcSize
    }

    char* retPtr = buf;

    if (getValue(Hour) < 12 || getValue(Type) != xs_date) {

        if (getValue(Type)==xs_gYear)
            fillYearString(retPtr, getValue(CentYear));
        else if (getValue(Type)==xs_gMonth)
        {
            *retPtr++ = DATE_SEPARATOR;
            *retPtr++ = DATE_SEPARATOR;
            fillString(retPtr, getValue(Month), 2);
        }
        else if (getValue(Type)==xs_gDay)
        {
            *retPtr++ = DATE_SEPARATOR;
            *retPtr++ = DATE_SEPARATOR;
            *retPtr++ = DATE_SEPARATOR;
            fillString(retPtr, getValue(Day), 2);        
        }
        else if (getValue(Type)==xs_gYearMonth)
        {
            fillYearString(retPtr, getValue(CentYear));
            *retPtr++ = DATE_SEPARATOR;
            fillString(retPtr, getValue(Month), 2);
        }
        else if (getValue(Type)==xs_gMonthDay)
        {
            *retPtr++ = DATE_SEPARATOR;
            *retPtr++ = DATE_SEPARATOR;
            fillString(retPtr, getValue(Month), 2);
            *retPtr++ = DATE_SEPARATOR;
            fillString(retPtr, getValue(Day), 2);        
        }
        else	
        {	      
            fillYearString(retPtr, getValue(CentYear));
            *retPtr++ = DATE_SEPARATOR;
            fillString(retPtr, getValue(Month), 2);
            *retPtr++ = DATE_SEPARATOR;
            fillString(retPtr, getValue(Day), 2);        
        }

        if (utcSize) {
            if (getValue(utc) != UTC_STD && (getValue(tz_hh) != 0 || getValue(tz_mm) != 0))
            {
                *retPtr++ = UTC_SET[getValue(utc)-1];
                fillString(retPtr, getValue(tz_hh), 2);
                *retPtr++ = TIME_SEPARATOR;
                fillString(retPtr, getValue(tz_mm), 2);
            }
            else {
                *retPtr++ = UTC_STD_CHAR;
            }
        }
        *retPtr = 0;    
    }
    else {
        /*
        * Need to reconvert things to get a recoverable time zone between
        * +12:00 and -11:59
        */
        int carry;
        int minute;
        int hour;
        int day;
        int month;
        int year;
        if (getValue(Minute) == 0) {
            minute = 0;
            carry = 0;
        }
        else {
            minute = 60 - getValue(Minute);
            carry = 1;
        }
        hour  = 24 - getValue(Hour) - carry;
        day   = getValue(Day) + 1;
        month = getValue(Month);
        year  = getValue(CentYear);

        while (1) {
            int temp = maxDayInMonthFor(year, month);
            if (day < 1) {
                day += maxDayInMonthFor(year, month - 1);
                carry = -1;
            }
            else if (day > temp) {
                day -= temp;
                carry = 1;
            }
            else {
                break;
            }

            temp = month + carry;
            month = modulo(temp, 1, 13);
            if (month <= 0) {
                month+= 12;
                year--;
            }
            year += fQuotient(temp, 1, 13);         
        }

        fillYearString(retPtr, year);
        *retPtr++ = DATE_SEPARATOR;
        fillString(retPtr, month, 2);
        *retPtr++ = DATE_SEPARATOR;
        fillString(retPtr, day, 2);        

        *retPtr++ = UTC_POS_CHAR;               
        fillString(retPtr, hour, 2);
        *retPtr++ = TIME_SEPARATOR;
        fillString(retPtr, minute, 2);                                
        *retPtr = 0;
    }      
}


/***
* 3.2.8 time
*
*  . either the time zone must be omitted or, 
*    if present, the time zone must be Coordinated Universal Time (UTC) indicated by a "Z".   
*
*  . Additionally, the canonical representation for midnight is 00:00:00.
*
***/
void XMLDateTime::printTime(char* buf) const
{

    int utcSize = (getValue(utc) == UTC_UNKNOWN) ? 0 : 1;

    char* retPtr = buf;

    // 'hh:mm:ss'Z'    ('.'s+)?
    //      8    1
    //

    fillString(retPtr, getValue(Hour), 2);
    if (getValue(Hour) == 24)
    {
        *(retPtr - 2) = '0';
        *(retPtr - 1) = '0';
    }
    *retPtr++ = TIME_SEPARATOR;
    fillString(retPtr, getValue(Minute), 2);
    *retPtr++ = TIME_SEPARATOR;
    fillString(retPtr, getValue(Second), 2);

    if (getValue(MiliSecond) != 0)    
    {
        *retPtr++ = '.';
        fillMilisString(retPtr, getValue(MiliSecond), DT_MILISECOND_DIGITS);
    }

    if (utcSize) {
        if (getValue(utc) != UTC_STD && ( getValue(tz_hh) != 0 || getValue(tz_mm) != 0))
        {
            *retPtr++ = UTC_SET[getValue(utc)-1];
            fillString(retPtr, getValue(tz_hh), 2);
            *retPtr++ = TIME_SEPARATOR;
            fillString(retPtr, getValue(tz_mm), 2);
        }
        else {
            *retPtr++ = UTC_STD_CHAR;
        }
    }
    *retPtr = 0;
}

void XMLDateTime::printDuration(char* buf ) const
{
    char* bufPtr = buf;
    int zeroDate=1, zeroTime=1;

    int neg = getValue(utc) == UTC_POS ? 1 : -1;

    if (neg == -1)
        *bufPtr++ = '-';

    *bufPtr++  = DURATION_STARTER;

    if (getValue(CentYear) != 0)
    {
        sprintf(bufPtr, "%d", neg*getValue(CentYear));
        bufPtr = buf + strlen(buf);
        *bufPtr++ = DURATION_Y;
        zeroDate = 0;
    }
    if (getValue(Month) != 0)
    {
        sprintf(bufPtr, "%d", neg*getValue(Month));
        bufPtr = buf + strlen(buf);
        *bufPtr++ = DURATION_M;
        zeroDate = 0;
    }
    if (getValue(Type) == xs_yearMonthDuration && zeroDate)
    {
        *bufPtr++ = '0';
        *bufPtr++ = DURATION_M;
    }

    if (getValue(Type) == xs_duration || getValue(Type) == xs_dayTimeDuration)
    {
        if (getValue(Day) != 0)
        {
            sprintf(bufPtr, "%d", neg*getValue(Day));
            bufPtr = buf + strlen(buf);
            *bufPtr++ = DURATION_D;
            zeroDate = 0;
        }
        if (getValue(Hour) != 0 || getValue(Minute) != 0 || getValue(Second) != 0 || getValue(MiliSecond) != 0)
            *bufPtr++ = DATETIME_SEPARATOR;
        else if (zeroDate)
        {
            *bufPtr++ = DATETIME_SEPARATOR;
            *bufPtr++ = '0';
            *bufPtr++ = DURATION_S;
        }

        if (getValue(Hour) != 0)
        {
            sprintf(bufPtr, "%d", neg*getValue(Hour));
            bufPtr = buf + strlen(buf);
            *bufPtr++ = DURATION_H;
            zeroTime= 0;
        }
        if (getValue(Minute) != 0)
        {
            sprintf(bufPtr, "%d", neg*getValue(Minute));
            bufPtr = buf + strlen(buf);
            *bufPtr++ = DURATION_M;
            zeroTime= 0;
        }
        if (getValue(Second) != 0 || getValue(MiliSecond)!=0)
        {
            sprintf(bufPtr, "%d", neg*getValue(Second));
            bufPtr = buf + strlen(buf);
            if (getValue(MiliSecond) != 0)
            {
                *bufPtr++ = '.';
                fillMilisString(bufPtr, neg*getValue(MiliSecond), DUR_MILISECOND_DIGITS);
            }
            *bufPtr++ = DURATION_S;
            zeroTime= 0;
        }
    }

    *bufPtr = '\0';
}

void XMLDateTime::fillString(char*& ptr, int value, size_t expLen) const
{
    char strBuffer[128];
    sprintf(strBuffer, "%d", value);
    size_t actualLen = strlen(strBuffer);
    size_t i;
    //append leading zeros
    if(expLen > actualLen) 
    {
        for (i = 0; i < expLen - actualLen; i++)
        {
            *ptr++ = '0';
        }
    }

    for (i = 0; i < actualLen; i++)
    {
        *ptr++ = strBuffer[i];
    }
}


void XMLDateTime::fillMilisString(char*& ptr, int value, unsigned short maxLen) const
{
    char strBuffer[128];
    char formatString[12];
    sprintf(formatString, "%%0%dd", (int)maxLen);
    sprintf(strBuffer, formatString, value);

    for (unsigned short i = 0; i < maxLen; i++)
    {
        if ( atoi(&strBuffer[i]) == 0 )
            break;
        *ptr++ = strBuffer[i];
    }
}

void XMLDateTime::fillYearString(char*& ptr, int value) const
{
    char strBuffer[16];
    // let's hope we get no years of 15 digits...
    sprintf(strBuffer, "%d", value);
    size_t actualLen = strlen(strBuffer);
    // don't forget that years can be negative...
    size_t negativeYear = 0;
    if(strBuffer[0] == '-')
    {
        *ptr++ = strBuffer[0];
        negativeYear = 1;
    }
    size_t i;
    //append leading zeros
    if(actualLen < 4 + negativeYear) 
    {
        for (i = 0; i < (4 + negativeYear) - actualLen; i++)
        {
            *ptr++ = '0';
        }
    }

    for (i = negativeYear; i < actualLen; i++)
    {
        *ptr++ = strBuffer[i];
    }
}

void XMLDateTime::get_string_value(char* buf) const
{
    switch( getValue(Type))
    {	
    case xs_gYearMonth:		printDate(buf); break;
    case xs_gYear:			printDate(buf); break;
    case xs_gMonthDay:		printDate(buf); break;
    case xs_gDay:			printDate(buf); break;
    case xs_gMonth:			printDate(buf); break;
    case xs_dateTime:		printDateTime(buf); break;
    case xs_date:			printDate(buf); break;
    case xs_time:			printTime(buf); break;
    case xs_duration:		printDuration(buf); break;
    case xs_dayTimeDuration:	printDuration(buf); break;
    case xs_yearMonthDuration:	printDuration(buf); break;
    default:			throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema type passed to XMLDateTime::get_string_value");
    }
}
