/*
 * Copyright 2001-2004 The Apache Software Foundation.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <stdlib.h>
#include <errno.h>
#include <math.h>

#include "sedna.h"
#include "XMLDateTime.h"

#ifndef MAX_MEM_STR_SIZE
#define MAX_MEM_STR_SIZE 100
#endif


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

static const int YMD_MIN_SIZE    = 10;   // CCYY-MM-DD
static const int YMONTH_MIN_SIZE = 7;    // CCYY_MM
static const int TIME_MIN_SIZE   = 8;    // hh:mm:ss
static const int TIMEZONE_SIZE   = 5;    // hh:mm
static const int DAY_SIZE        = 5;    // ---DD
//static const int MONTH_SIZE      = 6;    // --MM--
static const int MONTHDAY_SIZE   = 7;    // --MM-DD
static const int NOT_FOUND       = -1;

//define constants to be used in assigning default values for
//all date/time excluding duration
static const int YEAR_DEFAULT  = 2000;
static const int MONTH_DEFAULT = 01;
static const int DAY_DEFAULT   = 15;

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

//---------------------------------------------------------------------------------------
// Constructors
//---------------------------------------------------------------------------------------
XMLDateTime::XMLDateTime()
{
	counted_ptr_value = str_counted_ptr(new char[sizeof(int) * TOTAL_FIELDS]);
	string_value = counted_ptr_value.get();
	reset();
}

XMLDateTime::XMLDateTime(str_counted_ptr buf)
{
	counted_ptr_value = buf;
	string_value = buf.get();
}

XMLDateTime::XMLDateTime(char* buf)
{ string_value = buf; }


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
					newValue.setValue(MiliSecondLen, getValue(MiliSecondLen));
					break;
		case xs_dateTime:	newValue.setValue(CentYear, getValue(CentYear));
					newValue.setValue(Month, getValue(Month));
					newValue.setValue(Day, getValue(Day));
					newValue.setValue(Hour, getValue(Hour));
					newValue.setValue(Minute, getValue(Minute));
					newValue.setValue(Second, getValue(Second));
					newValue.setValue(MiliSecond, getValue(MiliSecond));
					newValue.setValue(MiliSecondLen, getValue(MiliSecondLen));
					break;
		case xs_duration:	newValue.setValue(CentYear, getValue(CentYear));
					newValue.setValue(Month, getValue(Month));
					newValue.setValue(Day, getValue(Day));
					newValue.setValue(Hour, getValue(Hour));
					newValue.setValue(Minute, getValue(Minute));
					newValue.setValue(Second, getValue(Second));
					newValue.setValue(MiliSecond, getValue(MiliSecond));
					newValue.setValue(MiliSecondLen, getValue(MiliSecondLen));
					break;
		case xdt_yearMonthDuration: 	newValue.setValue(CentYear, getValue(CentYear));
						newValue.setValue(Month, getValue(Month));
						break;
		case xdt_dayTimeDuration:	newValue.setValue(Day, getValue(Day));
						newValue.setValue(Hour, getValue(Hour));
						newValue.setValue(Minute, getValue(Minute));
						newValue.setValue(Second, getValue(Second));
						newValue.setValue(MiliSecond, getValue(MiliSecond));
						newValue.setValue(MiliSecondLen, getValue(MiliSecondLen));
						break;
	}

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
	if (dt.isDuration())
		dt.normalize();
	double milis = ( dt.getValue(MiliSecondLen) == 0 ) ? 0.0 : dt.getValue(MiliSecond)/pow(10.0,(double)dt.getValue(MiliSecondLen));
	return (double)dt.getValue(Second) + milis;
} 

XMLDateTime XMLDateTime::getTimezone() const
{ 
	XMLDateTime tz;
	tz.setValue(Type, xdt_dayTimeDuration);
	int neg = (getValue(utc) == UTC_NEG )? -1: 1;
	tz.setValue(Hour, neg * getValue(tz_hh));
	tz.setValue(Minute, neg * getValue(tz_mm));
	return tz;
}

//----------------------------------------------------------------------------------------
// Arithmetic operations on dateTimes and durations
//----------------------------------------------------------------------------------------
 
XMLDateTime addDurations(const XMLDateTime& d1, const XMLDateTime& d2)
{
    XMLDateTime newDuration;
    newDuration.setValue(XMLDateTime::Type, d1.getValue(XMLDateTime::Type));


   if (d1.getValue(XMLDateTime::Type) == xdt_yearMonthDuration)
   {
	newDuration.setValue(XMLDateTime::Month, d1.getValue(XMLDateTime::CentYear)*12 + d1.getValue(XMLDateTime::Month) + 
			d2.getValue(XMLDateTime::CentYear)*12 + d2.getValue(XMLDateTime::Month));
	newDuration.normalize();
	return newDuration;
   }
   else //must be xdt_dayTimeDuration
   {
	//add miliseconds
	int carry = 0;
	double milis1 = d1.getValue(XMLDateTime::MiliSecond) == 0 ? 0.0 :
		 d1.getValue(XMLDateTime::MiliSecond)/pow(10.0, (double)d1.getValue(XMLDateTime::MiliSecondLen));
	double milis2 = d2.getValue(XMLDateTime::MiliSecond) == 0 ? 0.0 :
		 d2.getValue(XMLDateTime::MiliSecond)/pow(10.0, (double)d2.getValue(XMLDateTime::MiliSecondLen));
	double milis = milis1 + milis2;
	if (milis>=1.0 || milis <=0.0)
	{
		if (milis>=1.0)
		{
			milis -= 1.0;
			carry = 1;
		}
		else if (milis <= 0.0)
		{
			milis += 1.0;
			carry = -1;
		}
		char tmpBuf[MAX_MEM_STR_SIZE];
		if (milis != 0.0)
			sprintf(tmpBuf, "%g", milis);
		else
			sprintf(tmpBuf, "0.0");
		newDuration.setValue(XMLDateTime::MiliSecond, atoi(&tmpBuf[2]));
		newDuration.setValue(XMLDateTime::MiliSecondLen, strlen(&tmpBuf[2]));
	}

/*
	newDuration.setValue(XMLDateTime::Second, d1.getValue(XMLDateTime::Second) + d2.getValue(XMLDateTime::Second) + carry);
	newDuration.setValue(XMLDateTime::Minute, d1.getValue(XMLDateTime::Minute) + d2.getValue(XMLDateTime::Minute));
	newDuration.setValue(XMLDateTime::Hour, d1.getValue(XMLDateTime::Hour) + d2.getValue(XMLDateTime::Hour));
	newDuration.setValue(XMLDateTime::Day, d1.getValue(XMLDateTime::Day) + d2.getValue(XMLDateTime::Day));
*/
/*
	char sbuf[1000];
	sprintf(sbuf, "d2.s = %d", d2.getValue(XMLDateTime::Second));
	throw USER_EXCEPTION2(FODT017, sbuf );
*/
 
 	newDuration.setValue(XMLDateTime::Second,
			d1.getValue(XMLDateTime::Second) + d2.getValue(XMLDateTime::Second) + carry +
			d1.getValue(XMLDateTime::Minute)*60 + d2.getValue(XMLDateTime::Minute)*60 +
			d1.getValue(XMLDateTime::Hour)*60*60 + d2.getValue(XMLDateTime::Hour)*60*60 +
			d1.getValue(XMLDateTime::Day)*60*60*24 + d2.getValue(XMLDateTime::Day)*60*60*24 );
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

	if (d.getValue(XMLDateTime::Type) == xdt_yearMonthDuration)
	{
		int months = d.getValue(XMLDateTime::CentYear)*12 + d.getValue(XMLDateTime::Month);
		double multMonths = months * v;
		int neg = multMonths > 0 ? 1 : -1;
		months = neg * (int)floor( neg * multMonths + 0.5);
		newDuration.setValue(XMLDateTime::Month, months);
		newDuration.setValue(XMLDateTime::utc, neg == 1? XMLDateTime::UTC_POS : XMLDateTime::UTC_NEG);
		newDuration.normalize();
		return newDuration;
	}
	else // must be xdt_dayTimeDuration
	{
		int miliSecond = d.getValue(XMLDateTime::MiliSecond);
		int miliSecondLen = d.getValue(XMLDateTime::MiliSecondLen);
		double milis = (miliSecond == 0)?0.0:miliSecond/pow(10.0, (double)miliSecondLen);
		long seconds = d.getValue(XMLDateTime::Second) + d.getValue(XMLDateTime::Minute)*60 
			+ d.getValue(XMLDateTime::Hour)*60*60 + d.getValue(XMLDateTime::Day)*60*60*24;
		double seconds_milis = (seconds + milis) * v;
		int neg = seconds_milis > 0 ? 1 : -1;
		newDuration.setValue(XMLDateTime::Second, (int)seconds_milis);
		char tmpBuf[MAX_MEM_STR_SIZE];
		if (seconds_milis - (int)seconds_milis != 0.0)
			sprintf(tmpBuf, "%g", neg * (seconds_milis - (int)seconds_milis));
		else
			sprintf(tmpBuf, "0.0");
		newDuration.setValue(XMLDateTime::MiliSecond, neg * atoi(&tmpBuf[2]));
		newDuration.setValue(XMLDateTime::MiliSecondLen, strlen(&tmpBuf[2]));

/*

		char sbuf[1000];
		sprintf(sbuf, "MS=%d, DIV=%g, seconds = %d, milis = %.50f, seconds_milis = %.50f, norm milis = %.50f, printed_milis = %d, mili len = %d", 
			d.getValue(XMLDateTime::MiliSecond), 
			pow((double)d.getValue(XMLDateTime::MiliSecondLen),10),
			seconds, milis, seconds_milis, seconds_milis -(int)milis, atoi(&tmpBuf[2]), strlen(&tmpBuf[2]) );
		throw USER_EXCEPTION2(FODT017, sbuf );
*/

		//newDuration.normalize();
	 	return newDuration;
	}
 }

double divideDurationByDuration(const XMLDateTime& d1, const XMLDateTime& d2)
{
	if (d1.getValue(XMLDateTime::Type) == xdt_yearMonthDuration)
	{
		int m1 = d1.getValue(XMLDateTime::CentYear)*12 + d1.getValue(XMLDateTime::Month);
		int m2 = d2.getValue(XMLDateTime::CentYear)*12 + d2.getValue(XMLDateTime::Month);
		return ((double)m1/ (double)m2);
	}
	else // must be xdt_dayTimeDuration
	{
		double milis1 = d1.getValue(XMLDateTime::MiliSecond) == 0 ? 0.0 :
		 	d1.getValue(XMLDateTime::MiliSecond)/pow(10.0, (double)d1.getValue(XMLDateTime::MiliSecondLen));
		long seconds1 = d1.getValue(XMLDateTime::Second) + d1.getValue(XMLDateTime::Minute)*60 + d1.getValue(XMLDateTime::Hour)*60*60 +
			d1.getValue(XMLDateTime::Day)*60*60*24;
		double milis2 = d2.getValue(XMLDateTime::MiliSecond) == 0 ? 0.0 :
		 	d2.getValue(XMLDateTime::MiliSecond)/pow(10.0, (double)d2.getValue(XMLDateTime::MiliSecondLen));
		long seconds2 = d2.getValue(XMLDateTime::Second) + d2.getValue(XMLDateTime::Minute)*60 + d2.getValue(XMLDateTime::Hour)*60*60 +
			d2.getValue(XMLDateTime::Day)*60*60*24;
		return ( (seconds1 + milis1) / (seconds2 + milis2));
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

    if (fDuration.getValue(XMLDateTime::Type) == xdt_yearMonthDuration )
    {
	//add months
	fNewDate.setValue(XMLDateTime::Month, modulo(dt.getValue(XMLDateTime::Month) + fDuration.getValue(XMLDateTime::Month), 1, 13));
	carry = fQuotient(fNewDate.getValue(XMLDateTime::Month), 1, 13);
	if (fNewDate.getValue(XMLDateTime::Month) <= 0) {
           fNewDate.setValue(XMLDateTime::Month, fNewDate.getValue(XMLDateTime::Month) + 12);
           carry--;
    	}

        //add years (may be modified additionaly below)
    	fNewDate.setValue(XMLDateTime::CentYear, dt.getValue(XMLDateTime::CentYear) + fDuration.getValue(XMLDateTime::CentYear) + carry);
    }

    if (fDuration.getValue(XMLDateTime::Type) == xdt_dayTimeDuration )
    {
	//add miliseconds
	carry = 0;
	double milis = dt.getValue(XMLDateTime::MiliSecond)/pow(10.0, (double)dt.getValue(XMLDateTime::MiliSecondLen)) +
			fDuration.getValue(XMLDateTime::MiliSecond)/pow(10.0, (double)fDuration.getValue(XMLDateTime::MiliSecondLen));
	if (milis>=1.0 || milis <=0.0)
	{
		if (milis>=1.0)
		{
			milis -= 1.0;
			carry = 1;
		}
		else if (milis <= 0.0)
		{
			milis += 1.0;
			carry = -1;
		}
		char tmpBuf[MAX_MEM_STR_SIZE];
		if (milis != 0.0)
			sprintf(tmpBuf, "%g", milis);
		else
			sprintf(tmpBuf,"0.0");
		fNewDate.setValue(XMLDateTime::MiliSecond, atoi(&tmpBuf[2]));
		fNewDate.setValue(XMLDateTime::MiliSecondLen, strlen(&tmpBuf[2]));
	}

	//add seconds
    	int temp = dt.getValue(XMLDateTime::Second) + fDuration.getValue(XMLDateTime::Second) + carry;
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
    
    	fNewDate.setValue(XMLDateTime::Day, dt.getValue(XMLDateTime::Day) + fDuration.getValue(XMLDateTime::Day) + carry);
    }

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

    //fNewDate->fValue[utc] = UTC_STD_CHAR;
    fNewDate.setValue(XMLDateTime::utc, XMLDateTime::UTC_STD);
    return fNewDate;
}

XMLDateTime subtractDurationFromDateTime(const XMLDateTime& d1, const XMLDateTime& d)
{
	return addDurationToDateTime(d1, multiplyDuration(d, -1.0));
}

XMLDateTime subtractDateTimes(const XMLDateTime& d1, const XMLDateTime& d2 )
{
	//FIXME: Implement this function
	return d1;
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
	XMLDateTime diff = subtractDurations(dtTz, tz);

	int utc_type = ( tz.getValue(XMLDateTime::Hour) < 0 )?XMLDateTime::UTC_NEG:XMLDateTime::UTC_POS;
	fNewDate.setValue(XMLDateTime::utc, utc_type);
	fNewDate.setValue(XMLDateTime::tz_hh, tz.getValue(XMLDateTime::Hour));
	fNewDate.setValue(XMLDateTime::tz_mm, tz.getValue(XMLDateTime::Minute));
	fNewDate = addDurationToDateTime(fNewDate, diff);
	return fNewDate;
}

int XMLDateTime::compare(const XMLDateTime& lValue
                            , const XMLDateTime& rValue)
                            //, MemoryManager* const memMgr)
{
    //
    // If any of the them is not normalized() yet,
    // we need to do something here.
    //

    XMLDateTime lTemp = lValue;
    XMLDateTime rTemp = rValue;

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

    if ( lTemp.getValue(MiliSecondLen))
    {
	double lMilis = lTemp.getValue(MiliSecond) == 0 ? 0.0 :
		 	lTemp.getValue(MiliSecond)/pow(10.0, (double)lTemp.getValue(MiliSecondLen));

	if (! rTemp.getValue(MiliSecondLen))
	{
		return GREATER_THAN;
	}

	double rMilis = rTemp.getValue(MiliSecond) == 0 ? 0.0 :
		 	rTemp.getValue(MiliSecond)/pow(10.0, (double)rTemp.getValue(MiliSecondLen));

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
   if ( lTemp.getValue(utc) != UTC_STD || rTemp.getValue(utc) != UTC_STD)
   {
	int lNeg = lTemp.getValue(utc) == UTC_POS ? 1 : -1;
	int rNeg = rTemp.getValue(utc) == UTC_POS ? 1 : -1;

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
    char* new_string_value = new char[ TOTAL_FIELDS * sizeof(int)];
    memcpy(new_string_value, toCopy.string_value, TOTAL_FIELDS * sizeof(int));
    counted_ptr_value = str_counted_ptr(new_string_value);
    string_value = counted_ptr_value.get();
}

//
// [-]{CCYY-MM-DD}[TimeZone]
//
void XMLDateTime::parseDate(const char* buf)
{
    setValue(Type, xs_date);
    int start = 0, end = strlen(buf);
    getDate(buf, start, end);
    parseTimeZone(buf, start, end);
    validateDateTime();
}

void XMLDateTime::parseTime(const char* buf)
{

    setValue(Type, xs_time);
    int start=0, end = strlen(buf);
    // time initialize to default values

    getTime(buf, start, end);

    validateDateTime();
    setValue(hasTime, true);
}

// 
// [-]{CCYY-MM-DD}'T'{HH:MM:SS.MS}[TimeZone]
// 
void XMLDateTime::parseDateTime(const char* buf)
{
    setValue(Type, xs_dateTime);
    int fStart = 0, fEnd = strlen(buf);
    getDate(buf,fStart, fEnd);

    //fStart is supposed to point to 'T'
    if (buf[fStart++] != DATETIME_SEPARATOR)
	throw USER_EXCEPTION2(FODT017, "Missing a time separator in dateTime value");

    getTime(buf, fStart, fEnd);
    validateDateTime();
    setValue(hasTime, true);
}

//
// {---DD}[TimeZone]
//  01234
//
void XMLDateTime::parseDay(const char* buf)
{
    setValue(Type, xs_gDay);
    int start=0, end=strlen(buf);

    if (buf[0] != DATE_SEPARATOR ||
        buf[1] != DATE_SEPARATOR ||
        buf[2] != DATE_SEPARATOR  )
	throw USER_EXCEPTION2(FODT004, "invalid gDay value");

    //initialize values
    setValue(Day, parseInt(buf, start+3, start+5));

    if ( DAY_SIZE < end )
    {        
        int pos = indexOf(UTC_SET, buf[DAY_SIZE]);
        if (pos == -1 )
	    throw USER_EXCEPTION2(FODT004, "invalid gDay value");
        else
        {
    	    setValue(utc, pos+1);
            getTimeZone(buf, DAY_SIZE, end);
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
    int fStart=0, fEnd=strlen(fBuffer);

    if (fBuffer[0] != DATE_SEPARATOR || fBuffer[1] != DATE_SEPARATOR  || fEnd < 4 )
	throw USER_EXCEPTION2(FODT005, "invalid gMonth value");

    // REVISIT: allow both --MM and --MM-- now. 
    // need to remove the following lines to disallow --MM-- 
    // when the errata is officially in the rec. 
    setValue(Month, parseInt(fBuffer, 2, 4));
 
    fStart = 4;
    if ( fEnd >= fStart+2 && fBuffer[fStart] == DATE_SEPARATOR && fBuffer[fStart+1] == DATE_SEPARATOR ) 
    { 
        fStart += 2; 
    } 

    //
    // parse TimeZone if any
    //
    if ( fStart < fEnd )
    {      
        int pos = indexOf(UTC_SET, fBuffer[fStart]);
        if ( pos == NOT_FOUND )
		throw USER_EXCEPTION2(FODT005, "invalid gMonth value");
        else
        {
    	    setValue(utc, pos+1);
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
    int fStart=0, fEnd=strlen(fBuffer);
    // skip the first '-' and search for timezone
    //
    int sign = findUTCSign(fBuffer, (fBuffer[0] == '-') ? 1 : 0, fEnd);

    if (sign == NOT_FOUND)
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
    int fStart=0, fEnd = strlen(fBuffer);

    if (fBuffer[0] != DATE_SEPARATOR ||
        fBuffer[1] != DATE_SEPARATOR ||
        fBuffer[4] != DATE_SEPARATOR )
	throw USER_EXCEPTION2(FODT006, "invalid gMonthDay value");

	
    //initialize
    setValue(Month, parseInt(fBuffer, 2, 4));	
    setValue(Day, parseInt(fBuffer, 5, 7));

    if ( MONTHDAY_SIZE < fEnd )
    {        
        int pos = indexOf(UTC_SET, fBuffer[MONTHDAY_SIZE]);
        if ( pos == NOT_FOUND )
		throw USER_EXCEPTION2(FODT006, "invalid gMonthDay value");
        else
        {
    	    setValue(utc, pos+1);
            getTimeZone(fBuffer,MONTHDAY_SIZE,fEnd);
        }
    }

    validateDateTime();
}

void XMLDateTime::parseYearMonth(const char* fBuffer)
{
    setValue(Type, xs_gYearMonth);
    int fStart=0, fEnd=strlen(fBuffer);

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
    int fStart=0, fEnd = strlen(fBuffer);
    // must start with '-' or 'P'
    //
    char c = fBuffer[fStart++];
    if ( (c != DURATION_STARTER) &&
         (c != '-')            )
	throw USER_EXCEPTION2(FODT007, "invalid duration start");

    // 'P' must ALWAYS be present in either case
    if ( (c == '-') &&
         (fBuffer[fStart++]!= DURATION_STARTER ))
	throw USER_EXCEPTION2(FODT007, "invalid duration");

    // java code
    //date[utc]=(c=='-')?'-':0;
    //fValue[utc] = UTC_STD;
    setValue(utc, (fBuffer[0] == '-'? UTC_NEG : UTC_STD));

    int negate = ( fBuffer[0] == '-'? -1 : 1);

    //
    // No negative value is allowed after 'P'
    //
    // eg P-1234, invalid
    //
    if (indexOf(fBuffer, fStart, fEnd, '-') != NOT_FOUND)
	throw USER_EXCEPTION2(FODT007, "invalid duration");

    //at least one number and designator must be seen after P
    bool designator = false;

    int endDate = indexOf(fBuffer, fStart, fEnd, DATETIME_SEPARATOR);
    if ( endDate == NOT_FOUND )
    {
        endDate = fEnd;  // 'T' absent
    }

    //find 'Y'
    int end = indexOf(fBuffer, fStart, endDate, DURATION_Y);
    if ( end != NOT_FOUND )
    {
        //scan year
        setValue(CentYear, negate * parseInt(fBuffer, fStart, end));
        fStart = end+1;
        designator = true;
    }

    end = indexOf(fBuffer, fStart, endDate, DURATION_M);
    if ( end != NOT_FOUND )
    {
        //scan month
        setValue(Month, negate * parseInt(fBuffer, fStart, end));
        fStart = end+1;
        designator = true;
    }

    end = indexOf(fBuffer, fStart, endDate, DURATION_D);
    if ( end != NOT_FOUND )
    {
        //scan day
        setValue(Day, negate * parseInt(fBuffer, fStart,end));
        fStart = end+1;
        designator = true;
    }

    if ( (fEnd == endDate) &&   // 'T' absent
         (fStart != fEnd)   )   // something after Day
	throw USER_EXCEPTION2(FODT007, "invalid duration");

    if ( fEnd != endDate ) // 'T' present
    {
        //scan hours, minutes, seconds
        //

        // skip 'T' first
        end = indexOf(fBuffer, ++fStart, fEnd, DURATION_H);
        if ( end != NOT_FOUND )
        {
            //scan hours
            setValue(Hour, negate * parseInt(fBuffer, fStart, end));
            fStart = end+1;
            designator = true;
        }

        end = indexOf(fBuffer, fStart, fEnd, DURATION_M);
        if ( end != NOT_FOUND )
        {
            //scan min
            setValue(Minute, negate * parseInt(fBuffer, fStart, end));
            fStart = end+1;
            designator = true;
        }

        end = indexOf(fBuffer, fStart, fEnd, DURATION_S);
        if ( end != NOT_FOUND )
        {
            //scan seconds
            int mlsec = indexOf (fBuffer, fStart, end, MILISECOND_SEPARATOR);

            /***
             * Schema Errata: E2-23
             * at least one digit must follow the decimal point if it appears. 
             * That is, the value of the seconds component must conform 
             * to the following pattern: [0-9]+(.[0-9]+)? 
             */
            if ( mlsec != NOT_FOUND )
            {
                /***
                 * make usure there is something after the '.' and before the end.
                 */
                if ( mlsec+1 == end )
		throw USER_EXCEPTION2(FODT007, "invalid milisecond value in duration");

                setValue(Second, negate * parseInt(fBuffer, fStart, mlsec));
                setValue(MiliSecond,negate * parseInt(fBuffer, mlsec+1, end));
		setValue(MiliSecondLen, end-mlsec-1);
            }
            else
            {
                setValue(Second, negate * parseInt(fBuffer, fStart,end));
            }

            fStart = end+1;
            designator = true;
        }

        // no additional data should appear after last item
        // P1Y1M1DT is illigal value as well
        if ( (fStart != fEnd) ||
              fBuffer[--fStart] == DATETIME_SEPARATOR )
	throw USER_EXCEPTION2(FODT007, "no time after time separator in duration");
    }

    if ( !designator )
    throw USER_EXCEPTION2(FODT007, "invalid duration");

    normalize();

}

//
// Parse xdt_yearMonth duration
// PnYnM: -P1Y2M
//
// [-]{'P'{[n'Y'][n'M']}}
//
//
void XMLDateTime::parseYearMonthDuration(const char* fBuffer)
{
    setValue(Type, xdt_yearMonthDuration);
    int fStart=0, fEnd = strlen(fBuffer);
    // must start with '-' or 'P'
    //
    char c = fBuffer[fStart++];
    if ( (c != DURATION_STARTER) &&
         (c != '-')            )
	throw USER_EXCEPTION2(FODT007, "invalid xdt_yearMonthDuration start");

    // 'P' must ALWAYS be present in either case
    if ( (c == '-') &&
         (fBuffer[fStart++]!= DURATION_STARTER ))
	throw USER_EXCEPTION2(FODT007, "invalid xdt_yearMonthDuration");

    // java code
    //date[utc]=(c=='-')?'-':0;
    //fValue[utc] = UTC_STD;
    setValue(utc, (fBuffer[0] == '-'? UTC_NEG : UTC_STD));

    int negate = ( fBuffer[0] == '-'? -1 : 1);

    //
    // No negative value is allowed after 'P'
    //
    // eg P-1234, invalid
    //
    if (indexOf(fBuffer, fStart, fEnd, '-') != NOT_FOUND)
	throw USER_EXCEPTION2(FODT007, "invalid xdt_yearMonthDuration");

    //at least one number and designator must be seen after P
    bool designator = false;

    //find 'Y'
    int end = indexOf(fBuffer, fStart, fEnd, DURATION_Y);
    if ( end != NOT_FOUND )
    {
        //scan year
        setValue(CentYear, negate * parseInt(fBuffer, fStart, end));
        fStart = end+1;
        designator = true;
    }

    end = indexOf(fBuffer, fStart, fEnd, DURATION_M);
    if ( end != NOT_FOUND )
    {
        //scan month
        setValue(Month, negate * parseInt(fBuffer, fStart, end));
        fStart = end+1;
        designator = true;
    }

    if ( !designator )
    throw USER_EXCEPTION2(FODT007, "invalid xdt_yearMonthDuration");

    normalize();
}

// xdt_dayTimeDuration
// PnDTnHnMnS: -P3DT10H30M
//
// [-]{'P'{[n'D']['T'][n'H'][n'M'][n'S']}}
//
//  Note: the n above shall be >= 0
//        if no time element found, 'T' shall be absent
//
void XMLDateTime::parseDayTimeDuration(const char* fBuffer)
{
    setValue(Type, xdt_dayTimeDuration);
    int fStart=0, fEnd = strlen(fBuffer);
    // must start with '-' or 'P'
    //
    char c = fBuffer[fStart++];
    if ( (c != DURATION_STARTER) &&
         (c != '-')            )
	throw USER_EXCEPTION2(FODT007, "invalid xdt_dayTimeDuration start");

    // 'P' must ALWAYS be present in either case
    if ( (c == '-') &&
         (fBuffer[fStart++]!= DURATION_STARTER ))
	throw USER_EXCEPTION2(FODT007, "invalid xdt_dayTimeDuration");

    setValue(utc, (fBuffer[0] == '-'? UTC_NEG : UTC_POS));

    int negate = ( fBuffer[0] == '-'? -1 : 1);

    //
    // No negative value is allowed after 'P'
    //
    // eg P-1234, invalid
    //
    if (indexOf(fBuffer, fStart, fEnd, '-') != NOT_FOUND)
	throw USER_EXCEPTION2(FODT007, "invalid duration");

    //at least one number and designator must be seen after P
    bool designator = false;

    int endDate = indexOf(fBuffer, fStart, fEnd, DATETIME_SEPARATOR);
    if ( endDate == NOT_FOUND )
    {
        endDate = fEnd;  // 'T' absent
    }

    int end = indexOf(fBuffer, fStart, endDate, DURATION_D);
    if ( end != NOT_FOUND )
    {
        //scan day
        setValue(Day, negate * parseInt(fBuffer, fStart,end));
        fStart = end+1;
        designator = true;
    }

    if ( (fEnd == endDate) &&   // 'T' absent
         (fStart != fEnd)   )   // something after Day
	throw USER_EXCEPTION2(FODT007, "invalid xdt_dayTimeDuration");

    if ( fEnd != endDate ) // 'T' present
    {
        //scan hours, minutes, seconds
        //

        // skip 'T' first
        end = indexOf(fBuffer, ++fStart, fEnd, DURATION_H);
        if ( end != NOT_FOUND )
        {
            //scan hours
            setValue(Hour, negate * parseInt(fBuffer, fStart, end));
            fStart = end+1;
            designator = true;
        }

        end = indexOf(fBuffer, fStart, fEnd, DURATION_M);
        if ( end != NOT_FOUND )
        {
            //scan min
            setValue(Minute, negate * parseInt(fBuffer, fStart, end));
            fStart = end+1;
            designator = true;
        }

        end = indexOf(fBuffer, fStart, fEnd, DURATION_S);
        if ( end != NOT_FOUND )
        {
            //scan seconds
            int mlsec = indexOf (fBuffer, fStart, end, MILISECOND_SEPARATOR);

            /***
             * Schema Errata: E2-23
             * at least one digit must follow the decimal point if it appears. 
             * That is, the value of the seconds component must conform 
             * to the following pattern: [0-9]+(.[0-9]+)? 
             */
            if ( mlsec != NOT_FOUND )
            {
                /***
                 * make usure there is something after the '.' and before the end.
                 */
                if ( mlsec+1 == end )
		throw USER_EXCEPTION2(FODT007, "invalid milisecond value in xdt_dayTimeDuration");

                setValue(Second, negate * parseInt(fBuffer, fStart, mlsec));
                setValue(MiliSecond,negate * parseInt(fBuffer, mlsec+1, end));
		setValue(MiliSecondLen, end-mlsec-1);
            }
            else
            {
                setValue(Second, negate * parseInt(fBuffer, fStart,end));
            }

            fStart = end+1;
            designator = true;
        }

        // no additional data should appear after last item
        // P1Y1M1DT is illigal value as well
        if ( (fStart != fEnd) ||
              fBuffer[--fStart] == DATETIME_SEPARATOR )
	throw USER_EXCEPTION2(FODT007, "no time after time separator in xdt_dayTimeDuration");
    }

    if ( !designator )
    throw USER_EXCEPTION2(FODT007, "invalid xdt_dayTimeDuration");

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
void XMLDateTime::getDate(const char* fBuffer, int& fStart, int& fEnd)
{

    // Ensure enough chars in buffer
    if ( (fStart+YMD_MIN_SIZE) > fEnd)
	throw USER_EXCEPTION2(FODT008, "incomplete date");

    getYearMonth(fBuffer, fStart, fEnd);    // Scan YearMonth and
                       // fStart point to the next '-'

    if (fBuffer[fStart++] != DATE_SEPARATOR)
	throw USER_EXCEPTION2(FODT008, "invalid date, CCYY-MM must be followed by a '-' sign");

    setValue(Day, parseInt(fBuffer, fStart, fStart+2));
    fStart += 2 ;  //fStart points right after the Day

    return;
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
void XMLDateTime::getTime(const char* fBuffer, int& fStart, int& fEnd)
{

    // Ensure enough chars in buffer
    if ( (fStart+TIME_MIN_SIZE) > fEnd)
	throw USER_EXCEPTION2(FODT009, "incomplete time");
        //"Imcomplete Time Format"

    // check (fixed) format first
    if ((fBuffer[fStart + 2] != TIME_SEPARATOR) ||
        (fBuffer[fStart + 5] != TIME_SEPARATOR)  )
	throw USER_EXCEPTION2(FODT009, "invalid time format");
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
    int sign = findUTCSign(fBuffer, fStart, fEnd);

    //parse miliseconds
    int milisec = (fBuffer[fStart] == MILISECOND_SEPARATOR)? fStart : NOT_FOUND;
    if ( milisec != NOT_FOUND )
    {
        fStart++;   // skip the '.'
        // make sure we have some thing between the '.' and fEnd
        if (fStart >= fEnd)
	throw USER_EXCEPTION2(FODT009, "invalid time: no digits after the '.'");
            //("ms shall be present once '.' is present" );

        if ( sign == NOT_FOUND )
        {
            setValue(MiliSecond, parseInt(fBuffer, fStart, fEnd));  //get ms between '.' and fEnd
	    setValue(MiliSecondLen, fEnd-fStart);
            fStart = fEnd;
        }
        else
        {
            setValue(MiliSecond, parseInt(fBuffer, fStart, sign));  //get ms between UTC sign and fEnd
	    setValue(MiliSecondLen, fEnd-fStart);
        }
	}
    else if(sign == 0 || sign != fStart)
	throw USER_EXCEPTION2(FODT010, "seconds have more than 2 digits");
        // seconds has more than 2 digits

    //parse UTC time zone (hh:mm)
    if ( sign > 0 ) {
        getTimeZone(fBuffer,sign,fEnd);
    }

}

//
// [-]{CCYY-MM}
//
// Note: CCYY could be more than 4 digits
//       fStart updated to point AFTER the second 'M' (probably meet the fEnd)
//
void XMLDateTime::getYearMonth(const char* fBuffer, int& fStart, int& fEnd)
{

    // Ensure enough chars in buffer
    if ( (fStart+YMONTH_MIN_SIZE) > fEnd)
	throw USER_EXCEPTION2(FODT011, "incomplete year month format");
        //"Imcomplete YearMonth Format";

    // skip the first leading '-'
    int start = ( fBuffer[0] == '-') ? fStart + 1 : fStart;

    //
    // search for year separator '-'
    //
    int yearSeparator = indexOf(fBuffer, start, fEnd, DATE_SEPARATOR);
    if ( yearSeparator == NOT_FOUND)
	throw USER_EXCEPTION2(FODT011, "year separator is missing or misplaced");
        //("Year separator is missing or misplaced");

    setValue(CentYear, parseIntYear(fBuffer,fStart,yearSeparator));
    fStart = yearSeparator + 1;  //skip the '-' and point to the first M

    //
    //gonna check we have enough byte for month
    //
    if ((fStart + 2) > fEnd )
	throw USER_EXCEPTION2(FODT012, "no month specified");
        //"no month in buffer"

    setValue(Month, parseInt(fBuffer,fStart, yearSeparator + 3));
    fStart += 2;  //fStart points right after the MONTH

    return;
}

void XMLDateTime::parseTimeZone(const char* fBuffer, int& fStart, int& fEnd)
{
    //fStart points right after the date   	 
  	if ( fStart < fEnd ) {
        int pos = indexOf(UTC_SET, fBuffer[fStart]);
    	if (pos == NOT_FOUND) 
	   throw USER_EXCEPTION2(FODT003, "no UTC sign in the timezone");
   		else { 
    	    	setValue(utc, pos+1);
  	        getTimeZone(fBuffer,fStart,fEnd);   		
   		}
    }

    return;
}

//
// 'Z'
// ['+'|'-']hh:mm
//
// Note: Assuming fStart points to the beginning of TimeZone section
//       fStart updated to meet fEnd
//
void XMLDateTime::getTimeZone(const char* fBuffer, const int& sign, int& fEnd)
{

    if ( fBuffer[sign] == UTC_STD_CHAR )
    {
        if ((sign + 1) != fEnd )
	throw USER_EXCEPTION2(FODT003, "extra characters after Z in timezone");
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
	throw USER_EXCEPTION2(FODT003, "error parsing time zone");
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
    if (getValue(Type) == xs_duration || getValue(Type) == xdt_yearMonthDuration ||
	getValue(Type) == xdt_dayTimeDuration)
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

    int negate = (getValue(utc) == UTC_POS)? 1: -1;
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
	setValue(utc, UTC_NEG);
	setValue(tz_hh, getValue(Hour));
	setValue(tz_mm, getValue(Minute));
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
    temp = negate * getValue(Minute);
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

    // if this is a dayTimeDuration, then we leave the rest of duration
    // in days, otherwise we carry to months and years
    if (getValue(Type) == xdt_dayTimeDuration)
	setValue(Day, getValue(Day) + carry*negate);
 
    else
    {
    	//add days
    	temp = negate * getValue(Day) + carry;
    	carry = fQuotient(temp, 31);
    	setValue(Day, negate * mod(temp, 31, carry));
    	if (negate * getValue(Day) < 0) {
        	setValue(Day, getValue(Day) + 31*negate);
        	carry--;
      	}

    	//add months
    	temp = negate * getValue(Month) + carry;
    	carry = fQuotient(temp, 12);
	setValue(Month, negate * mod(temp, 12, carry));

	if (negate * getValue(Month)< 0){
		setValue(Month, getValue(Month) + 12*negate);
		carry--;
	}
	
/*
	char buf[1000];
	sprintf(buf, "temp=%d, orig_carry=%d, value=%d, finalValue=%d, carry=%d, year = %d",
		temp, orig_carry, value, finalValue, carry, getValue(CentYear) + carry*negate);
	throw USER_EXCEPTION2(FODT011, buf);
*/

    	//add years
    	setValue(CentYear, getValue(CentYear) + carry*negate);
    }

    return;
}

void XMLDateTime::validateDateTime() const
{

    //REVISIT: should we throw an exception for not valid dates
    //          or reporting an error message should be sufficient?
    if (( getValue(Type)==xs_date || getValue(Type)==xs_dateTime || getValue(Type)==xs_gYear || getValue(Type)==xs_gYearMonth )
		 && getValue(CentYear) == 0 )
	throw USER_EXCEPTION2(FODT011, "0000 is an illegal value for year");
        //"The year \"0000\" is an illegal year value");

    if ((getValue(Type)==xs_date || getValue(Type)==xs_dateTime || getValue(Type)==xs_gMonth || getValue(Type)==xs_gYearMonth)
	&& ( getValue(Month) < 1  || getValue(Month) > 12  ))
	throw USER_EXCEPTION2(FODT012, "invalid month value, must be between 1 and 12");
		//"The month must have values 1 to 12");

    //validate days
    if ((getValue(Type)==xs_date || getValue(Type)==xs_dateTime || getValue(Type)==xs_gMonthDay || getValue(Type)==xs_gDay)
    	&& ( getValue(Day) > maxDayInMonthFor( getValue(CentYear), getValue(Month)) || getValue(Day) == 0 ))
	throw USER_EXCEPTION2(FODT008, "invalid day value");
        //"The day must have values 1 to 31");

    //validate hours
    if ((getValue(Hour) < 0)  ||
        (getValue(Hour) > 24) ||
        ((getValue(Hour) == 24) && ((getValue(Minute) !=0) ||
                                  (getValue(Second) !=0) ||
                                  (getValue(MiliSecond) !=0))))
	throw USER_EXCEPTION2(FODT013, "invalid hour, values must be between 0 and 23");
        //("Hour must have values 0-23");

    //validate minutes
    if ( getValue(Minute) < 0 ||
         getValue(Minute) > 59 )
	throw USER_EXCEPTION2(FODT014,"invalid minutes, values must be between 0 and 59");
        //"Minute must have values 0-59");

    //validate seconds
    if ( getValue(Second) < 0 ||
         getValue(Second) > 60 )
	throw USER_EXCEPTION2(FODT010,"invalid seconds, values must be between 0 and 60");
        //"Second must have values 0-60");

    //validate time-zone hours
    if ( (abs(getValue(tz_hh)) > 14) ||
         ((abs(getValue(tz_hh)) == 14) && (getValue(tz_mm)!= 0)) )
	throw USER_EXCEPTION2(FODT003, "invalid timezone hours, values must be between -14 and 14");
        //"Time zone should have range -14..+14");

    //validate time-zone minutes
    if ( abs(getValue(tz_mm)) > 59 )
	throw USER_EXCEPTION2(FODT003, "invalid timezone minutes, values must be between 0 and 59");
        //("Minute must have values 0-59");
	
    return;
}

// -----------------------------------------------------------------------
// locator and converter
// -----------------------------------------------------------------------
int XMLDateTime::indexOf(const char* fBuffer, const int start, const int end, const char ch) const
{
    for ( int i = start; i < end; i++ )
        if ( fBuffer[i] == ch )
            return i;

    return NOT_FOUND;
}

int XMLDateTime::indexOf(const char* array, const char  ch ) const
{
    for (int i=0; array[i] != 0; i++)
	if (array[i] == ch)
	  return i;
   return NOT_FOUND;
}

int XMLDateTime::findUTCSign (const char* fBuffer, const int start, const int fEnd)
{
    int  pos;
    for ( int index = start; index < fEnd; index++ )
    {
        pos = indexOf(UTC_SET, fBuffer[index]);
        if ( pos != NOT_FOUND)
        {
    	    setValue(utc, pos+1);
            return index;
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
int XMLDateTime::parseInt(const char* fBuffer, const int start, const int end) const
{
    int retVal = 0;
    for (int i=start; i < end; i++) {

        if (fBuffer[i] < '0' || fBuffer[i] > '9')
	throw USER_EXCEPTION2(FODT015, "invalid numeric character in dateTime or duration");

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
int XMLDateTime::parseIntYear(const char* fBuffer, const int fStart, const int end) const
{
    // skip the first leading '-'
    int start = ( fBuffer[0] == '-') ? fStart + 1 : fStart;

    int length = end - start;
    if (length < 4)
    throw USER_EXCEPTION2(FODT011, "invalid year, year must be in 'CCYY' format");
        //"Year must have 'CCYY' format");
    else if (length > 4 && fBuffer[start] == '0')
	throw USER_EXCEPTION2(FODT011, "leading zeros in year are only allowed if otherwise the year would have fewer than four digits");
        //"Leading zeros are required if the year value would otherwise have fewer than four digits;
        // otherwise they are forbidden");

    bool negative = (fBuffer[0] == '-');
    int  yearVal = parseInt(fBuffer, (negative ? 1 : 0), end);
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
    int miliSecondsLen = getValue(MiliSecondLen);
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

    if (miliSecondsLen)
    {
        *retPtr++ = '.';
	//if (retPtr-buf+miliSecondsLen > MAX_MEM_STR_SIZE)
		//throw USER_EXCEPTION2(FODT016, "string representation of dateTime value is too long");

	if (retPtr-buf+miliSecondsLen > MAX_MEM_STR_SIZE)
	{
	}

 	else fillMilisString(retPtr, getValue(MiliSecond), miliSecondsLen);

 	//fillMilisString(retPtr, getValue(MiliSecond), miliSecondsLen);
    }

    if (utcSize) {
        if (getValue(utc) != UTC_STD)
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
            if (getValue(utc) != UTC_STD)
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
    int miliSecondsLen = getValue(MiliSecondLen);
    int utcSize = (getValue(utc) == UTC_UNKNOWN) ? 0 : 1;
    
    //char* retBuf = new char[(10 + miliSecondsLen + utcSize + 1) * sizeof(char)];
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

    if (miliSecondsLen)
    {
        *retPtr++ = '.';
	fillMilisString(retPtr, getValue(MiliSecond), miliSecondsLen);
    }

    if (utcSize) {
        if (getValue(utc) != UTC_STD)
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
	int nonZeroDate, nonZeroTime;

	int neg = getValue(utc) == UTC_POS ? 1 : -1;

	if (neg == -1)
		*bufPtr++ = '-';

	*bufPtr++  = DURATION_STARTER;

	if (getValue(CentYear) != 0)
	{
		sprintf(bufPtr, "%d", neg*getValue(CentYear));
		bufPtr = buf + strlen(buf);
		*bufPtr++ = DURATION_Y;
		nonZeroDate = 1;
	}
	if (getValue(Month) != 0)
	{
		sprintf(bufPtr, "%d", neg*getValue(Month));
		bufPtr = buf + strlen(buf);
		*bufPtr++ = DURATION_M;
		nonZeroDate = 1;
	}
	if (getValue(Type) == xdt_yearMonthDuration && !nonZeroDate)
	{
		*bufPtr++ = '0';
		*bufPtr++ = DURATION_M;
	}

	if (getValue(Type) == xs_duration || getValue(Type) == xdt_dayTimeDuration)
	{
		if (getValue(Day) != 0)
		{
			sprintf(bufPtr, "%d", neg*getValue(Day));
			bufPtr = buf + strlen(buf);
			*bufPtr++ = DURATION_D;
			nonZeroDate = 1;
		}
		if (getValue(Hour) != 0 || getValue(Minute) != 0 || getValue(Second) != 0 || getValue(MiliSecond) != 0)
			*bufPtr++ = DATETIME_SEPARATOR;

		if (getValue(Hour) != 0)
		{
			sprintf(bufPtr, "%d", neg*getValue(Hour));
			bufPtr = buf + strlen(buf);
			*bufPtr++ = DURATION_H;
			nonZeroTime= 1;
		}
		if (getValue(Minute) != 0)
		{
			sprintf(bufPtr, "%d", neg*getValue(Minute));
			bufPtr = buf + strlen(buf);
			*bufPtr++ = DURATION_M;
			nonZeroTime= 1;
		}
		if (getValue(Second) != 0 || getValue(MiliSecond)!=0)
		{
			sprintf(bufPtr, "%d", neg*getValue(Second));
			bufPtr = buf + strlen(buf);
			if (getValue(MiliSecond) != 0)
			{
				*bufPtr++ = '.';
				fillMilisString(bufPtr, neg*getValue(MiliSecond), getValue(MiliSecondLen));
			}
			*bufPtr++ = DURATION_S;
			nonZeroTime= 1;
		}

		if (!nonZeroTime)
		{
			*bufPtr++ = '0';
			*bufPtr++ = DURATION_S;
		}
	}

	*bufPtr = '\0';
}

void XMLDateTime::fillString(char*& ptr, int value, int expLen) const
{
    char *strBuffer = new char[expLen+1];
    sprintf(strBuffer, "%d", value);
    int   actualLen = strlen(strBuffer);
    int   i;
    //append leading zeros
    for (i = 0; i < expLen - actualLen; i++)
    {
        *ptr++ = '0';
    }

    for (i = 0; i < actualLen; i++)
    {
        *ptr++ = strBuffer[i];
    }
    delete strBuffer;
}

void XMLDateTime::fillMilisString(char*& ptr, int value, int expLen) const
{
    char *strBuffer = new char[expLen+1];
    sprintf(strBuffer, "%d", value);
    int   actualLen = strlen(strBuffer);
    int   i;
    //append leading zeros
    for (i = 0; i < expLen - actualLen; i++)
    {
        *ptr++ = '0';
    }

    for (i = 0; i < actualLen && i < expLen; i++)
    {
        *ptr++ = strBuffer[i];
    }
    delete strBuffer;
}

int XMLDateTime::fillYearString(char*& ptr, int value) const
{
    char strBuffer[16];
    // let's hope we get no years of 15 digits...
    sprintf(strBuffer, "%d", value);
    int   actualLen = strlen(strBuffer);
    // don't forget that years can be negative...
    int negativeYear = 0;
    if(strBuffer[0] == '-')
    {
        *ptr++ = strBuffer[0];
        negativeYear = 1;
    }
    int   i;
    //append leading zeros
    for (i = 0; i < 4 - actualLen+negativeYear; i++)
    {
        *ptr++ = '0';
    }

    for (i = negativeYear; i < actualLen; i++)
    {
        *ptr++ = strBuffer[i];
    }
    if(actualLen > 4)
        return actualLen-4;
    return 0;
}

void XMLDateTime::get_string_value(char* buf)
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
		case xdt_dayTimeDuration:	printDuration(buf); break;
		case xdt_yearMonthDuration:	printDuration(buf); break;
		default:			throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema type passed to XMLDateTime::get_string_value");
	}
}
