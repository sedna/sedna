/*
* File:  XMLDateTime.h
* Copyright (C) 2010 ISP RAS
* The Institute for System Programming of the Russian Academy of Sciences
*/

#ifndef XML_DATETIME_H
#define XML_DATETIME_H

#include "common/sedna.h"
#include "u/utime.h"
#include "xs_decimal_t.h"
#include "tr/structures/nodetypes.h"

typedef int64_t bigint;

typedef struct
{
    int year;
    unsigned int day:9;
    unsigned int month:4;
    unsigned int hour:5;
    unsigned int minute:6;
    unsigned int second:6;
    unsigned int mili:18;
    unsigned int utc:4;
    unsigned int tz_hh:4;
    unsigned int tz_mm:6;
} xs_packed_datetime;

/*
#if (sizeof(xs_packed_datetime)!=12)
#error "Packed datetime structure is not 12 bytes long"
#endif
*/

typedef struct
{
    unsigned int years;
    unsigned int days;
    unsigned int neg:1;
    unsigned int months:4;
    unsigned int hours:5;
    unsigned int minutes:6;
    unsigned int seconds:6;
    unsigned int milis:10;
} xs_packed_duration;

/*
#if (sizeof(packed_duration)!=12)
#error "Packed datetime structure is not 12 bytes long"
#endif
*/

class XMLDateTime
{
public:

    enum valueIndex
    {
        Type       = 0,
        CentYear   ,
        Month      ,
        Day        ,
        Hour       ,
        Minute     ,
        Second     ,
        MiliSecond ,
        utc        ,
        tz_hh	   ,
        tz_mm	   ,
        TOTAL_FIELDS
    };


    int fields[TOTAL_FIELDS];

    static const int EQUAL = 0;
    static const int LESS_THAN = -1;
    static const int GREATER_THAN = 1;

    enum utcType
    {
        UTC_UNKNOWN = 0,
        UTC_STD        ,          // set in parse() or normalize()
        UTC_POS        ,          // set in parse()
        UTC_NEG                   // set in parse()
    };

    // -----------------------------------------------------------------------
    // ctors and dtor
    // -----------------------------------------------------------------------

    XMLDateTime();
    XMLDateTime(const xs_packed_datetime& storage, xmlscm_type type);
    XMLDateTime(const xs_packed_duration& storage, xmlscm_type type);
    XMLDateTime(const utm& t);

    // -----------------------------------------------------------------------
    // Copy ctor and Assignment operators
    // -----------------------------------------------------------------------

    XMLDateTime(const XMLDateTime&);
    XMLDateTime& operator=(const XMLDateTime&);

    // -----------------------------------------------------------------------
    // Implementation of Abstract Interface
    // -----------------------------------------------------------------------

    // -----------------------------------------------------------------------
    // Printers
    // -----------------------------------------------------------------------

    void                printDateTime(char* buf) const;

    void                printTime(char* buf)    const;

    void                printDate(char* buf)     const;

    void 		        printDuration(char *buf)  const;

    // -----------------------------------------------------------------------
    // parsers
    // -----------------------------------------------------------------------

    void                  parseDateTime(const char* buf);           //DateTime

    void                  parseDate(const char* buf);               //Date

    void                  parseTime(const char* buf);               //Time

    void                  parseDay(const char* buf);                //gDay

    void                  parseMonth(const char* buf);              //gMonth

    void                  parseYear(const char* buf);               //gYear

    void                  parseMonthDay(const char* buf);           //gMonthDay

    void                  parseYearMonth(const char* buf);          //gYearMonth

    void                  parseDuration(const char* buf);           //duration

    void                  parseYearMonthDuration(const char* buf);  //yearMonthDuration

    void                  parseDayTimeDuration(const char* buf);    //dayTimeDuration


    //---------------------------------------------------------------------------
    // Converter, backs up casting operations
    //---------------------------------------------------------------------------
    XMLDateTime convertTo(xmlscm_type type);

    //---------------------------------------------------------------------------
    // Component extraction functions
    //---------------------------------------------------------------------------
    int getYears() const;
    int getMonths() const;
    int getDays() const;
    int getHours() const;
    int getMinutes() const;
    double getSeconds() const;
    XMLDateTime getTimezone() const;

    void normalize();
    void normalizeDateTime();
    void normalizeDateTimeWeak();
    void normalizeDuration();

    bool isDuration(){ return (getValue(Type) == xs_duration ||
                               getValue(Type) == xs_dayTimeDuration ||
                               getValue(Type) == xs_yearMonthDuration);}
    static int compare(const XMLDateTime& d1, const XMLDateTime& d2);
    //------------------------------------------------------------------------
    // Gets a string representation of a dateTime or duration
    //------------------------------------------------------------------------
    void get_string_value(char* outputBuffer) const;

    //------------------------------------------------------------------------
    // Gets a raw representation of the character array where the data is stored
    //------------------------------------------------------------------------
    xs_packed_datetime getPackedDateTime() const;
    xs_packed_duration getPackedDuration() const;

    //-----------------------------------------------------------------------
    // Getter and setter functions
    //------------------------------------------------------------------------

    int getValue(int valueIndex) const
    {
        return fields[ valueIndex ];
    }

    void setValue(int valueIndex, int newValue)
    {
        fields[valueIndex] = newValue;
    }

private:

    // -----------------------------------------------------------------------
    // Comparison
    // -----------------------------------------------------------------------
    static void    addDuration(XMLDateTime* pDuration,
                                      const XMLDateTime* const pBaseDate,
                                      int index);

    // -----------------------------------------------------------------------
    // helper
    // -----------------------------------------------------------------------

    inline  void    reset();

    inline  void    assertBuffer() const;

    inline  void    copy(const XMLDateTime&);

    // allow multiple parsing
    inline  void    initParser();

    inline  bool    isNormalized() const;

    //------------------------------------------------------------------------
    // Accessor functions
    //------------------------------------------------------------------------


    // -----------------------------------------------------------------------
    // scaners
    // -----------------------------------------------------------------------

    void     getDate(const char* fBuffer, size_t& fStart, size_t& fEnd);

    void     getTime(const char* fBuffer, size_t& fStart, size_t& fEnd);

    void     getYearMonth(const char* fBuffer, size_t& fStart, size_t& fEnd);

    void     getTimeZone(const char* fBuffer, const size_t& fStart, size_t& end);

    void     parseTimeZone(const char* fBuffer, size_t& fStart, size_t& fEnd);

    // -----------------------------------------------------------------------
    // locator and converter
    // -----------------------------------------------------------------------

    char*    findUTCSign(const char* fBuffer,
                         const size_t fStart,
                         const size_t fEnd,
                         size_t& pos);

    char*    indexOf(const char* fBuffer,
                     const size_t start,
                     const size_t end,
                     const char ch,
                     size_t& pos) const;

    char*    indexOf(const char* array,
                     const char ch,
                     size_t& pos) const;

    int      parseInt(const char *fBuffer,
                      const size_t start,
                      const size_t end) const;

    int      parseIntYear(const char* fBuffer,
                          const size_t start,
                          const size_t end) const;

    double   parseMiliSecond(const char* fBuffer,
                             const size_t start,
                             const size_t end) const;

    // -----------------------------------------------------------------------
    // validator and normalizer
    // -----------------------------------------------------------------------

    void     validateDateTime()          const;


    void     fillString(char*& ptr, int value, size_t expLen) const;
    void     fillMilisString(char*& ptr, int value, unsigned short expLen) const;
    void     fillYearString(char*& ptr, int value) const;
};

inline void XMLDateTime::reset()
{
    for ( int i=0; i < TOTAL_FIELDS; i++ )
        setValue(i, 0);
}

inline bool XMLDateTime::isNormalized() const
{
    return ( getValue(utc) == UTC_STD ? true : false );
}

//---------------------------------------------------------------------------
// Arithmetic Functions on Durations and DateTimes
//---------------------------------------------------------------------------
XMLDateTime addDurations(const XMLDateTime& d1, const XMLDateTime& d2);
XMLDateTime subtractDurations(const XMLDateTime& d1, const XMLDateTime& d2);
XMLDateTime addDurationToDateTime(const XMLDateTime& dt, const XMLDateTime& d);
XMLDateTime subtractDurationFromDateTime(const XMLDateTime& dt, const XMLDateTime& d);
XMLDateTime subtractDateTimes(const XMLDateTime& dt1, const XMLDateTime& dt2);
XMLDateTime multiplyDuration(const XMLDateTime& d1, double v);
XMLDateTime divideDuration(const XMLDateTime& d1, double v);
xs_decimal_t divideDurationByDuration(const XMLDateTime& d1, const XMLDateTime& d2);

// -----------------------------------------------------------------------
// Comparisons
// -----------------------------------------------------------------------

inline bool xs_dateTime_equal(const XMLDateTime& d1, const XMLDateTime& d2)
{return XMLDateTime::compare(d1,d2)==0;}
inline bool xs_dateTime_not_equal(const XMLDateTime& d1, const XMLDateTime& d2)
{return !XMLDateTime::compare(d1,d2);}
inline bool xs_dateTime_less_than(const XMLDateTime& d1, const XMLDateTime& d2)
{return XMLDateTime::compare(d1,d2)<0;}
inline bool xs_dateTime_less_equal(const XMLDateTime& d1, const XMLDateTime& d2)
{return XMLDateTime::compare(d1,d2)<=0;}
inline bool xs_dateTime_greater_than(const XMLDateTime& d1, const XMLDateTime& d2)
{return XMLDateTime::compare(d1,d2)>0;}
inline bool xs_dateTime_greater_equal(const XMLDateTime& d1, const XMLDateTime& d2)
{return XMLDateTime::compare(d1,d2)>=0;}

//--------------------------------------------------------------------------
// Adjustment functions
//--------------------------------------------------------------------------
XMLDateTime adjustToTimezone(const XMLDateTime& d);
XMLDateTime adjustToTimezone(const XMLDateTime& d, const XMLDateTime& tz);
XMLDateTime fnDateTime(const XMLDateTime& dt, const XMLDateTime& t);

#endif /* XML_DATETIME_H */
