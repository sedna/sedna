/*
 * File: xs_fp_converter.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 *
 *
 * The Initial Developer of the Original Code is Michael H. Kay, based on a published algorithm by
 * Guy L. Steele and Jon L. White.
 *
 * Contributor(s): the appendInt routine, and some of the constant declarations (and some of the ideas) are
 * from the class AppenderHelper by Jack Shirazi in the O'Reilly book Java Performance Tuning..
 *
 * The contents of this file are subject to the Mozilla Public License Version 1.0 (the "License");
 * you may not use this file except in compliance with the License. You may obtain a copy of the
 * License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied.
 * See the License for the specific language governing rights and limitations under the License.
 *
 */

#include "common/sedna.h"

#include "tr/executor/base/xs_fp_converter.h"
#include "tr/executor/base/lip/lip_class.h"
#include "tr/executor/base/xs_helper.h"



#define FPC_doubleSignMask  ((int64_t)0x8000 << (int64_t)48) // 0x8000000000000000L
#define FPC_doubleExpMask   ((int64_t)0x7ff0 << (int64_t)48) // 0x7ff0000000000000L
#define FPC_doubleExpShift  52
#define FPC_doubleExpBias   1023
#define FPC_doubleFractMask (((int64_t)0xfffff << (int64_t)32) | (int64_t)0xffffffff) // 0xfffffffffffffL
#define FPC_floatSignMask   0x80000000
#define FPC_floatExpMask    0x7f800000
#define FPC_floatExpShift   23
#define FPC_floatExpBias    127
#define FPC_floatFractMask  0x7fffff




static char charForDigit[] = {
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
    };


class appender
{
private:
    char *s;
    int p;
public:
    appender(char *_s_) : s(_s_), p(0) { s[0] = '\0'; }
    void append(const char *str)
    {
        int len = strlen(str);
        memcpy(s + p, str, len);
        s[p += len] = '\0';
    }
    void append(char c)
    {
        s[p] = c;
        s[++p] = '\0';
    }
    char *get() { return s; }
};

// Format an integer, appending the string representation of the integer to a string buffer
static void appendInt(appender &a, int32_t i) 
{
    if (i < 0) 
    {
        if (i == INT_MIN) 
        {
            //cannot make this positive due to integer overflow
            a.append("-2147483648");
            return;
        }
        a.append('-');
        i = -i;
    }

    int c;
    if (i < 10) 
    {
        //one digit
        a.append(charForDigit[i]);
    } 
    else if (i < 100) 
    {
        //two digits
        a.append(charForDigit[i / 10]);
        a.append(charForDigit[i % 10]);
    } 
    else if (i < 1000) 
    {
        //three digits
        a.append(charForDigit[i / 100]);
        a.append(charForDigit[(c = i % 100) / 10]);
        a.append(charForDigit[c % 10]);
    } 
    else if (i < 10000) 
    {
        //four digits
        a.append(charForDigit[i / 1000]);
        a.append(charForDigit[(c = i % 1000) / 100]);
        a.append(charForDigit[(c %= 100) / 10]);
        a.append(charForDigit[c % 10]);
    } 
    else if (i < 100000) 
    {
        //five digits
        a.append(charForDigit[i / 10000]);
        a.append(charForDigit[(c = i % 10000) / 1000]);
        a.append(charForDigit[(c %= 1000) / 100]);
        a.append(charForDigit[(c %= 100) / 10]);
        a.append(charForDigit[c % 10]);
    } 
    else if (i < 1000000) 
    {
        //six digits
        a.append(charForDigit[i / 100000]);
        a.append(charForDigit[(c = i % 100000) / 10000]);
        a.append(charForDigit[(c %= 10000) / 1000]);
        a.append(charForDigit[(c %= 1000) / 100]);
        a.append(charForDigit[(c %= 100) / 10]);
        a.append(charForDigit[c % 10]);
    } 
    else if (i < 10000000) 
    {
        //seven digits
        a.append(charForDigit[i / 1000000]);
        a.append(charForDigit[(c = i % 1000000) / 100000]);
        a.append(charForDigit[(c %= 100000) / 10000]);
        a.append(charForDigit[(c %= 10000) / 1000]);
        a.append(charForDigit[(c %= 1000) / 100]);
        a.append(charForDigit[(c %= 100) / 10]);
        a.append(charForDigit[c % 10]);
    } 
    else if (i < 100000000) 
    {
        //eight digits
        a.append(charForDigit[i / 10000000]);
        a.append(charForDigit[(c = i % 10000000) / 1000000]);
        a.append(charForDigit[(c %= 1000000) / 100000]);
        a.append(charForDigit[(c %= 100000) / 10000]);
        a.append(charForDigit[(c %= 10000) / 1000]);
        a.append(charForDigit[(c %= 1000) / 100]);
        a.append(charForDigit[(c %= 100) / 10]);
        a.append(charForDigit[c % 10]);
    } 
    else if (i < 1000000000) 
    {
        //nine digits
        a.append(charForDigit[i / 100000000]);
        a.append(charForDigit[(c = i % 100000000) / 10000000]);
        a.append(charForDigit[(c %= 10000000) / 1000000]);
        a.append(charForDigit[(c %= 1000000) / 100000]);
        a.append(charForDigit[(c %= 100000) / 10000]);
        a.append(charForDigit[(c %= 10000) / 1000]);
        a.append(charForDigit[(c %= 1000) / 100]);
        a.append(charForDigit[(c %= 100) / 10]);
        a.append(charForDigit[c % 10]);
    }
    else 
    {
        //ten digits
        a.append(charForDigit[i / 1000000000]);
        a.append(charForDigit[(c = i % 1000000000) / 100000000]);
        a.append(charForDigit[(c %= 100000000) / 10000000]);
        a.append(charForDigit[(c %= 10000000) / 1000000]);
        a.append(charForDigit[(c %= 1000000) / 100000]);
        a.append(charForDigit[(c %= 100000) / 10000]);
        a.append(charForDigit[(c %= 10000) / 1000]);
        a.append(charForDigit[(c %= 1000) / 100]);
        a.append(charForDigit[(c %= 100) / 10]);
        a.append(charForDigit[c % 10]);
    }
}

/**
 * Implementation of the (FPP)2 algorithm from Steele and White, for doubles in the range
 * 0.01 to 1000000, and floats in the range 0.000001 to 1000000.
 * In this range (a) XPath requires that the output should not be in exponential
 * notation, and (b) the arithmetic can be handled using longs rather than BigIntegers
 * param a - the string buffer to which the formatted result is to be appended
 * param e - the exponent of the floating point number
 * param f - the fraction part of the floating point number, such that the "real" value of the
 * number is f * 2^(e-p), with p>=0 and 0 lt f lt 2^p
 * param p - the precision
 */
static void fppfpp(appender &a, int32_t e, int64_t f, int32_t p) 
{
    int64_t R = f << (int64_t)s_max(e-p, 0);
    int64_t S = (int64_t)1 << (int64_t)s_max(0, -(e-p));
    int64_t Mminus = (int64_t)1 << (int64_t)s_max(e-p, 0);
    int64_t Mplus = Mminus;
    bool initial = true;

    // simpleFixup

    if (f == (int64_t)1 << (int64_t)(p-1)) 
    {
        Mplus = Mplus << (int64_t)1;
        R = R << (int64_t)1;
        S = S << (int64_t)1;
    }
    int k = 0;
    while (R < (S+(int64_t)9)/(int64_t)10)  // (S+9)/10 == ceiling(S/10)
    {
        k--;
        R *= (int64_t)10;
        Mminus *= (int64_t)10;
        Mplus *= (int64_t)10;
    }
    while ((int64_t)2*R + Mplus >= (int64_t)2*S) 
    {
        S*= (int64_t)10;
        k++;
    }

    int z=0;
    for (z=k; z<0; z++) 
    {
        if (initial)
            a.append("0.");
        initial = false;
        a.append('0');
    }

    // end simpleFixup

    //int H = k-1;

    bool low;
    bool high;
    int U;
    while (true) 
    {
        k--;
        U = (int)(R*(int64_t)10 / S);
        R = R*(int64_t)10 % S;
        Mminus *= (int64_t)10;
        Mplus *= (int64_t)10;
        low = (int64_t)2*R < Mminus;
        high = (int64_t)2*R > (int64_t)2*S - Mplus;
        if (low || high) break;
        if (k == -1) 
        {
            if (initial)
                a.append('0');
            a.append('.');
        }
        a.append(charForDigit[U]);
        initial = false;
    }
    if (high && (!low || (int64_t)2*R > S))
        U++;
    if (k == -1) 
    {
        if (initial)
            a.append('0');
        a.append('.');
    }
    a.append(charForDigit[U]);
    for (z=0; z<k; z++)
        a.append('0');
}

/**
 * Implementation of the (FPP)2 algorithm from Steele and White, for doubles in the range
 * 0.000001 to 0.01. In this range XPath requires that the output should not be in exponential
 * notation, but the scale factors are large enough to exceed the capacity of long arithmetic.
 * param a - the string buffer to which the formatted result is to be appended
 * param e - the exponent of the floating point number
 * param f - the fraction part of the floating point number, such that the "real" value of the
 * number is f * 2^(e-p), with p>=0 and 0 lt f lt 2^p
 * param p - the precision
 */
static void fppfppBig(appender &a, int32_t e, int64_t f, int32_t p) 
{
    //long R = f << Math.max(e-p, 0);
    lip R = lip(f) << s_max(e - p, 0);

    //long S = 1L << Math.max(0, -(e-p));
    lip S = lip(1) << s_max(0, -(e - p));

    //long Mminus = 1 << Math.max(e-p, 0);
    lip Mminus = lip(1) << s_max(e - p, 0);

    //long Mplus = Mminus;
    lip Mplus = Mminus;

    bool initial = true;

    // simpleFixup

    if (f == (int64_t)1 << (p-1)) 
    {
        Mplus = Mplus << 1;
        R = R << 1;
        S = S << 1;
    }
    int k = 0;
    while ((R < ((S + lip(9)) / lip(10))))  // (S+9)/10 == ceiling(S/10)
    {
        k--;
        R *= 10;
        Mminus *= 10;
        Mplus *= 10;
    }
    while (((R << 1) + Mplus) >= (S << 1))
    {
        S *= 10;
        k++;
    }

    for (int z=k; z<0; z++) {
        if (initial) {
            a.append("0.");
        }
        initial = false;
        a.append('0');
    }

    // end simpleFixup

    //int H = k-1;

    bool low;
    bool high;
    int U;
    while (true) 
    {
        k--;
        lip R10 = R * 10;
        U = (int)(R10 / S);
        R = R10 % S;
        Mminus *= 10;
        Mplus *= 10;
        lip R2 = R << 1;
        low = R2 < Mminus;
        high = R2 > ((S << 1) - Mplus);
        if (low || high) break;
        if (k == -1) 
        {
            if (initial) 
                a.append('0');
            a.append('.');
        }
        a.append(charForDigit[U]);
        initial = false;
    }
    if (high && (!low || (R << 1) > S)) 
        U++;
    if (k == -1) 
    {
        if (initial)
            a.append('0');
        a.append('.');
    }
    a.append(charForDigit[U]);
    for (int z=0; z<k; z++)
        a.append('0');
}

/**
 * Implementation of the (FPP)2 algorithm from Steele and White, for numbers outside the range
 * 0.000001 to 1000000. In this range XPath requires that the output should be in exponential
 * notation
 * param a - the string buffer to which the formatted result is to be appended
 * param e - the exponent of the floating point number
 * param f - the fraction part of the floating point number, such that the "real" value of the
 * number is f * 2^(e-p), with p>=0 and 0 lt f lt 2^p
 * param p - the precision
 */
static void fppfppExponential(appender &a, int32_t e, int64_t f, int32_t p) 
{
	//char buf[1024];
    //long R = f << Math.max(e-p, 0);
    lip R = lip(f) << s_max(e - p, 0);
	//printf("R = %s\n", R.format(buf, NULL));

    //long S = 1L << Math.max(0, -(e-p));
    lip S = lip(1) << s_max(0, -(e-p));

    //long Mminus = 1 << Math.max(e-p, 0);
    lip Mminus = lip(1) << s_max(e-p, 0);

    //long Mplus = Mminus;
    lip Mplus = Mminus;

    bool initial = true;
    bool doneDot = false;

    // simpleFixup

    if (f == (int64_t)1 << (p-1)) 
    {
        Mplus = Mplus << 1;
        R = R << 1;
        S = S << 1;
    }
    int k = 0;
    while (R < ((S + lip(9)) / lip(10)))  // (S+9)/10 == ceiling(S/10)
    {
        k--;
        R *= lip(10);
        Mminus *= lip(10);
        Mplus *= lip(10);
    }
	//printf("R = %s\n", R.format(buf, NULL));
	//printf("S = %s\n", S.format(buf, NULL));
	//printf("Mplus = %s\n", Mplus.format(buf, NULL));
	//printf("R<<1 = %s\n", (R<<1).format(buf, NULL));

    while (((R << 1) + Mplus) >= (S << 1))
    {
        S *= 10;
        k++;
    }

    // end simpleFixup

    int H = k-1;

    bool low;
    bool high;
    int U;
    while (true) 
    {
        k--;
        lip R10 = R * 10;
        U = (int)(R10 / S);
		//printf("R = %s\n", R.format(buf, NULL));
		//printf("S = %s\n", S.format(buf, NULL));
		//printf("R10 = %s\n", R10.format(buf, NULL));
        R = R10 % S;
		//printf("R = %s\n", R.format(buf, NULL));
        Mminus *= 10;
        Mplus *= 10;
        lip R2 = R << 1;
        low = R2 < Mminus;
		//printf("R2 = %s\n", R2.format(buf, NULL));
		//printf("S = %s\n", S.format(buf, NULL));
		//printf("Mminus = %s\n", Mminus.format(buf, NULL));
		//printf("Mplus = %s\n", Mplus.format(buf, NULL));
        high = R2 > ((S << 1) - Mplus);
        if (low || high) break;

        a.append(charForDigit[U]);
        if (initial) {
            a.append('.');
            doneDot = true;
        }
        initial = false;
    }
    if (high && (!low || ((R << 1) > S)))
        U++;
    a.append(charForDigit[U]);

    if (!doneDot)
        a.append(".0");
    a.append('E');
    appendInt(a, H);
}




char *get_xs_double_lexical_representation_Saxon(char* s, double value) 
{
    double d = value;
    if (u_is_neg_inf(d))
        strcpy(s, "-INF");
    else if (u_is_pos_inf(d))
        strcpy(s, "INF");
    else if (u_is_nan(d))
        strcpy(s, "NaN");
    else if (d == 0.0) 
    {
        if ((double2__int64_bits(d) & FPC_doubleSignMask) != 0) 
            strcpy(s, "-0");
        else
            strcpy(s, "0");
    } 
    else if (d == DBL_MAX)
        strcpy(s, "1.7976931348623157E308");
    else if (d == -DBL_MAX)
        strcpy(s, "-1.7976931348623157E308");
    else if (d == DBL_MIN)
        strcpy(s, "4.9E-324");
    else if (d == -DBL_MIN)
        strcpy(s, "-4.9E-324");
    else 
    {
        appender a(s);
        if (d < 0) 
        {
            a.append('-');
            d = -d;
        }

        bool exponential = (d >= 1000000 || d < 0.000001);
        int64_t bits = double2__int64_bits(d);
        int64_t fraction = ((int64_t)1<<(int64_t)52) | (bits & FPC_doubleFractMask);
        int64_t rawExp = (bits & FPC_doubleExpMask) >> FPC_doubleExpShift;
        int32_t exp = (int32_t)rawExp - FPC_doubleExpBias;

        if (rawExp == 0) 
            // don't know how to handle this currently: hand it over to printf to deal with
            sprintf(s, "%E", value);
        else if (exponential) 
            fppfppExponential(a, exp, fraction, 52);
        else 
        {
            if (d <= 0.01)
                fppfppBig(a, exp, fraction, 52);
            else 
                fppfpp(a, exp, fraction, 52);
        }
    }
    return s;
}

char *get_xs_float_lexical_representation_Saxon(char* s, float value)
{
    float f = value;
    double d = (double)f;
    if (u_is_neg_inf(d))
        strcpy(s, "-INF");
    else if (u_is_pos_inf(d))
        strcpy(s, "INF");
    else if (u_is_nan(d))
        strcpy(s, "NaN");
    else if (d == 0.0) 
    {
        if ((double2__int64_bits(d) & FPC_doubleSignMask) != 0) 
            strcpy(s, "-0");
        else
            strcpy(s, "0");
    } 
    else if (f == FLT_MAX) 
        strcpy(s, "3.4028235E38");
    else if (f == -FLT_MAX)
        strcpy(s, "-3.4028235E38");
    else if (f == FLT_MIN)
        strcpy(s, "1.4E-45");
    else if (f == -FLT_MIN)
        strcpy(s, "-1.4E-45");
    else 
    {
        appender a(s);
        if (f < 0) 
        {
            a.append('-');
            f = -f;
        }
        bool exponential = (f >= 1000000 || f < 0.000001F);
        int32_t bits = float2__int32_bits(f);
        int32_t fraction = (1<<23) | (bits & FPC_floatFractMask);
        int32_t rawExp = ((bits & FPC_floatExpMask) >> FPC_floatExpShift);
        int32_t exp = rawExp - FPC_floatExpBias;
        if (rawExp == 0)
            // don't know how to handle this currently: hand it over to printf to deal with
            sprintf(s, "%E", d);
        else if (exponential)
            fppfppExponential(a, exp, fraction, 23);
        else
            fppfpp(a, exp, fraction, 23);
    }
    return s;
}
