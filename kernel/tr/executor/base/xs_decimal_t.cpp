/*
 * File:  xs_decimal_t.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */



#include "sedna.h"

#include "xs_decimal_t.h"
#include "xs_helper.h"
#include "PPBase.h"

extern "C" {
#include "decimal64.h"
}


static decContext dec_cxt;

typedef decNumber *(*decNumber_bin_fun)(decNumber *, const decNumber *, const decNumber *, decContext *);

static decNumber_bin_fun numerical_ops[] = {
    decNumberSubtract,
    decNumberAdd,
    decNumberMultiply,
    decNumberDivide,
    decNumberDivideInteger,
    decNumberRemainder
};


void xs_decimal_t::init()
{
    decContextDefault(&dec_cxt, DEC_INIT_DECIMAL64); // initialize
    dec_cxt.traps = 0;
}

/*******************************************************************************
 * SET FUNCTIONS
 ******************************************************************************/
void xs_decimal_t::set(__int64 a)
{
    get_xs_integer_lexical_representation(tr_globals::mem_str_buf, a);
    this->set(tr_globals::mem_str_buf);
}

void xs_decimal_t::set(float a)
{
    this->set((double)a);
}

void xs_decimal_t::set(double a)
{
    if (u_is_nan(a) || u_is_neg_inf(a) || u_is_pos_inf(a))
        throw USER_EXCEPTION2(FORG0002, "Cannot convert to xs:decimal type");

    double abs_a = a < 0.0 ? -a : a;
    if (abs_a > XS_DECIMAL_MAX_VALUE_DOUBLE || (a != 0 && abs_a < XS_DECIMAL_MIN_VALUE_DOUBLE))
        throw USER_EXCEPTION2(FOCA0001, "Cannot convert to xs:decimal type");

    sprintf(tr_globals::mem_str_buf, "%E", a);
    this->set(tr_globals::mem_str_buf, false);
}

void xs_decimal_t::set(bool a)
{
    dec_cxt.status = 0;
    if (a)
        decimal64FromString((decimal64*)(v.v1), "1.0", &dec_cxt);
    else
        decimal64FromString((decimal64*)(v.v1), "0.0", &dec_cxt);
}

void xs_decimal_t::set(const char *a, bool xs_compliant)
{
    if (strcmp(a, "NaN") == 0)
        throw USER_EXCEPTION2(FORG0001, "Cannot convert string \"NaN\" to xs:decimal");

    if (xs_compliant)
    {
        for (const char *c = a; *c != '\0'; c++)
            if (!(*c == '0' || *c == '1' || *c == '2' || *c == '3' || *c == '4' || 
                  *c == '5' || *c == '6' || *c == '7' || *c == '8' || *c == '9' || 
                  *c == '+' || *c == '-' || *c == '.'))
                throw USER_EXCEPTION2(FORG0001, "Cannot convert to xs:decimal type");
    }

    decNumber dv;
    dec_cxt.status = 0;
    decNumberFromString(&dv, a, &dec_cxt);

    if (dec_cxt.status & DEC_Errors)
    {
        //printf("0x%x %s\n", dec_cxt.status, decContextStatusToString(&dec_cxt));
        if (dec_cxt.status & DEC_IEEE_854_Overflow)
            throw USER_EXCEPTION2(FOAR0002, "xs:decimal overflow");
        else if (dec_cxt.status & DEC_IEEE_854_Underflow)
            decimal64FromString((decimal64*)(v.v1), "0.0", &dec_cxt);
        else // DEC_IEEE_854_Division_by_zero | DEC_IEEE_854_Invalid_operation
            throw USER_EXCEPTION2(FORG0001, "Cannot convert to xs:decimal type");
    }
    else if (dec_cxt.status & (DEC_Information ^ (DEC_Rounded | DEC_Inexact)))
    {
        throw USER_EXCEPTION2(FOCA0006, "Cannot convert to xs:decimal type");
    }

    dec_cxt.status = 0;
    //decNumberNormalize(&dv, &dv, &dec_cxt);
    decimal64FromNumber((decimal64*)(v.v1), &dv, &dec_cxt);
}

/*******************************************************************************
 * GET FUNCTIONS
 ******************************************************************************/
__int64 xs_decimal_t::get_int   () const
{
    decNumber dv, r;
    decimal64ToNumber((decimal64*)(v.v1), &dv);
    dec_cxt.status = 0;
	enum rounding old = dec_cxt.round;
	dec_cxt.round = DEC_ROUND_DOWN;
    decNumberToIntegralValue(&r, &dv, &dec_cxt);
	dec_cxt.round = old;
    decNumberToString(&r, tr_globals::mem_str_buf);
    return c_str2xs_integer(tr_globals::mem_str_buf);
}

float   xs_decimal_t::get_float () const
{
	decimal64ToString((decimal64*)(v.v1), tr_globals::mem_str_buf);
    return c_str2xs_float(tr_globals::mem_str_buf);
}

double  xs_decimal_t::get_double() const
{
	decimal64ToString((decimal64*)(v.v1), tr_globals::mem_str_buf);
    return c_str2xs_double(tr_globals::mem_str_buf);
}

bool    xs_decimal_t::get_bool  () const
{
    decNumber r;
    dec_cxt.status = 0;
    decimal64ToNumber((decimal64*)(v.v1), &r);
    return !(decNumberIsZero(&r) || decNumberIsNaN(&r));
}

char *xs_decimal_t::get_c_str(char *buf) const
{
    if (is_zero()) strcpy(buf, "0");
    else 
    {
        decNumber dv;
        dec_cxt.status = 0;
        decimal64ToNumber((decimal64*)(v.v1), &dv);
        decNumberNormalize(&dv, &dv, &dec_cxt);
        decNumberToString(&dv, buf);

        int E_pos = -1;
        int dot_pos = -1;
        char *start = buf;
        int max_len = MAX_MEM_STR_SIZE;
        if (*buf == '-')
        {
            start++;
            max_len--;
        }

        int i = 0;
        for (i = 0; start[i] != '\0' && (E_pos == -1 || dot_pos == -1); i++)
            if (start[i] == 'E') E_pos = i;
            else if (start[i] == '.') dot_pos = i;

        if (E_pos == -1) return buf;

        int exp = atoi(start + E_pos + 1);
        if (exp > 0)
        { // positive exponent
            int add0_pos = E_pos;
            if (dot_pos != -1)
            {
                add0_pos--;
                for (i = dot_pos; i < add0_pos; i++) 
                {
                    start[i] = start[i + 1];
                    exp--;
                }
            }

            for (i = 0; i < exp; i++)
                start[add0_pos + i] = '0';
            start[add0_pos + i] = '\0';
        }
        else
        { // negative exponent
            exp = -exp - 1;
            if (dot_pos == -1)
            {
                char c = *start;
                start[exp + 2] = c;
                start[exp + 3] = '\0';
            }
            else
            {
                start[1] = start[0];
                start[exp + 1 + E_pos] = '\0';
                for (i = E_pos - 1; i > 0; i--) start[exp + 1 + i] = start[i];
            }

            start[0] = '0';
            start[1] = '.';
            for (i = 0; i < exp; i++) start[i + 2] = '0';
        }
    }
    return buf;
}



/*******************************************************************************
 * OPERATIONS
 ******************************************************************************/
xs_decimal_t xs_decimal_t::operator - ()
{
    xs_decimal_t res;
    decNumber dv, r;

    decimal64ToNumber((decimal64*)(v.v1), &dv);
    dec_cxt.status = 0;
    decNumberMinus(&r, &dv, &dec_cxt);

    if (dec_cxt.status & DEC_Errors)
    {
        if (dec_cxt.status & DEC_IEEE_854_Division_by_zero)
            throw USER_EXCEPTION2(FOAR0001, "xs:decimal");
        else if (dec_cxt.status & DEC_IEEE_854_Overflow)
            throw USER_EXCEPTION2(FOAR0002, "xs:decimal overflow");
        else if (dec_cxt.status & DEC_IEEE_854_Underflow)
        {
            dec_cxt.status = 0;
            decNumberZero(&r);
        }
        else 
            throw USER_EXCEPTION2(SE1003, "numerical operation with xs:decimal");
    }

    decimal64FromNumber((decimal64*)(res.v.v1), &r, &dec_cxt);
    return res;
}

xs_decimal_t xs_decimal_t::numerical_operation(const xs_decimal_t & d, int idx) const
{
    xs_decimal_t res;
    decNumber dv, dd, r;

    decimal64ToNumber((decimal64*)(v.v1), &dv);
    decimal64ToNumber((decimal64*)(d.v.v1), &dd);

    dec_cxt.status = 0;
    (numerical_ops[idx])(&r, &dv, &dd, &dec_cxt);

    if (dec_cxt.status & DEC_Errors)
    {
        if (dec_cxt.status & DEC_IEEE_854_Division_by_zero)
            throw USER_EXCEPTION2(FOAR0001, "xs:decimal");
        else if (dec_cxt.status & DEC_IEEE_854_Overflow)
            throw USER_EXCEPTION2(FOAR0002, "xs:decimal overflow");
        else if (dec_cxt.status & DEC_IEEE_854_Underflow)
        {
            dec_cxt.status = 0;
            decNumberZero(&r);
        }
        else 
            throw USER_EXCEPTION2(SE1003, "numerical operation with xs:decimal");
    }

    decimal64FromNumber((decimal64*)(res.v.v1), &r, &dec_cxt);
    return res;
}

int xs_decimal_t::compare(const xs_decimal_t & d) const
{
    decNumber dv, dd, r;

    decimal64ToNumber((decimal64*)(v.v1), &dv);
    decimal64ToNumber((decimal64*)(d.v.v1), &dd);

    dec_cxt.status = 0;
    decNumberCompare(&r, &dv, &dd, &dec_cxt);

    U_ASSERT(!(dec_cxt.status & DEC_Errors));

    if (decNumberIsZero(&r)) return 0;
    else if (decNumberIsNegative(&r)) return -1;
    else return 1;
}

bool xs_decimal_t::is_zero() const
{
    decNumber r;
    dec_cxt.status = 0;
    decimal64ToNumber((decimal64*)(v.v1), &r);
    return decNumberIsZero(&r);
}

void xs_decimal_t::print() const
{ 
    printf("%s", get_c_str(tr_globals::mem_str_buf)); 
}
