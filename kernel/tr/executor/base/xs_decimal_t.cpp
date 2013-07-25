/*
 * File:  xs_decimal_t.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <math.h>

#include "common/sedna.h"

#include "tr/executor/base/xs_decimal_t.h"
#include "tr/executor/base/xs_helper.h"
#include "tr/executor/base/PPBase.h"

extern "C" {
#include "tr/executor/base/decNumber/decimal128.h"
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
    decContextDefault(&dec_cxt, DEC_INIT_DECIMAL128); // initialize
    dec_cxt.traps = 0;
}

/*******************************************************************************
 * SET FUNCTIONS
 ******************************************************************************/
void xs_decimal_t::set(int64_t a)
{
    get_xs_integer_lexical_representation(executor_globals::mem_str_buf, a);
    this->set(executor_globals::mem_str_buf, true);
}

void xs_decimal_t::set(float a)
{
    this->set((double)a);
}

void xs_decimal_t::set(double a)
{
    if (u_is_nan(a) || u_is_neg_inf(a) || u_is_pos_inf(a))
        throw XQUERY_EXCEPTION2(FOCA0002, "Cannot convert to xs:decimal type");

    double abs_a = a < 0.0 ? -a : a;
    if (abs_a > XS_DECIMAL_MAX_VALUE_DOUBLE || (a != 0 && abs_a < XS_DECIMAL_MIN_VALUE_DOUBLE))
        throw XQUERY_EXCEPTION2(FOCA0001, "Cannot convert to xs:decimal type");

    sprintf(executor_globals::mem_str_buf, "%E", a);
    this->set(executor_globals::mem_str_buf, false);
}

void xs_decimal_t::set(bool a)
{
    dec_cxt.status = 0;
    if (a)
        decimal128FromString((decimal128*)(v.v1), "1.0", &dec_cxt);
    else
        decimal128FromString((decimal128*)(v.v1), "0.0", &dec_cxt);
}

static const unsigned char decimal_allowed[16] = {0x00, 0x00, 0x00, 0x00,
                                                  0x00, 0x16, 0xFF, 0xC0, 
                                                  0x00, 0x00, 0x00, 0x00, 
                                                  0x00, 0x00, 0x00, 0x00};
#define IS_BYTE_DECIMAL_ALLOWED(byte) \
    (byte & 0x80 ? 0 : (decimal_allowed[(byte >> 3)] & (0x80 >> (byte & 7))))

void xs_decimal_t::set(const char *a, bool xs_compliant)
{
    char* norm_a = NULL;
    decNumber dv;
    try
    {
        stmt_str_buf res(1);
        collapse_string_normalization(a, res);
        norm_a = res.get_str();
        if (strcmp(norm_a, "NaN") == 0)
            throw XQUERY_EXCEPTION2(FORG0001, "Cannot convert string \"NaN\" to xs:decimal");

        if (xs_compliant)
        {
            for (unsigned char *c = (unsigned char*) norm_a; *c != '\0'; c++)
               if (!IS_BYTE_DECIMAL_ALLOWED(*c))
                    throw XQUERY_EXCEPTION2(FORG0001, "Cannot convert to xs:decimal type");
        }

        dec_cxt.status = 0;
        decNumberFromString(&dv, norm_a, &dec_cxt);
        str_buf_base::free_str(norm_a);
        norm_a = NULL;
    }
    catch(SednaUserException &ex)
    {
        if (norm_a != NULL)
        {
            str_buf_base::free_str(norm_a);
            norm_a = NULL;
        }
        throw ex;
    }


    if (dec_cxt.status & DEC_Errors)
    {
        //printf("0x%x %s\n", dec_cxt.status, decContextStatusToString(&dec_cxt));
        if (dec_cxt.status & DEC_IEEE_854_Overflow)
            throw XQUERY_EXCEPTION2(FOAR0002, "xs:decimal overflow");
        else if (dec_cxt.status & DEC_IEEE_854_Underflow)
            decimal128FromString((decimal128*)(v.v1), "0.0", &dec_cxt);
        else // DEC_IEEE_854_Division_by_zero | DEC_IEEE_854_Invalid_operation
            throw XQUERY_EXCEPTION2(FORG0001, "Cannot convert to xs:decimal type");
    }
    else if (dec_cxt.status & (DEC_Information ^ (DEC_Rounded | DEC_Inexact)))
    {
        throw XQUERY_EXCEPTION2(FOCA0006, "Cannot convert to xs:decimal type");
    }

    dec_cxt.status = 0;
    //decNumberNormalize(&dv, &dv, &dec_cxt);
    decimal128FromNumber((decimal128*)(v.v1), &dv, &dec_cxt);
}

/*******************************************************************************
 * GET FUNCTIONS
 ******************************************************************************/
int64_t xs_decimal_t::get_int   () const
{
    decNumber dv, r;
    decimal128ToNumber((decimal128*)(v.v1), &dv);
    dec_cxt.status = 0;
	enum rounding old = dec_cxt.round;
	dec_cxt.round = DEC_ROUND_DOWN;
    decNumberToIntegralValue(&r, &dv, &dec_cxt);
	dec_cxt.round = old;
    decNumberToString(&r, executor_globals::mem_str_buf);
    return c_str2xs_integer(executor_globals::mem_str_buf);
}

float   xs_decimal_t::get_float () const
{
	decimal128ToString((decimal128*)(v.v1), executor_globals::mem_str_buf);
    return c_str2xs_float(executor_globals::mem_str_buf);
}

double  xs_decimal_t::get_double() const
{
	decimal128ToString((decimal128*)(v.v1), executor_globals::mem_str_buf);
    return c_str2xs_double(executor_globals::mem_str_buf);
}

bool    xs_decimal_t::get_bool  () const
{
    decNumber r;
    dec_cxt.status = 0;
    decimal128ToNumber((decimal128*)(v.v1), &r);
    return !(decNumberIsZero(&r) || decNumberIsNaN(&r));
}

char *xs_decimal_t::get_c_str(char *buf) const
{
    if (is_zero()) strcpy(buf, "0");
    else 
    {
        decNumber dv;
        dec_cxt.status = 0;
        decimal128ToNumber((decimal128*)(v.v1), &dv);
        decNumberNormalize(&dv, &dv, &dec_cxt);
        decNumberToString(&dv, buf);

        int E_pos = -1;
        int dot_pos = -1;
        char *start = buf;
        int max_len = MAX_ATOMIC_LEX_REPR_SIZE;
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
xs_decimal_t xs_decimal_t::operator - () const
{
    xs_decimal_t res;
    decNumber dv, r;

    decimal128ToNumber((decimal128*)(v.v1), &dv);
    dec_cxt.status = 0;
    decNumberMinus(&r, &dv, &dec_cxt);

    if (dec_cxt.status & DEC_Errors)
    {
        if (dec_cxt.status & DEC_IEEE_854_Division_by_zero)
            throw XQUERY_EXCEPTION2(FOAR0001, "xs:decimal");
        else if (dec_cxt.status & DEC_IEEE_854_Overflow)
            throw XQUERY_EXCEPTION2(FOAR0002, "xs:decimal overflow");
        else if (dec_cxt.status & DEC_IEEE_854_Underflow)
        {
            dec_cxt.status = 0;
            decNumberZero(&r);
        }
        else 
            throw USER_EXCEPTION2(SE1003, "numerical operation with xs:decimal");
    }

    decimal128FromNumber((decimal128*)(res.v.v1), &r, &dec_cxt);
    return res;
}

xs_decimal_t xs_decimal_t::numerical_operation(const xs_decimal_t & d, int idx) const
{
    xs_decimal_t res;
    decNumber dv, dd, r;

    decimal128ToNumber((decimal128*)(v.v1), &dv);
    decimal128ToNumber((decimal128*)(d.v.v1), &dd);

    dec_cxt.status = 0;
    (numerical_ops[idx])(&r, &dv, &dd, &dec_cxt);

    if (dec_cxt.status & DEC_Errors)
    {
        if (dec_cxt.status & DEC_IEEE_854_Division_by_zero)
            throw XQUERY_EXCEPTION2(FOAR0001, "xs:decimal");
        else if (dec_cxt.status & DEC_IEEE_854_Overflow)
            throw XQUERY_EXCEPTION2(FOAR0002, "xs:decimal overflow");
        else if (dec_cxt.status & DEC_IEEE_854_Underflow)
        {
            dec_cxt.status = 0;
            decNumberZero(&r);
        }
        else 
            throw USER_EXCEPTION2(SE1003, "numerical operation with xs:decimal");
    }

    decimal128FromNumber((decimal128*)(res.v.v1), &r, &dec_cxt);
    return res;
}

int xs_decimal_t::compare(const xs_decimal_t & d) const
{
    decNumber dv, dd, r;

    decimal128ToNumber((decimal128*)(v.v1), &dv);
    decimal128ToNumber((decimal128*)(d.v.v1), &dd);

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
    decimal128ToNumber((decimal128*)(v.v1), &r);
    return decNumberIsZero(&r);
}

bool xs_decimal_t::is_negative() const
{
    decNumber r;
    dec_cxt.status = 0;
    decimal128ToNumber((decimal128*)(v.v1), &r);
    return decNumberIsNegative(&r);
}

void xs_decimal_t::print() const
{ 
    printf("%s", get_c_str(executor_globals::mem_str_buf)); 
}


xs_decimal_t xs_decimal_t::abs() const
{
    xs_decimal_t res;
    decNumber r1, r2;
    dec_cxt.status = 0;
    decimal128ToNumber((decimal128*)(v.v1), &r1);
    decNumberAbs(&r2, &r1, &dec_cxt);
    decimal128FromNumber((decimal128*)(res.v.v1), &r2, &dec_cxt);
    return res;
}

xs_decimal_t xs_decimal_t::ceil() const
{
    xs_decimal_t res;
    decNumber dv, integral, one, r;

    dec_cxt.status = 0;
    decimal128ToNumber((decimal128*)(v.v1), &dv);
	enum rounding old = dec_cxt.round;
	dec_cxt.round = DEC_ROUND_DOWN;
    decNumberToIntegralValue(&integral, &dv, &dec_cxt);
	dec_cxt.round = old;

    dec_cxt.status = 0;
    decNumberCompare(&r, &dv, &integral, &dec_cxt);

    U_ASSERT(!(dec_cxt.status & DEC_Errors));

    if (!decNumberIsZero(&r))
    {
        if (!decNumberIsNegative(&dv))
        {
            decNumberFromString(&one, "1", &dec_cxt);
            decNumberAdd(&integral, &integral, &one, &dec_cxt);
        }
    }

    decimal128FromNumber((decimal128*)(res.v.v1), &integral, &dec_cxt);
    return res;
}

xs_decimal_t xs_decimal_t::floor() const
{
    xs_decimal_t res;
    decNumber dv, integral, one, r;

    dec_cxt.status = 0;
    decimal128ToNumber((decimal128*)(v.v1), &dv);
	enum rounding old = dec_cxt.round;
	dec_cxt.round = DEC_ROUND_DOWN;
    decNumberToIntegralValue(&integral, &dv, &dec_cxt);
	dec_cxt.round = old;

    dec_cxt.status = 0;
    decNumberCompare(&r, &dv, &integral, &dec_cxt);

    U_ASSERT(!(dec_cxt.status & DEC_Errors));

    if (!decNumberIsZero(&r))
    {
        if (decNumberIsNegative(&dv))
        {
            decNumberFromString(&one, "1", &dec_cxt);
            decNumberSubtract(&integral, &integral, &one, &dec_cxt);
        }
    }

    decimal128FromNumber((decimal128*)(res.v.v1), &integral, &dec_cxt);
    return res;
}

xs_decimal_t xs_decimal_t::round() const
{
    xs_decimal_t c(this->ceil());
    xs_decimal_t f(this->floor());
    if ((c - *this) > (*this - f)) return f;
    else return c;
}

xs_decimal_t xs_decimal_t::round_half_to_even(int64_t precision) const
{
    xs_decimal_t m_i, m_f;
    int64_t y = 1;

    int64_t p = precision < 0 ? -precision : precision;
    for (int64_t j = 0; j < p; j++) y *= 10;

    if (precision < 0)
    {
        m_f = modf(*this / xs_decimal_t(y), &m_i);
        return m_i * y;
    }
    else
    {
        xs_decimal_t i;
        int s = 0;
        xs_decimal_t d = this->is_negative() ? (s = -1, -*this) : (s = 1, *this);

        xs_decimal_t f = modf(d, &i);

        if (y != 1) {
            m_f = modf(f * y, &m_i);
        } else {
            m_f = f;
            m_i = i;
            i.set((int64_t)0);
        }

        if (m_f == xs_decimal_t(0.5))
        {
            if ((m_i.get_int() % 2) == 1)
            {
                m_i = m_i + xs_decimal_t((int64_t)1);
            }
        }
        else if (m_f > xs_decimal_t(0.5))
        {
            m_i = m_i + xs_decimal_t((int64_t)1);
        }

        return xs_decimal_t((int64_t)s) * (i + m_i / y);
    }
}


xs_decimal_t modf(const xs_decimal_t &x, xs_decimal_t* /*out*/intptr)
{
    decNumber dv, i;
    decimal128ToNumber((decimal128*)(x.v.v1), &dv);
    dec_cxt.status = 0;
	enum rounding old = dec_cxt.round;
	dec_cxt.round = DEC_ROUND_DOWN;
    decNumberToIntegralValue(&i, &dv, &dec_cxt);
	dec_cxt.round = old;

    decimal128FromNumber((decimal128*)(intptr->v.v1), &i, &dec_cxt);
    return x - *intptr;
}
