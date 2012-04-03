/*
 * File:  utils.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <math.h>

#include "common/sedna.h"

#include "common/utils.h"
#include "common/u/uutils.h"
#include "common/errdbg/d_printf.h"
#include "common/u/uhdd.h"

#ifndef _WIN32
#include <strings.h>
#endif


using namespace std;

string to_string(u_timeb t)
{
    char buf[80];
    sprintf(buf, "%d.%03ld", (int)t.time, t.millitm);
    return string(buf);
}

double power(double a, double b)
{
    return exp(b * log(a));
}

#ifndef _WIN32
int _isnan(double x)
{
    return isnan(x);
}
#endif
    
void elim_disturb(void *base, size_t num, size_t width, compare_fun compare)
{
    int pos = -1;
    bool growing = true;
    int i = 0;
    for (i = 0; i < (int)num - 1; i++)
        if (compare((char*)base + i * width, (char*)base + (i + 1) * width) > 0)
        {
            if (i + 2 < (int)num)
            {
                if (compare((char*)base + i * width, (char*)base + (i + 2) * width) > 0)
                    pos = i;
                else { pos = i + 1; growing = false; }
            }
            else { pos = i + 1; growing = false; }

            break;
        }

    if (pos == -1) return;

    char * tmp = se_new char[width];

    if (growing)
    {
        for (i = pos; i < (int)num - 1; i++)
            if (compare((char*)base + i * width, (char*)base + (i + 1) * width) > 0)
            {
                /* swap*/
                memcpy(tmp, (char*)base + i * width, width);
                memcpy((char*)base + i * width, (char*)base + (i + 1) * width, width);
                memcpy((char*)base + (i + 1) * width, tmp, width);
            }
            else break;
    }
    else
    {
        for (i = pos; i > 0; i--)
            if (compare((char*)base + (i - 1) * width, (char*)base + i * width) > 0)
            {
                /* swap*/
                memcpy(tmp, (char*)base + i * width, width);
                memcpy((char*)base + i * width, (char*)base + (i - 1) * width, width);
                memcpy((char*)base + (i - 1) * width, tmp, width);
            }
            else break;
    }

    delete [] tmp;
}

void elim_disturb2(void *base, size_t num, size_t width, compare_fun compare)
{
    size_t lb = 0, rb = 0, med = 0, pos = 0;
    int cmp = 0;

    lb = 1; 
    rb = num;

    if (num == 1) return;

    while (true)
    {
        if (rb - lb == 1)
        {
            if (compare(base, (char*)base + lb * width) <= 0) pos = lb;
            else
            {
                if (compare(base, (char*)base + rb * width) <= 0) pos = rb;
                else pos = rb + 1;
            }
            break;
        }

        med = (rb + lb) / 2;
        cmp = compare(base, (char*)base + med * width);

        if (cmp < 0) rb = med;
        else 
            if (cmp > 0) lb = med;
            else
            {
                pos = med;
                break;
            }
    }

    if (pos == 1) return;

    /* swap*/
    char * tmp = se_new char[width];

    memcpy(tmp, base, width);
    memmove(base, (char*)base + width, width * (pos - 1));
    memcpy((char*)base + width * (pos - 1), tmp, width);

    delete [] tmp;
}

std::string trim(const std::string& str)
{
    std::string::size_type start = str.find_first_not_of(" \n\t\r");
    std::string::size_type end   = str.find_last_not_of(" \n\t\r");
    if (start == std::string::npos || end == std::string::npos)
        return "";
    return str.substr(start, end - start + 1);
}


