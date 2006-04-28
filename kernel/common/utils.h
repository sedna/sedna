/*
 * File:  utils.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __UTILS_H
#define __UTILS_H

#include <string>
#include <vector>

#include "sedna.h"


std::string int2string(int value);

/* datatypes and functions for working with time (time in sec + time in millisec)*/
#ifndef _WIN32
typedef  timeb u_timeb;
#else
typedef _timeb u_timeb;
#endif

void u_ftime(u_timeb *t);

u_timeb operator -(u_timeb t1, u_timeb t2);

u_timeb operator +(u_timeb t1, u_timeb t2);



std::string to_string(u_timeb t);

double power(double a, double b);

#ifndef _WIN32
int _isnan(double x);
#endif
    
template<class T>
std::vector<T> vector_concat(const std::vector<T>& v1, const std::vector<T> &v2)
{
    std::vector<T> res;

    res.reserve(v1.size() + v2.size());

    typename std::vector<T>::const_iterator it;
    for (it = v1.begin(); it != v1.end(); it++) res.push_back(*it);
    for (it = v2.begin(); it != v2.end(); it++) res.push_back(*it);

    return res;
}


/* function is used for eliminating disturbance from sorted array*/
/* parameters are like parameters for qsort*/
#ifdef _WIN32
typedef int (__cdecl *compare_fun)(const void* elem1, const void* elem2);
#else
typedef int (*compare_fun)(const void* elem1, const void* elem2);
#endif
void elim_disturb(void *base, size_t num, size_t width, compare_fun compare);
void elim_disturb2(void *base, size_t num, size_t width, compare_fun compare);

std::string trim(std::string str);

std::string get_sedna_data_path(std::string cfg_text);
#endif
