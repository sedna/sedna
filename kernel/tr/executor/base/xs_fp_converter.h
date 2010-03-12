/*
 * File: xs_fp_converter.h
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

#ifndef _FLOATINGPOINTCONVERTER_H
#define _FLOATINGPOINTCONVERTER_H

/**
 * These are utilities for formatting of floating-point numbers as strings.
 * 
 * The algorithm for converting a floating point number to a string is taken from Guy L. Steele and
 * Jon L. White, How to Print Floating-Point Numbers Accurately, ACM SIGPLAN 1990. It is algorithm
 * FPP2 from that paper. There are three separate implementations of the algorithm:

 * One using long arithmetic and generating non-exponential output representations
 * One using BigInteger arithmetic and generating non-exponential output representation
 * One using BigInteger arithmetic and generating exponential output representations
 * 
 * The choice of method depends on the value of the number being formatted.
 * 
 * The module contains some residual code (mainly the routine for formatting integers) from the class
 * AppenderHelper by Jack Shirazi in the O'Reilly book Java Performance Tuning. The floating point routines
 * in that module were found to be unsuitable, since they used floating point arithmetic which introduces
 * rounding errors.
 * 
 * There are several reasons for doing this conversion within Saxon, rather than leaving it all to Java.
 * Firstly, there are differences in the required output format, notably the absence of ".0" when formatting
 * whole numbers, and the different rules for the range of numbers where exponential notation is used.
 * Secondly, there are bugs in some Java implementations, for example JDK outputs 0.001 as 0.0010, and
 * IKVM/GNU gets things very wrong sometimes. Finally, this implementation is faster for "everyday" numbers,
 * though it is slower for more extreme numbers. It would probably be reasonable to hand over formatting
 * to the Java platform (at least when running the Sun JDK) for exponents outside the range -7 to +7.
 */



#include "common/sedna.h"

char *get_xs_double_lexical_representation_Saxon(char* s, double value);
char *get_xs_float_lexical_representation_Saxon(char* s, float value);

#endif

