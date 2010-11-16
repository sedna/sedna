/*
 * File:  nid.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _NID_H
#define _NID_H

#include "common/sedna.h"

/*	number of symbols encodable in the cell of the alphabet;
	no more than 255
 */
#define ALPHABET_SIZE 255

/*	max incrementing number
 */
#define MAX_LETTER 254

/* default letter fo resizing*/
#define DEF_LETTER 25


/* fraction number x/y */
struct fnumber {
private:
	int		x;
	int		y;
public:
	fnumber() {
		x=0;
		y=1;
	}
	fnumber(int the_x, int the_y) {
		if (the_x > the_y || the_x<0 || the_y<=0) 
			throw USER_EXCEPTION2(SE1003, "in fnumber");
		x = the_x;
		y = the_y;
	}
	int getx() {
		return x;
	};
	int gety() {
		return y;
	};
	void setx(int the_x) {
		if (the_x < 0 || the_x > y)
			throw USER_EXCEPTION2(SE1003, "Bad paramters in fnumber::setx");
		x = the_x;
	};
	void sety(int the_y) {
		if (the_y <= 0 || x > the_y)
			throw USER_EXCEPTION2(SE1003, "Bad paramters in fnumber::sety");
		y = the_y;
	};
};

typedef struct fnumber fnumber;

struct t_prefix {
	unsigned char*  prefix; // was char*, now unsigned char*
	shft            size;   // was int, now unsigned shft
	t_prefix() {
		prefix=NULL;
		size=0;
	};
	t_prefix(unsigned char* p, shft s) {
		prefix=p;
		size=s;
	};
};

typedef struct t_prefix t_prefix;



#endif

