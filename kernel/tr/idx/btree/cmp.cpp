/*
 * File:  cmp.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "btstruct.h"
#include "counted_ptr.h"
#include "XMLDateTime.h"


/* returns 1 if k1>k2; -1 if k1<k2; 0 if k1=k2 */
int bt_cmp_key(const bt_key& k1, const bt_key& k2)
{
    if (k1.type != k2.type)
        throw USER_EXCEPTION2(SE1008, "Attempt to compare keys of different types");

    double x;

    switch (k1.type)
    {
        case xs_integer	: return (k1.v.i_v - k2.v.i_v);
        case xs_float	: x = k1.v.f_v - k2.v.f_v;
                          if (x > 0.0) return 1;
                          else if (x < 0.0) return -1;
                          else return 0;
        case xs_double	: x = k1.v.d_v - k2.v.d_v;
                          if (x > 0.0) return 1;
                          else if (x < 0.0) return -1;
                          else return 0;
        case xs_string	: return strcmp(k1.v.s_v, k2.v.s_v);
	case xs_date:
	case xs_dateTime:
	case xs_time:
				return XMLDateTime::compare( XMLDateTime(k1.v.dt_v, k1.type), XMLDateTime(k2.v.dt_v, k2.type));

	case xs_yearMonthDuration:
	case xs_dayTimeDuration:
			return XMLDateTime::compare( XMLDateTime(k1.v.dur_v, k1.type), XMLDateTime(k2.v.dur_v, k2.type));

        default			: throw USER_EXCEPTION2(SE1008, "Unsupported type of index");
	}
}

/* returns 1 if o1>o2; -1 if o1<o2; 0 if o1=o2 */
int bt_cmp_obj(object o1, object o2) {
	int	i=0;
	unsigned char* c1 = (unsigned char*)&o1;
	unsigned char* c2 = (unsigned char*)&o2;
	/* byte-wise comparison */
	while(i<sizeof(object)) {
		if ((*c1)^(*c2)) {
			if ((*c1) > (*c2)) return 1;
			else return -1;
		}
		i++;
		c1++;
		c2++;
	}
	return 0;
}

/* make comparison of the key table element and key as bt_key struct;
   handle both fixed and variable size key cases;
   returns 1 if 1>2; -1 if 1<2; 0 if 1=2 */
int bt_cmp_key(char* pg, const void* tab_el, const bt_key& k2)
{
    /* assume we have correct page, conformant with key k2 */
    double x;

    switch (k2.type)
    {
        case xs_integer	: return (*(int*)tab_el - k2.v.i_v);
        case xs_float	: x = *(float*)tab_el - k2.v.f_v;
                          if (x > 0.0) return 1;
                          else if (x < 0.0) return -1;
                          else return 0;
        case xs_double	: x = *(double*)tab_el - k2.v.d_v;
                          if (x > 0.0) return 1;
                          else if (x < 0.0) return -1;
                          else return 0;
        case xs_string	: {
                              char *head = pg + *(shft*)tab_el;
                              int size1 = *((shft*)tab_el + 1);
                              int size2 = strlen(k2.v.s_v);

                              int res = memcmp(head, k2.v.s_v, s_min(size1, size2));
                              if (res != 0) return res;
                              else return size1 - size2;
                          }
	
	case xs_date:
	case xs_dateTime:
	case xs_time: 	
			return XMLDateTime::compare( XMLDateTime(*(xs_packed_datetime*)tab_el, k2.type), XMLDateTime(k2.v.dt_v, k2.type));

	case xs_yearMonthDuration:
	case xs_dayTimeDuration:
			return XMLDateTime::compare( XMLDateTime(*(xs_packed_duration*)tab_el, k2.type), XMLDateTime(k2.v.dur_v, k2.type));

        default			: throw USER_EXCEPTION2(SE1008, "Unsupported type of index");
	}
}
