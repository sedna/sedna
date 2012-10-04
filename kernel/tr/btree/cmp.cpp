/*
 * File:  cmp.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "auxiliary/counted_ptr.h"

#include "tr/btree/btstruct.h"
#include "tr/executor/base/XMLDateTime.h"
#include "tr/tr_globals.h"


/* returns 1 if k1>k2; -1 if k1<k2; 0 if k1=k2 */
int bt_cmp_key(const bt_key& k1, const bt_key& k2)
{
    if (k1.type != k2.type)
        throw USER_EXCEPTION2(SE1008, "Attempt to compare keys of different types");

    switch (k1.type)
    {
        case xs_integer	:
        {
            if(k1.v.i_v == k2.v.i_v) return 0;
            return (k1.v.i_v > k2.v.i_v ? 1 : -1);
        }
        case xs_float	:
        {
            if(k1.v.f_v == k2.v.f_v) return 0;
            return (k1.v.f_v > k2.v.f_v ? 1 : -1);
        }
        case xs_double	:
        {
            if(k1.v.d_v == k2.v.d_v) return 0;
            return (k1.v.d_v > k2.v.d_v ? 1 : -1);
        }
        case xs_string	: return sign(strcmp(k1.v.s_v, k2.v.s_v));
        case xs_date    :
        case xs_dateTime:
        case xs_time    :
                          return XMLDateTime::compare( XMLDateTime(k1.v.dt_v, k1.type), XMLDateTime(k2.v.dt_v, k2.type));
        case xs_yearMonthDuration:
        case xs_dayTimeDuration:
		                  return XMLDateTime::compare( XMLDateTime(k1.v.dur_v, k1.type), XMLDateTime(k2.v.dur_v, k2.type));

        default			: throw USER_EXCEPTION2(SE1008, "Unsupported type of index");
	}
}

/* returns 1 if o1>o2; -1 if o1<o2; 0 if o1=o2 */
template<typename object>
int bt_cmp_obj_tmpl(const object &o1, const object &o2) {
	if (o2 < o1) return 1;
	if (o1 < o2) return -1;
	return 0;

/*
	int	i=0;
	unsigned char* c1 = (unsigned char*)&o1;
	unsigned char* c2 = (unsigned char*)&o2;
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
*/
}

/* make comparison of the key table element and key as bt_key struct;
   handle both fixed and variable size key cases;
   returns 1 if 1>2; -1 if 1<2; 0 if 1=2 */
int bt_cmp_key(char* pg, const void* tab_el, const bt_key& k2)
{
    /* assume we have correct page, conformant with key k2 */
    switch (k2.type)
    {
        case xs_integer	:
        {
            int64_t k1 = *(int64_t*)tab_el;
            if(k1 == k2.v.i_v) return 0;
            return (k1 > k2.v.i_v ? 1 : -1);
        }
        case xs_float	:
        {
            float k1 = *(float*)tab_el;
            if(k1 == k2.v.f_v) return 0;
            return (k1 > k2.v.f_v ? 1 : -1);
        }
        case xs_double	:
        {
            double k1 = *(double*)tab_el;
            if(k1 == k2.v.d_v) return 0;
            return (k1 > k2.v.d_v ? 1 : -1);
        }
        case xs_string	: {
                              char *head = pg + *(shft*)tab_el;
                              int size1 = *((shft*)tab_el + 1);
                              int size2 = strlen(k2.v.s_v);

                              int res = memcmp(head, k2.v.s_v, s_min(size1, size2));
                              if (res != 0) return sign(res); /// There is no guarantee that memcmp returns 1, -1, 0!
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


#define MAKE_IMPLS(t) template int bt_cmp_obj_tmpl<t>(const t &o1, const t &o2);

#include "tr/btree/make_impl.h"
