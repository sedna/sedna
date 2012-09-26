/*
 * File:  numb_scheme.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _NUMB_SCHEME_H
#define _NUMB_SCHEME_H

#include <ostream>

#include "common/sedna.h"
#include "common/xptr/xptr.h"
#include "tr/nid/nid.h"
#include "tr/crmutils/crmbase.h"

#define MAXINTERNALPREFIX 11

/* typedef int64_t n_scheme; */
struct t_nid {
    unsigned char prefix[11];  /* actually union; if the size of string is above MAXINTERNALPREFIX
                                  keeps xptr to string (first 8 bytes) and size of nid (2 last bytes) */
    unsigned char size;        /* keeps the size of string if it is above MAXINTERNALPREFIX, 0 otherwise */
};

typedef struct t_nid t_nid;

extern int nid_block_count;
extern t_nid NIDNULL;

struct nid_hint_t {
    int size;
    int increment;
};

extern nid_hint_t * sizehnt;

inline
shft nid_get_size(const t_nid * nid) {
    const unsigned char size = nid->size;

    if (size == 0) {
        return *(shft*) ((nid->prefix) + sizeof(xptr));
    } else {
        return size;
    }
}

bool nid_parse(const xptr nid, xptr * prefix, shft * size);

inline
bool nid_is_external(const t_nid * nid) {
    return (nid->size == 0);
}


/* set static parameter of alphabet division proportion */
void		nid_set_proportion(fnumber p);

/* get nid of the node */
t_nid		nid_get_nid(xptr node);

/* save nid in the node */
void		nid_assign(xptr dsc, t_prefix p);

/* read the prefix contents given nid structure */
t_prefix	nid_get_prefix(t_nid the_nid);

/* get the maximum limit for prefixes of children */
/* DEPRECATED t_prefix	nid_child_limit(t_nid id, t_prefix p);*/

/* doc-order comparison */
int			nid_cmp(xptr node1, xptr node2);


//---------------------------------------------------------
//returns 2  if node1 is descandant of node2;
//returns -2 if node1 is ancestor of node2;
//returns 1  if node1 is after node2 in d.o.;
//returns -1 if node1 is before node2 in d.o.;
//returns 0  otherwise (node1 and node2 are the same)
int			nid_cmp_effective(xptr node1, xptr node2);

/* ancestor comparison (true if node1 is ancestor of node2) */
bool		nid_ancestor(xptr node1, xptr node2);

/* descendant comparison (true if node1 is descendant of node2) */
bool		nid_descendant(xptr node1, xptr node2);

/* generate nid between given left and rigth */
void		nid_create_between(xptr left, xptr right, xptr result);

/* generate nid for new outmost right sibling */
void		nid_create_right(xptr left, xptr parent, xptr result);

/* generate nid for new outmost left sibling */
void		nid_create_left(xptr right, xptr parent, xptr result);

/* generate nid for the first child of parent argument */
void		nid_create_child(xptr parent, xptr result);

/* generate nid for the root */
void		nid_create_root(xptr result, bool persistent);

/* free nid */
void		nid_delete(xptr node);

/* prints nid */
void		nid_print(xptr node, se_ostream& c);
void    	nid_print(xptr node, std::ostream& c);
void		nid_print(xptr node);

/*
 * All temporary blocks are deleted on the end of the kernel statement
 * including TMPNIDBLK. So it must be nulled when the last temporary
 * schema node is deleted.
 */
void    	nid_on_kernel_statement_end();


/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
/*                   Internal functions                            */
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/

/* wrapper upon nid generation functions depending on value of "p" */
char*		nid_extend_bitmap();

#endif /* _NUMB_SCHEME_H */
