/*
 * File:  nodes.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _NODES_H
#define _NODES_H

/*
 structures describing internal representation of XML data
*/

#include "common/sedna.h"

#include "common/base.h"
#include "tr/vmm/vmm.h"
#include "common/sm_vmm_data.h"
#include "tr/nid/numb_scheme.h"
#include "tr/cat/catptr.h"
#include "tr/tr_base.h"

struct n_dsc;

struct node_blk_hdr
{
  private :
    vmm_sm_blk_hdr sm_vmm;	/* sm/vmm parameters */
  public:
    xptr pblk;				/* previoius block */
    xptr nblk;				/* next block */

    schema_node_xptr snode;	/* pointer to the scm node; 0 for text blocks */
    shft dsc_size;			/* size of the descriptor in bytes */
    shft	desc_first;		/* shift to the first descriptor in block */
    shft	desc_last;		/* shift to the last descriptor in block */
    shft    count;          /* total number of descriptors in block */
    shft	free_first;		/* shift to the first empty space in block */

    shft indir_count; /* total number of indirection records in block*/
    shft free_first_indir; /* shift to the first free indirection pointer*/
    xptr pblk_indir; /* previous block with free indirection space*/
    xptr nblk_indir; /* next block with free indirection space*/

    static node_blk_hdr * init(void *p, shft dsc_size);

    void clear();

    inline n_dsc * getFirstNode() { return (desc_first == 0) ? NULL : (n_dsc *) ((char *) this + desc_first); }
    inline n_dsc * getLastNode()  { return (desc_last == 0) ? NULL : (n_dsc *) ((char *) this + desc_last);  }
    inline n_dsc * getFreeNode()  { return (free_first == 0) ?  NULL : (n_dsc *) ((char *) this + free_first);  }
};



/* Decriptor classification
   ------------------------
                  n_dsc                        s_dsc
                 /  |  \  \___
                /   |   \     \
           e_dsc  t_dsc  a_dsc d_dsc

 */

/* Converts node type to a string */
inline const char*
type2string(t_item type) {

    switch(type)
    {
    case element:                      return "element";
    case text:                         return "text";
    case attribute:                    return "attribute";
    case xml_namespace:                return "namespace";
    case document: case virtual_root:  return "document";
    case comment:                      return "comment";
    case pr_ins:                       return "processing-instruction";
    case cdata:                        return "cdata";
    }
    return "unknown";
}

/* node descriptor (abstract structure) */
struct n_dsc {

    t_nid   nid;        /* ordering scheme number */
    xptr    pdsc;       /* pointer to the record in the table of indirect addresses */
    xptr    ldsc;       /* pointer to the descriptor of left sibling item */
    xptr    rdsc;       /* pointer to the descriptor of right sibling item */
	xptr    indir;      /* record in indirection table*/
    shft    desc_next;  /* shift to the next descriptor in block */
    shft    desc_prev;  /* shift to the previous descriptor in block */

};

/* returns size of xtype in bytes (0 if size of datatype can vary) */
int xmlscm_type_size(xmlscm_type xtype);

inline bool is_string_type(xmlscm_type xtype)
{
    return (xtype == xs_string        ||
            xtype == xs_untypedAtomic ||
            xtype == xs_anyURI        ||
            (xs_normalizedString <= xtype && xtype <= xs_ENTITY));
}
inline bool is_numeric_type(xmlscm_type xtype)
{
    return (xs_float <= xtype && xtype <= xs_integer) ||
           (xs_nonPositiveInteger <= xtype && xtype <= xs_positiveInteger);
}
inline bool is_temporal_type(xmlscm_type xtype)
{
    return (xs_dateTime <= xtype && xtype <= xs_gMonth);
}
inline bool is_derived_from_xs_string(xmlscm_type xtype)
{
    return (xs_normalizedString <= xtype && xtype <= xs_ENTITY);
}
inline bool is_derived_from_xs_integer(xmlscm_type xtype)
{
    return (xs_nonPositiveInteger <= xtype && xtype <= xs_positiveInteger);
}
inline bool is_fixed_size_type(xmlscm_type xtype)
{
    return (xs_dateTime <= xtype && xtype <= xs_boolean) ||
           (xs_nonPositiveInteger <= xtype && xtype <= xs_positiveInteger);
}
inline bool is_primitive(xmlscm_type xtype)
{
    return (xs_dateTime <= xtype && xtype <= xs_NOTATION);
}

/* Descriptor of element node */
struct e_dsc : public n_dsc {
	xmlscm_type		type;		/* element type according to the scheme */
};

/* Descriptor of text-enabled node */
struct t_dsc : public n_dsc {
    union text_data_t {
        struct text_pointer_t {
            xptr p;					      /* pointer to the content */
//            strsize_t content_size; // FIXME : should be this
            size_t size;
        } lsp; /* long text pointer */
        char st[sizeof(text_pointer_t)]; /* short text itself */
    } data;

    int8_t ss; /* size of stored text */
};

enum { TEXT_IN_PSTR = -1, TEXT_IN_PSTR_LONG = -2 };

const size_t max_indsc_text_size = sizeof(t_dsc::text_data_t::text_pointer_t);

inline xptr textDereferance(xptr ptr)
{
    return block_xptr(ptr) + * (shft *) XADDR(ptr);
}

inline xptr textDereferenceCP(xptr ptr)
{
    CHECKP(ptr);
    return block_xptr(ptr) + * (shft *) XADDR(ptr);
}

inline bool isPstrLong(const t_dsc * d) {
    return (d->ss == TEXT_IN_PSTR_LONG);
}

inline bool isPstr(const t_dsc * d) {
    return (d->ss == TEXT_IN_PSTR);
}

inline bool isTextEmpty(const t_dsc * d) {
    return (d->ss == 0);
}

inline strsize_t getTextSize(const t_dsc * d) {
    if (d->ss >= 0) { return d->ss; }
    else { return d->data.lsp.size; }
}

inline xptr getTextPtr(const t_dsc * d) {
    switch (d->ss) {
        case 0: return XNULL;
        case TEXT_IN_PSTR: return textDereferenceCP(d->data.lsp.p); /* PSTR case */
        case TEXT_IN_PSTR_LONG: return d->data.lsp.p; /* PSTR_LONG case */
        default : return addr2xptr(d->data.st);
    }
}

inline char * textCopyToBuffer(char * dest, const t_dsc * src) {
    size_t size = (size_t) getTextSize(src);
    xptr data = getTextPtr(src);
    CHECKP(data);
    memcpy(dest, XADDR(data), size);
    return dest;
}

/* Descriptor of namespace node */
struct ns_dsc : public n_dsc {
	xmlns_ptr_pers ns;
};

/* Descriptor of attribute node */
struct a_dsc : public t_dsc {
	xmlscm_type		type;		/* attribute type according to the scheme */
};

/* Descriptor of document node  */
struct d_dsc : public t_dsc { };

/* Descriptor of processing instruction node */
struct pi_dsc : public t_dsc {
	shft			target;		/* size of the target part */
};

/*calculates the shift of the first address relatively to the second one */
#define CALCSHIFT(p1,p2) (shft)((char*)(p1)-(char*)(p2))

#define CALC_SHIFT(p) _depricated_

inline shft calcShift(void * p) {
    return ((shft)((char*)(p) - (char *) ((ptrdiff_t)(p) & PAGE_BIT_MASK)));
}

/* ============================================================================
  In all the following macros p and s is 'xptr*' pointer
 */
#define UPDATE_LEFT_POINTER(p,s) ((n_dsc*)XADDR(p))->ldsc=s;
#define UPDATE_RIGHT_POINTER(p,s) ((n_dsc*)XADDR(p))->rdsc=s;
/* ============================================================================
  In all the following macros b is 'node_blk_hdr *' pointer and n is 'n_dsc*' pointer
 */
#define GETPREVIOUSDESCRIPTOR_BL(b,n) (n_dsc*)((char*)b+((n_dsc*)n)->desc_prev)
#define GETNEXTDESCRIPTOR_BL(b,n) (n_dsc*)((char*)b+((n_dsc*)n)->desc_next)
/* ============================================================================
  In all the following macros b is 'node_blk_hdr *' pointer and s is structure name
 */
#define COUNTREFERENCES(b,s) ((int)((b->dsc_size-s)/ sizeof(xptr)))

#define CHILDCOUNT(p) COUNTREFERENCES((GETBLOCKBYNODE(p)),size_of_node(GETBLOCKBYNODE(p)))
/* ============================================================================
  In all the following macros b is 'n_dsc *' pointer
 */
#define GETNEXTDESCRIPTOR(b) GETNEXTDESCRIPTOR_BL((node_blk_hdr*)((int)(b) & PAGE_BIT_MASK),b)
#define GETPREVIOUSDESCRIPTOR(b) GETPREVIOUSDESCRIPTOR_BL((node_blk_hdr*)((int)(b) & PAGE_BIT_MASK),b)

#define GETSCHEMENODE(b) GETBLOCKBYNODE_ADDR(b)->snode
/* returns the pointer to the node block header*/
#define GETBLOCKBYNODE_ADDR(p) ((node_blk_hdr*)((int)p & PAGE_BIT_MASK))
 /* ============================================================================
  In all the following macros b is 'node_blk_hdr *' pointer and s is shift
 */
#define GETPOINTERTODESC(b,s) (n_dsc*)((char*)b+s)
#define GET_DSC(b,s) GETPOINTERTODESC(b,s)
/* ============================================================================
  In all the following macros p is 'shft*' pointer to inside of some block, s is shift in the block
 */
#define MAKEFREESPACE(p,s) *((shft*)p)=s

/* ============================================================================
  In all the following macros p is 'xptr*' pointer to inside of some block, s is shift in the block
 */
#define UPDATENEXTDESCRIPTOR(p,s) ((n_dsc*)XADDR(p))->desc_next=((shft) s)
#define UPDATEPREVIOUSDESCRIPTOR(p,s) ((n_dsc*)XADDR(p))->desc_prev=((shft) s)
/* ============================================================================
  In all the following macros p is 'xptr*' pointer to inside of some block
 */
/* returns the pointer to the block header*/
#define GETBLOCKBYNODE(p) (node_blk_hdr*)((int)XADDR(p) & PAGE_BIT_MASK)
/* adress of the left sibling of the node*/
#define GETLEFTPOINTER(p) ( ((n_dsc*)XADDR(p))->ldsc)
/* adress of the right sibling of the node*/
#define GETRIGHTPOINTER(p) (((n_dsc*)XADDR(p))->rdsc)
/*address of the parent indirection*/
#define GETPARENTPOINTER(p) (((n_dsc*)XADDR(p))->pdsc)
/*returns the pointer to schema node*/
#define GETSCHEMENODEX(b) (GETBLOCKBYNODE(b))->snode

/* ============================================================================
  In all the following macros p is 'shft*' pointer inside the block
 */
/*address of the next free space*/
#define GETPOINTERTONEXTFREESPACE(p) *((shft*)(p))
/* ============================================================================
  In all the following macros p is 'node_blk_hdr*' pointer to the begining of block
 */
/* first free space in block*/
#define GETBLOCKFIRSTFREESPACE(p) (((node_blk_hdr*)XADDR(p))->free_first)
/* Descriptor size*/
#define GETDESCRIPTORSIZE(p) (((node_blk_hdr*)XADDR(p))->dsc_size)
/* ============================================================================
  In all the following macros p is 'node_blk_hdr*' pointer
 */
/* increments the number of node descriptors in block*/
#define INCREMENTCOUNT(p) (((node_blk_hdr*)(p))->count++)
/* address of the first descriptor in the block */
//#define getBlockFirstDescriptorAbsolute(p)((void*)((char*)(p) + ((node_blk_hdr*)(p))->desc_first)))
/* address of the first free space in the block */
#define GETBLOCKFIRSTFREESPACEABSOLUTE(p)( (n_dsc*) ( (char*)p + ((node_blk_hdr*)p)->free_first))
/* address of the last descriptor in the block */
#define GETBLOCKLASTDESCRIPTORABSOLUTE(p)  ((n_dsc*) ((char*)(p) + ((node_blk_hdr*)(p))->desc_last))
#define GETBLOCKFIRSTDESCRIPTORABSOLUTE(p) (ADDR2XPTR((char*)(p) + ((node_blk_hdr*)(p))->desc_first))


/* ============================================================================
  In all the following macros p is the pointer to the begining of block  and s shft
 */
/* sets the pointe to the first free space in the block*/
#define UPDATEPOINTERTOFIRSTFREESPACEINBLOCK(p,s) ((node_blk_hdr*)(p))->free_first=((shft)s);
#define UPDATEPOINTERTOLASTDESCRIPTOR(p,s) ((node_blk_hdr*)(p))->desc_last=((shft)s);

#define GET_NODE_CHILD(n, i) (*(xptr*)((char*) n + size_of_node(GETBLOCKBYNODE_ADDR(n)) + i * sizeof(xptr)))


#define N_DSC(p)		((n_dsc*)(XADDR(p)))
#define E_DSC(p)		((e_dsc*)(XADDR(p)))
#define T_DSC(p)		((t_dsc*)(XADDR(p)))
#define A_DSC(p)		((a_dsc*)(XADDR(p)))
#define D_DSC(p)		((d_dsc*)(XADDR(p)))
#define NS_DSC(p)		((ns_dsc*)(XADDR(p)))
#define PI_DSC(p)		((pi_dsc*)(XADDR(p)))

//#define getBlockHeader(block) ()(node_blk_hdr *) XADDR(block))

inline int my_strcmp(const char* c1, const char* c2)
{
 if (c1==NULL && c2==NULL) return 0;
 if (c1==NULL||c2==NULL) return (c1==NULL)?-1:1;
 int res = strcmp(c1,c2); /// strcmp doesn't guarantee that return value either -1, 1 or 0!
 if(res < 0) return -1;
 if(res > 0) return 1;
 return res;
}

#endif
