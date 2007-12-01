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
#include "common/sm_vmm_data.h"
#include "tr/nid/numb_scheme.h"

struct schema_node;
struct xml_ns;

struct node_blk_hdr 
{
    vmm_sm_blk_hdr sm_vmm;	/* sm/vmm parameters */
    xptr pblk;				/* previoius block */
    xptr nblk;				/* next block */

    schema_node*	snode;	/* pointer to the scm node; 0 for text blocks */
    shft dsc_size;			/* size of the descriptor in bytes */
    shft	desc_first;		/* shift to the first descriptor in block */
    shft	desc_last;		/* shift to the last descriptor in block */
    shft    count;          /* total number of descriptors in block */    
    shft	free_first;		/* shift to the first empty space in block */
	static void init(void *p, shft dsc_size);
};



/* Decriptor classification
   ------------------------
                  n_dsc                        s_dsc
                 /  |  \  \___
                /   |   \     \
           e_dsc  t_dsc  a_dsc d_dsc
             
 */
/* type of schema node (item) */
enum t_item {
	element,
	text,
	attribute,
	document,
	virtual_root,
	xml_namespace,
	comment,
	pr_ins,
	cdata
};

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


/*
 * XML Schema Part 2 Datatypes are enumerated below
 * !!! Note: the order of types is significant, because some functions depend on
 * this order. If you are going to change something below, think twice! (AF)
 */

// Abstract base types
#define xs_anyType				0
#define xs_anySimpleType		1
#define xs_anyAtomicType		2

// Built-in simple, non-atomic types
#define xs_IDREFS				3
#define xs_NMTOKENS				4
#define xs_ENTITIES				5

// Built-in complex types
#define xs_untyped				6

// Built-in atomic types (Primitive types)
#define xs_dateTime				10
#define xs_date					11
#define xs_time					12
#define xs_duration				13
#define xs_yearMonthDuration	14
#define xs_dayTimeDuration		15
#define xs_gYearMonth			16
#define xs_gYear				17
#define xs_gMonthDay			18
#define xs_gDay					19
#define xs_gMonth				20
#define xs_float				21
#define xs_double				22
#define xs_decimal				23
#define xs_integer				24
#define xs_boolean				25
#define xs_untypedAtomic		26
#define xs_string				27
#define xs_base64Binary			28
#define xs_hexBinary			29
#define xs_anyURI				30
#define xs_QName				31
#define xs_NOTATION				32

// Special Sedna types
#define se_separator		    33
#define se_sequence             34

// Types derived from xs:string
#define xs_normalizedString		41
#define xs_token				42
#define xs_language				43
#define xs_NMTOKEN				44
#define xs_Name					45
#define xs_NCName				46
#define xs_ID					47
#define xs_IDREF				48
#define xs_ENTITY				49

// Types derived from xs:integer
#define xs_nonPositiveInteger   50
#define xs_negativeInteger      51
#define xs_long                 52
#define xs_int 				    53
#define xs_short                54
#define xs_byte                 55
#define xs_nonNegativeInteger   56
#define xs_unsignedLong         57
#define xs_unsignedInt          58
#define xs_unsignedShort        59
#define xs_unsignedByte         60
#define xs_positiveInteger      61

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

	static void init(void *p);
    static void init(void *p, xmlscm_type t);
};

/* Descriptor of text-enabled node */
struct t_dsc : public n_dsc {
	unsigned int	size;		/* size of the text node */
	xptr			data;		/* pointer to the content of that item */

	static void init(void *p);
};

/* Descriptor of namespace node */
struct ns_dsc : public n_dsc {
	xml_ns* ns;
	static void init(void *p);
};
/* Descriptor of attribute node */
struct a_dsc : public t_dsc {
	xmlscm_type		type;		/* attribute type according to the scheme */	
	
	static void init(void *p);
	static void init(void *p, xmlscm_type t);
};

/* Descriptor of document node  */
struct d_dsc : public t_dsc {
    
    static void init(void *p);
};

/* Descriptor of processing instruction node */
struct pi_dsc : public t_dsc {
	shft			target;		/* size of the target part */
	static void init(void *p);
};


/* the pointer to the first free space in parent indirection table*/
extern int* indirection_fs;

/*calculates the shift of the first address relatively to the second one */
#define CALCSHIFT(p1,p2) (shft)((char*)(p1)-(char*)(p2))


/* ============================================================================ 
  In all the following macros p and s is 'xptr*' pointer 
 */
#define UPDATELEFTPOINTER(p,s) ((n_dsc*)XADDR(p))->ldsc=s;
#define UPDATERIGHTPOINTER(p,s) ((n_dsc*)XADDR(p))->rdsc=s;
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




#define N_DSC(p)		((n_dsc*)(XADDR(p)))
#define E_DSC(p)		((e_dsc*)(XADDR(p)))
#define T_DSC(p)		((t_dsc*)(XADDR(p)))
#define A_DSC(p)		((a_dsc*)(XADDR(p)))
#define D_DSC(p)		((d_dsc*)(XADDR(p)))
#define NS_DSC(p)		((ns_dsc*)(XADDR(p)))
#define PI_DSC(p)		((pi_dsc*)(XADDR(p)))


int inline my_strcmp(const char* c1, const char* c2)
{
 if (c1==NULL && c2==NULL) return 0;
 if (c1==NULL||c2==NULL) return (c1==NULL)?-1:1;
 int res = strcmp(c1,c2); /// strcmp doesn't guarantee that return value either -1, 1 or 0!
 if(res < 0) return -1;
 if(res > 0) return 1;
 return res;
}

#endif
