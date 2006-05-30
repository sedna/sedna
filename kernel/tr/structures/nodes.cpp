/*
 * File:  nodes.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "nodes.h"
#include "xptr.h"
#include "crmutils.h"

void node_blk_hdr::init(void *p, shft dsc_size)
{
    node_blk_hdr *hdr = (node_blk_hdr*)p;
    // first 4 fields are filled by VMM
    //p->layer;
    //p->id;
    //p->prty;
    //p->paddr;
    
    hdr->pblk= XNULL;
    hdr->nblk= XNULL;
	hdr->snode=NULL;
	hdr->dsc_size=dsc_size;
    hdr->desc_first = 0;
	hdr->desc_last = 0;
	hdr->free_first = (shft) sizeof(node_blk_hdr);
	hdr->count = 0;
	int i;
	//char* ptr=((char*)p)+hdr->free_first;
	memset(((char*)hdr)+sizeof(node_blk_hdr),0,PAGE_SIZE-sizeof(node_blk_hdr));
	for (i = hdr->free_first;
		 i <= (int)PAGE_SIZE - dsc_size;
		 i = i+ dsc_size)
		*((shft *)((char*)p+i)) = (shft)i + dsc_size;
	 
	*((shft *)((char*)p+ (i - dsc_size)))=0;
	/*crm_out<<"\nFREE SPACE TEST";
	i=hdr->free_first;
	while (i!=0)
	{
	 crm_out<<"\n this space= " <<i;
	 i=*((shft *)((char*)p+i));
	 crm_out<<"next space= " <<i;
	}
	crm_out<<"\nEND OFTEST";*/
}

void e_dsc::init(void *p)
{
    e_dsc *d = (e_dsc*)p;
    d->nid = NIDNULL;
    d->pdsc= XNULL;
    d->ldsc= XNULL;
    d->rdsc= XNULL;
	d->desc_next=0;
	d->desc_prev=0;
    d->type	= xdt_untyped;
	d->indir=XNULL;
}

void e_dsc::init(void *p, xmlscm_type t)
{
    e_dsc *d = (e_dsc*)p;
    d->nid = NIDNULL;
    d->pdsc= XNULL;
    d->ldsc= XNULL;
    d->rdsc= XNULL;
	d->desc_next=0;
	d->desc_prev=0;
    d->type	= t;
	d->indir=XNULL;
}

void t_dsc::init(void *p) 
{
    t_dsc *d = (t_dsc*)p;
    d->nid = NIDNULL;
    d->pdsc= XNULL;
    d->ldsc= XNULL;
    d->rdsc= XNULL;
	d->desc_next=0;
	d->desc_prev=0;
    d->size = 0;
    d->data= XNULL;
	d->indir=XNULL;
}
void pi_dsc::init(void *p) 
{
    pi_dsc *d = (pi_dsc*)p;
    d->nid = NIDNULL;
    d->pdsc= XNULL;
    d->ldsc= XNULL;
    d->rdsc= XNULL;
	d->desc_next=0;
	d->desc_prev=0;
    d->size = 0;
    d->data= XNULL;
	d->indir=XNULL;
	d->target=0;
}
void ns_dsc::init(void *p) 
{
    ns_dsc *d = (ns_dsc*)p;
    d->nid = NIDNULL;
    d->pdsc= XNULL;
    d->ldsc= XNULL;
    d->rdsc= XNULL;
	d->desc_next=0;
	d->desc_prev=0;
    d->ns= NULL;
	d->indir=XNULL;
}

void a_dsc::init(void *p)
{
    a_dsc *d = (a_dsc*)p;
    d->nid = NIDNULL;
    d->pdsc= XNULL;
    d->ldsc= XNULL;
    d->rdsc= XNULL;
	d->desc_next=0;
	d->desc_prev=0;
    d->size = 0;
    d->data= XNULL;
	d->type	= xdt_untyped;
	d->indir=XNULL;
}
void a_dsc::init(void *p, xmlscm_type t)
{
    a_dsc *d = (a_dsc*)p;
    d->nid = NIDNULL;
    d->pdsc= XNULL;
    d->ldsc= XNULL;
    d->rdsc= XNULL;
	d->desc_next=0;
	d->desc_prev=0;
    d->size = 0;
    d->data= XNULL;
	d->type	= t;
	d->indir=XNULL;
}
void d_dsc::init(void *p)
{
	d_dsc *d = (d_dsc*)p;
    d->nid = NIDNULL;
    d->pdsc= XNULL;
    d->ldsc= XNULL;
    d->rdsc= XNULL;
	d->desc_next=0;
	d->desc_prev=0;
    d->size = 0;
    d->data= XNULL;
	d->indir=XNULL;
}

int xmlscm_type_size(xmlscm_type t)
{
    /////////////////////////////////////////////////////////////////////////////////////
    // FIXME!!! Size for some types can be defined now!
    /////////////////////////////////////////////////////////////////////////////////////
    
    switch (t) 
    {
		case xs_dateTime			: throw USER_EXCEPTION2(SE1002, "Size is undefined for type xs_dateTime yet (in xmlscm_type_size).");
		case xs_date				: throw USER_EXCEPTION2(SE1002, "Size is undefined for type xs_date yet (in xmlscm_type_size).");
		case xs_time				: throw USER_EXCEPTION2(SE1002, "Size is undefined for type xs_time yet (in xmlscm_type_size).");
		case xs_duration			: throw USER_EXCEPTION2(SE1002, "Size is undefined for type xs_duration yet (in xmlscm_type_size).");
		case xdt_yearMonthDuration	: throw USER_EXCEPTION2(SE1002, "Size is undefined for type xdt_yearMonthDuration yet (in xmlscm_type_size).");
		case xdt_dayTimeDuration	: throw USER_EXCEPTION2(SE1002, "Size is undefined for type xdt_dayTimeDuration yet (in xmlscm_type_size).");
		case xs_float				: return sizeof(float);
		case xs_double				: return sizeof(double);
		case xs_string				: return 0;
		case xs_normalizedString	: return 0;
		case xs_token				: return 0;
		case xs_language			: return 0;
		case xs_NMTOKEN				: return 0;
		case xs_Name				: return 0;
		case xs_NCName				: return 0;
		case xs_ID					: return 0;
		case xs_IDREF				: return 0;
		case xs_ENTITY				: return 0;
		case xs_decimal				: return sizeof(double);
		case xs_integer				: return sizeof(int);
		case xs_gYearMonth			: throw USER_EXCEPTION2(SE1002, "Size is undefined for type xs_gYearMonth yet (in xmlscm_type_size).");
		case xs_gYear				: throw USER_EXCEPTION2(SE1002, "Size is undefined for type xs_gYear yet (in xmlscm_type_size).");
		case xs_gMonthDay			: throw USER_EXCEPTION2(SE1002, "Size is undefined for type xs_gMonthDay yet (in xmlscm_type_size).");
		case xs_gDay				: throw USER_EXCEPTION2(SE1002, "Size is undefined for type xs_gDay yet (in xmlscm_type_size).");
		case xs_gMonth				: throw USER_EXCEPTION2(SE1002, "Size is undefined for type xs_gMonth yet (in xmlscm_type_size).");
		case xs_boolean				: return sizeof(bool);
		case xs_base64Binary		: throw USER_EXCEPTION2(SE1002, "Size is undefined. Type xs_base64Binary is not implemented yet (in xmlscm_type_size).");
		case xs_hexBinary			: throw USER_EXCEPTION2(SE1002, "Size is undefined. Type xs_hexBinary is not implemented yet (in xmlscm_type_size).");
		case xs_anyURI				: return 0;									//If it is implemented as string!
		case xs_QName				: return 0;									//If it is implemented as string!
		case xs_NOTATION			: return 0;									//If it is implemented as string!
		case xs_nonPositiveInteger  : throw USER_EXCEPTION2(SE1002, "Size is undefined. Type xs_nonPositiveInteger is not implemented yet (in xmlscm_type_size).");
		case xs_negativeInteger     : throw USER_EXCEPTION2(SE1002, "Size is undefined. Type xs_negativeInteger is not implemented yet (in xmlscm_type_size).");
		case xs_long                : throw USER_EXCEPTION2(SE1002, "Size is undefined. Type xs_long is not implemented yet (in xmlscm_type_size).");
		case xs_int 				: throw USER_EXCEPTION2(SE1002, "Size is undefined. Type xs_int is not implemented yet (in xmlscm_type_size).");
		case xs_short               : throw USER_EXCEPTION2(SE1002, "Size is undefined. Type xs_short is not implemented yet (in xmlscm_type_size).");
		case xs_byte                : throw USER_EXCEPTION2(SE1002, "Size is undefined. Type xs_byte is not implemented yet (in xmlscm_type_size).");
		case xs_nonNegativeInteger  : throw USER_EXCEPTION2(SE1002, "Size is undefined. Type xs_nonNegativeInteger is not implemented yet (in xmlscm_type_size).");
		case xs_unsignedLong        : throw USER_EXCEPTION2(SE1002, "Size is undefined. Type xs_unsignedLong is not implemented yet (in xmlscm_type_size).");
		case xs_unsignedInt         : throw USER_EXCEPTION2(SE1002, "Size is undefined. Type xs_unsignedInt is not implemented yet (in xmlscm_type_size).");
		case xs_unsignedShort       : throw USER_EXCEPTION2(SE1002, "Size is undefined. Type xs_unsignedShort is not implemented yet (in xmlscm_type_size).");
		case xs_unsignedByte        : throw USER_EXCEPTION2(SE1002, "Size is undefined. Type xs_unsignedByte is not implemented yet (in xmlscm_type_size).");
		case xs_positiveInteger     : throw USER_EXCEPTION2(SE1002, "Size is undefined. Type xs_positiveInteger is not implemented yet (in xmlscm_type_size).");
        default						: throw USER_EXCEPTION2(SE1003, "Unexpected case in xmlscm_type_size.");
    }
}
