/*
 * File:  nodes.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/structures/nodes.h"
#include "common/xptr.h"
#include "tr/crmutils/crmutils.h"

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

	hdr->indir_count =0;
	hdr->free_first_indir=PAGE_SIZE-sizeof(xptr);
	hdr->nblk_indir =XNULL;
	hdr->pblk_indir =XNULL;

	//int i=hdr->free_first;
	//char* ptr=((char*)p)+hdr->free_first;
	memset(((char*)hdr)+sizeof(node_blk_hdr),0,PAGE_SIZE-sizeof(node_blk_hdr));


	/*for (i = hdr->free_first;
		 i <= (int)PAGE_SIZE - dsc_size;
		 i = i+ dsc_size)
		*((shft *)((char*)p+i)) = (shft)i + dsc_size;*/
	shft descp=hdr->free_first;
	shft desci=hdr->free_first_indir;
	while (descp+2*dsc_size<desci-sizeof(xptr))
	{
		*((shft *)((char*)p+descp)) = descp + dsc_size;
		*((shft *)((char*)p+desci)) = desci - sizeof(xptr);
		descp+=dsc_size;
		desci-=sizeof(xptr);
	}
    *((shft *)((char*)p+ descp))=0;
	*((shft *)((char*)p+ desci))=0;
	//*((shft *)((char*)p+ (i - dsc_size)))=0;
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
    d->type	= xs_untyped;
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
	d->type	= xs_untyped;
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


int xmlscm_type_size(xmlscm_type xtype)
{
    switch(xtype)
    {
        case xs_float  : return sizeof(float);
        case xs_double : return sizeof(double);
        case xs_decimal: return sizeof(xs_decimal_t);
        case xs_integer: return sizeof(__int64);
        case xs_boolean: return sizeof(bool);
        default        :
            if (!is_fixed_size_type(xtype)) 
                return 0;
            else if (is_temporal_type(xtype)) 
            {
                if (xtype == xs_duration || xtype == xs_dayTimeDuration || xtype == xs_yearMonthDuration)
                    return sizeof(xs_packed_duration);
                else
                    return sizeof(xs_packed_datetime);
            }	
            else if (is_derived_from_xs_integer(xtype)) 
                return sizeof(__int64);
            else 
                throw USER_EXCEPTION2(SE1003, "Unexpected XML Schema simple type in xmlscm_type_size.");
    }
}
