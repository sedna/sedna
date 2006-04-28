/*
 * File:  micro.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "micro.h"
#include "node_utils.h"
#include "vmm.h"
#include "locks.h"
#include "schema.h"
#include "log.h"
#include "pstr.h"
#include "updates.h"
#include "pstr_long.h"
#include "indirection.h"
#include <string.h>
//#include <iostream.h>
#include "d_printf.h"
#ifdef _MYDEBUG
#include "crmutils.h"
#endif

/*void fillLogOfTextNodeChanged(xptr node, const bool inserted=false)
{
	if (IS_DATA_BLOCK(node))
	{
		t_dsc* res_nd=(t_dsc*)XADDR(node);
		xptr left_indir=res_nd->ldsc;
		xptr right_indir=res_nd->rdsc;
		xptr par_indir=res_nd->pdsc;
		xptr indir=res_nd->indir;
		xptr node_data=res_nd->data;
		shft size=res_nd->size;
		if (left_indir!=NULL)
		{
			CHECKP(left_indir);
			left_indir=((n_dsc*)XADDR(left_indir))->indir;
		}
		if (right_indir!=NULL)
		{
			CHECKP(left_indir);
			right_indir=((n_dsc*)XADDR(right_indir))->indir;
		}
		CHECKP(node_data);
		shft shift= *((shft*)XADDR(node_data));
		char* ptr=(char*)XADDR(BLOCKXPTR(node_data))+shift;
		hl_logical_log_text(indir,left_indir,right_indir,par_indir,ptr,size,inserted);
		hl_logical_log_indirection( -2, NULL);
	}
}*/
xptr textInsertProcedure(xptr parent,const void* value, int size, int& ins_type,text_type ttype)
{
	#ifdef _MYDEBUG
		crm_out<<" textInsertProcedure";
	#endif
	xptr tmp ;
	CHECKP(parent);
	node_blk_hdr* block=GETBLOCKBYNODE(parent);
	int chcnt=COUNTREFERENCES(block,size_of_node(block));
	xptr right_sib=giveAnyDmChildrenChild((n_dsc*)XADDR(parent),chcnt);
	xptr par_indir=XNULL;
	xptr left_sib=XNULL;
	if (right_sib==XNULL)
	{
		#ifdef _MYDEBUG
			crm_out<<" textInsertProcedure->1";
		#endif
		left_sib=getLastNonDmChildrenChild((n_dsc*)XADDR(parent),chcnt);
		if (left_sib!=XNULL)
		{
			CHECKP(left_sib);
			par_indir=GETPARENTPOINTER(left_sib);
		}	  
	}
	else
	{
		#ifdef _MYDEBUG
			crm_out<<" textInsertProcedure->3";
		#endif	
		CHECKP(right_sib);
		left_sib=GETLEFTPOINTER(right_sib);
		par_indir=GETPARENTPOINTER(right_sib);
	}
	CHECKP(parent);
	if (par_indir==XNULL)
	{
		#ifdef _MYDEBUG
			crm_out<<" textInsertProcedure->4";
		#endif
		par_indir=((n_dsc*)XADDR(parent))->indir;
	}

	if (left_sib!=XNULL) 
	{
		#ifdef _MYDEBUG
			crm_out<<" textInsertProcedure->5";
		#endif
		CHECKP(left_sib);
		if (GETTYPE(GETSCHEMENODEX(left_sib))==text)
		{
			#ifdef _MYDEBUG
				crm_out<<" textInsertProcedure->6";
			#endif
			//fillLogOfTextNodeChanged(left_sib);
			appendTextValue(left_sib,value, size,ttype);
			ins_type=1;

			return left_sib; 
		}
	}
    if (right_sib!=XNULL) 
	{
		#ifdef _MYDEBUG
			crm_out<<" textInsertProcedure->7";
		#endif
		CHECKP(right_sib);
		if (GETTYPE(GETSCHEMENODEX(right_sib))==text)
		{
			#ifdef _MYDEBUG
				crm_out<<" textInsertProcedure->8";
			#endif
			//fillLogOfTextNodeChanged(right_sib);
			insertTextValue(right_sib,value,size,ttype);
			ins_type=2;
			return right_sib;
		}
	}
	if (block->snode->is_node_in_scheme_and_in_data(NULL,NULL,text) )
	{
		#ifdef _MYDEBUG
			crm_out<<" textInsertProcedure->9";
		#endif
		xptr namesake= findNodeWithSameNameToInsertAfter(left_sib,right_sib,parent,NULL,text,NULL);
		if (namesake!=XNULL)
			tmp = addNewNodeOfSameSortAfter(namesake,left_sib, right_sib, parent,   par_indir, 0,text);
		else 
		{
			#ifdef _MYDEBUG
				crm_out<<" textInsertProcedure->10";
			#endif
			namesake= findNodeWithSameNameToInsertBefore(right_sib, parent,parent,NULL,text,NULL);
			if (namesake!=XNULL)  tmp=addNewNodeOfSameSortBefore(namesake,left_sib,right_sib, parent,   par_indir, 0,text);
		}
	}
	else 
	{
		#ifdef _MYDEBUG
			crm_out<<" textInsertProcedure->11";
		#endif
		schema_node* scm;
		CHECKP(parent);
		if (block->snode->has_child_by_schema(NULL,NULL,text)<0)  
			scm=   block->snode->add_child(NULL,NULL,text);
		else
			scm= block->snode->get_child(NULL,NULL,text);
		xptr newblock=createNewBlock(scm,IS_DATA_BLOCK(parent));
		tmp =addNewNodeFirstInRow(newblock, left_sib, right_sib, parent,par_indir , NULL,text);
	} 
    par_indir=GETPARENTPOINTER(tmp);
	CHECKP(parent);
	xptr* pos;
    if ((pos=elementContainsChild(((n_dsc*)XADDR(parent)),NULL,text,NULL))!=NULL)
	{
		#ifdef _MYDEBUG
			crm_out<<" textInsertProcedure->12";
		#endif
		CHECKP(tmp);
		n_dsc* node_d=getPreviousDescriptorOfSameSort (((n_dsc*)XADDR(tmp)));
	    if (node_d==NULL || node_d->pdsc!=par_indir)
		{
			#ifdef _MYDEBUG
				crm_out<<" textInsertProcedure->13";
			#endif
			CHECKP(parent);
			VMM_SIGNAL_MODIFICATION(parent);
			//PHYS LOG
			if (IS_DATA_BLOCK(parent)) 
				hl_phys_log_change(pos,sizeof(xptr));
			*pos=tmp;
		}
	}
    else
		addChildsBySchemeSplittingBlock(parent, NULL,text, tmp,NULL);
	CHECKP(tmp);
	//NODE STATISTICS
	(GETBLOCKBYNODE(tmp))->snode->nodecnt++;
	addTextValue(tmp,value, size,ttype);
	#ifdef _MYDEBUG
		crm_out<<" end of textInsertProcedure";
	#endif
	return tmp;
}
xptr thirdElementAndTextInsertProcedure(xptr  left_sib, xptr right_sib,  xptr parent,const char* name, xmlscm_type type,t_item node_type,xml_ns* ns)
{
	#ifdef _MYDEBUG
		crm_out<<" thirdElementAndTextInsertProcedure";
	#endif
	xptr tmp , tmp_ad;
	CHECKP(parent);
	xptr par_indir=((n_dsc*)XADDR(parent))->indir;
	schema_node* par_sc=GETSCHEMENODEX(parent);
	schema_node* scm=par_sc->get_child(ns,name,node_type);
	if (scm!=NULL && scm->bblk!=XNULL )
  	{
		#ifdef _MYDEBUG
			crm_out<<" thirdElementAndTextInsertProcedure->4";
		#endif
		xptr namesake= findNodeWithSameNameToInsertAfter(left_sib,right_sib,parent,name,node_type,ns);
		if (namesake!=XNULL)
			tmp = addNewNodeOfSameSortAfter(namesake, left_sib,right_sib,parent, par_indir, type,node_type);
		else 
		{
			#ifdef _MYDEBUG
				crm_out<<" thirdElementAndTextInsertProcedure->5";
			#endif
			namesake= findNodeWithSameNameToInsertBefore(left_sib,right_sib,parent,name,node_type,ns);
			if (namesake!=XNULL)  
				tmp=addNewNodeOfSameSortBefore(namesake,left_sib,right_sib,parent,par_indir, type,node_type);
			else 
				throw SYSTEM_EXCEPTION("Data Inconsistency");
		}
    }        
	else
	{
		#ifdef _MYDEBUG
			crm_out<<" thirdElementAndTextInsertProcedure->6";
		#endif
		if (scm==NULL) 
			scm=   par_sc->add_child(ns,name,node_type);
		xptr newblock=createNewBlock(scm,IS_DATA_BLOCK(parent));
		tmp =addNewNodeFirstInRow(newblock, left_sib, right_sib, parent,par_indir ,  type,node_type);
	}
	par_indir=GETPARENTPOINTER(tmp);
	CHECKP(parent);
	xptr* pos;
    if ((pos=elementContainsChild(((n_dsc*)XADDR(parent)),name,node_type,ns))!=NULL)
	{
		#ifdef _MYDEBUG
			crm_out<<" thirdElementAndTextInsertProcedure->7";
		#endif
		CHECKP(tmp);
		n_dsc* node_d=getPreviousDescriptorOfSameSort (((n_dsc*)XADDR(tmp)));
	    if (node_d==NULL || node_d->pdsc!=par_indir)
		{
			#ifdef _MYDEBUG
				crm_out<<" thirdElementAndTextInsertProcedure->8";
			#endif
			CHECKP(parent);
			VMM_SIGNAL_MODIFICATION(parent);
			//PHYS LOG
			if (IS_DATA_BLOCK(parent)) 
				hl_phys_log_change(pos,sizeof(xptr));
			*pos=tmp;
		}
	}
    else
		addChildsBySchemeSplittingBlock(parent, name,node_type, tmp,ns);
	CHECKP(tmp);
	#ifdef _MYDEBUG
			crm_out<<" end of thirdElementAndTextInsertProcedure";
	#endif
	return tmp;
}

xptr secondElementInsertProcedure(xptr right_sib,  xptr parent,t_item ntype, xmlscm_type type)
{
	#ifdef _MYDEBUG
		crm_out<<" secondElementInsertProcedure";
	#endif
	int res= splitBlockIfFullAfterLeftInsert(right_sib);
	n_dsc* new_node;
	xptr new_block;
	xptr left_sib=GETLEFTPOINTER(right_sib);
	xptr par_sib=GETPARENTPOINTER(right_sib);
	xptr tmp;
	xptr n_blk;
	switch (res)
	{
	case 0: n_blk=BLOCKXPTR(right_sib);break;
	case 1: case 2:n_blk=((node_blk_hdr*)XADDR(BLOCKXPTR(right_sib)))->pblk;CHECKP(n_blk);
	}
	node_blk_hdr *right=(node_blk_hdr*)XADDR(n_blk);
	new_node= GETBLOCKFIRSTFREESPACEABSOLUTE (right);
	VMM_SIGNAL_MODIFICATION(n_blk);
	//PHYS LOG
	if (IS_DATA_BLOCK(n_blk)) 
		hl_phys_log_change(&right->free_first,sizeof(shft));
    right->free_first=GETPOINTERTONEXTFREESPACE(new_node);
	//PHYS LOG
	if (IS_DATA_BLOCK(n_blk)) 
		hl_phys_log_change(new_node,right->dsc_size);
	switch (ntype)
	{
	case (element):
		{
			e_dsc::init(new_node, type);
			clear_references(right,new_node);
			break;
		}
	case (xml_namespace):
		ns_dsc::init(new_node);
		break;
	case (comment):
		t_dsc::init(new_node);
		break;
	case (cdata):
		t_dsc::init(new_node);
		break;
	case (pr_ins):
		pi_dsc::init(new_node);
		break;
	}
	new_node->pdsc=((n_dsc*)XADDR(right_sib))->pdsc;
	switch(res)
	{
	case 0:
		{
			new_node->desc_prev=((n_dsc*)XADDR(right_sib))->desc_prev;
			new_node->desc_next= CALCSHIFT(XADDR(right_sib),right);
			//PHYS LOG
			if (IS_DATA_BLOCK(right_sib))
				hl_phys_log_change(&((n_dsc*)XADDR(right_sib))->desc_prev,sizeof(shft));
			UPDATEPREVIOUSDESCRIPTOR(right_sib, CALCSHIFT(new_node,right));
			if (new_node->desc_prev!=0)
			{
				//PHYS LOG
				if (IS_DATA_BLOCK(right_sib)) 
					hl_phys_log_change(&((n_dsc*)((char*)right+new_node->desc_prev))->desc_next,sizeof(shft));
				((n_dsc*)((char*)right+new_node->desc_prev))->desc_next=CALCSHIFT(new_node,right);
			}
			else
			{
				//PHYS LOG
				if (IS_DATA_BLOCK(right_sib)) 
					hl_phys_log_change(&right->desc_first,sizeof(shft));
				right->desc_first=CALCSHIFT(new_node,right);
			}
			break;
		}
	case 1: case 2:
		{
			new_node->desc_next=0;
			new_node->desc_prev=right->desc_last;
			n_dsc* tmn=GETPOINTERTODESC(right,right->desc_last);
			//assumption that block not empty
			if (IS_DATA_BLOCK(right_sib))
			{
				hl_phys_log_change(&(tmn->desc_next),sizeof(shft));
				hl_phys_log_change(&(right->desc_last),sizeof(shft));
			}
			tmn->desc_next=CALCSHIFT(new_node,right);
			right->desc_last=tmn->desc_next;
			break;
		}
	
	}
	
	//PHYS LOG
	if (IS_DATA_BLOCK(right_sib)) 
		hl_phys_log_change(&right->count,sizeof(shft));
	INCREMENTCOUNT(right);
	tmp=add_record_to_indirection_table(ADDR2XPTR(new_node));
	CHECKP(n_blk);
	VMM_SIGNAL_MODIFICATION(n_blk);
	new_node->indir=tmp;
	createNID( left_sib, right_sib, parent,ADDR2XPTR(new_node)); 
	CHECKP(n_blk);
	
	xptr parindir=new_node->pdsc;
	n_dsc* neigh=getPreviousDescriptorOfSameSort(new_node);
    if (neigh==NULL || neigh->pdsc!=parindir)
	{
		#ifdef _MYDEBUG
			crm_out<<" secondElementInsertProcedure->1";
		#endif
		CHECKP(n_blk);
		xptr nodex=ADDR2XPTR(new_node);
		CHECKP(parent);
		VMM_SIGNAL_MODIFICATION(parent);
		updateChildPointer((n_dsc*)(XADDR(parent)),right_sib,nodex);
	}
	CHECKP(n_blk);
	#ifdef _MYDEBUG
		crm_out<<" end of secondElementInsertProcedure";
	#endif
	return insertBetween ( left_sib, right_sib, new_node);;
}


xptr firstNodeInsertProcedure(xptr left_sib,  xptr parent,t_item ntype,  xmlscm_type type)
{
	#ifdef _MYDEBUG
	crm_out<<" firstElementInsertProcedure";
	#endif
	xptr l_sib=left_sib;
	int res= splitBlockIfFullAfterRightInsert(l_sib);
	n_dsc* new_node;
	xptr new_block;
	
	xptr right_sib=GETRIGHTPOINTER(l_sib);
	xptr par_sib=GETPARENTPOINTER(l_sib);
	xptr tmp;
	xptr n_blk;
	switch (res)
	{
	case 0: n_blk=BLOCKXPTR(l_sib);break;
	case 1: case 2:case 3: n_blk=((node_blk_hdr*)XADDR(BLOCKXPTR(l_sib)))->nblk;CHECKP(n_blk);
	}
	node_blk_hdr* block=(node_blk_hdr*)XADDR(n_blk);
	new_node= GETBLOCKFIRSTFREESPACEABSOLUTE(block);
	VMM_SIGNAL_MODIFICATION(n_blk);
	//PHYS LOG
	if (IS_DATA_BLOCK(n_blk)) 
		hl_phys_log_change(&block->free_first,sizeof(shft));
	UPDATEPOINTERTOFIRSTFREESPACEINBLOCK(block,GETPOINTERTONEXTFREESPACE(new_node));
	//PHYS LOG
	if (IS_DATA_BLOCK(n_blk)) 
		hl_phys_log_change(new_node,block->dsc_size);
	switch (ntype)
	{
	case (element):
		{
			e_dsc::init(new_node, type);
			clear_references(block,new_node);
			break;
		}
	case (xml_namespace):
		ns_dsc::init(new_node);
		break;
	case (comment):
		t_dsc::init(new_node);
		break;
	case (cdata):
		t_dsc::init(new_node);
		break;
	case (pr_ins):
		pi_dsc::init(new_node);
		break;
	}
	new_node->pdsc=par_sib;
	switch(res)
	{
	case 0:
		{
			new_node->desc_next=((n_dsc*)XADDR(l_sib))->desc_next;
			new_node->desc_prev= CALCSHIFT(XADDR(l_sib),block);
			//PHYS LOG
			if (IS_DATA_BLOCK(l_sib))
				hl_phys_log_change(&((n_dsc*)XADDR(l_sib))->desc_next,sizeof(shft));
			UPDATENEXTDESCRIPTOR(l_sib, CALCSHIFT(new_node,block));
			if (new_node->desc_next!=0)
			{
				//PHYS LOG
				if (IS_DATA_BLOCK(l_sib)) 
					hl_phys_log_change(&((n_dsc*)((char*)block+new_node->desc_next))->desc_prev,sizeof(shft));	
				((n_dsc*)((char*)block+new_node->desc_next))->desc_prev=CALCSHIFT(new_node,block);
			}
			else
			{
				//PHYS LOG
				if (IS_DATA_BLOCK(l_sib)) 
					hl_phys_log_change(&block->desc_last,sizeof(shft));
				UPDATEPOINTERTOLASTDESCRIPTOR(block, CALCSHIFT(new_node,block));
			}
			break;
		}
	case 1: case 3:
		{
			new_node->desc_next=block->desc_first;
			new_node->desc_prev=0;
			n_dsc* tmn=GETPOINTERTODESC(block,block->desc_first);
			//assumption that block not empty
			if (IS_DATA_BLOCK(l_sib))
			{
				hl_phys_log_change(&(tmn->desc_prev),sizeof(shft));
				hl_phys_log_change(&(block->desc_first),sizeof(shft));
			}
			tmn->desc_prev=CALCSHIFT(new_node,block);
			block->desc_first=tmn->desc_prev;
			break;
		}
		case 2:
		{
			new_node->desc_next=0;
			new_node->desc_prev=0;
			//assumption that block not empty
			if (IS_DATA_BLOCK(l_sib))
			{
				hl_phys_log_change(&(block->desc_last),sizeof(shft));
				hl_phys_log_change(&(block->desc_first),sizeof(shft));
			}
			block->desc_last=CALCSHIFT(new_node,block);
			block->desc_first=block->desc_last;
			break;
		}
	}
	//PHYS LOG
	if (IS_DATA_BLOCK(l_sib)) 
		hl_phys_log_change(&block->count,sizeof(shft));
	INCREMENTCOUNT(block);
	tmp=add_record_to_indirection_table(ADDR2XPTR(new_node));
	CHECKP(n_blk);
	VMM_SIGNAL_MODIFICATION(n_blk);
	new_node->indir=tmp;
	createNID( l_sib, right_sib, parent,ADDR2XPTR(new_node)); 
	#ifdef _MYDEBUG
		crm_out<<"end of firstElementInsertProcedure";
	#endif
	CHECKP(n_blk);
	return insertBetween ( l_sib, right_sib, new_node);
}
xptr insert_element(xptr left_sib, xptr right_sib, xptr parent,const char* name, xmlscm_type type,xml_ns* ns)
{
    #ifdef _MYDEBUG
		crm_out<<" insert_element";
	#endif
	node_blk_hdr *left=NULL;
	node_blk_hdr *right=NULL;
	xptr truep=XNULL;
	xptr result=XNULL;
    if ((right_sib==XNULL)&&(left_sib==XNULL)&&(parent==XNULL)) throw SYSTEM_EXCEPTION("Bad parameters");
	if (left_sib!=XNULL) 
	{
		#ifdef _MYDEBUG
			crm_out<<" insert_element->1";
		#endif
		CHECKP(left_sib); 
		left= GETBLOCKBYNODE(left_sib);
		if ( right_sib!=GETRIGHTPOINTER(left_sib)) 
			if  ( right_sib!=XNULL)   throw SYSTEM_EXCEPTION("Bad parameters");
			else right_sib=GETRIGHTPOINTER(left_sib);
		xptr parind=GETPARENTPOINTER(left_sib);
		CHECKP(parind);
		truep=*((xptr*)XADDR(parind));
	}
	if (right_sib!=XNULL) 
	{
		#ifdef _MYDEBUG
			crm_out<<" insert_element->2";
		#endif
		CHECKP(right_sib);
		right = GETBLOCKBYNODE(right_sib);
		if  ( left_sib!=GETLEFTPOINTER(right_sib))
			if  ( left_sib!=XNULL)   throw SYSTEM_EXCEPTION("Bad parameters");
			else 
			{
				left_sib=GETLEFTPOINTER(right_sib);
				left= GETBLOCKBYNODE(left_sib);
			}
		if (truep==XNULL)
		{
			xptr parind=GETPARENTPOINTER(right_sib);
			CHECKP(parind);
			truep=*((xptr*)XADDR(parind));
		}
	}
	if (parent==XNULL) parent=truep;
	else if (parent!=truep && truep!=XNULL) throw SYSTEM_EXCEPTION("Bad parameters");
    if (left==NULL && right==NULL)
	{
		CHECKP(parent);
		node_blk_hdr* block=GETBLOCKBYNODE(parent);
		int chcnt=COUNTREFERENCES(block,size_of_node(block));
		right_sib=giveAnyDmChildrenChild((n_dsc*)XADDR(parent),chcnt);
		left_sib=XNULL;
		if (right_sib==XNULL)
		{
			left_sib=getLastNonDmChildrenChild((n_dsc*)XADDR(parent),chcnt);
			if (left_sib!=XNULL)
			{
				
				CHECKP(left_sib);
				left= GETBLOCKBYNODE(left_sib);
			}	  
		}
		else
		{
			right = GETBLOCKBYNODE(right_sib);
			CHECKP(right_sib);
            left_sib=GETLEFTPOINTER(right_sib);
			if (left_sib!=XNULL)left= GETBLOCKBYNODE(left_sib);
		}
	}
	if (IS_DATA_BLOCK(parent))
		down_concurrent_micro_ops_number();
	if (left!=NULL)
	{ 
		CHECKP(left_sib);
		if ( my_strcmp(GETNAME(left->snode),name)==0 && GETTYPE(left->snode)==element && left->snode->xmlns==ns) 
			result = firstNodeInsertProcedure( left_sib,  parent,element, type); 
	}
    if (right!=NULL && result==XNULL) 
	{
		#ifdef _MYDEBUG
			crm_out<<" insert_element->3";
		#endif
		CHECKP(right_sib);
		if (my_strcmp(GETNAME(right->snode),name)==0 && GETTYPE(right->snode)==element && right->snode->xmlns==ns) 
			result = secondElementInsertProcedure(right_sib,  parent,element,  type);
	}
	if (result==XNULL) result= thirdElementAndTextInsertProcedure( left_sib, right_sib,  parent, name, type,element,ns);
    //NODE STATISTICS
	(GETBLOCKBYNODE(result))->snode->nodecnt++;
	if (IS_DATA_BLOCK(parent))
	{
		//update_idx_add(result);
		n_dsc* res_nd=(n_dsc*)XADDR(result);
		xptr left_indir=res_nd->ldsc;
		xptr right_indir=res_nd->rdsc;
		xptr par_indir=res_nd->pdsc;
		xptr indir=res_nd->indir;
		if (left_indir!=NULL)
		{
			CHECKP(left_indir);
			left_indir=((n_dsc*)XADDR(left_indir))->indir;
		}
		if (right_indir!=NULL)
		{
			CHECKP(right_indir);
			right_indir=((n_dsc*)XADDR(right_indir))->indir;
		}
//		d_printf2("EL Node name=%s \n",name);
//		d_printf1("\nEL Node xptr=");
//		result.print();
//		d_printf1("\n EL Node indir=");
//		indir.print();
		hl_logical_log_element(indir,left_indir,right_indir,par_indir,name,type,(ns)?ns->uri:NULL,(ns)?ns->prefix:NULL ,true);
		CHECKP(result);
		up_concurrent_micro_ops_number();
	}
	return result;
}

xptr insert_attribute(xptr left_sib, xptr right_sib, xptr parent,const char* name, xmlscm_type type,const  char* value,int data_size,xml_ns* ns)
{
    #ifdef _MYDEBUG
		crm_out<<" insert_attribute";
	#endif
	node_blk_hdr *left=NULL;
	node_blk_hdr *right=NULL;
	if ((right_sib==XNULL)&&(left_sib==XNULL)&&(parent==XNULL)) throw SYSTEM_EXCEPTION("Bad parameters");
	if (right_sib!=XNULL) 
	{
		#ifdef _MYDEBUG
			crm_out<<" insert_attribute->1";
		#endif
		CHECKP(right_sib);
		right = GETBLOCKBYNODE(right_sib);
		if  ( left_sib!=GETLEFTPOINTER(right_sib))
			if  ( left_sib!=XNULL)   throw SYSTEM_EXCEPTION("Bad parameters");
			else left_sib=GETLEFTPOINTER(right_sib);
	}
	if (left_sib!=XNULL) 
	{
		#ifdef _MYDEBUG
			crm_out<<" insert_attribute->2";
		#endif
		CHECKP(left_sib); 
		left= GETBLOCKBYNODE(left_sib);
		if (GETTYPE(left->snode)!=attribute && GETTYPE(left->snode)!=xml_namespace) throw USER_EXCEPTION(SE2007);
		if ( right_sib!=GETRIGHTPOINTER(left_sib)) 
			if  ( right_sib!=XNULL)   throw SYSTEM_EXCEPTION("Bad parameters");
			else right_sib=GETRIGHTPOINTER(left_sib);
	}
	xptr par_indir=XNULL;
	if (parent==XNULL)
	{
		#ifdef _MYDEBUG
			crm_out<<" insert_attribute->3";
		#endif
		if (left_sib!=XNULL) 
			par_indir=GETPARENTPOINTER(left_sib);
		else if (right_sib!=XNULL) 
		{
			#ifdef _MYDEBUG
				crm_out<<" insert_attribute->4";
			#endif
			CHECKP(right_sib);
			par_indir=GETPARENTPOINTER(right_sib);
		}
		CHECKP(par_indir);
		parent=*((xptr*)XADDR(par_indir));
		CHECKP(parent);
	}
	else
	{
		CHECKP(parent);
		par_indir=((n_dsc*)XADDR(parent))->indir;
	}
	n_dsc* par_desc=(n_dsc*)XADDR(parent);
	if (getChildPointer(par_desc,name,attribute,ns)!=XNULL && (GETBLOCKBYNODE_ADDR(par_desc))->snode->type!=virtual_root) 
		throw USER_EXCEPTION(SE2008);
	int size=COUNTREFERENCES((GETBLOCKBYNODE(parent)),size_of_node((GETBLOCKBYNODE(parent))));
	if (((left_sib==XNULL)&&(right_sib==XNULL))&&giveAnyChild(par_desc,size)!=XNULL)
	{
		#ifdef _MYDEBUG
			crm_out<<" insert_attribute->5";
		#endif
		left_sib=giveAnyAttributeChild(par_desc,size);
		if (left_sib==NULL) left_sib=getLastNamespaceChild(par_desc,size);
		if (left_sib!=XNULL)
		{
			#ifdef _MYDEBUG
				crm_out<<" insert_attribute->6";
			#endif
			CHECKP(left_sib); 
			//left= GETBLOCKBYNODE(left_sib);
			right_sib=GETRIGHTPOINTER(left_sib);
		}
		else
		{
			#ifdef _MYDEBUG
				crm_out<<" insert_attribute->7";
			#endif
			right_sib=giveFirstByOrderChild(parent,size);
			left_sib=XNULL;
		}
	}
	CHECKP(parent);
	schema_node* scm_node=GETSCHEMENODE(par_desc);
	xptr tmp=XNULL;
	if (IS_DATA_BLOCK(parent))
		down_concurrent_micro_ops_number();
	if (scm_node->is_node_in_scheme_and_in_data(ns,name,attribute) )
	{
		#ifdef _MYDEBUG
			crm_out<<" insert_attribute->8";
		#endif
		xptr namesake= findAttributeWithSameNameToInsertAfter(parent,name,ns);
		if (namesake!=XNULL)
		{
			CHECKP(namesake);
			tmp = addNewNodeOfSameSortAfter(namesake,left_sib, right_sib, parent,   par_indir, type,attribute);
		}
		else 
		{
			#ifdef _MYDEBUG
				crm_out<<" insert_attribute->9";
			#endif
			namesake= findAttributeWithSameNameToInsertBefore(parent,name,ns);
			if (namesake!=XNULL)  
			{
				CHECKP(namesake);
				tmp=addNewNodeOfSameSortBefore(namesake,left_sib,right_sib, parent,   par_indir, type,attribute);
			}
		}
	}
	else
	{
		#ifdef _MYDEBUG
			crm_out<<" insert_attribute->10";
		#endif
		schema_node* scm;
		if (scm_node->has_child_by_schema(ns,name,attribute)<0)  
			scm=   scm_node->add_child(ns,name,attribute);
		else
			scm= scm_node->get_child(ns,name,attribute);
		xptr newblock=createNewBlock(scm,IS_DATA_BLOCK(parent));
		tmp =addNewNodeFirstInRow(newblock, left_sib, right_sib, parent,par_indir ,type,attribute);
	}
	addTextValue(tmp,value,  data_size);
	CHECKP(parent);
	xptr* pos;
    if ((pos=elementContainsChild(((n_dsc*)XADDR(parent)),name,attribute,ns))!=NULL)
	{
		VMM_SIGNAL_MODIFICATION(parent);
		//PHYS LOG
		if (IS_DATA_BLOCK(parent)) 
			hl_phys_log_change(pos,sizeof(xptr));
		*pos=tmp;
	}
	else
		addChildsBySchemeSplittingBlock(parent, name,attribute, tmp,ns);
	CHECKP(tmp);
	//NODE STATISTICS
	(GETBLOCKBYNODE(tmp))->snode->nodecnt++;
	(GETBLOCKBYNODE(tmp))->snode->textcnt+=data_size;
	if (IS_DATA_BLOCK(parent))
	{
		update_idx_add(tmp,value,data_size);
		n_dsc* res_nd=(n_dsc*)XADDR(tmp);
		xptr left_indir=res_nd->ldsc;
		xptr right_indir=res_nd->rdsc;
		xptr par_indir=res_nd->pdsc;
		xptr indir=res_nd->indir;
		if (left_indir!=NULL)
		{
			CHECKP(left_indir);
			left_indir=((n_dsc*)XADDR(left_indir))->indir;
		}
		if (right_indir!=NULL)
		{
			CHECKP(right_indir);
			right_indir=((n_dsc*)XADDR(right_indir))->indir;
		}
		hl_logical_log_attribute(indir,left_indir,right_indir,par_indir,name,type,value,data_size,(ns)?ns->uri:NULL,(ns)?ns->prefix:NULL ,true);
		CHECKP(tmp);
		up_concurrent_micro_ops_number();		
	}
	return tmp;

}


xptr insert_comment(xptr left_sib, xptr right_sib, xptr parent,const char* value, int size)
{
	node_blk_hdr *left=NULL;
	node_blk_hdr *right=NULL;
	xptr truep=XNULL;
	xptr result=XNULL;
    if ((right_sib==XNULL)&&(left_sib==XNULL)&&(parent==XNULL)) throw SYSTEM_EXCEPTION("Bad parameters");
	if (left_sib!=XNULL) 
	{
		CHECKP(left_sib); 
		left= GETBLOCKBYNODE(left_sib);
		if ( right_sib!=GETRIGHTPOINTER(left_sib)) 
			if  ( right_sib!=XNULL)   throw SYSTEM_EXCEPTION("Bad parameters");
			else right_sib=GETRIGHTPOINTER(left_sib);
		xptr parind=GETPARENTPOINTER(left_sib);
		CHECKP(parind);
		truep=*((xptr*)XADDR(parind));
	}
	if (right_sib!=XNULL) 
	{
		CHECKP(right_sib);
		right = GETBLOCKBYNODE(right_sib);
		if  ( left_sib!=GETLEFTPOINTER(right_sib))
			if  ( left_sib!=XNULL)   throw SYSTEM_EXCEPTION("Bad parameters");
			else 
			{
				left_sib=GETLEFTPOINTER(right_sib);
				left= GETBLOCKBYNODE(left_sib);
			}
		if (truep==XNULL)
		{
			xptr parind=GETPARENTPOINTER(right_sib);
			CHECKP(parind);
			truep=*((xptr*)XADDR(parind));
		}
	}
	if (parent==XNULL) parent=truep;
	else if (parent!=truep && truep!=XNULL) throw SYSTEM_EXCEPTION("Bad parameters");
    if (left==NULL && right==NULL)
	{
		CHECKP(parent);
		node_blk_hdr* block=GETBLOCKBYNODE(parent);
		int chcnt=COUNTREFERENCES(block,size_of_node(block));
		right_sib=giveAnyDmChildrenChild((n_dsc*)XADDR(parent),chcnt);
		left_sib=XNULL;
		if (right_sib==XNULL)
		{
			left_sib=getLastNonDmChildrenChild((n_dsc*)XADDR(parent),chcnt);
			if (left_sib!=XNULL)
			{
				
				CHECKP(left_sib);
				left= GETBLOCKBYNODE(left_sib);
			}	  
		}
		else
		{
			right = GETBLOCKBYNODE(right_sib);
			CHECKP(right_sib);
            left_sib=GETLEFTPOINTER(right_sib);
			if (left_sib!=XNULL)left= GETBLOCKBYNODE(left_sib);
		}
	}
	if (IS_DATA_BLOCK(parent))
		down_concurrent_micro_ops_number();
	if (left!=NULL)
	{ 
		CHECKP(left_sib);
		if ( GETTYPE(left->snode)==comment ) 
			result = firstNodeInsertProcedure( left_sib,  parent,comment,0); 
	}
    if (right!=NULL && result==XNULL) 
	{
		CHECKP(right_sib);
		if (GETTYPE(right->snode)==comment) 
			result = secondElementInsertProcedure(right_sib,  parent,comment,0);
	}
	if (result==XNULL) result= thirdElementAndTextInsertProcedure( left_sib, right_sib,  parent, NULL, 0,comment,NULL);
	//text value
	addTextValue(result,value, size);
	//NODE STATISTICS
	(GETBLOCKBYNODE(result))->snode->nodecnt++;
	(GETBLOCKBYNODE(result))->snode->textcnt+=size;
    if (IS_DATA_BLOCK(parent))
	{
		update_idx_add(result,value,size);
		n_dsc* res_nd=(n_dsc*)XADDR(result);
		xptr left_indir=res_nd->ldsc;
		xptr right_indir=res_nd->rdsc;
		xptr par_indir=res_nd->pdsc;
		xptr indir=res_nd->indir;
		if (left_indir!=NULL)
		{
			CHECKP(left_indir);
			left_indir=((n_dsc*)XADDR(left_indir))->indir;
		}
		if (right_indir!=NULL)
		{
			CHECKP(right_indir);
			right_indir=((n_dsc*)XADDR(right_indir))->indir;
		}
		hl_logical_log_comment(indir,left_indir,right_indir,par_indir,value,size,true);
		CHECKP(result);
		up_concurrent_micro_ops_number();		
	}
	return result;
}
xptr insert_cdata(xptr left_sib, xptr right_sib, xptr parent,const char* value, int size)
{
	node_blk_hdr *left=NULL;
	node_blk_hdr *right=NULL;
	xptr truep=XNULL;
	xptr result=XNULL;
    if ((right_sib==XNULL)&&(left_sib==XNULL)&&(parent==XNULL)) throw SYSTEM_EXCEPTION("Bad parameters");
	if (left_sib!=XNULL) 
	{
		CHECKP(left_sib); 
		left= GETBLOCKBYNODE(left_sib);
		if ( right_sib!=GETRIGHTPOINTER(left_sib)) 
			if  ( right_sib!=XNULL)   throw SYSTEM_EXCEPTION("Bad parameters");
			else right_sib=GETRIGHTPOINTER(left_sib);
		xptr parind=GETPARENTPOINTER(left_sib);
		CHECKP(parind);
		truep=*((xptr*)XADDR(parind));
	}
	if (right_sib!=XNULL) 
	{
		CHECKP(right_sib);
		right = GETBLOCKBYNODE(right_sib);
		if  ( left_sib!=GETLEFTPOINTER(right_sib))
			if  ( left_sib!=XNULL)   throw SYSTEM_EXCEPTION("Bad parameters");
			else 
			{
				left_sib=GETLEFTPOINTER(right_sib);
				left= GETBLOCKBYNODE(left_sib);
			}
		if (truep==XNULL)
		{
			xptr parind=GETPARENTPOINTER(right_sib);
			CHECKP(parind);
			truep=*((xptr*)XADDR(parind));
		}
	}
	if (parent==XNULL) parent=truep;
	else if (parent!=truep && truep!=XNULL) throw SYSTEM_EXCEPTION("Bad parameters");
    if (left==NULL && right==NULL)
	{
		CHECKP(parent);
		node_blk_hdr* block=GETBLOCKBYNODE(parent);
		int chcnt=COUNTREFERENCES(block,size_of_node(block));
		right_sib=giveAnyDmChildrenChild((n_dsc*)XADDR(parent),chcnt);
		left_sib=XNULL;
		if (right_sib==XNULL)
		{
			left_sib=getLastNonDmChildrenChild((n_dsc*)XADDR(parent),chcnt);
			if (left_sib!=XNULL)
			{
				
				CHECKP(left_sib);
				left= GETBLOCKBYNODE(left_sib);
			}	  
		}
		else
		{
			right = GETBLOCKBYNODE(right_sib);
			CHECKP(right_sib);
            left_sib=GETLEFTPOINTER(right_sib);
			if (left_sib!=XNULL)left= GETBLOCKBYNODE(left_sib);
		}
	}
	if (IS_DATA_BLOCK(parent))
		down_concurrent_micro_ops_number();
	if (left!=NULL)
	{ 
		CHECKP(left_sib);
		if ( GETTYPE(left->snode)==cdata ) 
			result = firstNodeInsertProcedure( left_sib,  parent,cdata,0); 
	}
    if (right!=NULL && result==XNULL) 
	{
		CHECKP(right_sib);
		if (GETTYPE(right->snode)==cdata) 
			result = secondElementInsertProcedure(right_sib,  parent,cdata,0);
	}
	if (result==XNULL) result= thirdElementAndTextInsertProcedure( left_sib, right_sib,  parent, NULL, 0,cdata,NULL);
	//text value
	addTextValue(result,value, size);
	//NODE STATISTICS
	(GETBLOCKBYNODE(result))->snode->nodecnt++;
	(GETBLOCKBYNODE(result))->snode->textcnt+=size;
    if (IS_DATA_BLOCK(parent))
	{
		update_idx_add(result,value,size);
		n_dsc* res_nd=(n_dsc*)XADDR(result);
		xptr left_indir=res_nd->ldsc;
		xptr right_indir=res_nd->rdsc;
		xptr par_indir=res_nd->pdsc;
		xptr indir=res_nd->indir;
		if (left_indir!=NULL)
		{
			CHECKP(left_indir);
			left_indir=((n_dsc*)XADDR(left_indir))->indir;
		}
		if (right_indir!=NULL)
		{
			CHECKP(right_indir);
			right_indir=((n_dsc*)XADDR(right_indir))->indir;
		}
		hl_logical_log_comment(indir,left_indir,right_indir,par_indir,value,size,true);
		CHECKP(result);
		up_concurrent_micro_ops_number();		
	}
	return result;
}
xptr insert_pi(xptr left_sib, xptr right_sib, xptr parent,const char* target, int tsize,const char* data, int dsize)
{
	node_blk_hdr *left=NULL;
	node_blk_hdr *right=NULL;
	xptr truep=XNULL;
	xptr result=XNULL;
    if ((right_sib==XNULL)&&(left_sib==XNULL)&&(parent==XNULL)) throw SYSTEM_EXCEPTION("Bad parameters");
	if (left_sib!=XNULL) 
	{
		CHECKP(left_sib); 
		left= GETBLOCKBYNODE(left_sib);
		if ( right_sib!=GETRIGHTPOINTER(left_sib)) 
			if  ( right_sib!=XNULL)   throw SYSTEM_EXCEPTION("micro.cpp,896,Bad parameters");
			else right_sib=GETRIGHTPOINTER(left_sib);
		xptr parind=GETPARENTPOINTER(left_sib);
		CHECKP(parind);
		truep=*((xptr*)XADDR(parind));
	}
	if (right_sib!=XNULL) 
	{
		CHECKP(right_sib);
		right = GETBLOCKBYNODE(right_sib);
		if  ( left_sib!=GETLEFTPOINTER(right_sib))
			if  ( left_sib!=XNULL)   throw SYSTEM_EXCEPTION("907,Bad parameters");
			else 
			{
				left_sib=GETLEFTPOINTER(right_sib);
				left= GETBLOCKBYNODE(left_sib);
			}
		if (truep==XNULL)
		{
			xptr parind=GETPARENTPOINTER(right_sib);
			CHECKP(parind);
			truep=*((xptr*)XADDR(parind));
		}
	}
	if (parent==XNULL) parent=truep;
	else if (parent!=truep && truep!=XNULL) throw SYSTEM_EXCEPTION("Bad parameters");
    if (left==NULL && right==NULL)
	{
		CHECKP(parent);
		node_blk_hdr* block=GETBLOCKBYNODE(parent);
		int chcnt=COUNTREFERENCES(block,size_of_node(block));
		right_sib=giveAnyDmChildrenChild((n_dsc*)XADDR(parent),chcnt);
		left_sib=XNULL;
		if (right_sib==XNULL)
		{
			left_sib=getLastNonDmChildrenChild((n_dsc*)XADDR(parent),chcnt);
			if (left_sib!=XNULL)
			{
				
				CHECKP(left_sib);
				left= GETBLOCKBYNODE(left_sib);
			}	  
		}
		else
		{
			right = GETBLOCKBYNODE(right_sib);
			CHECKP(right_sib);
            left_sib=GETLEFTPOINTER(right_sib);
			if (left_sib!=XNULL)left= GETBLOCKBYNODE(left_sib);
		}
	}
	if (IS_DATA_BLOCK(parent))
		down_concurrent_micro_ops_number();
	if (left!=NULL)
	{ 
		CHECKP(left_sib);
		if ( GETTYPE(left->snode)==pr_ins ) 
			result = firstNodeInsertProcedure( left_sib,  parent,pr_ins,0); 
	}
    if (right!=NULL && result==XNULL) 
	{
		CHECKP(right_sib);
		if (GETTYPE(right->snode)==pr_ins) 
			result = secondElementInsertProcedure(right_sib,  parent,pr_ins,0);
	}
	if (result==XNULL) result= thirdElementAndTextInsertProcedure( left_sib, right_sib,  parent, NULL, 0,pr_ins,NULL);
	//text value
	char* z=new char[tsize+dsize+1];
	memcpy(z,target,tsize);
	z[tsize]=' ';
	memcpy(z+tsize+1,data,dsize);
	addTextValue(result,z, tsize+dsize+1);
	//PHYS LOG
	if (IS_DATA_BLOCK(result)) 
	{
		hl_phys_log_change(&(((pi_dsc*)XADDR(result))->target),sizeof(shft));
	}
	((pi_dsc*)XADDR(result))->target=(shft)tsize;
	//NODE STATISTICS
	(GETBLOCKBYNODE(result))->snode->nodecnt++;
	(GETBLOCKBYNODE(result))->snode->textcnt+=(tsize+dsize+1);
    if (IS_DATA_BLOCK(parent))
	{
		update_idx_add(result,data,dsize);
		n_dsc* res_nd=(n_dsc*)XADDR(result);
		xptr left_indir=res_nd->ldsc;
		xptr right_indir=res_nd->rdsc;
		xptr par_indir=res_nd->pdsc;
		xptr indir=res_nd->indir;
		if (left_indir!=NULL)
		{
			CHECKP(left_indir);
			left_indir=((n_dsc*)XADDR(left_indir))->indir;
		}
		if (right_indir!=NULL)
		{
			CHECKP(right_indir);
			right_indir=((n_dsc*)XADDR(right_indir))->indir;
		}
		hl_logical_log_pi(indir,left_indir,right_indir,par_indir,z,tsize+dsize+1,tsize,true);
		CHECKP(result);
		up_concurrent_micro_ops_number();
		
	}
	delete[]z;
	return result;
}
xptr insert_namespace(xptr left_sib, xptr right_sib, xptr parent,xml_ns* ns)
{
	node_blk_hdr *left=NULL;
	node_blk_hdr *right=NULL;
	xptr result=XNULL;
	if ((right_sib==XNULL)&&(left_sib==XNULL)&&(parent==XNULL)) throw SYSTEM_EXCEPTION("Bad parameters");
	if (right_sib!=XNULL) 
	{
		CHECKP(right_sib);
		if  ( left_sib!=GETLEFTPOINTER(right_sib))
			if  ( left_sib!=XNULL)   throw SYSTEM_EXCEPTION("Bad parameters");
			else left_sib=GETLEFTPOINTER(right_sib);
	}
	if (left_sib!=XNULL) 
	{
		CHECKP(left_sib); 
		if ( right_sib!=GETRIGHTPOINTER(left_sib)) 
			if  ( right_sib!=XNULL)   throw SYSTEM_EXCEPTION("Bad parameters");
			else right_sib=GETRIGHTPOINTER(left_sib);
	}
	left= GETBLOCKBYNODE(left_sib);
	right = GETBLOCKBYNODE(right_sib);
	xptr par_indir=XNULL;
	if (parent==XNULL)
	{
		if (left_sib!=XNULL) 
			par_indir=GETPARENTPOINTER(left_sib);
		else if (right_sib!=XNULL) 
		{
			CHECKP(right_sib);
			par_indir=GETPARENTPOINTER(right_sib);
		}
		parent=removeIndirection(par_indir);
		CHECKP(parent);
	}
	else
	{
		CHECKP(parent);
		par_indir=((n_dsc*)XADDR(parent))->indir;
	}
	n_dsc* par_desc=(n_dsc*)XADDR(parent);
	int size=COUNTREFERENCES((GETBLOCKBYNODE(parent)),size_of_node((GETBLOCKBYNODE(parent))));
	if (((left_sib==XNULL)&&(right_sib==XNULL))&&giveAnyChild(par_desc,size)!=XNULL)
	{
		left_sib=getLastNamespaceChild(par_desc,size);
		if (left_sib==XNULL)
		{
			right_sib=giveFirstByOrderChild(parent,size);
		}
		else
		{
			left=GETBLOCKBYNODE(left_sib);
			CHECKP(left_sib);
			right_sib=GETRIGHTPOINTER(left_sib);
		}
	}
	if (IS_DATA_BLOCK(parent))
		down_concurrent_micro_ops_number();
	if (left!=NULL)
	{ 
		CHECKP(left_sib);
		result = firstNodeInsertProcedure( left_sib,  parent,xml_namespace,0); 
	}
	else
		result=thirdElementAndTextInsertProcedure(left_sib,right_sib,parent,NULL,NULL,xml_namespace,NULL);
	VMM_SIGNAL_MODIFICATION(result);
	((ns_dsc*)XADDR(result))->ns=ns;
	//NODE STATISTICS
	(GETBLOCKBYNODE(result))->snode->nodecnt++;
	if (IS_DATA_BLOCK(parent))
	{
		ns->counter++;
		n_dsc* res_nd=(n_dsc*)XADDR(result);
		xptr left_indir=res_nd->ldsc;
		xptr right_indir=res_nd->rdsc;
		xptr par_indir=res_nd->pdsc;
		xptr indir=res_nd->indir;
		xml_ns* nsl=((ns_dsc*)res_nd)->ns;
		if (left_indir!=NULL)
		{
			CHECKP(left_indir);
			left_indir=((n_dsc*)XADDR(left_indir))->indir;
		}
		if (right_indir!=NULL)
		{
			CHECKP(right_indir);
			right_indir=((n_dsc*)XADDR(right_indir))->indir;
		}
		hl_logical_log_namespace(indir,left_indir,right_indir,par_indir,(nsl)?nsl->uri:NULL,(nsl)?nsl->prefix:NULL ,true);
		CHECKP(result);
		up_concurrent_micro_ops_number();
		
	}
	return result;
}
xptr insert_text(xptr left_sib, xptr right_sib, xptr parent, const  void* value,int size,text_type ttype)
{
    //d_printf1("bit");fflush(stdout);
    #ifdef _MYDEBUG
		crm_out<<" insert_text";
	#endif
	node_blk_hdr *left=NULL;
	node_blk_hdr *right=NULL;
	xptr truep=XNULL;
	xptr result=XNULL;
	if (size<1)
		throw USER_EXCEPTION(SE2009);
	if ((right_sib==XNULL)&&(left_sib==XNULL)&&(parent==XNULL)) throw SYSTEM_EXCEPTION("Bad parameters");
	if (left_sib!=XNULL) 
	{
		#ifdef _MYDEBUG
			crm_out<<" insert_text->2";
		#endif
		CHECKP(left_sib); 
		left= GETBLOCKBYNODE(left_sib);
		if ( right_sib!=GETRIGHTPOINTER(left_sib)) 
			if  ( right_sib!=XNULL)   throw SYSTEM_EXCEPTION("Bad parameters");
			else right_sib=GETRIGHTPOINTER(left_sib);
		xptr parind=GETPARENTPOINTER(left_sib);
		CHECKP(parind);
		truep=*((xptr*)XADDR(parind));
	}
	if (right_sib!=XNULL) 
	{
		#ifdef _MYDEBUG
			crm_out<<" insert_text->1";
		#endif
		CHECKP(right_sib);
		right = GETBLOCKBYNODE(right_sib);
		if  ( left_sib!=GETLEFTPOINTER(right_sib))
			if  ( left_sib!=XNULL)   throw SYSTEM_EXCEPTION("Bad parameters");
			else left_sib=GETLEFTPOINTER(right_sib);
		if (truep==XNULL)
		{
			xptr parind=GETPARENTPOINTER(right_sib);
			CHECKP(parind);
			truep=*((xptr*)XADDR(parind));
		}
	}
	if (parent==XNULL) parent=truep;
	else if (parent!=truep && truep!=XNULL) throw SYSTEM_EXCEPTION("Bad parameters");
	if (IS_DATA_BLOCK(parent))
		down_concurrent_micro_ops_number();
    //d_printf1("0");fflush(stdout);
	int ins_type=0;
	if (left_sib!=XNULL)
	{
		CHECKP(left_sib);
		if (GETTYPE(left->snode)==text)
		{
		#ifdef _MYDEBUG
			crm_out<<" insert_text->3";
		#endif
            //d_printf1("1");fflush(stdout);
			//fillLogOfTextNodeChanged(left_sib);
            //d_printf1("!");fflush(stdout);
			appendTextValue(left_sib,value,size,ttype);
            //d_printf1("@");fflush(stdout);
			result=left_sib; 
            //d_printf1("#");fflush(stdout);
			ins_type=1;
		}
	}
    if (right_sib!=XNULL && result==XNULL) 
	{
		#ifdef _MYDEBUG
			crm_out<<" insert_text->4";
		#endif
		CHECKP(right_sib);
		if (GETTYPE(right->snode)==text)
		{
		 	#ifdef _MYDEBUG
				crm_out<<" insert_text->5";
			#endif
            //d_printf1("2");fflush(stdout);
			//fillLogOfTextNodeChanged(right_sib);
			insertTextValue(right_sib,value, size,ttype);
			result=right_sib;
			ins_type=2;
		}
	}
    if ((left!=NULL || right!=NULL)&& result==XNULL)
	{
		#ifdef _MYDEBUG
			crm_out<<" insert_text->6";
		#endif
        //d_printf1("3");fflush(stdout);
		xptr tmp=thirdElementAndTextInsertProcedure( left_sib, right_sib,  parent, NULL, 0,text,NULL);
		//NODE STATISTICS
		if (IS_DATA_BLOCK(parent))
			(GETBLOCKBYNODE(tmp))->snode->nodecnt++;
		addTextValue(tmp,value, size,ttype);
		result=tmp;
	}
	#ifdef _MYDEBUG
		crm_out<<"end of insert_text";
	#endif
    //d_printf1("4");fflush(stdout);
    if (result==XNULL)
	{
		result=textInsertProcedure(parent, value, size,ins_type,ttype);
	}
	CHECKP(result);
	(GETBLOCKBYNODE(result))->snode->textcnt+=size;
	if (IS_DATA_BLOCK(parent))
	{
		n_dsc* res_nd=(n_dsc*)XADDR(result);
		xptr indir=res_nd->indir;
		if (ins_type==0)
		{
			//n_dsc* res_nd=(n_dsc*)XADDR(result);
			xptr left_indir=res_nd->ldsc;
			xptr right_indir=res_nd->rdsc;
			xptr par_indir=res_nd->pdsc;
			//xptr indir=res_nd->indir;
			int sz=((t_dsc*)res_nd)->size;
			xptr ind_ptr=((t_dsc*)res_nd)->data;
			if (left_indir!=NULL)
			{
				CHECKP(left_indir);
				left_indir=((n_dsc*)XADDR(left_indir))->indir;
			}
			if (right_indir!=NULL)
			{
				CHECKP(right_indir);
				right_indir=((n_dsc*)XADDR(right_indir))->indir;
			}
			update_idx_add_txt(result);
			if (sz<=PSTRMAXSIZE)
			{
				CHECKP(ind_ptr);
				shft shift= *((shft*)XADDR(ind_ptr));
				char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;				
				hl_logical_log_text(indir,left_indir,right_indir,par_indir,data,sz,true);
				CHECKP(result);
			}
			else
			{
				hl_logical_log_text(indir,left_indir,right_indir,par_indir,ind_ptr,sz,true);
			}
		}
		else
		{
			if (ttype==text_mem)
				hl_logical_log_text_edit(indir, (char*)value,size,(ins_type==1)?false:true,true); 
			else
			{
				hl_logical_log_text_edit(indir, size,(ins_type==1)?false:true,true);
			}
		}

			up_concurrent_micro_ops_number();
	}

    //d_printf1("ait\n");fflush(stdout);
	return result;
}
void delete_node_inner_2 (xptr nodex, t_item type)
{
	//d_printf2("EL Node name=%s \n",name);
//	d_printf1("\nEL Node DELETE xptr=");
//	nodex.print();
	n_dsc* node=(n_dsc*)XADDR(nodex);
	//Deletion of inner nodes
	if (type==element||type==document)
	{
		//finding the last child node
		xptr child=getLastByOrderChildNode(nodex);
		xptr tmp;
		node_blk_hdr* block;
		//right-to-left recursive delete
		while(child!=XNULL)
		{
			CHECKP(child);
			tmp=((n_dsc*)XADDR(child))->ldsc;
			delete_node_inner_2(child,(GETBLOCKBYNODE(child))->snode->type);
			child=tmp;
		}
		CHECKP(nodex);
	}
#ifdef SE_ENABLE_FTSEARCH
 update_delete_sequence(nodex,(GETBLOCKBYNODE(nodex))->snode->ft_index_object); 
#endif
	xptr left_sib=GETLEFTPOINTER(nodex);
	xptr right_sib=GETRIGHTPOINTER(nodex);
	// 2 neigboring siblings merge if text
	if (left_sib!=XNULL && right_sib!=XNULL)
	{
		CHECKP(left_sib);
		if (GETTYPE((GETBLOCKBYNODE(left_sib))->snode)==text)
		{
			CHECKP(right_sib);
			if (GETTYPE((GETBLOCKBYNODE(right_sib))->snode)==text)
			{
				t_dsc* text_node=(t_dsc*)XADDR(right_sib);
				int size=text_node->size;
				if (size<=PSTRMAXSIZE)
				{
					char* z=new char[size];
					xptr ind_ptr=text_node->data;
					if (ind_ptr!=XNULL)
					{
						CHECKP(ind_ptr);
						shft shift= *((shft*)XADDR(ind_ptr));
						char* ptr=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
						memcpy(z,ptr,size);
					}
					CHECKP(left_sib);
					xptr lindir=((n_dsc*)XADDR(left_sib))->indir;
					if (IS_DATA_BLOCK(left_sib))
						down_concurrent_micro_ops_number();
					appendTextValue(left_sib,z,size,text_mem);
					hl_logical_log_text_edit(lindir, z,size,false,true); 
					if (IS_DATA_BLOCK(left_sib))
						up_concurrent_micro_ops_number();
					delete []z;
					CHECKP(right_sib);
					delete_node_inner_2 (right_sib, text);
				}
				else
				{
					
					CHECKP(left_sib);
					text_node=(t_dsc*)XADDR(left_sib);
					int l_size=text_node->size;
						if (l_size>PSTRMAXSIZE)
						{
							if (IS_DATA_BLOCK(left_sib))
							{
								update_idx_delete_text(left_sib);
								down_concurrent_micro_ops_number();
							}
							pstr_long_append_tail(left_sib, right_sib);
							CHECKP(left_sib);
							hl_logical_log_text_edit(text_node->indir,size,false,true); 
							if (IS_DATA_BLOCK(left_sib))
							{
								up_concurrent_micro_ops_number();
								update_idx_add_txt(left_sib);
							}
							CHECKP(right_sib);
							delete_node_inner_2 (right_sib, text);
							

						}
						else
						{
							char* z=new char[l_size];
							xptr ind_ptr=text_node->data;
							if (ind_ptr!=XNULL)
							{
								CHECKP(ind_ptr);
								shft shift= *((shft*)XADDR(ind_ptr));
								char* ptr=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
								memcpy(z,ptr,l_size);
							}
							CHECKP(right_sib);
							if (IS_DATA_BLOCK(right_sib))
								down_concurrent_micro_ops_number();
							hl_logical_log_text_edit(((n_dsc*)XADDR(right_sib))->indir, z,l_size,true,true); 
							insertTextValue(right_sib,z,l_size,text_mem);
							if (IS_DATA_BLOCK(right_sib))
								up_concurrent_micro_ops_number();
							delete []z;
							CHECKP(left_sib);
							delete_node_inner_2 (left_sib, text);
						}
				}				
			}
		}
	}
	//NODE STATISTICS
	CHECKP(nodex);
	(GETBLOCKBYNODE(nodex))->snode->nodecnt--;
	if (IS_DATA_BLOCK(nodex))
	{
	//	if (type!=document)
			down_concurrent_micro_ops_number();
		//logical log record
		
		xptr left_indir=node->ldsc;
		xptr right_indir=node->rdsc;
		xptr par_indir=node->pdsc;
		xptr indir=node->indir;
		if (left_indir!=NULL)
		{
			CHECKP(left_indir);
			left_indir=((n_dsc*)XADDR(left_indir))->indir;
		}
		if (right_indir!=NULL)
		{
			CHECKP(right_indir);
			right_indir=((n_dsc*)XADDR(right_indir))->indir;
		}
		CHECKP(nodex);
		schema_node* scn=(GETBLOCKBYNODE(nodex))->snode;
		switch (type)
		{
		case element:
			{
				xml_ns* ns=scn->xmlns;
				hl_logical_log_element(indir,left_indir,right_indir,par_indir,scn->name,((e_dsc*)node)->type,(ns)?ns->uri:NULL,(ns)?ns->prefix:NULL ,false);
				break;
			}
		case attribute:
			{
				char *name=scn->name;
				xml_ns *ns=scn->xmlns;
				xmlscm_type type=((a_dsc*)node)->type;
				xptr ind_ptr=((a_dsc*)node)->data;
				char* ptr=NULL;
				int size=((a_dsc*)node)->size;
				if (size>0)
				{
					xptr ind_ptr=((a_dsc*)node)->data;
					CHECKP(ind_ptr);
					shft shift= *((shft*)XADDR(ind_ptr));
					ptr=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
					update_idx_delete_text(scn,nodex,ptr,size);
					CHECKP(ind_ptr);
				}
				else
					update_idx_delete_text(scn,nodex,ptr,size);
				hl_logical_log_attribute(indir,left_indir,right_indir,par_indir,name,type,ptr,size,(ns)?ns->uri:NULL,(ns)?ns->prefix:NULL ,false);
				break;
			}
		case text: case comment: 
			{
				xptr ind_ptr=((t_dsc*)node)->data;
				char* ptr=NULL;
				int size=((t_dsc*)node)->size;
				update_idx_delete_text(nodex);
				if (type==text)
				{
					if (size<=PSTRMAXSIZE)
					{
						
						CHECKP(ind_ptr);
						shft shift= *((shft*)XADDR(ind_ptr));
						ptr=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
						CHECKP(ind_ptr);
						hl_logical_log_text(indir,left_indir,right_indir,par_indir,ptr,size,false);
					}
					else
					{

						hl_logical_log_text(indir,left_indir,right_indir,par_indir,ind_ptr,size,false);

					}
				}
				else
				{
					CHECKP(ind_ptr);
					shft shift= *((shft*)XADDR(ind_ptr));
					ptr=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
					CHECKP(ind_ptr);				
					hl_logical_log_comment(indir,left_indir,right_indir,par_indir,ptr,size,false);
				}
				break;
			}
		case cdata:
			{
				xptr ind_ptr=((t_dsc*)node)->data;
				char* ptr=NULL;
				int size=((t_dsc*)node)->size;	
				CHECKP(ind_ptr);
				shft shift= *((shft*)XADDR(ind_ptr));
				ptr=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
				CHECKP(ind_ptr);				
				hl_logical_log_comment(indir,left_indir,right_indir,par_indir,ptr,size,false);
				break;
			}				
		case pr_ins:
			{
				xptr ind_ptr=((t_dsc*)node)->data;
				char* ptr=NULL;
				int size=((t_dsc*)node)->size;
				int tsize=((pi_dsc*)node)->target;
				if (size>0)
				{
					xptr ind_ptr=((t_dsc*)node)->data;
					CHECKP(ind_ptr);
					shft shift= *((shft*)XADDR(ind_ptr));
					ptr=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
					update_idx_delete_text(scn,nodex,ptr+tsize+1,size-tsize-1);
					CHECKP(ind_ptr);
				}
				else
					update_idx_delete_text(scn,nodex,ptr,size);
				hl_logical_log_pi(indir,left_indir,right_indir,par_indir,ptr,size,tsize,false);
				break;
			}
		case xml_namespace:
			{
				xml_ns* ns=((ns_dsc*)node)->ns;
				hl_logical_log_namespace(indir,left_indir,right_indir,par_indir,(ns)?ns->uri:NULL,(ns)?ns->prefix:NULL ,false);
				break;
			}

		}
		
	}
	//update of parent  pointer to first child by sort 
	if (type!=document)
	{	
		xptr par_indir=node->pdsc;
		n_dsc* prev=getPreviousDescriptorOfSameSort(node);
		if (prev==NULL || prev->pdsc!=par_indir)
		{
			CHECKP(nodex);
			n_dsc* next=getNextDescriptorOfSameSort(node);
			xptr new_point=XNULL;
			if (next!=NULL && next->pdsc==par_indir) new_point=ADDR2XPTR(next);
			xptr parent=removeIndirection(par_indir);
			CHECKP(parent);
			VMM_SIGNAL_MODIFICATION(parent);
			updateChildPointer((n_dsc*)XADDR(parent),nodex,new_point);
		}
		CHECKP(nodex);
	}
	
	// right-left siblings update
	if (left_sib!=XNULL)  
	{
		CHECKP(left_sib)  ;
		VMM_SIGNAL_MODIFICATION(left_sib);
		//PHYS LOG
		if (IS_DATA_BLOCK(left_sib)) 
			hl_phys_log_change(&((n_dsc*)XADDR(left_sib))->rdsc,sizeof(xptr));
		((n_dsc*)XADDR(left_sib))->rdsc=right_sib;
	}
	if (right_sib!=XNULL)  
	{
		CHECKP(right_sib)  ;
		VMM_SIGNAL_MODIFICATION(right_sib);
		//PHYS LOG
		if (IS_DATA_BLOCK(right_sib)) 
			hl_phys_log_change(&((n_dsc*)XADDR(right_sib))->ldsc,sizeof(xptr));
		((n_dsc*)XADDR(right_sib))->ldsc=left_sib;
	}
	
	//text value deleting
	CHECKP(nodex);
	xptr ind=node->indir;
	if (type!=element && type!=xml_namespace&& ((t_dsc*)node)->size!=0)
		deleteTextValue(nodex);
	/*if (type==xml_namespace)
		((ns_dsc*)node)->ns->counter--;*/
	//indirection record
//	d_printf1("\nEL Node DELETE indir=");
//	ind.print();
	del_record_from_indirection_table(ind);
	CHECKP(nodex);
	//nid
	nid_delete(nodex);
	// same sort descriptor updates && block update
	CHECKP(nodex);
	VMM_SIGNAL_MODIFICATION(nodex);
	node_blk_hdr*  block=GETBLOCKBYNODE(nodex);
	if (node->desc_prev==0 && node->desc_next==0)
		deleteBlock(block);
	else
	{
		if (node->desc_prev==0) 
		{
			//PHYS LOG
			if (IS_DATA_BLOCK(nodex)) 
				hl_phys_log_change(&block->desc_first,sizeof(shft));
			block->desc_first=node->desc_next;
		}
		else 
		{
			//PHYS LOG
			if (IS_DATA_BLOCK(nodex)) 
				hl_phys_log_change(&(GETPOINTERTODESC(block,node->desc_prev))->desc_next,sizeof(shft));
			(GETPOINTERTODESC(block,node->desc_prev))->desc_next=node->desc_next;
		}
		if (node->desc_next==0) 
		{
			//PHYS LOG
			if (IS_DATA_BLOCK(nodex)) 
				hl_phys_log_change(&block->desc_last,sizeof(shft));
			block->desc_last=node->desc_prev;
		}
		else 
		{
			//PHYS LOG
			if (IS_DATA_BLOCK(nodex)) 
				hl_phys_log_change(&(GETPOINTERTODESC(block,node->desc_next))->desc_prev,sizeof(shft));
			(GETPOINTERTODESC(block,node->desc_next))->desc_prev=node->desc_prev;
		}
		//PHYS LOG
		if (IS_DATA_BLOCK(nodex)) 
		{
			hl_phys_log_change(node,sizeof(shft));
			hl_phys_log_change(&block->count,sizeof(shft));
			hl_phys_log_change(&block->free_first,sizeof(shft));
		}
		*((shft*) node)=block->free_first;
		block->free_first=CALCSHIFT(node,block);
		block->count=block->count-1;
	}
	if (IS_DATA_BLOCK(nodex)&& type!=document)
		up_concurrent_micro_ops_number();
}
void delete_node_inner (xptr node, node_blk_hdr*  block,t_item type)
{
	#ifdef _MYDEBUG
		crm_out<<"delete_node";
	#endif
	n_dsc* node_d=(n_dsc*)XADDR(node);
	xptr par_indir= GETPARENTPOINTER(node); 
	xptr left_sib=GETLEFTPOINTER(node);
	xptr right_sib=GETRIGHTPOINTER(node);
	if (type!=document)
	{
		#ifdef _MYDEBUG
			crm_out<<"delete_node->1";
		#endif
		n_dsc* prev=getPreviousDescriptorOfSameSort(node_d);
		if (prev==NULL || prev->pdsc!=par_indir)
		{
			#ifdef _MYDEBUG
				crm_out<<"delete_node->2";
			#endif
			CHECKP(node);
			n_dsc* next=getNextDescriptorOfSameSort(node_d);
			xptr new_point=XNULL;
			if (next!=NULL && next->pdsc==par_indir) new_point=ADDR2XPTR(next);
			CHECKP(par_indir);
			xptr parent=*((xptr*)XADDR(par_indir));
			CHECKP(parent);
			VMM_SIGNAL_MODIFICATION(parent);
			updateChildPointer((n_dsc*)XADDR(parent),node,new_point);
		}
		CHECKP(node);
	}
	if (left_sib!=XNULL)  
	{
		#ifdef _MYDEBUG
			crm_out<<"delete_node->3";
		#endif
		CHECKP(left_sib)  ;
		VMM_SIGNAL_MODIFICATION(left_sib);
		//PHYS LOG
		if (IS_DATA_BLOCK(left_sib)) 
			hl_phys_log_change(&((n_dsc*)XADDR(left_sib))->rdsc,sizeof(xptr));
		((n_dsc*)XADDR(left_sib))->rdsc=right_sib;
	}
	if (right_sib!=XNULL)  
	{
		#ifdef _MYDEBUG
			crm_out<<"delete_node->4";
		#endif
		CHECKP(right_sib)  ;
		VMM_SIGNAL_MODIFICATION(right_sib);
		//PHYS LOG
		if (IS_DATA_BLOCK(right_sib)) 
			hl_phys_log_change(&((n_dsc*)XADDR(right_sib))->ldsc,sizeof(xptr));
		((n_dsc*)XADDR(right_sib))->ldsc=left_sib;
	}
	CHECKP(node);
	VMM_SIGNAL_MODIFICATION(node);
	delete_inner_nodes(node_d);
	if (node_d->desc_prev==0 && node_d->desc_next==0)
	{
		#ifdef _MYDEBUG
			crm_out<<"delete_node->5";
		#endif
		deleteBlock(block);
		return;
	}
	if (node_d->desc_prev==0) 
	{
		//PHYS LOG
		if (IS_DATA_BLOCK(node)) 
			hl_phys_log_change(&block->desc_first,sizeof(shft));
		block->desc_first=node_d->desc_next;
	}
	else
	{
		//PHYS LOG
		if (IS_DATA_BLOCK(node))
			hl_phys_log_change(&(GETPOINTERTODESC(block,node_d->desc_prev))->desc_next,sizeof(shft));
		(GETPOINTERTODESC(block,node_d->desc_prev))->desc_next=node_d->desc_next;
	}
	if (node_d->desc_next==0) 
	{
		//PHYS LOG
		if (IS_DATA_BLOCK(node)) 
			hl_phys_log_change(&block->desc_last,sizeof(shft));
		block->desc_last=node_d->desc_prev;
	}
	else 
	{
		//PHYS LOG
		if (IS_DATA_BLOCK(node))
			hl_phys_log_change(&(GETPOINTERTODESC(block,node_d->desc_next))->desc_prev,sizeof(shft));
		(GETPOINTERTODESC(block,node_d->desc_next))->desc_prev=node_d->desc_prev;
	}
	//PHYS LOG
	if (IS_DATA_BLOCK(node)) 
	{
		hl_phys_log_change(node_d,sizeof(shft));
		hl_phys_log_change(&block->count,sizeof(shft));
		hl_phys_log_change(&block->free_first,sizeof(shft));
	}
	*((shft*) node_d)=block->free_first;
	block->free_first=CALCSHIFT(node_d,block);
	block->count=block->count-1;
	if (left_sib!=XNULL && right_sib!=XNULL)
	{
		CHECKP(left_sib);
		if (GETTYPE((GETBLOCKBYNODE(left_sib))->snode)==text)
		{
			CHECKP(right_sib);
			if (GETTYPE((GETBLOCKBYNODE(right_sib))->snode)==text)
			{
				t_dsc* text_node=(t_dsc*)XADDR(right_sib);
				int size=text_node->size;
				char* z=new char[size];
				xptr ind_ptr=text_node->data;
				if (ind_ptr!=XNULL)
				{
					CHECKP(ind_ptr);
					shft shift= *((shft*)XADDR(ind_ptr));
					char* ptr=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
					memcpy(z,ptr,size);
				}
				CHECKP(left_sib);
				appendTextValue(left_sib,z,size,text_mem);
				delete []z;
				CHECKP(right_sib);
				delete_node_inner (right_sib, GETBLOCKBYNODE(right_sib),text);
			}
		}
	}
}

void delete_node (xptr node)
{
	node_blk_hdr* block=GETBLOCKBYNODE(node);
	CHECKP(node);
	t_item type=GETTYPE(block->snode);
	if (type==document)
		throw USER_EXCEPTION(SE2036);
#ifdef SE_ENABLE_FTSEARCH
init_ft_sequences (node,XNULL,XNULL);
#endif
#ifdef FASTDELETE
	delete_node_inner (node,block,type);
#else 
	delete_node_inner_2 (node,type);
#endif
}

void delete_doc_node (xptr node)
{
	node_blk_hdr* block=GETBLOCKBYNODE(node);
	CHECKP(node);
	t_item type=GETTYPE(block->snode);
	if (type!=document)
		throw SYSTEM_EXCEPTION("Wrong type of node");
/*#ifdef SE_ENABLE_FTSEARCH
    init_ft_sequences (node,XNULL,XNULL);
#endif*/
#ifdef FASTDELETE
	delete_node_inner (node,block,type);
#else 
	delete_node_inner_2 (node,type);
#endif

}
