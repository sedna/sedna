/*
 * File:  mo_write_utils.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/mo/micro.h"
#include "tr/vmm/vmm.h"
#include "tr/locks/locks.h"
#include "tr/structures/schema.h"
#include "tr/structures/metadata.h"
#include "tr/structures/indirection.h"
//#include "crmutils.h"
#include "tr/pstr/pstr.h"
#include "tr/pstr/pstr_long.h"
#include "tr/crmutils/node_utils.h"
#include "tr/log/log.h"
#include "tr/idx/index_data.h"
#include "tr/strings/e_string.h"
void makeNewBlockConsistentAfterFilling(xptr block, xptr node,shft shift_size)
{
	shft counter=1;
	node_blk_hdr * block_hdr=(node_blk_hdr *)(XADDR(block));
	n_dsc* node_hdr=(n_dsc*)(XADDR(node));
	shft firstdesc= block_hdr->free_first;
	if (CALCSHIFT(node_hdr,block_hdr)<PAGE_SIZE) 
		block_hdr->free_first=CALCSHIFT(node_hdr,block_hdr);
	else block_hdr->free_first=0;
	block_hdr->desc_first=firstdesc;
	n_dsc* tmp=GETPOINTERTODESC(block_hdr,firstdesc);
	tmp->desc_prev=0;
	while (CALCSHIFT(node_hdr,tmp)>shift_size)
	{
		tmp->desc_next=CALCSHIFT(tmp,block_hdr)+shift_size;
		tmp=(n_dsc*)((char*)tmp+shift_size);
		tmp->desc_prev=CALCSHIFT(tmp,block_hdr)-shift_size;
		counter++;
	}
	tmp->desc_next=0;
	block_hdr->count+=counter;
	block_hdr->desc_last=CALCSHIFT(tmp,block_hdr);
}

void deleteBlock(node_blk_hdr * block)
{
    xptr bladdr=ADDR2XPTR(block);
	xptr tmp1=block->nblk;
	xptr tmp2=block->pblk;
	if (tmp1!=XNULL)
	{
		CHECKP(tmp1);
		VMM_SIGNAL_MODIFICATION(tmp1);
		//PHYS LOG REC
		if (IS_DATA_BLOCK(tmp1))
			hl_phys_log_change(&((node_blk_hdr *)XADDR(tmp1))->pblk,sizeof(xptr));
		((node_blk_hdr *)XADDR(tmp1))->pblk=tmp2;
	}
	if (tmp2!=XNULL)
	{
		CHECKP(tmp2);
		VMM_SIGNAL_MODIFICATION(tmp2);
		//PHYS LOG REC
		if (IS_DATA_BLOCK(tmp2))
			hl_phys_log_change(&((node_blk_hdr *)XADDR(tmp2))->nblk,sizeof(xptr));
		((node_blk_hdr *)XADDR(tmp2))->nblk=tmp1;
		CHECKP(bladdr);
	}
	else
	{
	  CHECKP(bladdr);
	  schema_node* scm=GETSCHEMENODEX(bladdr);
	  UPDATEFIRSTBLOCKPOINTER(scm,tmp1);
	}
	block->snode->blockcnt--;
	//PHYS LOG REC
	if (IS_DATA_BLOCK(bladdr))
	{
		hl_phys_log_change_blk(XADDR(bladdr));
	}
	vmm_delete_block(bladdr) ;
}

/* Makes block consistent after cutting the begining
*/
void makeBlockConsistentAfterCuttingTheBeginning(node_blk_hdr *block,n_dsc* node,shft counter)
{
	shft freespace=block->free_first;
	block->free_first=block->desc_first;
	n_dsc* f_leaved= GETNEXTDESCRIPTOR_BL(block,node);
	n_dsc* tmp=GETPOINTERTODESC(block,block->desc_first);
	n_dsc* nextnode= GETNEXTDESCRIPTOR_BL(block,tmp);
	while (tmp!=node)
	{
		MAKEFREESPACE(tmp,CALCSHIFT(nextnode,block));     
		tmp=nextnode;
		nextnode= GETNEXTDESCRIPTOR_BL(block,tmp);
	}
	MAKEFREESPACE(tmp,freespace);
	if (f_leaved!=((n_dsc*)(block))) 
	{
		f_leaved->desc_prev=0;
		block->desc_first=CALCSHIFT(f_leaved,block);   
		block->count-=counter;
	}
	else
	{
		deleteBlock(block);
		return;
	}
}

void makeBlockConsistentAfterCuttingTheEnd(node_blk_hdr *block,n_dsc* node,shft counter)
{
	shft freespace=block->free_first;
	block->free_first=CALCSHIFT(node,block);
	n_dsc* tmp= GETPREVIOUSDESCRIPTOR_BL(block,node);
	if (tmp!=((n_dsc*)(block))) 
	{
		tmp->desc_next=0;
		block->desc_last=CALCSHIFT(tmp,block);   
		block->count-=counter;
	}
	else 
	{
		deleteBlock(block);
		return;
	}
	tmp=node;
	n_dsc* nextnode= GETNEXTDESCRIPTOR_BL(block,tmp);
	while (nextnode!=((n_dsc*)(block)))
	{
		MAKEFREESPACE( tmp,CALCSHIFT(nextnode,block));     
		tmp=nextnode;
		nextnode= GETNEXTDESCRIPTOR_BL(block,tmp);
	}
	MAKEFREESPACE(tmp,freespace);
}

void updateChildPointer (n_dsc* parent,xptr old_xptr,xptr dest)
{
 xptr* childx=(xptr*)((char*)parent+size_of_node(GETBLOCKBYNODE_ADDR(parent)));

 while (*childx!=old_xptr)   childx+=1;
 //PHYS LOG
 if (IS_DATA_BLOCK(ADDR2XPTR(parent))) 
	 hl_phys_log_change(childx,sizeof(xptr));
 *childx=dest;
}
void shiftNodeToTheNewBlockExpanded(n_dsc* source,xptr dest,shft new_size,shft old_size,node_blk_hdr * block)
{
	int chcnt=((int)((new_size-old_size)/ sizeof(xptr)));
	shiftNodeToTheNewBlock( source,dest,old_size,block);
	VMM_SIGNAL_MODIFICATION(dest);
	xptr* childx=(xptr*)((char*)XADDR(dest)+old_size);
	for (int i=0;i<chcnt;i++)
	{
		*childx=XNULL;
		childx+=1;
	}

}

xptr shiftLastNodeToTheNextBlock(node_blk_hdr* block)
{
	n_dsc* source=GETPOINTERTODESC(block,block->desc_last);
	shft shift=source->desc_prev;
	n_dsc* prev_desc=NULL;
	xptr par_indir;
	xptr old_xptr=ADDR2XPTR(source);
	xptr next_blk=block->nblk;
    shft size=block->dsc_size;
	if (shift!=0)
	{
		prev_desc=GETPOINTERTODESC(block,shift);
		par_indir=prev_desc->pdsc;
	}
	else
	{
		node_blk_hdr * pblk=(node_blk_hdr *)XADDR(block->pblk);
		if (block->pblk!=XNULL)
		{
			CHECKP(block->pblk);
			prev_desc=GETPOINTERTODESC(pblk,pblk->desc_last);
			par_indir=prev_desc->pdsc;
			CHECKP(old_xptr);
		}
		else par_indir=XNULL;		
	}
    CHECKP(next_blk);
	node_blk_hdr* new_block= GETBLOCKBYNODE(next_blk);
	shft next_first=*((shft*)((char*)new_block+new_block->free_first));
	xptr dest=ADDR2XPTR(GETPOINTERTODESC(new_block,new_block->free_first));
	if (IS_DATA_BLOCK(dest)) 
		hl_phys_log_change(XADDR(dest),new_block->dsc_size);
	CHECKP(old_xptr);
	copyDescriptor (source, dest,size);
	if (new_block->dsc_size>size)
	{
		
		int chcnt=((int)((new_block->dsc_size-size)/ sizeof(xptr)));
		xptr* childx=(xptr*)((char*)XADDR(dest)+size);
		if (IS_DATA_BLOCK(dest))
			hl_phys_log_change(childx,sizeof(xptr)*chcnt);
		for (int i=0;i<chcnt;i++)
		{
			*childx=XNULL;
			childx+=1;
		}
	}
	n_dsc* new_pointer=(n_dsc*)(XADDR(dest));
	new_pointer->desc_next=new_block->desc_first;
	new_pointer->desc_prev=0;
	xptr tmp1= new_pointer->ldsc;
	xptr tmp2= new_pointer->rdsc;
	xptr indir=new_pointer->indir;
	if  (tmp1!=XNULL)
	{
		CHECKP(tmp1);
		VMM_SIGNAL_MODIFICATION(tmp1);
		//PHYS LOG
		if (IS_DATA_BLOCK(tmp1)) 
			hl_phys_log_change(&(((n_dsc*)XADDR(tmp1))->rdsc),sizeof(xptr));
		UPDATERIGHTPOINTER(tmp1,dest);
	}
	if  (tmp2!=XNULL)
	{
		CHECKP(tmp2);
		VMM_SIGNAL_MODIFICATION(tmp2);
		//PHYS LOG
		if (IS_DATA_BLOCK(tmp1)) 
			hl_phys_log_change(&(((n_dsc*)XADDR(tmp2))->ldsc),sizeof(xptr));
		UPDATELEFTPOINTER(tmp2,dest); 
	}
	CHECKP(indir);
	VMM_SIGNAL_MODIFICATION(indir);
	//PHYS LOG
	if (IS_DATA_BLOCK(indir)) 
		hl_phys_log_change(XADDR(indir),sizeof(xptr));
	*((xptr*)XADDR(indir))=dest;
	CHECKP( dest );
	indir=new_pointer->pdsc;
	if (indir!=par_indir)
	{
		CHECKP(indir);
		xptr parent= *((xptr*)XADDR(indir));
		CHECKP( parent );
		VMM_SIGNAL_MODIFICATION(parent);
		updateChildPointer((n_dsc*)(XADDR(parent)),old_xptr,dest);
	}
	CHECKP(old_xptr);
	//restoring the order in the previous block
	if (IS_DATA_BLOCK(old_xptr)) 
	{
		hl_phys_log_change(&(block->desc_last),sizeof(shft));
		hl_phys_log_change(&(block->free_first),sizeof(shft));
		hl_phys_log_change(&(block->count),sizeof(shft));
		hl_phys_log_change(source,sizeof(shft));
		hl_phys_log_change(&((GETPOINTERTODESC(block,source->desc_prev))->desc_next),sizeof(shft));
	}
	block->desc_last=source->desc_prev;
	block->count--;
	(GETPOINTERTODESC(block,source->desc_prev))->desc_next=0;
	*((shft*)source)=block->free_first;
	block->free_first=CALCSHIFT(source,block);
	CHECKP(dest);
	if (IS_DATA_BLOCK(dest)) 
	{
		hl_phys_log_change(&(new_block->desc_first),sizeof(shft));
		hl_phys_log_change(&(new_block->count),sizeof(shft));
		hl_phys_log_change(&(new_block->free_first),sizeof(shft));
		if (new_block->count>0)
			hl_phys_log_change(&((GETPOINTERTODESC(new_block,new_block->desc_first))->desc_prev),sizeof(shft));
		else
			hl_phys_log_change(&(new_block->desc_last),sizeof(shft));
	}
	new_block->count++;
	if (new_block->count>1)
		(GETPOINTERTODESC(new_block,new_block->desc_first))->desc_prev=new_block->free_first;
	else
		new_block->desc_last=CALCSHIFT(new_pointer,new_block);
	new_block->free_first=next_first;
	new_block->desc_first=CALCSHIFT(new_pointer,new_block);
	return dest;
}
xptr shiftFirstNodeToThePreviousBlock(node_blk_hdr* block)
{
	n_dsc* source=GETPOINTERTODESC(block,block->desc_first);
	xptr par_indir;
	xptr old_xptr=ADDR2XPTR(source);
	xptr prev_blk=block->pblk;
    shft size=block->dsc_size;
	node_blk_hdr * pr_blk=(node_blk_hdr *)XADDR(prev_blk);
    CHECKP(prev_blk);
	n_dsc* prev_desc=GETPOINTERTODESC(pr_blk,pr_blk->desc_last);
	par_indir=prev_desc->pdsc;

    shft next_first=*((shft*)((char*)pr_blk+pr_blk->free_first));
	xptr dest=ADDR2XPTR(GETPOINTERTODESC(pr_blk,pr_blk->free_first));
	if (IS_DATA_BLOCK(dest)) 
		hl_phys_log_change(XADDR(dest),pr_blk->dsc_size);
	CHECKP(old_xptr);
	copyDescriptor (source, dest,size);
	if (pr_blk->dsc_size>size)
	{
		
		int chcnt=((int)((pr_blk->dsc_size-size)/ sizeof(xptr)));
		xptr* childx=(xptr*)((char*)XADDR(dest)+size);
		for (int i=0;i<chcnt;i++)
		{
			*childx=XNULL;
			childx+=1;
		}
	}
	n_dsc* new_pointer=(n_dsc*)(XADDR(dest));
	new_pointer->desc_next=0;
	new_pointer->desc_prev=pr_blk->desc_last;
	xptr tmp1= new_pointer->ldsc;
	xptr tmp2= new_pointer->rdsc;
	xptr indir=new_pointer->indir;
	if  (tmp1!=XNULL)
	{
		CHECKP(tmp1);
		VMM_SIGNAL_MODIFICATION(tmp1);
		//PHYS LOG
		if (IS_DATA_BLOCK(tmp1)) 
			hl_phys_log_change(&(((n_dsc*)XADDR(tmp1))->rdsc),sizeof(xptr));
		UPDATERIGHTPOINTER(tmp1,dest);
	}
	if  (tmp2!=XNULL)
	{
		CHECKP(tmp2);
		VMM_SIGNAL_MODIFICATION(tmp2);
		//PHYS LOG
		if (IS_DATA_BLOCK(tmp1)) 
			hl_phys_log_change(&(((n_dsc*)XADDR(tmp2))->ldsc),sizeof(xptr));
		UPDATELEFTPOINTER(tmp2,dest); 
	}
	CHECKP(indir);
	VMM_SIGNAL_MODIFICATION(indir);
	//PHYS LOG
	if (IS_DATA_BLOCK(indir)) 
		hl_phys_log_change(XADDR(indir),sizeof(xptr));
	*((xptr*)XADDR(indir))=dest;
	CHECKP( dest );
	indir=new_pointer->pdsc;
	if (indir!=par_indir)
	{
		CHECKP(indir);
		xptr parent= *((xptr*)XADDR(indir));
		CHECKP( parent );
		VMM_SIGNAL_MODIFICATION(parent);
		updateChildPointer((n_dsc*)(XADDR(parent)),old_xptr,dest);
	}
	CHECKP(old_xptr);
	//restoring the order in the previous block
	if (IS_DATA_BLOCK(old_xptr)) 
	{
		hl_phys_log_change(&(block->desc_first),sizeof(shft));
		hl_phys_log_change(&(block->free_first),sizeof(shft));
		hl_phys_log_change(&(block->count),sizeof(shft));
		hl_phys_log_change(source,sizeof(shft));
		hl_phys_log_change(&((GETPOINTERTODESC(block,source->desc_next))->desc_prev),sizeof(shft));
	}
	block->desc_first=source->desc_next;
	block->count--;
	(GETPOINTERTODESC(block,source->desc_next))->desc_prev=0;
	*((shft*)source)=block->free_first;
	block->free_first=CALCSHIFT(source,block);
	CHECKP(dest);
	if (IS_DATA_BLOCK(dest)) 
	{
		hl_phys_log_change(&(pr_blk->desc_last),sizeof(shft));
		hl_phys_log_change(&(pr_blk->count),sizeof(shft));
		hl_phys_log_change(&(pr_blk->free_first),sizeof(shft));
		hl_phys_log_change(&((GETPOINTERTODESC(pr_blk,pr_blk->desc_last))->desc_next),sizeof(shft));
	}
	pr_blk->count++;
	(GETPOINTERTODESC(pr_blk,pr_blk->desc_last))->desc_next=pr_blk->free_first;
	pr_blk->free_first=next_first;
	pr_blk->desc_last=CALCSHIFT(new_pointer,pr_blk);
	return dest;
}
void shiftNodeToTheNewBlock(n_dsc* source,xptr dest,shft size,node_blk_hdr * block)
{
	shft shift=source->desc_prev;
	n_dsc* prev_desc=NULL;
	xptr par_indir;
	xptr old_xptr=ADDR2XPTR(source);
	if (shift!=0)
	{
		prev_desc=GETPOINTERTODESC(block,shift);
		par_indir=prev_desc->pdsc;
	}
	else
	{
		node_blk_hdr * pblk=(node_blk_hdr *)XADDR(block->pblk);
		if (block->pblk!=XNULL)
		{
			CHECKP(block->pblk);
			if (pblk->count==0) 
			{
				if (pblk->pblk!=XNULL)
				{                    	
					CHECKP(pblk->pblk);
					pblk=(node_blk_hdr *)XADDR(pblk->pblk);
				}
			}
			if (pblk->desc_last!=0)
			{
				prev_desc=GETPOINTERTODESC(pblk,pblk->desc_last);
				par_indir=prev_desc->pdsc;				
			}
			else par_indir=XNULL;
			CHECKP(old_xptr);
		}
		else par_indir=XNULL;
	}
	copyDescriptor (source, dest,size);
	n_dsc* new_pointer=(n_dsc*)(XADDR(dest));
	xptr tmp1= new_pointer->ldsc;
	xptr tmp2= new_pointer->rdsc;
	xptr indir=new_pointer->indir;
	if  (tmp1!=XNULL)
	{
		CHECKP(tmp1);
		VMM_SIGNAL_MODIFICATION(tmp1);
		//PHYS LOG
		if (IS_DATA_BLOCK(tmp1)) 
			hl_phys_log_change(&(((n_dsc*)XADDR(tmp1))->rdsc),sizeof(xptr));
		UPDATERIGHTPOINTER(tmp1,dest);
	}
	if  (tmp2!=XNULL)
	{
		CHECKP(tmp2);
		VMM_SIGNAL_MODIFICATION(tmp2);
		//PHYS LOG
		if (IS_DATA_BLOCK(tmp1)) 
			hl_phys_log_change(&(((n_dsc*)XADDR(tmp2))->ldsc),sizeof(xptr));
		UPDATELEFTPOINTER(tmp2,dest); 
	}
	
	CHECKP(indir);
	VMM_SIGNAL_MODIFICATION(indir);
	//PHYS LOG
	if (IS_DATA_BLOCK(indir)) 
		hl_phys_log_change(XADDR(indir),sizeof(xptr));
	*((xptr*)XADDR(indir))=dest;
	CHECKP( dest );
	indir=new_pointer->pdsc;
	if (indir!=par_indir)
	{
		CHECKP(indir);
		xptr parent= *((xptr*)XADDR(indir));
		CHECKP( parent );
		VMM_SIGNAL_MODIFICATION(parent);
		updateChildPointer((n_dsc*)(XADDR(parent)),old_xptr,dest);
	}
	CHECKP(dest);
}


xptr createBlockNextToTheCurrentBlock (node_blk_hdr * block)
{
    xptr old_blk=ADDR2XPTR(block);
	node_blk_hdr* tmp= se_new node_blk_hdr;
	*tmp=*block;
	xptr new_block;
	bool persistent= IS_DATA_BLOCK(old_blk);
	if (persistent)
	{
		vmm_alloc_data_block(&new_block);
		//PHYS LOG
		hl_phys_log_create_node_blk(XADDR(new_block));
	}
	else
		vmm_alloc_tmp_block(&new_block);
	node_blk_hdr::init(XADDR(new_block),tmp->dsc_size);
	node_blk_hdr * new_block_hdr=(node_blk_hdr *)(XADDR(new_block));
	VMM_SIGNAL_MODIFICATION(new_block);
	new_block_hdr->snode=tmp->snode;
    new_block_hdr->pblk=old_blk;   
    new_block_hdr->nblk=tmp->nblk;
	xptr tmp1=tmp->nblk;
	if (tmp1!=XNULL)
	{
		CHECKP(tmp1);
		VMM_SIGNAL_MODIFICATION(tmp1);
		//PHYS LOG REC
		node_blk_hdr * tmp_b=(node_blk_hdr *)(XADDR(tmp1));
		if (IS_DATA_BLOCK(tmp1))
			hl_phys_log_change(&(tmp_b->pblk),sizeof(xptr));
		tmp_b->pblk=new_block;
	}
	CHECKP(old_blk);
	//PHYS LOG REC
	if (IS_DATA_BLOCK(old_blk))
		hl_phys_log_change(&(block->nblk),sizeof(xptr));
	VMM_SIGNAL_MODIFICATION(old_blk);
	block->nblk=new_block;
	block->snode->blockcnt++;
	//CHECKP(new_block);
	delete tmp;
	return new_block;
}
int splitBlockIfFullAfterLeftInsert(xptr& nodex)
{
	shft shift;
	n_dsc* tmp;
	xptr med;
	n_dsc* node=(n_dsc*)(XADDR(nodex));
	node_blk_hdr * block_node= GETBLOCKBYNODE(nodex);
	int use_cs=0;
	if ((shift=block_node->free_first)==0)
	{
		shft nd=CALCSHIFT(node,block_node);
		if (!(block_node->desc_last==nd))
		{
			if (!(block_node->desc_first==nd))
			{
				xptr next_blk=block_node->nblk;
				if (next_blk!=XNULL)
				{
					int descsz=block_node->dsc_size;
					CHECKP(next_blk);
					node_blk_hdr * block_nxt= GETBLOCKBYNODE(next_blk);
					if (block_nxt->free_first!=0 && block_nxt->dsc_size>=descsz)
						use_cs=1;
					else
						use_cs=6;	
				}
				else
					use_cs=6;				
			}
			else
			{
				xptr next_blk=block_node->nblk;
				if (next_blk!=XNULL)
				{
					int descsz=block_node->dsc_size;
					CHECKP(next_blk);
					node_blk_hdr * block_nxt= GETBLOCKBYNODE(next_blk);
					if (block_nxt->free_first!=0 && block_nxt->dsc_size>=descsz)
						use_cs=1;
					else
						use_cs=7;	
				}
				else
					use_cs=7;	
			}
			if (use_cs==6)
			{
				CHECKP(nodex);
				xptr prev_blk=block_node->pblk;
				if (prev_blk!=XNULL)
				{
					int descsz=block_node->dsc_size;
					CHECKP(prev_blk);
					node_blk_hdr * block_prev= GETBLOCKBYNODE(prev_blk);
					if (block_prev->free_first!=0 && block_prev->dsc_size>=descsz)
						use_cs=2;
					else
						use_cs=3;			
				}
				else
					use_cs=3;
			}
			if (use_cs==7)
			{
				CHECKP(nodex);
				xptr prev_blk=block_node->pblk;
				if (prev_blk!=XNULL)
				{
					int descsz=block_node->dsc_size;
					CHECKP(prev_blk);
					node_blk_hdr * block_prev= GETBLOCKBYNODE(prev_blk);
					if (block_prev->free_first!=0 && block_prev->dsc_size>=descsz)
						use_cs=4;
					else
						use_cs=3;			
				}
				else
					use_cs=3;
			}
		}
		else
		{
			xptr next_blk=block_node->nblk;
			if (next_blk!=XNULL)
			{
				int descsz=block_node->dsc_size;
				CHECKP(next_blk);
				node_blk_hdr * block_nxt= GETBLOCKBYNODE(next_blk);
				if (block_nxt->free_first!=0 && block_nxt->dsc_size>=descsz)
				{
					CHECKP(nodex);
					use_cs=6;
				}
			}
			if (use_cs!=6)
			{
				CHECKP(nodex);
				xptr prev_blk=block_node->pblk;
				int descsz=block_node->dsc_size;
				if (prev_blk!=XNULL)
				{
					CHECKP(prev_blk);
					node_blk_hdr * block_prev= GETBLOCKBYNODE(prev_blk);
					if (block_prev->free_first!=0 && block_prev->dsc_size>=descsz)
						use_cs=2;
					else
						use_cs=5;			
				}
				else
					use_cs=5;
			}
		}
		
		switch (use_cs)
		{
		case 1:
			{
				
				CHECKP(nodex);
				shiftLastNodeToTheNextBlock(block_node);
				CHECKP(nodex);
				return 0;
			}
		case 2:
			{
				CHECKP(nodex);
				shiftFirstNodeToThePreviousBlock(block_node);
				CHECKP(nodex);
				return 0;
			}
		case 3:
			{
				CHECKP(nodex);
				createBlockNextToTheCurrentBlock(block_node);
				shiftLastNodeToTheNextBlock(block_node);
				CHECKP(nodex);
				return 0;
			}
		case 4:
			{
				CHECKP(nodex);
				//nodex=shiftFirstNodeToThePreviousBlock(block_node);
				return 1;
			}
		case 5:
			{
				CHECKP(nodex);
				createBlockNextToTheCurrentBlock(block_node);
				nodex=shiftLastNodeToTheNextBlock(block_node);
				//CHECKP(nodex);
				return 2;
			}
		case 6:
			{
				CHECKP(nodex);
				nodex=shiftLastNodeToTheNextBlock(block_node);
				//CHECKP(nodex);
				return 2;
			}
		}
		return 0;
	}
	else
	{
		return 0;
	}
	return 0;
}

int splitBlockIfFullAfterRightInsert(xptr& nodex)
{
	shft shift;
	n_dsc* tmp;
	xptr med;
	n_dsc* node=(n_dsc*)(XADDR(nodex));
	node_blk_hdr * block_node= GETBLOCKBYNODE(nodex);
	int use_cs=0;
	if ((shift=block_node->free_first)==0)
	{
		shft nd=CALCSHIFT(node,block_node);
		if (!(block_node->desc_last==nd))
		{
			if (!(block_node->desc_first==nd))
			{
				xptr next_blk=block_node->nblk;
				if (next_blk!=XNULL)
				{
					int descsz=block_node->dsc_size;
					CHECKP(next_blk);
					node_blk_hdr * block_nxt= GETBLOCKBYNODE(next_blk);
					if (block_nxt->free_first!=0 && block_nxt->dsc_size>=descsz)
						use_cs=1;
					else
						use_cs=6;	
				}
				else
					use_cs=6;				
			}
			else
			{
				xptr next_blk=block_node->nblk;
				if (next_blk!=XNULL)
				{
					int descsz=block_node->dsc_size;
					CHECKP(next_blk);
					node_blk_hdr * block_nxt= GETBLOCKBYNODE(next_blk);
					if (block_nxt->free_first!=0 && block_nxt->dsc_size>=descsz)
						use_cs=1;
					else
						use_cs=7;	
				}
				else
					use_cs=7;	
			}
			if (use_cs==6)
			{
				CHECKP(nodex);
				xptr prev_blk=block_node->pblk;
				if (prev_blk!=XNULL)
				{
					int descsz=block_node->dsc_size;
					CHECKP(prev_blk);
					node_blk_hdr * block_prev= GETBLOCKBYNODE(prev_blk);
					if (block_prev->free_first!=0 && block_prev->dsc_size>=descsz)
						use_cs=2;
					else
						use_cs=3;			
				}
				else
					use_cs=3;
			}
			if (use_cs==7)
			{
				CHECKP(nodex);
				xptr prev_blk=block_node->pblk;
				if (prev_blk!=XNULL)
				{
					int descsz=block_node->dsc_size;
					CHECKP(prev_blk);
					node_blk_hdr * block_prev= GETBLOCKBYNODE(prev_blk);
					if (block_prev->free_first!=0 && block_prev->dsc_size>=descsz)
						use_cs=4;
					else
						use_cs=3;			
				}
				else
					use_cs=3;
			}
		}
		else
		{
			xptr next_blk=block_node->nblk;
			if (next_blk!=XNULL)
				{
					int descsz=block_node->dsc_size;
					CHECKP(next_blk);
					node_blk_hdr * block_nxt= GETBLOCKBYNODE(next_blk);
					if (block_nxt->free_first!=0 && block_nxt->dsc_size>=descsz)
					{
						CHECKP(nodex);
						return 3;
					}
					else
					{
						CHECKP(nodex);
						xptr prev_blk=block_node->pblk;
						if (prev_blk!=XNULL)
						{
							CHECKP(prev_blk);
							node_blk_hdr * block_prev= GETBLOCKBYNODE(prev_blk);
							if (block_prev->free_first!=0 && block_prev->dsc_size>=descsz)
								use_cs=2;
							else
								use_cs=5;			
						}
						else
							use_cs=5;
					}


				}
			else
				use_cs=5;
		}
		switch (use_cs)
		{
		case 1:
			{
				
				CHECKP(nodex);
				shiftLastNodeToTheNextBlock(block_node);
				CHECKP(nodex);
				return 0;
			}
		case 2:
			{
				CHECKP(nodex);
				shiftFirstNodeToThePreviousBlock(block_node);
				CHECKP(nodex);
				return 0;
			}
		case 3:
			{
				CHECKP(nodex);
				createBlockNextToTheCurrentBlock(block_node);
				shiftLastNodeToTheNextBlock(block_node);
				CHECKP(nodex);
				return 0;
			}
		case 4:
			{
				CHECKP(nodex);
				nodex=shiftFirstNodeToThePreviousBlock(block_node);
				CHECKP(nodex);
				return 1;
			}
		case 5:
			{
				CHECKP(nodex);
				createBlockNextToTheCurrentBlock(block_node);
				CHECKP(nodex);
				return 2;
			}
		}
		return 0;
	}
	else
	{
		return 0;
	}
	return 0;
}

xptr splitBlockIfFullAndCheckWhereTheNodeIs(xptr nodex)
{
	shft shift;
	n_dsc* tmp;
	xptr med;
	n_dsc* node=(n_dsc*)(XADDR(nodex));
	node_blk_hdr * block_node= GETBLOCKBYNODE(nodex);
	bool mark;
	shft counter=0;
	if ((shift=block_node->free_first)==0)
	{
		if (block_node->desc_last==CALCSHIFT(node,block_node))
		{
			xptr new_block=createBlockNextToTheCurrentBlock(block_node);
			CHECKP(new_block);
			xptr new_pointer= new_block+ GETBLOCKFIRSTFREESPACE(new_block);
			int shift_size=GETDESCRIPTORSIZE(new_block);
			CHECKP(nodex);
			shiftNodeToTheNewBlock (node, new_pointer,shift_size,block_node); 
			counter++;
			CHECKP(nodex);
			VMM_SIGNAL_MODIFICATION(nodex);
			nodex= new_pointer;
			new_pointer+=shift_size;
			//PHYS LOG
			if (IS_DATA_BLOCK(nodex)) 
			{
				hl_phys_log_change(&block_node->free_first,sizeof(shft));
				hl_phys_log_change(&block_node->desc_last,sizeof(shft));
				hl_phys_log_change(&block_node->count,sizeof(shft));
				hl_phys_log_change(node,sizeof(shft));
			}

			makeBlockConsistentAfterCuttingTheEnd(block_node,node,counter);
			CHECKP(new_block) ;
			VMM_SIGNAL_MODIFICATION(new_block);
			makeNewBlockConsistentAfterFilling(new_block, new_pointer,shift_size);
		}
		else
		{	
			//PHYS LOG
			if (IS_DATA_BLOCK(nodex)) 
				hl_phys_log_change_blk(block_node);
			med=findMedianNodeDescriptor(block_node);
			tmp=(n_dsc*)(XADDR(med));
			mark=getNearestBorder(block_node,node)<0; 
			xptr new_block=createBlockNextToTheCurrentBlock(block_node);
			CHECKP(new_block);
			xptr new_pointer= new_block+ GETBLOCKFIRSTFREESPACE(new_block);
			int shift_size=GETDESCRIPTORSIZE(new_block);
			CHECKP(med);
			VMM_SIGNAL_MODIFICATION(med);
			while ( tmp->desc_next!=0)
			{        
				tmp=GETNEXTDESCRIPTOR_BL(block_node,tmp);
				if (mark && tmp==node) nodex= new_pointer;
				shiftNodeToTheNewBlock (tmp, new_pointer,shift_size,block_node); 
				counter++;
				CHECKP(med);
				new_pointer+=shift_size;
			}  
			makeBlockConsistentAfterCuttingTheEnd(block_node,GETNEXTDESCRIPTOR_BL(block_node,XADDR(med)),counter);
			CHECKP(new_block) ;
			VMM_SIGNAL_MODIFICATION(new_block);
			makeNewBlockConsistentAfterFilling(new_block, new_pointer,shift_size);
		}
	}
	return  nodex;
}

xptr insertBetween ( xptr left_sib, xptr right_sib, n_dsc* new_node)
{
	new_node->rdsc=right_sib;
	new_node->ldsc=left_sib;
	xptr nodex=ADDR2XPTR(new_node);
	if (right_sib!=XNULL)   
	{ 
		CHECKP(right_sib); 
		VMM_SIGNAL_MODIFICATION(right_sib);
		//PHYS LOG
		if (IS_DATA_BLOCK(right_sib)) 
			hl_phys_log_change(&((n_dsc*)XADDR(right_sib))->ldsc,sizeof(xptr));
		UPDATELEFTPOINTER(right_sib, nodex);
	}
	if (left_sib!=XNULL)  
	{ 
		CHECKP(left_sib);  
		VMM_SIGNAL_MODIFICATION(left_sib);
		//PHYS LOG
		if (IS_DATA_BLOCK(left_sib)) 
			hl_phys_log_change(&((n_dsc*)XADDR(left_sib))->rdsc,sizeof(xptr));
		UPDATERIGHTPOINTER(left_sib,nodex);
	}
	CHECKP(nodex);
    return nodex;
}

/* insert new element after the namesake element with the stated left-right siblings and parent*/
xptr addNewNodeOfSameSortAfter(xptr namesake, xptr left_sib,xptr right_sib, xptr parent, xptr par_indir, xmlscm_type type,t_item node_typ)
{
	xptr tmp;
	int res= splitBlockIfFullAfterRightInsert(namesake);
	xptr n_blk;
	n_dsc* oldnode= (n_dsc*) XADDR(namesake);
	switch (res)
	{
	case 0: n_blk=BLOCKXPTR(namesake);break;
	case 1: case 2:case 3: n_blk=((node_blk_hdr*)XADDR(BLOCKXPTR(namesake)))->nblk;CHECKP(n_blk);
	}
	node_blk_hdr* block_namesake=(node_blk_hdr*)XADDR(n_blk);
	
	n_dsc* new_node= GETBLOCKFIRSTFREESPACEABSOLUTE  (block_namesake);
	VMM_SIGNAL_MODIFICATION(namesake);
	//PHYS LOG
	if (IS_DATA_BLOCK(namesake)) 
		hl_phys_log_change(&block_namesake->free_first,sizeof(shft));
	block_namesake->free_first=GETPOINTERTONEXTFREESPACE(new_node);
	//PHYS LOG
	if (IS_DATA_BLOCK(namesake)) 
		hl_phys_log_change(new_node,block_namesake->dsc_size);
	switch (node_typ)
	{
	case(element):
		{
			e_dsc::init(new_node, type);
			clear_references(block_namesake,new_node);
			break; 
		}
	case(attribute):
		a_dsc::init(new_node, type);
		break;
	case(text):case(comment):case(cdata):
		t_dsc::init(new_node);
		break;
	case(xml_namespace):
		ns_dsc::init(new_node);
		break;
	case (pr_ins):
		pi_dsc::init(new_node);
		break;
	}
	new_node->pdsc=par_indir;
	switch(res)
	{
	case 0:
		{
			new_node->desc_next=oldnode->desc_next;
			new_node->desc_prev= CALCSHIFT(oldnode,block_namesake);
			//PHYS LOG
			if (IS_DATA_BLOCK(namesake))
				hl_phys_log_change(&((n_dsc*)XADDR(namesake))->desc_next,sizeof(shft));
			UPDATENEXTDESCRIPTOR(namesake, CALCSHIFT(new_node,block_namesake));
			if (new_node->desc_next!=0)
			{
				//PHYS LOG
				if (IS_DATA_BLOCK(namesake)) 
					hl_phys_log_change(&((n_dsc*)((char*)block_namesake+new_node->desc_next))->desc_prev,sizeof(shft));	
				((n_dsc*)((char*)block_namesake+new_node->desc_next))->desc_prev=CALCSHIFT(new_node,block_namesake);
			}
			else
			{
				//PHYS LOG
				if (IS_DATA_BLOCK(namesake)) 
					hl_phys_log_change(&block_namesake->desc_last,sizeof(shft));
				UPDATEPOINTERTOLASTDESCRIPTOR(block_namesake, CALCSHIFT(new_node,block_namesake));
			}
			break;
		}
	case 1: case 3:
		{
			new_node->desc_next=block_namesake->desc_first;
			new_node->desc_prev=0;
			n_dsc* tmn=GETPOINTERTODESC(block_namesake,block_namesake->desc_first);
			//assumption that block not empty
			if (IS_DATA_BLOCK(namesake))
			{
				hl_phys_log_change(&(tmn->desc_prev),sizeof(shft));
				hl_phys_log_change(&(block_namesake->desc_first),sizeof(shft));
			}
			tmn->desc_prev=CALCSHIFT(new_node,block_namesake);
			block_namesake->desc_first=tmn->desc_prev;
			break;
		}
		case 2:
		{
			new_node->desc_next=0;
			new_node->desc_prev=0;
			//assumption that block not empty
			if (IS_DATA_BLOCK(namesake))
			{
				hl_phys_log_change(&(block_namesake->desc_last),sizeof(shft));
				hl_phys_log_change(&(block_namesake->desc_first),sizeof(shft));
			}
			block_namesake->desc_last=CALCSHIFT(new_node,block_namesake);
			block_namesake->desc_first=block_namesake->desc_last;
			break;
		}
	}
	//PHYS LOG
	if (IS_DATA_BLOCK(namesake)) 
		hl_phys_log_change(&block_namesake->count,sizeof(shft));
	INCREMENTCOUNT(block_namesake);
	xptr nodex=ADDR2XPTR(new_node);
	tmp=add_record_to_indirection_table(nodex);
	CHECKP(n_blk);
	VMM_SIGNAL_MODIFICATION(namesake);
	new_node->indir=tmp;
	insertBetween ( left_sib, right_sib, new_node);
	//CHECKP(namesake);
	createNID( left_sib, right_sib, parent,nodex); 
	CHECKP(n_blk);
	return nodex;
}

/* insert new node before the namesake element with the stated left-right siblings and parent*/
xptr addNewNodeOfSameSortBefore(xptr namesake, xptr left_sib,xptr right_sib, xptr parent, xptr par_indir, xmlscm_type type,t_item node_typ)
{
	xptr tmp;
	int res= splitBlockIfFullAfterLeftInsert(namesake);
	xptr n_blk;
	n_dsc* oldnode= (n_dsc*) XADDR(namesake);
	switch (res)
	{
	case 0: n_blk=BLOCKXPTR(namesake);break;
	case 1: case 2:n_blk=((node_blk_hdr*)XADDR(BLOCKXPTR(namesake)))->pblk;CHECKP(n_blk);
	}
	
	node_blk_hdr* block_namesake=(node_blk_hdr*)XADDR(n_blk);
	n_dsc* new_node= GETBLOCKFIRSTFREESPACEABSOLUTE  (block_namesake);
	VMM_SIGNAL_MODIFICATION(namesake);
	
	//PHYS LOG
	if (IS_DATA_BLOCK(namesake)) 
		hl_phys_log_change(&block_namesake->free_first,sizeof(shft));
	block_namesake->free_first=GETPOINTERTONEXTFREESPACE(new_node);
	//PHYS LOG
	if (IS_DATA_BLOCK(namesake)) 
		hl_phys_log_change(new_node,block_namesake->dsc_size);
	switch (node_typ)
	{
	case(element):
		{
			e_dsc::init(new_node, type);
			clear_references(block_namesake,new_node);
			break; 
		}
	case(attribute):
		a_dsc::init(new_node, type);
		break;
	case(text):case(comment):case(cdata):
		t_dsc::init(new_node);
		break;
	case(xml_namespace):
		ns_dsc::init(new_node);
		break;
	case (pr_ins):
		pi_dsc::init(new_node);
		break;
	}
	new_node->pdsc=par_indir;
	switch(res)
	{
	case 0:
		{
			new_node->desc_prev=oldnode->desc_prev;
			new_node->desc_next= CALCSHIFT(oldnode,block_namesake);
			//PHYS LOG
			if (IS_DATA_BLOCK(namesake))
				hl_phys_log_change(&oldnode->desc_prev,sizeof(shft));
			UPDATEPREVIOUSDESCRIPTOR(namesake, CALCSHIFT(new_node,block_namesake));
			if (new_node->desc_prev!=0)
			{
				//PHYS LOG
				if (IS_DATA_BLOCK(namesake)) 
					hl_phys_log_change(&((n_dsc*)((char*)block_namesake+new_node->desc_prev))->desc_next,sizeof(shft));
				((n_dsc*)((char*)block_namesake+new_node->desc_prev))->desc_next=CALCSHIFT(new_node,block_namesake);
			}
			else
			{
				//PHYS LOG
				if (IS_DATA_BLOCK(namesake)) 
					hl_phys_log_change(&block_namesake->desc_first,sizeof(shft));
				block_namesake->desc_first=CALCSHIFT(new_node,block_namesake);
			}
			break;
		}
	case 1: case 2:
		{
			new_node->desc_next=0;
			new_node->desc_prev=block_namesake->desc_last;
			n_dsc* tmn=GETPOINTERTODESC(block_namesake,block_namesake->desc_last);
			//assumption that block not empty
			if (IS_DATA_BLOCK(namesake))
			{
				hl_phys_log_change(&(tmn->desc_next),sizeof(shft));
				hl_phys_log_change(&(block_namesake->desc_last),sizeof(shft));
			}
			tmn->desc_next=CALCSHIFT(new_node,block_namesake);
			block_namesake->desc_last=tmn->desc_next;
			break;
		}
	
	}
	//PHYS LOG
	if (IS_DATA_BLOCK(namesake)) 
		hl_phys_log_change(&block_namesake->count,sizeof(shft));
	INCREMENTCOUNT(block_namesake);
	xptr nodex=ADDR2XPTR(new_node);
	tmp=add_record_to_indirection_table(nodex);
	CHECKP(n_blk);
	VMM_SIGNAL_MODIFICATION(namesake);
	new_node->indir=tmp;
	insertBetween ( left_sib, right_sib, new_node);
	createNID( left_sib, right_sib, parent,nodex); 
	CHECKP(n_blk);
	return nodex;
}

/* creates first block with the descriptor fo the given schema node*/
xptr createNewBlock(schema_node* scm,bool persistent)
{
	xptr new_block;
	if (persistent)
	{
		vmm_alloc_data_block(&new_block);
		//PHYS LOG
		hl_phys_log_create_node_blk(XADDR(new_block));
	}
	else
		vmm_alloc_tmp_block(&new_block);
	int dscsz;
	VMM_SIGNAL_MODIFICATION(new_block);
	t_item type=GETTYPE(scm);
    switch (type)
	{
		case element:  dscsz=scm->get_child_count()*sizeof(xptr)+sizeof(e_dsc);
			break;
		case attribute: dscsz=sizeof(a_dsc);
			break;
		case text   :case comment: case cdata: dscsz=sizeof(t_dsc);
			break;
		case document: case virtual_root: dscsz=scm->get_child_count()*sizeof(xptr)+sizeof(d_dsc);
			break;
		case xml_namespace: dscsz=sizeof(ns_dsc);
			break;
		case (pr_ins): dscsz=sizeof(pi_dsc);
			break;
		default: dscsz=sizeof(n_dsc);
	}
	node_blk_hdr::init(XADDR(new_block),dscsz);
	node_blk_hdr * new_block_hdr=(node_blk_hdr *)(XADDR(new_block));
	new_block_hdr->snode=scm;
    UPDATEFIRSTBLOCKPOINTER(scm,new_block);
	scm->blockcnt++;
	return new_block;
}

/* inserts the first node descriptor of the current type*/
xptr addNewNodeFirstInRow(xptr newblock, xptr left_sib, xptr right_sib, xptr parent,
							 xptr par_indir ,  xmlscm_type type, t_item node_typ)
{
	node_blk_hdr * block_namesake=(node_blk_hdr*)XADDR(newblock);
	n_dsc* new_node= GETBLOCKFIRSTFREESPACEABSOLUTE  (block_namesake);
	block_namesake->free_first=GETPOINTERTONEXTFREESPACE(new_node);
	VMM_SIGNAL_MODIFICATION(newblock);
	switch (node_typ)
	{
	case(element):
		{
			e_dsc::init(new_node, type);
			clear_references(block_namesake,new_node);
			break; 
		}
	case(attribute):
		a_dsc::init(new_node, type);
		break;
	case(text):case(comment):case(cdata):
		t_dsc::init(new_node);
		break;
	case(xml_namespace):
		ns_dsc::init(new_node);
		break;
	case (pr_ins): 
		pi_dsc::init(new_node);
		break;
	}
	new_node->pdsc=par_indir;
	INCREMENTCOUNT(block_namesake);
	block_namesake->desc_first=CALCSHIFT(new_node,block_namesake);
	block_namesake->desc_last=CALCSHIFT(new_node,block_namesake);
	insertBetween ( left_sib, right_sib, new_node);
	xptr nodex=ADDR2XPTR(new_node);
	xptr tmp=add_record_to_indirection_table(nodex);
	CHECKP(newblock);
	VMM_SIGNAL_MODIFICATION(newblock);
	new_node->indir=tmp;
	createNID( left_sib, right_sib, parent,nodex); 
	CHECKP(newblock);
	return nodex;
}

/* creates new block of the same sort as the current one and expands the  node descriptor to the 
   maximum
*/
xptr createBlockNextToTheCurrentWithAdvancedDescriptor(node_blk_hdr* block)
{
	xptr old_blk=ADDR2XPTR(block);
	node_blk_hdr* tmp= se_new node_blk_hdr;
	*tmp=*block;
	tmp->dsc_size=tmp->snode->get_child_count()*sizeof(xptr)+size_of_node(tmp);
	xptr new_block;
	bool persistent= IS_DATA_BLOCK(old_blk);
	if (persistent)
	{
		vmm_alloc_data_block(&new_block);
		//PHYS LOG
		hl_phys_log_create_node_blk(XADDR(new_block));
	}
	else
		vmm_alloc_tmp_block(&new_block);
	VMM_SIGNAL_MODIFICATION(new_block);
	node_blk_hdr::init(XADDR(new_block),tmp->dsc_size);
	node_blk_hdr * new_block_hdr=(node_blk_hdr *)(XADDR(new_block));
	new_block_hdr->snode=tmp->snode;
    new_block_hdr->pblk=old_blk;   
    new_block_hdr->nblk=tmp->nblk;
	xptr tmp1=tmp->nblk;
	xptr tmp2=new_block_hdr->pblk;
	if (tmp1!=XNULL)
	{
		CHECKP(tmp1);
		VMM_SIGNAL_MODIFICATION(tmp1);
		//PHYS LOG REC
		node_blk_hdr * tmp_b=(node_blk_hdr *)(XADDR(tmp1));
		if (IS_DATA_BLOCK(tmp1))
			hl_phys_log_change(&(tmp_b->pblk),sizeof(xptr));
		tmp_b->pblk=new_block;
	}
    CHECKP(tmp2);
	if (IS_DATA_BLOCK(tmp2))
			hl_phys_log_change(&block->nblk,sizeof(xptr));
	VMM_SIGNAL_MODIFICATION(tmp2);
	block->nblk=new_block;
	block->snode->blockcnt++;
	CHECKP(new_block);
	delete tmp;
	return new_block;
	
}

/* creates new block of the same sort as the current one and expands the  node descriptor to the 
   maximum
*/
xptr createBlockPriorToTheCurrentWithAdvancedDescriptor(node_blk_hdr* block)
{
	xptr old_blk=ADDR2XPTR(block);
	node_blk_hdr* tmp= se_new node_blk_hdr;
	*tmp=*block;
	tmp->dsc_size=tmp->snode->get_child_count()*sizeof(xptr)+size_of_node(tmp);
	xptr new_block;
	bool persistent= IS_DATA_BLOCK(old_blk);
	if (persistent)
	{
	    //PHYS LOG
		//hl_phys_log_create_node_blk(XADDR(new_block));	
		vmm_alloc_data_block(&new_block);
		hl_phys_log_create_node_blk(XADDR(new_block));
	}
	else
		vmm_alloc_tmp_block(&new_block);
	VMM_SIGNAL_MODIFICATION(new_block);
	node_blk_hdr::init(XADDR(new_block),tmp->dsc_size);
	node_blk_hdr * new_block_hdr=(node_blk_hdr *)(XADDR(new_block));
	new_block_hdr->snode=tmp->snode;
    new_block_hdr->nblk=old_blk;   
    new_block_hdr->pblk=tmp->pblk;
	xptr tmp1=tmp->pblk;
	xptr tmp2=new_block_hdr->nblk;
	if (tmp1!=XNULL)
	{
		CHECKP(tmp1);
		VMM_SIGNAL_MODIFICATION(tmp1);
		//PHYS LOG REC
		node_blk_hdr * tmp_b=(node_blk_hdr *)(XADDR(tmp1));
		if (IS_DATA_BLOCK(tmp1))
			hl_phys_log_change(&tmp_b->nblk,sizeof(xptr));
		tmp_b->nblk=new_block;
	}
	else
		UPDATEFIRSTBLOCKPOINTER(tmp->snode,new_block);
	CHECKP(tmp2);
	VMM_SIGNAL_MODIFICATION(tmp2);
	//PHYS LOG REC
	if (IS_DATA_BLOCK(tmp2))
		hl_phys_log_change(&block->pblk,sizeof(xptr));
	block->pblk=new_block;
	CHECKP(new_block);
	new_block_hdr->snode->blockcnt++;
	delete tmp;
	return new_block;

}

/*splits the block into two parts and appends childs by scheme into descriptor of one of the parts*/
void addChildsBySchemeSplittingBlock(xptr parent, const char* name,t_item type, xptr child,xml_ns* ns)
{
	node_blk_hdr * parent_block;
	xptr new_block;
	n_dsc* tmp;
	int counter=1;
	parent_block= GETBLOCKBYNODE(parent);
	shft size=parent_block->dsc_size;
	VMM_SIGNAL_MODIFICATION(parent);
	if (parent_block->count==1 /*&& CALCSHIFT (XADDR(parent),parent_block)==sizeof(node_blk_hdr)*/)
	{
		if (IS_DATA_BLOCK(parent)) 
			hl_phys_log_change_blk(parent_block);
		n_dsc* par_dsc=(n_dsc*)XADDR(parent);
		char* ptr=((char*)parent_block)+sizeof(node_blk_hdr);
		if (ptr!=((char*)par_dsc))
		{
			
			//1. shift to the beggining		
		    //1.1 changing the pointers
			//1.1.1 indirection
			xptr new_pos=ADDR2XPTR(ptr);
			xptr tmp=par_dsc->indir;
			CHECKP(tmp);
			VMM_SIGNAL_MODIFICATION(tmp);
			if (IS_DATA_BLOCK(tmp))	hl_phys_log_change(XADDR(tmp),sizeof(xptr));
			*((xptr*)XADDR(tmp))=new_pos;
			//1.1.2 left
			CHECKP(parent);
			tmp=par_dsc->ldsc;
			if (tmp!=XNULL)
			{
				CHECKP(tmp);
				VMM_SIGNAL_MODIFICATION(tmp);
				if (IS_DATA_BLOCK(tmp))	hl_phys_log_change(&(((n_dsc*)XADDR(tmp))->rdsc),sizeof(xptr));
				((n_dsc*)XADDR(tmp))->rdsc=new_pos;
			}
			//1.1.3 right
			CHECKP(parent);
			tmp=par_dsc->rdsc;
			if (tmp!=XNULL)
			{
				CHECKP(tmp);
				VMM_SIGNAL_MODIFICATION(tmp);
				if (IS_DATA_BLOCK(tmp))	hl_phys_log_change(&(((n_dsc*)XADDR(tmp))->ldsc),sizeof(xptr));
				((n_dsc*)XADDR(tmp))->ldsc=new_pos;
			}
			//1.1.4 parent
			CHECKP(parent);
			tmp=par_dsc->pdsc;
			if (tmp!=XNULL)
			{
				n_dsc* node_d=getPreviousDescriptorOfSameSort (par_dsc);
				if (node_d==NULL || node_d->pdsc!=tmp)
				{
					xptr grandpa=removeIndirection(tmp);
					CHECKP(grandpa);
					VMM_SIGNAL_MODIFICATION(grandpa);
					updateChildPointer ((n_dsc*)XADDR(grandpa),parent,new_pos);
				}
			}
			else
			{
				metadata_sem_down();
				/*dn_metadata_cell * mdc=((col_schema_node*)parent_block->snode)->find_metadata_of_document_in_col(parent);
				mdc->root=new_pos;*/
				((col_schema_node*)parent_block->snode)->replace_document_pointer(parent,new_pos);
				metadata_sem_up();							
			}
			CHECKP(parent);
			VMM_SIGNAL_MODIFICATION(parent);			
			memmove (ptr,(char*)par_dsc,parent_block->dsc_size);
			//1.1.5 header pointers
			parent_block->desc_first=sizeof(node_blk_hdr);
			parent_block->desc_last=sizeof(node_blk_hdr);
		}
		else
			VMM_SIGNAL_MODIFICATION(parent);
		//zero filling
		ptr=((char*)parent_block)+sizeof(node_blk_hdr)+size;
		memset(ptr,0,PAGE_SIZE-sizeof(node_blk_hdr)-size);
		/*while (ptr!=NULL)
		{
		char* ptrt=(*((shft *)ptr)==0)?NULL:((char*)parent_block)+(*((shft *)ptr));
		*((shft *)ptr)=0;
		ptr=ptrt;
		}*/
		parent_block->dsc_size=size_of_node(parent_block)+sizeof(xptr)*parent_block->snode->get_child_count();
		int i;
		parent_block->free_first=sizeof(node_blk_hdr)+parent_block->dsc_size;
		ptr=((char*)parent_block)+parent_block->free_first;
		for (i = parent_block->free_first;
			i < (int)PAGE_SIZE - parent_block->dsc_size;
			i += parent_block->dsc_size)

		{
			*((shft *)((char*)parent_block + i)) = (shft)i + parent_block->dsc_size;
		}
		*((shft *)((char*)parent_block+(i-parent_block->dsc_size)))=0;
		*elementContainsChild((n_dsc*)(((char*)parent_block)+sizeof(node_blk_hdr)),name,type,ns)=child;
	}
	else
	{
		//PHYS LOG
		if (IS_DATA_BLOCK(parent)) 
			hl_phys_log_change_blk(parent_block);
		int mark=0;
		if (GETTYPE(parent_block->snode)==document) mark=1;
		if (getNearestBorder(parent_block,(n_dsc*)XADDR(parent))>0)
		{
			//1. shifting nodes to the previous block
			CHECKP(parent);
			shft par_next=((n_dsc*)XADDR(parent))->desc_prev;
			xptr nblk=parent_block->nblk;
			if (nblk!=XNULL)
			{
				int csz=parent_block->dsc_size;
				CHECKP(nblk);
				node_blk_hdr * nblk_hdr=GETBLOCKBYNODE(nblk);
				if (nblk_hdr->dsc_size>=size && nblk_hdr->free_first!=0)
				{
					int shift_size=nblk_hdr->dsc_size;
					CHECKP(parent);
					tmp= GETPOINTERTODESC(parent_block,parent_block->desc_last);  
					xptr answ;
					while (tmp->desc_prev!=par_next)
					{
						answ=shiftLastNodeToTheNextBlock(parent_block);
						if(mark)
						{
							metadata_sem_down();
							CHECKP(parent);
							((col_schema_node*)parent_block->snode)->replace_document_pointer(ADDR2XPTR(tmp),answ);
							/*dn_metadata_cell * mdc=((col_schema_node*)parent_block->snode)->find_metadata_of_document_in_col(ADDR2XPTR(tmp));
							mdc->root=answ;*/
							metadata_sem_up();
							CHECKP(answ);
						}
						if (nblk_hdr->free_first==0)
						{
							goto l_second;
						}
						CHECKP(parent);
						tmp=GETPREVIOUSDESCRIPTOR_BL(parent_block,tmp);
					}
					int posit=parent_block->snode->has_child_by_schema(ns,name,type);
					if (shift_size>=((shft)size_of_node(parent_block)+((shft)posit+1)*((shft)sizeof(xptr))))
					{
						answ=shiftLastNodeToTheNextBlock(parent_block);
						if(mark)
						{
							metadata_sem_down();
							CHECKP(parent);
							((col_schema_node*)parent_block->snode)->replace_document_pointer(ADDR2XPTR(tmp),answ);
							/*dn_metadata_cell * mdc=((col_schema_node*)parent_block->snode)->find_metadata_of_document_in_col(ADDR2XPTR(tmp));
							mdc->root=answ;*/
							metadata_sem_up();
							CHECKP(answ);
						}
						VMM_SIGNAL_MODIFICATION(answ);
						*elementContainsChild((n_dsc*)XADDR(answ),name,type,ns)=child;
						return;
					}
				}
			}

l_second:
			CHECKP(parent);
			new_block=createBlockNextToTheCurrentWithAdvancedDescriptor(parent_block);
			tmp=(n_dsc*)XADDR(parent);
			xptr new_pointer=new_block+ GETBLOCKFIRSTFREESPACE(new_block);
			shft shift_size=GETDESCRIPTORSIZE(new_block);
			CHECKP(parent);

			if(mark)
			{
				metadata_sem_down();
				((col_schema_node*)parent_block->snode)->replace_document_pointer(ADDR2XPTR(tmp),new_pointer);
				/*dn_metadata_cell * mdc=((col_schema_node*)parent_block->snode)->find_metadata_of_document_in_col(ADDR2XPTR(tmp));
				mdc->root=new_pointer;*/
				CHECKP(parent);
				metadata_sem_up();
			}
			shiftNodeToTheNewBlockExpanded (tmp,new_pointer,shift_size,size,parent_block);
			VMM_SIGNAL_MODIFICATION(new_pointer);
			*elementContainsChild((n_dsc*)XADDR(new_pointer),name,type,ns)=child;
			new_pointer+=shift_size;
			CHECKP(parent);
			while ( tmp->desc_next!=0)
			{        
				tmp=GETNEXTDESCRIPTOR_BL(parent_block,tmp);
				if (CALCSHIFT(XADDR(new_pointer),XADDR(new_block))>PAGE_SIZE)
				{
					CHECKP(new_block);
					VMM_SIGNAL_MODIFICATION(new_block);
					makeNewBlockConsistentAfterFilling(new_block, new_pointer,shift_size);
					new_block=createBlockNextToTheCurrentBlock((node_blk_hdr*)XADDR(new_block));
					CHECKP(new_block);
					new_pointer=new_block+  GETBLOCKFIRSTFREESPACE(new_block);
					CHECKP(parent);
				}
				if(mark)
				{
					metadata_sem_down();
					((col_schema_node*)parent_block->snode)->replace_document_pointer(ADDR2XPTR(tmp),new_pointer);
					/*dn_metadata_cell * mdc=((col_schema_node*)parent_block->snode)->find_metadata_of_document_in_col(ADDR2XPTR(tmp));
					mdc->root=new_pointer;*/
					metadata_sem_up();
				}
				shiftNodeToTheNewBlockExpanded (tmp, new_pointer,shift_size,size,parent_block);
				counter++; 
				new_pointer+=shift_size;
				CHECKP(parent);
			} 
			VMM_SIGNAL_MODIFICATION(parent);
			makeBlockConsistentAfterCuttingTheEnd(parent_block,
				(n_dsc*)XADDR(parent),counter);
			CHECKP(new_block);
			VMM_SIGNAL_MODIFICATION(new_block);
			makeNewBlockConsistentAfterFilling(new_block, new_pointer,shift_size);
		}
		else
		{
			//1. shifting nodes to the previous block
			CHECKP(parent);
			shft par_next=((n_dsc*)XADDR(parent))->desc_next;
			xptr pblk=parent_block->pblk;
			if (pblk!=XNULL)
			{
				//n_dsc* par_pr=GETNEXTDESCRIPTOR_BL(parent_block,((n_dsc*)XADDR(parent)));
				int csz=parent_block->dsc_size;
				CHECKP(pblk);
				node_blk_hdr * pblk_hdr=GETBLOCKBYNODE(pblk);
				if (pblk_hdr->dsc_size>=size && pblk_hdr->free_first!=0)
				{
					int shift_size=pblk_hdr->dsc_size;
					CHECKP(parent);
					tmp= GETPOINTERTODESC(parent_block,parent_block->desc_first);  
					xptr answ;
					while (tmp->desc_next!=par_next)
					{
						answ=shiftFirstNodeToThePreviousBlock(parent_block);
						if(mark)
						{
							metadata_sem_down();
							CHECKP(parent);
							((col_schema_node*)parent_block->snode)->replace_document_pointer(ADDR2XPTR(tmp),answ);
							/*dn_metadata_cell * mdc=((col_schema_node*)parent_block->snode)->find_metadata_of_document_in_col(ADDR2XPTR(tmp));
							mdc->root=answ;*/
							metadata_sem_up();
							CHECKP(answ);
						}
						if (pblk_hdr->free_first==0)
						{
							goto second;
						}
						CHECKP(parent);
						tmp=GETNEXTDESCRIPTOR_BL(parent_block,tmp);
					}
					int posit=parent_block->snode->has_child_by_schema(ns,name,type);
					if (shift_size>=((shft)size_of_node(parent_block)+((shft)posit+1)*((shft)sizeof(xptr))))
					{
						answ=shiftFirstNodeToThePreviousBlock(parent_block);
						if(mark)
						{
							metadata_sem_down();
							CHECKP(parent);
							((col_schema_node*)parent_block->snode)->replace_document_pointer(ADDR2XPTR(tmp),answ);
							/*dn_metadata_cell * mdc=((col_schema_node*)parent_block->snode)->find_metadata_of_document_in_col(ADDR2XPTR(tmp));
							mdc->root=answ;*/
							metadata_sem_up();
							CHECKP(answ);
						}
						VMM_SIGNAL_MODIFICATION(answ);
						*elementContainsChild((n_dsc*)XADDR(answ),name,type,ns)=child;
						return;
					}
				}
			}

second:
			//2. do the rest job
			CHECKP(parent);
			//shft par_next=((n_dsc*)XADDR(parent))->desc_next;
			tmp= GETPOINTERTODESC(parent_block,parent_block->desc_first);  
			new_block=createBlockPriorToTheCurrentWithAdvancedDescriptor(parent_block);
			xptr new_pointer= new_block+GETBLOCKFIRSTFREESPACE(new_block);
			int shift_size=GETDESCRIPTORSIZE(new_block);
			CHECKP(parent);
			while (tmp->desc_next!=par_next)
			{        
				if(mark)
				{
					metadata_sem_down();
					((col_schema_node*)parent_block->snode)->replace_document_pointer(ADDR2XPTR(tmp),new_pointer);
					/*dn_metadata_cell * mdc=((col_schema_node*)parent_block->snode)->find_metadata_of_document_in_col(ADDR2XPTR(tmp));
					mdc->root=new_pointer;*/
					metadata_sem_up();
				}
				shiftNodeToTheNewBlockExpanded (tmp, new_pointer,shift_size,size,parent_block);
				counter++;
				new_pointer+=shift_size;
				if (CALCSHIFT(XADDR(new_pointer),XADDR(new_block))>PAGE_SIZE) 
				{
					CHECKP(new_block);
					VMM_SIGNAL_MODIFICATION(new_block);
					makeNewBlockConsistentAfterFilling(new_block, new_pointer,shift_size);
					new_block=createBlockNextToTheCurrentBlock((node_blk_hdr*)XADDR(new_block));
					CHECKP(new_block);
					new_pointer=new_block+  GETBLOCKFIRSTFREESPACE(new_block);
				}
				CHECKP(parent);  
				tmp=GETNEXTDESCRIPTOR_BL(parent_block,tmp);
			}
			if(mark)
			{
				metadata_sem_down();
				((col_schema_node*)parent_block->snode)->replace_document_pointer(ADDR2XPTR(tmp),new_pointer);
				/*dn_metadata_cell * mdc=((col_schema_node*)parent_block->snode)->find_metadata_of_document_in_col(ADDR2XPTR(tmp));
				mdc->root=new_pointer;*/
				metadata_sem_up();
			}
			shiftNodeToTheNewBlockExpanded (tmp, new_pointer,shift_size,size,parent_block);
			VMM_SIGNAL_MODIFICATION(new_pointer);
			*elementContainsChild((n_dsc*)XADDR(new_pointer),name,type,ns)=child;    
			makeNewBlockConsistentAfterFilling(new_block, new_pointer+shift_size,shift_size);
			CHECKP(parent); 
			VMM_SIGNAL_MODIFICATION(parent);
			makeBlockConsistentAfterCuttingTheBeginning(parent_block,(n_dsc*)XADDR(parent), counter);
		}
	}      		 
}

/*node deletion*/
void delete_inner_nodes(n_dsc* node)
{
	xptr tmp,tmp2;
	n_dsc* tmp_n;
	int chcnt;
	xptr* childp;
	node_blk_hdr* block=GETBLOCKBYNODE_ADDR(node);
	//NODE STATISTICS
	block->snode->nodecnt--;
	node_blk_hdr* bl_tmp;
	t_item type=GETTYPE(block->snode);
	xptr node_x=ADDR2XPTR(node);
	xptr par_indir=node->indir;
	del_record_from_indirection_table(par_indir);
	nid_delete(node_x);
	CHECKP(node_x);
	switch(type)
	{
	case attribute: case text:case comment:case cdata:case pr_ins: 
		{
			deleteTextValue(node_x);
			return;
		}
	case document:
		{
			deleteTextValue(node_x);
			chcnt=COUNTREFERENCES(block,sizeof(d_dsc));
			childp=(xptr*)((char*)node+sizeof(d_dsc));
			break;
		}
	case element:
		{
			chcnt=COUNTREFERENCES(block,sizeof(e_dsc)); 
			childp=(xptr*)((char*)node+sizeof(e_dsc));
		}
	}
	for (int i=0; i<chcnt;i++)
	{
		xptr child=*childp;
		childp+=1;
		if (child!=XNULL)
		{ 
			CHECKP(child);
			xptr block= ADDR2XPTR(GETBLOCKBYNODE(child));
			n_dsc* node_d=(n_dsc*)XADDR(child);
			n_dsc* left_desc = getPreviousDescriptorOfSameSort(node_d);
			xptr left_block=(left_desc!=NULL)?ADDR2XPTR(GETBLOCKBYNODE_ADDR(left_desc)):XNULL;
			CHECKP(child);
			while (node_d->pdsc==par_indir)
			{
				bl_tmp=GETBLOCKBYNODE_ADDR(node_d);
				delete_inner_nodes(node_d);
				VMM_SIGNAL_MODIFICATION(child);
				//PHYS LOG
				if (IS_DATA_BLOCK(child)) 
				{
					hl_phys_log_change(&bl_tmp->count,sizeof(shft));
					hl_phys_log_change(node_d,sizeof(shft));
					hl_phys_log_change(&bl_tmp->free_first,sizeof(shft));
				}
				bl_tmp->count-=1;
				tmp_n= getNextDescriptorOfSameSort(node_d);            
				if (tmp_n!=NULL) tmp=ADDR2XPTR(tmp_n);
				else tmp=XNULL;
				CHECKP(child); 
				VMM_SIGNAL_MODIFICATION(child);
				*((shft*)node_d)=bl_tmp->free_first;
				bl_tmp->free_first=CALCSHIFT(node_d,bl_tmp);
				node_d=tmp_n;
				child=tmp;
				if (tmp==XNULL) break;
				CHECKP(child); 
			}
			xptr right_block=(child!=XNULL)? ADDR2XPTR(GETBLOCKBYNODE(child)):XNULL;
			if (right_block==XNULL && left_block==XNULL)
			{
				tmp= block;
				while (tmp!=XNULL) 
                { 
					CHECKP(tmp); 
					tmp2=((node_blk_hdr*)XADDR(tmp))->nblk;
					deleteBlock((node_blk_hdr*)XADDR(tmp));
					tmp=tmp2;
				}
				continue;
			}
			if (left_block!=XNULL )
			{
				tmp=left_block;
				CHECKP(tmp);
				tmp= ((node_blk_hdr*)XADDR(tmp))->nblk;
				while (tmp!= right_block) 
				{ 
					CHECKP(tmp) ;
					tmp2= ((node_blk_hdr*)XADDR(tmp))->nblk;
					deleteBlock((node_blk_hdr*)XADDR(tmp));
					tmp=tmp2;
				}  
			}
			else
			{
				tmp=right_block;
				CHECKP(tmp);
				tmp= ((node_blk_hdr*)XADDR(tmp))->pblk;
				while (tmp!= XNULL) 
				{ 
					CHECKP(tmp) ;
					tmp2= ((node_blk_hdr*)XADDR(tmp))->pblk;
					deleteBlock((node_blk_hdr*)XADDR(tmp));
					tmp=tmp2;
				}  
			}
			if (left_block==right_block)
			{
				CHECKP(left_block);
				VMM_SIGNAL_MODIFICATION(left_block);
				//PHYS LOG
				if (IS_DATA_BLOCK(left_block)) 
				{
					hl_phys_log_change(&left_desc->desc_next,sizeof(shft));
					hl_phys_log_change(&node_d->desc_prev,sizeof(shft));
				}
				left_desc->desc_next=CALCSHIFT(node_d,XADDR(left_block));
				node_d->desc_prev=CALCSHIFT(left_desc,XADDR(left_block));
			}
			else
			{
				if (left_block!=XNULL)
				{
					CHECKP(left_block);
					VMM_SIGNAL_MODIFICATION(left_block);
					//PHYS LOG
					if (IS_DATA_BLOCK(left_block)) 
					{
						hl_phys_log_change(&((node_blk_hdr*)XADDR(left_block))->desc_last,sizeof(shft));
						hl_phys_log_change(&left_desc->desc_next,sizeof(shft));
					}
					((node_blk_hdr*)XADDR(left_block))->desc_last=CALCSHIFT(left_desc,XADDR(left_block));
					left_desc->desc_next=0;
				}
				if (right_block!=XNULL)
				{
					CHECKP(right_block);
					VMM_SIGNAL_MODIFICATION(right_block);
					//PHYS LOG
					if (IS_DATA_BLOCK(left_block)) 
					{
						hl_phys_log_change(&((node_blk_hdr*)XADDR(right_block))->desc_first,sizeof(shft));
						hl_phys_log_change(&node_d->desc_prev,sizeof(shft));
					}
					((node_blk_hdr*)XADDR(right_block))->desc_first=CALCSHIFT(node_d,XADDR(right_block));
					node_d->desc_prev=0;
				}
			}
		}
		CHECKP(node_x) ;
	}
}

void copy_to_buffer(char * buf,const void* src, int size, text_type ttype)
{
	if (ttype==text_mem)
	{
		memcpy(buf,src,size);
		return;
	}	
	else if (ttype==text_doc)
	{
		xptr ptr=*(xptr*)src;
		CHECKP(ptr);
		memcpy(buf,(char*)XADDR(ptr),size);
		return;
	}
	else
	{ //ttype == text_estr
		estr_copy_to_buffer(buf, *(xptr*)src, size);
		return;
	}
}
/*inserts text value into the database*/
void addTextValue(xptr node,const void* text, unsigned int size,text_type ttype)
{
	t_dsc* test_desc= (t_dsc*)XADDR(node);
	//TEMP
/*	test_desc->data=XNULL;
	test_desc->size=0;
	return;
*/	//REMOVE
	//PHYS LOG
	if (size>STRMAXSIZE)
		throw USER_EXCEPTION(SE2037);

	if (size<1)
	{
		if (IS_DATA_BLOCK(node)) 
		{
			hl_phys_log_change(&test_desc->data,sizeof(xptr));
			hl_phys_log_change(&test_desc->size,sizeof(int));
		}
		VMM_SIGNAL_MODIFICATION(node);
		test_desc->data=XNULL;
		test_desc->size=0;
		return;
	}
	//test_desc->size=size;
	if (size<=PSTRMAXSIZE)
	{
		xptr blk=XNULL;
		t_dsc* neighb=(t_dsc*)getPreviousDescriptorOfSameSort(test_desc);
		while (neighb!=NULL && (neighb->data==XNULL ||neighb->size>PSTRMAXSIZE) ) neighb=(t_dsc*)getPreviousDescriptorOfSameSort(neighb);
		if (neighb!=NULL)
		{
			blk=BLOCKXPTR(neighb->data);
		}
		else
		{
			CHECKP(node);
			neighb=(t_dsc*)getNextDescriptorOfSameSort(test_desc);
			while (neighb!=NULL && (neighb->data==XNULL ||neighb->size>PSTRMAXSIZE)) neighb=(t_dsc*)getNextDescriptorOfSameSort(neighb);
			if (neighb!=NULL)
			{
				blk=BLOCKXPTR(neighb->data);
			}
			else	
			{
				blk=pstr_create_blk(IS_DATA_BLOCK(node));//NEED PERSISTENT VERSION		
			}
		}
		
		if (ttype==text_mem)
			pstr_allocate(blk,node,(const char*) text, size);
		else
		{
			char* buf=se_new char[size];
			copy_to_buffer(buf,text,size,ttype);
			pstr_allocate(blk,node,buf, size);
			delete [] buf;

		}
	}
	else
	{
		pstr_long_create_str(node, text,  size,ttype);
	}
	/*if (IS_DATA_BLOCK(node))
	{
		update_idx_add_txt(node,text,  size);
	}*/
}


/*appends currently existing text value */
void appendTextValue(xptr node,const void* text, unsigned int size,text_type ttype)
{
	   
	t_dsc* test_desc= (t_dsc*)XADDR(node);
	unsigned int cur_size=test_desc->size;
	if (((__int64)size+(__int64)cur_size)>STRMAXSIZE)
		throw USER_EXCEPTION(SE2037);
	//test_desc->size+=size;
	if (IS_DATA_BLOCK(node))
	{
		update_idx_delete_text(node);
	}
	if (cur_size>PSTRMAXSIZE)
	{
		pstr_long_append_tail(node, text,size,ttype);
	}
	else
	{
		xptr ind_ptr=test_desc->data;
		CHECKP(ind_ptr);
		shft shift= *((shft*)XADDR(ind_ptr));
		char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
		if (cur_size+size>PSTRMAXSIZE)
		{
			char* z=se_new char[cur_size];
			memcpy(z,data,cur_size);
            pstr_deallocate(node);
			pstr_long_create_str(node, z,  cur_size,text_mem);
			delete []z;
			pstr_long_append_tail(node, text, size,ttype);
		}
		else
		{
			char* z=se_new char[size+cur_size];
			memcpy(z,data,cur_size);
			copy_to_buffer(z+cur_size,text,size,ttype);
			//d_printf1("bm");fflush(stdout);
			//Index update
			pstr_modify(node, z, size+cur_size);
			//d_printf1("am");fflush(stdout);
			delete []z;

		}
	}
	if (IS_DATA_BLOCK(node))
	{
		update_idx_add_txt(node);
	}
}

/*appends currently existing text value. New  text is inserted from the first position */
void insertTextValue(xptr node,const void* text, unsigned int size,text_type ttype)
{
	t_dsc* test_desc= (t_dsc*)XADDR(node);
	unsigned int cur_size=test_desc->size;
	if (((__int64)size+(__int64)cur_size)>STRMAXSIZE)
		throw USER_EXCEPTION(SE2037);
	//test_desc->size+=size;
	if (IS_DATA_BLOCK(node))
	{
		update_idx_delete_text(node);
	}
	if (cur_size>PSTRMAXSIZE)
	{
		pstr_long_append_head(node, text, size,ttype);
	}
	else
	{
		xptr ind_ptr=test_desc->data;
		CHECKP(ind_ptr);
		shft shift= *((shft*)XADDR(ind_ptr));
		char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
		if ((cur_size+size)>PSTRMAXSIZE)
		{
			
			char* z=se_new char[cur_size];
			memcpy(z,data,cur_size);
            pstr_deallocate(node);
			pstr_long_create_str(node, text,  size,ttype);
			pstr_long_append_tail(node, z, cur_size,text_mem);
			delete []z;
		}
		else
		{			
			char* z=se_new char[size+cur_size];
			copy_to_buffer(z,text,size,ttype);
			memcpy(z+size,data,cur_size);
			pstr_modify(node, z, size+cur_size);
			delete []z;
		}
	}
	if (IS_DATA_BLOCK(node))
	{
		update_idx_add_txt(node);
	}

	
}
/*removes the head of currently existing text value. */
void delete_text_head(xptr node, int size)
{
	CHECKP(node);
    t_dsc* test_desc= (t_dsc*)XADDR(node);
	int cur_size=test_desc->size;
	if (cur_size<=size)
		throw SYSTEM_EXCEPTION("wrong recovery of text node");
	//test_desc->size+=size;
	if (IS_DATA_BLOCK(node))
	{
		update_idx_delete_text(node);
	}
	if (cur_size<=PSTRMAXSIZE)
	{
		    xptr ind_ptr=test_desc->data;
			CHECKP(ind_ptr);
			shft shift= *((shft*)XADDR(ind_ptr));
			char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
		    char* z=se_new char[cur_size-size];
			memcpy(z,data+size,cur_size-size);
			pstr_modify(node, z, cur_size-size);
			delete []z;
	}
	else
	{
		pstr_long_delete_head(node,size);
		if (cur_size-size<=PSTRMAXSIZE)
		{
			char* z= se_new char[cur_size-size];
			pstr_long_copy_to_buffer(z, node);
			pstr_long_delete_str(node);
			addTextValue(node,z, cur_size-size);
			delete [] z;

		}
		
	}
	if (IS_DATA_BLOCK(node))
	{
		update_idx_add_txt(node);
	}

	
}
/*removes the tail of currently existing text value. */
void delete_text_tail(xptr node, int size)
{
    CHECKP(node);
	t_dsc* test_desc= (t_dsc*)XADDR(node);
	int cur_size=test_desc->size;
	if (cur_size<=size)
		throw SYSTEM_EXCEPTION("wrong recovery of text node");
	//test_desc->size+=size;
	if (IS_DATA_BLOCK(node))
	{
		update_idx_delete_text(node);
	}
	if (cur_size<=PSTRMAXSIZE)
	{
		    xptr ind_ptr=test_desc->data;
			CHECKP(ind_ptr);
			shft shift= *((shft*)XADDR(ind_ptr));
			char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
		    char* z=se_new char[cur_size-size];
			memcpy(z,data,cur_size-size);
			pstr_modify(node, z, cur_size-size);
			delete []z;
	}
	else
	{
		pstr_long_truncate(node,size);
		if (cur_size-size<=PSTRMAXSIZE)
		{
			char* z= se_new char[cur_size-size];
			pstr_long_copy_to_buffer(z, node);
			pstr_long_delete_str(node);
			addTextValue(node,z, cur_size-size);
			delete [] z;

		}
		
	}
	if (IS_DATA_BLOCK(node))
	{
		update_idx_add_txt(node);
	}

	
}
/*deletes text value from the database*/
void deleteTextValue(xptr node)
{
	(GETBLOCKBYNODE(node))->snode->textcnt-=(((t_dsc*)XADDR(node))->size);
	if (((t_dsc*)XADDR(node))->size>PSTRMAXSIZE)
	{
		pstr_long_delete_str(node);
	}
	else
	{
		pstr_deallocate(node);
	}
}
void update_idx_add (xptr node)
{
	schema_node* scm=(GETBLOCKBYNODE(node))->snode;
	schema_index* ind=scm->index_object;
	if (ind==NULL) return;
	while (ind!=NULL)
	{
		ind->index->put_to_index(node,ind->object);
		ind=ind->next;
	}
	CHECKP(node);
}
void update_idx_add (xptr node,const char* value, int size)
{
	schema_node* scm=(GETBLOCKBYNODE(node))->snode;
	schema_index* ind=scm->index_object;
	if (ind==NULL) return;
	while (ind!=NULL)
	{
		ind->index->put_to_index(node,value,size,ind->object);
		ind=ind->next;
	}
	CHECKP(node);

}
void update_idx_add_txt (xptr node)
{
	CHECKP(node);
	schema_node* scm=(GETBLOCKBYNODE(node))->snode;
	schema_index* ind=scm->index_object;
	if (ind==NULL&&scm->parent->index_object==NULL) return;
	while (ind!=NULL)
	{
		ind->index->put_to_index(node,ind->object);
		ind=ind->next;
	}
	// parent
	scm=(GETBLOCKBYNODE(node))->snode->parent;
	ind=scm->index_object;
	CHECKP(node);
	if (ind==NULL) return;
	xptr pnode=removeIndirection(((n_dsc*)XADDR(node))->pdsc);
	while (ind!=NULL)
	{
		ind->index->put_to_index(pnode,ind->object);
		ind=ind->next;
	}
	CHECKP(node);
}
void update_idx_add_txt (xptr node,const char* value, int size)
{
	schema_node* scm=(GETBLOCKBYNODE(node))->snode;
	schema_index* ind=scm->index_object;
	if (ind==NULL&&scm->parent->index_object==NULL) return;
	while (ind!=NULL)
	{
		ind->index->put_to_index(node,value,size,ind->object);
		ind=ind->next;
	}
	// parent
	scm=(GETBLOCKBYNODE(node))->snode->parent;
	ind=scm->index_object;
	CHECKP(node);
	if (ind==NULL) return;
	xptr pnode=removeIndirection(((n_dsc*)XADDR(node))->pdsc);
	while (ind!=NULL)
	{
		ind->index->put_to_index(pnode,value,size,ind->object);
		ind=ind->next;
	}
	CHECKP(node);
}
void update_idx_delete (xptr node)
{
	schema_node* scm=(GETBLOCKBYNODE(node))->snode;
	schema_index* ind=scm->index_object;
	while (ind!=NULL)
	{
		ind->index->delete_from_index(node,ind->object);
		ind=ind->next;
	}
}
void update_idx_delete_text (xptr node,const char* value, int size)
{
	//CHECKP(node);
	update_idx_delete_text ((GETBLOCKBYNODE(node))->snode, node,value,size);
	CHECKP(node);
}
void update_idx_delete_text (xptr node)
{
	//CHECKP(node);
	int sz=((t_dsc*)XADDR(node))->size;
	xptr data=((t_dsc*)XADDR(node))->data;
	schema_node* sn=(GETBLOCKBYNODE(node))->snode;
	if (sz<=PSTRMAXSIZE)
	{
		CHECKP(data);
		shft shift= *((shft*)XADDR(data));
		char* val=(char*)XADDR(BLOCKXPTR(data))+shift;
		update_idx_delete_text (sn, node,val,sz);
		CHECKP(node);
	}
	else
	{
		//TODO
	}
}
void update_idx_delete_text (schema_node* scm,xptr node,const char* value, int size)
{
	schema_index* ind=scm->index_object;
	if (ind==NULL&&scm->parent->index_object==NULL) return;
	char* z=se_new char[size];
	memcpy(z,value,size);
	while (ind!=NULL)
	{
		ind->index->delete_from_index(node,z,size,ind->object);
		ind=ind->next;
	}
	// parent
	if (scm->type!=text)
	{
		delete [] z;
		return;
	}
	scm=scm->parent;
	ind=scm->index_object;
	if (ind==NULL)
	{
		delete [] z;
		return;
	}
	CHECKP(node);
	xptr pnode=removeIndirection(((n_dsc*)XADDR(node))->pdsc);
	while (ind!=NULL)
	{
		ind->index->delete_from_index(pnode,z,size,ind->object);
		ind=ind->next;
	}
	delete [] z;
}

