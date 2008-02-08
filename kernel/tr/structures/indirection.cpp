/*
 * File:  indirection.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <string>
#include <set>

#include "common/sedna.h"

#include "tr/structures/indirection.h"
#include "common/u/usem.h"
#include "tr/vmm/vmm.h"
#include "tr/log/log.h"
#include "common/sm_vmm_data.h"
#include "tr/executor/base/xptr_sequence.h"

/*new static variables*/
static std::set<xptr>* blocks_to_delete;
//static std::set<xptr>* created_blocks;
static int rollback_mode = MODE_NORMAL;
static xptr rollback_record;
/*
#define OTK_XPTR
using namespace std;
typedef std::pair<schema_node*,xptr> id_pair;
static UShMem itfe_file_mapping;

static xptr* data_indirection_table_free_entry;
static xptr tmp_indirection_table_free_entry;

static USemaphore indirection_table_sem;
#ifndef OTK_XPTR
static std::map<id_pair,xptr_sequence *>* deleted_cells;
#else
static std::map<id_pair,std::vector<xptr> *>* deleted_cells;
#endif

static std::set<xptr>* deleted_docs;


bool delete_mode = false;

static int redo_hint=-1;
static std::vector<xptr>* redo_blocks=NULL;
int indir_block_count=0;
int indir_node_count=0;
*/


static bool indirection_session_initialized = false;
static bool indirection_transaction_initialized = false;
static bool indirection_statement_initialized = false;

static xptr last_indir = XNULL;

bool is_rolled_back()
{
	return rollback_mode != MODE_NORMAL;
}
/*xptr indir_blk_hdr::init(xptr p)
{    
	char* pred = ((char *)XADDR(p)) + sizeof(indir_blk_hdr);
    char* cur = pred + sizeof(xptr);
	unsigned int bl_end=(unsigned int)(XADDR(p)) + PAGE_SIZE;
	VMM_SIGNAL_MODIFICATION(p);
    while ((unsigned int)(cur + sizeof(xptr)) <= bl_end)
    {
        *((xptr*)pred) = ADDR2XPTR(cur);
        pred = cur;
        cur += sizeof(xptr);		
    }
	
	*((xptr*)pred) = XNULL;
    
    return p + sizeof(indir_blk_hdr);

}
xptr fill_empty_block(xptr p)
{
    CHECKP(p);
	((indir_blk_hdr*)XADDR(p))->nblk=XNULL;
    char* pred = ((char *)XADDR(p)) + sizeof(indir_blk_hdr);
    char* cur = pred + sizeof(xptr);
	unsigned int bl_end=(unsigned int)(XADDR(p)) + PAGE_SIZE;
	VMM_SIGNAL_MODIFICATION(p);
    while ((unsigned int)(cur + sizeof(xptr)) <= bl_end)
    {
        *((xptr*)pred) = ADDR2XPTR(cur);
        pred = cur;
        cur += sizeof(xptr);		
    }
	*((xptr*)pred) = XNULL;
    
    return p + sizeof(indir_blk_hdr);
}

xptr create_new_cluster(int cl_size,doc_schema_node* root,schema_node* sch,std::vector<xptr>* blocks)
{
	int cnt=cl_size;
	xptr first=root->ind_free_space;
	sch->indir_blk_cnt++;
	xptr cur=XNULL;
	if (first != XNULL)
	{
		//1. case when some place in block is left
		//1.1 counting this place
		int holes=(int)(((unsigned int)XADDR(BLOCKXPTR(first))+PAGE_SIZE)-(unsigned int)XADDR(first))/sizeof(xptr)-1;
		//1.2 checking whether the whole cluster fits into block
		if (cnt<=holes)
		{
			//1.3 checking whether there is free space left in block
			if (cnt==holes)
			{
				root->ind_free_space=first+(cnt*sizeof(xptr));
			}
			else
			{
				//1.4 breaking the chain
				xptr last=first+(cnt*sizeof(xptr));
				CHECKP(last);
				VMM_SIGNAL_MODIFICATION(last);
				hl_phys_log_change(XADDR(last),sizeof(xptr));
				*(xptr*)XADDR(last)=XNULL;
				root->ind_free_space=last;
			}
//			hl_logical_log_indirection( cl_size,NULL);
			return first+sizeof(xptr);
		}
		else
		{
			if (holes>0)
			{
				cnt-=holes;
				cur=first+(holes*sizeof(xptr));
			}
		}
	}
	//redo.1 initializing blks
	std::vector<xptr>* blks=(blocks==NULL)?se_new vector<xptr>:blocks;
	//2.counting total pages needed
	int xinp=(PAGE_SIZE-sizeof(indir_blk_hdr))/sizeof(xptr);
	int tot_pages=cnt/xinp;
	if (cnt % xinp !=0) tot_pages++;
	xptr tmp;
	//3.creating chain of blocks that form cluster
	indir_block_count++;
	sch->indir_blk_cnt++;
	//redo.2 fill blks
	if (blocks==NULL)
	{
		vmm_alloc_data_block(&tmp);
		hl_phys_log_create_node_blk(XADDR(tmp));
		blks->push_back(tmp);
	}
	else
	{
		tmp=blks->back();
        vmm_rcv_alloc_indir_block(tmp);
		blks->pop_back();
	}
    tmp=fill_empty_block(tmp);
	if  (first!=XNULL)
	{
		CHECKP(first);
		VMM_SIGNAL_MODIFICATION(first);
		hl_phys_log_change(&(((indir_blk_hdr*)XADDR(BLOCKXPTR(first)))->nblk),sizeof(xptr));
		((indir_blk_hdr*)XADDR(BLOCKXPTR(first)))->nblk=BLOCKXPTR(tmp);
		
		if (cur!=XNULL)
		{
			hl_phys_log_change(XADDR(cur),sizeof(xptr));
			*(xptr*)XADDR(cur)=tmp;
			first=first+sizeof(xptr);
		}
		else
		{
			first=tmp;
		}
	}
	else
	{
		first=tmp;
		root->first_ind_blk=BLOCKXPTR(tmp);
	}
	cur=tmp+(xinp-1)*sizeof(xptr);
	for (int i=1;i<tot_pages;i++)
	{
		indir_block_count++;
		sch->indir_blk_cnt++;
		//redo.2 fill blks
		if (blocks==NULL)
		{
			vmm_alloc_data_block(&tmp);
			hl_phys_log_create_node_blk(XADDR(tmp));
			blks->push_back(tmp);
		}
		else
		{
			tmp=blks->back();
            vmm_rcv_alloc_indir_block(tmp); 
			blks->pop_back();
		}
		tmp=fill_empty_block(tmp);
		CHECKP(cur);
		VMM_SIGNAL_MODIFICATION(cur);
		((indir_blk_hdr*)XADDR(BLOCKXPTR(cur)))->nblk=BLOCKXPTR(tmp);
		*(xptr*)XADDR(cur)=tmp;
		
		cur=tmp+(xinp-1)*sizeof(xptr);
	}
	//4.Processing the last block in chain of blocks 
	cnt=cnt-(tot_pages-1)*xinp;
	if (cnt!=xinp)
	{
		cur=tmp+(cnt-1)*sizeof(xptr);
		CHECKP(cur);
		VMM_SIGNAL_MODIFICATION(cur);
		*(xptr*)XADDR(cur)=XNULL;
		
		root->ind_free_space=cur;

	}
	else
		root->ind_free_space=cur;
	//redo.2 save log
	if (blocks==NULL)
	{
//		hl_logical_log_indirection( cl_size, blks);
		delete blks;
	}
	return first;
}
*/
xptr add_record_to_indirection_table(xptr p)
{
	xptr rba;
	CHECKP(p);
	node_blk_hdr * nbh=(GETBLOCKBYNODE(p));
	if (rollback_mode==MODE_UNDO)
    {
        
		rba=rollback_record;        
    }
	else
	{
		
		
		if (nbh->free_first_indir!=0)
			rba=ADDR2XPTR(GETPOINTERTODESC(nbh,nbh->free_first_indir));
		else
		{
			xptr q_bl=nbh->snode->bblk_indir;
			CHECKP(q_bl);
			node_blk_hdr * qbh=(GETBLOCKBYNODE(q_bl));
			rba=ADDR2XPTR(GETPOINTERTODESC(qbh,qbh->free_first_indir));
		}

	}
	CHECKP(rba);		
	VMM_SIGNAL_MODIFICATION(rba);

	node_blk_hdr * nbi=(GETBLOCKBYNODE(rba));
	hl_phys_log_change(&(nbi->free_first_indir),sizeof(shft));
	hl_phys_log_change(&(nbi->indir_count),sizeof(shft));
	nbi->indir_count++;
	nbi->free_first_indir=*((shft*)((char*)nbi+nbi->free_first_indir));
	hl_phys_log_change(XADDR(rba),sizeof(xptr));
    *(xptr*)(XADDR(rba)) = p;
	if (nbh!=nbi)
	{
		if (nbi->indir_count>=nbi->count&& 
			(nbi->pblk_indir!=XNULL ||
			nbi->nblk_indir!=XNULL ||
			nbi->snode->bblk_indir==nbi->sm_vmm.p))
		{
			xptr l_bl=nbi->pblk_indir;
			xptr r_bl=nbi->nblk_indir;
			if (nbi->snode->bblk_indir==nbi->sm_vmm.p)
				nbi->snode->bblk_indir=r_bl;
			else
			if (l_bl!=XNULL)
			{
				CHECKP(l_bl);
				VMM_SIGNAL_MODIFICATION(l_bl);
				node_blk_hdr * lbi=(GETBLOCKBYNODE(l_bl));
				hl_phys_log_change(&(lbi->nblk_indir),sizeof(xptr));
				lbi->nblk_indir=r_bl;
			}
			if (r_bl!=XNULL)
			{
				CHECKP(r_bl);
				VMM_SIGNAL_MODIFICATION(r_bl);
				node_blk_hdr * rbi=(GETBLOCKBYNODE(r_bl));
				hl_phys_log_change(&(rbi->pblk_indir),sizeof(xptr));
				rbi->pblk_indir=l_bl;
			}
				
		}
		CHECKP(p);
		if (nbh->indir_count<nbh->count&& 
			(nbh->pblk_indir==XNULL &&
			nbh->nblk_indir==XNULL &&
			nbh->snode->bblk_indir!=nbh->sm_vmm.p))
		{
			VMM_SIGNAL_MODIFICATION(p);
			hl_phys_log_change(&(nbh->nblk_indir),sizeof(xptr));
			nbh->nblk_indir=nbh->snode->bblk_indir;
			nbh->snode->bblk_indir=nbh->sm_vmm.p;
			xptr r_bl=nbh->nblk_indir;
			if (r_bl!=XNULL)
			{
				CHECKP(r_bl);
				VMM_SIGNAL_MODIFICATION(r_bl);
				node_blk_hdr * rbi=(GETBLOCKBYNODE(r_bl));
				hl_phys_log_change(&(rbi->pblk_indir),sizeof(xptr));
				rbi->pblk_indir=rbi->snode->bblk_indir;
			}
		}

	}	    
	CHECKP(rba);	
	
    //USemaphoreUp(indirection_table_sem);
	//indir_node_count++;
    last_indir = rba; // we need this hint to apply dynamic xptr remapping durind redo

    return rba;
}
/*
xptr add_record_to_data_indirection_table(xptr p)
{
    
//	if (rollback_mode!=MODE_NORMAL)
	if (rollback_mode == MODE_UNDO)
    {
        // This fragment of code can be thrown out because rollback_record
        // was not changed
		if (redo_hint>0)
		{
			CHECKP(p);
			schema_node* sch=(GETBLOCKBYNODE(p))->snode;
//			DebugBreak();
			create_new_cluster(redo_hint,sch->root,sch,redo_blocks);
			redo_hint=-1;
		}
        CHECKP(rollback_record);
		if (rollback_mode==MODE_REDO)
		{
			CHECKP(p);
			schema_node* sch=(GETBLOCKBYNODE(p))->snode;
			sch->ind_entry = *(xptr*)(XADDR(rollback_record));
			RECOVERY_CRASH;
			CHECKP(rollback_record);
		}
		VMM_SIGNAL_MODIFICATION(rollback_record);
        *(xptr*)(XADDR(rollback_record)) = p;

        
		//redo_hint=-1;

        return rollback_record;
    }
	
    // it means delete from list
    //USemaphoreDown(indirection_table_sem);
	indir_node_count++;
	//1. get schema information
	CHECKP(p);
	schema_node* sch=(GETBLOCKBYNODE(p))->snode;
	xptr res= sch->ind_entry;
	if (res==XNULL)
	{
		//2.1 calculating cluster size
		int cl_size=(sch->cl_hint==0)?MIN_CLUSTER_SIZE:sch->cl_hint;
		//2.2 creating new cluster
		res=create_new_cluster(cl_size,sch->root,sch,NULL);		
	}
	//3. filling indirection record|updateing schema node
	CHECKP(res);
	VMM_SIGNAL_MODIFICATION(res);
	sch->ind_entry = *(xptr*)(XADDR(res));
	hl_phys_log_change(XADDR(res),sizeof(xptr));
	*(xptr*)(XADDR(res)) = p;
	
    //USemaphoreUp(indirection_table_sem);
	
    last_indir = res;

    return res;
}
*/
void del_record_from_indirection_table(xptr p)
{
	CHECKP(p);		
	VMM_SIGNAL_MODIFICATION(p);

	xptr node=*((xptr*)XADDR(p));
	node_blk_hdr* nbi=GETBLOCKBYNODE(p);
	node_blk_hdr* nbh=GETBLOCKBYNODE(node);
	hl_phys_log_change(&(nbi->free_first_indir),sizeof(shft));
	hl_phys_log_change(&(nbi->indir_count),sizeof(shft));
	hl_phys_log_change(XADDR(p),sizeof(shft));
	nbi->indir_count--;	
	if (nbi->count+nbi->indir_count==0)
			add_predeleted_block(ADDR2XPTR(nbi));
    *(shft*)(XADDR(p)) = nbi->free_first_indir;
	nbi->free_first_indir=CALCSHIFT(XADDR(p),nbi);

	if (nbh!=nbi)
	{
		if (nbi->indir_count<nbi->count&& 
			(nbi->pblk_indir==XNULL &&
			nbi->nblk_indir==XNULL &&
			nbi->snode->bblk_indir!=nbi->sm_vmm.p))
		{
			VMM_SIGNAL_MODIFICATION(p);
			hl_phys_log_change(&(nbi->nblk_indir),sizeof(xptr));
			nbi->nblk_indir=nbi->snode->bblk_indir;
			nbi->snode->bblk_indir=nbi->sm_vmm.p;
			xptr r_bl=nbi->nblk_indir;
			if (r_bl!=XNULL)
			{
				CHECKP(r_bl);
				VMM_SIGNAL_MODIFICATION(r_bl);
				node_blk_hdr * rbi=(GETBLOCKBYNODE(r_bl));
				hl_phys_log_change(&(rbi->pblk_indir),sizeof(xptr));
				rbi->pblk_indir=rbi->snode->bblk_indir;
			}
		}

		CHECKP(node);
		if (nbh->indir_count>=nbh->count&& 
			(nbh->pblk_indir!=XNULL ||
			nbh->nblk_indir!=XNULL ||
			nbh->snode->bblk_indir==nbh->sm_vmm.p))
		{
			xptr l_bl=nbh->pblk_indir;
			xptr r_bl=nbh->nblk_indir;
			if (nbh->snode->bblk_indir==nbh->sm_vmm.p)
				nbh->snode->bblk_indir=r_bl;
			else
			if (l_bl!=XNULL)
			{
				CHECKP(l_bl);
				VMM_SIGNAL_MODIFICATION(l_bl);
				node_blk_hdr * lbi=(GETBLOCKBYNODE(l_bl));
				hl_phys_log_change(&(lbi->nblk_indir),sizeof(xptr));
				lbi->nblk_indir=r_bl;
			}
			if (r_bl!=XNULL)
			{
				CHECKP(r_bl);
				VMM_SIGNAL_MODIFICATION(r_bl);
				node_blk_hdr * rbi=(GETBLOCKBYNODE(r_bl));
				hl_phys_log_change(&(rbi->pblk_indir),sizeof(xptr));
				rbi->pblk_indir=l_bl;
			}
				
		}
		
	}	

	CHECKP(p);


}
/*
void del_record_from_data_indirection_table(xptr p)
{
    // whenever we in rollback mode or not, we just save p to put it to the list
    // of free cells later

    // it means put to list
    if (!delete_mode) 
	{
		CHECKP(p);
		xptr node=*(xptr*)XADDR(p);
		CHECKP(node);
		schema_node* sch=(GETBLOCKBYNODE(node))->snode;
#ifndef OTK_XPTR
		std::map<id_pair,xptr_sequence *>::iterator it=deleted_cells->find(id_pair(sch,sch->root->first_ind_blk));
#else
		std::map<id_pair,std::vector<xptr> *>::iterator it=deleted_cells->find(id_pair(sch,sch->root->first_ind_blk));
#endif
		
		if (it==deleted_cells->end())
		{
			
#ifndef OTK_XPTR
			xptr_sequence* xs=se_new xptr_sequence();
			xs->add(p);
#else
			std::vector<xptr>* xs=se_new std::vector<xptr>();
			xs->push_back(p);
#endif
			(*deleted_cells)[id_pair(sch,sch->root->first_ind_blk)]=xs;
		}
		else
#ifndef OTK_XPTR
			it->second->add(p);
#else
			it->second->push_back(p);
#endif
			

	}
	CHECKP(p);
	VMM_SIGNAL_MODIFICATION(p);
	hl_phys_log_change(XADDR(p),sizeof(xptr));
	VMM_SIGNAL_MODIFICATION(p);
	*((xptr*)XADDR(p))=XNULL;
	
}
void clear_ind_sequence(xptr& p)
{
	xptr tmp=p;
	while (tmp!=XNULL)
	{
		CHECKP(tmp);
		p=((indir_blk_hdr*)XADDR(tmp))->nblk;
		hl_phys_log_change_blk(XADDR(tmp));
		vmm_delete_block(tmp);
		RECOVERY_CRASH;
		tmp=p;
	}
}

xptr add_record_to_tmp_indirection_table(xptr p)
{
    if (tmp_indirection_table_free_entry == NULL)
    {
        xptr tmp;
        vmm_alloc_tmp_block(&tmp);
        tmp_indirection_table_free_entry = fill_empty_block(tmp);
    }

    CHECKP(tmp_indirection_table_free_entry);

    xptr res = tmp_indirection_table_free_entry;
	VMM_SIGNAL_MODIFICATION(res);
    tmp_indirection_table_free_entry = *(xptr*)(XADDR(res));
    *(xptr*)(XADDR(res)) = p;

    

    return res;
}

void del_record_from_tmp_indirection_table(xptr p)
{
    CHECKP(p);
	VMM_SIGNAL_MODIFICATION(p);
    *(xptr*)(XADDR(p)) = tmp_indirection_table_free_entry;
    tmp_indirection_table_free_entry = p;

    
}

void sync_indirection_table()
{
    // this functions should be called before commit

    //USemaphoreDown(indirection_table_sem);

    if(indirection_transaction_initialized)
	{

#ifndef OTK_XPTR
	    std::map<id_pair,xptr_sequence *>::iterator it= deleted_cells->begin();
#else
	    std::map<id_pair,std::vector<xptr> *>::iterator it= deleted_cells->begin();
#endif
	    while (it!=deleted_cells->end())
	    {
		    if (deleted_docs->find(it->first.second)==deleted_docs->end())
		    {
			    xptr p;
			    for (int i = 0; i < it->second->size(); i++)
		        {
#ifndef OTK_XPTR
			       p = it->second->get(i);
#else
			       p = it->second->at(i);
#endif
			       CHECKP(p);
			       VMM_SIGNAL_MODIFICATION(p);
			       hl_phys_log_change(XADDR(p),sizeof(xptr));
			       *(xptr*)(XADDR(p)) = it->first.first->ind_entry;
			       it->first.first->ind_entry= p;
			
		        }
			    delete it->second;
			    it->second=NULL;
		    }
		it++;
	    }
	    std::set<xptr>::iterator it2= deleted_docs->begin();
        xptr block;
	    while (it2!=deleted_docs->end())
 	    {
            block = *it2;
		    clear_ind_sequence(block);
			it2++;
		}
	}
   // USemaphoreUp(indirection_table_sem);    
}
void clear_dc()
{
#ifndef OTK_XPTR
	 deleted_cells = se_new std::map<id_pair,xptr_sequence *>;
#else
    if (deleted_cells!=NULL)
    {
		std::map<id_pair,std::vector<xptr> *>::iterator it= deleted_cells->begin();    	
		while (it!=deleted_cells->end())
		{
			if (it->second!=NULL) delete it->second;
			
			++it;    	
		}
    	delete deleted_cells;
    }
	deleted_cells = se_new std::map<id_pair,std::vector<xptr> *>;
#endif

}
*/
void indirection_table_on_session_begin()
{
  /*  tmp_indirection_table_free_entry = XNULL;

    if (uOpenShMem(&itfe_file_mapping, CHARISMA_ITFE_SHARED_MEMORY_NAME, sizeof(xptr), __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4021, "CHARISMA_ITFE_SHARED_MEMORY_NAME");

    data_indirection_table_free_entry = (xptr*)uAttachShMem(itfe_file_mapping, NULL, sizeof(xptr), __sys_call_error);
    if (data_indirection_table_free_entry == NULL) 
        throw USER_EXCEPTION2(SE4023, "CHARISMA_ITFE_SHARED_MEMORY_NAME");

    if (USemaphoreOpen(&indirection_table_sem, INDIRECTION_TABLE_SEMAPHORE_STR, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4012, "INDIRECTION_TABLE_SEMAPHORE_STR");
        clear_dc();

	if (deleted_docs!=NULL) delete deleted_docs;    
	deleted_docs = se_new std::set<xptr>;
*/
    indirection_session_initialized = true;
}

void indirection_table_on_transaction_begin()
{
/*clear_dc();

    if (deleted_docs!=NULL) delete deleted_docs;    
	deleted_docs = se_new std::set<xptr>;
*/
	if (blocks_to_delete!=NULL) delete blocks_to_delete;    
	blocks_to_delete = se_new std::set<xptr>;

	/*if (created_blocks!=NULL) delete created_blocks;    
	created_blocks = se_new std::set<xptr>;*/

    indirection_transaction_initialized = true;
}

void indirection_table_on_statement_begin()
{
    indirection_statement_initialized = true;
}

void indirection_table_on_session_end()
{
    if (indirection_session_initialized)
    {
        // deinitialization
    
     /*   if (uDettachShMem(itfe_file_mapping, data_indirection_table_free_entry, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4024, "CHARISMA_ITFE_SHARED_MEMORY_NAME");
    
        if (uCloseShMem(itfe_file_mapping, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4022, "CHARISMA_ITFE_SHARED_MEMORY_NAME");
    
        if (USemaphoreClose(indirection_table_sem, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4013, "INDIRECTION_TABLE_SEMAPHORE_STR");
    */
        indirection_session_initialized = false;
    }
}

void sync_indirection_table()
{
	std::set<xptr> tmp_set;
	/*if (rollback_mode == MODE_UNDO)
		{
			
			//1. scan the created blocks and remove from the list some of them
			
			std::set<xptr>::iterator it= created_blocks->begin();
			xptr block;
			while (it!=created_blocks->end())
			{
				block = *it;
				CHECKP(block);
				node_blk_hdr* blk=GETBLOCKBYNODE(block);
				if (blk->count+blk->indir_count>0)
					tmp_set.insert(block);
				it++;
			}
		}
	*/	
		//2. scan the deleted blocks and delete some of them
		std::set<xptr>::iterator it2= blocks_to_delete->begin();
		xptr block;
		while (it2!=blocks_to_delete->end())
		{
			block = *it2;
			CHECKP(block);
			node_blk_hdr* blk=GETBLOCKBYNODE(block);
			if (blk->count+blk->indir_count==0)
					deleteBlock(blk);
			it2++;
		}
        
		/*if (rollback_mode == MODE_UNDO)
		{
			//3. scan the created blocks and log the rest of them to the journal
			std::set<xptr>::iterator it= tmp_set.begin();
			xptr block;
			while (it!=tmp_set.end())
			{
				block = *it;
				CHECKP(block);
				node_blk_hdr* blk=GETBLOCKBYNODE(block);
				hl_logical_log_block_creation(block,blk->pblk,blk->nblk,blk->dsc_size);
				it++;
			}
		}*/

		//delete created_blocks;
		delete blocks_to_delete;
        //created_blocks = NULL;
		blocks_to_delete=NULL;

}
void indirection_table_on_transaction_end()
{
    if (indirection_transaction_initialized)
    {
		sync_indirection_table();
		rollback_mode = MODE_NORMAL;
		/*tmp_indirection_table_free_entry = XNULL;
		clear_dc();
		delete deleted_cells;
		delete deleted_docs;
        deleted_cells = NULL;
		deleted_docs=NULL;
*/
		
    
        indirection_transaction_initialized = false;
    }
}

void indirection_table_on_statement_end()
{
    if (indirection_statement_initialized)
    {
//	    tmp_indirection_table_free_entry = XNULL;

        indirection_statement_initialized = false;
    }
}

// functions for rollback

void switch_to_rollback_mode(int type)
{
 /*   clear_dc();
    if (deleted_docs!=NULL) delete deleted_docs;    
	deleted_docs = se_new std::set<xptr>;
*/

	if (blocks_to_delete==NULL)     
	blocks_to_delete = se_new std::set<xptr>;

	/*if (created_blocks==NULL) 
	created_blocks = se_new std::set<xptr>;
	*/
    rollback_mode = type;
}
/*
void start_delete_mode(doc_schema_node* doc)
{
	delete_mode=true;
	deleted_docs->insert(doc->first_ind_blk);
}
void stop_delete_mode()
{
	delete_mode=false;
}
*/
void set_rollback_record(xptr p)
{
    rollback_record = p;
}
/*
void set_redo_hint(int cl_hint,std::vector<xptr>* blocks)
{
	if (rollback_mode!=MODE_UNDO)
    {
		redo_hint=cl_hint;
		redo_blocks=blocks;
	}
}
*/

//new indirection
/*void add_new_block(xptr block)
{
	if (IS_DATA_BLOCK(block)) {
		created_blocks->insert(block);
	}
}*/
void add_predeleted_block(xptr block)
{
	if (IS_DATA_BLOCK(block)) {
		blocks_to_delete->insert(block);
	}
}




/*
 *  Check indirection infrastructure to be consistent.
 * 
 *  For given schema node the function checks following invariants:
 *
 *   - occupied indirection record count equals to the number of 
 *     occupied node descriptors
 *
 *   - count of occupied indirection records, stored in indirection quote blocks
 *     is less than node descriptor count for that blocks
 *
 *   - count of occupied indirection records, stored in blocks, that are not 
 *     indirection quote blocks, is greater than or equal to node descriptor 
 *     count for that blocks
 *
 *   - check block list pointers and indirection quota block list pointers
 *
 *  If recourse parameter is set to true, recoursively repeat function for all
 *  schema node children
 *
 */

bool check_indirection_consistency_schema(schema_node * sn, bool recourse = false) 
{
	xptr b, left_nb;
	node_blk_hdr * nbh;
	int node_desc_count, ind_rec_count;
	sc_ref* sn_i;
	
	b = sn->bblk;
	node_desc_count = 0;
	ind_rec_count = 0;
	while (b != XNULL) {
		CHECKP(b);
		nbh = ((node_blk_hdr *) XADDR(b));
		
		/* Count the total number of node descriptors and indirection records */
	
		node_desc_count += nbh->count;
		ind_rec_count += nbh->indir_count;

		/* If the block is in inirection quota list, check if it should really be
		 * there. And if it's not there, check whether it really should not. */
		
		if (nbh->nblk_indir != XNULL || nbh->pblk_indir != XNULL) {
			if (nbh->count <= nbh->indir_count) 
				throw USER_EXCEPTION2(SE2030, "Indirection quota block has overfull indirection table");
		} else {
			if (nbh->count > nbh->indir_count) 
				throw USER_EXCEPTION2(SE2030, "Non-indirection quota block has underfull indirection table");
		}
		
		b = nbh->nblk;
	}

  /* Check if the total number of node descriptors equals to the 
	 * one of indirection records  */
	
	if (node_desc_count != ind_rec_count)
		throw USER_EXCEPTION2(SE2030, "Total number of node descriptors and total number of indirection table records differ");
	
	b = sn->bblk_indir;
	left_nb = XNULL;
	while (b != XNULL) {
		CHECKP(b);
		nbh = ((node_blk_hdr *) XADDR(b));
 		
    /* Check if quota block list pointer is ok and 
		 * belongs to the right schema node */		

		if (nbh->snode != sn)
			throw USER_EXCEPTION2(SE2030, "Unexpected block in indirection quota chain");
		
		if (nbh->pblk_indir != left_nb)
			throw USER_EXCEPTION2(SE2030, "Broken indirection quota chain");
		
		left_nb = b;

		b = nbh->nblk_indir;
	}
	
	if (recourse) {
		sn_i = sn->first_child;
		while (sn_i != NULL) {
			if (!check_indirection_consistency_schema(sn_i->snode, true)) {
				return false;
			}
			sn_i = sn_i->next;
		}
	}
	
	return true;
}

/*
 *  Check indirection infrastructure to be consistent wrapper.
 * 
 *  Function takes node pointer as input and computes it's schema node.
 *  Then, checks indirection consistency for that schema node.
 *
 */

bool check_indirection_consistency(xptr p, bool recourse = false) 
{
  CHECKP(p);
	schema_node * sn = (GETBLOCKBYNODE(p))->snode;
	
	return check_indirection_consistency_schema(sn, recourse);
}

// we use this function to fetch a hint about last indirection record (for redo purposes)
xptr get_last_indir()
{
	return last_indir;
}
