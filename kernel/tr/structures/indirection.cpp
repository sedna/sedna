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

static int rollback_mode = MODE_NORMAL;
bool delete_mode = false;
static xptr rollback_record;
static int redo_hint=-1;
static std::vector<xptr>* redo_blocks=NULL;
int indir_block_count=0;
int indir_node_count=0;

static bool indirection_session_initialized = false;
static bool indirection_transaction_initialized = false;
static bool indirection_statement_initialized = false;

static xptr last_indir = XNULL;

bool is_rolled_back()
{
	return rollback_mode != MODE_NORMAL;
}
xptr indir_blk_hdr::init(xptr p)
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
			hl_logical_log_indirection( cl_size,NULL);
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
		hl_logical_log_indirection( cl_size, blks);
		delete blks;
	}
	return first;
}
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

void del_record_from_data_indirection_table(xptr p)
{
    // whenever we in rollback mode or not, we just save p to put it to the list
    // of free cells later

    // it means put to list
    if (/*!rollback_mode&&*/!delete_mode) 
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
void indirection_table_on_session_begin()
{
    tmp_indirection_table_free_entry = XNULL;

    if (uOpenShMem(&itfe_file_mapping, CHARISMA_ITFE_SHARED_MEMORY_NAME, sizeof(xptr), __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4021, "CHARISMA_ITFE_SHARED_MEMORY_NAME");

    data_indirection_table_free_entry = (xptr*)uAttachShMem(itfe_file_mapping, NULL, sizeof(xptr), __sys_call_error);
    if (data_indirection_table_free_entry == NULL) 
        throw USER_EXCEPTION2(SE4023, "CHARISMA_ITFE_SHARED_MEMORY_NAME");

    if (USemaphoreOpen(&indirection_table_sem, INDIRECTION_TABLE_SEMAPHORE_STR, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4012, "INDIRECTION_TABLE_SEMAPHORE_STR");
        clear_dc();
/*#ifndef OTK_XPTR
	 deleted_cells = se_new std::map<id_pair,xptr_sequence *>;
#else

	deleted_cells = se_new std::map<id_pair,std::vector<xptr> *>;
#endif
*/
	if (deleted_docs!=NULL) delete deleted_docs;    
	deleted_docs = se_new std::set<xptr>;

    indirection_session_initialized = true;
}

void indirection_table_on_transaction_begin()
{
clear_dc();
/*
#ifndef OTK_XPTR
	deleted_cells = se_new std::map<id_pair,xptr_sequence *>;
#else
	deleted_cells = se_new std::map<id_pair,std::vector<xptr> *>;
#endif
*/
    if (deleted_docs!=NULL) delete deleted_docs;    
	deleted_docs = se_new std::set<xptr>;

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
    
        if (uDettachShMem(itfe_file_mapping, data_indirection_table_free_entry, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4024, "CHARISMA_ITFE_SHARED_MEMORY_NAME");
    
        if (uCloseShMem(itfe_file_mapping, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4022, "CHARISMA_ITFE_SHARED_MEMORY_NAME");
    
        if (USemaphoreClose(indirection_table_sem, __sys_call_error) != 0)
            throw USER_EXCEPTION2(SE4013, "INDIRECTION_TABLE_SEMAPHORE_STR");
    
        indirection_session_initialized = false;
    }
}

void indirection_table_on_transaction_end()
{
    if (indirection_transaction_initialized)
    {
        rollback_mode = MODE_NORMAL;
		tmp_indirection_table_free_entry = XNULL;
		clear_dc();
		delete deleted_cells;
		delete deleted_docs;
        deleted_cells = NULL;
		deleted_docs=NULL;
    
        indirection_transaction_initialized = false;
    }
}

void indirection_table_on_statement_end()
{
    if (indirection_statement_initialized)
    {
	    tmp_indirection_table_free_entry = XNULL;

        indirection_statement_initialized = false;
    }
}


// functions for rollback

void switch_to_rollback_mode(int type)
{
    clear_dc();
/*    delete deleted_cells;
	delete deleted_docs;
    deleted_cells = NULL;
	deleted_docs=NULL;
#ifndef OTK_XPTR
	deleted_cells = se_new std::map<id_pair,xptr_sequence *>;
#else
	deleted_cells = se_new std::map<id_pair,std::vector<xptr> *>;
#endif
*/
    if (deleted_docs!=NULL) delete deleted_docs;    
	deleted_docs = se_new std::set<xptr>;

    rollback_mode = type;
}
void start_delete_mode(doc_schema_node* doc)
{
	delete_mode=true;
	deleted_docs->insert(doc->first_ind_blk);
}
void stop_delete_mode()
{
	delete_mode=false;
}
void set_rollback_record(xptr p)
{
    rollback_record = p;
}

void set_redo_hint(int cl_hint,std::vector<xptr>* blocks)
{
	if (rollback_mode!=MODE_UNDO)
    {
		redo_hint=cl_hint;
		redo_blocks=blocks;
	}
}

xptr get_last_indir()
{
	return last_indir;
}
