/*
 * File:  metadata.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/structures/metadata.h"
#include "common/xptr.h"
#include "tr/mo/micro.h"
#include "tr/log/log.h"
#include "tr/structures/indirection.h"
#include "tr/crmutils/crmutils.h"
#include "tr/locks/locks.h"
#include "tr/idx/indexes.h"
#ifdef SE_ENABLE_FTSEARCH
#include "tr/ft/ft_index_data.h"
#include "tr/updates/updates.h"
#endif
using namespace std;
pers_sset<sn_metadata_cell,unsigned short> *metadata;
USemaphore metadata_sem;//SEMAPHOR!!!

static bool metadata_initialized = false;

//inits metadata library
void metadata_on_session_begin(pers_sset<sn_metadata_cell,unsigned short> *mdc)
{
    metadata = mdc;
	//SEMAPHOR INIT SECTION
    if (USemaphoreOpen(&metadata_sem, METADATA_SEMAPHORE_STR, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4012, "METADATA_SEMAPHORE_STR");

    metadata_initialized = true;
}

void metadata_on_session_end()
{
    if (!metadata_initialized) return;
	//SEMAPHOR RELEASE SECTION
    if (USemaphoreClose(metadata_sem, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4013, "METADATA_SEMAPHORE_STR");
    metadata_initialized = false;
}



void inline free_metadata_cell(pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* entry)
{
	sn_metadata_cell* mdc=entry->obj;
	metadata->rb_delete(entry);
	if (mdc->collection_name!=NULL) 
		scm_free(mdc->collection_name,true);
	else
	 scm_free(mdc->document_name,true);
	scm_free(mdc,true);
}


pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* search_metadata_cell(const char *collection_name, const char *document_name)
{
	return metadata->get(collection_name,document_name);	
}

void delete_document(const char *document_name)
{
	metadata_sem_down();
	pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* mdc=search_metadata_cell(NULL,document_name);
	if (mdc!=NULL)
	{
	//	down_concurrent_micro_ops_number();
		schema_node* snode=(mdc->obj)->snode;
		free_metadata_cell(mdc);
		metadata_sem_up();
		//0. turn on delete mode for indirection
		start_delete_mode((doc_schema_node*)snode);
		schema_ind_cell* sci=((doc_schema_node*)snode)->sc_idx;
		while (sci!=NULL)
		{
			delete_index(sci->index->index_title);
			sci=((doc_schema_node*)snode)->sc_idx;
		}
#ifdef SE_ENABLE_FTSEARCH
	    schema_ft_ind_cell* ftsci=((doc_schema_node*)snode)->sc_ft_idx;
		while (ftsci!=NULL)
		{
			ft_index_cell::delete_index(ftsci->index->index_title);
			ftsci=((doc_schema_node*)snode)->sc_ft_idx;
		}
#endif
		xptr blk=snode->bblk;
		if (blk!=XNULL)
		{
			CHECKP(blk);
			xptr ind=((n_dsc*)((char*)XADDR(blk)+((node_blk_hdr*)XADDR(blk))->desc_first))->indir;
			delete_doc_node(GETBLOCKFIRSTDESCRIPTORABSOLUTE((node_blk_hdr*)XADDR(blk)));
			hl_logical_log_document(ind,document_name,NULL,false);
		}
		
		
		snode->delete_scheme_node();
		up_concurrent_micro_ops_number();
		
		//0. turn off delete mode for indirection
		stop_delete_mode();
	}
	else
	{
		metadata_sem_up();
		throw USER_EXCEPTION2(SE2006, document_name);
	}
}

void delete_document(const char *collection_name,const char *document_name)
{
	metadata_sem_down();
	pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* cdc=search_metadata_cell(collection_name,NULL);
	if (cdc==NULL)
	{
		metadata_sem_up();
		throw USER_EXCEPTION2(SE2003, collection_name);
		return;
	}
	col_schema_node* coll=(col_schema_node*)cdc->obj->snode;
	pers_sset<dn_metadata_cell,unsigned int>::pers_sset_entry* mdc=coll->search_metadata_cell(document_name);
	if (mdc!=NULL) 
	{
		//down_concurrent_micro_ops_number();
		xptr node=(mdc->obj)->root;
		coll->free_metadata_cell(mdc);
		metadata_sem_up();
		if (node!=XNULL)
		{
			CHECKP(node);
			if ((GETBLOCKBYNODE(node))->count==1 && coll->eblk==BLOCKXPTR(node))
				coll->eblk=(GETBLOCKBYNODE(node))->pblk;
			xptr ind=((n_dsc*)XADDR(node))->indir;
#ifdef SE_ENABLE_FTSEARCH
			clear_ft_sequences();
#endif

			delete_doc_node(node);
			hl_logical_log_document(ind,document_name,collection_name,false);
		}
		up_concurrent_micro_ops_number();
#ifdef SE_ENABLE_FTSEARCH
	execute_modifications();
#endif
	}
	else
	{
		metadata_sem_up();
		throw USER_EXCEPTION2(SE2006,document_name);
	}
}

void delete_collection(const char *collection_name)
{
	metadata_sem_down();
	pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* cdc=search_metadata_cell(collection_name,NULL);
	if (cdc==NULL)
	{
		metadata_sem_up();
		throw USER_EXCEPTION2(SE2003, collection_name);
		return;
	}
	col_schema_node* coll=(col_schema_node*)cdc->obj->snode;
	free_metadata_cell(cdc);
	metadata_sem_up();
	//0. turn on delete mode for indirection
	start_delete_mode(coll);
	schema_ind_cell* sci=coll->sc_idx;
		while (sci!=NULL)
		{
			delete_index(sci->index->index_title);
			sci=coll->sc_idx;
		}
#ifdef SE_ENABLE_FTSEARCH
	    schema_ft_ind_cell* ftsci=coll->sc_ft_idx;
		while (ftsci!=NULL)
		{
			ft_index_cell::delete_index(ftsci->index->index_title);
			ftsci=coll->sc_ft_idx;
		}
#endif
	//1. deleting documents from collection
	pers_sset<dn_metadata_cell,unsigned int>::pers_sset_entry* tmp=coll->metadata->rb_minimum(coll->metadata->root);
	while (tmp!=NULL)
	{
		//down_concurrent_micro_ops_number();
		dn_metadata_cell* dn=tmp->obj;
		xptr node=dn->root;
		if (node!=XNULL)
		{
			CHECKP(node);
			hl_logical_log_document(((n_dsc*)XADDR(node))->indir,dn->document_name,collection_name,false);
			delete_doc_node(node);
		}
		up_concurrent_micro_ops_number();
		tmp=coll->metadata->rb_successor(tmp);
	}
	//2. deleting cells
	tmp=coll->metadata->rb_minimum(coll->metadata->root);
	while (tmp!=NULL)
	{
		coll->free_metadata(tmp);
		tmp=coll->metadata->rb_successor(tmp);
	}
	coll->free_map();
	down_concurrent_micro_ops_number();
	
	((schema_node*)coll)->delete_scheme_node();
	hl_logical_log_collection(collection_name,false);
	//free_metadata_cell(cdc);
	up_concurrent_micro_ops_number();
	//3. turn off delete mode for indirection
	stop_delete_mode();
	//metadata_sem_up();
}

xptr insert_document(const char *uri,bool persistent)
{
	doc_schema_node* scm=NULL;
	string name=get_name_from_uri(uri);
	char* name_ptr=NULL;
if (persistent)
	{	
    metadata_sem_down();
	
	if (search_metadata_cell(NULL,name.c_str())!=NULL)
	{
	  metadata_sem_up();
	  throw USER_EXCEPTION(SE2001);
	}
	
	down_concurrent_micro_ops_number();
	sn_metadata_cell* mdc=(sn_metadata_cell*)scm_malloc(sizeof(sn_metadata_cell),true);
	mdc->collection_name=NULL;
	mdc->document_name=(char*)scm_malloc(name.size()+1,true);
	name_ptr=mdc->document_name;
	strcpy(mdc->document_name,name.c_str());
	scm=	doc_schema_node::init(true);
	mdc->snode=scm;
	metadata->put(mdc);
	metadata_sem_up();
}
else
	scm=doc_schema_node::init(false);
	xptr blk=createNewBlock(scm,persistent);
	node_blk_hdr* block_hdr=(node_blk_hdr*) XADDR(blk);
	n_dsc* node= GETPOINTERTODESC(block_hdr,block_hdr->free_first);
	block_hdr->free_first=*((shft*)node);
	block_hdr->desc_first=CALCSHIFT(node,block_hdr);
	block_hdr->desc_last=block_hdr->desc_first;
	d_dsc::init(node);
	xptr nodex=ADDR2XPTR(node);
	xptr tmp=add_record_to_indirection_table(nodex);
	CHECKP(nodex);
	node->indir=tmp;
	block_hdr->count=1;
	//NODE STATISTICS
	block_hdr->snode->nodecnt++;
	VMM_SIGNAL_MODIFICATION(nodex);
	nid_create_root(nodex,persistent);
	if (persistent)
	{
	
		CHECKP(nodex);
		addTextValue(nodex,name_ptr,name.length());
		VMM_SIGNAL_MODIFICATION(nodex);
		hl_logical_log_document(node->indir,uri,NULL,true);							up_concurrent_micro_ops_number();
	}	
	return nodex;
}

schema_node *insert_collection(const char *collection_name)
{
	metadata_sem_down();
	if (search_metadata_cell(collection_name,NULL)!=NULL)
	{
		metadata_sem_up();	
		throw USER_EXCEPTION(SE2002);
	}
	down_concurrent_micro_ops_number();
	sn_metadata_cell* mdc=(sn_metadata_cell*)scm_malloc(sizeof(sn_metadata_cell),true);
	mdc->collection_name=(char*)scm_malloc(strlen(collection_name)+1,true);
	strcpy(mdc->collection_name,collection_name);
	mdc->document_name=NULL;
	col_schema_node* scm=col_schema_node::init(true);
	scm->metadata=pers_sset<dn_metadata_cell,unsigned int>::init();
	mdc->snode=scm;
	metadata->put(mdc);
	hl_logical_log_collection(collection_name,true);
	up_concurrent_micro_ops_number();
	metadata_sem_up();
	return scm;
}

xptr insert_document_in_collection(const char *collection_name, const char *uri)
{
	string name=get_name_from_uri(uri);
	if (find_document(collection_name,name.c_str())!=NULL)
	{
		throw USER_EXCEPTION(SE2004);
	}
	metadata_sem_down();
	sn_metadata_cell* coll=NULL;
	pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* ptr = search_metadata_cell(collection_name,NULL);
    if (ptr == NULL)
	{
		metadata_sem_up();
		throw USER_EXCEPTION(SE2003);
	}	
    coll=ptr->obj;
    down_concurrent_micro_ops_number();
	dn_metadata_cell* mdc=(dn_metadata_cell*)scm_malloc(sizeof(dn_metadata_cell),true);
	mdc->document_name=(char*)scm_malloc(name.size()+1,true);
	strcpy(mdc->document_name,name.c_str());
	col_schema_node* scm=(col_schema_node*)coll->snode;
	xptr block=XNULL;
	n_dsc* node=NULL;
	xptr blk= scm->eblk;
	if (blk!=XNULL)
	{
		CHECKP(blk);
		while ((GETBLOCKBYNODE(blk))->nblk!=XNULL)
		{
			blk=(GETBLOCKBYNODE(blk))->nblk;
			scm->eblk=blk;
			CHECKP(blk);		
		}
		if ((GETBLOCKBYNODE(blk))->free_first==0) 
		{
			block=createBlockNextToTheCurrentBlock(((node_blk_hdr*)XADDR(blk)));
			scm->eblk=block;
		}
	}
	else
	{
		block=createNewBlock(scm,true);
		scm->eblk=block;
	}
	xptr nodex;
	if (block==XNULL)
	{
		CHECKP(blk);
		VMM_SIGNAL_MODIFICATION(blk);
		node_blk_hdr* block_hdr=(node_blk_hdr*) XADDR(blk);
		node= GETPOINTERTODESC(block_hdr,block_hdr->free_first);
		//PHYS LOG
		if (IS_DATA_BLOCK(blk)) 
		{
			hl_phys_log_change(&block_hdr->free_first,sizeof(shft));
			hl_phys_log_change(&block_hdr->desc_last,sizeof(shft));
			hl_phys_log_change(&block_hdr->count,sizeof(shft));
			hl_phys_log_change(node,block_hdr->dsc_size);
			hl_phys_log_change(&(GETPOINTERTODESC(block_hdr,block_hdr->desc_last))->desc_next,sizeof(shft));
		}
		block_hdr->free_first=*((shft*)node);
		(GETPOINTERTODESC(block_hdr,block_hdr->desc_last))->desc_next=CALCSHIFT(node,block_hdr);
		d_dsc::init(node);
		nodex=ADDR2XPTR(node);
		xptr tmp=add_record_to_indirection_table(nodex);
		CHECKP(nodex);
		VMM_SIGNAL_MODIFICATION(blk);
		node->indir=tmp;
		//NODE STATISTICS
		block_hdr->snode->nodecnt++;
		hl_logical_log_document(node->indir,uri,collection_name,true);
		nid_create_root(nodex,true);
		CHECKP(nodex);
		VMM_SIGNAL_MODIFICATION(blk);
		node->desc_prev=block_hdr->desc_last;
		block_hdr->desc_last=CALCSHIFT(node,block_hdr);
		block_hdr->count++;
		addTextValue(nodex,mdc->document_name,name.length());
	}
	else
	{
		node_blk_hdr* block_hdr=(node_blk_hdr*) XADDR(block);
		node= GETPOINTERTODESC(block_hdr,block_hdr->free_first);
		block_hdr->free_first=*((shft*)node);
		block_hdr->desc_first=CALCSHIFT(node,block_hdr);
		block_hdr->desc_last=block_hdr->desc_first;
		block_hdr->count++;
		d_dsc::init(node);
		nodex=ADDR2XPTR(node);
		xptr tmp=add_record_to_indirection_table(nodex);
		CHECKP(block);
		VMM_SIGNAL_MODIFICATION(block);
		node->indir=tmp;
		//NODE STATISTICS
		block_hdr->snode->nodecnt++;
		hl_logical_log_document(node->indir,uri,collection_name,true);
		nid_create_root(nodex,true);
		CHECKP(nodex);
		addTextValue(nodex,mdc->document_name,name.length());
		CHECKP(block);
	}
	mdc->root=nodex;
	scm->metadata->put(mdc);
	metadata_sem_up();
	up_concurrent_micro_ops_number();
	return nodex;

}

schema_node *find_collection(const char *collection_name)
{
	metadata_sem_down();
	pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* mdc=search_metadata_cell(collection_name,NULL);
	if 	(mdc!=NULL)
	{
		schema_node* snode=mdc->obj->snode;
		metadata_sem_up();
		return snode ;
	}
	else
	{
		metadata_sem_up();
		return NULL;
	}
}

xptr find_document(const char *collection_name,const char *document_name)
{
	metadata_sem_down();
	pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* mdc=search_metadata_cell(collection_name,NULL);
	if 	(mdc!=NULL)
	{
		pers_sset<dn_metadata_cell,unsigned int>::pers_sset_entry* dc=((col_schema_node*)mdc->obj->snode)->search_metadata_cell(document_name);
		xptr res=(dc!=NULL)?dc->obj->root:XNULL;
		metadata_sem_up();
		return res;
	}
	else
	{
		metadata_sem_up();
		return XNULL;
	}
}

schema_node *find_document(const char *document_name)
{
	metadata_sem_down();
	pers_sset<sn_metadata_cell,unsigned short>::pers_sset_entry* mdc=search_metadata_cell(NULL,document_name);
	if 	(mdc!=NULL)
	{
		schema_node* snode=mdc->obj->snode;
		metadata_sem_up();
		return snode;
	}
	else
	{
		metadata_sem_up();
		return NULL;
	}
}

//UNREALIZED!!! TEMPORARY
string  get_name_from_uri(const char* uri)
{
 return string(uri);
}

