/*
 * File:  index_data.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string.h>
#include "ft_index_data.h"
#include "exceptions.h"
#include "xptr.h"
#include "node_utils.h"
#include "vmm.h"
#include "tuple.h"
#include "FTIndex.h"
#include "log.h"
#include "schema.h"
#include "casting_operations.h"
//#include "indexes.h"
#include "dm_accessors.h"


using namespace std;

pers_sset<ft_index_cell,unsigned short> *ft_indexdata;
index_id   *ft_idx_counter;
USemaphore ft_index_sem;//SEMAPHOR!!!

static bool ft_index_initialized = false;

//inits metadata library
void ft_index_on_session_begin(pers_sset<ft_index_cell,unsigned short> * _indexdata_, index_id *_idx_counter_)
{
    ft_indexdata = _indexdata_;
    ft_idx_counter = _idx_counter_;
	//SEMAPHOR INIT SECTION
	if (USemaphoreOpen(&ft_index_sem, FT_INDEX_SEMAPHORE_STR) != 0)
        throw USER_EXCEPTION2(SE4012, "FT_INDEX_SEMAPHORE_STR");

    ft_index_initialized = true;
}

void ft_index_on_session_end()
{
    if (!ft_index_initialized) return;
	//SEMAPHOR RELEASE SECTION
    if (USemaphoreClose(ft_index_sem) != 0)
        throw USER_EXCEPTION2(SE4013, "FT_INDEX_SEMAPHORE_STR");
    ft_index_initialized = false;
}

pers_sset<ft_index_cell,unsigned short>::pers_sset_entry* search_ft_indexdata_cell(const char *index_title)
{
	return ft_indexdata->get(index_title,NULL);	
}
ft_custom_cell* ft_custom_cell::init(xml_ns* _ns, const char* _local,ft_index_type _cm,bool persistent)
{
	ft_custom_cell* ftc=(ft_custom_cell*)scm_malloc(sizeof(ft_custom_cell),persistent);
	ftc->ns=_ns;
	if (_local!=NULL)
	{
		char* z=(char*)scm_malloc(strlen(_local)+1,persistent);
		strcpy(z,_local);
		ftc->local=z;
	}
	else
		ftc->local=NULL;
	ftc->cm=_cm;
	return ftc;
}
void ft_index_cell::delete_custom_tree (pers_sset<ft_custom_cell,unsigned short> * custom_tree)
{
	pers_sset<ft_custom_cell,unsigned short>::pers_sset_entry* tmp=custom_tree->rb_minimum(custom_tree->root);
	while (tmp!=NULL)
	{
		//coll->free_metadata(tmp);
		ft_custom_cell* mdc=tmp->obj;
		if (mdc->local)
			scm_free(mdc->local,IS_PH_PTR(mdc));
		scm_free(mdc,IS_PH_PTR(mdc));
		tmp->obj=NULL;
		tmp=custom_tree->rb_successor(tmp);
	}
	pers_sset<ft_custom_cell,unsigned short>::free(custom_tree);
}
void inline free_ft_indexdata_cell(pers_sset<ft_index_cell,unsigned short>::pers_sset_entry* entry)
{
	ft_index_cell* idc=entry->obj;
	ft_indexdata->rb_delete(entry);
	if (idc->index_title!=NULL)
		scm_free(idc->index_title,true);
	delete_PathExpr(idc->object);
	scm_free(idc->doc_name,true);
	if (idc->custom_tree!=NULL)
		ft_index_cell::delete_custom_tree(idc->custom_tree);
	scm_free(idc,true);
}
bool ft_index_cell::fits_to_index(schema_node* snode)
{
	t_scmnodes res;
	t_scmnodes objs=execute_abs_path_expr(snode->root,object);
	t_scmnodes::iterator it=objs.begin();
	while (it!=objs.end())
	{
		if (*it==snode)		
			return true;		
		it++;
	}
	return false;
}
ft_index_cell* ft_index_cell::create_index (PathExpr *object_path, ft_index_type it, doc_schema_node* schemaroot,const char * index_title, const char* doc_name,bool is_doc,std::vector< std::pair< std::pair<xml_ns*,char*>,ft_index_type> >* templ, bool just_heap)
{
	// I. Create and fill new index cell
	ft_index_sem_down();
	if (search_ft_indexdata_cell(index_title)!=NULL)
	{
		ft_index_sem_up();	
		throw USER_EXCEPTION(SE2033);
	}
	down_concurrent_micro_ops_number();
	ft_index_cell* idc=(ft_index_cell*)scm_malloc(sizeof(ft_index_cell),true);
    idc->id = *ft_idx_counter;
    (*ft_idx_counter)++;
	idc->schemaroot = schemaroot;
	schemaroot->create_ft_index(idc);
	idc->object = object_path;
	idc->ftype=it;
	idc->index_title=(char*)scm_malloc(strlen(index_title)+1,true);
	strcpy(idc->index_title,index_title);
	idc->doc_name=(char*)scm_malloc(strlen(doc_name)+1,true);
	strcpy(idc->doc_name,doc_name);
	idc->is_doc=is_doc;
	if (it==ft_customized_value && templ!=NULL)
	{
		idc->custom_tree= pers_sset<ft_custom_cell,unsigned short>::init();
		std::vector<std::pair<std::pair<xml_ns*,char*>,ft_index_type> >::iterator tmp=templ->begin();
		while (tmp!=templ->end())
		{
			
			idc->custom_tree->put(ft_custom_cell::init((*tmp).first.first,(*tmp).first.second,(*tmp).second));
			tmp++;
		}
	}
	/*idc->next = *indexdata;
	if ((*indexdata) != NULL) (*indexdata)->pred = idc;
	*indexdata = idc;*/
	ft_indexdata->put(idc);
	ft_index_sem_up();
    hl_logical_log_ft_index(object_path, it,(char *) index_title, doc_name,is_doc,idc->custom_tree,true);
	up_concurrent_micro_ops_number();
	// ALGORITHM: indexing data
	//II. Execute abs path (object_path) on the desriptive schema
	if (!just_heap)
	{
		t_scmnodes sobj = execute_abs_path_expr(schemaroot, object_path);
		//III. For each schema node found (sn_obj)
		std::vector<xptr> start_nodes;
		for (int i = 0; i < sobj.size(); i++)
		{	
			xptr blk= sobj[i]->bblk;
			sobj[i]->add_ft_index(idc);
			if (blk!=XNULL)
			{
			CHECKP(blk);
			start_nodes.push_back((GETBLOCKFIRSTDESCRIPTORABSOLUTE(((node_blk_hdr*)XADDR(blk)))));
			}
		}
		
		SednaIndexJob sij(idc);
		sij.create_index(&start_nodes);
	}
	return idc;
}
void ft_index_cell::delete_index (const char *index_title, bool just_heap)
{
	ft_index_sem_down();
	pers_sset<ft_index_cell,unsigned short>::pers_sset_entry* idc=search_ft_indexdata_cell(index_title);
	if (idc!=NULL)
	{
		
		if (!just_heap)
		{
			SednaIndexJob sij(idc->obj);
			sij.clear_index();		
		}
		down_concurrent_micro_ops_number();
		hl_logical_log_ft_index((idc->obj)->object,(idc->obj)->ftype,(idc->obj)->index_title,(idc->obj)->doc_name,(idc->obj)->is_doc,(idc->obj)->custom_tree,false);
		ft_index_cell* ic=idc->obj;
		doc_schema_node* sm=(idc->obj)->schemaroot;
		free_ft_indexdata_cell(idc);
		(*ft_idx_counter)--;
		sm->delete_ft_index(ic);
		ft_index_sem_up();
		up_concurrent_micro_ops_number();
	}
	else
		ft_index_sem_up();
}
ft_index_cell* ft_index_cell::find_index(const char* title)
{
	xptr res;
    ft_index_sem_down();
	pers_sset<ft_index_cell,unsigned short>::pers_sset_entry* idc=search_ft_indexdata_cell(title);
	if (idc==NULL)
	{
		ft_index_sem_up();	
		return NULL;
	}
	else
	{
		ft_index_sem_up();	
		return idc->obj;
	}
}
void ft_index_cell::update_index(xptr_sequence* upserted)
{
	SednaIndexJob sij(this);
	sij.update_index(upserted);

}
void ft_index_cell::insert_to_index(xptr_sequence* upserted)
{
	SednaIndexJob sij(this);
	sij.insert_into_index(upserted);

}
void ft_index_cell::delete_from_index(xptr_sequence* deleted)
{
	SednaIndexJob sij(this);
	sij.delete_from_index(deleted);
}
void ft_index_cell::change_index(xptr_sequence* inserted,xptr_sequence* updated,xptr_sequence* deleted)
{
	SednaIndexJob sij(this);
	sij.insert_into_index(inserted);
	sij.update_index(updated);
	sij.delete_from_index(deleted);
}