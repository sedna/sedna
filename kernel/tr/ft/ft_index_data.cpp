/*
 * File:  ft_index_data.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "tr/ft/ft_index_data.h"
#include "common/xptr.h"
#include "tr/crmutils/node_utils.h"
#include "tr/vmm/vmm.h"
#include "tr/executor/base/tuple.h"
#ifdef SE_ENABLE_DTSEARCH
#include "tr/ft/FTindex.h"
#endif
#include "tr/ft/ft_index.h"
#include "tr/log/log.h"
#include "tr/structures/schema.h"
#include "tr/executor/fo/casting_operations.h"
//#include "indexes.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/crmutils/crmutils.h"
#include "tr/idx/btree/btree.h"


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
	if (USemaphoreOpen(&ft_index_sem, FT_INDEX_SEMAPHORE_STR, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4012, "FT_INDEX_SEMAPHORE_STR");

    ft_index_initialized = true;
}

void ft_index_on_session_end()
{
    if (!ft_index_initialized) return;
	//SEMAPHOR RELEASE SECTION
    if (USemaphoreClose(ft_index_sem, __sys_call_error) != 0)
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
		RECOVERY_CRASH;
	}
	pers_sset<ft_custom_cell,unsigned short>::sset_free(custom_tree);
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
ft_index_cell* ft_index_cell::create_index (PathExpr *object_path, ft_index_type it, doc_schema_node* schemaroot,const char * index_title, const char* doc_name,bool is_doc,std::vector< std::pair< std::pair<xml_ns*,char*>,ft_index_type> >* templ, bool just_heap,ft_index_impl impl)
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
	idc->impl = impl;
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

	//only needed for ft_ind_native impl
	ftc_index_t ftc_idx;
	if (impl == ft_ind_native)
	{
		idc->ft_data.btree_root = bt_create(xs_string); //FIXME: moved from ft_idx_create
		ftc_idx = ftc_get_index(index_title, idc->ft_data.btree_root);
	}

	ft_index_sem_up();
    hl_logical_log_ft_index(object_path, it,(char *) index_title, doc_name,is_doc,idc->custom_tree,true);
	
	// ALGORITHM: indexing data
	//II. Execute abs path (object_path) on the desriptive schema
	t_scmnodes sobj = execute_abs_path_expr(schemaroot, object_path);
	//III. For each schema node found (sn_obj)
	
	std::vector<xptr> start_nodes;
	for (int i = 0; i < sobj.size(); i++)
	{	
		xptr blk;
		sobj[i]->add_ft_index(idc); // just_heap modified because this must be recovered (AK)
		RECOVERY_CRASH;
		if (!just_heap)
		{
    		blk = getUnemptyBlockFore(sobj[i]->bblk);
			if (blk!=XNULL)
			{
				CHECKP(blk);
				start_nodes.push_back((GETBLOCKFIRSTDESCRIPTORABSOLUTE((node_blk_hdr*)XADDR(blk))));
			}
		}
	}
		
	up_concurrent_micro_ops_number();
		
	if (!just_heap) // moved here because ph ft_index info must be recovered fully (AK)
	{
    	// ft_index recovery should take the responsibility here
		switch (impl)
		{
#ifdef SE_ENABLE_DTSEARCH
		case ft_ind_dtsearch:
			{
			SednaIndexJob sij(idc);
			sij.create_index(&start_nodes);
			break;
			}
#endif
		case ft_ind_native:
			{
				ft_idx_create(&start_nodes, &idc->ft_data, idc->ftype, idc->custom_tree, ftc_idx);
			break;
			}
		default:
			throw USER_EXCEPTION2(SE1002, "unknow full-text index implementation"); //TODO: check it's ok to throw here
		}
	}
	
	return idc;
}
void ft_index_cell::delete_index (const char *index_title, bool just_heap)
{
	ft_index_sem_down();
	pers_sset<ft_index_cell,unsigned short>::pers_sset_entry* idc=search_ft_indexdata_cell(index_title);
	if (idc!=NULL)
	{
		
		down_concurrent_micro_ops_number();
		hl_logical_log_ft_index((idc->obj)->object,(idc->obj)->ftype,(idc->obj)->index_title,(idc->obj)->doc_name,(idc->obj)->is_doc,(idc->obj)->custom_tree,false);
		if (!just_heap) //FIXME: mb move out of down_concurrent_micro_ops_number() block
		{
			switch (idc->obj->impl)
			{
#ifdef SE_ENABLE_DTSEARCH
			case ft_ind_dtsearch:
				{
					SednaIndexJob sij(idc->obj);
					sij.clear_index();		
					break;
				}
#endif
			case ft_ind_native:
				{
					ft_idx_delete(&idc->obj->ft_data);
					break;
				}
			default:
				ft_index_sem_up();
				throw SYSTEM_EXCEPTION("unknow full-text index implementation");
			}
		}
		ft_index_cell* ic=idc->obj;
		doc_schema_node* sm=(idc->obj)->schemaroot;
		free_ft_indexdata_cell(idc);
		RECOVERY_CRASH;
		(*ft_idx_counter)--;
		sm->delete_ft_index(ic);
		ft_index_sem_up();
		up_concurrent_micro_ops_number();
	}
	else
		ft_index_sem_up();
}
ft_index_cell* ft_index_cell::find_index(const char* title, ftc_index_t *ftc_idx, bool have_ftind_sem)
{
	xptr res;
	if (!have_ftind_sem)
		ft_index_sem_down();
	pers_sset<ft_index_cell,unsigned short>::pers_sset_entry* idc=search_ft_indexdata_cell(title);
	if (idc==NULL)
	{
		if (!have_ftind_sem)
			ft_index_sem_up();	
		return NULL;
	}
	else
	{
		if (idc->obj->impl == ft_ind_native && ftc_idx != NULL)
			*ftc_idx = ftc_get_index(title, idc->obj->ft_data.btree_root);
		if (!have_ftind_sem)
			ft_index_sem_up();
		return idc->obj;
	}
}
void ft_index_cell::update_index(xptr_sequence* upserted)
{
	switch (this->impl)
	{
#ifdef SE_ENABLE_DTSEARCH
	case ft_ind_dtsearch:
		{
			SednaIndexJob sij(this);
			sij.update_index(upserted);
			break;
		}
#endif
	default:
		throw USER_EXCEPTION2(SE1002, "unknow full-text index implementation"); //TODO: check it's ok to trow here
	}

}
void ft_index_cell::insert_to_index(xptr_sequence* upserted)
{
	switch (this->impl)
	{
#ifdef SE_ENABLE_DTSEARCH
	case ft_ind_dtsearch:
		{
			SednaIndexJob sij(this);
			sij.insert_into_index(upserted);
			break;
		}
#endif
	default:
		throw USER_EXCEPTION2(SE1002, "unknow full-text index implementation"); //TODO: check it's ok to trow here
	}

}
void ft_index_cell::delete_from_index(xptr_sequence* deleted)
{
	switch (this->impl)
	{
#ifdef SE_ENABLE_DTSEARCH
	case ft_ind_dtsearch:
		{
			SednaIndexJob sij(this);
			sij.delete_from_index(deleted);
			break;
		}
#endif
	default:
		throw USER_EXCEPTION2(SE1002, "unknow full-text index implementation"); //TODO: check it's ok to trow here
	}
}
void ft_index_cell::change_index(xptr_sequence* inserted,xptr_sequence* updated,xptr_sequence* deleted)
{
	switch (this->impl)
	{
#ifdef SE_ENABLE_DTSEARCH
	case ft_ind_dtsearch:
		{
			SednaIndexJob sij(this);
			sij.insert_into_index(inserted);
			sij.update_index(updated);
			sij.delete_from_index(deleted);
			break;
		}
#endif
	default:
		throw USER_EXCEPTION2(SE1002, "unknow full-text index implementation"); //TODO: check it's ok to trow here
	}
}


xptr ft_index_cell::put_buf_to_pstr(op_str_buf& tbuf)
{
	xptr res;
	int sz=tbuf.get_size();
	if (sz<=PSTRMAXSIZE)
	{
		char* mem=tbuf.c_str();
		res=pstr_do_allocate(this->pstr_sequence,mem,sz);
		if (res==XNULL)
		{
			xptr new_blk = pstr_create_blk(true);
			res= pstr_do_allocate(new_blk, mem, sz);
		}
		return res;
	}
	else
	{
		//long pstr
		//TODO ROMA
	}
	return res;
}


void ft_index_cell::remove_from_pstr(doc_serial_header& head )
{
	if (head.length<=PSTRMAXSIZE)
	{
		pstr_do_deallocate(
			BLOCKXPTR(head.ptr),
			head.ptr, head.length, true);
	}
	else
	{
		//long pstr
		pstr_long_delete_str2(head.ptr);
	}
}

void ft_index_cell::init_serial_tree()
{
	//1. create b-tree
	this->serial_root=bt_create(xs_integer);
	
	//2. create pstr block
	pstr_sequence=
		pstr_create_blk(true);
}
void ft_index_cell::destroy_serial_tree()
{
	//1. loop b-tree
	bt_cursor_tmpl<doc_serial_header> cursor=
		bt_lm_tmpl<doc_serial_header>(this->serial_root);
	std::set<xptr> res;
	if(!cursor.is_null())
		{
			do
			{
				doc_serial_header head=cursor.bt_next_obj();
				if (head.length<=PSTRMAXSIZE)
					res.insert(BLOCKXPTR(head.ptr));//3. store sequence of pstr blocks
				else
				{
					//2. delete all long pstrs
					pstr_long_delete_str2(head.ptr);
				}
				
			}
			while (cursor.bt_next_key());
		}
	
	//4. delete b-tree
	bt_drop(this->serial_root);
	//5. delete pstrs
	set<xptr>::iterator it=res.begin();
	while (it!=res.end())
	{
		vmm_delete_block(*it);
		++it;
	}
}
doc_serial_header ft_index_cell::serial_put (xptr& node, op_str_buf& tbuf)
{
	
	//1. serialize node to buf and fill serial header
	print_node_to_buffer(node,tbuf,this->ftype,this->custom_tree);
	//2. put buf to pstr
	doc_serial_header dsh(tbuf.get_size(),put_buf_to_pstr(tbuf));
	//3. put header to b-tree
	bt_key key;
	key.setnew(*((__int64 *)&node));
	bt_insert_tmpl<doc_serial_header>(this->serial_root,key,dsh);
	//4. return header
	return dsh;
}
void ft_index_cell::serial_remove (xptr& node)
{
	//1. find header in b-tree
	bt_key key;
	key.setnew(*((_int64*)&node));
	
	bt_cursor_tmpl<doc_serial_header> cursor=bt_find_tmpl<doc_serial_header>(this->serial_root, key);
	if (cursor.is_null())
	{	U_ASSERT(false); return;}
	doc_serial_header head=cursor.bt_next_obj();
	//2. remove header from b-tree
	bt_delete_tmpl<doc_serial_header>(this->serial_root,key);
	//4. remove data from  pstr
	remove_from_pstr(head);
	
}
doc_serial_header ft_index_cell::serial_get (xptr& node)
{
	//1. find header in b-tree
	bt_key key;
	key.setnew(*((_int64*)&node));
	
	bt_cursor_tmpl<doc_serial_header> cursor=
		bt_find_tmpl<doc_serial_header>(this->serial_root, key);
	if (cursor.is_null())
	{	U_ASSERT(false); return doc_serial_header();}
	doc_serial_header head=cursor.bt_next_obj();
	//2. find pstr and copy to tbuf
		
	return head;
}
doc_serial_header ft_index_cell::serial_update (xptr& node, op_str_buf& tbuf)
{
	
	serial_remove(node);
	return serial_put(node,tbuf);
}

void doc_serial_header::parse(const char* data, int size, void* p)
{
	doc_parser* dp=(doc_parser*)p;
	//parse here
	dp->fn(data,size,dp->p);
}
void doc_serial_header::serialize(string_consumer_fn fn, void *p)
{
	doc_parser dp(fn,p);
	if (this->length<=PSTRMAXSIZE)
	{
		
		CHECKP(this->ptr);
		shft shift= *((shft*)XADDR(this->ptr));
		char* data=(char*)XADDR(BLOCKXPTR(this->ptr))+shift;
		parse(data,length,&dp);
	}
	else
	{
		pstr_long_feed2(this->ptr,doc_serial_header::parse,&dp);
	}
	//3. return header
}