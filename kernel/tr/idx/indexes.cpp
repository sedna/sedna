/*
 * File:  indexes.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <vector>

#include "common/sedna.h"

#include "tr/idx/indexes.h"
#include "tr/structures/schema.h"
#include "tr/vmm/vmm.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/XPathOnSchema.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/log/log.h"

using namespace std;

void tuple_cell2bt_key(const tuple_cell& /*in*/ tc, bt_key& /*out*/ key)
{
    tuple_cell ltc = tuple_cell::make_sure_light_atomic(tc);
    // !!! ZAGLUSHKA
    // Note that the function call above should sometimes rase an exception
    // We must handle it and throw QEPTypeException that will be cought later
    // and the error counter will be incremented

    switch (ltc.get_atomic_type())
    {
        case xs_integer 		: key.setnew((__int64)(ltc.get_xs_integer()));	break; //!!! FIX THIS
        case xs_float			: key.setnew(ltc.get_xs_float());	break;
        case xs_double			: key.setnew(ltc.get_xs_double());	break;
        case xs_string			: key.setnew(ltc.get_str_mem());	break;
	case xs_date                    :
	case xs_dateTime                :
	case xs_time                    : key.setnew_dateTime(ltc.get_xs_dateTime(), ltc.get_atomic_type()); break;
	case xs_yearMonthDuration      :
	case xs_dayTimeDuration        : key.setnew_duration(ltc.get_xs_duration(), ltc.get_atomic_type()); break;

        default					: throw USER_EXCEPTION2(SE1003, "Unsupported type of index");
    }
}


//void bt_key2tuple_cell(const bt_key& /*in*/ key, tuple_cell& /*out*/ tc)
//{
//    switch (key.type)
//    {
//        case xs_float			: tc = tuple_cell::atomic(*(float*)(key.head));
//                                  break;
//        case xs_double			: tc = tuple_cell::atomic(*(double*)(key.head));
//                                  break;
//        case xs_string			: {
//                                      char *tmp = se_new char[key.size + 1];
//                                      memcpy(tmp, key.head, key.size);
//                                      tmp[key.size] = '\0';
//                                      tc = tuple_cell::atomic(xs_string, tmp);
//                                      break;
//                                  }
//        case xs_integer 		: tc = tuple_cell::atomic(*(int*)(key.head));
//                                  break;
//        default					: throw USER_EXCEPTION2(SE1003, "Unsupported type of index");
//    }
//}

pers_sset<index_cell,unsigned short>::pers_sset_entry* search_indexdata_cell(const char *index_title)
{
	return indexdata->get(index_title,NULL);	
}
void inline free_indexdata_cell(pers_sset<index_cell,unsigned short>::pers_sset_entry* entry)
{
	index_cell* idc=entry->obj;
	indexdata->rb_delete(entry);
	if (idc->index_title!=NULL)
		scm_free(idc->index_title,true);
	delete_PathExpr(idc->key);
	delete_PathExpr(idc->object);
	scm_free(idc->doc_name,true);
	scm_free(idc,true);
}
index_cell* create_index (PathExpr *object_path, 
                          PathExpr *key_path, 
                          xmlscm_type keytype, 
                          doc_schema_node* schemaroot,
                          const char * index_title, 
                          const char* doc_name,
                          bool is_doc)
{
    // I. Create and fill new index cell
	index_sem_down();
	if (search_indexdata_cell(index_title)!=NULL)
	{
		index_sem_up();	
		throw USER_EXCEPTION(SE2033);
	}
	down_concurrent_micro_ops_number();
	index_cell* idc=(index_cell*)scm_malloc(sizeof(index_cell),true);
    idc->id = *idx_counter;
    (*idx_counter)++;
	idc->keytype = keytype;
	idc->schemaroot = schemaroot;
	schemaroot->create_index(idc);
	idc->err_cntr = 0;
	idc->btree_root = bt_create(keytype);
	idc->object = object_path;
	idc->key = key_path;
	idc->index_title=(char*)scm_malloc(strlen(index_title)+1,true);
	strcpy(idc->index_title,index_title);
	idc->doc_name=(char*)scm_malloc(strlen(doc_name)+1,true);
	strcpy(idc->doc_name,doc_name);
	idc->is_doc=is_doc;
	/*idc->next = *indexdata;
	if ((*indexdata) != NULL) (*indexdata)->pred = idc;
	*indexdata = idc;*/
	indexdata->put(idc);
	index_sem_up();

	// ALGORITHM: indexing data

	//II. Execute abs path (object_path) on the desriptive schema
    t_scmnodes sobj = execute_abs_path_expr(schemaroot, object_path);
	//III. For each schema node found (sn_obj)
	for (int i = 0; i < sobj.size(); i++)
	{
		//IV. Execute path expression (key_path) on the descriptive schema
		t_scmnodes skey = execute_abs_path_expr(sobj[i], key_path);
		//V. For each schema node found (sn_key)
        for (int j = 0; j < skey.size(); j++)
		{
			//VI. Add pair <&ind,&sn_obj> into schema node (the special list is used)
			skey[j]->add_index(idc,sobj[i]);

			if (skey[j]->bblk != XNULL)
			{
				xptr blk = skey[j]->bblk;
				CHECKP(blk);
				xptr node_key = GETBLOCKFIRSTDESCRIPTORABSOLUTE(((node_blk_hdr*)XADDR(blk)));
				//VII. For every descriptor node_key that corresponds to sn_key.
				while (node_key != XNULL)
				{
					// Андрей, тут мне кажется надо light-atomic делать. Иначе после Chekp значение съедет.
					// конструировать ключь надо в стеке (то есть без new), тогда деструктор 
					// будет вызываться автоматически

                    try {

						//VIII. Evaluate key: (cast typed-value(node_key) as key_type)->bt_key
                        tuple_cell tc = cast(dm_typed_value(node_key), keytype);

						bt_key key;
                        tuple_cell2bt_key(tc, key);

						//X. Find descriptor object that corresponds to sn_obj
                        //   schema node and is an ancestor to node_key
						xptr obj_indir = getAncestorIndirectionByScheme((n_dsc*)XADDR(node_key), skey[j], sobj[i]);
						//XI. Insert pair <key,object> into b-tree
						CHECKP(idc->btree_root);
						bt_insert(idc->btree_root, key, obj_indir);

                    } catch (SednaUserException &e) {
						//IX. Increment the counter, which shows the number of 
                        //    items that were not inserted because they have
                        //    the type other than the index has
						idc->err_cntr++;
                    }
					
					node_key = getNextDescriptorOfSameSortXptr(node_key);
				}
			}
		}
	}
	hl_logical_log_index(object_path, key_path, keytype,index_title, doc_name,is_doc,true);
	up_concurrent_micro_ops_number();
	return idc;
}


counted_ptr<db_entity> find_entity(const char* title)
{
	xptr res;
    index_sem_down();
	pers_sset<index_cell,unsigned short>::pers_sset_entry* idc=search_indexdata_cell(title);
    if (idc!=NULL)
	{
		db_entity *res = se_new db_entity;
        res->name = se_new char[strlen(idc->obj->doc_name) + 1];
        strcpy(res->name, idc->obj->doc_name);
		res->type = (idc->obj->is_doc) ? dbe_document : dbe_collection;
		index_sem_up();
		return counted_ptr<db_entity>(res);
	}
	else 
	{
		index_sem_up();	
        throw USER_EXCEPTION2(SE1061, (std::string("Index '") + title + "'").c_str());
	}

}
void delete_index (const char *index_title)
{
	
	index_sem_down();
	pers_sset<index_cell,unsigned short>::pers_sset_entry* idc=search_indexdata_cell(index_title);
	if (idc!=NULL)
	{
		down_concurrent_micro_ops_number();
		xptr b_root=(idc->obj)->btree_root;
		bt_drop(b_root);
		hl_logical_log_index((idc->obj)->object,(idc->obj)->key,(idc->obj)->keytype, (idc->obj)->index_title,(idc->obj)->doc_name,(idc->obj)->is_doc,false);
		index_cell* ic=idc->obj;
		doc_schema_node* sm=(idc->obj)->schemaroot;
		free_indexdata_cell(idc);
		(*idx_counter)--;
		sm->delete_index(ic);
		index_sem_up();
		up_concurrent_micro_ops_number();
	}
	else
		index_sem_up();


}
xptr find_btree(index_id id)
{
     return XNULL;
}
xptr find_btree(const char* title)
{
    xptr res;

    index_sem_down();
	pers_sset<index_cell,unsigned short>::pers_sset_entry* idc=search_indexdata_cell(title);
    if (idc!=NULL)
	{
		res = idc->obj->btree_root;
		index_sem_up();
		return res;		
	}
	else 
	{
		index_sem_up();	
		return XNULL;
	}
}

xmlscm_type get_index_xmlscm_type(const char* title)
{
    xmlscm_type res;

    index_sem_down();
	pers_sset<index_cell,unsigned short>::pers_sset_entry* idc=search_indexdata_cell(title);
    if (idc!=NULL)
	{
		res = idc->obj->keytype;
		index_sem_up();
		return res;		
	}
	else 
	{
		index_sem_up();	
		return -1;
	}
}

