/*
 * File:  index_data.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string.h>

#include "common/sedna.h"

#include "tr/idx/index_data.h"
#include "common/xptr.h"
#include "tr/crmutils/node_utils.h"
#include "tr/vmm/vmm.h"
#include "tr/executor/base/tuple.h"
#include "tr/idx/btree/btree.h"
#include "tr/structures/schema.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/idx/indexes.h"
#include "tr/executor/base/dm_accessors.h"


using namespace std;

pers_sset<index_cell,unsigned short> *indexdata;
index_id   *idx_counter;
USemaphore index_sem;//SEMAPHOR!!!

static bool index_initialized = false;

//inits metadata library
void index_on_session_begin(pers_sset<index_cell,unsigned short> * _indexdata_, index_id *_idx_counter_)
{
    indexdata = _indexdata_;
    idx_counter = _idx_counter_;
	//SEMAPHOR INIT SECTION
	if (USemaphoreOpen(&index_sem, INDEX_SEMAPHORE_STR, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4012, "INDEX_SEMAPHORE_STR");

    index_initialized = true;
}

void index_on_session_end()
{
    if (!index_initialized) return;
	//SEMAPHOR RELEASE SECTION
    if (USemaphoreClose(index_sem, __sys_call_error) != 0)
        throw USER_EXCEPTION2(SE4013, "INDEX_SEMAPHORE_STR");
    index_initialized = false;
}


/*xptr find_btree(index_id id)
{
    xptr res;

    index_sem_down();

    index_cell *cur = *indexdata;
    while (cur != NULL)
    {
        if (cur->id == id) break;
        cur = cur->next;
    }

    res = cur->btree_root;

    index_sem_up();

    return res;
}*/
t_scmnodes index_cell::fits_to_index_as_key(schema_node* snode)
{
	t_scmnodes res;
	t_scmnodes objs=execute_abs_path_expr(snode->root, object);
	t_scmnodes::iterator it=objs.begin();
	while (it!=objs.end())
	{
		if ((*it)->is_ancestor_or_self(snode))
		{
			t_scmnodes keys=execute_abs_path_expr(*it, key);
			t_scmnodes::iterator it2=keys.begin();
			while (it2!=keys.end())
			{
				if (snode==*it2)
				{
					res.push_back(*it);
					break;
				}
				it2++;
			}
		}
		it++;
	}
	return res;
}
void index_cell::put_to_index(xptr node, schema_node* accessor)
{
	xptr acc=getNodeAncestorIndirectionByScheme(node,accessor);
	try
	{
		tuple_cell tc = cast(dm_typed_value(node), keytype);
		bt_key key;
		tuple_cell2bt_key(tc, key);
		bt_insert(btree_root, key, acc);
	}
	catch (SednaUserException &ex)
	{
		this->err_cntr++;
	}

}
void index_cell::delete_from_index(xptr node, schema_node* accessor)
{
	xptr acc=getNodeAncestorIndirectionByScheme(node,accessor);
	try
	{
		tuple_cell tc = cast(dm_typed_value(node), keytype);
		bt_key key;
		tuple_cell2bt_key(tc, key);
		bt_delete(btree_root, key, acc);
	}
	catch (SednaUserException &ex)
	{
		this->err_cntr--;
	}

}
void index_cell::delete_from_index(xptr node,const char* value, int size, schema_node* accessor)
{
	xptr acc=getNodeAncestorIndirectionByScheme(node,accessor);
	char* z =new char[size+1];
	memcpy(z,value,size);
	z[size]='\0';
	try
	{
		tuple_cell tc = cast(tuple_cell::atomic_deep(xs_string,z), keytype);
		bt_key key;
		tuple_cell2bt_key(tc, key);
		bt_delete(btree_root, key, acc);
	}
	catch (SednaUserException &ex)
	{
		this->err_cntr--;
	}

}
void index_cell::put_to_index(xptr node,const char* value, int size, schema_node* accessor)
{
	xptr acc=getNodeAncestorIndirectionByScheme(node,accessor);
	char* z =new char[size+1];
	memcpy(z,value,size);
	z[size]='\0';
	try
	{
		tuple_cell tc = cast(tuple_cell::atomic_deep(xs_string,z), keytype);
		bt_key key;
		tuple_cell2bt_key(tc, key);
		CHECKP(btree_root);
		bt_insert(btree_root, key, acc);
	}
	catch (SednaUserException &ex)
	{
		this->err_cntr++;
	}
}
