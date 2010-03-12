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
#include "tr/cat/catstore.h"


using namespace std;

// USemaphore ft_index_sem;//SEMAPHOR!!!

static bool ft_index_initialized = false;

//inits metadata library
void ft_index_on_session_begin()
{
    ft_index_initialized = true;
}

void ft_index_on_session_end()
{
    if (!ft_index_initialized) return;
    ft_index_initialized = false;
}

void delete_ft_custom_tree (ft_custom_tree_t * custom_tree)
{
    ft_custom_tree_t::sedna_rbtree_entry * tmp = custom_tree->rb_minimum(custom_tree->root);
    while (tmp!=NULL)
    {
        ft_custom_cell* mdc=tmp->obj;
        if (mdc->local) cat_free(mdc->local);
        delete mdc;
        tmp->obj=NULL;
        tmp=custom_tree->rb_successor(tmp);
    }
    ft_custom_tree_t::sset_free(custom_tree);
}


void ft_index_cell_object::serialize_data(se_simplestream &stream)
{
    uint8_t marker = 1;

    cs_set_hint(schemaroot);

    stream.write_string(index_title);
    stream.write(&ftype, sizeof(ft_index_type));
    stream.write(&impl, sizeof(ft_index_type));
    stream.write(&ft_data, sizeof(ft_idx_data_t));

    std::ostringstream obj_str(std::ios::out | std::ios::binary);
    PathExpr2lr(object, obj_str);
    stream.write_string(obj_str.str().c_str());

    stream.write_string(doc_name);
    stream.write(&is_doc, sizeof(bool));

    stream.write(&schemaroot, sizeof(schema_node_xptr));
    stream.write(&serial_root, sizeof(xptr));
    stream.write(&pstr_sequence, sizeof(xptr));

    if (custom_tree != NULL)
    {
        ft_custom_tree_t::sedna_rbtree_entry * tmp = custom_tree->rb_minimum(custom_tree->root);

        while (tmp != NULL)
        {
            stream.write(&marker, sizeof(uint8_t));
            stream.write(&tmp->obj->cm, sizeof(ft_index_type));
            stream.write_string(tmp->obj->local);
            stream.write(&tmp->obj->ns_pers, sizeof(xmlns_ptr_pers));

            tmp = custom_tree->rb_successor(tmp);
        }
    }

    marker = 0;
    stream.write(&marker, sizeof(uint8_t));
}

void ft_index_cell_object::deserialize_data(se_simplestream &stream)
{
    uint8_t marker = 1;
    xmlns_ptr_pers ct_ns;
    char* ct_local;
    ft_index_type ct_cm;
    char* obj_str;

    index_title = (char *) cat_malloc(this, stream.read_string_len());
    stream.read_string(SSTREAM_SAVED_LENGTH, index_title);
    stream.read(&ftype, sizeof(ft_index_type));
    stream.read(&impl, sizeof(ft_index_type));
    stream.read(&ft_data, sizeof(ft_idx_data_t));

    obj_str = (char *) cat_malloc_context(CATALOG_COMMON_CONTEXT, stream.read_string_len());
    stream.read_string(SSTREAM_SAVED_LENGTH, obj_str);
    object = lr2PathExpr(NULL, obj_str, pe_catalog_aspace);
    cat_free(obj_str);

    doc_name = (char *) cat_malloc(this, stream.read_string_len());
    stream.read_string(SSTREAM_SAVED_LENGTH, doc_name);
    stream.read(&is_doc, sizeof(bool));

    stream.read(&schemaroot, sizeof(schema_node_xptr));
    stream.read(&serial_root, sizeof(xptr));
    stream.read(&pstr_sequence, sizeof(xptr));

    stream.read(&marker, sizeof(uint8_t));

    custom_tree = (marker == 0) ? NULL : ft_custom_tree_t::init();
    while (marker != 0) {
        stream.read(&ct_cm, sizeof(ft_index_type));
        ct_local = new char[stream.read_string_len()];
        stream.read_string(SSTREAM_SAVED_LENGTH, ct_local);
        stream.read(&ct_ns, sizeof(xmlns_ptr_pers));

        custom_tree->put(new ft_custom_cell(ct_ns, NULL_XMLNS, ct_local, ct_cm));
        delete [] ct_local;

        stream.read(&marker, sizeof(uint8_t));
    }
}

void ft_index_cell_object::drop()
{
    schemaroot.modify()->delete_ftindex(p_object);
    catalog_delete_name(catobj_ft_indicies, this->index_title);

    cs_free(p_object);

    catalog_delete_object(this);
}

bool ft_index_cell_object::fits_to_index(schema_node_cptr snode)
{
	t_scmnodes res;
	t_scmnodes objs=execute_abs_path_expr(snode->root,object,NULL,NULL);
	t_scmnodes::iterator it=objs.begin();
	while (it!=objs.end())
	{
		if (*it==snode.ptr())		
			return true;		
		it++;
	}
	return false;
}


ft_index_cell_xptr create_ft_index(
        PathExpr *_object_path, ft_index_type _it, 
        doc_schema_node_xptr _schemaroot,
        const char * _index_title, const char* _doc_name, bool _is_doc,
        ft_index_template_t* _templ, bool just_heap, ft_index_impl _impl
    )
{
   // I. Create and fill new index cell

    if (catalog_find_name(catobj_ft_indicies, _index_title) != NULL) {
        throw USER_EXCEPTION(SE2033);
    }

    down_concurrent_micro_ops_number();
    ft_index_cell_cptr idc(ft_index_cell_object::create(_object_path, _it, _schemaroot, _index_title, _doc_name, _is_doc, _impl));
    _schemaroot.modify()->full_ft_index_list.add(idc.ptr());

    if (_it == ft_customized_value && _templ != NULL)
    {
        idc->custom_tree = ft_custom_tree_t::init();
        ft_index_template_t::iterator tmp=_templ->begin();
        while (tmp!=_templ->end())
        {
            idc->custom_tree->put(new ft_custom_cell(_schemaroot->xmlns_register(tmp->first.first), NULL_XMLNS, tmp->first.second, tmp->second));
            tmp++;
        }
    }

    // only needed for ft_ind_native impl

    ftc_index_t ftc_idx;
    if (_impl == ft_ind_native)
    {
        idc->ft_data.btree_root = bt_create(xs_string); //FIXME: moved from ft_idx_create
        ftc_idx = ftc_get_index(_index_title, idc->ft_data.btree_root);
    }

    hl_logical_log_ft_index(_object_path, _it, _index_title, _doc_name, _is_doc, idc->custom_tree, true);

    // ALGORITHM: indexing data
    //II. Execute abs path (object_path) on the desriptive schema
    t_scmnodes sobj = execute_abs_path_expr(_schemaroot, _object_path,NULL,NULL);
    //III. For each schema node found (sn_obj)

    std::vector<xptr> start_nodes;
    for (int i = 0; i < sobj.size(); i++)
    {
        xptr blk;
        sobj[i].modify()->ft_index_list.add(idc.ptr()); // just_heap modified because this must be recovered (AK)
        RECOVERY_CRASH;
        if (!just_heap)
        {
            blk = getNonemptyBlockLookFore(sobj[i]->bblk);
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
		switch (_impl)
		{
#ifdef SE_ENABLE_DTSEARCH
		case ft_ind_dtsearch:
			{
			SednaIndexJob sij(&*idc);
			sij.create_index(&start_nodes);
			break;
			}
#endif
		case ft_ind_native:
		{
			idc->init_serial_tree();
			ft_idx_create(&start_nodes, &idc->ft_data, idc->ftype, idc->custom_tree, ftc_idx);
			break;
		}
		default:
			throw USER_EXCEPTION2(SE1002, "unknow full-text index implementation"); //TODO: check it's ok to throw here
		}
    }

    return idc.ptr();
}

void delete_ft_index (const char *index_title, bool just_heap)
{
    ft_index_cell_cptr idc(index_title, true);

    if (idc.found()) {
        down_concurrent_micro_ops_number();
        hl_logical_log_ft_index(idc->object, idc->ftype, idc->index_title, idc->doc_name, idc->is_doc, idc->custom_tree, false);

        if (!just_heap) //FIXME: mb move out of down_concurrent_micro_ops_number() block
        {
            switch (idc->impl)
            {
    #ifdef SE_ENABLE_DTSEARCH
            case ft_ind_dtsearch:
                {
                    SednaIndexJob sij(&*idc);
                    sij.clear_index();
                    break;
                }
    #endif
            case ft_ind_native:
                {
                    ft_idx_delete(&idc->ft_data);
					idc->destroy_serial_tree();
                    break;
                }
            default:
                throw SYSTEM_EXCEPTION("unknow full-text index implementation");
            }
        }

        idc->drop();
        up_concurrent_micro_ops_number();
    }
}


ft_index_cell_xptr find_ft_index(const char* title, ftc_index_t *ftc_idx)
{
    ft_index_cell_cptr idc(title);
	if (idc.found())
	{
		if (ftc_idx && idc->impl == ft_ind_native)
			*ftc_idx = ftc_get_index(title, idc->ft_data.btree_root);
		return idc.ptr();
	}
	else
		return XNULL;
}

void ft_index_cell_object::update_index(xptr_sequence* upserted)
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
void ft_index_cell_object::insert_to_index(xptr_sequence* upserted)
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
void ft_index_cell_object::delete_from_index(xptr_sequence* deleted)
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
void ft_index_cell_object::change_index(xptr_sequence* inserted,xptr_sequence* updated,xptr_sequence* deleted)
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
	case ft_ind_native:
		{
			//TODO!
		}
	default:
		throw USER_EXCEPTION2(SE1002, "unknow full-text index implementation"); //TODO: check it's ok to trow here
	}
}


xptr ft_index_cell_object::put_buf_to_pstr(op_str_buf& tbuf)
{
	xptr res = XNULL;
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
		xptr pstr = pstr_long_create_str2(true, tbuf.get_ptr_to_text(), tbuf.get_size(), tbuf.get_type());
		return pstr;
	}
	U_ASSERT(false);
}


void ft_index_cell_object::remove_from_pstr(doc_serial_header& head )
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

void ft_index_cell_object::init_serial_tree()
{
	//1. create b-tree
	this->serial_root=bt_create(xs_integer);
	
	//2. create pstr block
	pstr_sequence=
		pstr_create_blk(true);
}
void ft_index_cell_object::destroy_serial_tree()
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
doc_serial_header ft_index_cell_object::serial_put (xptr& node, op_str_buf& tbuf)
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
void ft_index_cell_object::serial_remove (xptr& node)
{
	//1. find header in b-tree
	bt_key key;
	key.setnew(*((int64_t*)&node));
	
	bt_cursor_tmpl<doc_serial_header> cursor=bt_find_tmpl<doc_serial_header>(this->serial_root, key);
	if (cursor.is_null())
	{	U_ASSERT(false); return;}
	doc_serial_header head=cursor.bt_next_obj();
	//2. remove header from b-tree
	bt_delete_tmpl<doc_serial_header>(this->serial_root,key);
	//4. remove data from  pstr
	remove_from_pstr(head);
	
}
doc_serial_header ft_index_cell_object::serial_get (xptr& node)
{
	//1. find header in b-tree
	bt_key key;
	key.setnew(*((int64_t*)&node));
	
	bt_cursor_tmpl<doc_serial_header> cursor=
		bt_find_tmpl<doc_serial_header>(this->serial_root, key);
	if (cursor.is_null())
	{	U_ASSERT(false); return doc_serial_header();}
	doc_serial_header head=cursor.bt_next_obj();
	//2. find pstr and copy to tbuf
		
	return head;
}
doc_serial_header ft_index_cell_object::serial_update (xptr& node, op_str_buf& tbuf)
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
