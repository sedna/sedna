/*
 * File:  ft_index_data.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"

#include "tr/ft/ft_index_data.h"
#include "common/xptr.h"
#include "tr/vmm/vmm.h"
#include "tr/executor/base/tuple.h"
#ifdef SE_ENABLE_DTSEARCH
#include "tr/ft/FTindex.h"
#endif
#include "tr/ft/ft_index.h"
#include "tr/log/log.h"
#include "tr/structures/schema.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/btree/btree.h"
#include "tr/cat/catstore.h"
#include "common/u/uutils.h"
#include "tr/strings/opt_parser.h"
#include "tr/structures/nodeutils.h"
#include "tr/crmutils/ftserializer.h"


using namespace std;

// USemaphore ft_index_sem;//SEMAPHOR!!!

static bool ft_index_initialized = false;

//inits metadata library
void ft_index_on_session_begin()
{
    ft_index_initialized = true;
    FTSerializer::initSharedInstance();
}

void ft_index_on_session_end()
{
    if (!ft_index_initialized) return;
    FTSerializer::disposeSharedInstance();
    ft_index_initialized = false;
}

void delete_ft_custom_tree (ft_custom_tree_t * custom_tree)
{
    ft_custom_tree_t::sedna_rbtree_entry * tmp = custom_tree->rb_minimum(custom_tree->root);
    while (tmp!=NULL)
    {
        ft_custom_cell* mdc=tmp->obj;
        if (mdc->local) delete [] mdc->local;
        delete mdc;
        tmp->obj=NULL;
        tmp=custom_tree->rb_successor(tmp);
    }
    ft_custom_tree_t::sset_free(custom_tree);
}

void delete_cust_rules_vector(ft_index_template_t* &v)
{
    ft_index_template_t::iterator it;
    delete v;
    v = NULL;
}


void ft_index_cell_object::serialize_data(se_simplestream &stream)
{
    uint8_t marker = 1;

    cs_set_hint(schemaroot);

    stream.write_string(index_title);
    stream.write(&ftype, sizeof(ft_index_type));
    stream.write(&impl, sizeof(ft_index_impl));
    stream.write_string(stemming);
    stream.write(&fts_data, sizeof(struct FtsData));

    stream.write_string(object->toLRString().c_str());

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
            std::string qname = tmp->obj->getQName().toExpatQName();
            stream.write_string(qname.c_str());
            tmp = custom_tree->rb_successor(tmp);
        }
    }

    marker = 0;
    stream.write(&marker, sizeof(uint8_t));
}

void ft_index_cell_object::deserialize_data(se_simplestream &stream)
{
    uint8_t marker = 1;
    char* ct_name;
    ft_index_type ct_cm;
    char* obj_str = NULL;
    se_size_t len;

    setDefaultSpace(catalog_space_base);

    index_title = (char *) cat_malloc(this, stream.read_string_len());
    stream.read_string(SSTREAM_SAVED_LENGTH, index_title);
    stream.read(&ftype, sizeof(ft_index_type));
    stream.read(&impl, sizeof(ft_index_type));
    stemming = (char *) cat_malloc(this, stream.read_string_len());
    stream.read_string(SSTREAM_SAVED_LENGTH, stemming);
    stream.read(&fts_data, sizeof(struct FtsData));

    if ((len = stream.read_string_len()) != 0)
        obj_str = (char *)malloc(len);
    stream.read_string(SSTREAM_SAVED_LENGTH, obj_str);
    object = new xpath::PathExpression(obj_str, NULL);
    free(obj_str);

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
        ct_name = new char[stream.read_string_len()];
        stream.read_string(SSTREAM_SAVED_LENGTH, ct_name);

        custom_tree->put(new ft_custom_cell(xsd::QName::bulkloadParse(ct_name), ct_cm));
        delete [] ct_name;

        stream.read(&marker, sizeof(uint8_t));
    }

    popDefaultSpace();
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
    t_scmnodes objs;
    executePathExpression(snode->root, *object, &objs, NULL, NULL);
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
        xpath::PathExpression *_object_path, ft_index_type _it,
        doc_schema_node_xptr _schemaroot,
        const char * _index_title, const char* _doc_name, bool _is_doc,
        ft_index_template_t* _templ, bool rcv, const char *options
    )
{
   // I. Create and fill new index cell

    if (catalog_find_name(catobj_ft_indicies, _index_title) != NULL) {
        throw USER_EXCEPTION(SE2033);
    }

    down_concurrent_micro_ops_number();
    ft_index_cell_cptr idc(ft_index_cell_object::create(_object_path, _it, _schemaroot, _index_title, _doc_name, _is_doc, options));
    _schemaroot.modify()->full_ft_index_list->add(idc.ptr());

    if (_it == ft_customized_value && _templ != NULL)
    {
        idc->custom_tree = ft_custom_tree_t::init();
        ft_index_template_t::iterator tmp=_templ->begin();
        while (tmp!=_templ->end())
        {
            _schemaroot->xmlns_register(tmp->first.getXmlNs());
            idc->custom_tree->put(new ft_custom_cell(tmp->first, tmp->second));
            tmp++;
        }
    }

    // only needed for ft_ind_native impl (initialized with NULL to avoid GCC warning on release build)
    ftc_index_t ftc_idx = NULL;
    if (idc->impl == ft_ind_native)
    {
        fts_create(&idc->fts_data);
        ftc_idx = ftc_get_index(_index_title, &idc->fts_data);
    }

    hl_logical_log_ft_index(_object_path, _it, _index_title, _doc_name, options ? options:"", _is_doc, idc->custom_tree, true);

    // ALGORITHM: indexing data
    //II. Execute abs path (object_path) on the desriptive schema
    t_scmnodes sobj;
    executePathExpression(_schemaroot, *_object_path, &sobj, NULL, NULL);
    //III. For each schema node found (sn_obj)

    std::vector<xptr> start_nodes;
    for (unsigned int i = 0; i < sobj.size(); i++)
    {
        xptr blk;
        sobj[i].modify()->ft_index_list->add(idc.ptr()); // just_heap modified because this must be recovered (AK)
        RECOVERY_CRASH;
        if (!rcv || idc->impl != ft_ind_dtsearch)
        {
            blk = getNonemptyBlockLookFore(sobj[i]->bblk);
            if (blk!=XNULL)
            {
                CHECKP(blk);
                start_nodes.push_back(getFirstBlockNode(blk));
            }
        }
    }

    up_concurrent_micro_ops_number();

    if (!rcv || idc->impl != ft_ind_dtsearch) // moved here because ph ft_index info must be recovered fully (AK)
    {
        // ft_index recovery should take the responsibility here
		switch (idc->impl)
		{
#ifdef SE_ENABLE_DTSEARCH
		case ft_ind_dtsearch:
			{
			SednaIndexJob sij(&*idc);
			sij.create_index(&start_nodes);
			break;
			}
#else
		case ft_ind_dtsearch:
		{
			throw USER_EXCEPTION2(SE1002, "dtSearch support is disabled"); //TODO: check it's ok to throw here
		}
#endif
		case ft_ind_native:
		{
			//idc->init_serial_tree();
			//TODO: if stemmer==NULL and stemtype==both, all words are indexed w/o stemming and exact versions of words
			//      can't be searched without '~'; need to throw exception here or change stem_type to none.

			op_str_buf in_buf;

			for (std::vector<xptr>::iterator it = start_nodes.begin(); it != start_nodes.end(); ++it)
			{
				xptr tmp = *it;
				while (tmp != XNULL)
				{
					CHECKP(tmp);
					xptr tmp_indir = nodeGetIndirection(tmp);
					//TODO: see whether rewriting this to serialize directly to text parser (expat?), without writing to buffer first is better.
					in_buf.clear();
					FTSerializer::getSharedInstance()->printNodeToBuffer(tmp, &in_buf, idc->ftype, idc->custom_tree);
					//idc->serial_put(tmp, tmp_indir, in_buf);
					ft_index_update(ft_insert, tmp_indir, &in_buf, ftc_idx);

					tmp=getNextDescriptorOfSameSort(tmp);
				}
			}

			break;
		}
		default:
			throw USER_EXCEPTION2(SE1002, "unknow full-text index implementation"); //TODO: check it's ok to throw here
		}
    }

    return idc.ptr();
}
//TODO: usage of these just_heap options should be described somewhere
void delete_ft_index (const char *index_title, bool just_heap)
{
    ft_index_cell_cptr idc(index_title, true);

    if (idc.found()) {
		op_str_buf options_buf;
        down_concurrent_micro_ops_number();
		idc->write_options_str(&options_buf);
		hl_logical_log_ft_index(idc->object, idc->ftype, idc->index_title, idc->doc_name, options_buf.c_str(), idc->is_doc, idc->custom_tree, false);

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
                    ft_idx_delete(&idc->fts_data);
					//idc->destroy_serial_tree();
                    break;
                }
            default:
                throw SYSTEM_EXCEPTION("unknow full-text index implementation");
            }
        }
        if (idc->impl == ft_ind_native)
			ftc_drop_index(idc->index_title);

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
			*ftc_idx = ftc_get_index(title, &idc->fts_data);
		return idc.ptr();
	}
	else
		return XNULL;
}


static void ft_update_seq(xptr_sequence *seq, ft_index_cell_object *idc, ftc_index_t ftc_idx, ft_index_op_t op)
{
	op_str_buf in_buf;
	xptr_sequence::iterator it = seq->begin();

	while (it!=seq->end())
	{
		xptr node_indir = *it++;

	    //TODO: see whether rewriting this to serialize directly to text parser (expat?), without writing to buffer first is better.
		if (op == ft_delete || op == ft_update)
		{
			/*in_buf.clear();
			idc->serial_get(node_indir).serialize_to_buf(&in_buf);
			ft_index_update(ft_delete, node_indir, &in_buf, &idc->fts_data, ftc_idx);
			if (op == ft_delete)
				idc->serial_remove(node_indir);*/
			ft_index_delete_doc(ftc_idx, node_indir);
		}

		if (op == ft_update || op == ft_insert)
		{
			const xptr node = indirectionDereferenceCP(node_indir);
			U_ASSERT(node != XNULL);

			in_buf.clear();
			FTSerializer::getSharedInstance()->printNodeToBuffer(node, &in_buf, idc->ftype, idc->custom_tree);
			/*
			if (op == ft_update)
				idc->serial_update(node, node_indir, in_buf);
			else //ft_insert
				idc->serial_put(node, node_indir, in_buf);
			*/
			ft_index_update(ft_insert, node_indir, &in_buf, ftc_idx);
		}
	}
}

const char * ft_index_cell_object::impl_str()
{
	switch (this->impl)
	{
	case ft_ind_dtsearch:
		return "dtsearch";
	case ft_ind_native:
		return "native";
	default:
		return "unknown";
	}
}
void ft_index_cell_object::parse_options(const char * options)
{
	OptionsParser op;
	op.set_str(options);

	while (op.next_opt())
	{
		if (!strcmp(op.opt_name(), "native"))
		{
			if (op.opt_value()[0] || impl != ft_ind_undefined)
				throw USER_EXCEPTION2(SE3022, "bad options for full-text index");
			impl = ft_ind_native;
		}
		else if (!strcmp(op.opt_name(), "dtsearch"))
		{
			if (op.opt_value()[0] || impl != ft_ind_undefined)
				throw USER_EXCEPTION2(SE3022, "bad options for full-text index");
			impl = ft_ind_dtsearch;
		}
		else if (!strcmp(op.opt_name(), "backend"))
		{
			if (impl != ft_ind_undefined)
				throw USER_EXCEPTION2(SE3022, "bad options for full-text index");
			if (!strcmp(op.opt_value(), "native"))
			{
				impl = ft_ind_native;
			}
			else if (!strcmp(op.opt_value(), "dtsearch"))
			{
				impl = ft_ind_dtsearch;
			}
			else
				throw USER_EXCEPTION2(SE3022, "bad options for full-text index");
		}
		else if (!strcmp(op.opt_name(), "stemming"))
		{
			if (stemming != NULL)
				throw USER_EXCEPTION2(SE3022, "bad options for full-text index");
			stemming = cat_strcpy(this, op.opt_value());
		}
		else if (!strcmp(op.opt_name(), "stemtype"))
		{
			if (!strcmp(op.opt_value(), "both"))
				fts_data.stem_type = ftst_both;
			else
				throw USER_EXCEPTION2(SE3022, "bad options for full-text index");
		}
	}
}

void ft_index_cell_object::write_options_str(op_str_buf *buf)
{
	buf->append("backend=");
	buf->append(impl_str());
	if (stemming != NULL && stemming[0] != '\x0')
		OptionsParser::append_option(buf, "stemming", stemming);
	switch (fts_data.stem_type)
	{
	case ftst_default:
		break;
	case ftst_both:
		OptionsParser::append_option(buf, "stemtype", "both");
		break;
	default:
		throw USER_EXCEPTION2(SE3022, "unknown stem_type");
	}
}

void ft_index_cell_object::serialize_info(xptr left_sib, xptr parent)
{
	char sbuf[64];
	switch (this->impl)
	{
	case ft_ind_native:
		{
        if (left_sib == XNULL)
            left_sib = insert_element_i(XNULL,XNULL,parent,"partitions",xs_untyped,NULL_XMLNS);
        else
            left_sib = insert_element_i(left_sib,XNULL,XNULL,"partitions",xs_untyped,NULL_XMLNS);

		u_itoa(this->fts_data.npartitions, sbuf, 10);
		xptr node = insert_attribute_i(XNULL,XNULL,left_sib,"count",xs_untypedAtomic,sbuf,strlen(sbuf),NULL_XMLNS);
		xptr left_p = XNULL;
		int64_t total_sblob_blocks = 0;
		int64_t total_voc_blocks = 0;
		int64_t total_dellist_blocks = 0;
		for (int i = 0; i < this->fts_data.npartitions; i++)
		{
			if (left_p == XNULL)
				left_p = insert_element_i(XNULL,XNULL,left_sib,"partition",xs_untyped,NULL_XMLNS);
			else
				left_p = insert_element_i(left_p,XNULL,XNULL,"partition",xs_untyped,NULL_XMLNS);

			u_itoa(i, sbuf, 10);
			node = insert_attribute_i(XNULL,XNULL,left_p,"ind",xs_untypedAtomic,sbuf,strlen(sbuf),NULL_XMLNS);
			u_itoa(this->fts_data.partitions[i].sblob_blocks, sbuf, 10);
			node = insert_attribute_i(node,XNULL,XNULL,"sblob_blocks",xs_untypedAtomic,sbuf,strlen(sbuf),NULL_XMLNS);
			const int voc_blocks = bt_walk_nodes(this->fts_data.partitions[i].voc_btree_root);
			u_itoa(voc_blocks, sbuf, 10);
			node = insert_attribute_i(node,XNULL,XNULL,"voc_blocks",xs_untypedAtomic,sbuf,strlen(sbuf),NULL_XMLNS);
			const int dellist_blocks = bt_walk_nodes(this->fts_data.partitions[i].del_list);
			u_itoa(dellist_blocks, sbuf, 10);
			node = insert_attribute_i(node,XNULL,XNULL,"del_list_blocks",xs_untypedAtomic,sbuf,strlen(sbuf),NULL_XMLNS);

			total_sblob_blocks += this->fts_data.partitions[i].sblob_blocks;
			total_voc_blocks += voc_blocks;
			total_dellist_blocks += dellist_blocks;
		}
		if (left_p == XNULL)
			left_p = insert_element_i(XNULL,XNULL,left_sib,"total",xs_untyped,NULL_XMLNS);
		else
			left_p = insert_element_i(left_p,XNULL,XNULL,"total",xs_untyped,NULL_XMLNS);
		u_i64toa(total_sblob_blocks+total_voc_blocks+total_dellist_blocks, sbuf, 10);
		node = insert_attribute_i(XNULL,XNULL,left_p,"blocks",xs_untypedAtomic,sbuf,strlen(sbuf),NULL_XMLNS);
		u_i64toa(total_sblob_blocks, sbuf, 10);
		node = insert_attribute_i(XNULL,XNULL,left_p,"sblob_blocks",xs_untypedAtomic,sbuf,strlen(sbuf),NULL_XMLNS);
		u_i64toa(total_voc_blocks, sbuf, 10);
		node = insert_attribute_i(XNULL,XNULL,left_p,"voc_blocks",xs_untypedAtomic,sbuf,strlen(sbuf),NULL_XMLNS);
		u_i64toa(total_dellist_blocks, sbuf, 10);
		node = insert_attribute_i(XNULL,XNULL,left_p,"del_list_blocks",xs_untypedAtomic,sbuf,strlen(sbuf),NULL_XMLNS);

		return;
		}
	}
}

void ft_index_cell_object::update_index(update_history *h)
{
	xptr_sequence *inserted, *updated, *deleted;
	h->get_update_sequences(&inserted, &updated, &deleted);
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
			ftc_index_t ftc_idx = ftc_get_index(this->index_title, &this->fts_data);
			ft_update_seq(deleted, this, ftc_idx, ft_delete);
			ft_update_seq(updated, this, ftc_idx, ft_update);
			ft_update_seq(inserted, this, ftc_idx, ft_insert);
			break;
		}
	default:
		h->free_update_sequences(inserted, updated, deleted);
		throw USER_EXCEPTION2(SE1002, "unknow full-text index implementation"); //TODO: check it's ok to trow here
	}
	h->free_update_sequences(inserted, updated, deleted);
}

/*
xptr ft_index_cell_object::put_buf_to_pstr(op_str_buf& tbuf)
{
	xptr res = XNULL;
	str_off_t sz=tbuf.get_size();

	if (sz<=PSTRMAXSIZE)
	{
		const char* mem=tbuf.c_str();
		res=pstr_do_allocate(this->pstr_sequence,mem,(int)sz);
		if (res==XNULL)
		{
			xptr new_blk = pstr_create_blk(true);
			res= pstr_do_allocate(new_blk, mem, (int)sz);
			this->pstr_sequence = new_blk;
		}
		return res;
	}
	else
	{
		xptr pstr = pstr_long_create_str2(true, text_source_strbuf(&tbuf));
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
			head.ptr, (int)head.length, !same_block(this->pstr_sequence, head.ptr));
	}
	else
	{
		//long pstr
		pstr_long_delete_str2(head.ptr);
	}
}
*/
/*
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
doc_serial_header ft_index_cell_object::serial_put (xptr& node, xptr &node_indir, op_str_buf& tbuf)
{

	//1. serialize node to buf and fill serial header
	FTSerializer::getSharedInstance()->printNodeToBuffer(node, tbuf, this->ftype, this->custom_tree);
	//2. put buf to pstr
	doc_serial_header dsh(tbuf.get_size(),put_buf_to_pstr(tbuf));
	//3. put header to b-tree
	bt_key key;
	key.setnew(*((int64_t *)&node_indir));
	bt_insert_tmpl<doc_serial_header>(this->serial_root,key,dsh);
	//4. return header
	return dsh;
}
void ft_index_cell_object::serial_remove (xptr& node_indir)
{
	//1. find header in b-tree
	bt_key key;
	key.setnew(*((int64_t*)&node_indir));

	bt_cursor_tmpl<doc_serial_header> cursor=bt_find_tmpl<doc_serial_header>(this->serial_root, key);
	if (cursor.is_null())
	{	U_ASSERT(false); return;}
	doc_serial_header head=cursor.bt_next_obj();
	//2. remove header from b-tree
	bt_delete_tmpl<doc_serial_header>(this->serial_root,key);
	//4. remove data from  pstr
	remove_from_pstr(head);

}
doc_serial_header ft_index_cell_object::serial_get (xptr& node_indir)
{
	//1. find header in b-tree
	bt_key key;
	key.setnew(*((int64_t*)&node_indir));

	bt_cursor_tmpl<doc_serial_header> cursor=
		bt_find_tmpl<doc_serial_header>(this->serial_root, key);
	if (cursor.is_null())
	{	U_ASSERT(false); return doc_serial_header();}
	doc_serial_header head=cursor.bt_next_obj();
	//2. find pstr and copy to tbuf

	return head;
}
doc_serial_header ft_index_cell_object::serial_update (xptr& node, xptr& node_indir, op_str_buf& tbuf)
{
	serial_remove(node_indir);
	return serial_put(node, node_indir, tbuf);
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
		parse(data,(int)length,&dp);
	}
	else
	{
		pstr_long_feed2(this->ptr,doc_serial_header::parse,&dp);
	}
	//3. return header
}
void doc_serial_header::serialize_to_buf(op_str_buf *buf)
{
	if (this->length<=PSTRMAXSIZE)
	{
		CHECKP(this->ptr);
		shft shift= *((shft*)XADDR(this->ptr));
		tuple_cell tc = tuple_cell::atomic_pstr(xs_string, this->length, BLOCKXPTR(this->ptr) + shift);
		buf->append(tc);
	}
	else
	{
		tuple_cell tc = tuple_cell::atomic_pstr(xs_string, this->length, this->ptr);
		buf->append(tc);
	}
}
*/
