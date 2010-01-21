/*
 * File:  PPCreateFtIndex.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>
 
#include "common/sedna.h"

#include "tr/executor/root/PPCreateFtIndex.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/executor/base/xsd.h"


ft_index_type str2index_type(const char *str)
{
	if (!strcmp(str, "xml"))
		return  ft_xml;
	else if (!strcmp(str, "xml-hl"))
		return  ft_xml_hl;
	else if (!strcmp(str, "string-value"))
		return ft_string_value;
	else if (!strcmp(str, "delimited-value"))
		return ft_delimited_value;
	else if (!strcmp(str, "customized-value"))
		return ft_customized_value;
	else
		throw USER_EXCEPTION2(SE1071, "unknown full-text index type");
}

ft_index_template_t *make_cust_rules_vector(PPOpIn *cust_rules, dynamic_context *cxt)
{
	tuple t(1);
	ft_index_template_t * res = se_new ft_index_template_t();
	while (1)
	{
		tuple_cell tc;
		cust_rules->op->next(t);
		if (t.is_eos())
			break;

		tc = t.cells[0];
		if (!tc.is_atomic() || !is_string_type(tc.get_atomic_type()))
			throw USER_EXCEPTION(SE1071);
		tc = tuple_cell::make_sure_light_atomic(tc);
		const char *qname = tc.get_str_mem();

		char* prefix=NULL;
		separateLocalAndPrefix(prefix,qname);
		xmlns_ptr ns=NULL_XMLNS;
		if (prefix!=NULL)
		{
			ns=cxt->st_cxt->get_xmlns_by_prefix(prefix);
			delete prefix;
		}
		char* name = se_new char[strlen(qname)+1];
		strcpy(name, qname);
		std::pair<xmlns_ptr, char*> tag(ns, name);

		cust_rules->op->next(t);
		if (t.is_eos())
			throw USER_EXCEPTION(SE1071);

		tc = t.cells[0];
		if (!tc.is_atomic() || !is_string_type(tc.get_atomic_type()))
			throw USER_EXCEPTION(SE1071);
		tc = tuple_cell::make_sure_light_atomic(tc);

		const char *index_type = tc.get_str_mem();

		ft_index_type itype = str2index_type(index_type);

		res->push_back(ft_index_pair_t(tag, itype));
	}
	return res;
}

void delete_cust_rules_vector(ft_index_template_t* &v)
{
	ft_index_template_t::iterator it;
	for (it = v->begin(); it < v->end(); it++)
	{
		delete it->first.second;
	}
	delete v;
	v = NULL;
}





PPCreateFtIndex::PPCreateFtIndex(PathExpr *_object_path_,
                                 const char *_index_type_,
                                 PathExprRoot _root_,
                                 PPOpIn _index_name_,
                                 PPOpIn _cust_rules_,
                                 dynamic_context *_cxt_) :
                                                        object_path(_object_path_),
                                                        cust_rules(_cust_rules_),
                                                        root(_root_),
                                                        index_name(_index_name_),
                                                        cxt(_cxt_)
{
	if (_index_type_[0] == '!')
	{
		this->index_impl = ft_ind_native;
		_index_type_ = _index_type_ + 1;
	}
	else
		this->index_impl = ft_ind_dtsearch;
	index_type = str2index_type(_index_type_);
}

PPCreateFtIndex::PPCreateFtIndex(PathExpr *_object_path_,
                                 const char *_index_type_,
                                 PathExprRoot _root_,
                                 PPOpIn _index_name_,
                                 dynamic_context *_cxt_) :
                                                        object_path(_object_path_),
                                                        root(_root_),
                                                        index_name(_index_name_),
                                                        cxt(_cxt_)
{
	if (_index_type_[0] == '!')
	{
		this->index_impl = ft_ind_native;
		_index_type_ = _index_type_ + 1;
	}
	else
		this->index_impl = ft_ind_dtsearch;
	index_type = str2index_type(_index_type_);
}

PPCreateFtIndex::~PPCreateFtIndex()
{
    delete index_name.op;
    index_name.op = NULL;
    if (cust_rules.op)
    {
        delete cust_rules.op;
        cust_rules.op = NULL;
    }

    root.release();
    
    delete cxt;
    cxt = NULL;
}

void PPCreateFtIndex::open()
{
    dynamic_context::global_variables_open();
    index_name.op->open();
	if (cust_rules.op)
		cust_rules.op->open();
    root.open();
}

void PPCreateFtIndex::close()
{
    index_name.op->close();
	if (cust_rules.op)
		cust_rules.op->close();
    root.close();
    dynamic_context::global_variables_close();
}

void PPCreateFtIndex::accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    index_name.op->accept(v);
    if (cust_rules.op)
		cust_rules.op->accept(v);
    if(root.get_operation().op != NULL)
    {
        root.get_operation().op->accept(v);
    }
    v.pop();
}

void PPCreateFtIndex::execute()
{
    /* Determine index name */
    tuple_cell tc = get_name_from_PPOpIn(index_name, "index", "create full-text index");

    /* Determine document or collection name to create index on */
    counted_ptr<db_entity> db_ent = root.get_entity("index", "create full-text index"); 

    /* Get xptr on this document or collection*/
    xptr root_obj = get_schema_node(db_ent, (std::string("Unknown document/collection passed to create full-text index: ") + db_ent->name).c_str());

    //local_lock_mrg->put_lock_on_index(tc.get_str_mem());

	ft_index_template_t * cust_rules_vec = NULL;

	if (cust_rules.op)
		cust_rules_vec = make_cust_rules_vector(&cust_rules, cxt);

	ft_index_cell_xptr ftic = create_ft_index (object_path,
				index_type,
				(doc_schema_node_xptr)root_obj,
				tc.get_str_mem(),
				db_ent->name,
				(db_ent->type == dbe_document),
				cust_rules_vec,
				false,
				index_impl);

	if (cust_rules_vec)
		delete_cust_rules_vector(cust_rules_vec);
}
