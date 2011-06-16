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
#include "tr/auth/auc.h"

ft_index_type str2ft_index_type(const char *str)
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

const char* ft_index_type2str(ft_index_type type)
{
    switch(type)
    {
    case ft_xml: return "xml";
    case ft_xml_hl: return "xml-hl";
    case ft_string_value: return "string-value";
    case ft_delimited_value: return "delimited-value";
    case ft_customized_value: return "customized-value";
    default: throw USER_EXCEPTION2(SE1003, "Impossible case in full text index type to string conversion");
    }
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

        xsd::QName qname = xsd::QName::createResolveContext(tc.get_str_mem(), cxt);

        cust_rules->op->next(t);
        if (t.is_eos())
            throw USER_EXCEPTION(SE1071);

        tc = t.cells[0];
        if (!tc.is_atomic() || !is_string_type(tc.get_atomic_type()))
            throw USER_EXCEPTION(SE1071);
        tc = tuple_cell::make_sure_light_atomic(tc);

        const char *index_type = tc.get_str_mem();

        ft_index_type itype = str2ft_index_type(index_type);

        res->push_back(ft_index_pair_t(qname, itype));
    }
    return res;
}



PPCreateFtIndex::PPCreateFtIndex(xpath::PathExpression *_object_path_,
                                 const char *_index_type_,
                                 xpath::PathExprRoot _root_,
                                 PPOpIn _index_name_,
                                 PPOpIn _options_,
                                 PPOpIn _cust_rules_,
                                 dynamic_context *_cxt_) : PPUpdate("PPCreateFtIndex"),
                                                           object_path(_object_path_),
                                                           cust_rules(_cust_rules_),
                                                           root(_root_),
                                                           index_name(_index_name_),
                                                           options(_options_),
                                                           cxt(_cxt_)
{
	index_type = str2ft_index_type(_index_type_);
}

PPCreateFtIndex::~PPCreateFtIndex()
{
    delete index_name.op;
    index_name.op = NULL;
	if (options.op)
	{
		delete options.op;
		options.op = NULL;
	}
    if (cust_rules.op)
    {
        delete cust_rules.op;
        cust_rules.op = NULL;
    }

    root.release();
    
    delete cxt;
    cxt = NULL;
}

void PPCreateFtIndex::do_open()
{
    cxt->global_variables_open();
    index_name.op->open();
	if (options.op != NULL)
		options.op->open();
	if (cust_rules.op != NULL)
		cust_rules.op->open();
    root.open();
}

void PPCreateFtIndex::do_close()
{
    index_name.op->close();
	if (options.op != NULL)
		options.op->close();
	if (cust_rules.op)
		cust_rules.op->close();
    root.close();
    cxt->global_variables_close();
}

void PPCreateFtIndex::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    index_name.op->accept(v);
    if(root.get_operation().op != NULL)
        root.get_operation().op->accept(v);
    if (options.op != NULL)
		options.op->accept(v);
    if (cust_rules.op != NULL)
		cust_rules.op->accept(v);
    v.pop();
}

void PPCreateFtIndex::do_execute()
{
    /* Determine index name */
    tuple_cell tc = get_name_from_PPOpIn(index_name, "index", "create full-text index");
	tuple_cell options_tc;
	const char *options_str = NULL;
	if (options.op != NULL)
	{
		//FIXME: options is not a name, but besides wrong error messages get_name_from_PPOpIn is fine
		options_tc = get_name_from_PPOpIn(options, "options", "create full-text index");
		options_str = options_tc.get_str_mem();
	}

    /* Determine document or collection name to create index on */
    counted_ptr<db_entity> db_ent = root.get_entity("index", "create full-text index"); 

    /* Get xptr on this document or collection*/
    xptr root_obj = get_schema_node(db_ent, (std::string("Unknown document/collection passed to create full-text index: ") + db_ent->name).c_str());

    auth_for_create_ftindex(tc.get_str_mem(), db_ent->name, db_ent->type == dbe_collection);

    /* TODO: Review lock for full-text index!
    local_lock_mrg->put_lock_on_index(tc.get_str_mem()); */

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
				options_str);

	if (cust_rules_vec)
		delete_cust_rules_vector(cust_rules_vec);
}
