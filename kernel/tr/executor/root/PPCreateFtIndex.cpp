/*
 * File:  PPCreateFtIndex.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "sedna.h"

#include "PPCreateFtIndex.h"
#include "PPUtils.h"
#include "PPBase.h"


ft_index_type str2index_type(const char *str)
{
	if (!strcmp(str, "xml"))
		return  ft_xml;
	else if (!strcmp(str, "string-value"))
		return ft_string_value;
	else if (!strcmp(str, "delimited-value"))
		return ft_delimited_value;
	else if (!strcmp(str, "customized-value"))
		return ft_customized_value;
	else
		throw USER_EXCEPTION2(SE1071, "unknown full-text index type");
}

PPCreateFtIndex::PPCreateFtIndex(PathExpr *_object_path_,
                  char *_index_type_,
                  counted_ptr<db_entity> _db_ent_,
                  PPOpIn _index_name_,
				  PPOpIn _cust_rules_) :	object_path(_object_path_),
                                            db_ent(_db_ent_),
                                            index_name(_index_name_),
											cust_rules(_cust_rules_)
{
	index_type = str2index_type(_index_type_);
}
PPCreateFtIndex::PPCreateFtIndex(PathExpr *_object_path_,
                  char *_index_type_,
                  counted_ptr<db_entity> _db_ent_,
                  PPOpIn _index_name_) :	object_path(_object_path_),
                                            db_ent(_db_ent_),
                                            index_name(_index_name_)
{
	index_type = str2index_type(_index_type_);
}

PPCreateFtIndex::~PPCreateFtIndex()
{
    delete index_name.op;
    index_name.op = NULL;
}

void PPCreateFtIndex::open()
{
    root = get_schema_node(db_ent, "Unknown entity passed to PPCreateIndex");
    index_name.op->open();
	if (cust_rules.op)
		cust_rules.op->open();
}

void PPCreateFtIndex::close()
{
    index_name.op->close();
    root = NULL;
	if (cust_rules.op)
		cust_rules.op->close();
}


//FIXME: import it in some other way (this function is in PPConstructors.cpp)
//void separateLocalAndPrefix(NCName*& prefix,const char*& qname);
void separateLocalAndPrefix(char*& prefix,const char*& qname);
std::vector< std::pair< std::pair<xml_ns*,char*>,ft_index_type> > *make_cust_rules_vector(PPOpIn *cust_rules)
{
	tuple t(1);
	std::vector< std::pair< std::pair<xml_ns*,char*>,ft_index_type> > * res = new std::vector< std::pair< std::pair<xml_ns*,char*>,ft_index_type> >();
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
		xml_ns* ns=NULL;
		if (prefix!=NULL)
		{
			ns=tr_globals::st_ct.get_xmlns_by_prefix(prefix);
			delete prefix;			
		}
		char* name = new char[strlen(qname)+1];
		strcpy(name, qname);
		std::pair<xml_ns*,char*> tag(ns, name);
	
		cust_rules->op->next(t);
		if (t.is_eos())
			throw USER_EXCEPTION(SE1071);

		tc = t.cells[0];
		if (!tc.is_atomic() || !is_string_type(tc.get_atomic_type()))
			throw USER_EXCEPTION(SE1071);
		tc = tuple_cell::make_sure_light_atomic(tc);

		const char *index_type = tc.get_str_mem();

		ft_index_type itype = str2index_type(index_type);

		res->push_back(std::pair<std::pair<xml_ns*,char*>,ft_index_type>(tag, itype));
	}
	return res;
}

void delete_cust_rules_vector(std::vector< std::pair< std::pair<xml_ns*,char*>,ft_index_type> >* &v)
{
	std::vector< std::pair< std::pair<xml_ns*,char*>,ft_index_type> >::iterator it;
	for (it = v->begin(); it < v->end(); it++)
	{
		delete it->first.second;
	}
	delete v;
	v = NULL;
}


void PPCreateFtIndex::execute()
{
    tuple_cell tc;
    tuple t(1);
    index_name.op->next(t);
    if (t.is_eos()) throw USER_EXCEPTION(SE1071);

    tc = index_name.get(t);
    if (!tc.is_atomic() || tc.get_atomic_type() != xs_string)
        throw USER_EXCEPTION(SE1071);

    index_name.op->next(t);
    if (!t.is_eos()) throw USER_EXCEPTION(SE1071);
        
    tc = tuple_cell::make_sure_light_atomic(tc);

    //local_lock_mrg->put_lock_on_index(tc.get_str_mem());


	std::vector< std::pair< std::pair<xml_ns*,char*>,ft_index_type> > *cust_rules_vec = NULL;

	if (cust_rules.op)
		cust_rules_vec = make_cust_rules_vector(&cust_rules);

	ft_index_cell* ftic = ft_index_cell::create_index (object_path,
				index_type,
				(doc_schema_node*)root,
				tc.get_str_mem(),
				db_ent->name,
				(db_ent->type == dbe_document),
				cust_rules_vec);
	if (cust_rules_vec)
		delete_cust_rules_vector(cust_rules_vec);
}
