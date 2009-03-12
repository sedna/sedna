/*
 * File:  PPCreateFtIndex.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPCREATEFTINDEX_H
#define _PPCREATEFTINDEX_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/XPathOnSchema.h"
#include "tr/ft/ft_index_data.h"

ft_index_type str2index_type(const char *str);
std::vector< std::pair< std::pair<xml_ns*,char*>,ft_index_type> > *make_cust_rules_vector(PPOpIn *cust_rules, dynamic_context *cxt);
void delete_cust_rules_vector(std::vector< std::pair< std::pair<xml_ns*,char*>,ft_index_type> >* &v);

class PPCreateFtIndex : public PPUpdate
{
    // given parameters
    PathExpr *object_path;
	ft_index_type index_type;
	ft_index_impl index_impl;
	PPOpIn cust_rules;
    counted_ptr<db_entity> db_ent;
    PPOpIn index_name;
    dynamic_context *cxt;

    // obtained parameters and local data
    schema_node *root;
public:
    void open();
    void close();
    void execute();

    PPCreateFtIndex(PathExpr *_object_path_,
                    char* _index_type_,
                    counted_ptr<db_entity> _db_ent_,
                    PPOpIn _index_name_,
                    PPOpIn _cust_rules_,
                    dynamic_context *_cxt_);
    PPCreateFtIndex(PathExpr *_object_path_,
                    char *_index_type_,
                    counted_ptr<db_entity> _db_ent_,
                    PPOpIn _index_name_,
                    dynamic_context *_cxt_);

    ~PPCreateFtIndex();
};



#endif
