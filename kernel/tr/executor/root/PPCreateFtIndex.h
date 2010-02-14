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

ft_index_type str2ft_index_type(const char *str);
const char* ft_index_type2str(ft_index_type type);
ft_index_template_t* make_cust_rules_vector(PPOpIn *cust_rules, dynamic_context *cxt);
void delete_cust_rules_vector(ft_index_template_t* &v);

class PPCreateFtIndex : public PPUpdate
{
private:

    PathExpr *object_path;
	ft_index_type index_type;
	ft_index_impl index_impl;
	PPOpIn cust_rules;
    PathExprRoot root;    
    PPOpIn index_name;
    dynamic_context *cxt;
    
    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:
    
    PPCreateFtIndex(PathExpr *_object_path_,
                    const char* _index_type_,
                    PathExprRoot _root_,
                    PPOpIn _index_name_,
                    PPOpIn _cust_rules_,
                    dynamic_context *_cxt_);
 
    PPCreateFtIndex(PathExpr *_object_path_,
                    const char *_index_type_,
                    PathExprRoot _root_,
                    PPOpIn _index_name_,
                    dynamic_context *_cxt_);

    ~PPCreateFtIndex();
    
    inline ft_index_type get_index_type() const { return index_type; }
    inline const PathExpr* get_path_expression() const { return object_path; }
    inline const PathExprRoot& get_path_root() const { return root; }
};

#endif /* _PPCREATEFTINDEX_H */
