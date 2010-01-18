/*
 * File:  PPExplain.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "common/u/uutils.h"
#include "tr/executor/xqops/PPExplain.h"
#include "tr/executor/base/visitor/PPExplainVisitor.h"
#include "tr/mo/mo.h"

PPExplain::PPExplain(dynamic_context *_cxt_,
                     operation_info _info_,
                     PPQueryEssence* _qep_tree_) : PPIterator(_cxt_, _info_),
                                                   qep_tree(_qep_tree_),
                                                   scm(doc_schema_node_object::create(false))
{
}

PPExplain::~PPExplain()
{
    scm->drop();
}

void PPExplain::do_open ()
{
    first_time = true;
}

void PPExplain::do_reopen()
{
    first_time = true;
}

void PPExplain::do_close()
{
}

static inline xptr 
insertPrologDeclaration(const char* name, 
                        const char* value, 
                        const xptr& left, 
                        const xptr& parent, 
                        const xmlns_ptr& ns)
{
    xptr tmp = insert_element_i(left,XNULL,parent,"declaration",xs_untyped,ns);
    insert_attribute_i(XNULL,XNULL,tmp,name,xs_untypedAtomic,value,strlen(value),ns);
    return tmp;
}

void PPExplain::do_next (tuple &t)
{
    if(first_time) 
    {
        first_time = false;
        char buf[20];
        xmlns_ptr explain_ns = cxt->st_cxt->get_xmlns_by_prefix(SEDNA_NAMESPACE_PREFIX);

        xptr root = insert_doc_node(scm, "$explain", NULL);
        /* Fill information about prolog */
        xptr left = insert_element_i(XNULL,XNULL,root,"plolog",xs_untyped,explain_ns);

        xptr tmp = XNULL;
        /* Insert boundary space declaration */
        if(cxt->st_cxt->is_field_set_in_prolog(static_context::SC_BOUNDARY_SPACE))
            tmp = insertPrologDeclaration("boundary-space", 
                                          cxt->st_cxt->get_boundary_space() == xq_boundary_space_strip ? "strip" : "preserve",
                                          tmp, left, explain_ns);
        /* Insert default collation uri declaration */
        if(cxt->st_cxt->is_field_set_in_prolog(static_context::SC_DEFAULT_COLLATION_URI))
            tmp = insertPrologDeclaration("default-collation",
                                          cxt->st_cxt->get_default_collation_uri(),
                                          tmp, left, explain_ns);
        /* Insert base uri declaration */
        if(cxt->st_cxt->is_field_set_in_prolog(static_context::SC_BASE_URI))
            tmp = insertPrologDeclaration("base-uri",
                                          cxt->st_cxt->get_base_uri(),
                                          tmp, left, explain_ns);
        /* Insert construction declaration */
        if(cxt->st_cxt->is_field_set_in_prolog(static_context::SC_CONSTRUCTION))
            tmp = insertPrologDeclaration("construction", 
                                          cxt->st_cxt->get_construction_mode() ? "preserve" : "strip",
                                          tmp, left, explain_ns);
        /* Insert ordering mode declaration */
        if(cxt->st_cxt->is_field_set_in_prolog(static_context::SC_ORDERING_MODE))
            tmp = insertPrologDeclaration("ordering-mode", 
                                          cxt->st_cxt->get_ordering_mode() == xq_ordering_mode_ordered ? "ordered" : "unordered",
                                          tmp, left, explain_ns);
        /* Insert empty order declaration */
        if(cxt->st_cxt->is_field_set_in_prolog(static_context::SC_EMPTY_ORDER))
            tmp = insertPrologDeclaration("empty-order", 
                                          cxt->st_cxt->get_empty_order() == xq_empty_order_greatest ? "greatest" : "least",
                                          tmp, left, explain_ns);
        /* Insert copy-namespaces declaration (preserve) */
        if(cxt->st_cxt->is_field_set_in_prolog(static_context::SC_NAMESPACE_PRESERVE))
            tmp = insertPrologDeclaration("copy-namespaces", 
                                          cxt->st_cxt->is_namespace_preserve() ? "preserve" : "no-preserve",
                                          tmp, left, explain_ns);
        /* Insert copy-namespaces declaration (preserve) */
        if(cxt->st_cxt->is_field_set_in_prolog(static_context::SC_NAMESPACE_INHERIT))
            tmp = insertPrologDeclaration("copy-namespaces", 
                                          cxt->st_cxt->is_namespace_inherit() ? "inherit" : "no-inherit",
                                          tmp, left, explain_ns);
        /* Insert output indent option declaration */
        if(cxt->st_cxt->is_field_set_in_prolog(static_context::SC_OUTPUT_INDENT))
            tmp = insertPrologDeclaration("indent", 
                                          cxt->st_cxt->get_output_indent() == se_output_indent_yes ? "yes" : "no",
                                          tmp, left, explain_ns);

        /* Insert namespace declarations */
        std::vector<xmlns_ptr> nss = cxt->st_cxt->get_explicit_namespaces();
        std::vector<xmlns_ptr>::iterator it_end = nss.end();
        for(std::vector<xmlns_ptr>::iterator it = nss.begin(); it != it_end; it++)
        {
            tmp = insert_element_i(tmp,XNULL,left,"namespace",xs_untyped,explain_ns);
            xptr aleft = insert_attribute_i(XNULL,XNULL,tmp,"prefix",xs_untypedAtomic, (*it)->prefix, strlen((*it)->prefix), explain_ns);
            insert_attribute_i(aleft,XNULL,tmp,"uri",xs_untypedAtomic, (*it)->uri, strlen((*it)->uri), explain_ns);
        }
        
        /* Insert default element namespace declaration */
        xmlns_ptr defnsptr = cxt->st_cxt->get_default_namespace();
        if(defnsptr != NULL_XMLNS)
        {
            tmp = insert_element_i(tmp,XNULL,left,"default-element-namespace",xs_untyped,explain_ns);
            insert_attribute_i(XNULL,XNULL,tmp,"uri",xs_untypedAtomic, defnsptr->uri, strlen(defnsptr->uri), explain_ns);
        }

        /* Insert physical plan for each global variable */
        for(int i = 0; i < dynamic_context::glb_var_cxt.size; i++)
        {
            tmp = insert_element_i(tmp,XNULL,left,"variable",xs_untyped,explain_ns);
            u_itoa(i,buf,10);
            insert_attribute_i(XNULL,XNULL,tmp,"id",xs_untypedAtomic, buf, strlen(buf), explain_ns);
            PPExplainVisitor visitor(cxt, tmp);
            (dynamic_context::glb_var_cxt.producers[i]).op->accept(visitor);
        }
        
        /* Insert physical plan for each function */
        for(int i = 0; i < dynamic_context::funct_cxt.size; i++)
        {
            tmp = insert_element_i(tmp,XNULL,left,"function",xs_untyped,explain_ns);
            u_itoa(i,buf,10);
            insert_attribute_i(XNULL,XNULL,tmp,"id",xs_untypedAtomic, buf, strlen(buf), explain_ns);
            PPExplainVisitor visitor(cxt, tmp);
            (dynamic_context::funct_cxt.fun_decls[i]).op->accept(visitor);
        }
       
        
        /* Fill information about query body */
        left = insert_element_i(left,XNULL,root,"query",xs_untyped,explain_ns);

        PPExplainVisitor visitor(cxt, left);
        qep_tree->accept(visitor);
        
        t.copy(tuple_cell::node_indir(root));
    }
    else
    {
        t.set_eos();
        first_time = true;
    }
}

PPIterator* PPExplain::do_copy(dynamic_context *_cxt_)
{
    throw XQUERY_EXCEPTION2(SE1003, "Copy could not be called for explain function");
}

void PPExplain::do_accept(PPVisitor &v)
{
    throw XQUERY_EXCEPTION2(SE1003, "Accept could not be called for explain function");
}
