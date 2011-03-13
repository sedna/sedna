/*
* File:  PPAxisFP.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "common/sedna.h"

#include "tr/executor/xqops/PPAxisFP.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"

#include "tr/structures/schema.h"
#include "tr/structures/nodeoperations.h"
#include "tr/structures/nodeutils.h"

PPAxisFP::PPAxisFP(dynamic_context *_cxt_,
                   operation_info _info_,
                   PPOpIn _child_,
                   NodeTestType _nt_type_,
                   NodeTestData _nt_data_,
                   bool _following_) : PPIterator(_cxt_, _info_, "PPAxisFP"),
                   child(_child_),
                   nt_type(_nt_type_),
                   nt_data(_nt_data_),
                   following(_following_)
{
    NodeTestType type = nt_type;

    if (type == node_test_element)
        type = (nt_data.ncname_local == NULL ? node_test_wildcard_star : node_test_qname);

    switch (type)
    {
    case node_test_processing_instruction   : next_fun = &PPAxisFP::next_processing_instruction; break;
    case node_test_comment                  : next_fun = &PPAxisFP::next_comment; break;
    case node_test_text                     : next_fun = &PPAxisFP::next_text; break;
    case node_test_node                     : next_fun = &PPAxisFP::next_node; break;
    case node_test_qname                    : next_fun = &PPAxisFP::next_qname; break;
    case node_test_attribute                : next_fun = &PPAxisFP::next_attribute; break;
    case node_test_document                 : next_fun = &PPAxisFP::next_document; break;
    case node_test_wildcard_star            : next_fun = &PPAxisFP::next_wildcard_star; break;
    case node_test_wildcard_ncname_star     : next_fun = &PPAxisFP::next_wildcard_ncname_star; break;
    case node_test_wildcard_star_ncname     : next_fun = &PPAxisFP::next_wildcard_star_ncname; break;
    default                                 : throw USER_EXCEPTION2(SE1003, "PPAxisFP: unexpected node test");
    }
    merge_tree=NULL;
}

PPAxisFP::~PPAxisFP()
{
    delete child.op;
    child.op = NULL;
    if (merge_tree)
    {
        delete merge_tree;
    }
}

void PPAxisFP::do_open ()
{
    child.op->open();

    cur = XNULL;
    base = XNULL;
}

void PPAxisFP::do_reopen()
{
    child.op->reopen();
    if (merge_tree)
    {
        merge_tree->clear_merge();
    }
    cur = XNULL;
    base = XNULL;
}

void PPAxisFP::do_close()
{
    child.op->close();
}

void PPAxisFP::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}

void PPAxisFP::next_processing_instruction(tuple &t)
{
    if (!nt_data.ncname_local)
        next_qname_and_text(t,NULL,NULL,pr_ins,comp_type);
    else
        while (true)
        {
            next_qname_and_text(t,NULL,NULL,pr_ins,comp_type);
            if (t.is_eos()) return;
            PINode tmp=child.get(t).get_node();
            if (!tmp.isNull() && (tmp.checkp().compareTarget(nt_data.ncname_local) == 0)) {
                return;
            }
        }
}

void PPAxisFP::next_node(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
        if (following)
            cur = getNextNDNode(child.get(t).get_node());
        else
        {
            base=child.get(t).get_node();
            cur=getPreviousDONode(base);
            while (cur!=XNULL)
            {
                if (nid_cmp_effective(cur,base)==-2)
                    cur=getPreviousDONode(cur);
                else
                    break;
            }
        }
    }

    t.copy(tuple_cell::node(cur));
    if (following)
        cur = getNextDONode(cur);
    else
    {
        cur=getPreviousDONode(cur);
        while (cur!=XNULL)
        {
            if (nid_cmp_effective(cur,base)==-2)
                cur=getPreviousDONode(cur);
            else
                break;
        }
    }
}

void PPAxisFP::next_wildcard_star(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        base = child.get(t).get_node();
        if (following)
        {
            cur = getNextNDNode(base);
            while (true)
            {
                if (cur==XNULL || getNodeType(checkp(cur))==element)
                    break;
                else
                    cur = getNextDONode(cur);
            }
        }
        else
        {

            cur=getPreviousDONode(base);
            while (true)
            {
                if (cur==XNULL || (getNodeType(checkp(cur))==element && nid_cmp_effective(cur, base)!=-2))
                    break;
                else
                    cur = getPreviousDONode(cur);
            }
        }
    }
    t.copy(tuple_cell::node(cur));
    if (following)
    {
        cur = getNextDONode(cur);
        while (true)
        {
            if (cur==XNULL || getNodeType(checkp(cur))==element)
                break;
            else
                cur = getNextDONode(cur);
        }
    }
    else
    {
        cur=getPreviousDONode(cur);
        while (true)
        {
            if (cur==XNULL || (getNodeType(checkp(cur))==element && nid_cmp_effective(cur, base)!=-2))
                break;
            else
                cur = getPreviousDONode(cur);
        }
    }
}

void PPAxisFP::next_qname_and_text(tuple &t,const char* uri,const char* name,t_item type,comp_schema cfun)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        base = child.get(t).get_node();
        is_col= Node(base).isNodeInCollection();
        if (is_col)
        {
            if (following)
            {
                cur = getNextNDNode(base);
                while (true)
                {
                    if (cur==XNULL || cfun(getSchemaNode(cur),uri,name,type))
                        break;
                    else
                        cur = getNextDONode(cur);
                }
            }
            else
            {
                cur=getPreviousDONode(base);
                while (true)
                {
                    if (cur==XNULL || (cfun(getSchemaNode(cur),uri,name,type) && nid_cmp_effective(cur,base)!=-2))
                        break;
                    else
                        cur = getPreviousDONode(cur);
                }
            }
        }
        else
        {
            CHECKP(base);
            schema_node_xptr scm=(getSchemaNode(base))->root;
            if (desc_sch.find(scm)==desc_sch.end())
            {
                std::vector<schema_node_xptr> vscm;
                desc_sch[scm]=vscm;
                getSchemeDescendants(scm,uri,name, type, cfun,desc_sch[scm]);
            }
            std::vector<schema_node_xptr>* cv=&desc_sch[scm];
            std::vector<schema_node_xptr>::iterator it=cv->begin();
            if (merge_tree==NULL) merge_tree=se_new xptrChanneledMerge((following)?getNextDescriptorOfSameSort:getPreviousDescriptorOfSameSort,following);
            while (it!=cv->end())
            {
                xptr tmp=(following)?getNextNDNode(base,*it):getPreviousNANode(base,*it);
                if (tmp!=XNULL) merge_tree->addChannel(tmp);
                it++;
            }

            cur=merge_tree->getNextNode();
        }
    }
    t.copy(tuple_cell::node(cur));
    if (is_col)
    {
        if (following)
        {
            cur = getNextDONode(cur);
            while (true)
            {
                if (cur==XNULL || cfun(getSchemaNode(cur),uri,name,type))
                    break;
                else
                    cur = getNextDONode(cur);
            }
        }
        else
        {
            cur=getPreviousDONode(cur);
            while (true)
            {
                if (cur==XNULL || (cfun(getSchemaNode(cur),uri,name,type) && nid_cmp_effective(cur,base)!=-2))
                    break;
                else
                    cur = getPreviousDONode(cur);
            }
        }
    }
    else
    {
        cur=merge_tree->getNextNode();
    }
}

void PPAxisFP::next_wildcard_ncname_star(tuple &t)
{
    next_qname_and_text(t,nt_data.uri,NULL,element,comp_uri_type);
}
void PPAxisFP::next_wildcard_star_ncname(tuple &t)
{
    next_qname_and_text(t,NULL,nt_data.ncname_local,element,comp_local_type);
}
void PPAxisFP::next_comment(tuple &t)
{
    next_qname_and_text(t,NULL,NULL,comment,comp_type);
}
void PPAxisFP::next_text(tuple &t)
{
    next_qname_and_text(t,NULL,NULL,text,comp_type);
}
void PPAxisFP::next_qname(tuple &t)
{
    next_qname_and_text(t,nt_data.uri,nt_data.ncname_local,element,comp_qname_type);
}

void PPAxisFP::next_attribute(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}
void PPAxisFP::next_document(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}

PPIterator* PPAxisFP::do_copy(dynamic_context *_cxt_)
{
    PPAxisFP *res = se_new PPAxisFP(_cxt_, info, child, nt_type, nt_data,following);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

