/*
* File:  PPAxisSibling.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "common/sedna.h"

#include "tr/executor/xqops/PPAxisSibling.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/visitor/PPVisitor.h"

PPAxisSibling::PPAxisSibling(dynamic_context *_cxt_,
                             operation_info _info_,
                             PPOpIn _child_,
                             NodeTestType _nt_type_,
                             NodeTestData _nt_data_,
                             bool _following_) : PPIterator(_cxt_, _info_, "PPAxisSibling"),
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
    case node_test_processing_instruction   : next_fun = &PPAxisSibling::next_processing_instruction; break;
    case node_test_comment                  : next_fun = &PPAxisSibling::next_comment; break;
    case node_test_text                     : next_fun = &PPAxisSibling::next_text; break;
    case node_test_node                     : next_fun = &PPAxisSibling::next_node; break;
    case node_test_qname                    : next_fun = &PPAxisSibling::next_qname; break;
    case node_test_attribute                : next_fun = &PPAxisSibling::next_attribute; break;
    case node_test_document                 : next_fun = &PPAxisSibling::next_document; break;
    case node_test_wildcard_star            : next_fun = &PPAxisSibling::next_wildcard_star; break;
    case node_test_wildcard_ncname_star     : next_fun = &PPAxisSibling::next_wildcard_ncname_star; break;
    case node_test_wildcard_star_ncname     : next_fun = &PPAxisSibling::next_wildcard_star_ncname; break;
    default                                 : throw USER_EXCEPTION2(SE1003, "PPAxisSibling: unexpected node test");
    }
    merge_tree=NULL;
}

PPAxisSibling::~PPAxisSibling()
{
    delete child.op;
    child.op = NULL;
    if (merge_tree)
    {
        delete merge_tree;
    }
}

void PPAxisSibling::do_open ()
{
    child.op->open();
    cur = XNULL;
}

void PPAxisSibling::do_reopen()
{
    child.op->reopen();
    if (merge_tree)
    {
        merge_tree->clear_merge();
    }
    cur = XNULL;
}

void PPAxisSibling::do_close()
{
    child.op->close();
}

void PPAxisSibling::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


void PPAxisSibling::next_processing_instruction(tuple &t)
{
    if (!nt_data.ncname_local)
        next_qname_and_text(t,NULL,NULL,pr_ins,comp_type);
    else
        while (true)
        {
            next_qname_and_text(t,NULL,NULL,pr_ins,comp_type);
            if (t.is_eos()) return;
            xptr tmp=child.get(t).get_node();
            if (tmp!=XNULL)
            {
                CHECKP(tmp);
                pi_dsc* desc=(pi_dsc*)XADDR(tmp);
                size_t tsize=desc->target;
                if (tsize==strlen(nt_data.ncname_local)) {
                    char* data= (char*)XADDR(getTextPtr(desc));
                    if (strcmp(nt_data.ncname_local, std::string(data,tsize).c_str()) == 0) return;
                }
            }
        }
}
void PPAxisSibling::next_comment(tuple &t)
{
    next_qname_and_text(t,NULL,NULL,comment,comp_type);
}

void PPAxisSibling::next_text(tuple &t)
{
    next_qname_and_text(t,NULL,NULL,text,comp_type);
}

void PPAxisSibling::next_node(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
        xptr tmp=child.get(t).get_node();
        CHECKP(tmp);
        if (is_node_attribute(tmp)||GETSCHEMENODEX(tmp)->parent->type==virtual_root)continue;
        if (following)
            cur = ((n_dsc*)XADDR(tmp))->rdsc;
        else
        {
            cur = ((n_dsc*)XADDR(tmp))->ldsc;
            if (cur!=XNULL)
            {
                CHECKP(cur);
                if (!is_node_child(cur)) cur=XNULL;
            }
        }
    }
    t.copy(tuple_cell::node(cur));
    CHECKP(cur);
    if (following)
        cur = ((n_dsc*)XADDR(cur))->rdsc;
    else
    {
        cur=((n_dsc*)XADDR(cur))->ldsc;
        if (cur!=XNULL)
        {
            CHECKP(cur);
            if (!is_node_child(cur)) cur=XNULL;
        }
    }
}

void PPAxisSibling::next_qname(tuple &t)
{
    next_qname_and_text(t,nt_data.uri,nt_data.ncname_local,element,comp_qname_type);
}

void PPAxisSibling::next_wildcard_star(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        xptr tmp=child.get(t).get_node();
        CHECKP(tmp);
        if (is_node_attribute(tmp)||GETSCHEMENODEX(tmp)->parent->type==virtual_root)continue;
        if (following)
        {
            cur=((n_dsc*)XADDR(tmp))->rdsc;

            while (true)
            {
                if (cur==XNULL) break;
                CHECKP(cur);
                if (is_element(cur))
                    break;
                else
                    cur=((n_dsc*)XADDR(cur))->rdsc;
            }
        }
        else
        {
            cur=((n_dsc*)XADDR(tmp))->ldsc;
            while (true)
            {
                if (cur==XNULL) break;
                CHECKP(cur);
                if (is_element(cur))
                    break;
                else
                {
                    if (!is_node_child(cur))
                    {
                        cur=XNULL;
                        break;
                    }
                    else
                        cur=((n_dsc*)XADDR(cur))->ldsc;
                }
            }
        }
    }
    CHECKP(cur);
    t.copy(tuple_cell::node(cur));
    if (following)
    {
        cur=((n_dsc*)XADDR(cur))->rdsc;
        while (true)
        {
            if (cur==XNULL) break;
            CHECKP(cur);
            if (is_element(cur))
                break;
            else
                cur=((n_dsc*)XADDR(cur))->rdsc;
        }
    }
    else
    {
        cur=((n_dsc*)XADDR(cur))->ldsc;
        while (true)
        {
            if (cur==XNULL) break;
            CHECKP(cur);
            if (is_element(cur))
                break;
            else
            {
                if (!is_node_child(cur))
                {
                    cur=XNULL;
                    break;
                }
                else {
                    CHECKP(cur);
                    cur=((n_dsc*)XADDR(cur))->ldsc;
                }
            }
        }
    }
}

void PPAxisSibling::next_wildcard_ncname_star(tuple &t)
{
    next_qname_and_text(t,nt_data.uri,NULL,element,comp_uri_type);
}

void PPAxisSibling::next_wildcard_star_ncname(tuple &t)
{
    next_qname_and_text(t,NULL,nt_data.ncname_local,element,comp_local_type);
}

void PPAxisSibling::next_qname_and_text(tuple &t,const char* uri,const char* name,t_item type,comp_schema cfun)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
        CHECKP(cur);
        schema_node_xptr scm=(GETSCHEMENODEX(cur))->parent;
        if (scm->type==virtual_root)
        {
            cur=XNULL;
            continue;
        }
        if (desc_sch.find(scm)==desc_sch.end())
        {
            std::vector<schema_node_xptr> vscm;
            desc_sch[scm]=vscm;
            getSchemeChilds(scm,uri,name, type, cfun,desc_sch[scm]);
        }
        std::vector<schema_node_xptr>* cv=&desc_sch[scm];
        std::vector<schema_node_xptr>::iterator it=cv->begin();
        if (merge_tree==NULL) merge_tree=se_new xptrChanneledMerge((following)?getNextDescriptorOfSameSortXptr:getPreviousDescriptorOfSameSortXptr,following);
        while (it!=cv->end())
        {
            xptr tmp=(following)?getNextSiblingNode(cur,*it):getPreviousSiblingNode(cur,*it);
            if (tmp!=XNULL)
            {
                CHECKP(tmp);
                merge_tree->addChannel(tmp);
            }
            it++;
        }
        cur=merge_tree->getNextNode();
    }
    t.copy(tuple_cell::node(cur));
    CHECKP(cur);
    xptr ind=((n_dsc*)XADDR(cur))->pdsc;
    xptr tmp=merge_tree->getNextNode();
    if (tmp!=XNULL)
    {
        CHECKP(tmp);
        if (ind==((n_dsc*)XADDR(tmp))->pdsc)
            cur=tmp;
        else
        {
            cur=XNULL;
            merge_tree->clear_merge();
        }
    }
    else
        cur=XNULL;
}

void PPAxisSibling::next_attribute(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}
void PPAxisSibling::next_document(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}



PPIterator* PPAxisSibling::do_copy(dynamic_context *_cxt_)
{
    PPAxisSibling *res = se_new PPAxisSibling(_cxt_, info, child, nt_type, nt_data,following);
    res->child.op = child.op->copy(_cxt_);
    return res;
}
