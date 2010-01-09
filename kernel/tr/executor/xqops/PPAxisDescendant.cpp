/*
* File:  PPAxisDescendant.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "common/sedna.h"

#include "tr/executor/xqops/PPAxisDescendant.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/merge.h"
#include "tr/executor/base/PPVisitor.h"

using namespace std;

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPAxisDescendant && PPAxisDescendantOrSelf
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

void PPAxisDescendant::init_function()
{
    switch (nt_type)
    {
    case node_test_processing_instruction   : next_fun = &PPAxisDescendant::next_processing_instruction; break;
    case node_test_comment                  : next_fun = &PPAxisDescendant::next_comment; break;
    case node_test_text                     : next_fun = &PPAxisDescendant::next_text; break;
    case node_test_node                     : next_fun = &PPAxisDescendant::next_node; break;
    case node_test_qname                    : next_fun = &PPAxisDescendant::next_qname; break;
    case node_test_element                  : next_fun = &PPAxisDescendant::next_element; break;
    case node_test_document                 : next_fun = &PPAxisDescendant::next_document; break;
    case node_test_attribute                : next_fun = &PPAxisDescendant::next_attribute; break;
    case node_test_wildcard_star            : next_fun = &PPAxisDescendant::next_wildcard_star; break;
    case node_test_wildcard_ncname_star     : next_fun = &PPAxisDescendant::next_wildcard_ncname_star; break;
    case node_test_wildcard_star_ncname     : next_fun = &PPAxisDescendant::next_wildcard_star_ncname; break;
    default                                 : throw USER_EXCEPTION2(SE1003, "PPAxisDescandant: unexpected node test");
    }
    merge_tree=NULL;
}

PPAxisDescendant::PPAxisDescendant(dynamic_context *_cxt_,
                                   operation_info _info_,
                                   PPOpIn _child_,
                                   NodeTestType _nt_type_,
                                   NodeTestData _nt_data_): PPIterator(_cxt_, _info_),
                                   child(_child_),
                                   nt_type(_nt_type_),
                                   nt_data(_nt_data_)
{
    self = false;
    init_function();
}

PPAxisDescendant::PPAxisDescendant(dynamic_context *_cxt_,
                                   operation_info _info_,
                                   PPOpIn _child_,
                                   NodeTestType _nt_type_,
                                   NodeTestData _nt_data_,
                                   bool _self_) : PPIterator(_cxt_, _info_),
                                   child(_child_),
                                   nt_type(_nt_type_),
                                   nt_data(_nt_data_),
                                   self(_self_)
{
    init_function();
}

PPAxisDescendantOrSelf::PPAxisDescendantOrSelf(dynamic_context *_cxt_,
                                               operation_info _info_,
                                               PPOpIn _child_,
                                               NodeTestType _nt_type_,
                                               NodeTestData _nt_data_):
    PPAxisDescendant(_cxt_,_info_,_child_,_nt_type_,_nt_data_,true)
{
}


PPAxisDescendant::~PPAxisDescendant()
{
    delete child.op;
    child.op = NULL;
    if (merge_tree)
    {
        delete merge_tree;
    }
}

void PPAxisDescendant::do_open ()
{
    child.op->open();
    cur = XNULL;
}

void PPAxisDescendant::do_reopen()
{
    child.op->reopen();

    cur = XNULL;
    if (merge_tree)
    {
        merge_tree->clear_merge();
    }
}

void PPAxisDescendant::do_close()
{
    child.op->close();
}

void PPAxisDescendant::do_accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    child.op->accept(v);
    v.pop();
}

void PPAxisDescendantOrSelf::do_accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    child.op->accept(v);
    v.pop();
}


PPIterator* PPAxisDescendant::do_copy(dynamic_context *_cxt_)
{
    PPAxisDescendant *res = self ? se_new PPAxisDescendant(_cxt_, info, child, nt_type, nt_data, true) :
                                   se_new PPAxisDescendant(_cxt_, info, child, nt_type, nt_data);
    res->child.op = child.op->copy(_cxt_);
    return res;
}



void PPAxisDescendant::next_node(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
        if (cur!=XNULL)
        {
            CHECKP(cur);
            if(!self) cur = getFirstByOrderChildNode(cur);
        }
    }

    descstack.push_back(cur);
    t.copy(tuple_cell::node(cur));

    CHECKP(cur);
    xptr tmp = getFirstByOrderChildNode(cur);

    if (tmp!=XNULL)
    {
        cur=tmp;
        return;
    }
    while (!self || descstack.size()>1)
    {
        tmp = GETRIGHTPOINTER(cur);
        if (tmp!=XNULL)
        {
            cur=tmp;
            descstack.pop_back();
            return;
        }
        descstack.pop_back();
        if (descstack.size()>0)
        {
            cur=descstack[descstack.size()-1];
            CHECKP(cur);
        }
        else
            break;
    }
    while (descstack.size()>0) descstack.pop_back();
    cur=XNULL;
}


void PPAxisDescendant::next_qname_and_text(tuple &t,const char* uri,const char* name,t_item type,comp_schema cfun)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
        xptr tmp=child.get(t).get_node();
        if (tmp!=XNULL)
        {
            CHECKP(tmp);
            schema_node_xptr scm=(GETBLOCKBYNODE(tmp))->snode;
            if (desc_sch.find(scm)==desc_sch.end())
            {
                vector<schema_node_xptr> vscm;
                desc_sch[scm]=vscm;
                if (self)
                    getSchemeDescendantsOrSelf(scm,uri,name, type,cfun, desc_sch[scm]);
                else
                    getSchemeDescendants(scm,uri,name, type, cfun,desc_sch[scm]);
                curvect=&desc_sch[scm];
            }
            else
                curvect=&desc_sch[scm];
            std::vector<schema_node_xptr>::iterator it=curvect->begin();
            if (merge_tree==NULL) merge_tree=se_new xptrChanneledMerge(getNextDescriptorOfSameSortXptr,true);
            while (it!=curvect->end())
            {
                cur=getFirstDescandantByScheme(tmp,*it);
                if (cur!=XNULL) merge_tree->addChannel(cur);
                it++;
            }

            cur=merge_tree->getNextNode();
            ancestor=tmp;
        }
    }
    t.copy(tuple_cell::node(cur));
    cur=merge_tree->getNextNode();
    if (cur!=XNULL && nid_cmp_effective(cur,ancestor)!=2)
    {
        cur=XNULL;
        merge_tree->clear_merge();
    }
}

void PPAxisDescendant::next_comment(tuple &t)
{
    next_qname_and_text(t,NULL,NULL,comment,comp_type);
}
void PPAxisDescendant::next_qname(tuple &t)
{
    next_qname_and_text(t,nt_data.uri,nt_data.ncname_local,element,comp_qname_type);
}
void PPAxisDescendant::next_text(tuple &t)
{
    next_qname_and_text(t,NULL,NULL,text,comp_type);
}
void PPAxisDescendant::next_wildcard_ncname_star(tuple &t)
{
    next_qname_and_text(t,nt_data.uri,NULL,element,comp_uri_type);
}
void PPAxisDescendant::next_wildcard_star_ncname(tuple &t)
{
    next_qname_and_text(t,NULL,nt_data.ncname_local,element,comp_local_type);
}

void PPAxisDescendant::next_processing_instruction(tuple &t)
{
    if (!nt_data.ncname_local)
        next_qname_and_text(t,NULL,NULL,pr_ins,comp_type);
    else
    {
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
                    char* data = (char*) XADDR(getTextPtr(desc));
                    if (strcmp(nt_data.ncname_local, std::string(data,tsize).c_str()) == 0) return;
                }
            }
        }
    }
}

void PPAxisDescendant::next_wildcard_star(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
        if (cur!=XNULL)
        {
            CHECKP(cur);
            if(!self || (GETBLOCKBYNODE(cur))->snode->type!=element)
                cur=getFirstByOrderElementChild(cur);
        }
    }
    CHECKP(cur);
    descstack.push_back(cur);
    t.copy(tuple_cell::node(cur));
    xptr tmp = getFirstByOrderElementChild(cur);
    if (tmp!=XNULL)
    {
        cur=tmp;
        return;
    }
    while (!self || descstack.size()>1)
    {
        CHECKP(cur);
        tmp = GETRIGHTPOINTER(cur);
        if (tmp!=XNULL)
        {
            CHECKP(tmp);
            while ((GETBLOCKBYNODE(tmp))->snode->type != element)
            {
                tmp = GETRIGHTPOINTER(tmp);

                if (tmp == XNULL)
                {
                    break;
                }
                else
                {
                    CHECKP(tmp);
                }
            }

            if (tmp!=XNULL)
            {
                cur=tmp;
                descstack.pop_back();
                return;
            }
        }
        descstack.pop_back();
        if (descstack.size()>0)
        {
            cur=descstack[descstack.size()-1];
            CHECKP(cur);
        }
        else
            break;
    }
    while (descstack.size()>0) descstack.pop_back();
    cur=XNULL;
}

void PPAxisDescendant::next_document(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        if(self)
        {
            /* Works just like self::document(element()) in this case! */
            xptr node = child.get(t).get_node();

            if (node != XNULL)
            {
                CHECKP(node);
                schema_node_cptr scm = GETSCHEMENODEX(node);
                t_item type = scm->type;

                if (type != document) continue;
                if (nt_data.ncname_local == NULL) return;

                RelChildAxisMerge merge;
                xptr cur = merge.init(node,
                                      nt_data.uri,
                                      nt_data.ncname_local,
                                      element,
                                      comp_qname_type);

                if(cur != XNULL && merge.next(cur) == XNULL) return;
            }
        }
    }
}

void PPAxisDescendant::next_element(tuple &t)
{
    if(nt_data.ncname_local)
        next_qname(t);
    else
        next_wildcard_star(t);
}

void PPAxisDescendant::next_attribute(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
        if(self)
        {
            /* Works just like self::attribute() in this case! */
            xptr node=child.get(t).get_node();
            if (node!=XNULL)
            {
                CHECKP(node);
                schema_node_cptr scm = GETSCHEMENODEX(node);
                t_item type = scm->type;

                if (type != attribute) continue;

                if (nt_data.ncname_local == NULL ||
                    comp_qname_type(scm, nt_data.uri, nt_data.ncname_local, attribute)) return;
            }
        }
    }
}



///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPAxisDescendantAttr
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPAxisDescendantAttr::PPAxisDescendantAttr(dynamic_context *_cxt_,
                                           operation_info _info_,
                                           PPOpIn _child_,
                                           NodeTestType _nt_type_,
                                           NodeTestData _nt_data_) :
    PPAxisDescendant(_cxt_,_info_,_child_,_nt_type_,_nt_data_)
{
}

void PPAxisDescendantAttr::do_accept(PPVisitor &v)
{
    v.push  (this);
    v.visit (this);
    child.op->accept(v);
    v.pop();
}


void PPAxisDescendantAttr::next_processing_instruction(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}
void PPAxisDescendantAttr::next_comment(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}
void PPAxisDescendantAttr::next_text(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}
void PPAxisDescendantAttr::next_document(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}
void PPAxisDescendantAttr::next_element(tuple &t)
{
    while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
    }
}

void PPAxisDescendantAttr::next_attribute(tuple &t)
{
    if(nt_data.ncname_local)
        next_qname(t);
    else
        next_wildcard_star(t);
}

void PPAxisDescendantAttr::next_node(tuple &t)
{
    next_wildcard_star(t);
}
void PPAxisDescendantAttr::next_qname(tuple &t)
{
    next_qname_and_text(t,nt_data.uri,nt_data.ncname_local,attribute,comp_qname_type);
}
void PPAxisDescendantAttr::next_wildcard_ncname_star(tuple &t)
{
    next_qname_and_text(t,nt_data.uri,NULL,attribute,comp_uri_type);
}
void PPAxisDescendantAttr::next_wildcard_star_ncname(tuple &t)
{
    next_qname_and_text(t,NULL,nt_data.ncname_local,attribute,comp_local_type);
}

void PPAxisDescendantAttr::next_wildcard_star(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);

        cur = child.get(t).get_node();
        if (cur!=XNULL)
        {
            CHECKP(cur);
            xptr tmp=getFirstByOrderAttributeChild(cur);
            descstack.push_back(cur);
            if (tmp!=XNULL)
                cur=tmp;
            else
                cur=getFirstAttributeDescendantAndFillPath(descstack);

        }
    }
    t.copy(tuple_cell::node(cur));
    CHECKP(cur);
    cur=GETRIGHTPOINTER(cur);
    if (cur!=XNULL)
    {
        CHECKP(cur);
        if ((GETBLOCKBYNODE(cur))->snode->type==attribute) return;
    }
    cur=getFirstAttributeDescendantAndFillPath(descstack);
    while(cur==XNULL)
    {
        xptr node=descstack[descstack.size()-1];
        descstack.pop_back();
        node=getNextByOrderElement(node);
        if (/*node==XNULL && */descstack.size()==0) return;
        if (node!=XNULL)
        {
            cur=getFirstByOrderAttributeChild(node);
            descstack.push_back(node);
            if (cur==XNULL)
                cur=getFirstAttributeDescendantAndFillPath(descstack);
        }
    }
}
