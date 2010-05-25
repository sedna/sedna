/*
* File:  PPAxisSelf.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "common/sedna.h"

#include "tr/executor/xqops/PPAxisSelf.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/executor/base/merge.h"
#include "tr/executor/base/visitor/PPVisitor.h"

PPAxisSelf::PPAxisSelf(dynamic_context *_cxt_,
                       operation_info _info_,
                       PPOpIn _child_,
                       NodeTestType _nt_type_,
                       NodeTestData _nt_data_) : PPIterator(_cxt_, _info_, "PPAxisSelf"),
                       child(_child_),
                       nt_type(_nt_type_),
                       nt_data(_nt_data_)
{
    if(nt_type == node_test_element && nt_data.ncname_local == NULL)
        nt_type = node_test_wildcard_star;
}

PPAxisSelf::~PPAxisSelf()
{
    delete child.op;
    child.op = NULL;
}

void PPAxisSelf::do_open ()
{
    child.op->open();

}

void PPAxisSelf::do_reopen()
{
    child.op->reopen();

}

void PPAxisSelf::do_close()
{
    child.op->close();
}

void PPAxisSelf::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


PPIterator* PPAxisSelf::do_copy(dynamic_context *_cxt_)
{
    PPAxisSelf *res = se_new PPAxisSelf(_cxt_, info, child, nt_type, nt_data);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPAxisSelf::do_next (tuple &t)
{
    switch (nt_type)
    {
    case node_test_processing_instruction	:
        while (true)
        {
            child.op->next(t);
            if (t.is_eos()) return;
            if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
            xptr node=child.get(t).get_node();
            if (node!=XNULL)
            {
                CHECKP(node);
                t_item type=GETSCHEMENODEX(node)->type;
                if (type!=pr_ins) continue;
                else
                {
                    if (!nt_data.ncname_local) return;
                    else
                    {
                        pi_dsc* desc=(pi_dsc*)XADDR(node);
                        size_t tsize=desc->target;
                        if (tsize==strlen(nt_data.ncname_local)) {
                            char* data= (char*)XADDR(getTextPtr(desc));
                            if (strcmp(nt_data.ncname_local, std::string(data,tsize).c_str()) == 0) return;
                            else continue;
                        }
                    }
                }
            }
        }
    case node_test_comment					:
    case node_test_text						:
    case node_test_node						:
    case node_test_wildcard_star			:
        while (true)
        {
            child.op->next(t);
            if (t.is_eos()) return;
            if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
            xptr node=child.get(t).get_node();
            if (node!=XNULL)
            {
                CHECKP(node);
                t_item type=GETSCHEMENODEX(node)->type;
                switch (type)
                {
                case text						:
                    if (nt_type!=node_test_text && nt_type!=node_test_node) continue;
                    else return;
                case comment					:
                    if (nt_type!=node_test_comment && nt_type!=node_test_node) continue;
                    else return;
                case pr_ins					:
                    if (nt_type!=node_test_node) continue;
                    else return;
                case xml_namespace: continue;
                case element						:
                    if (nt_type!=node_test_text  &&  nt_type!=node_test_comment) return;
                    else continue;
                default:
                    if (nt_type==node_test_node ) return;
                    else continue;

                }
            }
        }
    case node_test_wildcard_ncname_star		:
    case node_test_wildcard_star_ncname		:
    case node_test_qname					:
    case node_test_element                  :
        while (true)
        {
            child.op->next(t);
            if (t.is_eos()) return;
            if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
            xptr node=child.get(t).get_node();
            if (node != XNULL)
            {
                CHECKP(node);
                schema_node_cptr scm = GETSCHEMENODEX(node);
                t_item type=scm->type;
                if (type != element) continue;
                comp_schema fun;
                const char *uri = NULL;
                const char *local = NULL;
                if (nt_type == node_test_qname || nt_type == node_test_element)
                {
                    fun = comp_qname_type;
                    uri = nt_data.uri;
                    local = nt_data.ncname_local;
                }
                else if (nt_type == node_test_wildcard_star_ncname)
                {
                    fun = comp_local_type;
                    local =  nt_data.ncname_local;
                }
                else /* node_test_wildcard_ncname_star */
                {
                    fun = comp_uri_type;
                    uri = nt_data.uri;
                }
                if (fun(scm,uri,local,element)) return;
            }
        }
    case node_test_attribute                :
        while (true)
        {
            child.op->next(t);
            if (t.is_eos()) return;
            if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
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
    case node_test_document                 :
        while (true)
        {
            child.op->next(t);
            if (t.is_eos()) return;
            if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
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
    default									: throw USER_EXCEPTION2(SE1003, "PPAxisSelf: unexpected node test");
    }

}
