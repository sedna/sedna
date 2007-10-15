/*
 * File:  PPAxisSelf.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPAxisSelf.h"
#include "tr/crmutils/node_utils.h"
#include "tr/executor/base/PPUtils.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPAxisSelf
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPAxisSelf::PPAxisSelf(dynamic_context *_cxt_, 
                         PPOpIn _child_,
                         NodeTestType _nt_type_,
                         NodeTestData _nt_data_) : PPIterator(_cxt_),
                                                   child(_child_),
                                                   nt_type(_nt_type_),
                                                   nt_data(_nt_data_)
{
    }

PPAxisSelf::~PPAxisSelf()
{
    delete child.op;
    child.op = NULL;
}

void PPAxisSelf::open  ()
{
    child.op->open();

 }

void PPAxisSelf::reopen()
{
    child.op->reopen();

}

void PPAxisSelf::close ()
{
    child.op->close();
}


bool PPAxisSelf::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
	return true;
}


PPIterator* PPAxisSelf::copy(dynamic_context *_cxt_)
{
    PPAxisSelf *res = se_new PPAxisSelf(_cxt_, child, nt_type, nt_data);
    res->child.op = child.op->copy(_cxt_);
    res->set_xquery_line(__xquery_line);
    return res;
}



void PPAxisSelf::next   (tuple &t) 
{ 
		SET_XQUERY_LINE(__xquery_line);

		switch (nt_type)
		{
        case node_test_string					: 
		
		case node_test_function_call			:
		case node_test_var_name					:
		
			throw USER_EXCEPTION2(SE1003, "in AxisSelf:var_name");
		case node_test_processing_instruction	:
			while (true)
			{
				child.op->next(t);
				if (t.is_eos()) {UNDO_XQUERY_LINE; return;}
				if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
				xptr node=child.get(t).get_node();
				if (node!=XNULL)
				{
					CHECKP(node);
					t_item type=(GETBLOCKBYNODE(node))->snode->type;
					if (type!=pr_ins) continue;
					else
					{
						if (!nt_data.ncname_local) {UNDO_XQUERY_LINE; return;}
						else
						{
							pi_dsc* desc=(pi_dsc*)XADDR(node);
							int tsize=desc->target;
							if (tsize==strlen(nt_data.ncname_local))
							{
								xptr ind_ptr=desc->data;
								CHECKP(ind_ptr);
								shft shift= *((shft*)XADDR(ind_ptr));
								char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
								if (strcmp(nt_data.ncname_local, std::string(data,tsize).c_str()) == 0) {UNDO_XQUERY_LINE; return;}
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
				if (t.is_eos()) {UNDO_XQUERY_LINE; return;}
				if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
				xptr node=child.get(t).get_node();
				if (node!=XNULL)
				{
					CHECKP(node);
					t_item type=(GETBLOCKBYNODE(node))->snode->type;
					switch (type)
					{
					case text						:
						if (nt_type!=node_test_text && nt_type!=node_test_node) continue;
						else {UNDO_XQUERY_LINE; return;}
					case comment					:
						if (nt_type!=node_test_comment && nt_type!=node_test_node) continue;
						else {UNDO_XQUERY_LINE; return;}
					case pr_ins					:
						if (nt_type!=node_test_node) continue;
						else {UNDO_XQUERY_LINE; return;}
					case xml_namespace: continue;
					case element						:
						if (nt_type!=node_test_text  &&  nt_type!=node_test_comment) {UNDO_XQUERY_LINE; return;}
						else continue;
					default:
						if (nt_type==node_test_node ) {UNDO_XQUERY_LINE; return;}
						else continue;

					}
				}
			}
		case node_test_wildcard_ncname_star		:
		case node_test_wildcard_star_ncname		:
		case node_test_qname					:
			while (true)
			{
				child.op->next(t);
				if (t.is_eos()) {UNDO_XQUERY_LINE; return;}
				if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION(XPTY0020);
				xptr node=child.get(t).get_node();
				if (node!=XNULL)
				{
					CHECKP(node);
					schema_node* scm=(GETBLOCKBYNODE(node))->snode;
					t_item type=scm->type;
					if (type!=element) continue;
					comp_schema fun;
					const char *uri=NULL;
					const char *local=NULL;
					if (nt_type==node_test_qname) 
					{
						fun=comp_qname_type;
						uri=nt_data.uri;
						local=nt_data.ncname_local;
					}
					else
					if (nt_type==node_test_wildcard_star_ncname) 
					{
						fun=comp_local_type;
						local=nt_data.ncname_local;
					}
					else if (nt_type==node_test_wildcard_ncname_star) 
					{
						fun=comp_uri_type;
						uri=nt_data.uri;
					}
					if (fun(scm,uri,local,element)) {UNDO_XQUERY_LINE; return;}
				}

			}
		default									: throw USER_EXCEPTION2(SE1003, "in AxisSelf");
		}

		UNDO_XQUERY_LINE;
		
}
