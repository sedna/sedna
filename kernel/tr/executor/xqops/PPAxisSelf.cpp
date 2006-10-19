/*
 * File:  PPAxisSelf.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPAxisSelf.h"
#include "node_utils.h"
#include "PPUtils.h"

using namespace tr_globals;

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPAxisSelf
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPAxisSelf::PPAxisSelf(variable_context *_cxt_, 
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


bool PPAxisSelf::result(PPIterator* cur, variable_context *cxt, void*& r)
{
	return true;
}


PPIterator* PPAxisSelf::copy(variable_context *_cxt_)
{
    PPAxisSelf *res = new PPAxisSelf(_cxt_, child, nt_type, nt_data);
    res->child.op = child.op->copy(_cxt_);

    return res;
}



void PPAxisSelf::next   (tuple &t) 
{ 
	
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
				if (t.is_eos()) return;
				if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);
				xptr node=child.get(t).get_node();
				if (node!=XNULL)
				{
					CHECKP(node);
					t_item type=(GETBLOCKBYNODE(node))->snode->type;
					if (type!=pr_ins) continue;
					else
					{
						if (strlen(nt_data.ncname)==0) return;
						else
						{
							pi_dsc* desc=(pi_dsc*)XADDR(node);
							int tsize=desc->target;
							if (tsize==strlen(nt_data.ncname))
							{
								xptr ind_ptr=desc->data;
								CHECKP(ind_ptr);
								shft shift= *((shft*)XADDR(ind_ptr));
								char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
								if (strcmp(nt_data.ncname, std::string(data,tsize).c_str()) == 0) return;
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
				if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);
				xptr node=child.get(t).get_node();
				if (node!=XNULL)
				{
					CHECKP(node);
					t_item type=(GETBLOCKBYNODE(node))->snode->type;
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
			while (true)
			{
				child.op->next(t);
				if (t.is_eos()) return;
				if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);
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
						uri=st_ct.get_uri_by_prefix(xs_QName_get_prefix(nt_data.qname),element);
						local=xs_QName_get_local_name(nt_data.qname);
					}
					else
					if (nt_type==node_test_wildcard_star_ncname) 
					{
						fun=comp_local_type;
						local=nt_data.ncname;
					}
					else if (nt_type==node_test_wildcard_ncname_star) 
					{
						fun=comp_uri_type;
						uri=st_ct.get_uri_by_prefix(nt_data.ncname,element) ;
					}
					if (fun(scm,uri,local,element)) return;
				}

			}
		default									: throw USER_EXCEPTION2(SE1003, "in AxisSelf");
		}
		
}
