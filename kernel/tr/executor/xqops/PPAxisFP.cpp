/*
 * File:  PPAxisFP.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPAxisFP.h"
#include "node_utils.h"
#include "PPUtils.h"

using namespace tr_globals;
//#include "PPStaticContext.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPAxisFP
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPAxisFP::PPAxisFP(variable_context *_cxt_, 
                         PPOpIn _child_,
                         NodeTestType _nt_type_,
                         NodeTestData _nt_data_, bool _following_) : PPIterator(_cxt_),
                                                   child(_child_),
                                                   nt_type(_nt_type_),
                                                   nt_data(_nt_data_),following(_following_)
{
    switch (nt_type)
    {
        case node_test_processing_instruction	: next_fun = &PPAxisFP::next_processing_instruction; break;
        case node_test_comment					: next_fun = &PPAxisFP::next_comment; break;
        case node_test_text						: next_fun = &PPAxisFP::next_text; break;
        case node_test_node						: next_fun = &PPAxisFP::next_node; break;
        case node_test_string					: next_fun = &PPAxisFP::next_string; break;
        case node_test_qname					: next_fun = &PPAxisFP::next_qname; break;
        case node_test_wildcard_star			: next_fun = &PPAxisFP::next_wildcard_star; break;
        case node_test_wildcard_ncname_star		: next_fun = &PPAxisFP::next_wildcard_ncname_star; break;
        case node_test_wildcard_star_ncname		: next_fun = &PPAxisFP::next_wildcard_star_ncname; break;
        case node_test_function_call			: next_fun = &PPAxisFP::next_function_call; break;
        case node_test_var_name					: next_fun = &PPAxisFP::next_var_name; break;
        default									: throw USER_EXCEPTION2(SE1003, "Unexpected node test");
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

void PPAxisFP::open  ()
{
    child.op->open();

    cur = XNULL;
	base=XNULL;
}

void PPAxisFP::reopen()
{
    child.op->reopen();
	if (merge_tree)
	{
		merge_tree->clear_merge();
	}
    cur = XNULL;
	base=XNULL;
}

void PPAxisFP::close ()
{
    child.op->close();
}


void PPAxisFP::next_processing_instruction(tuple &t)
{
    if (strlen(nt_data.ncname.n)==0)
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
				int tsize=desc->target;
				if (tsize==strlen(nt_data.ncname.n))
				{
					xptr ind_ptr=desc->data;
					CHECKP(ind_ptr);
					shft shift= *((shft*)XADDR(ind_ptr));
					char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
					if (strcmp(nt_data.ncname.n, std::string(data,tsize).c_str()) == 0) return;
				}
			}
		}   
}
void PPAxisFP::next_comment(tuple &t)
{
   next_qname_and_text(t,NULL,NULL,comment,comp_type);
}

void PPAxisFP::next_text(tuple &t)
{
     next_qname_and_text(t,NULL,NULL,text,comp_type);
}

void PPAxisFP::next_node(tuple &t)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) 		
			return;
        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);
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

void PPAxisFP::next_string(tuple &t)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisChild::next_string");
}

void PPAxisFP::next_qname(tuple &t)
{
	char * uri=st_ct.get_uri_by_prefix(nt_data.qname.Prefix,element) ;
	next_qname_and_text(t,uri,nt_data.qname.LocalPart.n,element,comp_qname_type);
}

void PPAxisFP::next_wildcard_star(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);
		
        base = child.get(t).get_node();
		if (following)
		{
			cur = getNextNDNode(base);
			while (true)
			{
				if (cur==XNULL || is_element(cur))
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
				if (cur==XNULL || (is_element(cur) && nid_cmp_effective(cur,base)!=-2)) 
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
			if (cur==XNULL || is_element(cur))
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
			if (cur==XNULL || (is_element(cur) && nid_cmp_effective(cur,base)!=-2)) 
				break;
			else			
				cur = getPreviousDONode(cur);
		}
	}		   
}

void PPAxisFP::next_wildcard_ncname_star(tuple &t)
{
    char * uri=st_ct.get_uri_by_prefix(nt_data.ncname,element) ;
	next_qname_and_text(t,uri,NULL,element,comp_uri_type);
}

void PPAxisFP::next_wildcard_star_ncname(tuple &t)
{
    next_qname_and_text(t,NULL,nt_data.ncname.n,element,comp_local_type);
}

void PPAxisFP::next_function_call(tuple &t)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisFP::next_function_call");
}

void PPAxisFP::next_var_name(tuple &t)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisFP::next_var_name");
}



PPIterator* PPAxisFP::copy(variable_context *_cxt_)
{
    PPAxisFP *res = new PPAxisFP(_cxt_, child, nt_type, nt_data,following);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

bool PPAxisFP::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    return true;
}
void PPAxisFP::next_qname_and_text(tuple &t,const char* uri,const char* name,t_item type,comp_schema cfun)
{
     while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);
		
        base = child.get(t).get_node();
		is_col=is_node_in_collection(base);
		if (is_col)
		{
			if (following)
			{
				cur = getNextNDNode(base);
				while (true)
				{
					if (cur==XNULL || cfun(GETSCHEMENODEX(cur),uri,name,type))
						break;
					else
						cur = getNextDONode(cur);
				}
			}
			else
			{
				base=getPreviousDONode(cur);
				while (true)
				{
					if (cur==XNULL || (cfun(GETSCHEMENODEX(cur),uri,name,type) && nid_cmp_effective(cur,base)!=-2)) 
						break;
					else			
						cur = getPreviousDONode(cur);
				}				
			}
		}		
		else
		{
			CHECKP(base);
			schema_node* scm=(GETSCHEMENODEX(base))->root;
			if (desc_sch.find(scm)==desc_sch.end())
			{
				std::vector<schema_node*> vscm;
				desc_sch[scm]=vscm;
				getSchemeDescendants(scm,uri,name, type, cfun,desc_sch[scm]);
			}	
			std::vector<schema_node*>* cv=&desc_sch[scm];
			std::vector<schema_node*>::iterator it=cv->begin();
			if (merge_tree==NULL) merge_tree=new xptrChanneledMerge((following)?getNextDescriptorOfSameSortXptr:getPreviousDescriptorOfSameSortXptr,following);
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
					if (cur==XNULL || cfun(GETSCHEMENODEX(cur),uri,name,type))
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
					if (cur==XNULL || (cfun(GETSCHEMENODEX(cur),uri,name,type) && nid_cmp_effective(cur,base)!=-2)) 
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

