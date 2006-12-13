/*
 * File:  PPAxisSibling.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPAxisSibling.h"
#include "node_utils.h"
#include "PPUtils.h"
using namespace tr_globals;
//#include "PPStaticContext.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPAxisSibling
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPAxisSibling::PPAxisSibling(dynamic_context *_cxt_, 
                         PPOpIn _child_,
                         NodeTestType _nt_type_,
                         NodeTestData _nt_data_, bool _following_) : PPIterator(_cxt_),
                                                   child(_child_),
                                                   nt_type(_nt_type_),
                                                   nt_data(_nt_data_),following(_following_)
{
    switch (nt_type)
    {
        case node_test_processing_instruction	: next_fun = &PPAxisSibling::next_processing_instruction; break;
        case node_test_comment					: next_fun = &PPAxisSibling::next_comment; break;
        case node_test_text						: next_fun = &PPAxisSibling::next_text; break;
        case node_test_node						: next_fun = &PPAxisSibling::next_node; break;
        case node_test_string					: next_fun = &PPAxisSibling::next_string; break;
        case node_test_qname					: next_fun = &PPAxisSibling::next_qname; break;
        case node_test_wildcard_star			: next_fun = &PPAxisSibling::next_wildcard_star; break;
        case node_test_wildcard_ncname_star		: next_fun = &PPAxisSibling::next_wildcard_ncname_star; break;
        case node_test_wildcard_star_ncname		: next_fun = &PPAxisSibling::next_wildcard_star_ncname; break;
        case node_test_function_call			: next_fun = &PPAxisSibling::next_function_call; break;
        case node_test_var_name					: next_fun = &PPAxisSibling::next_var_name; break;
        default									: throw USER_EXCEPTION2(SE1003, "Unexpected node test");
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

void PPAxisSibling::open  ()
{
    child.op->open();

    cur = XNULL;
}

void PPAxisSibling::reopen()
{
    child.op->reopen();
	if (merge_tree)
	{
		merge_tree->clear_merge();
	}
    cur = XNULL;
}

void PPAxisSibling::close ()
{
    child.op->close();
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
				int tsize=desc->target;
				if (tsize==strlen(nt_data.ncname_local))
				{
					xptr ind_ptr=desc->data;
					CHECKP(ind_ptr);
					shft shift= *((shft*)XADDR(ind_ptr));
					char* data=(char*)XADDR(BLOCKXPTR(ind_ptr))+shift;
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
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);
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

void PPAxisSibling::next_string(tuple &t)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisChild::next_string");
}

void PPAxisSibling::next_qname(tuple &t)
{
	char * uri=st_ct.get_uri_by_prefix(nt_data.ncname_prefix,element) ;
	next_qname_and_text(t,uri,nt_data.ncname_local,element,comp_qname_type);
}

void PPAxisSibling::next_wildcard_star(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);
		
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
				else
					cur=((n_dsc*)XADDR(cur))->ldsc;
			}
		}
	}		
}

void PPAxisSibling::next_wildcard_ncname_star(tuple &t)
{
    char * uri=st_ct.get_uri_by_prefix(nt_data.ncname_prefix,element) ;
	next_qname_and_text(t,uri,NULL,element,comp_uri_type);
}

void PPAxisSibling::next_wildcard_star_ncname(tuple &t)
{
    next_qname_and_text(t,NULL,nt_data.ncname_local,element,comp_local_type);
}

void PPAxisSibling::next_function_call(tuple &t)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisSibling::next_function_call");
}

void PPAxisSibling::next_var_name(tuple &t)
{
    throw USER_EXCEPTION2(SE1002, "PPAxisSibling::next_var_name");
}



PPIterator* PPAxisSibling::copy(dynamic_context *_cxt_)
{
    PPAxisSibling *res = new PPAxisSibling(_cxt_, child, nt_type, nt_data,following);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

bool PPAxisSibling::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    return true;
}
void PPAxisSibling::next_qname_and_text(tuple &t,const char* uri,const char* name,t_item type,comp_schema cfun)
{
     while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XPTY0020);
		
        cur = child.get(t).get_node();
		CHECKP(cur);
		schema_node* scm=(GETSCHEMENODEX(cur))->parent;
		if (scm->type==virtual_root)
		{
			cur=XNULL;
			continue;
		}
		if (desc_sch.find(scm)==desc_sch.end())
		{
			std::vector<schema_node*> vscm;
			desc_sch[scm]=vscm;
			getSchemeChilds(scm,uri,name, type, cfun,desc_sch[scm]);
		}	
		std::vector<schema_node*>* cv=&desc_sch[scm];
		std::vector<schema_node*>::iterator it=cv->begin();
		if (merge_tree==NULL) merge_tree=new xptrChanneledMerge((following)?getNextDescriptorOfSameSortXptr:getPreviousDescriptorOfSameSortXptr,following);
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
