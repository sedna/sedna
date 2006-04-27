/*
 * File:  PPAxisDescendant.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "PPAxisDescendant.h"
#include "node_utils.h"
#include "PPUtils.h"
using namespace std;
using namespace tr_globals;
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPAxisDescendant
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
void PPAxisDescendant::init_function()
{
	switch (nt_type)
    {
        case node_test_processing_instruction	: next_fun = &PPAxisDescendant::next_processing_instruction; break;
        case node_test_comment					: next_fun = &PPAxisDescendant::next_comment; break;
        case node_test_text						: next_fun = &PPAxisDescendant::next_text; break;
        case node_test_node						: next_fun = &PPAxisDescendant::next_node; break;
		case node_test_qname					: next_fun = &PPAxisDescendant::next_qname; break;
        case node_test_wildcard_star			: next_fun = &PPAxisDescendant::next_wildcard_star; break;
        case node_test_wildcard_ncname_star		: next_fun = &PPAxisDescendant::next_wildcard_ncname_star; break;
        case node_test_wildcard_star_ncname		: next_fun = &PPAxisDescendant::next_wildcard_star_ncname; break;
        case node_test_function_call			: next_fun = &PPAxisDescendant::next_function_call; break;
		default									: throw USER_EXCEPTION2(SE1003, "in AxisDescandant");
    }
	merge_tree=NULL;
}
PPAxisDescendant::PPAxisDescendant(variable_context *_cxt_, 
                         PPOpIn _child_,
                         NodeTestType _nt_type_,
						 NodeTestData _nt_data_):PPIterator(_cxt_),
                                                   child(_child_),
                                                   nt_type(_nt_type_),
                                                   nt_data(_nt_data_)
{
	self=false; 
	init_function();
}
PPAxisDescendant::PPAxisDescendant(variable_context *_cxt_, 
                         PPOpIn _child_,
                         NodeTestType _nt_type_,
                         NodeTestData _nt_data_,bool _self_) : PPIterator(_cxt_),
                                                   child(_child_),
                                                   nt_type(_nt_type_),
                                                   nt_data(_nt_data_),
												   self(_self_)
{
    init_function();
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

void PPAxisDescendant::open  ()
{
    child.op->open();

    cur = XNULL;
}

void PPAxisDescendant::reopen()
{
    child.op->reopen();

    cur = XNULL;
	if (merge_tree)
	{
		merge_tree->clear_merge();
	}
}

void PPAxisDescendant::close ()
{
    child.op->close();
}
PPIterator* PPAxisDescendant::copy(variable_context *_cxt_)
{
    PPAxisDescendant *res = new PPAxisDescendant(_cxt_, child, nt_type, nt_data);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPAxisDescendant::result(PPIterator* cur, variable_context *cxt, void*& r)
{
	return true;
}

void PPAxisDescendant::next_comment(tuple &t)
{
   next_qname_and_text(t,NULL,NULL,comment,comp_type);
}
void PPAxisDescendant::next_function_call(tuple &t)
{
	throw USER_EXCEPTION2(SE1002, "in AxisDescandant:function_call");
}

void PPAxisDescendant::next_node(tuple &t)
{
    while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

		if (!(child.get(t).is_node())) throw USER_EXCEPTION(XP0020);
		
        cur = child.get(t).get_node();
		if (cur!=XNULL)
		{
			CHECKP(cur);
			if(!self)	cur=getFirstByOrderChildNode(cur);
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
void PPAxisDescendant::next_processing_instruction(tuple &t)
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
void PPAxisDescendant::next_qname(tuple &t)
{
	char * uri=st_ct.get_uri_by_prefix(nt_data.qname.Prefix,element) ;
	next_qname_and_text(t,uri,nt_data.qname.LocalPart.n,element,comp_qname_type);
}
void PPAxisDescendant::next_qname_and_text(tuple &t,const char* uri,const char* name,t_item type,comp_schema cfun)
{
    while (cur == NULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XP0020);
        xptr tmp=child.get(t).get_node();
		if (tmp!=XNULL)
		{
			CHECKP(tmp);
			schema_node* scm=(GETBLOCKBYNODE(tmp))->snode;
			if (desc_sch.find(scm)==desc_sch.end())
			{
				vector<schema_node*> vscm;
				desc_sch[scm]=vscm;
				if (self)
					getSchemeDescendantsOrSelf(scm,uri,name, type,cfun, desc_sch[scm]);
				else
					getSchemeDescendants(scm,uri,name, type, cfun,desc_sch[scm]);
				curvect=&desc_sch[scm];
			}
			else
				curvect=&desc_sch[scm];
			std::vector<schema_node*>::iterator it=curvect->begin();
			if (merge_tree==NULL) merge_tree=new xptrChanneledMerge(getNextDescriptorOfSameSortXptr,true);
			while (it!=curvect->end())
			{
				//xptr tmp=(following)?getNextNDNode(base,*it):getPreviousNANode(base,*it);
				cur=getFirstDescandantByScheme(tmp,*it);				
				if (cur!=XNULL) merge_tree->addChannel(cur);
				it++;
			}
			
			cur=merge_tree->getNextNode();			
			ancestor=tmp;
			/*for (i=0;i<curvect->size();i++)
			{
				cur=getFirstDescandantByScheme(tmp,(*curvect)[i]);
				if (cur!=XNULL)
				{
					curpos=i;
					
					break;
				}
			}*/


		}
		/*if (cur==XNULL)
		{
			t.set_eos();
			return;
		}*/
	}
    t.copy(tuple_cell::node(cur));
	cur=merge_tree->getNextNode();
	if (cur!=XNULL && nid_cmp_effective(cur,ancestor)!=2)
	{
		cur=XNULL;
		merge_tree->clear_merge();
	}
	
    
}
void PPAxisDescendant::next_text(tuple &t)
{
   next_qname_and_text(t,NULL,NULL,text,comp_type);
}
void PPAxisDescendant::next_wildcard_ncname_star(tuple &t)
{
    char * uri=st_ct.get_uri_by_prefix(nt_data.ncname,element) ;
	next_qname_and_text(t,uri,NULL,element,comp_uri_type);
}

void PPAxisDescendant::next_wildcard_star_ncname(tuple &t)
{
    next_qname_and_text(t,NULL,nt_data.ncname.n,element,comp_local_type);
}
void PPAxisDescendant::next_wildcard_star(tuple &t)
{
	while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XP0020);
		
        cur = child.get(t).get_node();
		if (cur!=XNULL)
		{
			CHECKP(cur);
			if(!self || (GETBLOCKBYNODE(cur))->snode->type!=element)
				cur=getFirstByOrderElementChild(cur);
		}
    }
	descstack.push_back(cur);
    t.copy(tuple_cell::node(cur));
    CHECKP(cur);
    xptr tmp = getFirstByOrderElementChild(cur);
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
			CHECKP(tmp);
			if ((GETBLOCKBYNODE(tmp))->snode->type!=element)
				tmp=GETRIGHTPOINTER(tmp);
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
PPAxisDescendantOrSelf::PPAxisDescendantOrSelf(variable_context *_cxt_, 
                         PPOpIn _child_,
                         NodeTestType _nt_type_,
						 NodeTestData _nt_data_):PPAxisDescendant(_cxt_, _child_, _nt_type_, _nt_data_,true)
{
 
}
PPAxisDescendantAttr::PPAxisDescendantAttr(variable_context *_cxt_, 
                         PPOpIn _child_,
                         NodeTestType _nt_type_,
						 NodeTestData _nt_data_):PPAxisDescendant(_cxt_, _child_, _nt_type_, _nt_data_)
{
 
}
void PPAxisDescendantAttr::next_text(tuple &t)
{
   while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XP0020);
    }
	next_wildcard_star(t);
}
void PPAxisDescendantAttr::next_node(tuple &t)
{
	/*while (true)
    {
        child.op->next(t);
        if (t.is_eos()) return;
        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XP0020);
    }*/
	next_wildcard_star(t);
}
void PPAxisDescendantAttr::next_qname(tuple &t)
{
	char * uri=st_ct.get_uri_by_prefix(nt_data.qname.Prefix,attribute) ;
	next_qname_and_text(t,uri,nt_data.qname.LocalPart.n,attribute,comp_qname_type);
}
void PPAxisDescendantAttr::next_wildcard_ncname_star(tuple &t)
{
    char * uri=st_ct.get_uri_by_prefix(nt_data.ncname,attribute) ;
	next_qname_and_text(t,uri,NULL,attribute,comp_uri_type);
}

void PPAxisDescendantAttr::next_wildcard_star_ncname(tuple &t)
{
    next_qname_and_text(t,NULL,nt_data.ncname.n,attribute,comp_local_type);
}

void PPAxisDescendantAttr::next_wildcard_star(tuple &t)
{
	while (cur == XNULL)
    {
        child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION(XP0020);
		
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
