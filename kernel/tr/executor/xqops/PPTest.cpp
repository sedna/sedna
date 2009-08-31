/*
 * File:  PPTest.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <ios>
#include <sstream>

#include "common/sedna.h"

#include "tr/executor/xqops/PPTest.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/strings/strings.h"
#include "tr/pstr/pstr.h"

using namespace std;
//#include <atlstr.h>
//#define USE_DTSEARCH_NAMESPACE
//#include <dtsfc.h> 
//#include "FTsearch.h"
//#include "FTindex.h"
//op_str_buf req_buf;
//SednaSearchJob sj;
bool fit;
//#include <dtsfclib.h>

PPTest::PPTest(dynamic_context *_cxt_,
               operation_info _info_,
               PPOpIn _seq_) : PPIterator(_cxt_, _info_),
                               seq(_seq_) 
{
	this->test_fun=&PPTest::checkTreeConsistency;
}

PPTest::~PPTest()
{
	delete seq.op;
	seq.op = NULL;
	
}

void PPTest::do_open ()
{
    seq.op->open();
}

void PPTest::do_reopen()
{
    seq.op->reopen();
}

void PPTest::do_close()
{
    seq.op->close();
}
void PPTest::do_next (tuple &t)
{
	tuple t1(seq.ts);
	seq.op->next(t1);
	//Preliminary node analysis
	if (t1.is_eos()) 
	{
		t.set_eos();
		return;
	}
	tuple_cell& tc= t1.cells[0];
	if (!tc.is_node())
	{
		throw XQUERY_EXCEPTION(SE2031);
	}
	xptr node=tc.get_node();
	CHECKP(node);
	std::ostringstream strg;
	strg<< "checking the node: xptr=(" << node.layer<< ",0x"<< hex << node.addr <<")\n";
	try
	{
		(this->*test_fun)(node);
		strg<<"true";
		
	}
	catch(SednaException &e) 
	{
       strg << e.getMsg() << endl;
	   throw ;
    }
	t.copy(tuple_cell::atomic_deep(xs_string,strg.str().c_str())) ; 
	
	/*while (true)
	{
		seq.op->next(t);
		//Preliminary node analysis
		if (t.is_eos()) 
		{
			//t.set_eos();
			return;
		}
		tuple_cell& tc= t.cells[0];
		if (!tc.is_node())
		{
			//throw USER_EXCEPTION(SE2031);
			req_buf.set(tc);
		}
		else
		{
			xptr node=tc.get_node();
			CHECKP(node);
			if (checkFT(node)) return;
		}
	}*/
	/*if (fit)
	{
		seq.op->next(t);
		//Preliminary node analysis
		if (t.is_eos()) 
		{
			//t.set_eos();
			return;
		}
		tuple_cell& tc= t.cells[0];
		int command;
		if (!tc.is_node())
		{
			//throw USER_EXCEPTION(SE2031);
			command=tc.get_xs_integer();
		}
		seq.op->next(t);
		//Preliminary node analysis
		if (t.is_eos()) 
		{
			//t.set_eos();
			return;
		}
		tc= t.cells[0];
		if (!tc.is_node())
		{
			//throw USER_EXCEPTION(SE2031);
			if (command==1)
			{
				req_buf.set(tc);
				sj=se_new SednaSearchJob(&seq);
				sj->set_request(tc);
			}
			else if (command==2)
			{
				req_buf.set(tc);
				sj=se_new SednaSearchJob();
				sj->set_request(tc);
				seq.op->next(t);
				tc= t.cells[0];
				sj->set_index(tc);
			}
			else
			{
				req_buf.set(tc);
				SednaIndexJob si(&seq);
				si.set_index_name(tc);
				si.create_index();
				si.SetActionCompress();
				si.Execute();
				t.set_eos();
				return;
			}
		}
		fit=false;
	}
	sj->get_next_result(t);
	if (t.is_eos())
		fit=true;
	//int res= checkFT(seq);
	//t.copy(tuple_cell::atomic(res));	
	*/
}

PPIterator* PPTest::do_copy(dynamic_context *_cxt_)
{
	PPTest *res ;
	res = se_new PPTest(_cxt_, info, seq);
	res->seq.op = seq.op->copy(_cxt_);
	return res;
}
xptr get_root (xptr node)
{
	CHECKP(node);
	xptr tmp=node;
	while (true)
	{
      if (((n_dsc*)XADDR(tmp))->pdsc==XNULL) return tmp;
	  tmp=removeIndirection(((n_dsc*)XADDR(tmp))->pdsc);
	}	
}
bool is_same_root(xptr x, xptr y)
{
	return get_root(x)==get_root(y);
}
/*
bool PPTest::checkFT(xptr node)
{
	dtsSearchJob *searchJob = se_new dtsSearchJob;
	dtsSearchResults *results = se_new dtsSearchResults();
    searchJob->resultsHandle = results->getHandle();
	//searchJob->pReportCallBack = SearchingCallback;
	SampleDataSource sample(node) ;
	dtsDataSource dataSource;
	sample.makeInterface(dataSource);
	searchJob->dataSourceToSearch = &dataSource;
	searchJob->action.searchFiles = true;
	strcpy(searchJob->request, req_buf.c_str());
	short result = 0;
    dtssDoSearchJob(*searchJob, result);
    delete searchJob;
	bool res=results->getCount()>0;
	delete results;
	return res;
}
int PPTest::checkFT(PPOpIn _seq_)
{
	dtsSearchJob *searchJob = se_new dtsSearchJob;
	dtsSearchResults *results = se_new dtsSearchResults();
    searchJob->resultsHandle = results->getHandle();
	//searchJob->pReportCallBack = SearchingCallback;
	SampleDataSource2 sample(_seq_) ;
	dtsDataSource dataSource;
	sample.makeInterface(dataSource);
	searchJob->dataSourceToSearch = &dataSource;
	searchJob->action.searchFiles = true;
	strcpy(searchJob->request, req_buf.c_str());
	short result = 0;
    dtssDoSearchJob(*searchJob, result);
	int res=results->getCount();
	delete searchJob;
	
	delete results;
	return res;
}*/
void PPTest::checkTreeConsistency(xptr node)
{
	CHECKP(node);
	n_dsc* node_d=(n_dsc*)XADDR(node);
	t_nid nd=node_d->nid;
	schema_node_cptr scn=(GETBLOCKBYNODE(node))->snode;
	node_blk_hdr* n_blk=GETBLOCKBYNODE(node);
#ifdef DESC_CONSIST
	if (node_d->desc_prev!=0)
	{
		n_dsc* pr_n=(n_dsc*)((char*)n_blk + node_d->desc_prev );
		if (pr_n->desc_next!=CALCSHIFT(node_d,n_blk) || pr_n==node_d)
			throw XQUERY_EXCEPTION(SE2030);	  
	}
	if (node_d->desc_next!=0)
	{
		n_dsc* pr_n=(n_dsc*)((char*)n_blk + node_d->desc_next );
		if (pr_n->desc_prev!=CALCSHIFT(node_d,n_blk) || pr_n==node_d)
			throw XQUERY_EXCEPTION(SE2030);	  
	}
	
	//1. indirection test
	xptr indir=node_d->indir;
	if (removeIndirection(indir)!=node)
		throw XQUERY_EXCEPTION(SE2030);
	//2. parent test
	CHECKP(node);
	xptr par_indir=node_d->pdsc;
	xptr parent;
	n_dsc* prev_dsc=getPreviousDescriptorOfSameSort(node_d);
	xptr prev_x=(prev_dsc==NULL)?XNULL:ADDR2XPTR(prev_dsc);
    if (par_indir!=XNULL) 
	{
		parent=removeIndirection(par_indir);
		if (!nid_ancestor(parent,node))
			throw XQUERY_EXCEPTION(SE2025);
		if (prev_dsc==NULL|| prev_dsc->pdsc!=par_indir)
		{
			CHECKP(parent);
			xptr* ptr=elementContainsChild((n_dsc*)XADDR(parent),scn->name,scn->type,scn->get_xmlns());
			if (ptr==NULL || *ptr!=node)
				throw XQUERY_EXCEPTION(SE2026); 
		}
	}
	//3. left siblings + nid comparison
	CHECKP(node);
	xptr left=node_d->ldsc;
	if (left!=XNULL)
	{
		CHECKP(left);
		if (((n_dsc*)XADDR(left))->rdsc!=node)
			throw XQUERY_EXCEPTION(SE2027);
		if (nid_cmp(left,node)>=0)
			throw XQUERY_EXCEPTION(SE2028);
	}
	//4. descriptor's order
	if (prev_x!=XNULL && scn->type!=document)
	{
		bool lt=nid_cmp(prev_x,node)<0;
		CHECKP(prev_x);
		if (!lt || getNextDescriptorOfSameSort(prev_dsc)!=node_d   )
		{
			if (is_same_root(prev_x,node))
				throw XQUERY_EXCEPTION(SE2029);
		}
	}
#endif
#ifdef PSTR_CONSIST
	//5.1 nid pstr consistency
	CHECKP(node);
	if (nd.size==0&& is_last_shft_in_blk(*((xptr*)nd.prefix)))
			check_blk_consistency(*((xptr*)nd.prefix));
	//5.2 nid pstr consistency
	CHECKP(node);
	if (scn->textcnt&& ((t_dsc*)node_d)->data!=XNULL&&((t_dsc*)node_d)->size<=PSTRMAXSIZE && is_last_shft_in_blk(((t_dsc*)node_d)->data))
	{
		CHECKP(node);
		check_blk_consistency(((t_dsc*)node_d)->data);	
	}
#endif	
	//recursive walkthrough
	CHECKP(node);
	xptr child=giveFirstByOrderChild(node,CHILDCOUNT(node));
	while (child!=XNULL)
	{
		checkTreeConsistency(child);
		CHECKP(child);
		child=((n_dsc*)XADDR(child))->rdsc;
	}
}
