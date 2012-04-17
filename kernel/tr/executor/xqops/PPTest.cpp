/*
 * File:  PPTest.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <ios>
#include <sstream>

#include "common/sedna.h"

#include "tr/executor/xqops/PPTest.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/executor/base/visitor/PPVisitor.h"
#include "tr/executor/xpath/XPathTypes.h"

#include "tr/strings/strings.h"
#include "tr/pstr/pstr.h"

#include "tr/structures/nodeutils.h"
#include "tr/mo/nodemoutils.h"
#include "tr/opt/alg/Predicates.h"

#include "tr/opt/alg/DataGraph.h"
#include "tr/executor/base/SCElementProducer.h"

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
               PPOpIn _seq_) : PPIterator(_cxt_, _info_, "PPTest"),
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
	strg<< "checking the node: xptr=(" << node.layer<< ",0x"<< hex << node.getOffs() <<")\n";
	try
	{
		test_fun(node);
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

bool is_same_root(xptr x, xptr y)
{
	return getRootNode(x)==getRootNode(y);
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

using namespace internal;

void PPTest::checkTreeConsistency(xptr node)
{
	CHECKP(node);
	t_nid nd = Node(node).getNID();
	schema_node_cptr scn=getSchemaNode(node);
#ifdef DESC_CONSIST
	xptr dprev = nodeGetPrev(node);
	if (dprev != XNULL) {
		if (nodeGetNext(dprev) != node) {
			throw XQUERY_EXCEPTION(SE2030);
		}
	}
    xptr dnext = nodeGetNext(node);
    if (dnext != XNULL) {
        if (nodeGetPrev(dnext) != node) {
            throw XQUERY_EXCEPTION(SE2030);
        }
    }
	//1. indirection test
	xptr indir = getIndirectionSafeCP(node);
	if (indirectionDereferenceCP(indir)!=node)
		throw XQUERY_EXCEPTION(SE2030);
	//2. parent test
	CHECKP(node);
	xptr par_indir = nodeGetParentIndirection(node);
	xptr parent;
	xptr prev = getPreviousDescriptorOfSameSort(node);
    if (par_indir != XNULL)
	{
		parent=indirectionDereferenceCP(par_indir);
		if (!nid_ancestor(parent,node))
			throw XQUERY_EXCEPTION(SE2025);
		if(prev != XNULL) CHECKP(prev);
		if (prev == XNULL || nodeGetParentIndirection(prev) != par_indir)
		{
			CHECKP(parent);
			xptr ptr = getFirstChildBySchema(parent, scn);
			if (ptr == XNULL || ptr != node)
				throw XQUERY_EXCEPTION(SE2026);
		}
	}
	//3. left siblings + nid comparison
	CHECKP(node);
	xptr left = nodeGetLeftSibling(node);
	if (left!=XNULL)
	{
		CHECKP(left);
		if (nodeGetRightSibling(left)!=node)
			throw XQUERY_EXCEPTION(SE2027);
		if (nid_cmp(left,node)>=0)
			throw XQUERY_EXCEPTION(SE2028);
	}
	//4. descriptor's order
	if (prev != XNULL && scn->type != document)
	{
		bool lt=nid_cmp(prev,node)<0;
		CHECKP(prev);
		if (!lt || getNextDescriptorOfSameSort(prev) != node)
		{
			if (is_same_root(prev, node))
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
	if (scn->textcnt > 0 && CommonTextNode(node).isPstr()) {
		CHECKP(node);
		check_blk_consistency(CommonTextNode(node).getTextPointer());
	}
#endif
	//recursive walkthrough
	CHECKP(node);
	xptr child=getFirstChild(node);
	while (child!=XNULL)
	{
		checkTreeConsistency(child);
		CHECKP(child);
		child=nodeGetRightSibling(child);
	}
}

void PPTest::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    seq.op->accept(v);
    v.pop();
}

void PPDataGraph::do_next(tuple& t)
{
    static DataGraphMaster dgm;

    graphString.op->next(t);

    if (!t.eos) {
        TextBufferReader reader(text_source_tuple_cell(atomize(t.cells[0])));
        reader.read();

        std::string list(reader.buffer, reader.size);
        AutoSchemeList scml(list.c_str());

//        DataRoot dr(scml.get()->at(0).internal.list);
//        pe::Path x(scml.get()->at(1).internal.list);

        DataGraph *dg = dgm.createGraphFromLR(scml.get());

        dg->precompile();

        IElementProducer * rootProducer = SCElementProducer::getVirtualRoot(XNULL);
        IElementProducer * dgElement = dg->toXML(rootProducer);

        t.cells[0] = dgElement->close();
//        t.cells[0] = tuple_cell::atomic_deep(xs_string, dg->toLRString().c_str());
    }
}

void PPDataGraph::do_accept(PPVisitor& v)
{
    //   v.visit(this);
}

void PPDataGraph::do_close()
{
    graphString.op->close();
}

PPIterator* PPDataGraph::do_copy(dynamic_context* _cxt_)
{
    return new PPDataGraph(_cxt_, info, PPOpIn(graphString.op->copy(_cxt_), 1));
}

void PPDataGraph::do_open()
{
    graphString.op->open();
}

void PPDataGraph::do_reopen()
{
    graphString.op->reopen();
}

PPDataGraph::PPDataGraph(dynamic_context* _cxt_, operation_info _info_, PPOpIn _seq_)
    : PPIterator(_cxt_, _info_, "PPDataGraph"), graphString(_seq_)
{
}

PPDataGraph::~PPDataGraph()
{
    delete graphString.op;
    graphString.op = NULL;
}



