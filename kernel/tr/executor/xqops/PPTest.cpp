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
#include "tr/opt/path/XPathTypes.h"

#include "tr/strings/strings.h"
#include "tr/pstr/pstr.h"

#include "tr/structures/nodeutils.h"
#include "tr/mo/nodemoutils.h"

#include "tr/opt/graphs/Predicates.h"
#include "tr/opt/graphs/DataGraphs.h"

#include "tr/models/SCElementProducer.h"

#include "tr/opt/path/XPathLookup.h"
#include "tr/models/XmlConstructor.h"

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
                               seq(_seq_), data(NULL)
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

static
std::string schemaPath(schema_node_cptr snode) {
    std::stringstream path;
    std::stack<schema_node_cptr> path_sn;

    while (snode.found()) {
        path_sn.push(snode);
        snode = snode->parent;
    };

    while (!path_sn.empty()) {
        path << path_sn.top()->get_qname().getColonizedName().c_str() << "/";
        path_sn.pop();
    }

    return path.str();
};

void PPTest::do_next (tuple &t)
{
/*

// Path parsing
   
    seq.op->next(t);

    if (t.eos) {
        return;
    }

    TextBufferReader reader(text_source_tuple_cell(atomize(t.cells[0])));
    reader.read();
    std::string list(reader.buffer, reader.size);
    AutoSchemeList scml(list.c_str());
    pe::Path path(scml.get());
    pe::AtomizedPath ap = path.atomize();

    t.cells[0] = tuple_cell::atomic_deep(xs_string, ap.reverse().__toString().c_str());
    t.eos = false;

// Schema traverse
  
    __test_tmp * snodes = NULL;
    
    if (data != NULL) {
        snodes = (__test_tmp *) data;
    } else {
        seq.op->next(t);

        if (t.eos) {
            return;
        }
    
        DataRoot root(DataRoot::drt_document, "auction");
        const char * name = t.cells[0].get_str_mem();
        doc_schema_node_cptr dataroot_snode(root.getSchemaNode().ptr());
        snodes = new __test_tmp();
        dataroot_snode->find_descendant(name, element, &snodes->x);
        snodes->i = 0;
        data = snodes;
    };

    if (snodes->i == snodes->x.size()) {
        delete snodes;
        data = NULL;
        return do_next(t);
    };

    schema_node_cptr snode = snodes->x.at(snodes->i);

    std::stringstream path;
    std::stack<schema_node_cptr> path_sn;

    while (snode.found()) {
        path_sn.push(snode);
        snode = snode->parent;
    };

    while (!path_sn.empty()) {
        path << path_sn.top()->get_qname().getColonizedName().c_str() << "/";
        path_sn.pop();
    }

    t.cells[0] = tuple_cell::atomic_deep(xs_string, path.str().c_str());
    t.eos = false;
    snodes->i++;
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
/*
    static opt::VariableUsageGraph dgm;

    if (data != NULL) {
        phop::ITupleOperator * op = (phop::ITupleOperator *) data;
        
        op->next();
        
        if (op->get().eos) {
            data = NULL;
            u_ftime(&t_end);

            t.cells[0] = tuple_cell::atomic_deep(xs_string,
              (to_string(t_end - t_start) + " " +
               to_string(t_end - t_opt) + " " +
               to_string(t_opt - t_start) + " ").c_str());

            return;
        };
        
        t.cells[0] = op->get().cells[idx];

        return;
    };

    inputString.op->next(t);

    if (!t.eos) {
        u_ftime(&t_start);
      
        TextBufferReader reader(text_source_tuple_cell(atomize(t.cells[0])));
        reader.read();

        std::string list(reader.buffer, reader.size);
        AutoSchemeList scml(list.c_str());

//        DataRoot dr(scml.get()->at(0).internal.list);
//        pe::Path x(scml.get()->at(1).internal.list);

        opt::DataGraph *dg = dgm.createGraphFromLR(scml.get());

        IElementProducer * rootProducer = SCElementProducer::getVirtualRoot(XNULL);
//        IElementProducer * dgElement = dg->toXML(rootProducer);

        phop::GraphExecutionBlock * block  = new phop::GraphExecutionBlock();
        phop::GraphExecutionBlock::push(block);

        phop::ITupleOperator * op = dgm.compile(dg);
        
        idx = phop::GraphExecutionBlock::current()->resultMap[dg->outputNodes.at(0)->absoluteIndex];

        block->context = new phop::ExecutionContext();
        block->context->collation = cxt->get_static_context()->get_default_collation();
        
        op->reset();

        phop::MappedTupleIn mt = dynamic_cast<phop::BinaryTupleOperator *>(
            dynamic_cast<phop::UnaryTupleOperator *>(op)->__in().op
            )->__left();
        
        op = mt.op;
        idx = 4;
        data = op;

        u_ftime(&t_opt);

//        do_next(t);
        XmlConstructor vroot(VirtualRootConstructor(0));
        t.cells[0] = op->toXML(vroot).getLastChild();
        PPOpIn op(dgm.compile(dg), 1);
    }
*/
}

PPDataGraph::PPDataGraph(dynamic_context* _cxt_, operation_info _info_, PPOpIn _seq_)
: PPInternalFunction(_cxt_, _info_, _seq_), data(NULL) {}


PPIterator* PPDataGraph::do_copy(dynamic_context* _cxt_)
{
    return new PPDataGraph(_cxt_, info, PPOpIn(inputString.op->copy(_cxt_), 1));
}


PPInternalFunction::PPInternalFunction(dynamic_context* _cxt_, operation_info _info_, PPOpIn _seq_)
    : PPIterator(_cxt_, _info_, "InternalFunction"), inputString(_seq_) { }

PPInternalFunction::~PPInternalFunction()
{
    delete inputString.op;
    inputString.op = NULL;
}

void PPInternalFunction::do_open()
{
    inputString.op->open();
}

void PPInternalFunction::do_reopen()
{
    inputString.op->reopen();
}

void PPInternalFunction::do_accept(PPVisitor& v)
{
    // inputString.op->
}

void PPInternalFunction::do_close()
{
    inputString.op->close();
}



PPAbsPathExec::PPAbsPathExec(dynamic_context* _cxt_, operation_info _info_, PPOpIn _seq_)
    : PPInternalFunction(_cxt_, _info_, _seq_) { }

PPIterator* PPAbsPathExec::do_copy(dynamic_context* _cxt_)
{
    return new PPAbsPathExec(_cxt_, info, inputString);
}

void PPAbsPathExec::do_next(tuple& t)
{
    inputString.op->next(t);

    if (!t.eos) {
        TextBufferReader reader(text_source_tuple_cell(atomize(t.cells[0])));
        reader.read();

        std::string list(reader.buffer, reader.size);
        AutoSchemeList scml(list.c_str());

        pe::Path path(scml.get());
    }
}

PPSchemaScan::PPSchemaScan(dynamic_context* _cxt_, operation_info _info_, PPOpIn _seq_)
    : PPInternalFunction(_cxt_, _info_, _seq_), data(NULL) { }

PPIterator* PPSchemaScan::do_copy(dynamic_context* _cxt_)
{
    return new PPSchemaScan(_cxt_, info, inputString);
}

struct __test_tmp {
    SchemaNodePtrList x;
    std::vector<schema_node_xptr>::size_type i;
};

void PPSchemaScan::do_next(tuple& t)
{
    __test_tmp * snodes = NULL;

    if (data != NULL) {
        snodes = (__test_tmp *) data;
    } else {
        inputString.op->next(t);

        if (t.eos) {
            return;
        }

        DataRoot root(DataRoot::drt_document, "auction");
        TextBufferReader reader(text_source_tuple_cell(atomize(t.cells[0])));
        reader.read();
        std::string list(reader.buffer, reader.size);
        AutoSchemeList scml(list.c_str());
        pe::Path path(scml.get());
        pe::SchemaLookup sclkp(path);
        snodes = new __test_tmp();

        sclkp.compile();
//        sclkp.findSomething(root, &snodes->x);
        sclkp.execute(root.getSchemaNode(), &snodes->x);

        snodes->i = 0;
        data = snodes;
    };

    if (snodes->i == snodes->x.size()) {
        delete snodes;
        data = NULL;
        return do_next(t);
    };

    schema_node_cptr snode = snodes->x.at(snodes->i);

    t.cells[0] = tuple_cell::atomic_deep(xs_string, schemaPath(snode).c_str());
    t.eos = false;
    snodes->i++;
}

