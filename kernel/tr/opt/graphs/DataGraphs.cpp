#include "DataGraphs.h"
#include "DataGraphCollection.h"
#include "tr/models/XmlConstructor.h"

#include <algorithm>

using namespace opt;

DataGraph::DataGraph(DataGraphMaster* _owner) : owner(_owner)
{
    U_ASSERT(_owner != NULL);
    memset(predicates, 0, sizeof(predicates[0]) * MAX_GRAPH_SIZE);
    memset(dataNodes, 0, sizeof(dataNodes[0]) * MAX_GRAPH_SIZE);
}

void DataGraphWrapper::update()
{
    nodes.clear();
    out.clear();
    in.clear();
    predicates.clear();

    outTuples.clear();
    inTuples.clear();

    FOR_ALL_GRAPH_ELEMENTS(dg->dataNodes, i) {
        nodes.push_back(dg->dataNodes[i]);

        if ((dg->dataNodes[i]->indexBit & dg->outputNodes) > 0) {
            out.push_back(dg->dataNodes[i]);

            if (dg->dataNodes[i]->varTupleId != invalidTupleId) {
                outTuples.insert(dg->dataNodes[i]->varTupleId);
            }
        };

        if ((dg->dataNodes[i]->indexBit & dg->inputNodes) > 0) {
            in.push_back(dg->dataNodes[i]);
            U_ASSERT(dg->dataNodes[i]->varTupleId != invalidTupleId);
            inTuples.insert(dg->dataNodes[i]->varTupleId);
        };
    };

    FOR_ALL_GRAPH_ELEMENTS(dg->predicates, i) {
        predicates.push_back(dg->predicates[i]);
    };

    std::sort(nodes.begin(), nodes.end());
    std::sort(out.begin(), out.end());
    std::sort(in.begin(), in.end());
    std::sort(predicates.begin(), predicates.end());
}

DataGraphWrapper::DataGraphWrapper(DataGraph* _dg) : dg(_dg)
{
    nodes.reserve(MAX_GRAPH_SIZE / 2);
    in.reserve(MAX_GRAPH_SIZE / 2);
    out.reserve(MAX_GRAPH_SIZE / 2);
    predicates.reserve(MAX_GRAPH_SIZE / 2);

    update();
}

void DataGraphWrapper::rebuild()
{
    DataGraphBuilder builder;
    
    builder.nodes = nodes;
    builder.out = out;
    builder.predicates = predicates;

    builder.make(dg->owner, dg);

    update();
}

DataGraph* DataGraphBuilder::make(DataGraphMaster* master, DataGraph* graph)
{
    PlanDesc lastIndex = 0;

    U_ASSERT(nodes.size() + predicates.size() <= MAX_GRAPH_SIZE);

    memset(graph->dataNodes, 0, sizeof(graph->dataNodes[0]) * MAX_GRAPH_SIZE);
    memset(graph->predicates, 0, sizeof(graph->predicates[0]) * MAX_GRAPH_SIZE);
    
    graph->outputNodes = 0;
    graph->inputNodes = 0;
    
    for (DataNodeList::const_iterator it = nodes.begin(); it != nodes.end(); ++it) {
        DataNode * n = (*it);
        n->setIndex(lastIndex);
        n->parent = graph;
        graph->dataNodes[lastIndex] = n;
        
        if (std::binary_search(out.begin(), out.end(), n))
        {
            graph->outputNodes |= n->indexBit;
        };

        if (n->type == opt::DataNode::dnExternal) {
            graph->inputNodes |= n->indexBit;
        }

        if (n->varTupleId != invalidTupleId) {
            master->addVariable(n);
        }
        
        lastIndex++;
    };

    for (PredicateList::const_iterator it = predicates.begin(); it != predicates.end(); ++it) {
        (*it)->setIndex(lastIndex);
        graph->predicates[lastIndex] = (*it);
        lastIndex++;
    };

    return graph;
}

DataGraph* DataGraphBuilder::build(DataGraphMaster* master)
{
    DataGraph * result = new DataGraph(master);
    make(master, result);
    master->allGraphs.push_back(result);
    return result;
}

void DataGraphIndex::update()
{
    allPredicates = 0;

    memset(nodes, 0, sizeof(nodes[0]) * MAX_GRAPH_SIZE);
    memset(predicates, 0, sizeof(predicates[0]) * MAX_GRAPH_SIZE);

    unsigned aI = 0;

    FOR_ALL_GRAPH_ELEMENTS(dg->dataNodes, i) {
        DataNode * n = dg->dataNodes[i];
        DataNodeIndex * nidx = nodes + i;

        nidx->output = (n->indexBit & dg->outputNodes) > 0;
        nidx->input = (n->indexBit & dg->inputNodes) > 0;
        
        n->absoluteIndex = aI++;
        nidx->p = n;
        nidx->predicates = 0;
    };
    
    FOR_ALL_GRAPH_ELEMENTS(dg->predicates, i) {
        Predicate * p = dg->predicates[i];
        PredicateIndex * pidx = predicates + i;

        pidx->p = p;
        pidx->dataNodeMask = 0;

        for (DataNodeList::const_iterator it = p->dataNodeList.begin(); it != p->dataNodeList.end(); ++it) {
            pidx->dataNodeMask |= (*it)->indexBit;
            nodes[(*it)->index].predicates |= p->indexBit;
        }

        pidx->evaluateAfter = 0;
        for (PredicateList::const_iterator it = p->evaluateAfter.begin(); it != p->evaluateAfter.end(); ++it) {
            pidx->evaluateAfter |= (*it)->indexBit;
        }
        
        pidx->neighbours = 0;

        allPredicates |= dg->predicates[i]->indexBit;
    };

    FOR_ALL_GRAPH_ELEMENTS(dg->predicates, i) {
        Predicate * p = dg->predicates[i];
        PredicateIndex * pidx = predicates + i;
        
        for (DataNodeList::const_iterator jt = p->dataNodeList.begin(); jt != p->dataNodeList.end(); ++jt) {
            pidx->neighbours |= nodes[(*jt)->index].predicates;
        }

        pidx->neighbours &= ~p->indexBit;
    };
}


DataGraphIndex::DataGraphIndex(DataGraph* _dg)
  : dg(_dg), allPredicates(0)
{
    update();
}

XmlConstructor & DataGraph::toXML(XmlConstructor & producer) const
{
    producer.openElement(CDGQNAME("datagraph"));

    FOR_ALL_GRAPH_ELEMENTS(dataNodes, i) {
        dataNodes[i]->toXML(producer);
    };

    FOR_ALL_GRAPH_ELEMENTS(predicates, i) {
        predicates[i]->toXML(producer);
    };

    producer.closeElement();

    return producer;
}

XmlConstructor & DataNode::toXML(XmlConstructor& element) const
{
    const char * nodetype = NULL;

    switch(type) {
        case dnDatabase : nodetype = "root"; break;
        case dnFreeNode : nodetype = "free"; break;
        case dnConst : nodetype = "const"; break;
        case dnExternal : nodetype = "ext"; break;
        case dnAlias : nodetype = "alias"; break;
        case dnReplaced : nodetype = "REMOVED"; break;
    };

    element.openElement(CDGQNAME("node"));

    element.addAttributeValue(CDGQNAME("type"), nodetype);
    element.addAttributeValue(CDGQNAME("index"), tuple_cell::atomic_int(index));

    if (varTupleId != opt::invalidTupleId) {
        element.addAttributeValue(CDGQNAME("varId"), tuple_cell::atomic_int(varTupleId));

        VariableInfo & vinfo = parent->owner->getVariable(varTupleId);

        if (!vinfo.name.empty()) {
            element.addAttributeValue(CDGQNAME("varName"), vinfo.name);
        };
    };

    if ((parent->outputNodes & this->indexBit) > 0) {
        element.addAttributeValue(CDGQNAME("output"), tuple_cell::atomic(true));
    };

    switch(type) {
        case dnDatabase :
            element.addElementValue(CDGQNAME("root"), root.toLRString());
            element.addElementValue(CDGQNAME("path"), path.toXPathString());
            break;
        case dnAlias :
            element.addElementValue(CDGQNAME("source"), tuple_cell::atomic_int(aliasFor->index));
            break;
        case dnReplaced :
            element.addElementValue(CDGQNAME("with"), tuple_cell::atomic_int(replacedWith->index));
            break;
        case dnConst :
            U_ASSERT(!sequence.isnull());
            for (MemoryTupleSequence::const_iterator it = sequence->begin(); it != sequence->end(); ++it) {
                element.addElementValue(CDGQNAME("value"), *it);
            };
            break;
        case dnFreeNode :
            break;
        case dnExternal :
            break;
    };

    element.closeElement();

    return element;
}


