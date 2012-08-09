/*
 * File:  []
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _VARIABLE_GRAPH_H
#define _VARIABLE_GRAPH_H

#include <map>
#include <vector>
#include <string>
#include <sstream>

#include "tr/opt/OptTypes.h"
#include "tr/opt/types/TupleInfo.h"

#include "tr/executor/por2qep/scheme_tree.h"
#include "tr/models/XmlConstructor.h"

namespace opt {

/**
 * @note There is absolutely no need for var names to be unique.
 * Name scope problems are resolved during static query analisys.
 */
class VariableUsageGraph : public IXMLSerializable {
public:
    VariableUsageGraph();
    virtual ~VariableUsageGraph();

    TupleInfoMap variableMap;
    MemoryTupleSequencePtr alwaysTrueSequence;

    TupleInfo & addVariableDeclaration(TupleId tid, rqp::RPBase * op, DataNode * dataNode = NULL);
    TupleInfo & addVariableUsage(TupleId tid, rqp::RPBase * op, DataNode * dataNode = NULL);
    TupleInfo & addVariableName(TupleId tid, const std::string & name);
    TupleInfo & mergeVariables(TupleId alias, TupleId master);

    void addVariableDataNode(opt::DataNode* dn);
    void removeVariableDataNode(DataNode* p);

    void cleanup();

    TupleInfo & getVariable(TupleId tid)
    {
        U_ASSERT(variableMap.find(tid) != variableMap.end());

        TupleInfo & info = variableMap.at(tid);

        while (info.pointsTo != opt::invalidTupleId) {
            info = variableMap.at(info.pointsTo);
        };

        return info;
    };

    virtual XmlConstructor& toXML(XmlConstructor& constructor) const;
};

}

#endif /* _VARIABLE_GRAPH_H */
