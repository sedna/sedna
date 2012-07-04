#ifndef _PLAN_ALGORITHMS_H_
#define _PLAN_ALGORITHMS_H_

#include "AllOperations.h"

#include "tr/opt/graphs/DataGraphCollection.h"
#include "tr/opt/graphs/DataGraphs.h"

namespace rqp {

struct VarStatInfoItem {
    enum influence_t {
        inf_op,
        inf_function,
        inf_not,
        inf_dg,
        inf_no_null_preserve,
    } influence;

    rqp::RPBase * rp;
    opt::DataNode * dn;

    VarStatInfoItem()
      : influence(inf_op), rp(NULL), dn(NULL) {};
      
    VarStatInfoItem(enum influence_t _influence, rqp::RPBase * _rp, opt::DataNode * _dn)
      : influence(_influence), rp(_rp), dn(_dn) {};
};

typedef std::list<VarStatInfoItem> VarPath;

struct VarStatInfo {
    VarPath path;

    bool used;
    bool singleton;

    VarStatInfo() : used(false), singleton(false) {};
};

typedef std::map<opt::TupleId, VarStatInfo> VarInfoMap;

struct PlanRewriter {
    rqp::RPBase* inputOp;
    rqp::OperationList traverseStack;
    VarInfoMap varMap;

    std::vector<opt::TupleId> scopes;
    std::vector<size_t> scopeMarkers;

    VarStatInfo & declVar(opt::TupleId varid)
    {
        VarInfoMap::iterator it = varMap.find(varid);

        scopes.push_back(varid);

        if (it == varMap.end()) {
            return varMap.insert(VarInfoMap::value_type(varid, VarStatInfo())).first->second;
        } else {
            return it->second;
        };
    };

    RPBase * getParent()
    {
        if (traverseStack.size() < 2) {
            return NULL;
        }

        return traverseStack.at(traverseStack.size() - 2);
    };
    
    void openScope()
    {
        scopeMarkers.push_back(scopes.size());
    };
    
    void closeScope()
    {
        while (scopes.size() > scopeMarkers.back()) {
            varMap.at(scopes.back()).path.pop_back();
            scopes.pop_back();
        };

        scopeMarkers.pop_back();
    };

    void do_execute();

    inline 
    void traverse(RPBase * op)
    {
        if (null_op != op) {
            traverseStack.push_back(op);
            do_execute();
        }
    };
    
    void traverseChildren(const rqp::OperationList & children)
    {
        for (rqp::OperationList::const_iterator it = children.begin(); it != children.end(); ++it) {
            traverse(*it);
        };
    };

    void execute();

    PlanRewriter(rqp::RPBase* op) : inputOp(op) {};
};

}

#endif /* _PLAN_ALGORITHMS_H_ */
