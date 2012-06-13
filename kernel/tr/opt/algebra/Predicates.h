#ifndef _PREDICATES_H
#define _PREDICATES_H

#include "tr/opt/OptTypes.h"
#include "tr/opt/path/XPathTypes.h"
#include "tr/opt/path/DataSources.h"
#include "tr/opt/algebra/DataGraphs.h"
#include "tr/executor/base/tuple.h"

namespace phop {
    class IFunction;
}

class XmlConstructor;

namespace opt {

struct BinaryPredicate : public opt::Predicate {
    BinaryPredicate(DataGraph * dg, DataNode * left, DataNode * right);
    
    DataNode * left() const { return dataNodeList[0]; };
    DataNode * right() const { return dataNodeList[1]; };
};

/*
 * Predicate is used for test and switch expressions
 * Returns a constant boolean value (the result of comparison)
 */

struct PhantomPredicate : public opt::Predicate {
    DataNode * goalNode;
    tuple_cell value;

    virtual void * compile(PhysicalModel * model);

    virtual std::string toLRString() const;
    virtual XmlConstructor & toXML(XmlConstructor & ) const;
};

/*
 * Predicate for function evaluation
 */
struct FPredicate : public BinaryPredicate {
    phop::IFunction * func;

    FPredicate(DataGraph * dg, DataNode* left, DataNode* right, phop::IFunction * f);

    virtual void * compile(PhysicalModel * model);

    virtual std::string toLRString() const;
    virtual XmlConstructor & toXML(XmlConstructor & ) const;
};

/*
 * Predicate for value operations
 */
struct VPredicate : public BinaryPredicate {
    Comparison cmp;

    VPredicate(opt::DataGraph* dg, opt::DataNode* left, opt::DataNode* right, const opt::Comparison& _cmp);
    
    virtual void * compile(PhysicalModel * model);

    virtual std::string toLRString() const;
    virtual XmlConstructor & toXML(XmlConstructor & ) const;
};

/*
 * Predicate for structural operations
 */
struct SPredicate : public BinaryPredicate {
    bool outer;
    pe::Path path;

    SPredicate(DataGraph* dg, DataNode* left, DataNode* right, const pe::Path & path);
    
    virtual void * compile(PhysicalModel * model);

    virtual std::string toLRString() const;
    virtual XmlConstructor & toXML(XmlConstructor & ) const;
};

};

#endif /* _PREDICATES_H */
