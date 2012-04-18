#ifndef _OPERATIONS_H
#define _OPERATIONS_H

#include "tr/opt/OptTypes.h"
#include "tr/opt/phm/PhysicalModel.h"
#include "tr/opt/alg/Predicates.h"

struct Statistics;

class BinaryOpPrototype : public POProt {
public:
    BinaryOpPrototype(const prot_info_t* pinfo, const POProtIn & _left, const POProtIn & _right)
      : POProt(pinfo), result(NULL) { in.push_back(_left); in.push_back(_right); };
};

class SortMergeJoinPrototype : public BinaryOpPrototype {
protected:
    const Comparison cmp;
public:
    SortMergeJoinPrototype(PhysicalModel * model, const POProtIn & _left, const POProtIn & _right, const Comparison& _cmp);
};

class StructuralJoinPrototype : public BinaryOpPrototype {
    pe::Path path;
public:
    StructuralJoinPrototype(PhysicalModel * model, const POProtIn & _left, const POProtIn & _right, const pe::Path& _path);
};

class AbsPathScanPrototype : public POProt {
    DataRoot dataRoot;
    TupleRef result;
public:
    AbsPathScanPrototype(PhysicalModel * model, const TupleRef & tref);
};

class PathEvaluationPrototype : public POProt {
    pe::Path path;
    TupleRef result;
public:
    PathEvaluationPrototype(PhysicalModel * model, const POProtIn & _left, const TupleRef & _right, const pe::Path& _path);
};

#endif /* _OPERATIONS_H */
