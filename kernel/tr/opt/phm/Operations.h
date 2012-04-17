#ifndef _OPERATIONS_H
#define _OPERATIONS_H

#include "tr/opt/OptTypes.h"
#include "tr/opt/phm/PhysicalModel.h"
#include "tr/opt/alg/Predicates.h"

struct Statistics;

class POProtIn { public:
    POProt * op;
    TupleId index;

    POProtIn() : op(NULL), index(0) {};
    POProtIn(POProt * _op, const TupleId _index) : op(_op), index(_index) {};
    POProtIn(const POProtIn& _other) : op(_other.op), index(_other.index) {};
};

/*
class TupleIn { public:
    POProtIn opIn;
    Statistics * statistics;
};
*/

// TODO: Actually redudant data structure, that holds 
class SchemeElement : public IPlanDisposable {
public:
    bool available;
    bool evaluated;

    Statistics * statistics;
    DataNode * node;
    POProtIn pop;

    SchemeElement() : available(false), evaluated(false), statistics(NULL), node(NULL) {};
};

class SortMergeJoinPrototype : public POProt {
protected:
    SchemeElement * leftIn;
    SchemeElement * rightIn;

    const Comparison cmp;
public:
    SortMergeJoinPrototype(SchemeElement * _left, SchemeElement * _right, const Comparison& _cmp);

    virtual PPIterator* compile();
};

class PathExpressionPrototype : public POProt {
protected:
    const pe::Path path;
public:
    PathExpressionPrototype(const pe::Path& _path);

    virtual PPIterator* compile();
};

class AxisStepPrototype : public PathExpressionPrototype {
public:
    AxisStepPrototype(SchemeElement* _in, SchemeElement* _out, const pe::Path& _path);

    virtual PPIterator* compile();
};

class StructuralSortMergeJoinPrototype : public PathExpressionPrototype {
protected:
    SchemeElement * leftIn;
    SchemeElement * rightIn;
public:
    StructuralSortMergeJoinPrototype(SchemeElement* _left, SchemeElement* _right, const pe::Path& _path);

    virtual PPIterator* compile();
};

class AbsPathScanPrototype : public PathExpressionPrototype {
private:
    const DataRoot& dataRoot;
public:
    AbsPathScanPrototype(SchemeElement* _in, const pe::Path& _path, const DataRoot& _dataRoot);

    virtual PPIterator* compile();
};


/*
class PathStepPrototype : public PathExpressionPrototype {
private:
    SchemeElement * leftIn;
    SchemeElement * rightIn;
public:
    PathStepPrototype(SchemeElement* _left, SchemeElement* _right, const pe::Path& _path)
    : PathExpressionPrototype(_left, _right, _path) {};

    virtual OperationCost* evaluateCost();
    virtual PPIterator* compile();
};

*/

#endif /* _OPERATIONS_H */
