
/* Basic operations
 *  - TupleCellSet{Node} getNodes(AbsPath)
 *  - TupleCellSet{Node} path(TupleCell, Path)
 *  - bool testP1(TupleCell, testFunc)
 *  - bool testP2(TupleCell, TupleCell, testFunc)
 *  - TupleSet sort(TupleSet, keyIndex)
 *  - //
 */

#include <tr/executor/xpath/XPathTypes.h>

struct SchemaStatistics;

class SchemaNodeIterator {
};

class TupleCellIterator {
};

class TupleIterator {
};

struct SchemaScan {
    SchemaScan(const pe::AbsPath & path);
    double cost(SchemaStatistics * ss) const;
};

struct NodeHeapMerge {
    NodeHeapMerge(int streamCount);
    double cost(SchemaStatistics * ss) const;
};

