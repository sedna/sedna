#ifndef OPTTYPES_H
#define OPTTYPES_H

#include <vector>
#include <set>
#include <map>

#include "common/sedna.h"

namespace pe {
    class Path;
};

typedef uint64_t PlanDesc;
typedef std::pair<float, float> Range;
typedef std::set<PlanDesc> PlanDescSet;
typedef int TupleId;


static const int DeBruijnBitPosition[32] =
{
  0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
  31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
};

static const uint32_t DeBruijnSequence = 0x077CB531UL;

inline static int get_bit_position(uint32_t v)
{
    return DeBruijnBitPosition[((v & -v) * DeBruijnSequence) >> 27];
}


class PlanDescIterator {
  private:
    PlanDesc z;
    int pos;
  public:
    PlanDescIterator(PlanDesc _desc) : z(_desc), pos(0) {};

    inline bool empty() const { return z == 0; };
    
    inline int next() {
        if (z == 0) {
            return -1;
        }

        if ((z & 0xFFFFFFFFUL) == 0) {
            z >>= 32;
            pos += 32;
        }

        int i = get_bit_position(z & 0xFFFFFFFFUL);

        z >>= i;
        z >>= 1;
        pos += i+1;

        return pos-1;
    };
};

struct Predicate;
struct DataNode;
struct Variable;
struct DataGraph;

class PhysicalModel;

typedef std::vector<Predicate *> PredicateList;
typedef std::vector<DataNode *> DataNodeList;
typedef std::vector<DataGraph *> DataGraphList;

typedef std::map<TupleId, DataNode *> VariableMap;
typedef std::map<std::string, DataNode *> VariableNameMap;

typedef std::set<Predicate *> PredicateSet;
typedef std::set<TupleId> TupleScheme;
typedef std::map<TupleId, int> LogicalToPhysicalTupleMap;
typedef std::vector<int> TupleMap;

class OptimizationSpace {};

void setSpace(OptimizationSpace *);
void popSpace(OptimizationSpace *);

/*
static inline
Range &operator= (Range &x, const double y) {
    x.first = y;
    x.second = y;
    return x;
};
*/

static inline Range operator* (const Range &x, const Range &y) { return Range(x.first * y.first, x.second * y.second); }
static inline Range operator+ (const Range &x, const Range &y) { return Range(x.first + y.first, x.second + y.second); }
static inline Range operator* (const Range &x, double y) { return Range(x.first * y, x.second * y); }
static inline Range operator+ (const Range &x, double y) { return Range(x.first + y, x.second + y); }

class IPlanDisposable {
public:
    virtual ~IPlanDisposable() {};
};

inline static
PlanDesc singlePlanDesc(uint8_t item) { return 1ULL << item; };


#endif /* OPTTYPES_H */
