#ifndef JOINS_H
#define JOINS_H

#include "SequenceModel.h"

class GSMJMergeComparator {
public:
    bool stepLeft;
    bool stepRight;

    virtual bool compare(tuple left, tuple right) = 0;
};

AbstractSequence * createEqualValueJoin();

#endif /* PLAN_EXECUTION_H */
