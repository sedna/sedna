#ifndef _FN_COMPARISON_H_
#define _FN_COMPARISON_H_

#include "Functions.h"
#include "tr/opt/OptTypes.h"

class CollationHandler;

struct ComparisonData : public IFunctionData {
    opt::Comparison cmp;

    ComparisonData(const opt::Comparison & _cmp) : cmp(_cmp){};

    virtual XmlConstructor& toXML(XmlConstructor& constructor) const;
};

extern phop::FunctionInfo * generalComparisonFunction;

#endif /* _FN_COMPARISON_H_ */
