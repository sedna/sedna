#ifndef _FN_COMPARISON_H_
#define _FN_COMPARISON_H_

#include "Functions.h"
#include "tr/opt/OptTypes.h"

struct ComparisonData : public IFunctionData {
    opt::Comparison cmp;
    ComparisonData(const opt::Comparison & _cmp) : cmp(_cmp) {};
};


#endif /* _FN_COMPARISON_H_ */
  