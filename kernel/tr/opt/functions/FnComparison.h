#ifndef _FN_COMPARISON_H_
#define _FN_COMPARISON_H_

#include "Functions.h"
#include "tr/opt/OptTypes.h"

class CollationHandler;

struct ComparisonData : public IFunctionData {
    opt::Comparison cmp;
    
    ComparisonData(const opt::Comparison & _cmp)
      : cmp(_cmp){};
};

extern phop::FunctionInfo * general_comparison_function;

/*
extern phop::FunctionInfo * g_eq_function;
extern phop::FunctionInfo * g_ge_function;
extern phop::FunctionInfo * g_gt_function;
extern phop::FunctionInfo * g_le_function;
extern phop::FunctionInfo * g_lt_function;
*/

#endif /* _FN_COMPARISON_H_ */
  