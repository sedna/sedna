#ifndef _FN_AXIS_STEP_H_
#define _FN_AXIS_STEP_H_

#include "Functions.h"
#include "tr/opt/path/XPathTypes.h"

struct AxisStepData : public IFunctionData {
    pe::Step step;

    AxisStepData(const pe::Step & _step) : step(_step) {};
    virtual XmlConstructor& toXML(XmlConstructor& constructor) const;
    
};

extern phop::FunctionInfo * axisStepFunction;

#endif /* _FN_CONSTRUCTORS_H_ */
