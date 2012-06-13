#ifndef _FUNCTIONS_H_
#define _FUNCTIONS_H_

namespace opt {
    class DataGraph;
};

class FunctionProvider
{
public:
    virtual opt::DataGraph * applyGraph(opt::DataGraph * dg) = 0;
};

class OptNotEmpty : public FunctionProvider
{
public:
    virtual opt::DataGraph* applyGraph(opt::DataGraph* dg);
};

#endif /* _FUNCTIONS_H_ */
