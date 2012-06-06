#ifndef _FUNCTIONS_H_
#define _FUNCTIONS_H_

#include "tr/opt/OptTypes.h"
#include "tr/executor/base/tuple.h"

namespace opt {
  class FPredicate;
}

class dynamic_context;

namespace phop {

struct function_info_t {
    const char * uri;
    const char * name;

    bool preservesNull;
    bool hasDataGraph;
    bool rty;
};

class IFunction;

typedef std::map<xsd::QName, IFunction *> FunctionMap;

class FunctionLibrary
{
private:
    FunctionMap functions;
public:
    FunctionLibrary() {};
    ~FunctionLibrary() {};

    void registerFunction(IFunction * function);
    IFunction * findFunction(const xsd::QName & qname);
};

extern FunctionLibrary * functionLibrary;

void initializeFunctionLibrary();

struct IFunctionOpInstance : public opt::IPlanDisposable
{
private:
    const function_info_t * m_info;
public:
    const function_info_t & info() const { return *m_info; };
    
    virtual void reset() = 0;
    virtual tuple_cell eval(const tuple & t) = 0;
};

class IFunction
{
private:
    const function_info_t * m_info;
public:
    const function_info_t & info() const { return m_info; };

    const std::string getName() const
    {
        return "{" + std::string(info().uri) + "}:" + std::string(info().name);
    };

    virtual void rewrite(opt::DataGraph * dg, opt::FPredicate * rep) = 0;
    virtual IFunctionOpInstance * createInstance() = 0;
};

}

#endif /* _FUNCTIONS_H_ */
