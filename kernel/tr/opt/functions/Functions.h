#ifndef _FUNCTIONS_H_
#define _FUNCTIONS_H_

#include "tr/opt/OptTypes.h"
#include "tr/executor/base/tuple.h"

namespace opt {
  class FPredicate;
}

class dynamic_context;

struct IFunctionData {
    virtual ~IFunctionData() {};
};

namespace phop {

typedef bool (rule_func_t)(PlanRewriter * pr, rqp::FunCall * op);

struct function_info_t
{
    rule_func_t rule_func;
};

struct FunctionInfo
{
    const char * uri;
    const char * localname;
    const function_info_t * finfo;
    IFunctionData * default_data;

    getQName();

    FunctionInfo(const char * _uri, const char * _localname, const function_info_t * _finfo) :
      uri(_uri), localname(_localname), finfo(_finfo), default_data(NULL) {};

    ~FunctionInfo() { delete default_data; };
};

typedef std::map<std::string, FunctionInfo *> FunctionMap;

class FunctionLibrary
{
private:
    FunctionMap functions;
public:
    FunctionLibrary();
    ~FunctionLibrary();

    FunctionInfo * registerFunction(const char * uri, const char * localname, const function_info_t * finfo);
    FunctionInfo * findFunction(const xsd::QName & qname);
};

extern FunctionLibrary * functionLibrary;

void initializeFunctionLibrary();

};

#endif /* _FUNCTIONS_H_ */
