#ifndef _FUNCTIONS_H_
#define _FUNCTIONS_H_

#include "tr/opt/OptTypes.h"
#include "tr/executor/base/tuple.h"

namespace rqp {
    struct PlanRewriter;
    class FunCall;
}

namespace opt {
    class FPredicate;
}

class dynamic_context;

struct IFunctionData {
    virtual ~IFunctionData() {};
};

typedef bool (*rule_func_t) (rqp::PlanRewriter * pr, rqp::FunCall * op);

namespace phop {
  
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

//    getQName();

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

void initFunctionLibrary();

};

#define FN_URI (predefinedNamespaces[namespace_fn].uri)

inline static
phop::FunctionLibrary * getFunctionLibrary()
{
    if (phop::functionLibrary == NULL) {
        phop::initFunctionLibrary();
    };

    return phop::functionLibrary;
};


#endif /* _FUNCTIONS_H_ */
