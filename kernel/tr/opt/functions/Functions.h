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
    const char * default_prefix;

    xsd::QName getQName() const
    {
        return xsd::constQName(xmlns_touch(default_prefix, uri), localname);
    };

    FunctionInfo(
        const char * _default_prefix,
        const char * _uri,
        const char * _localname,
        const function_info_t * _finfo) :

      uri(_uri),
      localname(_localname),
      finfo(_finfo),
      default_data(NULL),
      default_prefix(_default_prefix)

        {};

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

    FunctionInfo * registerFunction(
        const char * default_prefix,
        const char * uri,
        const char * localname,
        const function_info_t * finfo);

    FunctionInfo * findFunction(const xsd::QName & qname);
};

extern FunctionLibrary * functionLibrary;

void initFunctionLibrary();

};

#define FN_NS (predefinedNamespaces[namespace_fn])

inline static
phop::FunctionLibrary * getFunctionLibrary()
{
    if (phop::functionLibrary == NULL) {
        phop::initFunctionLibrary();
    };

    return phop::functionLibrary;
};


#endif /* _FUNCTIONS_H_ */
