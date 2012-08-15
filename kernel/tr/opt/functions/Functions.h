#ifndef _FUNCTIONS_H_
#define _FUNCTIONS_H_

#include "tr/opt/OptTypes.h"
#include "tr/executor/base/tuple.h"

#include "tr/models/XmlConstructor.h"

struct IFunctionData : public IXMLSerializable
{
    virtual ~IFunctionData() {};
};

namespace phop {

struct function_info_t
{
    const char * default_prefix;
    const char * uri;
    const char * localname;

    int argc;
};

struct FunctionInfo
{
    const function_info_t * finfo;

    FunctionInfo(const function_info_t * _finfo) : finfo(_finfo) {};
    virtual ~FunctionInfo() {};

    xsd::QName getQName() const
    {
        return xsd::constQName(xmlns_touch(finfo->default_prefix, finfo->uri), finfo->localname);
    };

    virtual void execute(rqp::FunCallParams * funcall, executor::DynamicContext * dynamicContext) = 0;
    virtual bool transform(rqp::FunCallParams * funcall, rqp::RewritingContext * rewritingContext) { return false; };

//    virtual bool checkStatic(rqp::FunCallParams * funcall);
//    virtual bool checkParams(rqp::FunCallParams * funcall);
};

typedef std::map<std::string, FunctionInfo *> FunctionMap;

class FunctionLibrary
{
private:
    FunctionMap functions;
public:
    FunctionLibrary();
    ~FunctionLibrary();

    FunctionInfo * registerFunction(FunctionInfo * finfo);
    FunctionInfo * findFunction(const xsd::QName & qname, int argc);
};

extern FunctionLibrary * functionLibrary;

void initFunctionLibrary();

};

#define FN_NS (predefinedNamespaces[namespace_fn])
#define SE_NS (predefinedNamespaces[namespace_se])

inline static
phop::FunctionLibrary * getFunctionLibrary()
{
    if (phop::functionLibrary == NULL) {
        phop::initFunctionLibrary();
    };

    return phop::functionLibrary;
};


#endif /* _FUNCTIONS_H_ */
