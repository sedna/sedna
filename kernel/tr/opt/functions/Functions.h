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

enum function_flags_t {
    fn_preserves_null = 0x01,
    fn_in_block = 0x02,
};



struct function_methods_t
{
    
};

struct FunctionInfo
{
private:
    // NOTE: We cannot use xsd::QName for name here, because xml_ns storage is transaction depended
    std::string name;

    int argc;
    int flags;
public:
    int getArgumentCount() const { return argc; };
    const std::string & getName() const { return name; };
//    const q & getQName() const { return name; };
    bool getFlag(function_flags_t _f) const { return (flags & _f) > 0; }

    FunctionInfo(const std::string &_uri, const std::string &_local_name, int _argc)
        : name("{" + _uri + "}:" + _local_name), argc(_argc), flags(0) {};

    FunctionInfo& setFlag(function_flags_t _f) { flags |= _f; return *this; };
};

typedef std::map<std::string, FunctionInfo *> FunctionMap;

class FunctionLibrary
{
private:
    FunctionMap functions;
public:
    FunctionLibrary() {};
    ~FunctionLibrary() {};

    FunctionInfo * registerFunction(FunctionInfo * function);
    FunctionInfo * findFunction(const xsd::QName & qname);
};

extern FunctionLibrary * functionLibrary;

void initializeFunctionLibrary();

};

#endif /* _FUNCTIONS_H_ */
