#include "tr/opt/functions/Functions.h"
#include "tr/opt/SequenceModel.h"
#include "tr/executor/base/namespaces.h"

using namespace phop;

FunctionLibrary * phop::functionLibrary = NULL;

void phop::initFunctionLibrary()
{
    phop::functionLibrary = new FunctionLibrary;
}

FunctionLibrary::FunctionLibrary()
{

}

FunctionLibrary::~FunctionLibrary()
{
    for (FunctionMap::const_iterator it = functions.begin(); it != functions.end(); ++it)
    {
        delete it->second;
    };
}

static
std::string getSearchName(const char* uri, const char* localname)
{
    return std::string(localname) + ((uri == NULL) ? "@{}" : ("@{" + std::string(uri) + "}"));
};

FunctionInfo* FunctionLibrary::registerFunction(
    const char* default_prefix,
    const char* uri,
    const char* localname,
    const function_info_t* finfo)
{
    FunctionInfo * result = new FunctionInfo(default_prefix, uri, localname, finfo);
    std::string name = getSearchName(uri, localname);
    functions.insert(FunctionMap::value_type(name, result));
    return result;
}

FunctionInfo* FunctionLibrary::findFunction(const xsd::QName& qname)
{
    std::string name = getSearchName(qname.getUri(), qname.getLocalName());

    FunctionMap::const_iterator it = functions.find(name);

    if (it == functions.end()) {
        U_ASSERT(false);
        return NULL;
    } else {
        return it->second;
    };
}


