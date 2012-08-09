#include "tr/opt/functions/Functions.h"
#include "tr/opt/algorithms/SequenceModel.h"
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

FunctionInfo* FunctionLibrary::registerFunction(FunctionInfo * finfo)
{
    std::string name = getSearchName(finfo->finfo->uri, finfo->finfo->localname);
    functions.insert(FunctionMap::value_type(name, finfo));
    return finfo;
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


