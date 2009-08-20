#ifndef __XQUERY_MODULE_H__
#define __XQUERY_MODULE_H__

#include <map>
#include <string>

#include "XQFunction.h"
#include "XQuerytoLR.h"
#include "XQCommon.h"

// This clas incorporates all the info about main or library module
namespace sedna
{
    class XQueryDriver;

    class XQueryModule
    {
        friend class Sema;  // to change XQuery-specific state
        friend class Cycle;  // to change XQuery-specific state

    private:
        ASTNode *ast; // corresponding ast tree
        XQueryDriver *drv;

        void testDupAndSerial(XQueryDriver *drv);
        void initXQueryInfo();

        // some XQuery-specific module info
        std::string *module_uri; // uri for a library module; NULL for main module
        nsBindType nsBinds; // known namespaces (predefined + from-prolog)
        nsPair defElemNsp; // default element namespace
        nsPair defFuncNsp; // default function namespace

        XQFunctionInfo funcs; // all function defined in prolog
        XQVariablesInfo vars; // all variables defined in this module
        XQStringHash unres_vars; // variables that cannot have been resolved during intiial semantic analysis phase
        XQFunctionInfo unres_funcs; // functions that cannot have been resolved during intiial semantic analysis phase

        XQStringHash imported; // contains URIs for imported modules

        ASTLocation *xpdy0002; // not NULL if after analysis we should emit dynamic error (this is just to emit dynamic errors after static ones)

    public:
        XQueryModule(ASTNode *ast_tree, XQueryDriver *driver);

        void doSemanticAnalysis();
        void doPostSemanticAnalysis(const XQVariablesInfo &libVars, bool check_cycles); // unresolved variables checking and cycle lookup
        std::string getLR();
        std::string getIR();
        std::string getModuleURI();

        std::vector<std::string> getImportedModules() const;

        ~XQueryModule()
        {
            delete ast;
            delete module_uri;
        }
    };
}

#endif
