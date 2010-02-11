#ifndef __XQUERY_MODULE_H__
#define __XQUERY_MODULE_H__

#include <map>
#include <string>

#include "XQFunction.h"
#include "XQCommon.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/xqops/PPVariable.h"

// This class incorporates all the info about main or library module
namespace sedna
{
    class XQueryDriver;
    class LReturn;

    class XQueryModule
    {
        friend class Sema;  // to change XQuery-specific state
        friend class Cycle;  // to change XQuery-specific state

    private:
        ASTNode *ast; // corresponding ast tree
        PPQueryEssence *qep; // qep-tree;
        // NOTE: only one of the above exists; when qep has been created, ast is discarded
        // also for library module after por has been processed qep AND ast contain NULL, since there is no qep-tree for prolog

        XQueryDriver *drv;
        LReturn *lr; // for library modules to process lreturn optimization whent the

        void testDupAndSerial(XQueryDriver *drv);
        void initXQueryInfo();

        // some XQuery-specific module info
        std::string *module_uri; // uri for a library module; NULL for main module
        nsBindType nsBinds; // known namespaces (predefined + from-prolog)
        nsPair defElemNsp; // default element namespace
        nsPair defFuncNsp; // default function namespace
        bool isDefaultOrdered; // true, if prolog defines default is ordered (also true by default)
        bool isExplainOn; // true, if we will use Sedna explain feature on query execution
        bool isProfileOn; // true, if we will use Sedna profile feature on query execution

        XQFunctionInfo funcs; // all function defined in prolog
        XQVariablesInfo vars; // all variables defined in this module
        XQStringHash unres_vars; // variables that could not have been resolved during intitial semantic analysis phase
        XQFunctionInfo unres_funcs; // functions that could not have been resolved during intitial semantic analysis phase

        XQStringHash imported; // contains URIs for imported modules

        ASTLocation *xpdy0002; // not NULL if after analysis we should emit dynamic error (this is just to emit dynamic errors after static ones)

        typedef std::pair<std::string, PPGlobalVariable *> unresPorVarInfo;
        std::multimap<std::string, PPGlobalVariable *> unresPorVars; // contains unresolved global var ids

        // predefined static context; used when we want to create qep in some specified static context (usually, unmanaged one)
        // see modules import and triggers as examples
        static_context *qep_sx;

    public:
        XQueryModule(ASTNode *ast_tree, XQueryDriver *driver);
        XQueryModule(const char *ast_str, XQueryDriver *driver);

        void doSemanticAnalysis();
        void doLReturnAnalysis();
        void doPostSemanticAnalysis(const XQVariablesInfo &libVars, bool check_cycles); // unresolved variables checking and cycle lookup
        std::string getLR();
        std::string getIR();
        std::string getIR(ASTNode *ast);

        std::string getModuleURI();

        ASTNode* getTree();

        PPQueryEssence *getQEP();

        void porLibModule();

        std::vector<std::string> getImportedModules() const;

        void setOrderedMode(bool ordered)
        {
            isDefaultOrdered = ordered;
        }

        bool getOrderedMode() const
        {
            return isDefaultOrdered;
        }

        void setFlags(uint8_t flags)
        {
            if (flags & 1)
                isExplainOn = true;

            if (flags & 2)
                isProfileOn = true;
        }

        bool turnedExplain() const
        {
            return isExplainOn;
        }

        bool turnedProfile() const
        {
            return isProfileOn;
        }

        bool getFunctionInfo(const std::string &name, XQFunction &xqf) const;
        ASTVarDecl *getVariableInfo(const std::string &name) const;
        bool getLReturnFunctionInfo(const std::string &name, XQFunction &xqf);
        bool getLReturnVariableInfo(const std::string &name, XQVariable &xqv);

        void addToUnresolvedPor(const std::string &name, PPGlobalVariable *var);

        void setStaticContextForQEP(static_context *sx);

        size_t getFunctionCount() const
        {
            return funcs.size();
        }

        size_t getVarCount() const
        {
            return vars.size();
        }

        ~XQueryModule();
    };
}

#endif
