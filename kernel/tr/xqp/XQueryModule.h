#ifndef __XQUERY_MODULE_H__
#define __XQUERY_MODULE_H__

#include <map>
#include <string>

#include "tr/xqp/XQFunction.h"
#include "tr/xqp/XQCommon.h"
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
        LReturn *lr; // for library modules to process lreturn optimization when we refernece some of its vars or funcs

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

        /*
         * dynmaic context for the module:
         *   library modules -- it's set outside by the driver
         *   main modules -- create it themselves
         */
        dynamic_context *dyn_cxt;

        /*
         * true, if library module is actually being used
         * it's being used if some of its vars or funcs are being used
         */
        bool is_used;

    public:
        XQueryModule(ASTNode *ast_tree, XQueryDriver *driver);
        XQueryModule(const char *ast_str, XQueryDriver *driver);

        void doSemanticAnalysis();
        void doLReturnAnalysis();
        void doPostSemanticAnalysis(XQVariablesInfo &libVars, bool check_cycles); // unresolved variables checking and cycle lookup

        std::string getLR();
        std::string getIR();
        std::string getIR(ASTNode *ast);

        std::string getModuleURI();

        ASTNode* getTree();

        rqp::RPBase *getOPT(bool is_subquery);
        PPQueryEssence *getQEP(bool is_subquery);

        /*
         * fills dynamic_context with vars and funcs from the library module
         *
         * doesn't make sense for a main module
         */
        void porLibModule();

        /*
         * This function sets dynamic context for library module. It's used
         * by the driver to run two-step enumeration process for library modules.
         *
         * Should not be used with main modules since they create contexts
         * themselves.
         */
        void set_dynamic_context(dynamic_context *mod_context)
        {
            U_ASSERT(module_uri != NULL && dyn_cxt == NULL);
            dyn_cxt = mod_context;
        }

        /*
         * create ids for vars and funcs from the library module
         *
         * doesn't make sense for a main module
         */
        void enumerate_vars_funcs();

        // returns all modules that this module imports in
        std::vector<std::string> getImportedModules() const;

        // ordered mode get-set
        void setOrderedMode(bool ordered)
        {
            isDefaultOrdered = ordered;
        }
        bool getOrderedMode() const
        {
            return isDefaultOrdered;
        }

        // some option flags getters-setters
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

        // usability getter-setter
        bool module_is_needed() const
        {
            return is_used;
        }
        void set_module_as_needed()
        {
            is_used = true;
        }

        // these functions return info about vars anf funcs
        bool getFunctionInfo(const std::string &name, XQFunction **xqf);
        bool getVariableInfo(const std::string &name, XQVariable **xqv);

        /*
         * these functions return info about vars anf funcs
         * they also guarantee lreturn info by executing lr-visitor
         */
        void getLReturnFunctionInfo(const std::string &name, XQFunction **xqf);
        void getLReturnVariableInfo(const std::string &name, XQVariable **xqv);

        ~XQueryModule();
    };
}

#endif
