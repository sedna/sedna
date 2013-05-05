/*
 * File:  XQueryDriver.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef XQUERY_DRIVER_H
#define XQUERY_DRIVER_H

#include <ostream>
#include <string>
#include <set>
#include <deque>

#include "XQueryLexer.h"
#include "XQueryParser.h"
#include "XQueryModule.h"
#include "XQFunction.h"

namespace sedna
{
    struct ErrorInfo
    {
        int error_code;
        char *error_msg;

        struct ErrorInfo *next;
    };

    class XQueryDriver
    {
        friend class sedna::XQueryParser; // for lexer access
        friend class Sema; // to change XQuery-specific state
        friend class Cycle;  // to change XQuery-specific state

        private:
            XQueryLexer *lexer;
            ErrorInfo *errors, *err_tail;

            /* ok, we can have several modules since Sedna grammar permits this. Modules are separated from each other
               by '\n\\n':
                st1
                \
                st2
            */
            std::deque<XQueryModule *> mods; // main modules (library modules) that we're evaluating

            // Some XQuery-specific info about modules, types, imported modules
            static XQStdFunctionInfo stdFuncs;
            XQFunctionInfo libFuncs;
            XQVariablesInfo libVars; // all variables declared in all processed libraries

            typedef std::vector<XQueryModule *> modSequence;

            struct xsTypeInfo
            {
                ASTType::TypeMod type; // category of type, i.e. proper context
                xmlscm_type xtype; // internal type

                xsTypeInfo(ASTType::TypeMod mod_ = ASTType::ABSTRACT, xmlscm_type type_ = xs_anyType) : type(mod_), xtype(type_) {}
            };
            std::map<std::string, xsTypeInfo> xsTypes; // known XQuery types

            std::map<std::string, modSequence> libModules; // imported modules
            std::vector<std::string> import_chain; // current import chain to look for cycles

            void initXQueryInfo();
            int getLibraryModule(const char *uri);
            void emitErrors();

            ASTNode *getASTFromQuery(const char *query);

        public:

            XQueryDriver() : lexer(NULL), errors(NULL)
            {
                initXQueryInfo();

                if (XQueryDriver::stdFuncs.empty())
                {
                    registerStandardFunctions("http://www.w3.org/2005/xpath-functions", xqueryFunctions);
                    registerStandardFunctions("http://modis.ispras.ru/Sedna/SQL", sqlFunctions);
                    registerStandardFunctions("http://sedna.org/crypto", cryptoFunctions);
                    registerStandardFunctions("http://www.modis.ispras.ru/sedna", seFunctions);
                }
            }

            ~XQueryDriver();

            // flags set options for module:
            //     1 - turn on explain
            //     2 - turn on profile
            void addModule(ASTNode *mod, uint8_t flags);

            void doSemanticAnalysis();
            void doLReturnAnalysis();

            PPQueryEssence *getQEPForModule(unsigned int ind, bool is_subquery);

            size_t getModulesCount() const
            {
                return mods.size();
            }

            std::string getLRRepresentation(size_t mod_ind);
            std::string getIRRepresentation(size_t ind_mod);
            std::string getParsedModuleName();

            void error(const sedna::XQueryParser::location_type &loc, int code, const char *msg);
            void error(const sedna::XQueryParser::location_type &loc, int code, std::string msg);
            void error(int code, const char *msg);
            bool gotErrors() const
            {
                return (errors != NULL);
            }

            bool parse(const char *query);
            void parseAST(const char *ast);

            int getErrorCode() const;
            std::string getErrorMsg() const;

            static void registerStandardFunctions(const char *uri_nsp, XQFunction *funcs);

            // returns lreturn-specific info about funcs and vars (xquery ones included)
            XQFunction *getStdFuncInfo(const std::string &name) const;
            XQFunction *getLReturnFunctionInfo(const std::string &name);
            XQVariable *getLReturnVariableInfo(const std::string &name);

            // returns vars and funcs ids
            global_var_dsc getGlobalVariableId(const std::string &name);
            function_id getGlobalFunctionId(const std::string &name);

            // returns type by string
            xmlscm_type getXsType(const char *type);

            // process library modules and attach them to parent_context
            void porLibModules(dynamic_context *parent_context);
    };
}

#endif
