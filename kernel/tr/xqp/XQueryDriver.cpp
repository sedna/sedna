/*
 * File:  XQueryDriver.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "XQFunction.h"
#include "XQueryDriver.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sstream>

#include "tr/xqp/modules.h"
#include "tr/xqp/serial/deser.h"

#include "tr/opt/algebra/PlanRewriter.h"

#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/models/XmlConstructor.h"
#include "tr/debugstream.h"

namespace sedna
{
    XQStdFunctionInfo XQueryDriver::stdFuncs;

    XQueryDriver::~XQueryDriver()
    {
        ErrorInfo *ner, *er;

        // delete errors stack
        if (errors)
        {
            er = errors;
            while (er)
            {
                ner = er->next;

                if (er->error_msg)
                    free(er->error_msg);

                free(er);

                er = ner;
            }
        }

        for (unsigned int i = 0; i < mods.size(); i++)
            delete mods[i];

        std::map<std::string, std::vector<XQueryModule *> >::iterator it;

        for (it = libModules.begin(); it != libModules.end(); it++)
        {
            std::vector<XQueryModule *> vec = it->second;

            for (unsigned int i = 0; i < vec.size(); i++)
                delete vec[i];
        }
    }

    bool XQueryDriver::parse(const char *query)
    {
        bool res;
        std::istringstream query_stream(query);
        XQueryLexer lex(*this, &query_stream);
        XQueryParser parser(*this);

        lexer = &lex; // parser use lexer to get next token
        res = (parser.parse() != 0);
        lexer = NULL;

        emitErrors();

        return res;
    }

    void XQueryDriver::parseAST(const char *ast)
    {
        XQueryModule *mod = new XQueryModule(ast, this);

        mods.push_back(mod);
    }

    void XQueryDriver::emitErrors()
    {
        std::string err_msg;

        if (getErrorCode() != -1)
        {
            err_msg = getErrorMsg();
            if (err_msg != "")
                throw USER_EXCEPTION2(getErrorCode(), err_msg.c_str());
            else
                throw USER_EXCEPTION(getErrorCode());
        }
    }

    void XQueryDriver::error(int code, const char *msg)
    {
        ASTLocation dummy;

        dummy.begin.line = 0;
        dummy.begin.column = 0;

        error(dummy, code, msg);
    }

    void XQueryDriver::error(const sedna::XQueryParser::location_type &loc, int code, std::string msg)
    {
        error(loc, code, msg.c_str());
    }

    void XQueryDriver::error(const sedna::XQueryParser::location_type &loc, int code, const char *msg)
    {
        ErrorInfo *err = (ErrorInfo *)malloc(sizeof(ErrorInfo));

        // if message is empty string -- discard it
        if (msg && !strlen(msg))
            msg = NULL;

        if (msg)
        {
            err->error_msg = (char *)malloc(strlen(msg) + 64);

            if (loc.begin.line == 0 && loc.begin.column == 0)
                sprintf(err->error_msg, "%s", msg);
            else
                sprintf(err->error_msg, "at (%d:%d), %s", loc.begin.line, loc.begin.column, msg);
        }
        else
        {
            if (loc.begin.line != 0 || loc.begin.column != 0)
            {
                err->error_msg = (char *)malloc(64);
                sprintf(err->error_msg, "at (%d:%d)", loc.begin.line, loc.begin.column);
            }
            else
            {
                err->error_msg = NULL;
            }
        }

        err->error_code = code;
        err->next = NULL;

        if (!errors)
        {
            errors = err_tail = err;
        }
        else
        {
            err_tail->next = err;
            err_tail = err;
        }
    }

    int XQueryDriver::getErrorCode() const
    {
        if (errors)
            return errors->error_code;
        else
            return -1;
    }

    std::string XQueryDriver::getErrorMsg() const
    {
        int first_code;
        ErrorInfo *err;
        std::string err_msg;

        if (!errors) return "";

        first_code = errors->error_code;
        err = errors;

        while (err && err->error_code == first_code && err->error_msg)
        {
            if (err->error_msg)
            {
                err_msg += err->error_msg;
                err_msg += "\n         ";
            }

            err = err->next;
        }

        return err_msg;
    }

    void XQueryDriver::addModule(ASTNode *mod, uint8_t flags)
    {
        XQueryModule *xqmod = new XQueryModule(mod, this);

        // set feature flags on module
        xqmod->setFlags(flags);

        mods.push_back(xqmod);
    }

    std::string XQueryDriver::getParsedModuleName()
    {
        if (mods.size() == 0)
            return "";

        return mods[0]->getModuleURI();
    }

    void XQueryDriver::doSemanticAnalysis()
    {
        std::string err_msg;

        for (unsigned int i = 0; i < mods.size(); i++)
            mods[i]->doSemanticAnalysis();

        emitErrors();

        // check if all modules share the same uri
        if (mods.size() > 1)
        {
            std::string mod_uri = mods[0]->getModuleURI();

            for (unsigned int i = 1; i < mods.size(); i++)
            {
                std::string mu = mods[i]->getModuleURI();

                if (mu != mod_uri)
                {
                    error(SE1072, NULL);
                    break;
                }
            }
        }

        emitErrors();

        // then we must do post-semantic analysis for library modules
        std::map<std::string, std::vector<XQueryModule *> >::iterator libit;
        for (libit = libModules.begin(); libit != libModules.end(); libit++)
        {
            for (unsigned int i = 0; i < libit->second.size(); i++)
                (libit->second)[i]->doPostSemanticAnalysis(libVars, false);

            emitErrors();
        }

        // then we must check for cycles in libarary modules (we cannot do it during first check since there can be unresolved funcs and vars in other modules)
        for (libit = libModules.begin(); libit != libModules.end(); libit++)
        {
            for (unsigned int i = 0; i < libit->second.size(); i++)
                (libit->second)[i]->doPostSemanticAnalysis(libVars, true);

            emitErrors();
        }

        for (unsigned int i = 0; i < mods.size(); i++)
        {
            mods[i]->doPostSemanticAnalysis(libVars, true);
            emitErrors();
        }
    }

    void XQueryDriver::doLReturnAnalysis()
    {
        for (unsigned int i = 0; i < mods.size(); i++)
            mods[i]->doLReturnAnalysis();
    }

    std::string XQueryDriver::getLRRepresentation(size_t mod_ind)
    {
        return mods[mod_ind]->getLR();
    }

    std::string XQueryDriver::getIRRepresentation(size_t ind_mod)
    {
        return mods[ind_mod]->getIR();
    }

    void XQueryDriver::initXQueryInfo()
    {
        // predefined XQuery types
        xsTypes["anyAtomicType"] = xsTypeInfo(ASTType::ATOMIC, xs_anyAtomicType);
        xsTypes["string"] = xsTypeInfo(ASTType::ATOMIC, xs_string);
        xsTypes["normalizedString"] = xsTypeInfo(ASTType::ATOMIC, xs_normalizedString);
        xsTypes["token"] = xsTypeInfo(ASTType::ATOMIC, xs_token);
        xsTypes["language"] = xsTypeInfo(ASTType::ATOMIC, xs_language);
        xsTypes["NMTOKEN"] = xsTypeInfo(ASTType::ATOMIC, xs_NMTOKEN);
        xsTypes["Name"] = xsTypeInfo(ASTType::ATOMIC, xs_Name);
        xsTypes["NCName"] = xsTypeInfo(ASTType::ATOMIC, xs_NCName);
        xsTypes["ID"] = xsTypeInfo(ASTType::ATOMIC, xs_ID);
        xsTypes["IDREF"] = xsTypeInfo(ASTType::ATOMIC, xs_IDREF);
        xsTypes["ENTITY"] = xsTypeInfo(ASTType::ATOMIC, xs_ENTITY);
        xsTypes["untypedAtomic"] = xsTypeInfo(ASTType::ATOMIC, xs_untypedAtomic);
        xsTypes["dateTime"] = xsTypeInfo(ASTType::ATOMIC, xs_dateTime);
        xsTypes["date"] = xsTypeInfo(ASTType::ATOMIC, xs_date);
        xsTypes["time"] = xsTypeInfo(ASTType::ATOMIC, xs_time);
        xsTypes["duration"] = xsTypeInfo(ASTType::ATOMIC, xs_duration);
        xsTypes["yearMonthDuration"] = xsTypeInfo(ASTType::ATOMIC, xs_yearMonthDuration);
        xsTypes["dayTimeDuration"] = xsTypeInfo(ASTType::ATOMIC, xs_dayTimeDuration);
        xsTypes["float"] = xsTypeInfo(ASTType::ATOMIC, xs_float);
        xsTypes["double"] = xsTypeInfo(ASTType::ATOMIC, xs_double);
        xsTypes["decimal"] = xsTypeInfo(ASTType::ATOMIC, xs_decimal);
        xsTypes["integer"] = xsTypeInfo(ASTType::ATOMIC, xs_integer);
        xsTypes["nonPositiveInteger"] = xsTypeInfo(ASTType::ATOMIC, xs_nonPositiveInteger);
        xsTypes["negativeInteger"] = xsTypeInfo(ASTType::ATOMIC, xs_negativeInteger);
        xsTypes["long"] = xsTypeInfo(ASTType::ATOMIC, xs_long);
        xsTypes["int"] = xsTypeInfo(ASTType::ATOMIC, xs_int);
        xsTypes["short"] = xsTypeInfo(ASTType::ATOMIC, xs_short);
        xsTypes["byte"] = xsTypeInfo(ASTType::ATOMIC, xs_byte);
        xsTypes["nonNegativeInteger"] = xsTypeInfo(ASTType::ATOMIC, xs_nonNegativeInteger);
        xsTypes["positiveInteger"] = xsTypeInfo(ASTType::ATOMIC, xs_positiveInteger);
        xsTypes["unsignedLong"] = xsTypeInfo(ASTType::ATOMIC, xs_unsignedLong);
        xsTypes["unsignedInt"] = xsTypeInfo(ASTType::ATOMIC, xs_unsignedInt);
        xsTypes["unsignedShort"] = xsTypeInfo(ASTType::ATOMIC, xs_unsignedShort);
        xsTypes["unsignedByte"] = xsTypeInfo(ASTType::ATOMIC, xs_unsignedByte);
        xsTypes["gYearMonth"] = xsTypeInfo(ASTType::ATOMIC, xs_gYearMonth);
        xsTypes["gMonthDay"] = xsTypeInfo(ASTType::ATOMIC, xs_gMonthDay);
        xsTypes["gDay"] = xsTypeInfo(ASTType::ATOMIC, xs_gDay);
        xsTypes["gMonth"] = xsTypeInfo(ASTType::ATOMIC, xs_gMonth);
        xsTypes["gYear"] = xsTypeInfo(ASTType::ATOMIC, xs_gYear);
        xsTypes["boolean"] = xsTypeInfo(ASTType::ATOMIC, xs_boolean);
        xsTypes["base64Binary"] = xsTypeInfo(ASTType::ATOMIC, xs_base64Binary);
        xsTypes["hexBinary"] = xsTypeInfo(ASTType::ATOMIC, xs_hexBinary);
        xsTypes["anyURI"] = xsTypeInfo(ASTType::ATOMIC, xs_anyURI);
        xsTypes["QName"] = xsTypeInfo(ASTType::ATOMIC, xs_QName);
        xsTypes["NOTATION"] = xsTypeInfo(ASTType::ATOMIC, xs_NOTATION);

        xsTypes["anyType"] = xsTypeInfo(ASTType::ABSTRACT, xs_anyType);
        xsTypes["anySimpleType"] = xsTypeInfo(ASTType::ABSTRACT, xs_anySimpleType);
        xsTypes["IDREFS"] = xsTypeInfo(ASTType::LIST, xs_IDREFS);
        xsTypes["NMTOKENS"] = xsTypeInfo(ASTType::LIST, xs_NMTOKENS);
        xsTypes["ENTITIES"] = xsTypeInfo(ASTType::LIST, xs_ENTITIES);
        xsTypes["untyped"] = xsTypeInfo(ASTType::COMPLEX, xs_untyped);
    }

    void XQueryDriver::registerStandardFunctions(const char *uri_nsp, XQFunction *funcs)
    {
        std::string reg_name;

        while (funcs->local != "")
        {
            reg_name = std::string("{") + uri_nsp + "}" + funcs->local;

            U_ASSERT(XQueryDriver::stdFuncs.find(reg_name) == XQueryDriver::stdFuncs.end());

            XQueryDriver::stdFuncs[reg_name] = *funcs;
            XQueryDriver::stdFuncs[reg_name].uri = uri_nsp;
            XQueryDriver::stdFuncs[reg_name].mod_uri = "";

            funcs++;
        }
    }

    int XQueryDriver::getLibraryModule(const char *uri)
    {
        ASTNodesVector *mods;
        char *mod_str;
        XQueryModule *mod;

        mod_str = get_module(uri);

        if (!strcmp(mod_str, "#f"))
        {
            free(mod_str);
            return 1;
        }

        mods = dsGetASTNodesFromString(mod_str);

        free(mod_str);

        for (unsigned int i = 0; i < mods->size(); i++)
        {
            mod = new XQueryModule((*mods)[i], this);
            libModules[uri].push_back(mod);
            mod->doSemanticAnalysis();
        }

        delete mods; // not deep delete, since we reuse ast trees

        return 0;
    }

    ASTNode *XQueryDriver::getASTFromQuery(const char *query)
    {
        XQueryModule *tmp_mod;
        ASTNode *res;

        try
        {
            parse(query); // module for this query has been added to mods

            tmp_mod = mods.back();
            mods.pop_back();

            res = dynamic_cast<ASTQuery *>(dynamic_cast<ASTMainModule *>(tmp_mod->getTree())->query)->query->dup();

            delete tmp_mod;
        }
        catch (SednaUserException)
        {
            throw SYSTEM_EXCEPTION("internal parser error: subexpression syntax error");
        }

        return res;
    }

    XQFunction *XQueryDriver::getStdFuncInfo(const std::string &name) const
    {
        return &stdFuncs[name];
    }

    XQFunction *XQueryDriver::getLReturnFunctionInfo(const std::string &name)
    {
        XQFunction *xqf;

        // function should be resolved by sema
        U_ASSERT(libFuncs.find(name) != libFuncs.end());

        xqf = libFuncs[name];

        xqf->mod->getLReturnFunctionInfo(name, &xqf);

        return xqf;
    }

    XQVariable *XQueryDriver::getLReturnVariableInfo(const std::string &name)
    {
        XQVariable *xqv;

        // variable should be resolved by sema
        U_ASSERT(libVars.find(name) != libVars.end());

        xqv = libVars[name];

        xqv->mod->getLReturnVariableInfo(name, &xqv);

        return xqv;
    }

    PPQueryEssence *XQueryDriver::getQEPForModule(unsigned int ind, bool is_subquery)
    {
        PPQueryEssence *res = (mods[ind])->getQEP(is_subquery);

        if (!tr_globals::first_transaction) {
            res->optimizedPlan = (mods[ind])->getOPT(is_subquery);
            selectDataGraphs(res->optimizedPlan);

            XmlConstructor xmlConstructor(VirtualRootConstructor(0));
            debug_xml("opt.g", res->optimizedPlan->toXML(xmlConstructor).getLastChild());
        }

        // some errors might have happened
        if (gotErrors())
        {
            delete res;
            emitErrors(); // this function will throw an exception
        }

        return res;
    }

    void XQueryDriver::porLibModules(dynamic_context *parent_context)
    {
        std::map<std::string, modSequence>::iterator it;

        /*
         * The main caveat here is that we want to issue function
         * anf global vars ids _before_ we do main qep-build pass.
         * We must do it because, for example, functions from different
         * modules can reference each other.
         *
         * So, we do it in two-pass fashion: first we enumerate vars
         * and funcs and then perform qep-building.
         *
         * Also we process only library modules that we actually need. We need
         * only the modules we are using funcs and vars from.
         */

        // First step: setting dynamic contexts and enumerating
        for (it = libModules.begin(); it != libModules.end(); it++)
        {
            modSequence::iterator mit;

            for (mit = it->second.begin(); mit != it->second.end(); mit++)
            {
                if ((*mit)->module_is_needed())
                {
                    dynamic_context *dyn_cxt = new dynamic_context(new static_context());
                    parent_context->add_child_context(dyn_cxt);
                    (*mit)->set_dynamic_context(dyn_cxt);
                    (*mit)->enumerate_vars_funcs();
                }
            }
        }

        // Second step: processing funcs and vars
        for (it = libModules.begin(); it != libModules.end(); it++)
        {
            modSequence::iterator mit;

            for (mit = it->second.begin(); mit != it->second.end(); mit++)
            {
                if ((*mit)->module_is_needed())
                {
                    (*mit)->porLibModule();
                }
            }
        }
    }

    global_var_dsc XQueryDriver::getGlobalVariableId(const std::string &name)
    {
        XQVariable *xqv = libVars.find(name)->second;

        U_ASSERT(xqv->id.first && xqv->id.second != INVALID_VAR_DSC);

        return xqv->id;
    }

    function_id XQueryDriver::getGlobalFunctionId(const std::string &name)
    {
        XQFunction *xqf = libFuncs.find(name)->second;

        U_ASSERT(xqf->id.first);

        return xqf->id;
    }

    xmlscm_type XQueryDriver::getXsType(const char *type)
    {
        U_ASSERT(xsTypes.find(type) != xsTypes.end());

        return xsTypes[type].xtype;
    }
}

