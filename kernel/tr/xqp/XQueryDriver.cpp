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

#include "tr/pq/pq.h"
#include "tr/xqp/serial/deser.h"

namespace sedna
{
    XQFunctionInfo XQueryDriver::stdFuncs;

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
        std::string err_msg;
        std::istringstream query_stream(query);
        XQueryLexer lex(*this, &query_stream);
        XQueryParser parser(*this);

        lexer = &lex; // parser use lexer to get next token
        res = parser.parse();
        lexer = NULL;

        emitErrors();

        return res;
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

    void XQueryDriver::addModule(ASTNode *mod)
    {
        XQueryModule *xqmod = new XQueryModule(mod, this);

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

    StringVector XQueryDriver::getLRRepresentation()
    {
        StringVector vec;

        for (unsigned int i = 0; i < mods.size(); i++)
            vec.push_back(mods[i]->getLR());

        return vec;
    }

    std::string XQueryDriver::getLRForModules(const XQueryModule *mod)
    {
        std::string res;
        std::set<std::string> lred;
        std::vector<std::string> imported;

        imported = mod->getImportedModules();

        for (unsigned int i = 0; i < imported.size(); i++)
            res += getLRForModule(imported[i].c_str(), lred);

        return res;
    }

    std::string XQueryDriver::getLRForModule(const char *mod_uri, std::set<std::string> &lred)
    {
        std::string res, lr, res_mod;
        std::vector<XQueryModule *> modules;

        if (libModules.find(mod_uri) == libModules.end())
            return "";

        modules = libModules[mod_uri];

        // then, lr modules themselves
        if (lred.find(mod_uri) == lred.end())
        {
            for (unsigned int i = 0; i < modules.size(); i++)
            {
                lr = modules[i]->getLR();
                //lr = prepare_module(lr); // call chicken to optimize module
                res_mod += lr;
            }

            lred.insert(mod_uri);
        }
        else
        {
            return res;
        }

        // first, lr children modules
        for (unsigned int i = 0; i < modules.size(); i++)
        {
            std::vector<std::string> imported = modules[i]->getImportedModules();

            for (unsigned int j = 0; j < imported.size(); j++)
                res += getLRForModule(imported[j].c_str(), lred);
        }

        res += res_mod;

        return res;
    }

    StringVector XQueryDriver::getIRRepresentation()
    {
        StringVector vec;

        for (unsigned int i = 0; i < mods.size(); i++)
            vec.push_back(mods[i]->getIR());

        return vec;
    }

    void XQueryDriver::initXQueryInfo()
    {
        // predefined XQuery types
        xsTypes["anyAtomicType"] = ASTType::ATOMIC;
        xsTypes["string"] = ASTType::ATOMIC;
        xsTypes["normalizedString"] = ASTType::ATOMIC;
        xsTypes["token"] = ASTType::ATOMIC;
        xsTypes["language"] = ASTType::ATOMIC;
        xsTypes["NMTOKEN"] = ASTType::ATOMIC;
        xsTypes["Name"] = ASTType::ATOMIC;
        xsTypes["NCName"] = ASTType::ATOMIC;
        xsTypes["ID"] = ASTType::ATOMIC;
        xsTypes["IDREF"] = ASTType::ATOMIC;
        xsTypes["ENTITY"] = ASTType::ATOMIC;
        xsTypes["untypedAtomic"] = ASTType::ATOMIC;
        xsTypes["dateTime"] = ASTType::ATOMIC;
        xsTypes["date"] = ASTType::ATOMIC;
        xsTypes["time"] = ASTType::ATOMIC;
        xsTypes["duration"] = ASTType::ATOMIC;
        xsTypes["yearMonthDuration"] = ASTType::ATOMIC;
        xsTypes["dayTimeDuration"] = ASTType::ATOMIC;
        xsTypes["float"] = ASTType::ATOMIC;
        xsTypes["double"] = ASTType::ATOMIC;
        xsTypes["decimal"] = ASTType::ATOMIC;
        xsTypes["integer"] = ASTType::ATOMIC;
        xsTypes["nonPositiveInteger"] = ASTType::ATOMIC;
        xsTypes["negativeInteger"] = ASTType::ATOMIC;
        xsTypes["long"] = ASTType::ATOMIC;
        xsTypes["int"] = ASTType::ATOMIC;
        xsTypes["short"] = ASTType::ATOMIC;
        xsTypes["byte"] = ASTType::ATOMIC;
        xsTypes["nonNegativeInteger"] = ASTType::ATOMIC;
        xsTypes["positiveInteger"] = ASTType::ATOMIC;
        xsTypes["unsignedLong"] = ASTType::ATOMIC;
        xsTypes["unsignedInt"] = ASTType::ATOMIC;
        xsTypes["unsignedShort"] = ASTType::ATOMIC;
        xsTypes["unsignedByte"] = ASTType::ATOMIC;
        xsTypes["gYearMonth"] = ASTType::ATOMIC;
        xsTypes["gMonthDay"] = ASTType::ATOMIC;
        xsTypes["gDay"] = ASTType::ATOMIC;
        xsTypes["gMonth"] = ASTType::ATOMIC;
        xsTypes["gYear"] = ASTType::ATOMIC;
        xsTypes["boolean"] = ASTType::ATOMIC;
        xsTypes["base64Binary"] = ASTType::ATOMIC;
        xsTypes["hexBinary"] = ASTType::ATOMIC;
        xsTypes["anyURI"] = ASTType::ATOMIC;
        xsTypes["QName"] = ASTType::ATOMIC;
        xsTypes["NOTATION"] = ASTType::ATOMIC;

        xsTypes["anyType"] = ASTType::COMPLEX;
        xsTypes["anySimpleType"] = ASTType::SIMPLE;
        xsTypes["IDREFS"] = ASTType::LIST;
        xsTypes["NMTOKENS"] = ASTType::LIST;
        xsTypes["ENTITIES"] = ASTType::LIST;
        xsTypes["untyped"] = ASTType::COMPLEX;
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
        catch (SednaUserException &e)
        {
            throw SYSTEM_EXCEPTION("internal parser error: subexpression syntax error");
        }

        return res;
    }

    XQFunction XQueryDriver::getStdFuncInfo(const std::string &name) const
    {
        return stdFuncs[name];
    }

    XQFunction XQueryDriver::getLReturnFunctionInfo(const std::string &name)
    {
        XQFunction xqf;
        modSequence ms;

        ms = libModules[libFuncs[name].uri];

        for (modSequence::iterator it = ms.begin(); it != ms.end(); it++)
        {
            if ((*it)->getLReturnFunctionInfo(name, xqf))
                break;
        }

        return xqf;
    }

    XQVariable XQueryDriver::getLReturnVariableInfo(const std::string &name)
    {
        XQVariable xqv(name.c_str(), NULL);
        modSequence ms;
        std::string uri;
        unsigned int fp, lp;

        // get uri from name
        fp = name.find('{');
        lp = name.find('}');
        U_ASSERT(fp != std::string::npos && lp != std::string::npos);
        uri = name.substr(fp + 1, lp - fp - 1);

        ms = libModules[uri];

        for (modSequence::iterator it = ms.begin(); it != ms.end(); it++)
        {
            if ((*it)->getLReturnVariableInfo(name, xqv))
                break;
        }

        return xqv;
    }

    size_t XQueryDriver::getVarCount() const
    {
        return libVars.size();
    }

    size_t XQueryDriver::getFuncCount() const
    {
        return libFuncs.size();
    }

    size_t XQueryDriver::getLibModCount() const
    {
        size_t count = 0;
        std::map<std::string, modSequence>::const_iterator it;

        for (it = libModules.begin(); it != libModules.end(); it++)
            count += it->second.size();

        return count;
    }

    PPQueryEssence *XQueryDriver::getQEPForModule(unsigned int ind)
    {
        return (mods[ind])->getQEP();
    }

    void XQueryDriver::porLibModules()
    {
        std::map<std::string, modSequence>::iterator it;

        for (it = libModules.begin(); it != libModules.end(); it++)
        {
            modSequence::iterator mit;

            for (mit = it->second.begin(); mit != it->second.end(); it++)
            {
                (*mit)->porLibModule();
            }
        }
    }

    var_id XQueryDriver::getGlobalVariableId(const std::string &name)
    {
        return libVars.find(name)->second->getId();
    }

    var_id XQueryDriver::getGlobalFunctionId(const std::string &name)
    {
        return libFuncs.find(name)->second.decl->getId();
    }
}
