/*
 * File:  cycle.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/lr/LRVisitor.h"
#include "tr/xqp/sema/cycle.h"
#include "tr/xqp/sema/Sema.h"
#include "common/errdbg/exceptions.h"

namespace sedna
{
    void Cycle::visit(ASTAlterUser &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTAttr &n)
    {
        if (n.cont)
            VisitNodesVector(n.cont, *this);
    }

    void Cycle::visit(ASTAttrConst &n)
    {
        if (n.name)
            n.name->accept(*this);

        if (n.expr)
            n.expr->accept(*this);
    }

    void Cycle::visit(ASTAttribTest &n)
    {
        if (n.type)
            n.type->accept(*this);
    }

    void Cycle::visit(ASTAxisStep &n)
    {
        if (n.cont)
            n.cont->accept(*this);

        n.test->accept(*this);

        if (n.preds)
        {
            for (unsigned int i = 0; i < n.preds->size(); i++)
                (*n.preds)[i]->accept(*this);
        }
    }

    void Cycle::visit(ASTBaseURI &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTBop &n)
    {
        n.lop->accept(*this);
        n.rop->accept(*this);
    }

    void Cycle::visit(ASTBoundSpaceDecl &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTCase &n)
    {
        if (n.var)
        {
            setParamMode();
            n.var->accept(*this);
            unsetParamMode();
        }

        n.expr->accept(*this);

        if (n.var)
            bound_vars.pop_back();
    }

    void Cycle::visit(ASTCast &n)
    {
        n.expr->accept(*this);
    }

    void Cycle::visit(ASTCastable &n)
    {
        n.expr->accept(*this);
    }

    void Cycle::visit(ASTCharCont &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTCommTest &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTCommentConst &n)
    {
        n.expr->accept(*this);
    }

    void Cycle::visit(ASTConstDecl &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTCreateColl &n)
    {
        n.coll->accept(*this);
    }

    void Cycle::visit(ASTCreateDoc &n)
    {
        n.doc->accept(*this);

        if (n.coll)
            n.coll->accept(*this);
    }

    void Cycle::visit(ASTCreateFtIndex &n)
    {
        n.name->accept(*this);
        n.path->accept(*this);

        if (n.cust_expr)
            n.cust_expr->accept(*this);
    }

    void Cycle::visit(ASTCreateIndex &n)
    {
        n.name->accept(*this);
        n.on_path->accept(*this);
        n.by_path->accept(*this);
    }

    void Cycle::visit(ASTCreateRole &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTCreateTrg &n)
    {
        n.path->accept(*this);

        // add special trigger variables
        bound_vars.push_back(XQVariable("NEW", NULL));
        bound_vars.push_back(XQVariable("OLD", NULL));
        bound_vars.push_back(XQVariable("WHERE", NULL));

        VisitNodesVector(n.do_exprs, *this);

        bound_vars.pop_back();
        bound_vars.pop_back();
        bound_vars.pop_back();

        if (n.trimmed_path)
            n.trimmed_path->accept(*this);
    }

    void Cycle::visit(ASTCreateUser &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTDDO &n)
    {
        n.expr->accept(*this);
    }

    void Cycle::visit(ASTDeclareCopyNsp &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTDefCollation &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTDefNamespaceDecl &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTDocConst &n)
    {
        n.expr->accept(*this);
    }

    void Cycle::visit(ASTDocTest &n)
    {
        if (n.elem_test)
            n.elem_test->accept(*this);
    }

    void Cycle::visit(ASTDropColl &n)
    {
        n.coll->accept(*this);
    }

    void Cycle::visit(ASTDropDoc &n)
    {
        n.doc->accept(*this);

        if (n.coll)
            n.coll->accept(*this);
    }

    void Cycle::visit(ASTDropFtIndex &n)
    {
        n.index->accept(*this);
    }

    void Cycle::visit(ASTDropIndex &n)
    {
        n.index->accept(*this);
    }

    void Cycle::visit(ASTDropMod &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTDropRole &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTDropTrg &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTDropUser &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTElem &n)
    {
        VisitNodesVector(n.attrs, *this);
        VisitNodesVector(n.cont, *this);
    }

    void Cycle::visit(ASTElemConst &n)
    {
        if (n.name)
            n.name->accept(*this);

        if (n.expr)
            n.expr->accept(*this);
    }

    void Cycle::visit(ASTElementTest &n)
    {
        if (n.type)
            n.type->accept(*this);
    }

    void Cycle::visit(ASTEmptyTest &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTError &n)
    {
        throw SYSTEM_EXCEPTION("If you see this, you are very unlucky. Anyway, this is an internal parser error.");
    }

    void Cycle::visit(ASTExtExpr &n)
    {
        if (n.expr)
            n.expr->accept(*this);
    }

    void Cycle::visit(ASTFilterStep &n)
    {
        if (n.cont)
            n.cont->accept(*this);

        if (n.expr)
            n.expr->accept(*this);

        if (n.preds)
        {
            for (unsigned int i = 0; i < n.preds->size(); i++)
                (*n.preds)[i]->accept(*this);
        }
    }

    void Cycle::visit(ASTFLWOR &n)
    {
        size_t count;

        VisitNodesVector(n.fls, *this);
        count = param_count;

        if (n.where)
            n.where->accept(*this);

        if (n.order_by)
            n.order_by->accept(*this);

        n.ret->accept(*this);

        bound_vars.erase(bound_vars.begin() + (bound_vars.size() - count), bound_vars.end());
    }

    void Cycle::visit(ASTFor &n)
    {
        n.expr->accept(*this);

        setParamMode();

        n.tv->accept(*this);
        if (n.pv)
            n.pv->accept(*this);

        unsetParamMode();
    }

    void Cycle::visit(ASTFunCall &n)
    {
        // "usual" function
        unsigned int arity = (n.params) ? n.params->size() : 0;
        std::string name = CREATE_INTNAME(*n.uri, *n.local);
        const XQFunction *xqf;

        // first of all, we should check arguments
        VisitNodesVector(n.params, *this);

        // then find the function
        xqf = Sema::findFunction(name, arity, mod, drv);

        if (xqf && xqf->decl)
        {
            bool extern_import = false;

            extern_import = (xqf->mod_uri != "" && (mod_chain.empty() || mod_chain.back() != *n.uri));

            if (extern_import)
                mod_chain.push_back(*n.uri);

            xqf->decl->accept(*this);

            if (extern_import)
            {
                U_ASSERT(mod_chain.back() == *n.uri);
                mod_chain.pop_back();
            }
        }
    }

    void Cycle::visit(ASTFuncDecl &n)
    {
        std::string name;
        unsigned int params_count = 0;

        name = CREATE_INTNAME(*n.func_uri, *n.local);

        // check if we've got a module cycle
        if (!mod_chain.empty())
        {
            std::string mod_uri = mod_chain.back();
            std::string err;

            for (int i = mod_chain.size() - 2; i >= 0; i--)
            {
                if (mod_chain[i] == mod_uri)
                {
                    for (unsigned int j = i; j < mod_chain.size() - 1; j++)
                    {
                        err.append(mod_chain[j]);
                        err.append(" => ");
                    }

                    err.append(mod_uri);
                    drv->error(XQST0093, err.c_str());
                    return;
                }
            }
        }

        // cycle cannot start from functions in main module
        if (chain.empty() && !mod->module_uri)
            return;

        // check chain
        for (int i = chain.size() - 1; i >= 0; i--)
            if (chain[i] == name) // we don't want to cycle on recursive fun-calls
                return;

        // set bound parameters
        if (n.params)
        {
            setParamMode();

            for (unsigned int i = 0; i < n.params->size(); i++)
                (*n.params)[i]->accept(*this);

            params_count = param_count;

            unsetParamMode();
        }

        // place itself in chain
        chain.push_back(name);

        // check body
        if (n.body)
            n.body->accept(*this); // side effect: param_count may change, so we save it to params_count

        // we should clear bounded variables
        if (n.params)
            bound_vars.erase(bound_vars.begin() + bound_vars.size() - params_count, bound_vars.end());

        // remove itself from chain
        chain.pop_back();
    }

    void Cycle::visit(ASTGrantPriv &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTGrantRole &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTIf &n)
    {
        n.i_expr->accept(*this);
        n.t_expr->accept(*this);
        n.e_expr->accept(*this);
    }

    void Cycle::visit(ASTInstOf &n)
    {
        n.expr->accept(*this);
    }

    void Cycle::visit(ASTItemTest &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTLet &n)
    {
        n.expr->accept(*this);

        setParamMode();
        n.tv->accept(*this);
        unsetParamMode();
    }

    void Cycle::visit(ASTLibModule &n)
    {
        is_imported = n.is_internal;

        mod_chain.clear();
        mod_chain.push_back(*mod->module_uri);

        n.prolog->accept(*this);
    }

    void Cycle::visit(ASTLit &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTLoadFile &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTLoadModule &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTMainModule &n)
    {
        if (drv == NULL)
            throw SYSTEM_EXCEPTION("Driver is not set for cycle analyzer!");

        n.prolog->accept(*this);
    }

    void Cycle::visit(ASTMetaCols &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTMetaDocs &n)
    {
        if (n.coll)
            n.coll->accept(*this);
    }

    void Cycle::visit(ASTMetaSchemaCol &n)
    {
        n.coll->accept(*this);
    }

    void Cycle::visit(ASTMetaSchemaDoc &n)
    {
        n.doc->accept(*this);

        if (n.coll)
            n.coll->accept(*this);
    }

    void Cycle::visit(ASTModImport &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTModuleDecl &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTNameTest &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTNamespaceDecl &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTNodeTest &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTNsp &n)
    {
        // nothing to do since namespaces cannot contain computed content
    }

    void Cycle::visit(ASTOption &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTOrdExpr &n)
    {
        n.expr->accept(*this);
    }

    void Cycle::visit(ASTOrder &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTOrderBy &n)
    {
        VisitNodesVector(n.specs, *this);
    }

    void Cycle::visit(ASTOrderEmpty &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTOrderMod &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTOrderModInt &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTOrderSpec &n)
    {
        n.expr->accept(*this);
    }

    void Cycle::visit(ASTPIConst &n)
    {
        if (n.name)
            n.name->accept(*this);

        if (n.expr)
            n.expr->accept(*this);
    }

    void Cycle::visit(ASTPi &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTPiTest &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTPosVar &n)
    {
        n.var->accept(*this);
    }

    void Cycle::visit(ASTPragma &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTPred &n)
    {
        std::vector<ASTPred::ASTConjunct>::iterator it;

        for (it = n.conjuncts.begin(); it != n.conjuncts.end(); it++)
        {
            it->expr->accept(*this);
        }

        for (it = n.others.begin(); it != n.others.end(); it++)
        {
            it->expr->accept(*this);
        }
    }

    void Cycle::visit(ASTProlog &n)
    {
        VisitNodesVector(n.decls, *this);
    }

    void Cycle::visit(ASTQName &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTQuantExpr &n)
    {
        n.expr->accept(*this);

        setParamMode();
        n.var->accept(*this);
        unsetParamMode();

        n.sat->accept(*this);

        bound_vars.pop_back();
    }

    void Cycle::visit(ASTQuery &n)
    {
        n.query->accept(*this);
    }

    void Cycle::visit(ASTRenameColl &n)
    {
        n.name_old->accept(*this);
        n.name_new->accept(*this);
    }

    void Cycle::visit(ASTRevokePriv &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTRevokeRole &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTSchemaAttrTest &n)
    {
        throw SYSTEM_EXCEPTION("If you see this, you are very unlucky. Anyway, this is an internal parser error.");
    }

    void Cycle::visit(ASTSchemaElemTest &n)
    {
        throw SYSTEM_EXCEPTION("If you see this, you are very unlucky. Anyway, this is an internal parser error.");
    }

    void Cycle::visit(ASTSeq &n)
    {
        for (unsigned int i = 0; i < n.exprs->size(); i++)
            (*n.exprs)[i]->accept(*this);
    }

    void Cycle::visit(ASTSpaceSeq &n)
    {
        n.expr->accept(*this);
    }

    void Cycle::visit(ASTTextConst &n)
    {
        n.expr->accept(*this);
    }

    void Cycle::visit(ASTTextTest &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTTreat &n)
    {
        n.expr->accept(*this);
    }

    void Cycle::visit(ASTType &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTTypeSeq &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTTypeSingle &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTTypeSwitch &n)
    {
        n.expr->accept(*this);
        VisitNodesVector(n.cases, *this);
        n.def_case->accept(*this);
    }

    void Cycle::visit(ASTTypeVar &n)
    {
        n.var->accept(*this);
    }

    void Cycle::visit(ASTUop &n)
    {
        n.expr->accept(*this);
    }

    void Cycle::visit(ASTUpdDel &n)
    {
        n.what->accept(*this);
    }

    void Cycle::visit(ASTUpdInsert &n)
    {
        n.what->accept(*this);
        n.where->accept(*this);
    }

    void Cycle::visit(ASTUpdMove &n)
    {
        setParamMode();
        n.var->accept(*this);
        unsetParamMode();

        n.what->accept(*this);
        n.where->accept(*this);

        bound_vars.pop_back();
    }

    void Cycle::visit(ASTUpdRename &n)
    {
        n.what->accept(*this);
    }

    void Cycle::visit(ASTUpdReplace &n)
    {
        setParamMode();
        n.var->accept(*this);
        unsetParamMode();

        n.what->accept(*this);
        n.new_expr->accept(*this);

        bound_vars.pop_back();
    }

    void Cycle::visit(ASTVar &n)
    {
        std::string name;

        if (*n.local == "$%v")
            return;

        // it's just a param
        if (param_mode)
        {
            param_count++;
            bound_vars.push_back(XQVariable(CREATE_INTNAME(*n.uri, *n.local).c_str(), &n));
            return;
        }

        // if it's a usual reference then find who we reference
        name = CREATE_INTNAME(*n.uri, *n.local);

        // first, check if variable is bound
        if (bound_vars.size() > 0)
        {
            for (int i = bound_vars.size() - 1; i >= 0; i--)
            {
                if (bound_vars[i].int_name == name)
                    return;
            }
        }

        // check, if we've already checked this var
        if (var_cache.find(name) != var_cache.end())
            return;

        // then, check prolog (for main modules only; for library module all vars are in libVars)
        if (mod_chain.empty() && mod->vars.find(name) != mod->vars.end())
        {
            XQVariable *xqv;
            mod->getVariableInfo(name, &xqv);

            xqv->var->accept(*this); // check corresponding declaration
            return;
        }

        if (mod->module_uri) // analysis started for lib module
        {
            if (drv->libVars.find(name) != drv->libVars.end() && drv->libVars[name])
            {
                bool extern_import = false;

                // we've got inter(intra)-module dependency here
                if (mod_chain.empty() || mod_chain.back() != *n.uri)
                {
                    extern_import = true;
                    mod_chain.push_back(*n.uri);
                }

                drv->libVars[name]->var->accept(*this);

                if (extern_import)
                {
                    U_ASSERT(mod_chain.back() == *n.uri);
                    mod_chain.pop_back();
                }
            }
        }
        // we don't need to check inter-module dependcies for main module since inter-module cycle cannot start from main module
    }

    void Cycle::visit(ASTVarDecl &n)
    {
        std::string err, name;
        ASTVar *var;

        var = dynamic_cast<ASTVar *>(n.var);
        U_ASSERT(var != NULL);
        name = CREATE_INTNAME(*var->uri, *var->local);

        // first, check if we've got a module cycle
        if (!mod_chain.empty())
        {
            std::string mod_uri = mod_chain.back();

            for (int i = mod_chain.size() - 2; i >= 0; i--)
            {
                if (mod_chain[i] == mod_uri)
                {
                    for (unsigned int j = i; j < mod_chain.size() - 1; j++)
                    {
                        err.append(mod_chain[j]);
                        err.append(" => ");
                    }

                    err.append(mod_uri);
                    drv->error(XQST0093, err.c_str());
                    var_cache[name] = &n; // cache the result

                    return;
                }
            }
        }

        // then, check if we've got a variable cycle
        for (int i = chain.size() - 1; i >= 0; i--)
        {
            if (chain[i] == name)
            {
                for (unsigned int j = i; j < chain.size(); j++)
                {
                    err.append(chain[j]);
                    err.append(" => ");
                }

                err.append(name);
                drv->error(XQST0054, err.c_str());
                var_cache[name] = &n; // cache the result

                return;
            }
        }

        chain.push_back(name);
        n.expr->accept(*this);
        chain.pop_back();
    }

    void Cycle::visit(ASTVersionDecl &n)
    {
        // nothing to do
    }

    void Cycle::visit(ASTXMLComm &n)
    {
        // nothing to do
    }

    void Cycle::setParamMode()
    {
        param_mode = true;
        param_count = 0;
    }

    void Cycle::unsetParamMode()
    {
        param_mode = false;
    }
}
