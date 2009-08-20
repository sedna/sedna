/*
 * File:  Sema.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/visitor/LRVisitor.h"
#include "tr/xqp/sema/Sema.h"
#include "common/errdbg/exceptions.h"
#include "tr/executor/base/xs_names.h"

#define DIAG_DUP_PROLOG(bl1, bc1, bl2, bc2) (std::string("first declaration at (") + int2string(bl1) + ":" + int2string(bc1) + ")")
#define DIAG_QNAME(pref, loc) (std::string("cannot resolve QName: ") + pref + ":" + loc)

#define CREATE_INTNAME(u, l) ((u == "") ? (l) : (std::string("{") + (u) + std::string("}") + (l)))

namespace sedna
{
    static const char *axis_str[] = {
        "child ",
        "descendant ",
        "attr-axis ",
        "self ",
        "descendant-or-self ",
        "following-sibling ",
        "following ",
        "parent ",
        "ancestor ",
        "preceding-sibling ",
        "preceding ",
        "ancestor-or-self ",
    };

    static const char *priveleges[] = {
        "CREATE-USER",
        "CREATE-DOCUMENT",
        "CREATE-COLLECTION",
        "CREATE-INDEX",
        "CREATE-TRIGGER",
        "LOAD",
        "DROP",
        "QUERY",
        "INSERT",
        "DELETE",
        "RENAME",
        "REPLACE",
        "RETRIEVE-METADATA",
        "ALL",
        NULL
    };

    void Sema::visit(ASTAlterUser &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTAttr &n)
    {
        const char *uri;

        // first, try to resolve prefix
        uri = resolveQName(n.loc, n.pref->c_str(), "");

        if (uri)
            n.uri = new std::string(uri);

        if (n.cont)
            VisitNodesVector(n.cont, *this);
    }

    void Sema::visit(ASTAttrConst &n)
    {
        if (n.pref)
        {
            resolveQName(n.loc, n.pref->c_str(), "");
        }
        else
        {
            n.name->accept(*this);
        }

        if (n.expr)
            n.expr->accept(*this);
    }

    void Sema::visit(ASTAttribTest &n)
    {
        att_test = true;
        if (n.name) n.name->accept(*this);
        att_test = false;

        if (n.type) n.type->accept(*this);
    }

    void Sema::visit(ASTAxis &n)
    {
        n.expr->accept(*this);
        n.test->accept(*this);
    }

    void Sema::visit(ASTAxisStep &n)
    {
        throw SYSTEM_EXCEPTION("If you see this, you are very unlucky. Anyway, this is an internal parser error.");
    }

    void Sema::visit(ASTBaseURI &n)
    {
        std::string loc;

        if (dupLocations[PrologBaseURI])
        {
            loc = DIAG_DUP_PROLOG(dupLocations[PrologBaseURI]->begin.line, dupLocations[PrologBaseURI]->begin.column,
                                n.loc.begin.line, n.loc.begin.column);

            drv->error(n.loc, XQST0032, loc.c_str());
        }
        else
        {
            dupLocations[PrologBaseURI] = &n.loc;
        }
    }

    void Sema::visit(ASTBop &n)
    {
        n.lop->accept(*this);
        n.rop->accept(*this);
    }

    void Sema::visit(ASTBoundSpaceDecl &n)
    {
        std::string loc;

        if (dupLocations[PrologBoundSpace])
        {
            loc = DIAG_DUP_PROLOG(dupLocations[PrologBoundSpace]->begin.line, dupLocations[PrologBoundSpace]->begin.column,
                                n.loc.begin.line, n.loc.begin.column);

            drv->error(n.loc, XQST0068, loc.c_str());
        }
        else
        {
            dupLocations[PrologBoundSpace] = &n.loc;
        }
    }

    void Sema::visit(ASTCase &n)
    {
        if (n.type)
            n.type->accept(*this);

        n.fd->accept(*this);
    }

    void Sema::visit(ASTCast &n)
    {
        ASTLit *lit;
        ASTType *type;
        std::string *t_pref, *t_loc;
        std::string *l_pref, *l_loc;
        const char *uri;

        n.expr->accept(*this);

        casting_mode = true;
        n.type->accept(*this);
        casting_mode = false;

        // one special case requires static resolving: cast literal xs:string to xs:QName
        U_ASSERT(dynamic_cast<ASTTypeSingle *>(n.type) != NULL);
        U_ASSERT(dynamic_cast<ASTType *>(dynamic_cast<ASTTypeSingle *>(n.type)->type) != NULL);

        lit = dynamic_cast<ASTLit *>(n.expr);
        type = static_cast<ASTType *>(static_cast<ASTTypeSingle *>(n.type)->type);
        ASTParseQName(type->name, &t_pref, &t_loc);

        if (lit && lit->type == ASTLit::STRING && *t_loc == "QName")
        {
            ASTParseQName(lit->lit, &l_pref, &l_loc);

            if (l_pref->size() == 0 && lit->lit->find(':') != std::string::npos)
                *l_pref = ":";

            if (l_pref->size() > 0 && !check_constraints_for_xs_NCName(l_pref->c_str()))
                drv->error(n.loc, FORG0001, std::string("invalid prefix in QName constructor: ") + *l_pref);

            if (!check_constraints_for_xs_NCName(l_loc->c_str()))
                drv->error(n.loc, FORG0001, std::string("invalid local part in QName constructor: ") + *l_loc);

            uri = resolveQName(n.loc, l_pref->c_str(), NULL, FONS0004);

            if (uri)
            {
                ASTNode *qname = new ASTQName(n.loc, new std::string(uri), l_pref, l_loc);

                modifyParent(qname, true, true);
            }
            else
            {
                delete l_pref;
                delete l_loc;
            }

            delete t_pref;
            delete t_loc;
        }
    }

    void Sema::visit(ASTCastable &n)
    {
        ASTLit *lit;
        ASTType *type;
        std::string *t_pref, *t_loc;
        std::string *l_pref, *l_loc;
        const char *uri;

        n.expr->accept(*this);

        casting_mode = true;
        n.type->accept(*this);
        casting_mode = false;

        // one special case requires static resolving: cast literal xs:string to xs:QName
        U_ASSERT(dynamic_cast<ASTTypeSingle *>(n.type) != NULL);
        U_ASSERT(dynamic_cast<ASTType *>(dynamic_cast<ASTTypeSingle *>(n.type)->type) != NULL);

        lit = dynamic_cast<ASTLit *>(n.expr);
        type = static_cast<ASTType *>(static_cast<ASTTypeSingle *>(n.type)->type);
        ASTParseQName(type->name, &t_pref, &t_loc);

        if (lit && lit->type == ASTLit::STRING && *t_loc == "QName")
        {
            ASTNode *tf_node;
            ASTParseQName(lit->lit, &l_pref, &l_loc);

            if (l_pref->size() == 0 && lit->lit->find(':') != std::string::npos)
                *l_pref = ":";

            if (l_pref->size() > 0 && !check_constraints_for_xs_NCName(l_pref->c_str()))
            {
                tf_node = new ASTFunCall(n.loc, new std::string("fn:false"));
            }
            else if (!check_constraints_for_xs_NCName(l_loc->c_str()))
            {
                tf_node = new ASTFunCall(n.loc, new std::string("fn:false"));
            }
            else
            {
                uri = resolveQName(n.loc, l_pref->c_str(), NULL, -1); // -1 disables error reporting

                if (uri)
                {
                    tf_node = new ASTFunCall(n.loc, new std::string("fn:true"));
                }
                else
                {
                    tf_node = new ASTFunCall(n.loc, new std::string("fn:false"));
                }
            }

            modifyParent(tf_node, true, true);

            delete l_pref;
            delete l_loc;
            delete t_pref;
            delete t_loc;
        }
    }

    void Sema::visit(ASTCharCont &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTCommTest &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTCommentConst &n)
    {
        n.expr->accept(*this);
    }

    void Sema::visit(ASTConstDecl &n)
    {
        std::string loc;

        if (dupLocations[PrologDeclConst])
        {
            loc = DIAG_DUP_PROLOG(dupLocations[PrologDeclConst]->begin.line, dupLocations[PrologDeclConst]->begin.column,
                                n.loc.begin.line, n.loc.begin.column);

            drv->error(n.loc, XQST0067, loc.c_str());
        }
        else
        {
            dupLocations[PrologDeclConst] = &n.loc;
        }
    }

    void Sema::visit(ASTCreateColl &n)
    {
        n.coll->accept(*this);
    }

    void Sema::visit(ASTCreateDoc &n)
    {
        n.doc->accept(*this);

        if (n.coll)
            n.coll->accept(*this);
    }

    void Sema::visit(ASTCreateFtIndex &n)
    {
        n.name->accept(*this);
        n.path->accept(*this);

        // check path for well-formdness
        if (getDocCollFromAbsXPath(n.path) == NULL)
            return;

        if (*n.type == "xml" || *n.type == "string-value" || *n.type == "delimited-value" ||
            *n.type == "!xml" || *n.type == "!string-value" || *n.type == "!delimited-value")
        {
            if (n.cust_expr)
            {
                drv->error(n.loc, SE5081, std::string("customized-value expression is not expected for ") + *n.type + " type");
            }

            return;
        }

        if (*n.type == "customized-value" || *n.type == "!customized-value")
        {
            if (!n.cust_expr)
                drv->error(n.loc, SE5081, "customized-value expression is absent");
            else
                n.cust_expr->accept(*this);

            return;
        }

        drv->error(n.loc, SE5080, *n.type);
    }

    void Sema::visit(ASTCreateIndex &n)
    {
        ASTNode *doccoll;

        n.name->accept(*this);
        n.on_path->accept(*this);

        // by-path is relatieve so we should add $%v as bounded to avoid semantic errors
        bound_vars.push_back(XQVariable("$%v", NULL));
        n.by_path->accept(*this);
        bound_vars.pop_back();

        n.type->accept(*this);

        if ((doccoll = getDocCollFromAbsXPath(n.on_path)) == NULL)
            return;

        n.by_path = modifyRelIndexXPath(n.by_path, doccoll);
    }

    void Sema::visit(ASTCreateRole &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTCreateTrg &n)
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

        if (!getDocCollFromAbsXPath(n.path))
            return;

        if (n.t_mod == ASTCreateTrg::BEFORE && n.a_mod == ASTCreateTrg::INSERT && n.g_mod == ASTCreateTrg::NODE)
        {
            getLeafAndTrimmedPath(n.path, &n.leaf_name, &n.leaf_type, &n.trimmed_path);
        }

        ASTNode *last = n.do_exprs->back();

        if (n.g_mod == ASTCreateTrg::NODE && (dynamic_cast<ASTUpdInsert *>(last) ||
            dynamic_cast<ASTUpdDel *>(last) || dynamic_cast<ASTUpdReplace *>(last) ||
            dynamic_cast<ASTUpdRename *>(last) || dynamic_cast<ASTUpdMove *>(last)))
        {
            drv->error(last->loc, SE3210, NULL);
        }
    }

    void Sema::visit(ASTCreateUser &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTDDO &n)
    {
        n.expr->accept(*this);
    }

    void Sema::visit(ASTDeclareCopyNsp &n)
    {
        std::string loc;

        if (dupLocations[PrologCopyNsp])
        {
            loc = DIAG_DUP_PROLOG(dupLocations[PrologCopyNsp]->begin.line, dupLocations[PrologCopyNsp]->begin.column,
                                n.loc.begin.line, n.loc.begin.column);

            drv->error(n.loc, XQST0055, loc.c_str());
        }
        else
        {
            dupLocations[PrologCopyNsp] = &n.loc;
        }
    }

    void Sema::visit(ASTDefCollation &n)
    {
        std::string loc;

        if (dupLocations[PrologColl])
        {
            loc = DIAG_DUP_PROLOG(dupLocations[PrologColl]->begin.line, dupLocations[PrologColl]->begin.column,
                                n.loc.begin.line, n.loc.begin.column);

            drv->error(n.loc, XQST0038, loc.c_str());
        }
        else
        {
            dupLocations[PrologColl] = &n.loc;
        }
    }

    void Sema::visit(ASTDefNamespaceDecl &n)
    {
        std::string err;

        if (n.type == ASTDefNamespaceDecl::ELEMENT)
        {
            if (mod->defElemNsp.second != NULL)
            {
                err = "default element namespace is already defined at (" + int2string(mod->defElemNsp.second->begin.line) +
                    ":" + int2string(mod->defElemNsp.second->begin.column) + ")";

                drv->error(n.loc, XQST0066, err.c_str());
                return;
            }

            mod->defElemNsp = nsPair(*n.uri, &n.loc);
        }
        else
        {
            if (mod->defFuncNsp.second != NULL)
            {
                err = "default function namespace is already defined at (" + int2string(mod->defFuncNsp.second->begin.line) +
                    ":" + int2string(mod->defFuncNsp.second->begin.column) + ")";

                drv->error(n.loc, XQST0066, err.c_str());
                return;
            }

            mod->defFuncNsp = nsPair(*n.uri, &n.loc);
        }
    }

    void Sema::visit(ASTDocConst &n)
    {
        n.expr->accept(*this);
    }

    void Sema::visit(ASTDocTest &n)
    {
        if (n.elem_test) n.elem_test->accept(*this);
    }

    void Sema::visit(ASTDropColl &n)
    {
        n.coll->accept(*this);
    }

    void Sema::visit(ASTDropDoc &n)
    {
        n.doc->accept(*this);

        if (n.coll)
            n.coll->accept(*this);
    }

    void Sema::visit(ASTDropFtIndex &n)
    {
        n.index->accept(*this);
    }

    void Sema::visit(ASTDropIndex &n)
    {
        n.index->accept(*this);
    }

    void Sema::visit(ASTDropMod &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTDropRole &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTDropTrg &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTDropUser &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTElem &n)
    {
        std::set<std::string> attrs_names; // for duplicate resolving
        ASTNodesVector attrs;
        ASTNodesVector nsps;

        // first of all, we should process attributes, since namespaces can affect QNames resolving
        elemNsps.push_back(elNspInfo(nsBindType(), nsPair()));

        // first, establish in-scope namespaces context
        if (elemNsps.size() == 1)
            elemNsps.back().second = mod->defElemNsp;
        else
            elemNsps.back().second = elemNsps[elemNsps.size() - 2].second;

        elemNsps.back().second.second = NULL; // get rid of location to allow redefinition

        // we should process namespaces and attributes differently; so first we separate them
        if (n.attrs)
        {
            for (unsigned int i = 0; i < n.attrs->size(); i++)
            {
                if (ASTNsp *ns = dynamic_cast<ASTNsp *>((*n.attrs)[i]))
                    nsps.push_back(ns);
                else
                    attrs.push_back((*n.attrs)[i]);
            }

            // also, regroup attrs to contain first namespaces and then attributes
            n.attrs->clear();

            for (unsigned int i = 0; i < nsps.size(); i++)
                n.attrs->push_back(nsps[i]);
            for (unsigned int i = 0; i < attrs.size(); i++)
                n.attrs->push_back(attrs[i]);
        }

        // then populate in-scope namespaces by going throug namespaces declarations
        VisitNodesVector(&nsps, *this);

        // resolve element QName (this is affected by in-scope namespaces!)
        resolveQName(n.loc, n.pref->c_str(), NULL);

        // then go through attributes declarations
        VisitNodesVector(&attrs, *this);

        // check for dups among attribute names
        for (unsigned int i = 0; i < attrs.size(); i++)
        {
            ASTAttr *a = dynamic_cast<ASTAttr *>(attrs[i]);
            U_ASSERT(a != NULL);

            if (a->uri) // uri can be NULL in case of resolving error
            {
                std::string name = CREATE_INTNAME(*a->uri, *a->local);

                if (attrs_names.find(name) != attrs_names.end())
                {
                    drv->error(a->loc, XQST0040, name + " is already defined");
                    break;
                }
                else
                {
                    attrs_names.insert(name);
                }
            }
        }

        // then, we should parse content
        VisitNodesVector(n.cont, *this);

        // get rid of in-scope namespaces
        elemNsps.pop_back();
    }

    void Sema::visit(ASTElemConst &n)
    {
        if (n.pref)
        {
            resolveQName(n.loc, n.pref->c_str(), NULL);
        }
        else
        {
            n.name->accept(*this);
        }

        if (n.expr)
            n.expr->accept(*this);
    }

    void Sema::visit(ASTElementTest &n)
    {
        if (n.name) n.name->accept(*this);
        if (n.type) n.type->accept(*this);
    }

    void Sema::visit(ASTEmptyTest &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTError &n)
    {
        throw SYSTEM_EXCEPTION("If you see this, you are very unlucky. Anyway, this is an internal parser error.");
    }

    void Sema::visit(ASTExtExpr &n)
    {
        VisitNodesVector(n.pragmas, *this);

        if (n.expr)
            n.expr->accept(*this);
        else
            drv->error(n.loc, XQST0079, NULL); // since we don't support any pragmas yet
    }

    void Sema::visit(ASTFilterStep &n)
    {
        throw SYSTEM_EXCEPTION("If you see this, you are very unlucky. Anyway, this is an internal parser error.");
    }

    void Sema::visit(ASTFor &n)
    {
        param_mode = true;
        param_count = 0;

        n.tv->accept(*this);
        if (n.pv)
            n.pv->accept(*this);

        param_mode = false;

        if (param_count == 2) // got usual+positional variables
        {
            std::string pos_name, for_name;

            U_ASSERT(bound_vars.size() >= 2);

            pos_name = bound_vars.back().first;
            bound_vars.pop_back();

            for_name = bound_vars.back().first;
            bound_vars.pop_back();

            if (pos_name == for_name)
            {
                drv->error(n.loc, XQST0089, pos_name);
            }
        }
        else if (param_count == 1)
        {
            bound_vars.pop_back();
        }

        n.expr->accept(*this);
        n.fd->accept(*this);
    }

    void Sema::visit(ASTFunCall &n)
    {
        const char *uri;

        // first, resolve the prefix
        if (!n.uri)
        {
            // first, resolve the prefix
            uri = resolveQName(n.loc, n.pref->c_str(), mod->defFuncNsp.first.c_str());

            if (!uri) return;

            n.uri = new std::string(uri);
        }

        // then we check if we've got constructor function in fact
        if (*n.uri == "http://www.w3.org/2001/XMLSchema" && *n.local != "anyAtomicType" && *n.local != "NOTATION" &&
             drv->xsTypes.find(*n.local) != drv->xsTypes.end() && drv->xsTypes[*n.local] == ASTType::ATOMIC)
        {
            // check if we've got only one argument
            if (!n.params || n.params->size() != 1)
            {
                drv->error(n.loc, XPST0017, "constructor function must have exactly one argument");
                return;
            }

            // create cast expression
            ASTCast *cast = new ASTCast(n.loc, n.params->back(), new ASTTypeSingle(n.loc,
                                        new ASTType(n.loc, new std::string(*n.pref + ":" + *n.local), ASTType::ATOMIC), ASTTypeSingle::OPT));

            // attach it instead of us
            modifyParent(cast, true, true);

            n.params->pop_back(); // to avoid dup
            delete n.params;
            n.params = NULL;

            return;
        }

        // "usual" function
        unsigned int arity = (n.params) ? n.params->size() : 0;
        std::string name = CREATE_INTNAME(*n.uri, *n.local);
        const XQFunction *xqf;

        // first of all, we should check arguments
        VisitNodesVector(n.params, *this);

        // then find the function
        xqf = findFunction(name, arity, mod, drv);

        if (xqf == NULL) // not found, try to resolve later
        {
            // ignore external module's function imports on load module phase
            if (mod->module_uri && !is_imported && *n.uri != *mod->module_uri && mod->imported.find(*n.uri) != mod->imported.end())
                return;

            if (mod->unres_funcs.find(name) == mod->unres_funcs.end())
            {
                XQFunction fun;

                fun.uri = *n.uri;
                fun.local = *n.local;
                fun.min_arg = fun.max_arg = arity;
                fun.int_name = "";
                fun.decl = NULL;
                fun.loc = &n.loc;
                fun.mod_uri = (mod->module_uri) ? *mod->module_uri : "";

                mod->unres_funcs[name + "/" + int2string(arity)] = fun;
            }

            return;
        }

        n.int_name = new std::string(xqf->int_name);

        if (*n.int_name != "") // standard function
            rewriteStdFunCall(n, *n.int_name);
    }

    void Sema::visit(ASTFunDef &n)
    {
        unsigned int params = 0;

        param_mode = true;
        param_count = 0;
        VisitNodesVector(n.vars, *this);
        params = param_count;
        param_mode = false;

        // we may have one problem here; if for expression has overlapping bindings, fun-def will contain duplicate vars, which is not an error
        // so, we just get rid of duplicates respecting later bindings
        if (params == n.vars->size() && params > 1) // no error occured
        {
            unsigned int offs = 0, i, j;

            for (i = bound_vars.size() - params; i < bound_vars.size(); i++)
            {
                for (j = i + 1; j < bound_vars.size(); j++)
                    if (bound_vars[j].first == bound_vars[i].first)
                    {
                        delete (*n.vars)[i - bound_vars.size() + params - offs];
                        n.vars->erase(n.vars->begin() + i - bound_vars.size() + params - offs);
                        offs++;
                        break;
                    }
            }
        }

        n.fun->accept(*this);

        bound_vars.erase(bound_vars.begin() + bound_vars.size() - params, bound_vars.end());
    }

    void Sema::visit(ASTFuncDecl &n)
    {
        const char *uri;
        XQFunction func;
        std::string name;
        unsigned int params_count = 0;

        if (!n.func_uri)
        {
            // first, resolve the prefix
            uri = resolveQName(n.loc, n.pref->c_str(), mod->defFuncNsp.first.c_str());

            if (!uri) return;

            n.func_uri = new std::string(uri);

            if (!strlen(uri))
            {
                drv->error(n.loc, XQST0060, std::string("function '") + *n.pref + ((n.pref->size()) ? ":" : "") + *n.local + "' is in no namespace");
                return;
            }

            if (mod->module_uri && *mod->module_uri != *n.func_uri)
            {
                drv->error(n.loc, XQST0048,
                            std::string("function '") + *n.pref + ((n.pref->size()) ? ":" : "") + *n.local + "' is not in the library module namespace " + *mod->module_uri);

                return;
            }

            if (*n.func_uri == "http://www.w3.org/XML/1998/namespace" ||
                *n.func_uri == "http://www.w3.org/2001/XMLSchema" ||
                *n.func_uri == "http://www.w3.org/2001/XMLSchema-instance" ||
                *n.func_uri == "http://www.w3.org/2005/xpath-functions")
            {
                drv->error(n.loc, XQST0045,
                            std::string("function '") + *n.pref + ((n.pref->size()) ? ":" : "") + *n.local + "' must not be in namespace " + *n.func_uri);
                return;
            }
        }

        func.uri = *n.func_uri;
        func.local = *n.local;
        func.min_arg = func.max_arg = (n.params) ? n.params->size() : 0;
        func.int_name = "";
        func.decl = (n.body) ? &n : NULL;
        func.loc = &n.loc;
        func.mod_uri = (mod->module_uri) ? *mod->module_uri : "";

        name = CREATE_INTNAME(*n.func_uri, *n.local);

        // check for clash in standard functions
        if (findFunction(name, func.min_arg, mod, drv))
        {
            drv->error(n.loc, XQST0034,
                std::string("function '") + *n.func_uri + ((n.func_uri->size() == 0) ? "" : ":") +
                        *n.local + "(" + int2string(func.min_arg) + ")' has been already declared");

            return;
        }

        mod->funcs[name + "/" + int2string(func.min_arg)] = func;

        if (mod->module_uri)
        {
            std::string name_wa = name + "/" + int2string(func.min_arg);

            if (drv->libFuncs.find(name_wa) != drv->libFuncs.end()) // function is being exported by another sub-module
            {
                drv->error(n.loc, XQST0034,
                           std::string("function ") + name_wa + " has been already declared in another module with the same namespace");
            }
            else
            {
                drv->libFuncs[name + "/" + int2string(func.min_arg)] = func;
            }
        }

        // check parameters
        if (n.params)
        {
            param_mode = true;
            param_count = 0;

            for (unsigned int i = 0; i < n.params->size(); i++)
                (*n.params)[i]->accept(*this);

            // check for duplicate params
            for (unsigned int i = bound_vars.size() - 1; i >= bound_vars.size() - param_count + 1; i--)
            {
                for (unsigned int j = bound_vars.size() - param_count; j < i; j++)
                {
                    if (bound_vars[j].first == bound_vars[i].first)
                    {
                        drv->error(n.loc, XQST0039, std::string("variable ") + bound_vars[j].first);
                        return;
                    }
                }
            }

            params_count = param_count;

            param_mode = false;
        }

        // check result type
        if (n.ret)
            n.ret->accept(*this);

        // check body
        if (n.body)
            n.body->accept(*this); // side effect: param_count may change, so we save it to params_count

        // we should clear bounded variables
        if (n.params)
            bound_vars.erase(bound_vars.begin() + bound_vars.size() - params_count, bound_vars.end());
    }

    void Sema::visit(ASTGrantPriv &n)
    {
        unsigned int i = 0;
        bool found = false;

        while (priveleges[i])
            if (!strcmp(priveleges[i++], n.priv->c_str()))
            {
                found = true;
                break;
            }

        if (!found)
            drv->error(n.loc, SE3069, n.priv->c_str());
    }

    void Sema::visit(ASTGrantRole &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTIf &n)
    {
        n.i_expr->accept(*this);
        n.t_expr->accept(*this);
        n.e_expr->accept(*this);
    }

    void Sema::visit(ASTInstOf &n)
    {
        n.expr->accept(*this);
        n.type->accept(*this);
    }

    void Sema::visit(ASTItemTest &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTLet &n)
    {
        n.expr->accept(*this);
        n.fd->accept(*this);
    }

    void Sema::visit(ASTLibModule &n)
    {
        is_imported = n.is_internal;

        n.moduleDecl->accept(*this);
        n.prolog->accept(*this);
    }

    void Sema::visit(ASTLit &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTLoadFile &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTLoadModule &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTMainModule &n)
    {
        if (drv == NULL)
            throw SYSTEM_EXCEPTION("Driver is not set for semantic analyzer!");

        n.prolog->accept(*this);
        n.query->accept(*this);
    }

    void Sema::visit(ASTMetaCols &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTMetaDocs &n)
    {
        if (n.coll)
            n.coll->accept(*this);
    }

    void Sema::visit(ASTMetaSchemaCol &n)
    {
        n.coll->accept(*this);
    }

    void Sema::visit(ASTMetaSchemaDoc &n)
    {
        n.doc->accept(*this);

        if (n.coll)
            n.coll->accept(*this);
    }

    void Sema::visit(ASTModImport &n)
    {
        std::string err;

        if (n.name && (*n.name == "xml" || *n.name == "xmlns"))
        {
            err = std::string("'") + *n.name + "' cannot be used as a namespace in import module";
            drv->error(n.loc, XQST0070, err.c_str());
            return;
        }

        if (*n.uri == "http://www.w3.org/XML/1998/namespace")
        {
            drv->error(n.loc, XQST0070, "'http://www.w3.org/XML/1998/namespace' cannot be used as an import module URI");
            return;
        }

        if (*n.uri == "")
        {
            drv->error(n.loc, XQST0088, "import module URI cannot be empty");
            return;
        }

        // check for duplicate import
        if (mod->imported.find(*n.uri) != mod->imported.end())
        {
            ASTLocation loc = mod->imported.find(*n.uri)->second->loc;

            drv->error(n.loc, XQST0047, *n.uri + ", the first definition is at (" +
                    int2string(loc.begin.line) + ":" + int2string(loc.begin.column) + ")");
            return;
        }

        // bind the prefix if it's specified
        if (n.name)
        {
            nsBindType::const_iterator it;

            it = mod->nsBinds.find(*n.name);

            if (it != mod->nsBinds.end() && it->second.second != NULL)
            {
                err = std::string("'") + *n.name + "' is already defined at (" + int2string(it->second.second->begin.line) +
                        ":" + int2string(it->second.second->begin.column) + ")";

                drv->error(n.loc, XQST0033, err.c_str());
                return;
            }
            else
                mod->nsBinds[*n.name] = nsPair(*n.uri, &n.loc);
        }

        mod->imported[*n.uri] = &n;

        // ignore intra-module imports (imports with the same uri) since they are needed only for resolving
        if (mod->module_uri && *n.uri == *mod->module_uri)
            return;

        // we don't follow external imports during 'load module' phase
        if (!is_imported && mod->module_uri && *mod->module_uri != *n.uri)
            return;

        // check if we've already got this one
        if (drv->libModules.find(*n.uri) != drv->libModules.end())
            return;

        if (drv->getLibraryModule(n.uri->c_str()))
        {
            drv->error(n.loc, XQST0059, *n.uri);
        }
    }

    void Sema::visit(ASTModuleDecl &n)
    {
        if (n.uri->size() == 0)
        {
            drv->error(n.loc, XQST0088, "module declaration URI cannot be empty");
            return;
        }

        mod->module_uri = new std::string(*n.uri);

        if (*n.name == "xml" || *n.name == "xmlns")
        {
            drv->error(n.loc, XQST0070, std::string("module declaration URI cannot be '") + *n.name + "'");
            return;
        }

        mod->nsBinds[*n.name] = nsPair(*n.uri, &n.loc);
    }

    void Sema::visit(ASTNameTest &n)
    {
        const char *uri;

        if (*n.pref == "*" && *n.local == "*")
        {
            return;
        }
        else if (*n.pref == "*")
        {
            n.uri = new std::string("*");
            return;
        }

        uri = resolveQName(n.loc, n.pref->c_str(), (att_test) ? "" : NULL);

        if (uri)
            n.uri = new std::string(uri);
    }

    void Sema::visit(ASTNamespaceDecl &n)
    {
        nsBindType::const_iterator it;
        std::string err;

        if (*n.name == "xml")
        {
            drv->error(n.loc, XQST0070, "'xml' cannot be used as a prefix");
            return;
        }
        else if (*n.name == "xmlns")
        {
            drv->error(n.loc, XQST0070, "'xmlns' cannot be used as a prefix");
            return;
        }
        else if (*n.uri == "http://www.w3.org/XML/1998/namespace")
        {
            drv->error(n.loc, XQST0070, "'http://www.w3.org/XML/1998/namespace' cannot be used as a namespace URI");
            return;
        }

        it = mod->nsBinds.find(*n.name);

        if (it != mod->nsBinds.end() && it->second.second != NULL)
        {
            err = std::string("'") + *n.name + "' is already defined at (" + int2string(it->second.second->begin.line) +
                    ":" + int2string(it->second.second->begin.column) + ")";

            drv->error(n.loc, XQST0033, err.c_str());
            return;
        }
        else
            mod->nsBinds[*n.name] = nsPair(*n.uri, &n.loc);
    }

    void Sema::visit(ASTNodeTest &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTNsp &n)
    {
        if (*n.name == "xmlns")
        {
            drv->error(n.loc, XQST0070, std::string("'") + *n.name + "' cannot be used as a namespace prefix");
            return;
        }

        if (*n.name == "xml" && n.cont && *n.cont != "http://www.w3.org/XML/1998/namespace")
        {
            drv->error(n.loc, XQST0070, "prefix 'xml' cannot be bind to namespace other than '\"http://www.w3.org/XML/1998/namespace\"'");
            return;
        }

        if (*n.name != "xml" && n.cont && *n.cont == "http://www.w3.org/XML/1998/namespace")
        {
            drv->error(n.loc, XQST0070, "'http://www.w3.org/XML/1998/namespace' cannot be used as a namespace URI");
            return;
        }

        if (*n.name != "" && !n.cont)
        {
            drv->error(n.loc, XQST0085, std::string("namespace URI for '") + *n.name + "' is a zero-length string");
            return;
        }

        if (*n.name == "")
        {
            ASTLocation *loc = elemNsps.back().second.second;

            if (loc != NULL)
            {
                drv->error(n.loc, XQST0071,
                           std::string("default namespace has been declared at (") + int2string(loc->begin.line) + ":" + int2string(loc->begin.column) + ")");
                return;
            }
            else
            {
                elemNsps.back().second = nsPair((n.cont) ? *n.cont : "", &n.loc);
            }
        }
        else
        {
            if (elemNsps.back().first.find(*n.name) == elemNsps.back().first.end())
            {
                elemNsps.back().first[*n.name] = nsPair(*n.cont, &n.loc);
            }
            else
            {
                ASTLocation *loc = elemNsps.back().first[*n.name].second;

                drv->error(n.loc, XQST0071, std::string("namespace '") + *n.name + "' has been declared at (" +
                        int2string(loc->begin.line) + ":" + int2string(loc->begin.column) + ")");
                return;
            }
        }
    }

    void Sema::visit(ASTOption &n)
    {
        const char *uri;
        std::string key, val, err;

        if (*n.pref == "")
        {
            drv->error(n.loc, XPST0081, "QName must be prefixed in option declaration");
            return;
        }

        uri = resolveQName(n.loc, n.pref->c_str(), "");

        if (uri == NULL)
            return;

        n.uri = new std::string(uri); // add info about resolved uri into AST
        n.options = new std::vector<ASTOption::option>;

        // ignore all options except 'se:output' and 'se:character-map'
        if (*n.uri == "http://www.modis.ispras.ru/sedna" && *n.local == "output")
        {
            // parse options string; if error it will be signaled by the function
            parseOption(n.loc, *n.opt, *n.options, ';');

            for (unsigned int i = 0; i < n.options->size(); i++)
            {
                key = (*n.options)[i].first;
                val = (*n.options)[i].second;

                if (key == "indent")
                {
                    if (val != "yes" && val != "no")
                        drv->error(n.loc, SE5072, val.c_str());
                }
                else if (key == "method")
                {
                    if (val != "xml" && val != "html")
                        drv->error(n.loc, SE5071, val.c_str());
                }
                else
                    drv->error(n.loc, SE5068, key.c_str());
            }
        }
        else if (*n.uri == "http://www.modis.ispras.ru/sedna" && *n.local == "character-map")
        {
            // parse options string; if error it will be signaled by the function
            parseOption(n.loc, *n.opt, *n.options, '!');

            for (unsigned int i = 0; i < n.options->size(); i++)
            {
                key = (*n.options)[i].first;
                val = (*n.options)[i].second;
            }
        }
    }

    void Sema::visit(ASTOrdExpr &n)
    {
        n.expr->accept(*this);
    }

    void Sema::visit(ASTOrder &n)
    {
        std::string loc;

        if (dupLocations[PrologOrder])
        {
            loc = DIAG_DUP_PROLOG(dupLocations[PrologOrder]->begin.line, dupLocations[PrologOrder]->begin.column,
                                n.loc.begin.line, n.loc.begin.column);

            drv->error(n.loc, XQST0065, loc.c_str());
        }
        else
        {
            dupLocations[PrologOrder] = &n.loc;
        }
    }

    void Sema::visit(ASTOrderBy &n)
    {
        VisitNodesVector(n.specs, *this);
    }

    void Sema::visit(ASTOrderByRet &n)
    {
        n.iter_expr->accept(*this);
        n.ret_expr->accept(*this);
    }

    void Sema::visit(ASTOrderEmpty &n)
    {
        std::string loc;

        if (dupLocations[PrologOrderEmpty])
        {
            loc = DIAG_DUP_PROLOG(dupLocations[PrologOrderEmpty]->begin.line, dupLocations[PrologOrderEmpty]->begin.column,
                                n.loc.begin.line, n.loc.begin.column);

            drv->error(n.loc, XQST0069, loc.c_str());
        }
        else
        {
            dupLocations[PrologOrderEmpty] = &n.loc;
        }
    }

    void Sema::visit(ASTOrderMod &n)
    {
        if (n.ad_mod)
            n.ad_mod->accept(*this);

        if (n.em_mod)
            n.em_mod->accept(*this);

        if (n.col_mod)
            n.col_mod->accept(*this);
    }

    void Sema::visit(ASTOrderModInt &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTOrderSpec &n)
    {
        n.expr->accept(*this);

        if (n.mod)
            n.mod->accept(*this);
    }

    void Sema::visit(ASTPIConst &n)
    {
        if (n.ncname && n.ncname->size() == 3 && toupper((*n.ncname)[0]) == 'X' && toupper((*n.ncname)[1]) == 'M' && toupper((*n.ncname)[2]) == 'L')
        {
            drv->error(n.loc, XQDY0064, std::string("reserved processing instruction name: '") + *n.ncname + "'");
        }
        else if (!n.ncname)
        {
            n.name->accept(*this);
        }

        if (n.expr)
            n.expr->accept(*this);
    }

    void Sema::visit(ASTPi &n)
    {
        if (n.name->size() == 3 && toupper((*n.name)[0]) == 'X' && toupper((*n.name)[1]) == 'M' && toupper((*n.name)[2]) == 'L')
        {
            drv->error(n.loc, XPST0003, std::string("reserved processing instruction name: '") + *n.name + "'");
        }
    }

    void Sema::visit(ASTPiTest &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTPosVar &n)
    {
        n.var->accept(*this);
    }

    void Sema::visit(ASTPragma &n)
    {
        if (*n.pref == "")
        {
            drv->error(n.loc, XPST0081, "pragma QName must be prefixed");
            return;
        }

        resolveQName(n.loc, n.pref->c_str(), "");
    }

    void Sema::visit(ASTPred &n)
    {
        n.iter_expr->accept(*this);
        n.pred_expr->accept(*this);
    }

    void Sema::visit(ASTProlog &n)
    {
        unsigned int i = 0;
        ASTOption *opt;

        while (i < n.decls->size())
        {
            (*n.decls)[i]->accept(*this);

            // delete (as "ignore") all options except 'se:output' and 'se:character-map'
            if ((opt = dynamic_cast<ASTOption *>((*n.decls)[i])) && opt->uri &&
                 (*opt->uri != "http://www.modis.ispras.ru/sedna" || (*opt->local != "output" && *opt->local != "character-map")))
            {
                delete *(n.decls->begin() + i);
                n.decls->erase(n.decls->begin() + i);
                continue;
            }

            i++;
        }
    }

    void Sema::visit(ASTQName &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTQuantExpr &n)
    {
        n.expr->accept(*this);
        n.fd->accept(*this);
    }

    void Sema::visit(ASTQuery &n)
    {
        n.query->accept(*this);
    }

    void Sema::visit(ASTRenameColl &n)
    {
        n.name_old->accept(*this);
        n.name_new->accept(*this);
    }

    void Sema::visit(ASTRet &n)
    {
        n.iter_expr->accept(*this);
        n.ret_expr->accept(*this);
    }

    void Sema::visit(ASTRevokePriv &n)
    {
        unsigned int i = 0;
        bool found = false;

        while (priveleges[i])
            if (!strcmp(priveleges[i++], n.priv->c_str()))
        {
            found = true;
            break;
        }

        if (!found)
            drv->error(n.loc, SE3069, n.priv->c_str());
    }

    void Sema::visit(ASTRevokeRole &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTSchemaAttrTest &n)
    {
        att_test = true;
        n.name->accept(*this);
        att_test = false;

        drv->error(n.loc, XPST0008, "there is no schema to test");
    }

    void Sema::visit(ASTSchemaElemTest &n)
    {
        n.name->accept(*this);

        drv->error(n.loc, XPST0008, "there is no schema to test");
    }

    void Sema::visit(ASTSeq &n)
    {
        for (unsigned int i = 0; i < n.exprs->size(); i++)
            (*n.exprs)[i]->accept(*this);
    }

    void Sema::visit(ASTSpaceSeq &n)
    {
        n.expr->accept(*this);
    }

    void Sema::visit(ASTTextConst &n)
    {
        n.expr->accept(*this);
    }

    void Sema::visit(ASTTextTest &n)
    {
        // nothing to do
    }

    void Sema::visit(ASTTreat &n)
    {
        n.expr->accept(*this);
        n.type->accept(*this);
    }

    void Sema::visit(ASTType &n)
    {
        const char *uri;
        std::string *pref, *loc, err;
        std::map<std::string, ASTType::TypeMod>::const_iterator it;

        ASTParseQName(n.name, &pref, &loc);

        uri = resolveQName(n.loc, pref->c_str(), NULL);

        if (uri == NULL)
        {
            delete pref;
            delete loc;

            return;
        }

        if (strcmp(uri, "http://www.w3.org/2001/XMLSchema"))
        {
            if (n.type == ASTType::ATOMIC)
            {
                err = std::string("unknown atomic type: ") + *n.name;
                drv->error(n.loc, XPST0051, err.c_str());
            }
            else
            {
                err = std::string("unknown type: ") + *n.name;
                drv->error(n.loc, XPST0008, err.c_str());
            }

            delete pref;
            delete loc;

            return;
        }

        it = drv->xsTypes.find(*loc);

        if (it == drv->xsTypes.end() || (it->second != n.type && n.type != ASTType::ANY))
        {
            if (n.type == ASTType::ATOMIC)
            {
                err = std::string("unknown atomic type: ") + *n.name;
                drv->error(n.loc, XPST0051, err.c_str());
            }
            else
            {
                err = std::string("unknown type: ") + *n.name;
                drv->error(n.loc, XPST0008, err.c_str());
            }

            delete pref;
            delete loc;

            return;
        }

        if (casting_mode && !strcmp(uri, "http://www.w3.org/2001/XMLSchema") && (*loc == "anyAtomicType" || *loc == "NOTATION"))
        {
            drv->error(n.loc, XPST0080, std::string("cannot use 'xs:") + *loc + "' in casting");
        }

        delete pref;
        delete loc;
    }

    void Sema::visit(ASTTypeSeq &n)
    {
        // we just propagate analysis to the actual test
        n.type_test->accept(*this);
    }

    void Sema::visit(ASTTypeSingle &n)
    {
        n.type->accept(*this);
    }

    void Sema::visit(ASTTypeSwitch &n)
    {
        n.expr->accept(*this);
        VisitNodesVector(n.cases, *this);
        n.def_case->accept(*this);
    }

    void Sema::visit(ASTTypeVar &n)
    {
        n.type->accept(*this);
        n.var->accept(*this);
    }

    void Sema::visit(ASTUnio &n)
    {
        // we will run vars ib param mode to make duplicate removal a bit easier
        // duplicates could exist because of for-variables rebindings
        // we cannot get 'unbound variable' errors here since they cannot contradict to ancestor fun-defs
        unsigned int params = 0;

        param_mode = true;
        param_count = 0;
        VisitNodesVector(n.vars, *this);
        params = param_count;
        param_mode = false;

        U_ASSERT(params == n.vars->size());

        // get rid of duplicates respecting later bindings
        if (params > 1) // no error occured
        {
            unsigned int offs = 0, i, j;

            for (i = bound_vars.size() - params; i < bound_vars.size(); i++)
            {
                for (j = i + 1; j < bound_vars.size(); j++)
                    if (bound_vars[j].first == bound_vars[i].first)
                {
                    delete (*n.vars)[i - bound_vars.size() + params - offs];
                    n.vars->erase(n.vars->begin() + i - bound_vars.size() + params - offs);
                    offs++;
                    break;
                }
            }
        }

        bound_vars.erase(bound_vars.begin() + bound_vars.size() - params, bound_vars.end());
    }

    void Sema::visit(ASTUop &n)
    {
        n.expr->accept(*this);
    }

    void Sema::visit(ASTUpdDel &n)
    {
        n.what->accept(*this);
    }

    void Sema::visit(ASTUpdInsert &n)
    {
        n.what->accept(*this);
        n.where->accept(*this);
    }

    void Sema::visit(ASTUpdMove &n)
    {
        n.what->accept(*this);
        n.where->accept(*this);
    }

    void Sema::visit(ASTUpdRename &n)
    {
        n.what->accept(*this);
    }

    void Sema::visit(ASTUpdReplace &n)
    {
        n.what->accept(*this);
        n.new_expr->accept(*this);
    }

    void Sema::visit(ASTVar &n)
    {
        const char *uri;
        std::string name;

        // resolve prefix
        if (n.uri == NULL)
        {
            uri = resolveQName(n.loc, n.pref->c_str(), "");

            if (uri == NULL) return;

            n.uri = new std::string(uri);
        }

        // it's just a param
        if (param_mode)
        {
            param_count++;
            bound_vars.push_back(XQVariable(CREATE_INTNAME(*n.uri, *n.local), &n));
            return;
        }

        // if it's a usual reference then find who we reference
        name = CREATE_INTNAME(*n.uri, *n.local);

        // first, check if variable is bound
        if (bound_vars.size() > 0)
        {
            for (int i = bound_vars.size() - 1; i >= 0; i--)
            {
                if (bound_vars[i].first == name)
                    return;
            }
        }

        // if we have unbound context at the top level; then just give an error
        if (*n.local == "$%v")
        {
            if (!mod->xpdy0002)
                mod->xpdy0002 = &n.loc;
            return;
        }

        // then, check prolog
        if (mod->vars.find(name) != mod->vars.end())
        {
            return;
        }

        // ok, don't worry; try to find it in library modules
        if (mod->imported.find(*n.uri) != mod->imported.end())
        {
            if (drv->libVars.find(name) != drv->libVars.end())
            {
                return;
            }
            else if (mod->module_uri) // if library module, maybe try to resolve later (intra(inter)-module imports)
            {
                if (!is_imported && *mod->module_uri != *n.uri) // ignore external imports during load module
                    return;

                mod->unres_vars[name] = &n; // try to resolve var later
                return;
            }
        }

        // ww've looked everywhere we could
        drv->error(n.loc, XPST0008, name);
    }

    void Sema::visit(ASTVarDecl &n)
    {
        ASTVar *var;
        const char *uri;
        std::string err, name;

        // first, resolve the prefix
        var = dynamic_cast<ASTVar *>(n.var);

        U_ASSERT(var != NULL);

        if (var->uri == NULL)
        {
            uri = resolveQName(n.loc, var->pref->c_str(), "");

            if (uri == NULL) return;

            var->uri = new std::string(uri);
        }

        // then analyze the type
        if (!is_imported && n.type)
            n.type->accept(*this);

        // for library module main URI and var-uri must be equal
        if (!is_imported && mod->module_uri && *mod->module_uri != *var->uri)
        {
            err = std::string("variable '") + *var->pref + ":" + *var->local + "' is not in the library module namespace " + *mod->module_uri;

            drv->error(n.loc, XQST0048, err.c_str());

            return;
        }

        name = CREATE_INTNAME(*var->uri, *var->local);

        if (!is_imported && mod->vars.find(name) != mod->vars.end())
        {
            drv->error(var->loc, XQST0049,
                    std::string("variable '") + *var->uri + ((var->uri->size() == 0) ? "" : ":") + *var->local + "' has already been declared");

            return;
        }

        if (!is_imported && drv->libVars.find(name) != drv->libVars.end())
        {
            drv->error(var->loc, XQST0049,
                    std::string("variable '") + *var->uri + ((var->uri->size() == 0) ? "" : ":") + *var->local + "' has already been declared in another module");

            return;
        }

        // then analyze the body, note that will be poosibly unresolved functions, but thats ok since we do additional
        // checking afterwards
        // external variables ar not supported for now
        if (n.expr == NULL)
            drv->error(n.loc, SE5012, "external variables are not supported in Sedna");
        else
            n.expr->accept(*this);

        // then add variables as known
        mod->vars[name] = &n;

        if (mod->module_uri)
            drv->libVars[name] = &n;
    }

    void Sema::visit(ASTVersionDecl &n)
    {
        if (*n.xq_version != "1.0")
        {
            drv->error(n.loc, XQST0031, n.xq_version->c_str());
        }

        if (n.encoding && !checkXQueryEncoding(n.encoding->c_str()))
        {
            drv->error(n.loc, XQST0087, std::string("'") + *n.encoding + "'");
        }
    }

    void Sema::visit(ASTXMLComm &n)
    {
        // nothing to do
    }

    // Some additional function

    // Checks query encoding specified via xquery...; declaration
    // As XML1.0 specifies: EncName     ::=      [A-Za-z] ([A-Za-z0-9._] | '-')* /* Encoding name contains only Latin characters */
    bool Sema::checkXQueryEncoding(const char *enc)
    {
#define IS_ALPHA(c) (((c) >= 'a' && (c) <= 'z') || ((c) >= 'A' && (c) <= 'Z'))
        if (!IS_ALPHA(enc[0]))
            return false;

        for (unsigned int i = 1; i < strlen(enc); i++)
        {
            if (!IS_ALPHA(enc[i]) && !isdigit(enc[i]) && enc[i] != '-' && enc[i] != '.' && enc[i] != '_')
                return false;
        }

        return true;
#undef IS_ALPHA
    }

    // resolves qname prefix
    // Parameters:
    //      pref -- prefix to check
    //      def_uri -- if prefix "" then use it; if it is NULL default element namespace is searched
    // Returns:
    //      uri if found, NULL if not (or prefix is "")
    const char *Sema::resolveQName(ASTLocation &loc, const char *pref, const char *def_uri, int err_code)
    {
        nsBindType::const_iterator it;

        if (pref == NULL)
            return NULL;

        if (!strlen(pref))
        {
            if (def_uri)
            {
                return def_uri;
            }
            else if (elemNsps.size() > 0)
            {
                return elemNsps.back().second.first.c_str();
            }
            else
            {
                return mod->defElemNsp.first.c_str();
            }
        }

        // if we've got prefix we should resolve it using first in-scope namespaces
        for (int i = elemNsps.size() - 1; i >= 0; i--)
            if (elemNsps[i].first.find(pref) != elemNsps[i].first.end())
                return elemNsps[i].first[pref].first.c_str();

        // and then default ones
        it = mod->nsBinds.find(pref); // find namespace

        if (it == mod->nsBinds.end() || it->second.first == "")
        {
            if (err_code >= 0)
            {
                std::string err;

                err = std::string("prefix '") + pref + "' has not been defined";

                drv->error(loc, err_code, err.c_str());
            }

            return NULL;
        }
        else
            return it->second.first.c_str();
    }

    static bool parseKeyValue(const std::string &opt, const char delim, std::string &key, std::string &val)
    {
        unsigned int pos, beg, end;

        pos = opt.find(delim); // find delimeter

        if (pos == std::string::npos) return false;

        // trim WSes for key
        beg = 0;
        while (beg < pos && isspace(opt[beg])) beg++;

        end = pos - 1;
        while (end >= 0 && isspace(opt[end])) end--;

        key = std::string(opt, beg, end - beg + 1);

        // trim WSes for value
        beg = pos + 1;
        while (beg < opt.size() && isspace(opt[beg])) beg++;

        end = opt.size() - 1;
        while (end > pos && isspace(opt[end])) end--;

        val = std::string(opt, beg, end - beg + 1);

        return true;
    }

    void Sema::parseOption(const ASTLocation &loc, const std::string &opt, std::vector<std::pair<std::string, std::string> > &opts, const char delim)
    {
        std::string key, val;
        unsigned int beg = 0, pos;

        while((pos = opt.find(delim, beg)) != std::string::npos)
        {
            if (parseKeyValue(std::string(opt, beg, pos - beg), '=', key, val))
            {
                opts.push_back(std::pair<std::string, std::string>(key, val));
            }
            else if (pos == beg)
            {
                drv->error(loc, SE5067, "empty key=value pair");
            }
            else
            {
                drv->error(loc, SE5067, std::string(opt, beg, pos - beg).c_str());
            }

            beg = pos + 1;
        }

        // if we've got single option without delimeter parse it as well
        if (beg < opt.size())
        {
            if (parseKeyValue(std::string(opt, beg, opt.size() - beg), '=', key, val))
            {
                opts.push_back(std::pair<std::string, std::string>(key, val));
            }
            else
            {
                drv->error(loc, SE5067, std::string(opt, beg, opt.size() - beg).c_str());
            }
        }
    }

    XQFunction *Sema::findFunction(std::string name, unsigned int arity, XQueryModule *mod, XQueryDriver *drv)
    {
        XQFunction *xqf;
        std::string uri, name_wa;
        unsigned int fp, lp;

        // construct name with arity
        name_wa = name + "/" + int2string(arity);

        // check prolog functions
        if (mod->funcs.find(name_wa) != mod->funcs.end())
        {
            return &(mod->funcs[name_wa]);
        }

        // take uri from name
        fp = name.find('{');
        lp = name.find('}');

        if (fp == std::string::npos) // this could happen if we continue analysis after XQST0060 error
            return NULL;

        U_ASSERT(fp != std::string::npos && lp != std::string::npos);
        uri = name.substr(fp + 1, lp - fp - 1);

        // check library functions; but only if we're importing corresponding uri
        if (mod->imported.find(uri) != mod->imported.end() && drv->libFuncs.find(name_wa) != drv->libFuncs.end())
        {
            return &(drv->libFuncs[name_wa]);
        }

        // check standard XQuery functions
        if (drv->stdFuncs.find(name) != drv->stdFuncs.end())
        {
            xqf = &(drv->stdFuncs[name]);

            if (arity >= xqf->min_arg && arity <= xqf->max_arg)
                return xqf;
        }

        return NULL;
    }

    void Sema::rewriteStdFunCall(ASTFunCall &n, std::string name)
    {
        ASTNode *ddo;

        if (name == "!fn!index-scan")
        {
            ASTLit *l;

            l = dynamic_cast<ASTLit *>((*n.params)[2]);

            if (!l || l->type != ASTLit::STRING || (*l->lit != "GT" && *l->lit != "LT" && *l->lit != "GE" && *l->lit != "LE" && *l->lit != "EQ"))
                drv->error(n.loc, SE5050, "index scan condition must be predefined string constant (GT, LT, GE, LE or EQ)");

            ddo = new ASTDDO(n.loc, &n);
            modifyParent(ddo, false, false);
        }
        else if (name == "!fn!index-scan-between")
        {
            ASTLit *l;

            l = dynamic_cast<ASTLit *>((*n.params)[3]);

            if (!l || l->type != ASTLit::STRING || (*l->lit != "INT" && *l->lit != "SEG" && *l->lit != "HINTL" && *l->lit != "HINTR"))
                drv->error(n.loc, SE5051, "index scan condition must be predefined string constant (INT, SEG, HINTL or HINTR)");

            ddo = new ASTDDO(n.loc, &n);
            modifyParent(ddo, false, false);
        }
        else if (name == "!fn!ftindex-scan" || name == "!fn!ftindex-scan2")
        {
            ddo = new ASTDDO(n.loc, &n);
            modifyParent(ddo, false, false);
        }
        else if (name == "!fn!name" || name == "!fn!namespace-uri" || name == "!fn!string-length" || name == "!fn!normalize-space" ||
                 name == "!fn!string" || name == "!fn!local-name" || name == "!fn!number" || name == "!fn!base-uri" || name == "!fn!root")
        {
            if (!n.params)
            {
                n.params = new ASTNodesVector();

                if (name == "!fn!normalize-space")
                {
                    ASTNodesVector *params = new ASTNodesVector();

                    params->push_back(new ASTVar(n.loc, new std::string("$%v")));
                    n.params->push_back(new ASTFunCall(n.loc, new std::string("fn:string"), params));
                }
                else
                {
                    n.params->push_back(new ASTVar(n.loc, new std::string("$%v")));
                }

                n.params->back()->accept(*this);
            }
        }
        else if (name == "!fn!position" || name == "!fn!last")
        {
            n.params = new ASTNodesVector();
            n.params->push_back(new ASTVar(n.loc, new std::string("$%v")));

            n.params->back()->accept(*this);
        }
        else if (name == "!fn!contains")
        {
            if (n.params->size() == 3)
            {
                ASTLit *l;

                l = dynamic_cast<ASTLit *>((*n.params)[2]);

                if (l && l->type == ASTLit::STRING && *l->lit == "http://www.w3.org/2005/xpath-functions/collation/codepoint")
                {
                    delete l;
                    n.params->pop_back();
                }
                else
                {
                    drv->error(n.loc, FOCH0002, (l && l->type == ASTLit::STRING) ? *l->lit + " in fn:contains" : "fn:contains");
                }
            }
        }
        else if (name == "!fn!lang")
        {
            ASTNode *param, *alt;

            if (n.params->size() == 1)
                param = new ASTVar(n.loc, new std::string("$%v"));
            else
                param = (*n.params)[1];

            /* rewrite to:
                let $testlang as xs:string? := fn:lowercase(fn:string($arg0)),
                    $node as node()  := $arg1,
                    $attr as node()? := $node/ancestor-or-self::*[@xml:lang][1]/@xml:lang

                return
                    (fn:not(fn:empty($attr)) and
                    (let $lang_value as xs:string := fn:lower-case(fn:string($attr))
                        return
                            (($lang_value eq $testlang) or fn:starts-with($lang_value, fn:concat($testlang, '-')))
                    ))
            */
            alt = new ASTLet(n.loc, NULL, new ASTFunCall(n.loc, new std::string("fn:lower-case"), new ASTFunCall(n.loc, new std::string("fn:string"), (*n.params)[0])),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTTypeSingle(n.loc, new ASTType(n.loc, new std::string("xs:string"), ASTType::ATOMIC), ASTTypeSingle::OPT),
                  new ASTVar(n.loc, new std::string("testlang"))), new ASTLet(n.loc, NULL, param, new ASTFunDef(n.loc, new ASTTypeVar(n.loc,
                  new ASTTypeSeq(n.loc, new ASTNodeTest(n.loc), ASTTypeSeq::ONE), new ASTVar(n.loc, new std::string("node"))),
                  new ASTLet(n.loc, NULL, new ASTDDO(n.loc, new ASTAxis(n.loc, ASTAxisStep::ATTRIBUTE, new ASTDDO(n.loc, new ASTRet(n.loc, new ASTVar(n.loc, new std::string("node")),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTType(n.loc, new std::string("xs:anyType"), ASTType::COMPLEX), new ASTVar(n.loc, new std::string("$%v"))),
                  new ASTPred(n.loc, new ASTPred(n.loc, new ASTAxis(n.loc, ASTAxisStep::ANCESTOR_OR_SELF, new ASTVar(n.loc, new std::string("$%v")),
                  new ASTElementTest(n.loc, new ASTNameTest(n.loc, new std::string("*:*")))),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTType(n.loc, new std::string("xs:anyType"), ASTType::COMPLEX), new ASTVar(n.loc, new std::string("$%v"))),
                  new ASTDDO(n.loc, new ASTAxis(n.loc, ASTAxisStep::ATTRIBUTE, new ASTVar(n.loc, new std::string("$%v")),
                  new ASTAttribTest(n.loc, new ASTNameTest(n.loc, new std::string("xml:lang"))))))),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTType(n.loc, new std::string("xs:anyType"), ASTType::COMPLEX), new ASTVar(n.loc, new std::string("$%v"))),
                  new ASTLit(n.loc, ASTLit::INTEGER, new std::string("1"))))))), new ASTAttribTest(n.loc, new ASTNameTest(n.loc, new std::string("xml:lang"))))),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTTypeSeq(n.loc, new ASTNodeTest(n.loc), ASTTypeSeq::OPT), new ASTVar(n.loc, new std::string("attr"))),
                  new ASTBop(n.loc, ASTBop::AND,
                  new ASTFunCall(n.loc, new std::string("fn:not"), new ASTFunCall(n.loc, new std::string("fn:empty"), new ASTVar(n.loc, new std::string("attr")))),
                  new ASTLet(n.loc, NULL, new ASTFunCall(n.loc, new std::string("fn:lower-case"),
                  new ASTFunCall(n.loc, new std::string("fn:string"), new ASTVar(n.loc, new std::string("attr")))),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTTypeSingle(n.loc, new ASTType(n.loc, new std::string("xs:string"), ASTType::ATOMIC), ASTTypeSingle::ONE),
                  new ASTVar(n.loc, new std::string("lang_value"))), new ASTBop(n.loc, ASTBop::OR,
                  new ASTBop(n.loc, ASTBop::EQ_V, new ASTVar(n.loc, new std::string("lang_value")), new ASTVar(n.loc, new std::string("testlang"))),
                  new ASTFunCall(n.loc, new std::string("fn:starts-with"), new ASTVar(n.loc, new std::string("lang_value")),
                  new ASTFunCall(n.loc, new std::string("fn:concat"), new ASTVar(n.loc, new std::string("testlang")),
                  new ASTLit(n.loc,  ASTLit::STRING, new std::string("-"))))))))))))));

            n.params->clear();
            delete n.params;
            n.params = NULL;

            modifyParent(alt, true, true);
        }
        else if (name == "!fn!id")
        {
            ASTNode *param, *alt;

            if (n.params->size() == 1)
                param = new ASTVar(n.loc, new std::string("$%v"));
            else
                param = (*n.params)[1];

            /* rewrite to:
                    let $arg as xs:string*    := $arg0,
                        $node as node()       := $arg1,
                        $idrefs as xs:string* := for $s in $arg
                                                 return fn:tokenize(fn:normalize-space($s), ' ')
                                                 [. castable as xs:IDREF]

                        return
                            $node/ancestor-or-self::node()[last()]/
                                      descendant-or-self::*[@*
                                                           [self::xml:id or fn:ends-with(fn:lower-case(fn:local-name(.)), 'id')]
                                                           [some $s as xs:string in $idrefs satisfies $s eq fn:string()]]
            */

            alt = new ASTLet(n.loc, NULL, (*n.params)[0], new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTTypeSeq(n.loc,
                  new ASTType(n.loc, new std::string("xs:string"), ASTType::ATOMIC), ASTTypeSeq::ZERO_OR_MORE), new ASTVar(n.loc, new std::string("arg"))),
                  new ASTLet(n.loc, NULL, param, new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTTypeSeq(n.loc, new ASTNodeTest(n.loc), ASTTypeSeq::ONE),
                  new ASTVar(n.loc, new std::string("node"))), new ASTLet(n.loc, NULL, new ASTRet(n.loc, new ASTVar(n.loc, new std::string("arg")),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTType(n.loc, new std::string("xs:anyType"), ASTType::COMPLEX), new ASTVar(n.loc, new std::string("s"))),
                  new ASTPred(n.loc, new ASTFunCall(n.loc, new std::string("fn:tokenize"), new ASTFunCall(n.loc, new std::string("fn:normalize-space"),
                  new ASTVar(n.loc, new std::string("s"))), new ASTLit(n.loc, ASTLit::STRING, new std::string(" "))), 
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTType(n.loc, new std::string("xs:anyType"), ASTType::COMPLEX), new ASTVar(n.loc, new std::string("$%v"))),
                  new ASTCastable(n.loc, new ASTVar(n.loc, new std::string("$%v")),
                  new ASTTypeSingle(n.loc, new ASTType(n.loc, new std::string("xs:IDREF"), ASTType::ATOMIC))))))),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTTypeSeq(n.loc, new ASTType(n.loc, new std::string("xs:string"), ASTType::ATOMIC), ASTTypeSeq::ZERO_OR_MORE),
                  new ASTVar(n.loc, new std::string("idrefs"))), new ASTDDO(n.loc, new ASTRet(n.loc, new ASTDDO(n.loc, new ASTRet(n.loc, new ASTVar(n.loc, new std::string("node")),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTType(n.loc, new std::string("xs:anyType"), ASTType::COMPLEX), new ASTVar(n.loc, new std::string("$%v"))),
                  new ASTPred(n.loc, new ASTAxis(n.loc, ASTAxisStep::ANCESTOR_OR_SELF, new ASTVar(n.loc, new std::string("$%v")), new ASTNodeTest(n.loc)),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTType(n.loc, new std::string("xs:anyType"), ASTType::COMPLEX), new ASTVar(n.loc, new std::string("$%v"))),
                  new ASTFunCall(n.loc, new std::string("fn:last"))))))), 
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTType(n.loc, new std::string("xs:anyType"), ASTType::COMPLEX), new ASTVar(n.loc, new std::string("$%v"))),
                  new ASTPred(n.loc, new ASTAxis(n.loc, ASTAxisStep::DESCENDANT_OR_SELF, new ASTVar(n.loc, new std::string("$%v")),
                  new ASTElementTest(n.loc, new ASTNameTest(n.loc, new std::string("*:*")))),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTType(n.loc, new std::string("xs:anyType"), ASTType::COMPLEX), new ASTVar(n.loc, new std::string("$%v"))),
                  new ASTDDO(n.loc, new ASTRet(n.loc, new ASTVar(n.loc, new std::string("$%v")),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTType(n.loc, new std::string("xs:anyType"), ASTType::COMPLEX), new ASTVar(n.loc, new std::string("$%v"))),
                  new ASTPred(n.loc, new ASTPred(n.loc, new ASTAxis(n.loc, ASTAxisStep::ATTRIBUTE, new ASTVar(n.loc, new std::string("$%v")),
                  new ASTAttribTest(n.loc, new ASTNameTest(n.loc, new std::string("*:*")))),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTType(n.loc, new std::string("xs:anyType"), ASTType::COMPLEX), new ASTVar(n.loc, new std::string("$%v"))),
                  new ASTBop(n.loc, ASTBop::OR, new ASTDDO(n.loc, new ASTAxis(n.loc, ASTAxisStep::SELF, new ASTVar(n.loc, new std::string("$%v")),
                  new ASTAttribTest(n.loc, new ASTNameTest(n.loc, new std::string("xml:id"))))),
                  new ASTFunCall(n.loc, new std::string("fn:ends-with"), new ASTFunCall(n.loc, new std::string("fn:lower-case"),
                  new ASTFunCall(n.loc, new std::string("fn:local-name"), new ASTVar(n.loc, new std::string("$%v")))), new ASTLit(n.loc, ASTLit::STRING, new std::string("id")))))),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTType(n.loc, new std::string("xs:anyType"), ASTType::COMPLEX), new ASTVar(n.loc, new std::string("$%v"))),
                  new ASTQuantExpr(n.loc, new ASTVar(n.loc, new std::string("idrefs")), new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTTypeSingle(n.loc,
                  new ASTType(n.loc, new std::string("xs:string"), ASTType::ATOMIC)), new ASTVar(n.loc, new std::string("s"))), new ASTBop(n.loc, ASTBop::EQ_V,
                  new ASTVar(n.loc, new std::string("s")), new ASTFunCall(n.loc, new std::string("fn:string"), new ASTVar(n.loc, new std::string("$%v"))))),
                  ASTQuantExpr::SOME)))))))))))))))));

            n.params->clear();
            delete n.params;
            n.params = NULL;

            modifyParent(alt, true, true);
        }

        else if (name == "!fn!idref")
        {
            ASTNode *param, *alt;

            if (n.params->size() == 1)
                param = new ASTVar(n.loc, new std::string("$%v"));
            else
                param = (*n.params)[1];

            /* rewrite to:
                let $node as node()       := $arg1,
                    $ids as xs:string*    := $arg0[. castable as xs:NCName]

                return
                    $node/ancestor-or-self::node()[last()]//@*
                        [fn:contains(fn:lower-case(fn:local-name(.)), 'idref')]
                        [let $attr_values as xs:string* := fn:tokenize(fn:normalize-space(fn:string()), ' ')
                            return some $s as xs:string in $ids satisfies some $attr as xs:string in $attr_values satisfies $attr eq $s]
            */

            alt = new ASTLet(n.loc, NULL, param, new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTTypeSeq(n.loc, new ASTNodeTest(n.loc), ASTTypeSeq::ONE),
                  new ASTVar(n.loc, new std::string("node"))), new ASTLet(n.loc, NULL, new ASTPred(n.loc, (*n.params)[0],
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTType(n.loc, new std::string("xs:anyType"), ASTType::COMPLEX), new ASTVar(n.loc, new std::string("$%v"))),
                  new ASTCastable(n.loc, new ASTVar(n.loc, new std::string("$%v")), new ASTTypeSingle(n.loc, new ASTType(n.loc, new std::string("xs:NCName"), ASTType::ATOMIC))))),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTTypeSeq(n.loc, new ASTType(n.loc, new std::string("xs:string"), ASTType::ATOMIC), ASTTypeSeq::ZERO_OR_MORE),
                  new ASTVar(n.loc, new std::string("ids"))), new ASTDDO(n.loc, new ASTRet(n.loc, new ASTDDO(n.loc,
                  new ASTAxis(n.loc, ASTAxisStep::DESCENDANT_OR_SELF, new ASTDDO(n.loc, new ASTRet(n.loc, new ASTVar(n.loc, new std::string("node")),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTType(n.loc, new std::string("xs:anyType"), ASTType::COMPLEX), new ASTVar(n.loc, new std::string("$%v"))),
                  new ASTPred(n.loc, new ASTAxis(n.loc, ASTAxisStep::ANCESTOR_OR_SELF, new ASTVar(n.loc, new std::string("$%v")), new ASTNodeTest(n.loc)),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTType(n.loc, new std::string("xs:anyType"), ASTType::COMPLEX), new ASTVar(n.loc, new std::string("$%v"))),
                  new ASTFunCall(n.loc, new std::string("fn:last"))))))), new ASTNodeTest(n.loc))),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTType(n.loc, new std::string("xs:anyType"), ASTType::COMPLEX), new ASTVar(n.loc, new std::string("$%v"))),
                  new ASTPred(n.loc, new ASTPred(n.loc, new ASTAxis(n.loc, ASTAxisStep::ATTRIBUTE, new ASTVar(n.loc, new std::string("$%v")),
                  new ASTAttribTest(n.loc, new ASTNameTest(n.loc, new std::string("*:*")))),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTType(n.loc, new std::string("xs:anyType"), ASTType::COMPLEX), new ASTVar(n.loc, new std::string("$%v"))),
                  new ASTFunCall(n.loc, new std::string("fn:contains"), new ASTFunCall(n.loc, new std::string("fn:lower-case"),
                  new ASTFunCall(n.loc, new std::string("fn:local-name"), new ASTVar(n.loc, new std::string("$%v")))), new ASTLit(n.loc, ASTLit::STRING, new std::string("idref"))))),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTType(n.loc, new std::string("xs:anyType"), ASTType::COMPLEX), new ASTVar(n.loc, new std::string("$%v"))),
                  new ASTLet(n.loc, NULL, new ASTFunCall(n.loc, new std::string("fn:tokenize"), new ASTFunCall(n.loc, new std::string("fn:normalize-space"),
                  new ASTFunCall(n.loc, new std::string("fn:string"), new ASTVar(n.loc, new std::string("$%v")))), new ASTLit(n.loc, ASTLit::STRING, new std::string(" "))),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTTypeSeq(n.loc, new ASTType(n.loc, new std::string("xs:string"), ASTType::ATOMIC), ASTTypeSeq::ZERO_OR_MORE),
                  new ASTVar(n.loc, new std::string("attr_values"))), new ASTQuantExpr(n.loc, new ASTVar(n.loc, new std::string("ids")), 
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTTypeSingle(n.loc, new ASTType(n.loc, new std::string("xs:string"), ASTType::ATOMIC)),
                  new ASTVar(n.loc, new std::string("s"))), new ASTQuantExpr(n.loc, new ASTVar(n.loc, new std::string("attr_values")),
                  new ASTFunDef(n.loc, new ASTTypeVar(n.loc, new ASTTypeSingle(n.loc, new ASTType(n.loc, new std::string("xs:string"), ASTType::ATOMIC)),
                  new ASTVar(n.loc, new std::string("attr"))), new ASTBop(n.loc, ASTBop::EQ_V, new ASTVar(n.loc, new std::string("attr")),
                  new ASTVar(n.loc, new std::string("s")))), ASTQuantExpr::SOME)), ASTQuantExpr::SOME))))))))))));

            n.params->clear();
            delete n.params;
            n.params = NULL;

            modifyParent(alt, true, true);
        }
    }

    ASTNode *Sema::getDocCollFromAbsXPath(ASTNode *path)
    {
        ASTFunCall *f = dynamic_cast<ASTFunCall *>(path);

        if (f && *f->int_name == "!fn!collection")
            return f;
        else if (f && *f->int_name == "!fn!document")
        {
            if (f->params->size() == 2)
            {
                drv->error(f->loc, SE5049, "document in collection instead of collection is not permitted in on-XPath");
                return NULL;
            }

            return f;
        }
        else if (f)
        {
            drv->error(f->loc, SE5049, std::string("function call ") + *f->pref + ((*f->pref == "") ? "" : ":") +
                    *f->local + " is not permitted in on-XPath");
            return NULL;
        }

        if (ASTDDO *d = dynamic_cast<ASTDDO *>(path))
            return Sema::getDocCollFromAbsXPath(d->expr);

        if (ASTAxis *a = dynamic_cast<ASTAxis *>(path))
        {
            switch (a->axis)
            {
                case ASTAxisStep::CHILD:
                case ASTAxisStep::DESCENDANT:
                case ASTAxisStep::DESCENDANT_OR_SELF:
                case ASTAxisStep::ATTRIBUTE:
                case ASTAxisStep::SELF:
                    return Sema::getDocCollFromAbsXPath(a->expr);
                    break;
                default:
                    drv->error(a->loc, SE5049, std::string("axis ") + axis_str[a->axis] + "is not permitted in on-XPath");
                    return NULL;
            }
        }

        drv->error(path->loc, SE5049, "incorrect on-XPath");

        return NULL;
    }

    ASTNode* Sema::modifyRelIndexXPath(ASTNode *path, ASTNode *doccoll)
    {
        if (ASTDDO *d = dynamic_cast<ASTDDO *>(path))
        {
            d->expr = Sema::modifyRelIndexXPath(d->expr, doccoll);
        }
        else if (ASTAxis *a = dynamic_cast<ASTAxis *>(path))
        {
            switch (a->axis)
            {
                case ASTAxisStep::CHILD:
                case ASTAxisStep::DESCENDANT:
                case ASTAxisStep::DESCENDANT_OR_SELF:
                case ASTAxisStep::ATTRIBUTE:
                case ASTAxisStep::SELF:
                    a->expr = Sema::modifyRelIndexXPath(a->expr, doccoll);
                    break;
                default:
                    drv->error(a->loc, SE5049, std::string("axis ") + axis_str[a->axis] + "is not permitted in by-XPath in create index statement");
                    break;
            }
        }
        else if (ASTVar *v = dynamic_cast<ASTVar *>(path))
        {
            if (*v->local != "$%v")
            {
                drv->error(path->loc, SE5049, "incorrect by-XPath in create index statement: variable references are prohibited");
            }
            else
            {
                delete v;
                return doccoll->dup();
            }
        }
        else
        {
            drv->error(path->loc, SE5049, "incorrect by-XPath in create index statement");
        }

        return path;
    }

    void Sema::getLeafAndTrimmedPath(ASTNode *path, std::string **ln, int *lt, ASTNode **t_path)
    {
        if (ASTDDO *d = dynamic_cast<ASTDDO *>(path))
        {
            Sema::getLeafAndTrimmedPath(d->expr, ln, lt, t_path);
        }
        else if (ASTAxis *a = dynamic_cast<ASTAxis *>(path))
        {
            ASTAttribTest *at;
            ASTElementTest *et;

            switch (a->axis)
            {
                case ASTAxisStep::CHILD:
                case ASTAxisStep::ATTRIBUTE:
                case ASTAxisStep::DESCENDANT:
                case ASTAxisStep::DESCENDANT_OR_SELF:
                case ASTAxisStep::SELF:

                    if (!(*ln))
                    {
                        if ((at = dynamic_cast<ASTAttribTest *>(a->test)))
                        {
                            ASTNameTest *nt = dynamic_cast<ASTNameTest *>(at->name);

                            *ln = new std::string(*nt->local);
                            *lt = 1;
                        }
                        else if ((et = dynamic_cast<ASTElementTest *>(a->test)))
                        {
                            ASTNameTest *nt = dynamic_cast<ASTNameTest *>(et->name);

                            *ln = new std::string(*nt->local);
                            *lt = 0;
                        }
                        else
                        {
                            drv->error(a->loc, SE3207, "trigger path should end with element or attribute test");
                            return;
                        }
                    }

                    if (a->axis == ASTAxisStep::SELF)
                        Sema::getLeafAndTrimmedPath(a->expr, ln, lt, t_path);
                    else if (a->axis == ASTAxisStep::CHILD || a->axis == ASTAxisStep::ATTRIBUTE)
                        *t_path = a->expr->dup();
                    else
                        *t_path = new ASTAxis(a->loc, ASTAxisStep::DESCENDANT_OR_SELF, a->expr->dup(), new ASTElementTest(a->loc));

                    break;

                default:
                    drv->error(a->loc, SE5049, std::string("axis ") + axis_str[a->axis] + "is not permitted in on XPath in the statement");
                    break;
            }
        }
        else
        {
            drv->error(path->loc, SE5049, "incorrect on-XPath in the statement");
        }
    }
}