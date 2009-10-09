/*
 * File:  lreturn.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/sema/LReturn.h"
#include "common/errdbg/exceptions.h"

#define IGNORE_OFFERS(count) (offers.erase(offers.begin() + (offers.size() - count), offers.end()))

namespace sedna
{
    void LReturn::visit(ASTAlterUser &n)
    {
        offers.push_back(defDDOOffer);
    }

    void LReturn::visit(ASTAttr &n)
    {
        unsigned int count = (n.cont) ? n.cont->size() : 0;

        VisisASTNodesVector(n.cont);

        if (count)
            IGNORE_OFFERS(count);

        offers.push_back(defDDOOffer);
    }

    void LReturn::visit(ASTAttrConst &n)
    {
        if (n.expr)
            n.expr->accept(*this);

        offers.pop_back();

        offers.push_back(defDDOOffer);
    }

    void LReturn::visit(ASTAttribTest &n)
    {
        offers.push_back(defDDOOffer);
    }

    void LReturn::visit(ASTAxisStep &n)
    {
        parentRequest origReq = parentReq;
        childOffer off_cont = {true, true, true, true}; // by default we work with some context we don't know about (maybe need to refine this later)
        childOffer off_this;

        // check predicates
        if (n.preds)
        {
            parentRequest req;

            req.calledOnce = origReq.calledOnce;
            req.distinctOnly = false;

            VisistASTNodesVector(n.preds, *this, req);
            IGNORE_OFFERS(n.preds->size());
        }

        // try to merge some axes together
        if (n.axis == ASTAxisStep::CHILD || n.axis == ASTAxisStep::SELF || n.axis == ASTAxisStep::ATTRIBUTE)
        {
            ASTAxisStep *ns = dynamic_cast<ASTAxisStep *>(n.cont);

            if (ns && ns->axis == ASTAxisStep::DESCENDANT_OR_SELF && !ns->preds && dynamic_cast<ASTNodeTest *>(ns->test)) // d-o-s::node()
            {
                switch (n.axis)
                {
                    case ASTAxisStep::CHILD:
                        n.axis = ASTAxisStep::DESCENDANT;
                        break;
                    case ASTAxisStep::SELF:
                        n.axis = ASTAxisStep::DESCENDANT_OR_SELF;
                        break;
                    case ASTAxisStep::ATTRIBUTE:
                        n.axis = ASTAxisStep::DESCENDANT_ATTRIBUTE;
                        break;
                    default:
                        break;
                }

                // get rid of the next step
                n.cont = ns.cont;
                ns.cont = NULL;
                delete ns;
            }
        }

        if (n.cont)
        {
            parentReq.toDDO = false;
            parentReq.toDistinct = false;
            parentReq.calledOnce = origReq.calledOnce;
            n.cont->accept(*this);
            off_cont = getOffer();

            // it seems that we need only to distinct, not order, intermediate results since we can order them at the last step
            // exception: self axis is a filter itself, so we don't want to put distinct below it
            if (!off_cont.isDistincted && n.axis != ASTAxisStep::SELF)
            {
                n.cont = new ASTDDO(n.loc, n.cont, true);
                off_cont.isDistincted = true;
                off_cont.isOrdered = false;
            }
        }

        // now we need to write our initial offer to parent
        // important assumption here: context is already distincted (if not, we insert 'distinct' earlier)
        switch (n.axis)
        {
            case ASTAxisStep::CHILD:
                off_this.isOrdered = off_cont.isOrdered;
                off_this.isDistincted = off_cont.isDistincted;
                off_this.isMax1 = false;
                off_this.isSingleLevel = off_cont.isSingleLevel;
                break;
            case ASTAxisStep::DESCENDANT:
                off_this.isOrdered = off_cont.isOrdered && off_cont.isSingleLevel;
                off_this.isDistincted = off_cont.isDistincted && off_cont.isSingleLevel;
                off_this.isMax1 = false;
                off_this.isSingleLevel = false;
                break;
            case ASTAxisStep::ATTRIBUTE:
                off_this.isOrdered = off_cont.isOrdered;
                off_this.isDistincted = off_cont.isDistincted;
                off_this.isMax1 = false; // can refine it later (check if this is the named-attribute retrieval. then it would be true)
                off_this.isSingleLevel = off_cont.isSingleLevel;
                break;
            case ASTAxisStep::SELF:
                off_this.isOrdered = off_cont.isOrdered;
                off_this.isDistincted = off_cont.isDistincted;
                off_this.isMax1 = off_cont.isMax1;
                off_this.isSingleLevel = off_cont.isSingleLevel;
                break;
            case ASTAxisStep::DESCENDANT_OR_SELF:
                off_this.isOrdered = off_cont.isOrdered && off_cont.isSingleLevel;
                off_this.isDistincted = off_cont.isDistincted && off_cont.isSingleLevel;
                off_this.isMax1 = false;
                off_this.isSingleLevel = false;
                break;
            case ASTAxisStep::FOLLOWING_SIBLING:
                off_this.isOrdered = off_cont.isMax1;
                off_this.isDistincted = off_cont.isMax1;
                off_this.isMax1 = false;
                off_this.isSingleLevel = off_cont.isSingleLevel;
                break;
            case ASTAxisStep::FOLLOWING:
                off_this.isOrdered = off_cont.isMax1;
                off_this.isDistincted = off_cont.isMax1;
                off_this.isMax1 = false;
                off_this.isSingleLevel = false;
                break;
            case ASTAxisStep::PARENT:
                off_this.isOrdered = off_cont.isOrdered;
                off_this.isDistincted = off_cont.isMax1;
                off_this.isMax1 = off_cont.isMax1;
                off_this.isSingleLevel = off_cont.isSingleLevel;
                break;
            case ASTAxisStep::ANCESTOR: 
                off_this.isOrdered = false; // assume here that it returns non-ordered even for a singleton (it'll be reverse-order probably)
                off_this.isDistincted = off_cont.isMax1;
                off_this.isMax1 = false;
                off_this.isSingleLevel = false;
                break;
            case ASTAxisStep::PRECEDING_SIBLING:
                off_this.isOrdered = false; // assume here that it returns non-ordered even for a singleton (it'll be reverse-order probably)
                off_this.isDistincted = off_cont.isMax1;
                off_this.isMax1 = false;
                off_this.isSingleLevel = off_cont.isSingleLevel;
                break;
            case ASTAxisStep::PRECEDING:
                off_this.isOrdered = false; // assume here that it returns non-ordered even for a singleton (it'll be reverse-order probably)
                off_this.isDistincted = off_cont.isMax1;
                off_this.isMax1 = false;
                off_this.isSingleLevel = false;
                break;
            case ASTAxisStep::ANCESTOR_OR_SELF:
                off_this.isOrdered = false; // assume here that it returns non-ordered even for a singleton (it'll be reverse-order probably)
                off_this.isDistincted = off_cont.isMax1;
                off_this.isMax1 = false;
                off_this.isSingleLevel = false;
                break;
            case ASTAxisStep::DESCENDANT_ATTRIBUTE:
                off_this.isOrdered = off_cont.isOrdered && off_cont.isSingleLevel;
                off_this.isDistincted = off_cont.isDistincted && off_cont.isSingleLevel;
                off_this.isMax1 = false;
                off_this.isSingleLevel = false;
                break;
        }

        // if this is the last step the we need to order it
        if (n.isLast)
        {
            ASTNode *ddo;

            if (isModeOrdered && !origReq.distinctOnly && (!off_this.isOrdered || !off_this.isDistincted))
            {
                ddo = new ASTDDO(n.loc, &n);
                modifyParent(ddo, false, false);

                off_this.isOrdered = true;
                off_this.isDistincted = true;
            }
            else if ((!isModeOrdered || origReq.distinctOnly) && !off_this.isDistincted)
            {
                ddo = new ASTDDO(n.loc, &n, true);
                modifyParent(ddo, false, false);

                off_this.isOrdered = false;
                off_this.isDistincted = true;
            }
        }

        setOffer(off_this);
    }

    void LReturn::visit(ASTBaseURI &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTBop &n)
    {
        n.lop->accept(*this);
        n.rop->accept(*this);

    }

    void LReturn::visit(ASTBoundSpaceDecl &n)
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

    void LReturn::visit(ASTCase &n)
    {
        bool param_ok;

        if (n.var)
        {
            setParamMode();
            n.var->accept(*this);
            unsetParamMode();

            param_ok = (param_count == 1);
        }

        if (n.type)
            n.type->accept(*this);

        n.expr->accept(*this);

        if (n.var && param_ok)
        {
            bound_vars.pop_back();
        }
    }

    void LReturn::visit(ASTCast &n)
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
        }

        delete t_pref;
        delete t_loc;
    }

    void LReturn::visit(ASTCastable &n)
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
        }

        delete t_pref;
        delete t_loc;
    }

    void LReturn::visit(ASTCharCont &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTCommTest &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTCommentConst &n)
    {
        n.expr->accept(*this);
    }

    void LReturn::visit(ASTConstDecl &n)
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

    void LReturn::visit(ASTCreateColl &n)
    {
        n.coll->accept(*this);
    }

    void LReturn::visit(ASTCreateDoc &n)
    {
        n.doc->accept(*this);

        if (n.coll)
            n.coll->accept(*this);
    }

    void LReturn::visit(ASTCreateFtIndex &n)
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

    void LReturn::visit(ASTCreateIndex &n)
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

    void LReturn::visit(ASTCreateRole &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTCreateTrg &n)
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

    void LReturn::visit(ASTCreateUser &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTDDO &n)
    {
        n.expr->accept(*this);
    }

    void LReturn::visit(ASTDeclareCopyNsp &n)
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

    void LReturn::visit(ASTDefCollation &n)
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

    void LReturn::visit(ASTDefNamespaceDecl &n)
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

    void LReturn::visit(ASTDocConst &n)
    {
        n.expr->accept(*this);
    }

    void LReturn::visit(ASTDocTest &n)
    {
        if (n.elem_test) n.elem_test->accept(*this);
    }

    void LReturn::visit(ASTDropColl &n)
    {
        n.coll->accept(*this);
    }

    void LReturn::visit(ASTDropDoc &n)
    {
        n.doc->accept(*this);

        if (n.coll)
            n.coll->accept(*this);
    }

    void LReturn::visit(ASTDropFtIndex &n)
    {
        n.index->accept(*this);
    }

    void LReturn::visit(ASTDropIndex &n)
    {
        n.index->accept(*this);
    }

    void LReturn::visit(ASTDropMod &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTDropRole &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTDropTrg &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTDropUser &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTElem &n)
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

    void LReturn::visit(ASTElemConst &n)
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

    void LReturn::visit(ASTElementTest &n)
    {
        if (n.name) n.name->accept(*this);
        if (n.type) n.type->accept(*this);
    }

    void LReturn::visit(ASTEmptyTest &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTError &n)
    {
        throw SYSTEM_EXCEPTION("If you see this, you are very unlucky. Anyway, this is an internal parser error.");
    }

    void LReturn::visit(ASTExtExpr &n)
    {
        VisitNodesVector(n.pragmas, *this);

        if (n.expr)
            n.expr->accept(*this);
        else
            drv->error(n.loc, XQST0079, NULL); // since we don't support any pragmas yet
    }

    void LReturn::visit(ASTFilterStep &n)
    {
        bool found = false;

        if (n.cont)
        {
            n.cont->accept(*this);
            bound_vars.push_back(XQVariable("$%v", NULL));
        }

        if (n.expr)
        {
            n.expr->accept(*this);
        }
        else // context expression
        {
            for (int i = bound_vars.size() - 1; i >= 0; i--)
            {
                if (bound_vars[i].first == "$%v")
                {
                    found = true;
                    break;
                }
            }

            if (!found)
            {
                if (!mod->xpdy0002)
                    mod->xpdy0002 = &n.loc;
                return;
            }
        }

        if (n.preds)
        {
            bound_vars.push_back(XQVariable("$%v", NULL));

            for (unsigned int i = 0; i < n.preds->size(); i++)
                (*n.preds)[i]->accept(*this);

            bound_vars.pop_back();
        }

        if (n.cont)
            bound_vars.pop_back();
    }

    void LReturn::visit(ASTFor &n)
    {
        unsigned int params;

        n.expr->accept(*this);

        setParamMode();

        n.tv->accept(*this);
        if (n.pv)
            n.pv->accept(*this);

        unsetParamMode();

        params = param_count;

        if (param_count == 2) // got usual+positional variables
        {
            std::string pos_name, for_name;

            U_ASSERT(bound_vars.size() >= 2);

            pos_name = bound_vars.back().first;
            for_name = (*(bound_vars.end() - 2)).first;

            if (pos_name == for_name)
            {
                drv->error(n.loc, XQST0089, pos_name);
            }
        }

        n.fd->accept(*this);

        bound_vars.erase(bound_vars.begin() + (bound_vars.size() - params), bound_vars.end());
    }

    void LReturn::visit(ASTFunCall &n)
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

        if (!n.int_name)
        {
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

                n.int_name = new std::string("");

                return;
            }

            n.int_name = new std::string(xqf->int_name);

            if (*n.int_name != "") // standard function
                rewriteStdFunCall(n, *n.int_name);
        }
    }

    void LReturn::visit(ASTFuncDecl &n)
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
            setParamMode();

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

            unsetParamMode();
        }

        // check result type
        if (n.ret)
            n.ret->accept(*this);

        // check body
        if (n.body)
            n.body->accept(*this); // side effect: param_count may change, so we save it to params_count

        // we should clear bounded variables
        if (n.params)
            bound_vars.erase(bound_vars.begin() + (bound_vars.size() - params_count), bound_vars.end());
    }

    void LReturn::visit(ASTGrantPriv &n)
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

    void LReturn::visit(ASTGrantRole &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTIf &n)
    {
        n.i_expr->accept(*this);
        n.t_expr->accept(*this);
        n.e_expr->accept(*this);
    }

    void LReturn::visit(ASTInstOf &n)
    {
        n.expr->accept(*this);
        n.type->accept(*this);
    }

    void LReturn::visit(ASTItemTest &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTLet &n)
    {
        bool param_ok;

        n.expr->accept(*this);

        setParamMode();
        n.tv->accept(*this);
        unsetParamMode();

        param_ok = (param_count == 1);

        n.fd->accept(*this);

        if (param_ok)
            bound_vars.pop_back();
    }

    void LReturn::visit(ASTLibModule &n)
    {
        is_imported = n.is_internal;

        n.moduleDecl->accept(*this);
        n.prolog->accept(*this);
    }

    void LReturn::visit(ASTLit &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTLoadFile &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTLoadModule &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTMainModule &n)
    {
        if (drv == NULL)
            throw SYSTEM_EXCEPTION("Driver is not set for semantic analyzer!");

        n.prolog->accept(*this);
        n.query->accept(*this);
    }

    void LReturn::visit(ASTMetaCols &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTMetaDocs &n)
    {
        if (n.coll)
            n.coll->accept(*this);
    }

    void LReturn::visit(ASTMetaSchemaCol &n)
    {
        n.coll->accept(*this);
    }

    void LReturn::visit(ASTMetaSchemaDoc &n)
    {
        n.doc->accept(*this);

        if (n.coll)
            n.coll->accept(*this);
    }

    void LReturn::visit(ASTModImport &n)
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

    void LReturn::visit(ASTModuleDecl &n)
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

    void LReturn::visit(ASTNameTest &n)
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

        if (!n.uri)
        {
            uri = resolveQName(n.loc, n.pref->c_str(), (att_test) ? "" : NULL);

            if (uri)
                n.uri = new std::string(uri);
        }
    }

    void LReturn::visit(ASTNamespaceDecl &n)
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

    void LReturn::visit(ASTNodeTest &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTNsp &n)
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

    void LReturn::visit(ASTOption &n)
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

    void LReturn::visit(ASTOrdExpr &n)
    {
        n.expr->accept(*this);
    }

    void LReturn::visit(ASTOrder &n)
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

    void LReturn::visit(ASTOrderBy &n)
    {
        VisitNodesVector(n.specs, *this);
    }

    void LReturn::visit(ASTOrderByRet &n)
    {
        unsigned int params = 0;
        n.iter_expr->accept(*this);

        setParamMode();
        VisitNodesVector(n.vars, *this);
        unsetParamMode();
        params = param_count;

        // we may have one problem here; if for-let expressions have overlapping bindings, we'll get duplicate vars, which is not an error.
        // so, we just get rid of duplicates respecting later bindings
        if (params == n.vars->size() && params > 1) // no error occured
        {
            unsigned int offs = 0, i, j;

            for (i = bound_vars.size() - params; i < bound_vars.size(); i++)
            {
                for (j = i + 1; j < bound_vars.size(); j++)
                {
                    if (bound_vars[j].first == bound_vars[i].first)
                    {
                        delete (*n.vars)[i - bound_vars.size() + params - offs];
                        n.vars->erase(n.vars->begin() + (i - bound_vars.size() + params - offs));
                        offs++;
                        break;
                    }
                }
            }
        }

        n.ord_expr->accept(*this);
        n.ret_expr->accept(*this);

        bound_vars.erase(bound_vars.begin() + (bound_vars.size() - params), bound_vars.end());
    }

    void LReturn::visit(ASTOrderEmpty &n)
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

    void LReturn::visit(ASTOrderMod &n)
    {
        if (n.ad_mod)
            n.ad_mod->accept(*this);

        if (n.em_mod)
            n.em_mod->accept(*this);

        if (n.col_mod)
            n.col_mod->accept(*this);
    }

    void LReturn::visit(ASTOrderModInt &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTOrderSpec &n)
    {
        n.expr->accept(*this);

        if (n.mod)
            n.mod->accept(*this);
    }

    void LReturn::visit(ASTPIConst &n)
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

    void LReturn::visit(ASTPi &n)
    {
        if (n.name->size() == 3 && toupper((*n.name)[0]) == 'X' && toupper((*n.name)[1]) == 'M' && toupper((*n.name)[2]) == 'L')
        {
            drv->error(n.loc, XPST0003, std::string("reserved processing instruction name: '") + *n.name + "'");
        }
    }

    void LReturn::visit(ASTPiTest &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTPosVar &n)
    {
        n.var->accept(*this);
    }

    void LReturn::visit(ASTPragma &n)
    {
        if (*n.pref == "")
        {
            drv->error(n.loc, XPST0081, "pragma QName must be prefixed");
            return;
        }

        resolveQName(n.loc, n.pref->c_str(), "");
    }

    void LReturn::visit(ASTProlog &n)
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

    void LReturn::visit(ASTQName &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTQuantExpr &n)
    {
        bool param_ok;

        n.expr->accept(*this);

        setParamMode();
        n.var->accept(*this);
        unsetParamMode();

        param_ok = (param_count == 1);

        n.sat->accept(*this);

        if (param_ok)
            bound_vars.pop_back();
    }

    void LReturn::visit(ASTQuery &n)
    {
        n.query->accept(*this);
    }

    void LReturn::visit(ASTRenameColl &n)
    {
        n.name_old->accept(*this);
        n.name_new->accept(*this);
    }

    void LReturn::visit(ASTRevokePriv &n)
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

    void LReturn::visit(ASTRevokeRole &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTSchemaAttrTest &n)
    {
        att_test = true;
        n.name->accept(*this);
        att_test = false;

        drv->error(n.loc, XPST0008, "there is no schema to test");
    }

    void LReturn::visit(ASTSchemaElemTest &n)
    {
        n.name->accept(*this);

        drv->error(n.loc, XPST0008, "there is no schema to test");
    }

    void LReturn::visit(ASTSeq &n)
    {
        for (unsigned int i = 0; i < n.exprs->size(); i++)
            (*n.exprs)[i]->accept(*this);
    }

    void LReturn::visit(ASTSpaceSeq &n)
    {
        n.expr->accept(*this);
    }

    void LReturn::visit(ASTTextConst &n)
    {
        n.expr->accept(*this);
    }

    void LReturn::visit(ASTTextTest &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTTreat &n)
    {
        n.expr->accept(*this);
        n.type->accept(*this);
    }

    void LReturn::visit(ASTType &n)
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

    void LReturn::visit(ASTTypeSeq &n)
    {
        // we just propagate analysis to the actual test
        n.type_test->accept(*this);
    }

    void LReturn::visit(ASTTypeSingle &n)
    {
        n.type->accept(*this);
    }

    void LReturn::visit(ASTTypeSwitch &n)
    {
        n.expr->accept(*this);
        VisitNodesVector(n.cases, *this);
        n.def_case->accept(*this);
    }

    void LReturn::visit(ASTTypeVar &n)
    {
        n.type->accept(*this);
        n.var->accept(*this);
    }

    void LReturn::visit(ASTUnio &n)
    {
        // we will run vars ib param mode to make duplicate removal a bit easier
        // duplicates could exist because of for-variables rebindings
        // we cannot get 'unbound variable' errors here since they cannot contradict to ancestor fun-defs
        unsigned int params = 0;

        setParamMode();
        VisitNodesVector(n.vars, *this);
        params = param_count;
        unsetParamMode();

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
                    n.vars->erase(n.vars->begin() + (i - bound_vars.size() + params - offs));
                    offs++;
                    break;
                }
            }
        }

        bound_vars.erase(bound_vars.begin() + (bound_vars.size() - params), bound_vars.end());
    }

    void LReturn::visit(ASTUop &n)
    {
        n.expr->accept(*this);
    }

    void LReturn::visit(ASTUpdDel &n)
    {
        n.what->accept(*this);
    }

    void LReturn::visit(ASTUpdInsert &n)
    {
        n.what->accept(*this);
        n.where->accept(*this);
    }

    void LReturn::visit(ASTUpdMove &n)
    {
        bool param_ok;

        setParamMode();
        n.var->accept(*this);
        unsetParamMode();
        param_ok = (param_count == 1);

        n.what->accept(*this);
        n.where->accept(*this);

        if (param_ok)
            bound_vars.pop_back();
    }

    void LReturn::visit(ASTUpdRename &n)
    {
        n.what->accept(*this);
    }

    void LReturn::visit(ASTUpdReplace &n)
    {
        bool param_ok;

        setParamMode();
        n.var->accept(*this);
        unsetParamMode();
        param_ok = (param_count == 1);

        n.what->accept(*this);
        n.new_expr->accept(*this);

        if (param_ok)
            bound_vars.pop_back();
    }

    void LReturn::visit(ASTVar &n)
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

    void LReturn::visit(ASTVarDecl &n)
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

    void LReturn::visit(ASTVersionDecl &n)
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

    void LReturn::visit(ASTXMLComm &n)
    {
        // nothing to do
    }

    // Some additional function

    void LReturn::setParamMode()
    {
        param_mode = true;
        param_count = 0;
    }
    void LReturn::unsetParamMode()
    {
        param_mode = false;
    }

    void LReturn::setOffer(const childOffer off)
    {
        offers.push_back(off);
    }

    childOffer LReturn::getOffer()
    {
        childOffer res = offers.back();

        offers.pop_back();

        return res;
    }

    void LReturn::VisitNodesVector(ASTNodesVector *nodes, ASTVisitor &v, parentRequest req)
    {
        ASTNodesVector::iterator it;
        ASTNode *node;

        if (nodes == NULL) return;

        for (unsigned int i = 0; i < nodes->size(); i++)
        {
            parentReq = req;
            node = (*nodes)[i];
            node->accept(v);
        }
    }
}
