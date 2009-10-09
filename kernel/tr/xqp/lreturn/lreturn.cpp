/*
 * File:  lreturn.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/lreturn/lreturn.h"
#include "common/errdbg/exceptions.h"

#define IGNORE_OFFERS(count) (offers.erase(offers.begin() + (offers.size() - count), offers.end()))

namespace sedna
{
    void LReturn::visit(ASTAlterUser &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTAttr &n)
    {
        unsigned int count = (n.cont) ? n.cont->size() : 0;
        childOffer off;

        if (n.cont)
        {
            parentRequest req(getParentRequest());

            req.distinctOnly = false;
            VisitNodesVector(n.cont, *this, req);
            off = mergeOffers(count);
        }

        // direct attribute constructor creates only one node
        off.exi.isOrdered = true;
        off.exi.isDistincted = true;
        off.exi.isSingleLevel = true;
        off.exi.isMax1 = true;
        off.isCached = false;
        off.exi.useConstructors = true;

        setOffer(off);
    }

    void LReturn::visit(ASTAttrConst &n)
    {
        childOffer off;
        unsigned int count = 0;

        if (n.name)
        {
            parentRequest req(getParentRequest());

            req.distinctOnly = true;
            setParentRequest(req);
            n.name->accept(*this);
            count++;
        }

        if (n.expr)
        {
            parentRequest req(getParentRequest());

            req.distinctOnly = false;
            setParentRequest(req);
            n.expr->accept(*this);
            count++;
        }

        off = mergeOffers(count);

        // computed attribute constructor creates only one node
        off.exi.isOrdered = true;
        off.exi.isDistincted = true;
        off.exi.isSingleLevel = true;
        off.exi.isMax1 = true;
        off.isCached = false;
        off.exi.useConstructors = true;

        setOffer(off);
    }

    void LReturn::visit(ASTAttribTest &n)
    {
        // don't need to go to name and type here since there is nothing down there we could compute
        setOffer(childOffer());
    }

    void LReturn::visit(ASTAxisStep &n)
    {
        childOffer off_cont; // by default we work with some context we don't know about (maybe need to refine this later)
        childOffer off_this;
        childOffer off_preds;

        // check predicates
        if (n.preds)
        {
            parentRequest req;

            req.calledOnce = false;
            req.distinctOnly = true;

            // bind the context
            bound_vars.push_back(XQVariable("$%v", NULL));

            VisitNodesVector(n.preds, *this, req);
            off_preds = mergeOffers(n.preds->size());

            ignoreVariables(off_preds, 1); // sweep out the context
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
                n.cont = ns->cont;
                ns->cont = NULL;
                delete ns;
            }
        }

        if (n.cont)
        {
            parentRequest req(getParentRequest());

            req.distinctOnly = false;
            setParentRequest(req);
            n.cont->accept(*this);
            off_this = off_cont = getOffer();

            off_this.isCached = false;

            // it seems that we need only to distinct, not order, intermediate results since we can order them at the last step
            // exception: self axis is a filter itself, so we don't want to put distinct below it
            // it is commented for now: we should reason about distict-on-each-step later though
/*            if (!off_cont.exi.isDistincted && n.axis != ASTAxisStep::SELF)
            {
                n.cont = new ASTDDO(n.getLocation(), n.cont, true);
                off_cont.exi.isDistincted = true;
                off_cont.exi.isOrdered = false;
            }*/
        }

        // now we need to write our initial offer to parent
        switch (n.axis)
        {
            case ASTAxisStep::CHILD:
                off_this.exi.isOrdered = off_cont.exi.isOrdered;
                off_this.exi.isDistincted = off_cont.exi.isDistincted;
                off_this.exi.isMax1 = false;
                off_this.exi.isSingleLevel = off_cont.exi.isSingleLevel;
                break;
            case ASTAxisStep::DESCENDANT:
                off_this.exi.isOrdered = off_cont.exi.isOrdered && off_cont.exi.isSingleLevel;
                off_this.exi.isDistincted = off_cont.exi.isDistincted && off_cont.exi.isSingleLevel;
                off_this.exi.isMax1 = false;
                off_this.exi.isSingleLevel = false;
                break;
            case ASTAxisStep::ATTRIBUTE:
                off_this.exi.isOrdered = off_cont.exi.isOrdered;
                off_this.exi.isDistincted = off_cont.exi.isDistincted;
                off_this.exi.isMax1 = false; // can refine it later (check if this is the named-attribute retrieval. then it would be true)
                off_this.exi.isSingleLevel = off_cont.exi.isSingleLevel;
                break;
            case ASTAxisStep::SELF:
                off_this.exi.isOrdered = off_cont.exi.isOrdered;
                off_this.exi.isDistincted = off_cont.exi.isDistincted;
                off_this.exi.isMax1 = off_cont.exi.isMax1;
                off_this.exi.isSingleLevel = off_cont.exi.isSingleLevel;
                break;
            case ASTAxisStep::DESCENDANT_OR_SELF:
                off_this.exi.isOrdered = off_cont.exi.isOrdered && off_cont.exi.isSingleLevel;
                off_this.exi.isDistincted = off_cont.exi.isDistincted && off_cont.exi.isSingleLevel;
                off_this.exi.isMax1 = false;
                off_this.exi.isSingleLevel = false;
                break;
            case ASTAxisStep::FOLLOWING_SIBLING:
                off_this.exi.isOrdered = off_cont.exi.isMax1;
                off_this.exi.isDistincted = off_cont.exi.isMax1;
                off_this.exi.isMax1 = false;
                off_this.exi.isSingleLevel = off_cont.exi.isSingleLevel;
                break;
            case ASTAxisStep::FOLLOWING:
                off_this.exi.isOrdered = off_cont.exi.isMax1;
                off_this.exi.isDistincted = off_cont.exi.isMax1;
                off_this.exi.isMax1 = false;
                off_this.exi.isSingleLevel = false;
                break;
            case ASTAxisStep::PARENT:
                off_this.exi.isOrdered = off_cont.exi.isOrdered;
                off_this.exi.isDistincted = off_cont.exi.isMax1;
                off_this.exi.isMax1 = off_cont.exi.isMax1;
                off_this.exi.isSingleLevel = off_cont.exi.isSingleLevel;
                break;
            case ASTAxisStep::ANCESTOR: 
                off_this.exi.isOrdered = false; // assume here that it returns non-ordered even for a singleton (it'll be reverse-order probably)
                off_this.exi.isDistincted = off_cont.exi.isMax1;
                off_this.exi.isMax1 = false;
                off_this.exi.isSingleLevel = false;
                break;
            case ASTAxisStep::PRECEDING_SIBLING:
                off_this.exi.isOrdered = false; // assume here that it returns non-ordered even for a singleton (it'll be reverse-order probably)
                off_this.exi.isDistincted = off_cont.exi.isMax1;
                off_this.exi.isMax1 = false;
                off_this.exi.isSingleLevel = off_cont.exi.isSingleLevel;
                break;
            case ASTAxisStep::PRECEDING:
                off_this.exi.isOrdered = false; // assume here that it returns non-ordered even for a singleton (it'll be reverse-order probably)
                off_this.exi.isDistincted = off_cont.exi.isMax1;
                off_this.exi.isMax1 = false;
                off_this.exi.isSingleLevel = false;
                break;
            case ASTAxisStep::ANCESTOR_OR_SELF:
                off_this.exi.isOrdered = false; // assume here that it returns non-ordered even for a singleton (it'll be reverse-order probably)
                off_this.exi.isDistincted = off_cont.exi.isMax1;
                off_this.exi.isMax1 = false;
                off_this.exi.isSingleLevel = false;
                break;
            case ASTAxisStep::DESCENDANT_ATTRIBUTE:
                off_this.exi.isOrdered = off_cont.exi.isOrdered && off_cont.exi.isSingleLevel;
                off_this.exi.isDistincted = off_cont.exi.isDistincted && off_cont.exi.isSingleLevel;
                off_this.exi.isMax1 = false;
                off_this.exi.isSingleLevel = false;
                break;
        }

        // set used variable appropriately (from context AND from predicates)
        off_this.usedVars.insert(off_preds.usedVars.begin(), off_preds.usedVars.end());

        // now we need to decide if we want to cache it
        if (!getParentRequest().calledOnce)
        {
            cacheTheNode(&n, off_this);

            // if we cache this step then we don't need to cache the previous one
            if (off_this.isCached && n.cont && off_cont.isCached)
                n.cont->setCached(false);
        }

        // if this is the last step the we need to order(distinct) it
        if (n.isLast)
        {
            ASTNode *ddo;

            if (isModeOrdered && !getParentRequest().distinctOnly && (!off_this.exi.isOrdered || !off_this.exi.isDistincted))
            {
                ddo = new ASTDDO(n.getLocation(), &n);

                // if we cache the node then cache upstreamed ddo instead
                if (off_this.isCached)
                {
                    n.setCached(false);
                    ddo->setCached(true);
                }

                modifyParent(ddo, false, false);

                off_this.exi.isOrdered = true;
                off_this.exi.isDistincted = true;
            }
            else if ((!isModeOrdered || getParentRequest().distinctOnly) && !off_this.exi.isDistincted)
            {
                ddo = new ASTDDO(n.getLocation(), &n, false);

                // if we cache the node then cache upstreamed ddo instead
                if (off_this.isCached)
                {
                    n.setCached(false);
                    ddo->setCached(true);
                }

                modifyParent(ddo, false, false);

                off_this.exi.isOrdered = false;
                off_this.exi.isDistincted = true;
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
        childOffer lof, rof, bopof;
        parentRequest req(getParentRequest());
        ASTNode *ddo = NULL;

        // we want distinct-only for these ops since they either do not depend on doc-order or do not work for non-singletons (XPTY0004)
        if (n.op >= ASTBop::OR && n.op <= ASTBop::GE_G)
            req.distinctOnly = true;

        setParentRequest(req);
        n.lop->accept(*this);
        lof = getOffer();

        setParentRequest(req);
        n.rop->accept(*this);
        rof = getOffer();

        bopof = mergeOffers(2);

        bopof.exi.isDistincted = true;
        bopof.exi.isOrdered = true;
        bopof.exi.isSingleLevel = true;
        bopof.exi.isMax1 = true;
        bopof.exi.useConstructors = false;

        if (n.op >= ASTBop::UNION && n.op <= ASTBop::EXCEPT)
        {
            bool left_ddo = lof.exi.isOrdered && lof.exi.isDistincted;
            bool right_ddo = rof.exi.isOrdered && rof.exi.isDistincted;

            bopof.exi.isSingleLevel = lof.exi.isSingleLevel && rof.exi.isSingleLevel;
            bopof.exi.useConstructors = lof.exi.useConstructors || rof.exi.useConstructors;

            switch (n.op)
            {
                case ASTBop::INTERSECT:
                    bopof.exi.isMax1 = lof.exi.isMax1 || rof.exi.isMax1;
                    break;

                case ASTBop::EXCEPT:
                    bopof.exi.isMax1 = lof.exi.isMax1;
                    break;

                case ASTBop::UNION:
                    bopof.exi.isMax1 = false;
                    break;

                default:
                    bopof.exi.isMax1 = true;
            }

            bopof.exi.isOrdered = bopof.exi.isMax1;

            if (!left_ddo || !right_ddo || !isModeOrdered || getParentRequest().distinctOnly)
            {
                if (!lof.exi.isMax1)
                    n.lop = new ASTDDO(n.lop->getLocation(), n.lop, false);

                if (!rof.exi.isMax1)
                    n.rop = new ASTDDO(n.rop->getLocation(), n.rop, false);

                if (lof.exi.isMax1 && rof.exi.isMax1 && isModeOrdered && !getParentRequest().distinctOnly)
                {
                    n.doc_order = true;
                    bopof.exi.isOrdered = true;
                }
                else if (isModeOrdered && !getParentRequest().distinctOnly && !bopof.exi.isMax1)
                {
                    ddo = new ASTDDO(n.getLocation(), &n);
                    modifyParent(ddo, false, false);
                    bopof.exi.isOrdered = true;
                }
            }
            else
            {
                n.doc_order = true;
                bopof.exi.isOrdered = true;
            }
        }

        // now, consider caching ops that deal with sequences
        if (n.op >= ASTBop::EQ_G && n.op <= ASTBop::EXCEPT && !getParentRequest().calledOnce)
        {
            cacheTheNode(&n, bopof);

            // if we cache this the we don't need to cache children
            if (bopof.isCached)
            {
                // if we've upstreamed ddo the we should cache that instead of the current node
                if (ddo)
                {
                    ddo->setCached(true);
                    n.setCached(false);
                }

                if (lof.isCached)
                    n.lop->setCached(false);
                if (rof.isCached)
                    n.rop->setCached(false);
            }
        }

        setOffer(bopof);
    }

    void LReturn::visit(ASTBoundSpaceDecl &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTCase &n)
    {
        parentRequest req(getParentRequest());
        childOffer coff;

        // first, bind variable
        if (n.var)
        {
            childOffer ts_off;

            // variable is bound to typeswitch expression; so we should check its properties
            // we do it via special typeswitch-bound "$%ts" variable, which should be last in bound_vars
            U_ASSERT(bound_vars.back().int_name == "$%ts");
            ts_off.exi = bound_vars.back().exp_info;

            setParamMode();
            n.var->accept(*this);
            unsetParamMode();

            bound_vars.back().exp_info = ts_off.exi;
        }

        setParentRequest(req);
        n.expr->accept(*this);

        coff = getOffer();

        coff.isCached = false; // we don't cache this node

        if (n.var)
        {
            ignoreVariables(coff, 1);
        }

        setOffer(coff);
    }

    void LReturn::visit(ASTCast &n)
    {
        parentRequest req(getParentRequest());
        childOffer off_this, eoff;

        req.distinctOnly = true; // cast doesn't work for >1 sequencies
        setParentRequest(req);
        n.expr->accept(*this);

        eoff = getOffer();

        // default offer makes perfect sense here since cast always returns atomic
        off_this.usedVars = eoff.usedVars;

        setOffer(off_this);
    }

    void LReturn::visit(ASTCastable &n)
    {
        parentRequest req(getParentRequest());
        childOffer off_this, eoff;

        req.distinctOnly = true; // castable always returns false for 1+ sequencies
        setParentRequest(req);
        n.expr->accept(*this);

        eoff = getOffer();

        // default offer makes perfect sense here since castable always returns atomic
        off_this.usedVars = eoff.usedVars;

        setOffer(off_this);
    }

    void LReturn::visit(ASTCharCont &n)
    {
        // nothing to do
        setOffer(childOffer());
    }

    void LReturn::visit(ASTCommTest &n)
    {
        // nothing to do
        setOffer(childOffer());
    }

    void LReturn::visit(ASTCommentConst &n)
    {
        parentRequest req(getParentRequest());
        childOffer off;

        req.distinctOnly = false;
        setParentRequest(req);
        n.expr->accept(*this);

        off = getOffer();

        // comment constructor creates only one node
        off.exi.isOrdered = true;
        off.exi.isDistincted = true;
        off.exi.isSingleLevel = true;
        off.exi.isMax1 = true;
        off.isCached = false;
        off.exi.useConstructors = true;

        setOffer(off);
    }

    void LReturn::visit(ASTConstDecl &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTCreateColl &n)
    {
        parentRequest req;

        req.distinctOnly = true;
        req.calledOnce = true;

        setParentRequest(req);
        n.coll->accept(*this);
    }

    void LReturn::visit(ASTCreateDoc &n)
    {
        parentRequest req;

        req.distinctOnly = true;
        req.calledOnce = true;

        setParentRequest(req);
        n.doc->accept(*this);

        if (n.coll)
        {
            setParentRequest(req);
            n.coll->accept(*this);
        }
    }

    void LReturn::visit(ASTCreateFtIndex &n)
    {
        // nothing to do
        // name cannot be computed
        // path is strict, so don't need to analyze
        // cust_expr contains only constants
        // type is a constant
    }

    void LReturn::visit(ASTCreateIndex &n)
    {
        // nothing to do
        // name cannot be computed
        // on_path and by_path are strict, so don't need to analyze
        // type is a constant
    }

    void LReturn::visit(ASTCreateRole &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTCreateTrg &n)
    {
        // name is a constant, path is strict; so we need only to check-optimize do-expressions
        parentRequest req;

        req.calledOnce = true;
        req.distinctOnly = false;

        VisitNodesVector(n.do_exprs, *this, req);
    }

    void LReturn::visit(ASTCreateUser &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTDDO &n)
    {
        // in fact we should not get here since all ddo-ops are inserted in dynamic mode without lreturn visitor taking them (we should not do re-analysis either)
        parentRequest req;
        childOffer off;

        req.distinctOnly = true;
        req.calledOnce = getParentRequest().calledOnce;

        setParentRequest(req);
        n.expr->accept(*this);

        off = getOffer();

        off.exi.isOrdered = true;
        off.exi.isDistincted = true;
        off.isCached = false;

        setOffer(off);
    }

    void LReturn::visit(ASTDeclareCopyNsp &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTDefCollation &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTDefNamespaceDecl &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTDocConst &n)
    {
        parentRequest req(getParentRequest());
        childOffer off;

        req.distinctOnly = false;
        setParentRequest(req);
        n.expr->accept(*this);

        off = getOffer();

        // document constructor creates only one node
        off.exi.isOrdered = true;
        off.exi.isDistincted = true;
        off.exi.isSingleLevel = true;
        off.exi.isMax1 = true;
        off.isCached = false;
        off.exi.useConstructors = true;

        setOffer(off);
    }

    void LReturn::visit(ASTDocTest &n)
    {
        // nothing to do
        setOffer(childOffer());
    }

    void LReturn::visit(ASTDropColl &n)
    {
        parentRequest req;

        req.distinctOnly = true;
        req.calledOnce = true;

        setParentRequest(req);
        n.coll->accept(*this);
    }

    void LReturn::visit(ASTDropDoc &n)
    {
        parentRequest req;

        req.distinctOnly = true;
        req.calledOnce = true;

        setParentRequest(req);
        n.doc->accept(*this);

        if (n.coll)
        {
            setParentRequest(req);
            n.coll->accept(*this);
        }
    }

    void LReturn::visit(ASTDropFtIndex &n)
    {
        parentRequest req;

        req.distinctOnly = true;
        req.calledOnce = true;

        setParentRequest(req);
        n.index->accept(*this);
    }

    void LReturn::visit(ASTDropIndex &n)
    {
        parentRequest req;

        req.distinctOnly = true;
        req.calledOnce = true;

        setParentRequest(req);
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
        unsigned int count = (n.cont) ? n.cont->size() : 0;
        childOffer off;

        if (n.cont)
        {
            parentRequest req(getParentRequest());

            req.distinctOnly = false;
            VisitNodesVector(n.cont, *this, req);
            off = mergeOffers(count);
        }

        // direct element constructor creates only one node
        off.exi.isOrdered = true;
        off.exi.isDistincted = true;
        off.exi.isSingleLevel = true;
        off.exi.isMax1 = true;
        off.isCached = false;
        off.exi.useConstructors = true;

        setOffer(off);
    }

    void LReturn::visit(ASTElemConst &n)
    {
        childOffer off;
        unsigned int count = 0;

        if (n.name)
        {
            parentRequest req(getParentRequest());

            req.distinctOnly = true;
            setParentRequest(req);
            n.name->accept(*this);
            count++;
        }

        if (n.expr)
        {
            parentRequest req(getParentRequest());

            req.distinctOnly = false;
            setParentRequest(req);
            n.expr->accept(*this);
            count++;
        }

        off = mergeOffers(count);

        // computed element constructor creates only one node
        off.exi.isOrdered = true;
        off.exi.isDistincted = true;
        off.exi.isSingleLevel = true;
        off.exi.isMax1 = true;
        off.isCached = false;
        off.exi.useConstructors = true;

        setOffer(off);
    }

    void LReturn::visit(ASTElementTest &n)
    {
        // nothing to do
        setOffer(childOffer());
    }

    void LReturn::visit(ASTEmptyTest &n)
    {
        // nothing to do
        setOffer(childOffer());
    }

    void LReturn::visit(ASTError &n)
    {
        throw SYSTEM_EXCEPTION("If you see this, you are very unlucky. Anyway, this is an internal parser error.");
    }

    void LReturn::visit(ASTExtExpr &n)
    {
        childOffer off;
        parentRequest req(getParentRequest());

        setParentRequest(req);
        n.expr->accept(*this);

        off = getOffer();

        off.isCached = false;

        setOffer(off);
    }

    void LReturn::visit(ASTFilterStep &n)
    {
        childOffer off_cont; // by default we work with some context we don't know about (maybe need to refine this later)
        childOffer off_this;
        childOffer off_preds;
        childOffer off_pe;

        // check predicates
        if (n.preds)
        {
            parentRequest req;

            req.calledOnce = false;
            req.distinctOnly = true;

            // bind the context
            bound_vars.push_back(XQVariable("$%v", NULL));

            VisitNodesVector(n.preds, *this, req);
            off_preds = mergeOffers(n.preds->size());

            // clean out the context
            ignoreVariables(off_preds, 1);
        }

        // primary expression
        if (n.expr)
        {
            parentRequest req(getParentRequest());

            // bind the context
            bound_vars.push_back(XQVariable("$%v", NULL));

            setParentRequest(req);
            n.expr->accept(*this);
            off_this = off_pe = getOffer();

            off_this.isCached = false;

            ignoreVariables(off_this, 1);
        }

        // context
        if (n.cont)
        {
            parentRequest req(getParentRequest());

            req.distinctOnly = false;
            setParentRequest(req);
            n.cont->accept(*this);
            off_cont = getOffer();

            // if we have context item expression then the result will be dictated by the previous step
            if (!n.expr)
            {
                off_this = off_cont;
                off_this.isCached = false;
            }
        }

        // set usedvars properly
        off_this.usedVars.insert(off_preds.usedVars.begin(), off_preds.usedVars.end());

        // now we need to decide if we want to cache it
        if (!getParentRequest().calledOnce)
        {
            cacheTheNode(&n, off_this);

            // if we cache this step then we don't need to cache the previous one
            if (off_this.isCached && n.cont && off_cont.isCached)
                n.cont->setCached(false);
        }

        // if we are in ordered mode and we use context nodes then we should ddo the previous step
        if (n.cont && isModeOrdered && (!off_cont.exi.isOrdered || !off_cont.exi.isDistincted) && off_pe.usedVars.find("$%v") != off_pe.usedVars.end())
        {
            ASTNode *ddo = new ASTDDO(n.getLocation(), n.cont);

            // if we've cached the previous step then move cache to the ddo
            if (off_cont.isCached)
            {
                ddo->setCached(true);
                n.cont->setCached(false);
            }
        }
        else if (n.cont && !off_cont.exi.isDistincted) // if we're in unordered mode or we don't use context we need only distinct
        {
            ASTNode *ddo = new ASTDDO(n.getLocation(), n.cont, false);

            // if we've cached the previous step then move cache to the ddo
            if (off_cont.isCached)
            {
                ddo->setCached(true);
                n.cont->setCached(false);
            }
        }

        // if this is the last and not the only step then we need to order(distinct) it
        if (n.isLast && n.cont)
        {
            ASTNode *ddo;

            if (isModeOrdered && !getParentRequest().distinctOnly && (!off_this.exi.isOrdered || !off_this.exi.isDistincted))
            {
                ddo = new ASTDDO(n.getLocation(), &n);

                // if we cache the node then cache upstreamed ddo instead
                if (off_this.isCached)
                {
                    n.setCached(false);
                    ddo->setCached(true);
                }

                modifyParent(ddo, false, false);

                off_this.exi.isOrdered = true;
                off_this.exi.isDistincted = true;
            }
            else if ((!isModeOrdered || getParentRequest().distinctOnly) && !off_this.exi.isDistincted)
            {
                ddo = new ASTDDO(n.getLocation(), &n, false);

                // if we cache the node then cache upstreamed ddo instead
                if (off_this.isCached)
                {
                    n.setCached(false);
                    ddo->setCached(true);
                }

                modifyParent(ddo, false, false);

                off_this.exi.isOrdered = false;
                off_this.exi.isDistincted = true;
            }
        }

        setOffer(off_this);
    }

    void LReturn::visit(ASTFor &n)
    {
        unsigned int count = (n.pv) ? 2 : 1;
        childOffer off_e, off_this, off_var;
        parentRequest req(getParentRequest());

        req.distinctOnly = false;
        setParentRequest(req);
        n.expr->accept(*this);
        off_e = getOffer();

        setParamMode();
        if (n.pv)
            n.pv->accept(*this);
        n.tv->accept(*this);
        unsetParamMode();

        // since for iterates over its sequence we should use default offer
        // but alleviate it with some expr-specific properties
        off_var = childOffer();
        off_var.exi.useConstructors = off_e.exi.useConstructors;
        off_var.isCached = off_e.isCached;
        off_var.usedVars = off_e.usedVars;

        bound_vars.back().exp_info = off_var.exi; // change offer for for-variable

        req.calledOnce = false;
        req.distinctOnly = getParentRequest().distinctOnly;
        setParentRequest(req);
        n.fd->accept(*this);
        off_this = getOffer();

        ignoreVariables(off_this, count);

        off_this.usedVars.insert(off_e.usedVars.begin(), off_e.usedVars.end());

        // consider to cache
        if (!getParentRequest().calledOnce)
        {
            cacheTheNode(&n, off_this);

            // if we cache this step then we don't need to cache the previous one
            if (off_this.isCached)
                n.fd->setCached(false);
            if (off_var.isCached)
                n.tv->setCached(false);
        }

        setOffer(off_this);
    }

    void LReturn::visit(ASTFunCall &n)
    {
        std::string int_name = CREATE_INTNAME(*n.uri, *n.local);
        unsigned int arity = (n.params) ? n.params->size() : 0;
        XQFunction xqf;
        childOffer off_this, off_params;
        bool standFunc = (*n.int_name != "");

        if (standFunc) // standard function
        {
            xqf = drv->getStdFuncInfo(int_name);
        }
        else
        {
            xqf = getFunctionInfo(int_name);
        }

        if (n.params)
        {
            parentRequest req;

            req.calledOnce = getParentRequest().calledOnce;

            for (unsigned int i = 0; i < arity; i++)
            {
                if (i < sizeof(param_mask))
                {
                    req.distinctOnly = (xqf.mask & (0x1 << i));
                }
                else if (standFunc)
                {
                    req.distinctOnly = (xqf.mask == maxParamMask);
                }
                else
                {
                    req.distinctOnly = isParamDistinctOnly(xqf.decl, i);
                }

                n.params->at(i)->accept(*this);
            }

            off_params = mergeOffers(arity);
        }

        off_this.exi = xqf.exp_info;
        off_this.usedVars = off_params.usedVars;

        if (!getParentRequest().calledOnce && xqf.toCache) // consider to cache
        {
            cacheTheNode(&n, off_this);
        }

        setOffer(off_this);
    }

    void LReturn::visit(ASTFuncDecl &n)
    {
    }

    void LReturn::visit(ASTGrantPriv &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTGrantRole &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTIf &n)
    {
        parentRequest req;
        childOffer off_this, off_if, off_t, off_e;

        req.distinctOnly = true; // for if-expression we need only distinct because of EBV
        req.calledOnce = getParentRequest().calledOnce;
        setParentRequest(req);
        n.i_expr->accept(*this);
        off_if = getOffer();

        req = getParentRequest();
        setParentRequest(req);
        n.t_expr->accept(*this);
        off_t = getOffer();

        setParentRequest(req);
        n.e_expr->accept(*this);
        off_e = getOffer();

        off_this.exi.isOrdered = off_t.exi.isOrdered && off_e.exi.isOrdered;
        off_this.exi.isDistincted = off_t.exi.isDistincted && off_e.exi.isDistincted;
        off_this.exi.isMax1 = off_t.exi.isMax1 && off_e.exi.isMax1;
        off_this.exi.isSingleLevel = off_t.exi.isSingleLevel && off_e.exi.isSingleLevel;
        off_this.exi.useConstructors = off_t.exi.useConstructors || off_e.exi.useConstructors;

        off_this.usedVars = off_if.usedVars;
        off_this.usedVars.insert(off_t.usedVars.begin(), off_t.usedVars.end());
        off_this.usedVars.insert(off_e.usedVars.begin(), off_e.usedVars.end());

        // consider to cache
        if (!getParentRequest().calledOnce)
        {
            cacheTheNode(&n, off_this);

            if (off_this.isCached)
            {
                if (off_if.isCached)
                    n.i_expr->setCached(false);
                if (off_t.isCached)
                    n.t_expr->setCached(false);
                if (off_e.isCached)
                    n.e_expr->setCached(false);
            }
        }

        setOffer(off_this);
    }

    void LReturn::visit(ASTInstOf &n)
    {
        parentRequest req(getParentRequest());
        childOffer off_this, eoff;

        req.distinctOnly = true;
        setParentRequest(req);
        n.expr->accept(*this);

        eoff = getOffer();

        // default offer makes perfect sense here since instance-of always returns atomic
        off_this.usedVars = eoff.usedVars;

        setOffer(off_this);
    }

    void LReturn::visit(ASTItemTest &n)
    {
        // nothing to do
        setOffer(childOffer());
    }

    void LReturn::visit(ASTLet &n)
    {
        childOffer off_e, off_this, off_var;
        parentRequest req(getParentRequest());
        bool isVarSeq = isVarSequence(dynamic_cast<ASTTypeVar *>(n.tv));

        req.distinctOnly = !isVarSeq; // if we wait for singleton then do just distinct
        setParentRequest(req);
        n.expr->accept(*this);
        off_e = getOffer();

        // if type annotation tells us it waits for singleton then we should respect this
        // else, var-body analysis will know better
        if (!isVarSeq)
        {
            off_var = childOffer();

            off_var.exi.useConstructors = off_e.exi.useConstructors;
            off_var.usedVars = off_e.usedVars;
            off_var.isCached = off_e.isCached;
        }
        else
        {
            off_var = off_e;
        }

        setParamMode();
        n.tv->accept(*this);
        unsetParamMode();

        bound_vars.back().exp_info = off_var.exi;

        req = getParentRequest();
        setParentRequest(req);
        n.fd->accept(*this);
        off_this = getOffer();

        ignoreVariables(off_this, 1);

        off_this.usedVars.insert(off_e.usedVars.begin(), off_e.usedVars.end());

        // consider to cache
        if (!getParentRequest().calledOnce)
        {
            cacheTheNode(&n, off_this);

            // if we cache this step then we don't need to cache the previous one
            if (off_this.isCached)
                n.fd->setCached(false);
            if (off_var.isCached)
                n.tv->setCached(false);
        }

        setOffer(off_this);
    }

    void LReturn::visit(ASTLibModule &n)
    {
        // nothing to do
        // we optimize library vars and functions on-demand from main module
    }

    void LReturn::visit(ASTLit &n)
    {
        setOffer(childOffer());
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
        {
            parentRequest req;

            req.distinctOnly = true;
            req.calledOnce = true;

            setParentRequest(req);
            n.coll->accept(*this);
        }
    }

    void LReturn::visit(ASTMetaSchemaCol &n)
    {
        parentRequest req;

        req.distinctOnly = true;
        req.calledOnce = true;

        setParentRequest(req);
        n.coll->accept(*this);
    }

    void LReturn::visit(ASTMetaSchemaDoc &n)
    {
        parentRequest req;

        req.distinctOnly = true;
        req.calledOnce = true;

        setParentRequest(req);
        n.doc->accept(*this);

        if (n.coll)
        {
            setParentRequest(req);
            n.coll->accept(*this);
        }
    }

    void LReturn::visit(ASTModImport &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTModuleDecl &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTNameTest &n)
    {
        // nothing to do
        setOffer(childOffer());
    }

    void LReturn::visit(ASTNamespaceDecl &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTNodeTest &n)
    {
        // nothing to do
        setOffer(childOffer());
    }

    void LReturn::visit(ASTNsp &n)
    {
        // nothing to do
        setOffer(childOffer());
    }

    void LReturn::visit(ASTOption &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTOrdExpr &n)
    {
        bool oldMode = isModeOrdered;
        parentRequest req(getParentRequest());

        if (n.type == ASTOrdExpr::ORDERED)
            isModeOrdered = true;
        else
            isModeOrdered = false;

        setParentRequest(req);
        n.expr->accept(*this);

        isModeOrdered = oldMode;
        setOffer(getOffer());
    }

    void LReturn::visit(ASTOrder &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTOrderBy &n)
    {
        unsigned int count = n.specs->size();
        childOffer off;
        parentRequest req;

        req.distinctOnly = true; // since there will be atomization and expected singleton
        req.calledOnce = false;
        VisitNodesVector(n.specs, *this, req);
        off = mergeOffers(count);

        // since result will be ignored by ASTOrderByRet except for usedVars and useConstructors parts,
        // which will be set with mergeOffers, so we don't set other params here

        setOffer(off);
    }

    void LReturn::visit(ASTOrderByRet &n)
    {
        parentRequest req;
        childOffer off_fl, off_ord, off_this;

        setParamMode();
        ASTVisitor::VisitNodesVector(n.vars, *this);
        unsetParamMode();

        req.distinctOnly = false;
        req.calledOnce = getParentRequest().calledOnce;
        setParentRequest(req);
        n.iter_expr->accept(*this);
        off_fl = getOffer();

        setParentRequest(req); // actually it will be ignored by ASTOrderBy
        n.ord_expr->accept(*this);
        off_ord = getOffer();

        req.distinctOnly = getParentRequest().distinctOnly;
        req.calledOnce = false;
        n.ret_expr->accept(*this);
        off_this = getOffer();

        // collect all used vars and unbind
        // NOTE: order is important here; we shouldn't ignore vars used by for-let clauses
        off_this.usedVars.insert(off_ord.usedVars.begin(), off_ord.usedVars.end());
        ignoreVariables(off_this, n.vars->size());
        off_this.usedVars.insert(off_fl.usedVars.begin(), off_fl.usedVars.end());

        off_this.exi.useConstructors = off_this.exi.useConstructors || off_fl.exi.useConstructors;

        // consider to cache
        if (!getParentRequest().calledOnce)
        {
            cacheTheNode(&n, off_this);

            if (off_this.isCached)
            {
                n.iter_expr->setCached(false);
                n.ord_expr->setCached(false);
                n.ret_expr->setCached(false);
            }
        }

        setOffer(off_this);
    }

    void LReturn::visit(ASTOrderEmpty &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTOrderMod &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTOrderModInt &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTOrderSpec &n)
    {
        childOffer off;

        setParentRequest(getParentRequest());
        n.expr->accept(*this);
        off = getOffer();

        // propagate cache if any
        if (off.isCached)
        {
            n.setCached(true);
            n.expr->setCached(false);
        }

        setOffer(off);
    }

    void LReturn::visit(ASTPIConst &n)
    {
    }

    void LReturn::visit(ASTPi &n)
    {
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
    }

    void LReturn::visit(ASTProlog &n)
    {
    }

    void LReturn::visit(ASTQName &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTQuantExpr &n)
    {
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
    }

    void LReturn::visit(ASTRevokeRole &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTSchemaAttrTest &n)
    {
    }

    void LReturn::visit(ASTSchemaElemTest &n)
    {
    }

    void LReturn::visit(ASTSeq &n)
    {
    }

    void LReturn::visit(ASTSpaceSeq &n)
    {
    }

    void LReturn::visit(ASTTextConst &n)
    {
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
    }

    void LReturn::visit(ASTTypeVar &n)
    {
        n.type->accept(*this);
        n.var->accept(*this);
    }

    void LReturn::visit(ASTUnio &n)
    {
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
    }

    void LReturn::visit(ASTUpdRename &n)
    {
        n.what->accept(*this);
    }

    void LReturn::visit(ASTUpdReplace &n)
    {
    }

    void LReturn::visit(ASTVar &n)
    {
    }

    void LReturn::visit(ASTVarDecl &n)
    {
    }

    void LReturn::visit(ASTVersionDecl &n)
    {
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

    bool LReturn::isOfferCorrect(const childOffer &off)
    {
        if (off.exi.isMax1)
        {
            if (!off.exi.isOrdered)
                return false;

            if (!off.exi.isDistincted)
                return false;

            if (!off.exi.isSingleLevel)
                return false;
        }

        return true;
    }

    void LReturn::setOffer(const childOffer &off)
    {
        U_ASSERT(isOfferCorrect(off));

        offers.push_back(off);
    }

    LReturn::childOffer LReturn::getOffer()
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
            setParentRequest(req);
            node = (*nodes)[i];
            node->accept(v);
        }
    }

    LReturn::childOffer LReturn::mergeOffers(unsigned int count)
    {
        childOffer res;

        // max1 only if we have one child and he is max1
        res.exi.isMax1 = (count == 0) || ((count == 1) && offers.back().exi.isMax1);
        res.exi.isOrdered = (count == 0) || ((count == 1) && offers.back().exi.isOrdered);
        res.exi.isSingleLevel = (count == 0) || ((count == 1) && offers.back().exi.isSingleLevel);
        res.exi.isDistincted = (count == 0) || ((count == 1) && offers.back().exi.isDistincted);

        while (count--)
        {
            childOffer c = offers.back();
            offers.pop_back();

            if (!c.usedVars.empty())
                res.usedVars.insert(c.usedVars.begin(), c.usedVars.end());

            if (c.exi.useConstructors)
                res.exi.useConstructors = true;
        }

        return res;
    }

    void LReturn::ignoreVariables(LReturn::childOffer &coff, unsigned int count)
    {
        for (unsigned int i = 0; i < count; i++)
        {
            coff.usedVars.erase(bound_vars.back().int_name);
            bound_vars.pop_back();
        }
    }

    const LReturn::parentRequest &LReturn::getParentRequest() const
    {
        return pareqs.back();
    }

    void LReturn::setParentRequest(const LReturn::parentRequest &preq)
    {
        parentReq = preq;
    }

    void LReturn::addToPath(ASTNode *nod)
    {
        ASTVisitor::addToPath(nod);
        pareqs.push_back(parentReq);
    }

    void LReturn::removeFromPath(ASTNode *nod)
    {
        ASTVisitor::removeFromPath(nod);
        pareqs.pop_back();
    }

    void LReturn::cacheTheNode(ASTNode *nod, LReturn::childOffer &off) const
    {
        if (!off.usedVars.empty() || off.exi.useConstructors)
            return;

        nod->setCached(true);
        off.isCached = true;
    }

    XQFunction LReturn::getFunctionInfo(const std::string &name)
    {
        XQFunction xqf;
        funcInfo::iterator it;

        // first, look in cache
        if (isModeOrdered)
        {
            it = funcOrdCache.find(name);

            if (it != funcOrdCache.end())
                return it->second;
        }
        else
        {
            it = funcUnordCache.find(name);

            if (it != funcUnordCache.end())
                return it->second;
        }

        // try to look in library functions cache
        it = funcLibCache.find(name);

        if (it != funcLibCache.end())
            return it->second;

        // then, try to process it as a local
        if (mod->getFunctionInfo(name, xqf))
        {
            xqf.decl->accept(*this);
            // now, function info is cached in ordered or unordered list
            if (isModeOrdered)
                return funcOrdCache[name];
            else
                return funcUnordCache[name];
        }

        // else, the function is defined in some of the modules
        xqf = drv->getLReturnFunctionInfo(name);

        // since we've obtained this info from driver we should locally cache it
        funcLibCache[name] = xqf;

        return xqf;
    }

    bool LReturn::isVarSequence(ASTTypeVar *var)
    {
        ASTTypeSeq *seq = dynamic_cast<ASTTypeSeq *>(var->type);

        if (seq && (seq->mod == ASTTypeSeq::ZERO_OR_MORE || seq->mod == ASTTypeSeq::ONE_OR_MORE))
            return true;

        return false;
    }

    bool LReturn::isParamDistinctOnly(const ASTFuncDecl *fd, unsigned int nparam)
    {
        ASTTypeVar *tv = dynamic_cast<ASTTypeVar *>(fd->params->at(nparam));

        U_ASSERT(tv);

        return !isVarSequence(tv);
    }
}
