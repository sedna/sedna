/*
 * File:  lreturn.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/lreturn/lreturn.h"
#include "common/errdbg/exceptions.h"
#include "tr/xqp/sema/Sema.h"

namespace sedna
{
    // reverse means that we must return contra-operation (e.g >= becomes <=, but = remains the same)
    static operation_compare_condition operToCondition(ASTBop::Oper op, bool reverse)
    {
        operation_compare_condition occ;

        switch (op)
        {
            case ASTBop::EQ_V:
                occ = OCC_VALUE_EQUAL;
                break;
            case ASTBop::NE_V:
                occ = OCC_VALUE_NOT_EQUAL;
                break;
            case ASTBop::GE_V:
                occ = reverse ? OCC_VALUE_LESS_EQUAL : OCC_VALUE_GREATER_EQUAL;
                break;
            case ASTBop::GT_V:
                occ = reverse ? OCC_VALUE_LESS : OCC_VALUE_GREATER;
                break;
            case ASTBop::LE_V:
                occ = reverse ? OCC_VALUE_GREATER_EQUAL : OCC_VALUE_LESS_EQUAL;
                break;
            case ASTBop::LT_V:
                occ = reverse ? OCC_VALUE_GREATER : OCC_VALUE_LESS;
                break;
            case ASTBop::EQ_G:
                occ = OCC_GENERAL_EQUAL;
                break;
            case ASTBop::NE_G:
                occ = OCC_GENERAL_NOT_EQUAL;
                break;
            case ASTBop::GE_G:
                occ = reverse ? OCC_GENERAL_LESS_EQUAL : OCC_GENERAL_GREATER_EQUAL;
                break;
            case ASTBop::GT_G:
                occ = reverse ? OCC_GENERAL_LESS : OCC_GENERAL_GREATER;
                break;
            case ASTBop::LE_G:
                occ = reverse ? OCC_GENERAL_GREATER_EQUAL : OCC_GENERAL_LESS_EQUAL;
                break;
            case ASTBop::LT_G:
                occ = reverse ? OCC_GENERAL_GREATER : OCC_GENERAL_LESS;
                break;
            default:
                throw SYSTEM_EXCEPTION("Unexpected compare condition");
        }

        return occ;
    }

    void LReturn::visit(ASTAlterUser &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTAttr &n)
    {
        size_t count = (n.cont) ? n.cont->size() : 0;
        childOffer off;

        if (n.cont)
        {
            parentRequest req(getParentRequest());

            req.distinctOnly = false;
            req.deep_copy = true;
            req.atomize = true;
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

        n.deep_copy = getParentRequest().deep_copy;

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
            req.deep_copy = true;
            req.atomize = false;
            setParentRequest(req);
            n.name->accept(*this);
            count++;
        }

        if (n.expr)
        {
            parentRequest req(getParentRequest());

            req.distinctOnly = false;
            req.atomize = true;
            req.deep_copy = true;
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

        n.deep_copy = getParentRequest().deep_copy;

        setOffer(off);
    }

    void LReturn::visit(ASTAttribTest &n)
    {
        if (param_mode)
            bound_vars.back().isNodes = true;
    }

    void LReturn::visit(ASTAxisStep &n)
    {
        childOffer off_cont; // by default we work with some context we don't know about (maybe need to refine this later)
        childOffer off_this;
        childOffer off_preds;
        bool trying_abs_path = false; // trying to add this axis to abs-path

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
        if ((n.axis == ASTAxisStep::CHILD || n.axis == ASTAxisStep::SELF || n.axis == ASTAxisStep::ATTRIBUTE) && !n.preds)
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

            // we propagate distinctOnly here since neither axis-test (obviously) nor predicate (see inner-focus) cannot see
            // ordering from the previous step and so cannot use it
            req.deep_copy = true;
            req.atomize = false;
            setParentRequest(req);
            n.cont->accept(*this);
            off_this = off_cont = getOffer();

            off_this.isCached = false;

            // it seems that we need only to distinct, not order, intermediate results since we can order them at the last step
            // exception: self axis is a filter itself, so we don't want to put distinct below it
            // it is commented for now: we should reason about distict-on-each-step later though
            // one pro we could consider: distinct-on-each-step allows us to shrink the sequence, which makes following steps
            // less consuming
/*            if (!off_cont.exi.isDistincted && n.axis != ASTAxisStep::SELF)
            {
                n.cont = new ASTDDO(n.getLocation(), n.cont, true);
                off_cont.exi.isDistincted = true;
                off_cont.exi.isOrdered = false;
            }*/

            trying_abs_path = off_cont.in_abs_path;
        }
        else
        {
            // if we've got axis without context then we use some outer context -- report this
            off_this.usedVars.insert("$%v");

            // if we haven't got the context then ask parent if we need abs-path
            trying_abs_path = getParentRequest().start_abspath;
        }

        // check if we can add this axis to abs-path
        if (trying_abs_path && !n.isSuitableForAbsPath())
            trying_abs_path = false;

        // now we need to write our initial offer to parent
        switch (n.axis)
        {
            case ASTAxisStep::CHILD:
                off_this.exi.isOrdered = off_cont.exi.isOrdered && off_cont.exi.isSingleLevel;
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
                off_this.exi.isOrdered = off_cont.exi.isOrdered && off_cont.exi.isSingleLevel;
                off_this.exi.isDistincted = off_cont.exi.isDistincted;
                off_this.exi.isMax1 = false; // TODO: can refine it later (check if this is the named-attribute retrieval. then it would be true)
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
        if (!trying_abs_path && !getParentRequest().calledOnce)
        {
            cacheTheNode(&n, off_this);

            // if we cache this step then we don't need to cache the previous one
            if (off_this.isCached && n.cont && off_cont.isCached)
                n.cont->setCached(false);
        }

        // abs-path ddoed by its design
        if (trying_abs_path)
        {
            off_this.exi.isOrdered = true;
            off_this.exi.isDistincted = true;
            off_this.in_abs_path = true;
        }
        else
        {
            off_this.in_abs_path = false;
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

        // axis step never uses position or last
        // except in predicates of course, but these would be not the same positions
        off_this.use_last = false;
        off_this.use_position = false;

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

        req.atomize = false;
        req.deep_copy = true;

        setParentRequest(req);
        n.lop->accept(*this);
        lof = getOffer();

        setParentRequest(req);
        n.rop->accept(*this);
        rof = getOffer();

        bopof.exi.isMax1 = (n.op == ASTBop::TO) ? false : true;

        bopof.usedVars.insert(lof.usedVars.begin(), lof.usedVars.end());
        bopof.usedVars.insert(rof.usedVars.begin(), rof.usedVars.end());

        bopof.use_last = lof.use_last || rof.use_last;
        bopof.use_position = lof.use_position || rof.use_position;

        if (n.op >= ASTBop::UNION && n.op <= ASTBop::EXCEPT)
        {
            bool leftSorted = lof.exi.isMax1 || lof.exi.isDistincted;
            bool rightSorted = rof.exi.isMax1 || rof.exi.isDistincted;

            bool left_ddo = lof.exi.isOrdered && lof.exi.isDistincted;
            bool right_ddo = rof.exi.isOrdered && rof.exi.isDistincted;

            /* In case the result of the operation contains more nodes than
             * children, order them at child level */
            bool moveDDOtoChildren = false;

            bopof.exi.useConstructors = lof.exi.useConstructors || rof.exi.useConstructors;

            switch (n.op)
            {
                case ASTBop::INTERSECT:
                    moveDDOtoChildren = false;
                    bopof.exi.isMax1 = lof.exi.isMax1 || rof.exi.isMax1;
                    break;

                case ASTBop::EXCEPT:
                    moveDDOtoChildren = false;
                    bopof.exi.isMax1 = lof.exi.isMax1;
                    break;

                case ASTBop::UNION:
                    moveDDOtoChildren = true;
                    bopof.exi.isMax1 = false;
                    break;

                default:
                    bopof.exi.isMax1 = true;
            }

            bopof.exi.isSingleLevel = (lof.exi.isSingleLevel && rof.exi.isSingleLevel) || bopof.exi.isMax1;

            /* We assume that both operations return distinctOnly result by their nature,
              because they actually want AT LEAST distincted result */

            bopof.exi.isDistincted = true;

            bool docOrderNeeded = isModeOrdered && !getParentRequest().distinctOnly;

            /* Both children should be eather in DDO or not together,
             * so if one child is in DDO, propagade another to DDO too
             * (instead of loosing sort order) */

            n.doc_order = left_ddo || right_ddo ||
                (docOrderNeeded && moveDDOtoChildren);

            leftSorted = (!n.doc_order && leftSorted) ||
              left_ddo || lof.exi.isMax1;

            rightSorted = (!n.doc_order && rightSorted) ||
              right_ddo || rof.exi.isMax1;

            if (!leftSorted) {
                n.lop = new ASTDDO(n.lop->getLocation(), n.lop, n.doc_order);
            }

            if (!rightSorted) {
                n.rop = new ASTDDO(n.rop->getLocation(), n.rop, n.doc_order);
            }

            bopof.exi.isOrdered = bopof.exi.isMax1 || n.doc_order;

            if (!bopof.exi.isOrdered && docOrderNeeded) {
                ddo = new ASTDDO(n.getLocation(), &n);
                modifyParent(ddo, false, false);
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
            if (n.type)
                n.type->accept(*this);
            unsetParamMode();

            // if we've got no type annotation or it isn't too precise use typeswitch analysis instead
            if (!n.type || (!bound_vars.back().exp_info.isMax1 && (bound_vars.back().isNodes || ts_off.exi.isMax1)))
                bound_vars.back().exp_info = ts_off.exi;
            else
                bound_vars.back().exp_info.useConstructors = ts_off.exi.useConstructors;
        }

        req.atomize = false;
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
        req.atomize = false;
        req.deep_copy = true;
        setParentRequest(req);
        n.expr->accept(*this);

        eoff = getOffer();

        // default offer makes perfect sense here since cast always returns atomic
        off_this.usedVars = eoff.usedVars;
        off_this.use_last = eoff.use_last;
        off_this.use_position = eoff.use_position;

        setOffer(off_this);
    }

    void LReturn::visit(ASTCastable &n)
    {
        parentRequest req(getParentRequest());
        childOffer off_this, eoff;

        req.atomize = false;
        req.deep_copy = true;
        req.distinctOnly = true; // castable always returns false for 1+ sequencies
        setParentRequest(req);
        n.expr->accept(*this);

        eoff = getOffer();

        // default offer makes perfect sense here since castable always returns atomic
        off_this.usedVars = eoff.usedVars;
        off_this.use_last = eoff.use_last;
        off_this.use_position = eoff.use_position;

        setOffer(off_this);
    }

    void LReturn::visit(ASTCharCont &n)
    {
        // text node
        setOffer(childOffer());
    }

    void LReturn::visit(ASTCommTest &n)
    {
        if (param_mode)
            bound_vars.back().isNodes = true;
    }

    void LReturn::visit(ASTCommentConst &n)
    {
        parentRequest req(getParentRequest());
        childOffer off;

        req.distinctOnly = false;
        req.atomize = true;
        req.deep_copy = true;
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

        n.deep_copy = getParentRequest().deep_copy;

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
        // name cannot be computed
        // cust_expr contains only constants
        // type is a constant
        parentRequest req;

        req.calledOnce = true;
        req.distinctOnly = false;

        setParentRequest(req);
        n.path->accept(*this);
    }

    void LReturn::visit(ASTCreateIndex &n)
    {
        // name cannot be computed
        // type is a constant
        parentRequest req;

        req.calledOnce = true;
        req.distinctOnly = false;

        setParentRequest(req);
        n.on_path->accept(*this);

        req.start_abspath = true;
        setParentRequest(req);
        n.by_path->accept(*this);
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

        setParentRequest(req);
        n.path->accept(*this);

        // add special trigger variables
        // they all represent one const-node so the default offer will suit them
        bound_vars.push_back(XQVariable("NEW", NULL));
        bound_vars.push_back(XQVariable("OLD", NULL));
        bound_vars.push_back(XQVariable("WHERE", NULL));

        VisitNodesVector(n.do_exprs, *this, req);

        bound_vars.pop_back();
        bound_vars.pop_back();
        bound_vars.pop_back();
    }

    void LReturn::visit(ASTCreateUser &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTDDO &n)
    {
        // in fact we should not get here since all ddo-ops are inserted in dynamic mode without lreturn visitor taking them (we should not do re-analysis either)
        // to get rif of re-analysis we'll assume that we've already been under ddo and just set some more or less appropriate offer
        childOffer off;

        off.exi.isOrdered = n.true_ddo;
        off.exi.isDistincted = true;
        off.exi.isMax1 = false;
        off.exi.isSingleLevel = false;
        off.exi.useConstructors = true;
        off.isCached = n.isCached();

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
        req.deep_copy = false; // attach nodes to the document
        req.atomize = false;
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
        if (param_mode)
            bound_vars.back().isNodes = true;
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
        childOffer off;

        if (n.cont)
        {
            parentRequest req(getParentRequest());

            req.distinctOnly = false;
            req.deep_copy = false; // attach nodes to this element directly
            req.atomize = false;

            VisitNodesVector(n.cont, *this, req);
            off = mergeOffers(n.cont->size());
        }

        // direct element constructor creates only one node
        off.exi.isOrdered = true;
        off.exi.isDistincted = true;
        off.exi.isSingleLevel = true;
        off.exi.isMax1 = true;
        off.isCached = false;
        off.exi.useConstructors = true;

        n.deep_copy = getParentRequest().deep_copy;

        if (n.attrs)
        {
            parentRequest req;
            childOffer offa;

            req.calledOnce = getParentRequest().calledOnce;
            req.deep_copy = false;

            VisitNodesVector(n.attrs, *this, req);
            offa = mergeOffers(n.attrs->size());

            off.usedVars.insert(offa.usedVars.begin(), offa.usedVars.end());
        }

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
            req.deep_copy = true;
            req.atomize = false;
            setParentRequest(req);
            n.name->accept(*this);
            count++;
        }

        if (n.expr)
        {
            parentRequest req(getParentRequest());

            req.distinctOnly = false;
            req.deep_copy = false; // attach nodes to this element directly
            req.atomize = false;
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

        n.deep_copy = getParentRequest().deep_copy;

        setOffer(off);
    }

    void LReturn::visit(ASTElementTest &n)
    {
        if (param_mode)
            bound_vars.back().isNodes = true;
    }

    void LReturn::visit(ASTEmptyTest &n)
    {
        if (param_mode)
            bound_vars.back().isNodes = false;
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
        bool trying_abs_path = false; // true, if we try to merge filter step into abs-path

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
            parentRequest req;

            // bind the context
            bound_vars.push_back(XQVariable("$%v", NULL));

            req.distinctOnly = getParentRequest().distinctOnly;
            req.calledOnce = false;
            setParentRequest(req);
            n.expr->accept(*this);
            off_this = off_pe = getOffer();

            off_this.isCached = false;

            //Remove context from the list of bound variables
            bound_vars.pop_back();

            // check if we use last and/or position in expression (needed for qep generation)
            n.use_last = off_pe.use_last;
            n.use_pos = off_pe.use_position;
        }

        // context
        if (n.cont)
        {
            parentRequest req(getParentRequest());

            // we propagate distinctOnly here iff the primary expression doesn't use the context of the previous step
            // if it does, then it could expose absence of mandatory ordering using position()
            req.distinctOnly = !n.use_pos && off_pe.usedVars.find("$%v") == off_pe.usedVars.end();
            req.atomize = false;
            req.deep_copy = true;
            setParentRequest(req);
            n.cont->accept(*this);
            off_cont = getOffer();

            // if we have context, we use its position and last, not global one (ex: .[a/position()])
            off_this.use_last = false;
            off_this.use_position = false;

            // if we have context item expression then the result will be dictated by the previous step
            if (!n.expr)
            {
                off_this = off_pe = off_cont;
                off_this.isCached = false;
                off_pe.usedVars.insert("$%v"); // since . is a context
                trying_abs_path = off_cont.in_abs_path && !n.preds; // since . without preds cannot destroy abspath
            }
            else
            {
                off_this.usedVars.insert(off_cont.usedVars.begin(), off_cont.usedVars.end());
            }
        }

        // set usedvars properly
        off_this.usedVars.insert(off_preds.usedVars.begin(), off_preds.usedVars.end());

        // if we've got single context step, that means using some outer context, which we should report of
        if (!n.cont && !n.expr)
        {
            off_this.usedVars.insert("$%v");
            trying_abs_path = getParentRequest().start_abspath && !n.preds;
        }

        // if primary expression will be called several times then we must change the offer
        if (n.expr && !off_cont.exi.isMax1)
        {
            off_this.exi.isDistincted = false;
            off_this.exi.isMax1 = false;
            off_this.exi.isOrdered = false;
            off_this.exi.isSingleLevel = false;
        }

        // now we've analyzed context; if it emanates <=1 nodes then we should disable caching of primary expression
        if (n.expr && off_cont.exi.isMax1)
            n.expr->setCached(false);

        // now we need to decide if we want to cache it
        if (!trying_abs_path && !getParentRequest().calledOnce)
        {
            cacheTheNode(&n, off_this);

            // if we cache this step then we don't need to cache the previous one
            if (off_this.isCached && n.cont && off_cont.isCached)
                n.cont->setCached(false);
        }

        // if we have E1/E2 path expression and we're in E2 we don't need to order-distinct previous step if it's first filter step
        // of course, we always consider axis step for ddo-distinct
        ASTFilterStep *pfs = dynamic_cast<ASTFilterStep *>(n.cont);
        if ((pfs && !pfs->isFirstStep()) || !pfs)
        {
            // if we are in ordered mode and we use context nodes then we should ddo the previous step
            if (n.cont && isModeOrdered && (!off_cont.exi.isOrdered || !off_cont.exi.isDistincted) &&
                    (off_pe.usedVars.find("$%v") != off_pe.usedVars.end() || n.use_pos))
            {
                ASTNode *ddo = new ASTDDO(n.getLocation(), n.cont);

                // if we've cached the previous step then move cache to the ddo
                if (off_cont.isCached)
                {
                    ddo->setCached(true);
                    n.cont->setCached(false);
                }

                // context expression now becomes ddoed
                if (!n.expr)
                {
                    off_this.exi.isDistincted = true;
                    off_this.exi.isOrdered = true;
                }

                n.cont = ddo;
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

                // context expression now becomes distincted
                if (!n.expr)
                {
                    off_this.exi.isDistincted = true;
                    off_this.exi.isOrdered = false;
                }

                n.cont = ddo;
            }
        }

        // abs-path ddoed by its design
        if (trying_abs_path)
        {
            off_this.exi.isOrdered = true;
            off_this.exi.isDistincted = true;
            off_this.in_abs_path = true;
        }
        else
        {
            off_this.in_abs_path = false;
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

        // filter steps usually don't participate in abs-paths; execpt for context-expression
        setOffer(off_this);
    }

    void LReturn::visit(ASTFLWOR &n)
    {
        parentRequest req;
        ASTNodesVector::iterator it;
        childOffer off_this, off;
        size_t var_count;

        var_count = bound_vars.size();

        req.calledOnce = getParentRequest().calledOnce;
        req.distinctOnly = false;

        n.fls->at(0)->accept(*this);

        req.calledOnce = false; // subsequent for-lets will be called more than once generally
        for (it = n.fls->begin() + 1; it != n.fls->end(); it++)
        {
            (*it)->accept(*this);
        }

        if (n.where)
        {
            req.calledOnce = false;
            req.distinctOnly = true;

            n.where->accept(*this);
        }

        if (n.order_by)
        {
            setParentRequest(req); // actually it will be ignored by ASTOrderBy
            n.order_by->accept(*this);
        }

        req.calledOnce = false;
        req.distinctOnly = getParentRequest().distinctOnly;
        req.atomize = false;
        req.deep_copy = getParentRequest().deep_copy;
        setParentRequest(req);

        n.ret->accept(*this);
        off_this = getOffer();

        // refine the offer
        if (n.order_by)
        {
            off = getOffer();
            off_this.usedVars.insert(off.usedVars.begin(), off.usedVars.end());

            if (off.use_last)
                off_this.use_last = true;

            if (off.use_position)
                off_this.use_position = true;
        }

        if (n.where)
        {
            off = getOffer();
            off_this.usedVars.insert(off.usedVars.begin(), off.usedVars.end());

            if (off.use_last)
                off_this.use_last = true;

            if (off.use_position)
                off_this.use_position = true;
        }

        bool resIsMax1 = true; // we iterate several times over "return"?

        for (size_t i = n.fls->size(); i >= 1; i--)
        {
            size_t idx = i - 1;
            const ASTFor *f = dynamic_cast<const ASTFor *>(n.fls->at(idx));

            off = getOffer();

            // ignore bound vars from "return" expression,
            ignoreVariables(off_this, (f && f->usesPosVar()) ? 2 : 1);

            // but add vars from binding expression
            off_this.usedVars.insert(off.usedVars.begin(), off.usedVars.end());

            if (!off.exi.isMax1)
                resIsMax1 = false;

            if (off.use_last)
                off_this.use_last = true;

            if (off.use_position)
                off_this.use_position = true;

            // consider to cache for-let based on information from off_this
            // (since it accumulates lower and current for-lets, where, order-by and return by now)
            if (idx > 0)
            {
                cacheTheNode(n.fls->at(idx), off_this);
            }
        }

        // consider to cache order-by (similar to for-let)
        if (n.order_by && !getParentRequest().calledOnce)
        {
            cacheTheNode(n.order_by, off_this);
        }

        // if we've got 'order by' or the result is made in several iterations alter our offer
        if (!resIsMax1 || n.order_by)
        {
            off_this.exi.isOrdered = off_this.exi.isMax1;
            off_this.exi.isSingleLevel = off_this.exi.isMax1;
            off_this.exi.isDistincted = false;
            off_this.exi.isMax1 = false;
        }

        U_ASSERT(var_count == bound_vars.size());

        // consider to cache
        if (!getParentRequest().calledOnce)
        {
            cacheTheNode(&n, off_this);

            if (off_this.isCached)
            {
                n.ret->setCached(false);
            }
        }

        setOffer(off_this);
    }

    void LReturn::visit(ASTFor &n)
    {
        childOffer off_e, off_this, off_var;
        parentRequest req(getParentRequest());

        // we should ignore distinct only since binding expression doesn't emit the result
        req.distinctOnly = false;
        req.atomize = false;
        req.deep_copy = true;
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

        bound_vars.back().exp_info = off_var.exi; // change offer for for-variable

        setOffer(off_e);
    }

    void LReturn::visit(ASTFunCall &n)
    {
        std::string int_name = CREATE_INTNAME(*n.uri, *n.local);
        size_t arity = (n.params) ? n.params->size() : 0;
        XQFunction *xqf;
        childOffer off_this, off_params;
        bool standFunc = (*n.int_name != "");
        ASTNode *ddo = NULL;

        int_name += "/" + int2string(arity);

        if (standFunc) // standard function
        {
            // for standard function get name wo/a
            xqf = drv->getStdFuncInfo(CREATE_INTNAME(*n.uri, *n.local));
        }
        else
        {
            xqf = getFunctionInfo(int_name);

            // we're using this function
            xqf->is_used = true;

            if (xqf->mod)
                xqf->mod->set_module_as_needed();

        }

        if (n.params)
        {
            parentRequest req;

            req.calledOnce = getParentRequest().calledOnce;

            for (unsigned int i = 0; i < arity; i++)
            {
                if (i < sizeof(param_mask))
                {
                    req.distinctOnly = ((xqf->mask & (0x1 << i)) != 0);
                }
                else if (standFunc)
                {
                    req.distinctOnly = (xqf->mask == maxParamMask);
                }
                else
                {
                    xqExprInfo exi;

                    setParamMode();
                    (*xqf->decl->params)[i]->accept(*this);
                    unsetParamMode();

                    exi = bound_vars.back().exp_info;
                    bound_vars.pop_back();

                    req.distinctOnly = exi.isMax1;
                }

                setParentRequest(req);
                (*n.params)[i]->accept(*this);
            }
        }

        // for standard functions result analysis is based on params
        if (standFunc)
        {
            // first create params vector
            std::vector<xqExprInfo> params(arity);
            childOffer off;

            for (unsigned i = 0; i < arity; i++)
            {
                off = getOffer();
                params[arity - i - 1] = off.exi;
                off_this.usedVars.insert(off.usedVars.begin(), off.usedVars.end());

                if (off.use_last)
                    off_this.use_last = true;
                if (off.use_position)
                    off_this.use_position = true;
            }

            // call merger to analyse result
            off_this.exi = xqf->merger(params);

            // for index-scan functions we need custom ddo
            if (*n.int_name == "!fn!index-scan" || *n.int_name == "!fn!index-scan-between")
            {
                if (!getParentRequest().distinctOnly && isModeOrdered && !dynamic_cast<ASTStep *>(getParent()))
                {
                    ddo = new ASTDDO(n.getLocation(), &n);
                    modifyParent(ddo, false, false);

                    off_this.exi.isDistincted = true;
                    off_this.exi.isOrdered = true;
                }
            }
            else if (*n.int_name == "!fn!unordered")
            {
                // if we've got final ddo, convert it to distinct
                if (ASTDDO *param = dynamic_cast<ASTDDO *>(n.params->at(0)))
                {
                    param->true_ddo = false;

                    off_this.exi.isDistincted = true;
                    off_this.exi.isOrdered = false;
                }

                // we need to get rid of this fun-call
                modifyParent(n.params->at(0), false, true);
                n.params->clear();
            }
            else if (*n.int_name == "!fn!last")
            {
                off_this.usedVars.clear();
                off_this.use_last = true;
            }
            else if (*n.int_name == "!fn!position")
            {
                off_this.usedVars.clear();
                off_this.use_position = true;
            }
            else if (*n.int_name == "!fn!collection" || (*n.int_name == "!fn!document" && n.params->size() == 1))
            {
                off_this.in_abs_path = true; // start analyze abs-path possibility
            }
        }
        else
        {
            off_params = mergeOffers(arity);
            off_this.exi = xqf->exp_info;
            off_this.usedVars = off_params.usedVars;
            off_this.use_last = off_params.use_last;
            off_this.use_position = off_params.use_position;
        }

        if (!getParentRequest().calledOnce && xqf->toCache) // consider to cache
        {
            cacheTheNode(&n, off_this);

            if (off_this.isCached)
            {
                for (unsigned int i = 0; i < arity; i++)
                {
                    n.params->at(i)->setCached(false);
                }

                if (ddo)
                {
                    ddo->setCached(true);
                    n.setCached(false);
                }
            }
        }

        setOffer(off_this);
    }

    void LReturn::visit(ASTFuncDecl &n)
    {
        size_t arity = (n.params) ? n.params->size() : 0;
        childOffer off_this, boff;
        std::string name = CREATE_INTNAME(*n.func_uri, *n.local);

        name += "/" + int2string(arity);

        // first, we should bind params
        if (n.params)
        {
            setParamMode();
            for (unsigned int i = 0; i < arity; i++)
            {
                // we don't need parent requests for param-mode
                (*n.params)[i]->accept(*this);
            }
            unsetParamMode();
        }

        // then, analyze return type
        setParamMode();
        bound_vars.push_back(XQVariable("$%r", NULL)); // dummy variable to keep type-analysis for return
        n.ret->accept(*this);
        unsetParamMode();

        // body can contain recursive references
        // so we should set pre-offer based strictly on type analysis
        XQVariable v = bound_vars.back();
        funcCache[name]->exp_info = v.exp_info;
        bound_vars.pop_back();

        // then optimize body
        if (n.body)
        {
            parentRequest req;

            req.distinctOnly = false;
            req.calledOnce = true;

            setParentRequest(req);
            n.body->accept(*this);
            boff = getOffer();
        }

        // then, generate offer
        // for user-defined functions, for which type says node-sequence, use body analysis instead
        if (n.body && !v.exp_info.isMax1 && (v.isNodes || boff.exi.isMax1))
        {
            funcCache[name]->exp_info = boff.exi;
        }

        // get rid of params
        while (arity--)
            bound_vars.pop_back();
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
        req.deep_copy = true;
        req.atomize = false;
        setParentRequest(req);
        n.i_expr->accept(*this);
        off_if = getOffer();

        req = getParentRequest();
        req.atomize = false;
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

        off_this.use_last = off_if.use_last || off_t.use_last || off_e.use_last;
        off_this.use_position = off_if.use_position || off_t.use_position || off_e.use_position;

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
        req.deep_copy = true;
        req.atomize = false;
        setParentRequest(req);
        n.expr->accept(*this);

        eoff = getOffer();

        // default offer makes perfect sense here since instance-of always returns atomic
        off_this.usedVars = eoff.usedVars;
        off_this.use_last = eoff.use_last;
        off_this.use_position = eoff.use_position;

        setOffer(off_this);
    }

    void LReturn::visit(ASTItemTest &n)
    {
        if (param_mode)
            bound_vars.back().isNodes = true;
    }

    void LReturn::visit(ASTLet &n)
    {
        childOffer off_e, off_this, off_var;
        parentRequest req(getParentRequest());

        setParamMode();
        n.tv->accept(*this);
        unsetParamMode();

        // for now we should remove our variable from bound since the expression must not see it
        XQVariable var = bound_vars.back();
        bound_vars.pop_back();

        req.distinctOnly = getParentRequest().distinctOnly || var.exp_info.isMax1; // if we wait for singleton then do just distinct
        req.deep_copy = true;
        req.atomize = false;
        setParentRequest(req);
        n.expr->accept(*this);
        off_e = getOffer();

        // if type annotation tells us it waits for singleton then we should respect this
        // else, var-body analysis will know better
        if (var.exp_info.isMax1 || (!var.isNodes && !off_e.exi.isMax1))
        {
            off_var.exi = var.exp_info;
            off_var.exi.useConstructors = off_e.exi.useConstructors;
            off_var.isCached = off_e.isCached;
        }
        else
        {
            off_var = off_e;
        }

        // now, bind variable
        var.exp_info = off_var.exi;
        bound_vars.push_back(var);

        setOffer(off_e);
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

        // we don't need to analyze prolog here
        // funcs and vars are processed on-demand
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
        // we should't have got here anyway
        U_ASSERT(false);
    }

    void LReturn::visit(ASTNamespaceDecl &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTNodeTest &n)
    {
        if (param_mode)
            bound_vars.back().isNodes = true;
    }

    void LReturn::visit(ASTNsp &n)
    {
        // nothing to do
        childOffer off;
        setOffer(off);
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
        size_t count = n.specs->size();
        childOffer off;
        parentRequest req;

        req.distinctOnly = true; // since there will be atomization and expected singleton
        req.calledOnce = false;
        VisitNodesVector(n.specs, *this, req);
        off = mergeOffers(count);

        // since result will be ignored by ASTFLWOR except for usedVars and useConstructors parts,
        // which will be set with mergeOffers, so we don't set other params here

        setOffer(off);
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
        parentRequest req(getParentRequest());

        req.deep_copy = true;
        req.atomize = false;
        setParentRequest(req);

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
        childOffer off;
        unsigned int count = 0;

        if (n.name)
        {
            parentRequest req(getParentRequest());

            req.deep_copy = true;
            req.distinctOnly = true;
            req.atomize = false;
            setParentRequest(req);
            n.name->accept(*this);
            count++;
        }

        if (n.expr)
        {
            parentRequest req(getParentRequest());

            req.distinctOnly = false;
            req.deep_copy = true;
            req.atomize = true;
            setParentRequest(req);
            n.expr->accept(*this);
            count++;
        }

        off = mergeOffers(count);

        // pi constructor creates only one node
        off.exi.isOrdered = true;
        off.exi.isDistincted = true;
        off.exi.isSingleLevel = true;
        off.exi.isMax1 = true;
        off.isCached = false;
        off.exi.useConstructors = true;

        n.deep_copy = getParentRequest().deep_copy;

        setOffer(off);
    }

    void LReturn::visit(ASTPi &n)
    {
        childOffer off;

        // direct pi constructor creates only one node
        off.exi.isOrdered = true;
        off.exi.isDistincted = true;
        off.exi.isSingleLevel = true;
        off.exi.isMax1 = true;
        off.isCached = false;
        off.exi.useConstructors = true;

        n.deep_copy = getParentRequest().deep_copy;

        setOffer(off);
    }

    void LReturn::visit(ASTPiTest &n)
    {
        if (param_mode)
            bound_vars.back().isNodes = true;
    }

    void LReturn::visit(ASTPosVar &n)
    {
        if (param_mode)
        {
            n.var->accept(*this);

            bound_vars.back().exp_info = childOffer().exi;
            bound_vars.back().isNodes = false;
        }
    }

    void LReturn::visit(ASTPragma &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTPred &n)
    {
        parentRequest req;
        std::vector<ASTPred::ASTConjunct>::iterator it;
        childOffer off, off_this;
        std::vector<ASTPred::ASTConjunct> local_others;
        ASTPred::ASTConjunct others_and;

        req.calledOnce = false;
        req.distinctOnly = true;

        others_and.expr = NULL;
        others_and.use_cxt = false;
        others_and.use_pos = false;
        others_and.use_last = false;

        // here we mark out the "true" conjuncts (w/o pos and context usage)
        // all other conjuncts are stitched together using logical and
        for (it = n.others.begin(); it != n.others.end(); it++)
        {
            // check if expression is a candidate for conjunct
            // Note! it doesn't actually analyze properly children dependency on context!
            ASTNode *cand = checkIfPosConjunct(it->expr);

            setParentRequest(req);

            if (cand)
                cand->accept(*this);
            else
                it->expr->accept(*this);

            off = getOffer();

            it->use_last = off.use_last;
            it->use_pos = off.use_position;
            it->use_cxt = (off.usedVars.find("$%v") != off.usedVars.end());

            off_this.usedVars.insert(off.usedVars.begin(), off.usedVars.end());

            if (cand && !it->use_pos && !it->use_cxt) // "true" positional conjunct
            {
                ASTBop *bop = dynamic_cast<ASTBop *>(it->expr);

                it->expr = cand;
                it->op = operToCondition(bop->op, cand == bop->lop);
                n.conjuncts.push_back(*it);

                // get rid of ASTBop
                bop->modifyChild(cand, NULL);
                delete bop;
            }
            else
            {
                // See bug #3328520
                // if candidate failed, the whole predicate use position().
                // for example: element/element[position() = (1 to ./test/element)] - in this
                // case checkIfPosConjunct returns non null (since it doesn't check context
                // dependency), but candidate failed since it->use_cxt will be true.
                it->use_pos = cand || it->use_pos;

                if (!others_and.expr)
                {
                    others_and = *it;
                }
                else
                {
                    others_and.expr = new ASTBop(it->expr->getLocation(), ASTBop::AND, others_and.expr, it->expr);

                    if (it->use_cxt)
                        others_and.use_cxt = true;

                    if (it->use_pos)
                        others_and.use_pos = true;

                    if (it->use_last)
                        others_and.use_last = true;
                }
            }
        }

        n.others.clear();

        if (others_and.expr)
            n.others.push_back(others_and);

        setOffer(off_this);
    }

    void LReturn::visit(ASTProlog &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTQName &n)
    {
        setOffer(childOffer());
    }

    void LReturn::visit(ASTQuantExpr &n)
    {
        childOffer off_var, off_e, off_sat, off_this;
        parentRequest req;

        req.distinctOnly = false;
        req.calledOnce = getParentRequest().calledOnce;
        setParentRequest(req);
        n.expr->accept(*this);
        off_e = getOffer();

        setParamMode();
        n.var->accept(*this);
        unsetParamMode();

        // since quant iterates over the expression we should use default offer
        // but alleviate it with some expr-specific properties
        off_var = childOffer();
        off_var.exi.useConstructors = off_e.exi.useConstructors;
        off_var.isCached = off_e.isCached;

        bound_vars.back().exp_info = off_var.exi; // change offer for quant-variable

        // now look into 'satisfy'-expression
        req.distinctOnly = true; // since we need only EBV from test expression
        req.calledOnce = false;
        setParentRequest(req);
        n.sat->accept(*this);
        off_sat = getOffer();

        off_this = childOffer(); // default offer will do since this expression results in boolean
        off_this.usedVars = off_sat.usedVars;
        ignoreVariables(off_this, 1); // ignore variable
        off_this.usedVars.insert(off_e.usedVars.begin(), off_e.usedVars.end());
        off_this.use_last = off_e.use_last || off_sat.use_last;
        off_this.use_position = off_e.use_position || off_sat.use_position;

        // consider to cache
        if (!getParentRequest().calledOnce)
        {
            cacheTheNode(&n, off_this);

            if (off_this.isCached)
                n.sat->setCached(false);
        }

        setOffer(off_this);
    }

    void LReturn::visit(ASTQuery &n)
    {
        parentRequest req;

        req.distinctOnly = false;
        req.calledOnce = true;

        setParentRequest(req);
        n.query->accept(*this);
    }

    void LReturn::visit(ASTRenameColl &n)
    {
        parentRequest req;

        req.distinctOnly = true;
        req.calledOnce = true;

        setParentRequest(req);
        n.name_old->accept(*this);

        setParentRequest(req);
        n.name_new->accept(*this);
    }

    void LReturn::visit(ASTRevokePriv &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTRevokeRole &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTSchemaAttrTest &n)
    {
        // nothing to do (should be errored by sema)
    }

    void LReturn::visit(ASTSchemaElemTest &n)
    {
        // nothing to do (should be errored by sema)
    }

    void LReturn::visit(ASTSeq &n)
    {
        parentRequest req;
        childOffer off;
        size_t count = n.exprs->size();

        req = getParentRequest(); // careful to propagate atomize property
        req.distinctOnly = false;
        VisitNodesVector(n.exprs, *this, req);
        off = mergeOffers(count);

        setOffer(off);
    }

    void LReturn::visit(ASTSpaceSeq &n)
    {
        parentRequest req(getParentRequest());
        childOffer off;

        n.atomize = req.atomize;

        setParentRequest(req);
        n.expr->accept(*this);
        off = getOffer();

        setOffer(off);
    }

    void LReturn::visit(ASTTextConst &n)
    {
        childOffer off;
        parentRequest req;

        req.distinctOnly = false;
        req.deep_copy = true;
        req.atomize = true;
        req.calledOnce = getParentRequest().calledOnce;
        setParentRequest(req);
        n.expr->accept(*this);
        off = getOffer();

        // text-node constructor creates only one node
        off.exi.isOrdered = true;
        off.exi.isDistincted = true;
        off.exi.isSingleLevel = true;
        off.exi.isMax1 = true;
        off.isCached = false;
        off.exi.useConstructors = true;

        n.deep_copy = getParentRequest().deep_copy;

        setOffer(off);
    }

    void LReturn::visit(ASTTextTest &n)
    {
        if (param_mode)
            bound_vars.back().isNodes = true;
    }

    void LReturn::visit(ASTTreat &n)
    {
        parentRequest req(getParentRequest());
        childOffer off;

        req.deep_copy = true;
        req.atomize = false;
        setParentRequest(req);
        n.expr->accept(*this);
        off = getOffer();

        setOffer(off);
    }

    void LReturn::visit(ASTType &n)
    {
        if (param_mode)
        {
            std::string *pref, *loc;

            ASTParseQName(n.name, &pref, &loc);

            bound_vars.back().isNodes = (*loc == "anyType"); // anyType is used quirky way to indicate any type (nodes included)

            delete pref;
            delete loc;
        }
    }

    void LReturn::visit(ASTTypeSeq &n)
    {
        // here we need just adjust some parameters for the last bound variable in param mode

        // first check the type (it'll probably set isNodes property for var)
        n.type_test->accept(*this);

        XQVariable &var = bound_vars.back();

        if (n.mod == ASTTypeSeq::ONE || n.mod == ASTTypeSeq::OPT || n.mod == ASTTypeSeq::EMPTY)
        {
            var.exp_info.isDistincted = true;
            var.exp_info.isOrdered = true;
            var.exp_info.isSingleLevel = true;
            var.exp_info.isMax1 = true;
        }
        else
        {
            // for node sequence we cannot say anything about distinct-order properties; expression analysis will catch it later
            // for non-node sequencies we set such properites as 'true' since they don't have meaning for non-nodes
            var.exp_info.isDistincted = !var.isNodes;
            var.exp_info.isOrdered = !var.isNodes;
            var.exp_info.isSingleLevel = !var.isNodes;
            var.exp_info.isMax1 = false;
        }

        var.exp_info.useConstructors = var.isNodes; // assume the possibily of tmp-nodes if var is bound to nodes
    }

    void LReturn::visit(ASTTypeSingle &n)
    {
        // here we need just adjust some parameters for the last bound variable in param mode

        // first check the type (it'll probably set only isNodes property for var)
        n.type->accept(*this);

        XQVariable &var = bound_vars.back();
        var.exp_info.isDistincted = true;
        var.exp_info.isOrdered = true;
        var.exp_info.isSingleLevel = true;
        var.exp_info.isMax1 = true;
    }

    void LReturn::visit(ASTTypeSwitch &n)
    {
        parentRequest req;
        childOffer off_e, off_this;

        req.distinctOnly = false;
        req.calledOnce = getParentRequest().calledOnce;
        setParentRequest(req);
        n.expr->accept(*this);
        off_e = getOffer();

        // bind internal $%ts variable for cases
        XQVariable var("$%ts", NULL);
        var.exp_info = off_e.exi;
        var.isNodes = true; // always assume nodes since we cannot say for sure
        bound_vars.push_back(var);

        req = getParentRequest();
        req.atomize = false;
        VisitNodesVector(n.cases, *this, req);

        setParentRequest(req);
        n.def_case->accept(*this);

        off_this = mergeOffersSwitch(n.cases->size() + 1);

        // unbind variable
        bound_vars.pop_back();

        // add variables from the main expression
        off_this.usedVars.insert(off_e.usedVars.begin(), off_e.usedVars.end());

        // consider to cache
        if (!getParentRequest().calledOnce)
        {
            cacheTheNode(&n, off_this);

            if (off_this.isCached)
            {
                // now we should disable caching for cases
                for (unsigned int i = 0; i < n.cases->size(); i++)
                    n.cases->at(i)->setCached(false);

                n.def_case->setCached(false);
            }
        }

        setOffer(off_this);
    }

    void LReturn::visit(ASTTypeVar &n)
    {
        n.var->accept(*this);
        n.type->accept(*this); // this will change propertiers for the bound variable
    }

    void LReturn::visit(ASTUop &n)
    {
        parentRequest req;
        childOffer off;

        req.distinctOnly = true; // empty or singleton expected
        req.calledOnce = getParentRequest().calledOnce;

        setParentRequest(req);
        n.expr->accept(*this);
        off = getOffer();

        setOffer(off);
    }

    void LReturn::visit(ASTUpdDel &n)
    {
        parentRequest req;

        req.distinctOnly = true; // the order of delete doesn't matter
        req.calledOnce = true;

        setParentRequest(req);
        n.what->accept(*this);
    }

    void LReturn::visit(ASTUpdInsert &n)
    {
        parentRequest req;

        req.distinctOnly = false;
        req.calledOnce = true;
        setParentRequest(req);
        n.what->accept(*this);

        setParentRequest(req);
        n.where->accept(*this);
    }

    void LReturn::visit(ASTUpdMove &n)
    {
        // It seems that move is not actually in the current version of Sedna
        // However I provide analysis similar to replace

        childOffer off_e, off_var;
        parentRequest req;

        setParamMode();
        n.var->accept(*this);
        unsetParamMode();

        // for now we should remove our variable from bound since the expression must not see it
        XQVariable var = bound_vars.back();
        bound_vars.pop_back();

        req.distinctOnly = var.exp_info.isMax1; // if we wait for singleton then do just distinct
        req.calledOnce = true;
        setParentRequest(req);
        n.what->accept(*this);
        off_e = getOffer();

        // move actually iterates so we should use default offer
        // notice also that there cannot be tmp-nodes in what-sequence
        // now, bind variable
        var.exp_info = childOffer().exi;
        bound_vars.push_back(var);

        req.distinctOnly = false;
        setParentRequest(req);
        n.where->accept(*this);
    }

    void LReturn::visit(ASTUpdRename &n)
    {
        parentRequest req;

        req.calledOnce = true;
        req.distinctOnly = false;
        setParentRequest(req);
        n.what->accept(*this);
    }

    void LReturn::visit(ASTUpdReplace &n)
    {
        childOffer off_e, off_var;
        parentRequest req;

        setParamMode();
        n.var->accept(*this);
        unsetParamMode();

        // for now we should remove our variable from bound since the expression must not see it
        XQVariable var = bound_vars.back();
        bound_vars.pop_back();

        req.distinctOnly = var.exp_info.isMax1; // if we wait for singleton then do just distinct
        req.calledOnce = true;
        setParentRequest(req);
        n.what->accept(*this);
        off_e = getOffer();

        // replace actually iterates so we should use default offer
        // notice also that there cannot be tmp-nodes in what-sequence
        // now, bind variable
        var.exp_info = childOffer().exi;
        bound_vars.push_back(var);

        req.distinctOnly = false;
        setParentRequest(req);
        n.new_expr->accept(*this);
    }

    void LReturn::visit(ASTVar &n)
    {
        childOffer off;
        std::string name = CREATE_INTNAME(*n.uri, *n.local);

        // in param mode we just create record in bound_vars
        if (param_mode)
        {
            XQVariable var(name.c_str(), NULL);

            // for such variable we assume worst offer; type-sequence possibly will refine it
            var.exp_info.isDistincted = false;
            var.exp_info.isOrdered = false;
            var.exp_info.isMax1 = false;
            var.exp_info.isSingleLevel = false;
            var.exp_info.useConstructors = true;

            var.isNodes = true;

            bound_vars.push_back(var);
        }
        else // else we process it as a var-reference
        {
            xqExprInfo ivar;
            bool found = false;

            // first, we should look in bound variables
            for (size_t i = 0; i < bound_vars.size(); i++)
            {
                const XQVariable &var = bound_vars[bound_vars.size() - i - 1];

                if (var.int_name == name)
                {
                    ivar = var.exp_info;
                    found = true;
                    break;
                }
            }

            // if we haven't found this var in bound-vars then check module and libmodule vars
            if (!found)
            {
                XQVariable *xqv = getVariableInfo(name);

                xqv->is_used = true;
                xqv->mod->set_module_as_needed();

                ivar = xqv->exp_info;
            }

            off.exi = ivar;
            off.usedVars.insert(name);

            setOffer(off);
        }
    }

    void LReturn::visit(ASTVarDecl &n)
    {
        parentRequest req;
        childOffer off;

        setParamMode();
        n.var->accept(*this);

        if (n.type)
            n.type->accept(*this);

        unsetParamMode();

        XQVariable var = bound_vars.back();
        bound_vars.pop_back();

        // now analyze the body
        req.calledOnce = true;
        req.distinctOnly = var.exp_info.isMax1; // if we wait for singleton then do just distinct
        setParentRequest(req);
        n.expr->accept(*this);
        off = getOffer();

        // for variables for which type says node-sequence, use body analysis instead
        if (!var.exp_info.isMax1 && (var.isNodes || off.exi.isMax1))
        {
            var.exp_info = off.exi;
        }

        // make global variables never report about constructors
        // since in some sense this tmp-nodes are permanent (same on each var-ref)
        var.exp_info.useConstructors = false;

        XQVariable *mod_var;

        mod->getVariableInfo(var.int_name, &mod_var);

        mod_var->exp_info = var.exp_info;
        mod_var->isNodes = var.isNodes;

        varCache[var.int_name] = mod_var;
    }

    void LReturn::visit(ASTVersionDecl &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTXMLComm &n)
    {
        childOffer off;

        // xml comment constructor creates only one node
        off.exi.isOrdered = true;
        off.exi.isDistincted = true;
        off.exi.isSingleLevel = true;
        off.exi.isMax1 = true;
        off.isCached = false;
        off.exi.useConstructors = true;

        n.deep_copy = getParentRequest().deep_copy;

        setOffer(off);
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

    // merge offers in sequence-like manner (when result will be union of the expressions)
    LReturn::childOffer LReturn::mergeOffers(size_t count)
    {
        childOffer res;

        if (!count)
            return res;

        // max1 only if we have one child and he is max1
        res.exi.isMax1 = ((count == 1) && offers.back().exi.isMax1);
        res.exi.isOrdered = ((count == 1) && offers.back().exi.isOrdered);
        res.exi.isSingleLevel = ((count == 1) && offers.back().exi.isSingleLevel);
        res.exi.isDistincted = ((count == 1) && offers.back().exi.isDistincted);

        while (count--)
        {
            childOffer c = offers.back();
            offers.pop_back();

            if (!c.usedVars.empty())
                res.usedVars.insert(c.usedVars.begin(), c.usedVars.end());

            if (c.exi.useConstructors)
                res.exi.useConstructors = true;

            if (c.use_last)
                res.use_last = true;

            if (c.use_position)
                res.use_position = true;
        }

        return res;
    }

    // merge offers in typeswitch-like manner (when result will be one of the expressions)
    LReturn::childOffer LReturn::mergeOffersSwitch(size_t count)
    {
        childOffer res;

        if (!count)
            return res;

        while (count--)
        {
            childOffer c = offers.back();
            offers.pop_back();

            if (!c.exi.isMax1)
                res.exi.isMax1 = false;

            if (!c.exi.isOrdered)
                res.exi.isOrdered = false;

            if (!c.exi.isDistincted)
                res.exi.isDistincted = false;

            if (!c.exi.isSingleLevel)
                res.exi.isSingleLevel = false;

            if (!c.usedVars.empty())
                res.usedVars.insert(c.usedVars.begin(), c.usedVars.end());

            if (c.exi.useConstructors)
                res.exi.useConstructors = true;

            if (c.use_last)
                res.use_last = true;

            if (c.use_position)
                res.use_position = true;
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

    inline const LReturn::parentRequest &LReturn::getParentRequest() const
    {
        return pareqs.back();
    }

    inline void LReturn::setParentRequest(const LReturn::parentRequest &preq)
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
        {
            off.isCached = false;
            return;
        }

        nod->setCached(true);
        off.isCached = true;
    }

    XQVariable *LReturn::getVariableInfo(const std::string &name)
    {
        varInfo::const_iterator it;

        // first,look in cache
        it = varCache.find(name);

        if (it != varCache.end())
            return it->second;

        // then, try to process it as a local one
        XQVariable *mv;

        if (mod->getVariableInfo(name, &mv))
        {
            bool oldModeOrdered = isModeOrdered;

            /*
             * global var is evaluated in the context of the module, but not in
             * the context of the current subexpression
             */
            isModeOrdered = mod->getOrderedMode();
            mv->var->accept(*this);
            isModeOrdered = oldModeOrdered;

            // now, variable info cache is updated
            return mv;
        }

        // else, the variable is defined in some of the library modules
        XQVariable *xqv = drv->getLReturnVariableInfo(name);

        // since we've obtained this info from driver we should locally cache it
        varCache[name] = xqv;

        return xqv;
    }

    XQFunction *LReturn::getFunctionInfo(const std::string &name)
    {
        XQFunction *xqf;
        funcInfo::const_iterator it;

        // first,look in cache
        it = funcCache.find(name);

        if (it != funcCache.end())
            return it->second;

        // then, try to process it as a local one
        if (mod->getFunctionInfo(name, &xqf))
        {
            bool oldModeOrdered = isModeOrdered;

            /*
             * function is evaluated in the context of the module, but not in
             * the context of the current subexpression
             */
            isModeOrdered = mod->getOrderedMode();
            funcCache[name] = xqf;
            xqf->decl->accept(*this);
            isModeOrdered = oldModeOrdered;

            // now, function info cache is updated
            return xqf;
        }

        // else, the function is defined in some of the library modules
        xqf = drv->getLReturnFunctionInfo(name);

        // since we've obtained this info from driver we should locally cache it
        funcCache[name] = xqf;

        return xqf;
    }

    // check if this is optimization-case conjunct (i.e. fn:position() >(<,<=, etc) <expr> where <expr> doesn't depend on position or context)
    // if true, then return <expr>, else return NULL
    // Note! it doesn't check if <expr> depends on position or context right now. That check must be performed additionally.
    ASTNode *LReturn::checkIfPosConjunct(const ASTNode *n)
    {
        // consider to check for conjunct
        const ASTBop *bop = dynamic_cast<const ASTBop *>(n);

        if (bop && ((bop->op >= ASTBop::EQ_V && bop->op <= ASTBop::GE_V) || (bop->op >= ASTBop::EQ_G && bop->op <= ASTBop::GE_G)))
        {
            ASTFunCall *fc = dynamic_cast<ASTFunCall *>(bop->lop);

            if (fc && fc->isFnPosition())
            {
                return bop->rop;
            }
            else if (!fc)
            {
                fc = dynamic_cast<ASTFunCall *>(bop->rop);

                if (fc && fc->isFnPosition())
                    return bop->lop;
            }
        }

        return NULL;
    }
}
