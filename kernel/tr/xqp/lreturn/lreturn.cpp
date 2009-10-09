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
        setOffer(childOffer());
    }

    void LReturn::visit(ASTAttr &n)
    {
        unsigned int count = (n.cont) ? n.cont->size() : 0;
        childOffer off;

        if (n.cont)
        {
            VisitNodesVector(n.cont, *this, parentRequest());
            off = mergeOffers(count);
        }

        // direct attribute constructor creates only one node
        off.isOrdered = true;
        off.isDistincted = true;
        off.isSingleLevel = true;
        off.isMax1 = true;

        // consider for caching
        if (!getParentRequest().calledOnce)
            cacheTheNode(&n, off);

        setOffer(off);
    }

    void LReturn::visit(ASTAttrConst &n)
    {
        childOffer off;
        unsigned int count = 0;
        parentRequest req(getParentRequest());

        if (n.name)
        {
            setParentRequest(req);
            n.name->accept(*this);
            count++;
        }

        if (n.expr)
        {
            setParentRequest(req);
            n.expr->accept(*this);
            count++;
        }

        off = mergeOffers(count);

        // computed attribute constructor creates only one node
        off.isOrdered = true;
        off.isDistincted = true;
        off.isSingleLevel = true;
        off.isMax1 = true;

        // consider for caching
        if (!getParentRequest().calledOnce)
            cacheTheNode(&n, off);

        setOffer(off);
    }

    void LReturn::visit(ASTAttribTest &n)
    {
        setOffer(childOffer());
    }

    void LReturn::visit(ASTAxisStep &n)
    {
        parentRequest origReq = parentReq;
        childOffer off_cont; // by default we work with some context we don't know about (maybe need to refine this later)
        childOffer off_this;

        // check predicates
        if (n.preds)
        {
            parentRequest req;

            req.calledOnce = origReq.calledOnce;
            req.distinctOnly = false;

            VisitNodesVector(n.preds, *this, req);
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
                n.cont = ns->cont;
                ns->cont = NULL;
                delete ns;
            }
        }

        if (n.cont)
        {
            parentReq.distinctOnly = false;
            parentReq.calledOnce = origReq.calledOnce;
            n.cont->accept(*this);
            off_cont = getOffer();

            // it seems that we need only to distinct, not order, intermediate results since we can order them at the last step
            // exception: self axis is a filter itself, so we don't want to put distinct below it
            if (!off_cont.isDistincted && n.axis != ASTAxisStep::SELF)
            {
                n.cont = new ASTDDO(n.getLocation(), n.cont, true);
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
                ddo = new ASTDDO(n.getLocation(), &n);
                modifyParent(ddo, false, false);

                off_this.isOrdered = true;
                off_this.isDistincted = true;
            }
            else if ((!isModeOrdered || origReq.distinctOnly) && !off_this.isDistincted)
            {
                ddo = new ASTDDO(n.getLocation(), &n, true);
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
    }

    void LReturn::visit(ASTCase &n)
    {
    }

    void LReturn::visit(ASTCast &n)
    {
    }

    void LReturn::visit(ASTCastable &n)
    {
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
    }

    void LReturn::visit(ASTCreateIndex &n)
    {
    }

    void LReturn::visit(ASTCreateRole &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTCreateTrg &n)
    {
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
    }

    void LReturn::visit(ASTDefCollation &n)
    {
    }

    void LReturn::visit(ASTDefNamespaceDecl &n)
    {
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
    }

    void LReturn::visit(ASTElemConst &n)
    {
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
    }

    void LReturn::visit(ASTFilterStep &n)
    {
    }

    void LReturn::visit(ASTFor &n)
    {
    }

    void LReturn::visit(ASTFunCall &n)
    {
    }

    void LReturn::visit(ASTFuncDecl &n)
    {
    }

    void LReturn::visit(ASTGrantPriv &n)
    {
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
    }

    void LReturn::visit(ASTLibModule &n)
    {
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
    }

    void LReturn::visit(ASTModuleDecl &n)
    {
    }

    void LReturn::visit(ASTNameTest &n)
    {
    }

    void LReturn::visit(ASTNamespaceDecl &n)
    {
    }

    void LReturn::visit(ASTNodeTest &n)
    {
        // nothing to do
    }

    void LReturn::visit(ASTNsp &n)
    {
    }

    void LReturn::visit(ASTOption &n)
    {
    }

    void LReturn::visit(ASTOrdExpr &n)
    {
        n.expr->accept(*this);
    }

    void LReturn::visit(ASTOrder &n)
    {
    }

    void LReturn::visit(ASTOrderBy &n)
    {
    }

    void LReturn::visit(ASTOrderByRet &n)
    {
    }

    void LReturn::visit(ASTOrderEmpty &n)
    {
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

    void LReturn::setOffer(const childOffer &off)
    {
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
            parentReq = req;
            node = (*nodes)[i];
            node->accept(v);
        }
    }

    LReturn::childOffer LReturn::mergeOffers(unsigned int count)
    {
        childOffer res;

        // max1 only if we have one child and he is max1
        res.isMax1 = (count == 0) || ((count == 1) && offers.back().isMax1);
        res.isOrdered = (count == 0) || ((count == 1) && offers.back().isOrdered);
        res.isSingleLevel = (count == 0) || ((count == 1) && offers.back().isSingleLevel);

        while (count--)
        {
            childOffer c = offers.back();
            offers.pop_back();

            if (!c.isDistincted)
                res.isDistincted = false;

            if (c.cached)
            {
                if (!res.cached)
                    res.cached = new ASTNodesVector();

                res.cached->insert(res.cached->end(), c.cached->begin(), c.cached->end());
            }

            if (c.useBoundVars)
                res.useBoundVars = true;
        }

        return res;
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
        if (off.useBoundVars)
            return;

        if (off.cached)
        {
            while (off.cached->size())
            {
                ASTNode *cachedNode = off.cached->back();
                off.cached->pop_back();

                cachedNode->setCached(false); // disabel caching for child
            }
        }
        else
        {
            off.cached = new ASTNodesVector();
        }

        nod->setCached(true);

        off.cached->push_back(nod);
    }
}
