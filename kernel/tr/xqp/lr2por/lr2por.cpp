/*
 * File:  lreturn.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/lr2por/lr2por.h"
#include "common/errdbg/exceptions.h"
#include "tr/executor/base/dynamic_context.h"
#include "tr/executor/base/PPOperations.h"

namespace sedna
{
    void lr2por::visit(ASTAlterUser &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTAttr &n)
    {
    }

    void lr2por::visit(ASTAttrConst &n)
    {
    }

    void lr2por::visit(ASTAttribTest &n)
    {
    }

    void lr2por::visit(ASTAxisStep &n)
    {
    }

    void lr2por::visit(ASTBaseURI &n)
    {
        st_cxt->set_base_uri(n.uri->c_str());
    }

    void lr2por::visit(ASTBop &n)
    {
    }

    void lr2por::visit(ASTBoundSpaceDecl &n)
    {
        st_cxt->boundary_space = (n.mod == ASTBoundSpaceDecl::STRIP) ? xq_boundary_space_strip : xq_boundary_space_preserve;
    }

    void lr2por::visit(ASTCase &n)
    {
    }

    void lr2por::visit(ASTCast &n)
    {
    }

    void lr2por::visit(ASTCastable &n)
    {
    }

    void lr2por::visit(ASTCharCont &n)
    {
    }

    void lr2por::visit(ASTCommTest &n)
    {
    }

    void lr2por::visit(ASTCommentConst &n)
    {
    }

    void lr2por::visit(ASTConstDecl &n)
    {
        st_cxt->preserve_type = (n.mod == ASTConstDecl::PRESERVE) ? true : false;
    }

    void lr2por::visit(ASTCreateColl &n)
    {
    }

    void lr2por::visit(ASTCreateDoc &n)
    {
    }

    void lr2por::visit(ASTCreateFtIndex &n)
    {
    }

    void lr2por::visit(ASTCreateIndex &n)
    {
    }

    void lr2por::visit(ASTCreateRole &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTCreateTrg &n)
    {
    }

    void lr2por::visit(ASTCreateUser &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTDDO &n)
    {
    }

    void lr2por::visit(ASTDeclareCopyNsp &n)
    {
        st_cxt->cn_preserve = (n.pres_mod == ASTDeclareCopyNsp::PRESERVE) ? true : false;
        st_cxt->cn_inherit = (n.pres_mod == ASTDeclareCopyNsp::INHERIT) ? true : false;
    }

    void lr2por::visit(ASTDefCollation &n)
    {
        st_cxt->set_default_collation_uri(n.uri->c_str());
    }

    void lr2por::visit(ASTDefNamespaceDecl &n)
    {
        // we don't add default function namespace, since it is resolved in sema
        if (n.type == ASTDefNamespaceDecl::ELEMENT)
            st_cxt->add_to_context(NULL, n.uri->c_str());
    }

    void lr2por::visit(ASTDocConst &n)
    {
    }

    void lr2por::visit(ASTDocTest &n)
    {
    }

    void lr2por::visit(ASTDropColl &n)
    {
    }

    void lr2por::visit(ASTDropDoc &n)
    {
    }

    void lr2por::visit(ASTDropFtIndex &n)
    {
    }

    void lr2por::visit(ASTDropIndex &n)
    {
    }

    void lr2por::visit(ASTDropMod &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTDropRole &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTDropTrg &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTDropUser &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTElem &n)
    {
    }

    void lr2por::visit(ASTElemConst &n)
    {
    }

    void lr2por::visit(ASTElementTest &n)
    {
    }

    void lr2por::visit(ASTEmptyTest &n)
    {
    }

    void lr2por::visit(ASTError &n)
    {
        throw SYSTEM_EXCEPTION("If you see this, you are very unlucky. Anyway, this is an internal parser error.");
    }

    void lr2por::visit(ASTExtExpr &n)
    {
    }

    void lr2por::visit(ASTFilterStep &n)
    {
    }

    void lr2por::visit(ASTFor &n)
    {
    }

    void lr2por::visit(ASTFunCall &n)
    {
    }

    void lr2por::visit(ASTFuncDecl &n)
    {
        int id;
        function_declaration fd;
        unsigned int arity = (n.params) ? n.params->size() : 0;

        var_num = 0;
        id = drv->getNewGlobFunId();
        fd = dynamic_context::funct_cxt.fun_decls[id];

        n.ret->accept(*this);
        fd.ret_st = getOffer().st;

        if (n.params)
        {
            fd.args = se_new sequence_type[arity];

            setParamMode();
            for (unsigned int i = 0; i < arity; i++)
            {
                n.params->at(i)->accept(*this);
                fd.args[i] = getOffer().st;
            }
            unsetParamMode();
        }
        else
        {
            fd.args = NULL;
        }

        // body is evaluated in a dummy dynamic context
        dynamic_context dc(st_cxt, 0); // ok, it will become illegal when we exit the function, but since function-body is copied
                                       // during evaluation it will not be a problem
        dyn_cxt = &dc;
        n.body->accept(*this);

        fd.op = getOffer().opin.op;
        dyn_cxt = NULL;

        fd.st_cxt = st_cxt;
        fd.cxt_size = var_num;
        fd.num = arity;

        // get rid of vars
        while (arity--)
            bound_vars.pop_back();
    }

    void lr2por::visit(ASTGrantPriv &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTGrantRole &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTIf &n)
    {
    }

    void lr2por::visit(ASTInstOf &n)
    {
    }

    void lr2por::visit(ASTItemTest &n)
    {
    }

    void lr2por::visit(ASTLet &n)
    {
    }

    void lr2por::visit(ASTLibModule &n)
    {
        // nothing to do
        // we optimize library vars and functions on-demand from main module
    }

    void lr2por::visit(ASTLit &n)
    {
    }

    void lr2por::visit(ASTLoadFile &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTLoadModule &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTMainModule &n)
    {
        if (drv == NULL)
            throw SYSTEM_EXCEPTION("Driver is not set for semantic analyzer!");

        n.prolog->accept(*this);
        n.query->accept(*this);
    }

    void lr2por::visit(ASTMetaCols &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTMetaDocs &n)
    {
    }

    void lr2por::visit(ASTMetaSchemaCol &n)
    {
    }

    void lr2por::visit(ASTMetaSchemaDoc &n)
    {
    }

    void lr2por::visit(ASTModImport &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTModuleDecl &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTNameTest &n)
    {
    }

    void lr2por::visit(ASTNamespaceDecl &n)
    {
        st_cxt->add_to_context(n.name->c_str(), n.uri->c_str());
    }

    void lr2por::visit(ASTNodeTest &n)
    {
    }

    void lr2por::visit(ASTNsp &n)
    {
    }

    void lr2por::visit(ASTOption &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTOrdExpr &n)
    {
    }

    void lr2por::visit(ASTOrder &n)
    {
        st_cxt->ordering_mode = (n.mod == ASTOrder::ORDERED) ? xq_ordering_mode_ordered : xq_ordering_mode_unordered;
    }

    void lr2por::visit(ASTOrderBy &n)
    {
    }

    void lr2por::visit(ASTOrderByRet &n)
    {
    }

    void lr2por::visit(ASTOrderEmpty &n)
    {
        st_cxt->empty_order = (n.mod == ASTOrderEmpty::EMPTY_GREATEST) ? xq_empty_order_greatest : xq_empty_order_least;
    }

    void lr2por::visit(ASTOrderMod &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTOrderModInt &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTOrderSpec &n)
    {
    }

    void lr2por::visit(ASTPIConst &n)
    {
    }

    void lr2por::visit(ASTPi &n)
    {
    }

    void lr2por::visit(ASTPiTest &n)
    {
    }

    void lr2por::visit(ASTPosVar &n)
    {
    }

    void lr2por::visit(ASTPragma &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTProlog &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTQName &n)
    {
    }

    void lr2por::visit(ASTQuantExpr &n)
    {
    }

    void lr2por::visit(ASTQuery &n)
    {
    }

    void lr2por::visit(ASTRenameColl &n)
    {
    }

    void lr2por::visit(ASTRevokePriv &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTRevokeRole &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTSchemaAttrTest &n)
    {
        // nothing to do (should be errored by sema)
    }

    void lr2por::visit(ASTSchemaElemTest &n)
    {
        // nothing to do (should be errored by sema)
    }

    void lr2por::visit(ASTSeq &n)
    {
    }

    void lr2por::visit(ASTSpaceSeq &n)
    {
    }

    void lr2por::visit(ASTTextConst &n)
    {
    }

    void lr2por::visit(ASTTextTest &n)
    {
    }

    void lr2por::visit(ASTTreat &n)
    {
    }

    void lr2por::visit(ASTType &n)
    {
    }

    void lr2por::visit(ASTTypeSeq &n)
    {
    }

    void lr2por::visit(ASTTypeSingle &n)
    {
    }

    void lr2por::visit(ASTTypeSwitch &n)
    {
    }

    void lr2por::visit(ASTTypeVar &n)
    {
    }

    void lr2por::visit(ASTUnio &n)
    {
    }

    void lr2por::visit(ASTUop &n)
    {
    }

    void lr2por::visit(ASTUpdDel &n)
    {
    }

    void lr2por::visit(ASTUpdInsert &n)
    {
    }

    void lr2por::visit(ASTUpdMove &n)
    {
    }

    void lr2por::visit(ASTUpdRename &n)
    {
    }

    void lr2por::visit(ASTUpdReplace &n)
    {
    }

    void lr2por::visit(ASTVar &n)
    {
    }

    void lr2por::visit(ASTVarDecl &n)
    {
        childOffer off, off_this, off_type;
        dynamic_context *cxt;
        int id;
        PPVarIterator *var;
        operation_info oi;

        // analyze the type
        if (n.type)
        {
            n.type->accept(*this);
            off_type = getOffer();
        }

        // analyze the body
        var_num = 0;
        n.expr->accept(*this);
        off = getOffer();

        cxt = new dynamic_context(st_cxt, var_num);
        id = drv->getNewGlobVarId();
        oi.query_line = n.getLocation().begin.line;
        oi.query_col = n.getLocation().begin.column;

        if (n.type)
            var = new PPVarDecl(cxt, oi, id, off.opin, off_type.st);
        else
            var = new PPVarDecl(cxt, oi, id, off.opin);

        dynamic_context::glb_var_cxt.producers[id].op = var;
        dynamic_context::glb_var_cxt.producers[id].cxt = cxt;
    }

    void lr2por::visit(ASTVersionDecl &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTXMLComm &n)
    {
    }

    // Some additional function

    void lr2por::setParamMode()
    {
        param_mode = true;
        param_count = 0;
    }
    void lr2por::unsetParamMode()
    {
        param_mode = false;
    }

    void lr2por::setOffer(const childOffer &off)
    {
        offers.push_back(off);
    }

    lr2por::childOffer lr2por::getOffer()
    {
        childOffer res = offers.back();

        offers.pop_back();

        return res;
    }

    void lr2por::VisitNodesVector(ASTNodesVector *nodes, ASTVisitor &v, parentRequest req)
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

    const lr2por::parentRequest &lr2por::getParentRequest() const
    {
        return pareqs.back();
    }

    void lr2por::setParentRequest(const lr2por::parentRequest &preq)
    {
        parentReq = preq;
    }

    void lr2por::addToPath(ASTNode *nod)
    {
        ASTVisitor::addToPath(nod);
        pareqs.push_back(parentReq);
    }

    void lr2por::removeFromPath(ASTNode *nod)
    {
        ASTVisitor::removeFromPath(nod);
        pareqs.pop_back();
    }

    XQVariable lr2por::getVariableInfo(const std::string &name)
    {
        varInfo::iterator it;


        // first,look in cache
        it = varCache.find(name);

        if (it != varCache.end())
            return it->second;

        // then, try to process it as a local one
        ASTVarDecl *vd = mod->getVariableInfo(name);
        if (vd)
        {
            bool oldModeOrdered = isModeOrdered;

            isModeOrdered = mod->getOrderedMode();
            vd->accept(*this);
            isModeOrdered = oldModeOrdered;

            // now, variable info cache is updated
            return varCache[name];
        }

        // else, the variable is defined in some of the library modules
        XQVariable xqv = drv->getLReturnVariableInfo(name);

        // since we've obtained this info from driver we should locally cache it
        varCache[name] = xqv;

        return xqv;
    }

    XQFunction lr2por::getFunctionInfo(const std::string &name)
    {
        XQFunction xqf;
        funcInfo::iterator it;

        // first,look in cache
        it = funcCache.find(name);

        if (it != funcCache.end())
            return it->second;

        // then, try to process it as a local one
        if (mod->getFunctionInfo(name, xqf))
        {
            bool oldModeOrdered = isModeOrdered;

            isModeOrdered = mod->getOrderedMode();
            funcCache[name] = xqf;
            xqf.decl->accept(*this);
            isModeOrdered = oldModeOrdered;

            // now, function info cache is updated
            return funcCache[name];
        }

        // else, the function is defined in some of the library modules
        xqf = drv->getLReturnFunctionInfo(name);

        // since we've obtained this info from driver we should locally cache it
        funcCache[name] = xqf;

        return xqf;
    }
}
