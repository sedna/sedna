/*
 * File:  lr2por.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/lr2por/lr2por.h"
#include "common/errdbg/exceptions.h"
#include "tr/executor/base/dynamic_context.h"
#include "tr/executor/base/PPOperations.h"
#include "tr/executor/fo/op_map.h"

#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers_data.h"
#endif

namespace sedna
{
    static const char *axis_str[] =
    {
        "PPAxisChild ",
        "PPAxisDescendant ",
        "PPAxisAttribute ",
        "PPAxisSelf ",
        "PPAxisDescendantOrSelf ",
        "PPAxisDescendantAttr ",
        "PPAxisFollowingSibling ",
        "PPAxisFollowing ",
        "PPAxisParent ",
        "PPAxisAncestor ",
        "PPAxisPrecedingSibling ",
        "PPAxisPreceding ",
        "PPAxisAncestorOrSelf ",
    };

    void lr2por::visit(ASTAlterUser &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTAttr &n)
    {
        arr_of_PPOpIn seq;
        size_t count = 0;
        PPOpIn content;
        childOffer off_this;

        if (n.cont)
        {
            ASTVisitor::VisitNodesVector(n.cont, *this);
            count += n.cont->size();
        }

        if (count == 0)
        {
            content = PPOpIn(new PPNil(dyn_cxt, createOperationInfo(n)), 1);
        }
        else
        {
            seq.reserve(count);

            while (count--)
                seq.push_back(getOffer().opin);

            std::reverse(seq.begin(), seq.end());

            content = PPOpIn(new PPSequence(dyn_cxt, createOperationInfo(n), seq), 1);
        }

        std::string name = *n.pref + ":" + *n.local;

        off_this.opin.op = new PPAttributeConstructor(dyn_cxt, createOperationInfo(n), name.c_str(), content, n.deep_copy);
        off_this.opin.ts = 1;

        setOffer(off_this);
    }

    void lr2por::visit(ASTAttrConst &n)
    {
        childOffer off_this, off_name, off_cont;

        if (n.name)
        {
            n.name->accept(*this);
            off_name = getOffer();
        }

        if (n.expr)
        {
            n.expr->accept(*this);
            off_cont = getOffer();
        }
        else
        {
            off_cont.opin = PPOpIn(new PPNil(dyn_cxt, createOperationInfo(n)), 1);
        }

        if (n.name)
        {
            off_this.opin.op = new PPAttributeConstructor(dyn_cxt, createOperationInfo(n), off_name.opin, off_cont.opin, n.deep_copy);
        }
        else
        {
            std::string name = *n.pref + ":" + *n.local;

            off_this.opin.op = new PPAttributeConstructor(dyn_cxt, createOperationInfo(n), name.c_str(), off_cont.opin, n.deep_copy);
        }

        off_this.opin.ts = 1;

        setOffer(off_this);
    }

    void lr2por::visit(ASTAttribTest &n)
    {
    }

    void lr2por::visit(ASTAxisStep &n)
    {
        childOffer off_cont, off_this;
        operation_info oi;

        oi = createOperationInfo(n);

        if (n.cont)
        {
            n.cont->accept(*this);
            off_cont = getOffer();
        }
        else
        {
            off_cont = getContextOffer(oi);
        }

        // look if we can prolong PPAbsPath
        if (n.axis <= ASTAxisStep::DESCENDANT_ATTRIBUTE && !n.preds && n.cont)
        {
            std::string lr;

            lr = getlrForAxisStep(n);

            off_this.opin = off_cont.opin;
            off_this.lr_path = off_cont.lr_path + lr;

            return;
        }
        else if (n.cont)
        {
             if (PPAbsPath *apa = dynamic_cast<PPAbsPath *>(off_this.opin.op)) // need to close PPAbsPath
             {
                 off_cont.lr_path += ')';

                 if (off_cont.lr_path != "()")
                 {
                     PathExpr *pe = lr2PathExpr(dyn_cxt, off_cont.lr_path.c_str(), pe_local_aspace);
                     apa->setPathExpr(pe);
                 }
             }
        }

        // determine if we need sequence checker
        bool need_checker = isStepNeedsChecker(n);

        // now we need to construct qep for xpath axis step
        if (n.preds)
        {
            var_id axis_cxt = var_num++; // axis context
            arr_of_var_dsc vars;
            PPOpIn preds;

            preds.op = new PPVariable(dyn_cxt, oi, axis_cxt);
            preds.ts = 1;

            preds = getPPForAxis(n, preds, oi);

            for (unsigned int i = 0; i < n.preds->size(); i++)
            {
                childOffer off;
                var_id pred_cxt; // predicate context
                var_id pos_var, last_var;
                ASTPred *pred = dynamic_cast<ASTPred *>(n.preds->at(i));
                parentRequest req;

                pred_cxt = var_num++;
                pos_var = last_var = -1;

                // bind context
                bound_vars.push_back(l2pVarInfo("$%v", pred_cxt));
                vars.push_back(pred_cxt);

                // bind pos if needed
                if (pred->usePosition())
                {
                    pos_var = var_num++;
                    bound_vars.push_back(l2pVarInfo("$%pos", pos_var));
                }

                // bind last if needed
                if (pred->useLast())
                {
                    last_var = var_num++;
                    bound_vars.push_back(l2pVarInfo("$%last", last_var));
                }

                req.pred_cxt = preds;
                setParentRequest(req);
                n.preds->at(i)->accept(*this);
                off = getOffer();

                preds = off.opin;

                if (last_var != -1)
                    bound_vars.pop_back();

                if (pos_var != -1)
                    bound_vars.pop_back();

                bound_vars.pop_back();
            }

            if (need_checker)
                preds.op = new PPSeqChecker(dyn_cxt, oi, preds, PPSeqChecker::CHECK_NODE);

            vars.push_back(axis_cxt);

            off_this.opin.op = new PPReturn(dyn_cxt, oi, vars, off_cont.opin, preds, -1);
            off_this.opin.ts = 1;
        }
        else
        {
            off_this.opin = getPPForAxis(n, off_cont.opin, oi);
        }

        setOffer(off_this);
    }

    void lr2por::visit(ASTBaseURI &n)
    {
        st_cxt->set_base_uri(n.uri->c_str());
    }

    void lr2por::visit(ASTBop &n)
    {
        childOffer off_this, off_l, off_r;

        switch (n.op)
        {
            case ASTBop::EQ_G:
                n.lop->accept(*this);
                off_l = getOffer();

                n.rop->accept(*this);
                off_r = getOffer();

                off_this.opin = PPOpIn(PPGeneralComparison::PPEQGeneralComparison(dyn_cxt, createOperationInfo(n), off_l.opin, off_r.opin), 1);
                break;
            case ASTBop::LT_G:
                n.lop->accept(*this);
                off_l = getOffer();

                n.rop->accept(*this);
                off_r = getOffer();

                off_this.opin = PPOpIn(PPGeneralComparison::PPLTGeneralComparison(dyn_cxt, createOperationInfo(n), off_l.opin, off_r.opin), 1);
                break;
            case ASTBop::LE_G:
                n.lop->accept(*this);
                off_l = getOffer();

                n.rop->accept(*this);
                off_r = getOffer();

                off_this.opin = PPOpIn(PPGeneralComparison::PPLEGeneralComparison(dyn_cxt, createOperationInfo(n), off_l.opin, off_r.opin), 1);
                break;
            case ASTBop::GT_G:
                n.lop->accept(*this);
                off_l = getOffer();

                n.rop->accept(*this);
                off_r = getOffer();

                off_this.opin = PPOpIn(PPGeneralComparison::PPGTGeneralComparison(dyn_cxt, createOperationInfo(n), off_l.opin, off_r.opin), 1);
                break;
            case ASTBop::GE_G:
                n.lop->accept(*this);
                off_l = getOffer();

                n.rop->accept(*this);
                off_r = getOffer();

                off_this.opin = PPOpIn(PPGeneralComparison::PPGEGeneralComparison(dyn_cxt, createOperationInfo(n), off_l.opin, off_r.opin), 1);
                break;
            case ASTBop::NE_G:
                n.lop->accept(*this);
                off_l = getOffer();

                n.rop->accept(*this);
                off_r = getOffer();

                off_this.opin = PPOpIn(PPGeneralComparison::PPNEGeneralComparison(dyn_cxt, createOperationInfo(n), off_l.opin, off_r.opin), 1);
                break;

            case ASTBop::IS:
                n.lop->accept(*this);
                off_l = getOffer();

                n.rop->accept(*this);
                off_r = getOffer();

                off_this.opin = PPOpIn(PPNodeComparison::PPEQNodeComparison(dyn_cxt, createOperationInfo(n), off_l.opin, off_r.opin), 1);
                break;
            case ASTBop::PREC:
                n.lop->accept(*this);
                off_l = getOffer();

                n.rop->accept(*this);
                off_r = getOffer();

                off_this.opin = PPOpIn(PPNodeComparison::PPLTNodeComparison(dyn_cxt, createOperationInfo(n), off_l.opin, off_r.opin), 1);
                break;
            case ASTBop::FOLLOW:
                n.lop->accept(*this);
                off_l = getOffer();

                n.rop->accept(*this);
                off_r = getOffer();

                off_this.opin = PPOpIn(PPNodeComparison::PPGTNodeComparison(dyn_cxt, createOperationInfo(n), off_l.opin, off_r.opin), 1);
                break;

            case ASTBop::TO:
                n.lop->accept(*this);
                off_l = getOffer();

                n.rop->accept(*this);
                off_r = getOffer();

                off_this.opin = PPOpIn(new PPRange(dyn_cxt, createOperationInfo(n), off_l.opin, off_r.opin), 1);
                break;

            default:
                var_op_num = 0;
                calc_ops = new arr_of_PPOpIn();
                op_tree = NULL;

                make_binary_op(n); // side effects: op_tree, calc_ops and var_op_num contain meaningful values now

                off_this.opin = PPOpIn(new PPCalculate(dyn_cxt, createOperationInfo(n), calc_ops, op_tree), 1);
        }

        setOffer(off_this);
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
        childOffer off_this, off_cont;

        n.expr->accept(*this); // computed comments always have expression
        off_cont = getOffer();

        off_this.opin.op = new PPCommentConstructor(dyn_cxt, createOperationInfo(n), off_cont.opin, n.deep_copy);
        off_this.opin.ts = 1;

        setOffer(off_this);
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
        childOffer off_this, off_cont;

        n.expr->accept(*this);
        off_cont = getOffer();

        off_this.opin.op = new PPDocumentConstructor(dyn_cxt, createOperationInfo(n), off_cont.opin);
        off_this.opin.ts = 1;

        setOffer(off_this);
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
        arr_of_PPOpIn seq;
        size_t count = 0;
        PPOpIn content;
        childOffer off_this;

        if (n.attrs)
        {
            ASTVisitor::VisitNodesVector(n.attrs, *this);
            count += n.attrs->size();
        }

        if (n.cont)
        {
            ASTVisitor::VisitNodesVector(n.cont, *this);
            count += n.cont->size();
        }

        if (count == 0)
        {
            content = PPOpIn(new PPNil(dyn_cxt, createOperationInfo(n)), 1);
        }
        else
        {
            seq.reserve(count);

            while (count--)
                seq.push_back(getOffer().opin);

            std::reverse(seq.begin(), seq.end());

            content = PPOpIn(new PPSequence(dyn_cxt, createOperationInfo(n), seq), 1);
        }

        std::string name = *n.pref + ":" + *n.local;

        off_this.opin.op = new PPElementConstructor(dyn_cxt, createOperationInfo(n), name.c_str(), content, n.deep_copy, n.nsp_expected);
        off_this.opin.ts = 1;

        setOffer(off_this);
    }

    void lr2por::visit(ASTElemConst &n)
    {
        childOffer off_this, off_name, off_cont;

        if (n.name)
        {
            n.name->accept(*this);
            off_name = getOffer();
        }

        if (n.expr)
        {
            n.expr->accept(*this);
            off_cont = getOffer();
        }
        else
        {
            off_cont.opin = PPOpIn(new PPNil(dyn_cxt, createOperationInfo(n)), 1);
        }

        if (n.name)
        {
            off_this.opin.op = new PPElementConstructor(dyn_cxt, createOperationInfo(n), off_name.opin, off_cont.opin, n.deep_copy, false);
        }
        else
        {
            std::string name = *n.pref + ":" + *n.local;

            off_this.opin.op = new PPElementConstructor(dyn_cxt, createOperationInfo(n), name.c_str(), off_cont.opin, n.deep_copy, false);
        }

        off_this.opin.ts = 1;

        setOffer(off_this);
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

    void lr2por::visit(ASTFLWOR &n)
    {
        ASTNodesVector::iterator it;
        unsigned int var_count = 0;
        std::vector<PPOpIn> fl_ops;
        childOffer off_ob, off_this, off_where;
        PPOpIn where, fl_close, nil;

        var_count = bound_vars.size();
        ASTVisitor::VisitNodesVector(n.fls, *this);
        var_count = bound_vars.size() - var_count;

        n.ret->accept(*this);
        off_this = getOffer();

        if (n.where)
        {
            n.where->accept(*this);
            off_where = getOffer();

            // create empty-sequence for 'else' part of where
            // NOTE: we should define nil.ts later according with STuple if it's present!
            nil.op = new PPNil(dyn_cxt, createOperationInfo(*n.where));
        }

        if (n.order_by)
        {
            parentRequest req;
            req.var_count = var_count;
            setParentRequest(req);

            n.order_by->accept(*this);
            off_ob = getOffer(); // order-by returns PPSTuple + orbs

            if (n.where)
            {
                nil.ts = off_ob.opin.ts;
                fl_close.op = new PPIf(dyn_cxt, createOperationInfo(*n.where), off_where.opin, off_ob.opin, nil);
                fl_close.ts = nil.ts;
            }
            else
            {
                fl_close = off_ob.opin;
            }
        }
        else
        {
            if (n.where)
            {
                nil.ts = off_this.opin.ts;
                fl_close.op = new PPIf(dyn_cxt, createOperationInfo(*n.where), off_where.opin, off_this.opin, nil);
                fl_close.ts = nil.ts;
            }
            else
            {
                fl_close = off_this.opin;
            }
        }

        // make for-let tree with 'fl_close' rightmost leaf
        PPOpIn flop = fl_close;
        std::vector<l2pVarInfo> un_vars; // unique bindings to remedy (for $i ... for $i like situations)
        size_t let_num = 0; // number of let-vars in un_vars

        for (size_t i = 0; i < n.fls->size(); i++)
        {
            childOffer off = getOffer();
            arr_of_var_dsc vars;
            var_id pos_var = -1;
            bool use_position = false;

            // we use position only in 'for' with positional variable
            if (const ASTFor *f = dynamic_cast<const ASTFor *>((*n.fls)[i]))
                use_position = f->usesPosVar();

            if (use_position)
            {
                pos_var = bound_vars.back().second;

                if (n.order_by)
                {
                    if (checkAndAddIfUnique(un_vars, bound_vars.back()))
                    {
                        un_vars.back().second = getVarNum(); // need to assign new bindings for for-let variables in the main return
                    }
                }

                bound_vars.pop_back();
            }

            vars.push_back(bound_vars.back().second); // main for(let) variable

            if (n.order_by)
            {
                if (checkAndAddIfUnique(un_vars, bound_vars.back()))
                {
                    un_vars.back().second = getVarNum(); // get unique binding (see above)

                    if (dynamic_cast<ASTLet *>((*n.fls)[i])) // for let-clause remember position
                        let_num++;
                }
            }
            bound_vars.pop_back();

            if (off.st.type.type == st_atomic_type && off.st.type.info.single_type == xs_anyType)
                flop.op = new PPReturn(dyn_cxt, createOperationInfo(*(*n.fls)[i]), vars, off.opin, flop, pos_var);
            else
                flop.op = new PPReturn(dyn_cxt, createOperationInfo(*(*n.fls)[i]), vars, off.opin, flop, pos_var, off.st);
        }

        if (n.order_by)
        {
            PPOpIn ob, ret;
            arr_of_var_dsc vars(un_vars.size());
            bool isStable = dynamic_cast<const ASTOrderBy *>(n.order_by)->isStable();

            ob.op = new PPOrderBy(dyn_cxt, createOperationInfo(*n.order_by), isStable, flop, off_ob.orbs, un_vars.size());
            ob.ts = un_vars.size();

            ret = off_this.opin;

            // create slets
            for (size_t i = 0; i < un_vars.size(); i++)
            {
                if (i < let_num)
                {
                    arr_of_var_dsc slet_vars;
                    slet_vars.push_back(getVarNum()); // new binding for PPSLet

                    ret.op = new PPSLet(dyn_cxt, createOperationInfo(*n.ret), slet_vars,
                                    PPOpIn(new PPVariable(dyn_cxt, createOperationInfo(*n.ret), un_vars[i].second), 1), ret);
                }

                vars[un_vars.size() - i - 1] = un_vars[i].second;
            }

            off_this.opin.op = new PPReturn(dyn_cxt, createOperationInfo(n), vars, ob, ret, -1);
            off_this.opin.ts = 1;
        }
        else
        {
            off_this.opin = flop;
        }

        setOffer(off_this);
   }

    void lr2por::visit(ASTFor &n)
    {
        childOffer off_this;

        // we need to build expr before we bind variables since there could be same-name bindings
        n.expr->accept(*this);

        off_this = getOffer();

        setParamMode();
        n.tv->accept(*this);
        off_this.st = getOffer().st;

        if (n.pv)
            n.pv->accept(*this);
        unsetParamMode();

        setOffer(off_this);
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
        childOffer off_i, off_t, off_e, off_this;

        n.i_expr->accept(*this);
        off_i = getOffer();

        n.t_expr->accept(*this);
        off_t = getOffer();

        n.e_expr->accept(*this);
        off_e = getOffer();

        U_ASSERT(off_t.opin.ts == off_e.opin.ts);

        off_this.opin = PPOpIn(new PPIf(dyn_cxt, createOperationInfo(n), off_i.opin, off_t.opin, off_e.opin), off_t.opin.ts);

        setOffer(off_this);
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
        childOffer off_this;
        xmlscm_type type;
        tuple_cell tc;

        switch (n.type)
        {
            case ASTLit::INTEGER:
                type = xs_integer;
                break;
            case ASTLit::DOUBLE:
                type = xs_double;
                break;
            case ASTLit::DECIMAL:
                type = xs_decimal;
                break;
            case ASTLit::STRING:
                type = xs_string;
                break;
        }

        tc = string2tuple_cell(*n.lit, type);

        off_this.opin = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), tc), 1);

        setOffer(off_this);
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
        PPOpIn cont;
        tuple_cell tc;
        childOffer off_this;

        tc = string2tuple_cell(*n.cont, xs_string);
        cont = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), tc), 1);

        off_this.opin.op = new PPNamespaceConstructor(dyn_cxt, createOperationInfo(n), n.name->c_str(), cont);
        off_this.opin.ts = 1;

        setOffer(off_this);
    }

    void lr2por::visit(ASTOption &n)
    {
        std::vector<ASTOption::option>::const_iterator it;
        if (*n.local == "output")
        {
            for (it = n.options->begin(); it != n.options->end(); it++)
            {
                if (it->first == "method" && it->second == "xml")
                    dynamic_context::output_method = se_output_method_xml;
                else if (it->first == "indent" && it->second == "yes")
                    st_cxt->output_indent = se_output_indent_yes;
                else if (it->first == "indent" && it->second == "no")
                    st_cxt->output_indent = se_output_indent_no;
            }
        }
        else if (*n.local == "character-map")
        {
            for (it = n.options->begin(); it != n.options->end(); it++)
            {
                dynamic_context::add_char_mapping(it->first.c_str(), it->second.c_str());
            }
        }
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
        // rebind variables since there could be for-let rebindings (e.g. for $i ... for $i ... order by $i)
        std::vector<l2pVarInfo> reb_vars;
        childOffer off_this;
        size_t vars_count = getParentRequest().var_count;

        for (unsigned int i = 1; i <= vars_count; i++)
        {
            checkAndAddIfUnique(reb_vars, bound_vars[bound_vars.size() - i]);
        }

        std::reverse(reb_vars.begin(), reb_vars.end());

        ASTVisitor::VisitNodesVector(n.specs, *this);

        // prepare our offer
        size_t tuple_size = reb_vars.size() + n.specs->size();
        arr_of_PPOpIn opi(tuple_size);
        operation_info oi = createOperationInfo(n);

        for (size_t i = 0; i < reb_vars.size(); i++)
        {
            opi[i].op = new PPVariable(dyn_cxt, oi, reb_vars[i].second);
            opi[i].ts = 1;
        }

        off_this.orbs = arr_of_orb_modifier(n.specs->size());

        for (size_t i = 0; i < n.specs->size(); i++)
        {
            childOffer off = getOffer();

            opi[tuple_size - i - 1] = off.opin;

            off_this.orbs[n.specs->size() - i - 1] = off.orbs[0];
        }

        // create PPSTuple
        off_this.opin.op = new PPSTuple(dyn_cxt, oi, opi);
        off_this.opin.ts = tuple_size;

        setOffer(off_this);
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
        childOffer off_this, off_name, off_cont;

        if (n.name)
        {
            n.name->accept(*this);
            off_name = getOffer();
        }
        else
        {
            tuple_cell tc = string2tuple_cell(*n.ncname, xs_NCName);

            off_name.opin = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), tc), 1);
        }

        if (n.expr)
        {
            n.expr->accept(*this);
            off_cont = getOffer();
        }
        else
        {
            off_cont.opin = PPOpIn(new PPNil(dyn_cxt, createOperationInfo(n)), 1);
        }

        off_this.opin.op = new PPPIConstructor(dyn_cxt, createOperationInfo(n), off_name.opin, off_cont.opin, n.deep_copy);
        off_this.opin.ts = 1;

        setOffer(off_this);
    }

    void lr2por::visit(ASTPi &n)
    {
        PPOpIn name, cont;
        tuple_cell tc;
        childOffer off_this;

        tc = string2tuple_cell(*n.name, xs_NCName);

        name = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), tc), 1);

        if (*n.cont == "")
        {
            cont = PPOpIn(new PPNil(dyn_cxt, createOperationInfo(n)), 1);
        }
        else
        {
            tc = string2tuple_cell(*n.cont, xs_string);
            cont = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), tc), 1);
        }

        off_this.opin.op = new PPPIConstructor(dyn_cxt, createOperationInfo(n), name, cont, n.deep_copy);
        off_this.opin.ts = 1;

        setOffer(off_this);
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

    void lr2por::visit(ASTPred &n)
    {
        bool use_last = n.useLast();
        bool use_pos = n.usePosition();
        var_id pos_var = -1, last_var = -1, cxt_var = -1;
        childOffer off, off_this;
        operation_info oip;

        oip = createOperationInfo(n);

        for (int i = bound_vars.size() - 1; i >= 0; i--)
        {
            if (last_var == -1 && bound_vars[i].first == "$%last")
            {
                last_var = bound_vars[i].second;
            }
            else if (pos_var == -1 && bound_vars[i].first == "$%pos")
            {
                pos_var = bound_vars[i].second;
            }
            else if (cxt_var == -1 && bound_vars[i].first == "$%v")
            {
                cxt_var = bound_vars[i].second;
            }
        }

        arr_of_var_dsc vars;
        vars.push_back(cxt_var);

        arr_of_PPOpIn conj;
        arr_of_comp_cond cond;

        ASTPred::ASTConjuncts::iterator it;

        for (it = n.conjuncts.begin(); it != n.conjuncts.end(); it++)
        {
            it->expr->accept(*this);
            off = getOffer();

            cond.push_back(it->op);
            conj.push_back(off.opin);
        }

        if (n.others.size() > 0)
        {
            U_ASSERT(n.others.size() == 1);

            n.others[0].expr->accept(*this);
            off = getOffer();
        }
        else
        {
            off.opin.op = new PPFnTrue(dyn_cxt, oip);
            off.opin.ts = 1;
        }

        if (use_last)
        {
            if (use_pos)
            {
                off_this.opin.op = new PPPred2(dyn_cxt, oip, vars, getParentRequest().pred_cxt, conj,
                        cond, off.opin, false, last_var, pos_var);
            }
            else
            {
                off_this.opin.op = new PPPred2(dyn_cxt, oip, vars, getParentRequest().pred_cxt, conj,
                        cond, off.opin, false, last_var);
            }
        }
        else
        {
            if (use_pos)
            {
                off_this.opin.op = new PPPred1(dyn_cxt, oip, vars, getParentRequest().pred_cxt, conj,
                        cond, off.opin, false, pos_var);
            }
            else
            {
                off_this.opin.op = new PPPred1(dyn_cxt, oip, vars, getParentRequest().pred_cxt, conj,
                        cond, off.opin, (n.others.size() == 1) ? !n.others[0].use_cxt : true);
            }
        }

        off_this.opin.ts = getParentRequest().pred_cxt.ts;

        setOffer(off_this);
    }

    void lr2por::visit(ASTProlog &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTQName &n)
    {
        childOffer off_this;

        char *qname = xs_QName_create(n.uri->c_str(), n.pref->c_str(), n.local->c_str(), malloc, dyn_cxt);

        off_this.opin.op = new PPConst(dyn_cxt, createOperationInfo(n), tuple_cell::atomic(xs_QName, qname));
        off_this.opin.ts = 1;

        setOffer(off_this);
    }

    void lr2por::visit(ASTQuantExpr &n)
    {
        childOffer off_e, off_sat, off_type, off_this;
        bool got_type = false;
        PPOpIn op;

        n.expr->accept(*this);
        off_e = getOffer();

        setParamMode();
        n.var->accept(*this);
        off_type = getOffer();
        got_type = !(off_type.st.type.type == st_atomic_type && off_type.st.type.info.single_type == xs_anyType);

        unsetParamMode();

        n.sat->accept(*this);
        off_sat = getOffer();

        if (n.type == ASTQuantExpr::SOME)
        {
            op = off_sat.opin;
        }
        else /* EVERY */
        {
            op.op = new PPFnNot(dyn_cxt, createOperationInfo(*n.sat), off_sat.opin);
            op.ts = off_sat.opin.ts;
        }

        arr_of_var_dsc vars;
        vars.push_back(bound_vars.back().second);

        if (got_type)
        {
            op.op = new PPSelect(dyn_cxt, createOperationInfo(*n.expr), vars, off_e.opin, op, off_type.st);
            op.ts = off_e.opin.ts;
        }
        else
        {
            op.op = new PPSelect(dyn_cxt, createOperationInfo(*n.expr), vars, off_e.opin, op);
            op.ts = off_e.opin.ts;
        }

        off_this.opin = PPOpIn(new PPFnExists(dyn_cxt, createOperationInfo(n), op), 1);

        setOffer(off_this);
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
        childOffer off_this, off_cont;

        n.expr->accept(*this);
        off_cont = getOffer();

        off_this.opin.op = new PPTextConstructor(dyn_cxt, createOperationInfo(n), off_cont.opin, n.deep_copy);
        off_this.opin.ts = 1;

        setOffer(off_this);
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
        childOffer off_this;

        var_op_num = 0;
        calc_ops = new arr_of_PPOpIn();
        op_tree = NULL;

        make_unary_op(n); // side effects: op_tree, calc_ops and var_op_num contain meaningful values now

        off_this.opin = PPOpIn(new PPCalculate(dyn_cxt, createOperationInfo(n), calc_ops, op_tree), 1);

        setOffer(off_this);
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
        std::string name = CREATE_INTNAME(*n.uri, *n.local);
        childOffer off_this;
        operation_info oi;

        if (param_mode)
        {
            bound_vars.push_back(l2pVarInfo(name, var_num++));

            return;
        }

        oi = createOperationInfo(n);

        // in usual mode we must resolve name to the id given
        // first, check if variable is bound
        if (bound_vars.size() > 0)
        {
            for (int i = bound_vars.size() - 1; i >= 0; i--)
            {
                if (bound_vars[i].first == name)
                {
                    if (name == "OLD" || name == "WHERE" || name == "NEW")
                    {
                        trigger_parameter_type var_type;

                        if (name == "NEW")
                            var_type = TRIGGER_PARAMETER_NEW;
                        else if (name == "OLD")
                            var_type = TRIGGER_PARAMETER_OLD;
                        else
                            var_type = TRIGGER_PARAMETER_WHERE;

                        PPXptr *xp = new PPXptr(dyn_cxt, oi, var_type);

#ifdef SE_ENABLE_TRIGGERS
                        if (qep_parameters)
                            qep_parameters->push_back(xp);
#endif

                        off_this.opin.op = xp;
                        off_this.opin.ts = 1;
                    }
                    else
                    {
                        off_this.opin.op = new PPVariable(dyn_cxt, oi, bound_vars[i].second);
                        off_this.opin.ts = 1;
                    }

                    setOffer(off_this);

                    return;
                }
            }
        }

        // then find it in globals
        var_id id = getGlobalVariableId(name);

        PPGlobalVariable *pgv = new PPGlobalVariable(dyn_cxt, oi, id);
        off_this.opin.op = pgv;
        off_this.opin.ts = 1;

        // if we've got id=-1 then it is because of module inter(intra)-relationships we resolve them later, after the main phase
        if (id == -1)
            mod->addToUnresolvedPor(name, pgv);

        setOffer(off_this);
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

        oi = createOperationInfo(n);

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
        PPOpIn cont;
        tuple_cell tc;
        childOffer off_this;

        tc = string2tuple_cell(*n.cont, xs_string);
        cont = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), tc), 1);

        off_this.opin.op = new PPCommentConstructor(dyn_cxt, createOperationInfo(n), cont, n.deep_copy);
        off_this.opin.ts = 1;

        setOffer(off_this);
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

    var_id lr2por::getGlobalVariableId(const std::string &name)
    {
        varInfo::iterator it;
        var_id id;

        // first,look in cache
        it = varCache.find(name);

        if (it != varCache.end())
            return it->second;

        // then, try to process it as a local one
        ASTVarDecl *vd = mod->getVariableInfo(name);
        if (vd)
        {
            id = vd->getId();

            varCache[name] = id;

            return id;
        }

        // else, the variable is defined in some of the library modules
        id = drv->getGlobalVariableId(name);

        // since we've obtained this info from driver we should locally cache it
        varCache[name] = id;

        return id;
    }

    var_id lr2por::getGlobalFunctionId(const std::string &name)
    {
        XQFunction xqf;
        funcInfo::iterator it;
        var_id id;

        // first,look in cache
        it = funcCache.find(name);

        if (it != funcCache.end())
            return it->second;

        // then, try to process it as a local one
        if (mod->getFunctionInfo(name, xqf))
        {
            id = xqf.decl->getId();

            funcCache[name] = id;

            return id;
        }

        // else, the function is defined in some of the library modules
        id = drv->getGlobalFunctionId(name);

        // since we've obtained this info from driver we should locally cache it
        funcCache[name] = id;

        return id;
    }

    lr2por::childOffer lr2por::getContextOffer(operation_info oi) const
    {
        var_id id = -1;
        childOffer off_this;

        for (int i = bound_vars.size() - 1; i >= 0; i--)
        {
            if (bound_vars[i].first == "$%v")
            {
                id = bound_vars[i].second;
                break;
            }
        }

        U_ASSERT(id != -1);

        off_this.opin.op = new PPVariable(dyn_cxt, oi, id);
        off_this.opin.ts = 1;

        return off_this;
    }

    // we want to insert sequence checker when this is not a first step, and the
    // previous step wasn't context item (.) or axis step
    bool lr2por::isStepNeedsChecker(const ASTStep &st) const
    {
        bool need_checker = false;

        if (!st.isFirstStep())
        {
            if (dynamic_cast<ASTAxisStep *>(st.cont) == NULL)
            {
                ASTFilterStep *fs = dynamic_cast<ASTFilterStep *>(st.cont);

                if (fs && !fs->expr)
                    need_checker = false;
                else
                    need_checker = true;
            }
        }

        return need_checker;
    }

    std::string lr2por::getlrForAxisStep(const ASTAxisStep &s)
    {
        std::string res = "(";
        childOffer off;

        res += axis_str[s.axis];

        s.test->accept(*this);
        off = getOffer();

        res += off.test_type;
        res += " ";
        res += off.test_data;
        res += ")";

        return res;
    }

    PPOpIn lr2por::getPPForAxis(const ASTAxisStep &s, PPOpIn cont, operation_info oi)
    {
        NodeTestType ntype;
        NodeTestData ndata;
        childOffer off;
        PPOpIn op;

        scheme_list *sl = make_tree_from_scheme_list(getlrForAxisStep(s).c_str());
        set_node_test_type_and_data(sl, ntype, ndata, pe_local_aspace);
        delete sl;

        switch (s.axis)
        {
            case ASTAxisStep::CHILD:
                op.op = new PPAxisChild(dyn_cxt, oi, cont, ntype, ndata);
                break;
            case ASTAxisStep::DESCENDANT:
                op.op = new PPAxisDescendant(dyn_cxt, oi, cont, ntype, ndata);
                break;
            case ASTAxisStep::ATTRIBUTE:
                op.op = new PPAxisAttribute(dyn_cxt, oi, cont, ntype, ndata);
                break;
            case ASTAxisStep::SELF:
                op.op = new PPAxisSelf(dyn_cxt, oi, cont, ntype, ndata);
                break;
            case ASTAxisStep::DESCENDANT_OR_SELF:
                op.op = new PPAxisDescendantOrSelf(dyn_cxt, oi, cont, ntype, ndata);
                break;
            case ASTAxisStep::FOLLOWING_SIBLING:
                op.op = new PPAxisSibling(dyn_cxt, oi, cont, ntype, ndata, true);
                break;
            case ASTAxisStep::FOLLOWING:
                op.op = new PPAxisFP(dyn_cxt, oi, cont, ntype, ndata, true);
                break;
            case ASTAxisStep::PARENT:
                op.op = new PPAxisParent(dyn_cxt, oi, cont, ntype, ndata);
                break;
            case ASTAxisStep::ANCESTOR:
                op.op = new PPAxisAncestor(dyn_cxt, oi, cont, ntype, ndata);
                break;
            case ASTAxisStep::PRECEDING_SIBLING:
                op.op = new PPAxisSibling(dyn_cxt, oi, cont, ntype, ndata, false);
                break;
            case ASTAxisStep::PRECEDING:
                op.op = new PPAxisFP(dyn_cxt, oi, cont, ntype, ndata, false);
                break;
            case ASTAxisStep::ANCESTOR_OR_SELF:
                op.op = new PPAxisAncestorOrSelf(dyn_cxt, oi, cont, ntype, ndata);
                break;
            case ASTAxisStep::DESCENDANT_ATTRIBUTE:
                op.op = new PPAxisDescendantAttr(dyn_cxt, oi, cont, ntype, ndata);
                break;
        }

        op.ts = 1;

        return op;
    }

    operation_info lr2por::createOperationInfo(const ASTNode &n)
    {
        operation_info oi;

        oi.query_line = n.getLocation().begin.line;
        oi.query_col = n.getLocation().begin.column;

        return oi;
    }

    void lr2por::alterOffer(childOffer &off_this, const childOffer &off)
    {
    }

    bool lr2por::checkAndAddIfUnique(std::vector<l2pVarInfo> &un_vars, const l2pVarInfo &var)
    {
        bool dup = false;

        for (unsigned int i = 0; i < un_vars.size(); i++)
        {
            if (un_vars[i].first == var.first)
            {
                dup = true;
                break;
            }
        }

        if (!dup)
        {
            un_vars.push_back(var);
            return true;
        }

        return false;
    }

    var_id lr2por::getVarNum()
    {
        return var_num++;
    }

    CalcOp *lr2por::make_CalcOp(ASTNode *n, bool logical)
    {
        CalcOp *res;

        if (ASTBop *b = dynamic_cast<ASTBop *>(n))
        {
            make_binary_op(*b);
            res = op_tree;
        }
        else if (ASTUop *u = dynamic_cast<ASTUop *>(n))
        {
            make_unary_op(*u);
            res = op_tree;
        }
        else
        {
            childOffer off;
            int var_op_num_saved = var_op_num;
            arr_of_PPOpIn *ops_saved = calc_ops;
            CalcOp *op_tree_saved = op_tree;

            n->accept(*this); // side effect: some subexpression might change var_op_num, ops and op_tree;

            off = getOffer();

            // return current ppcalculate context
            // TODO: make this proper via accessor functions and storing contexts in vector or something
            var_op_num = var_op_num_saved;
            op_tree = op_tree_saved;
            calc_ops = ops_saved;

            calc_ops->push_back(off.opin);

            if (logical)
                res = new LeafEffectBoolOp(calc_ops, var_op_num++);
            else
                res = new LeafAtomOp(calc_ops, var_op_num++);
        }

        return res;
    }

    void lr2por::make_binary_op(ASTBop &n)
    {
        CalcOp *lop, *rop;
        bool logic_op = (n.op == ASTBop::AND || n.op == ASTBop::OR);

        // left operand
        lop = make_CalcOp(n.lop, logic_op);
        // right operand
        rop = make_CalcOp(n.rop, logic_op);

        // it seems that there are some facilities for type-based consistency checkings in por2qep
        // however, as I see it now, types never make their way into por from Scheme's lr2por
        // so, for now I assume that operands have xs:anyType abstract type
        // It seems ok, and that's how it works now anyway; notice that dynamic checkings are in place
        // regardless of what we say about types here (AK)
        xmlscm_type lt = xs_anyType, rt = xs_anyType;
        get_binary_op_res r;

        switch (n.op)
        {
            case ASTBop::AND:
                op_tree = new BinaryOpAnd(lop, rop);
                return;
                break;
            case ASTBop::OR:
                op_tree = new BinaryOpOr(lop, rop);
                return;
                break;

            case ASTBop::EQ_V:
                r = get_binary_op(xqbop_eq, lt, rt);
                break;
            case ASTBop::NE_V:
                r = get_binary_op(xqbop_ne, lt, rt);
                break;
            case ASTBop::LT_V:
                r = get_binary_op(xqbop_lt, lt, rt);
                break;
            case ASTBop::LE_V:
                r = get_binary_op(xqbop_le, lt, rt);
                break;
            case ASTBop::GT_V:
                r = get_binary_op(xqbop_gt, lt, rt);
                break;
            case ASTBop::GE_V:
                r = get_binary_op(xqbop_ge, lt, rt);
                break;
            case ASTBop::PLUS:
                r = get_binary_op(xqbop_add, lt, rt);
                break;
            case ASTBop::MINUS:
                r = get_binary_op(xqbop_sub, lt, rt);
                break;
            case ASTBop::DIV:
                r = get_binary_op(xqbop_div, lt, rt);
                break;
            case ASTBop::IDIV:
                r = get_binary_op(xqbop_idiv, lt, rt);
                break;
            case ASTBop::MOD:
                r = get_binary_op(xqbop_mod, lt, rt);
                break;

            default:
                throw USER_EXCEPTION2(SE4001, "make_binary_op cannot process the operation");
        }

        if (r.collation)
            op_tree = new BinaryOpCollation(lop, rop, r.f.bf_c);
        else
            op_tree = new BinaryOp(lop, rop, r.f.bf);
    }

    void lr2por::make_unary_op(ASTUop &n)
    {
        CalcOp *lop;

        // operand
        lop = make_CalcOp(n.expr, false); // unary operations are always arithmetical

        // it seems that there are some facilities for type-based consistency checkings in por2qep
        // however, as I see it now, types never make their way into por from Scheme's lr2por
        // so, for now I assume that operands have xs:anyType abstract type
        // It seems ok, and that's how it works now anyway; notice that dynamic checkings are in place
        // regardless of what we say about types here (AK)
        xmlscm_type lt = xs_anyType;

        switch (n.op)
        {
            case ASTUop::PLUS:
                op_tree = new UnaryOp(lop, get_unary_op(xquop_plus, lt));
                break;

            case ASTUop::MINUS:
                op_tree = new UnaryOp(lop, get_unary_op(xquop_minus, lt));
                break;
        }
    }
}
