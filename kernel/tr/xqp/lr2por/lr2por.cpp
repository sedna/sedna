/*
 * File:  lr2por.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/xqp/lr2por/lr2por.h"
#include "common/errdbg/exceptions.h"
#include "tr/executor/base/dynamic_context.h"
#include "tr/executor/base/PPOperations.h"
#include "tr/executor/fo/op_map.h"
#include "tr/executor/por2qep/ext.h"

#ifdef SE_ENABLE_TRIGGERS
#include "tr/triggers/triggers_data.h"
#endif

#include "tr/executor/base/OperationHeaders.h"

#include <algorithm>

namespace sedna
{
    void lr2por::visit(ASTAlterUser &n)
    {
        qep = new PPAlterUser(PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.user, xs_string)), 1),
                              PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.psw, xs_string)), 1), dyn_cxt);
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

            // we've obtained our context sequence in reverse order
            std::reverse(seq.begin(), seq.end());

            content = PPOpIn(new PPSequence(dyn_cxt, createOperationInfo(n), seq), 1);
        }

        std::string name = n.pref->empty() ?
                           *n.local :
                           *n.pref + ":" + *n.local;

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
            std::string name = n.pref->empty() ?
                               *n.local :
                               *n.pref + ":" + *n.local;

            off_this.opin.op = new PPAttributeConstructor(dyn_cxt, createOperationInfo(n), name.c_str(), off_cont.opin, n.deep_copy);
        }

        off_this.opin.ts = 1;

        setOffer(off_this);
    }

    void lr2por::visit(ASTAttribTest &n)
    {
        // we ignore type here except for sequence types
        childOffer off_this;

        if (n.name)
        {
            n.name->accept(*this);
            off_this = getOffer();

            if (off_this.st.type.info.ea.nne == st_nne_name) {
                off_this.serialized_form = "(attribute " + off_this.serialized_form + ")";
            } else if (off_this.st.type.info.ea.nne == st_nne_wildcard) {
                off_this.serialized_form = "(attribute any)";
            } else {
              /* In this case --- do nothing. That means that it is not an attribute check. */
            }
        }
        else
        {
            off_this.serialized_form = "(attribute any)";
            off_this.st.type.info.ea.nne = st_nne_wildcard;
        }

        off_this.st.type.type = st_attribute;

        if (n.type)
        {
            childOffer off_type;

            n.type->accept(*this);
            off_type = getOffer();

            off_this.st.type.info.ea.type_name = off_type.st.type.info.single_type;
            off_this.st.type.info.ea.tne = st_tne_present;
        }
        else
        {
            off_this.st.type.info.ea.tne = st_tne_nothing;
        }

        setOffer(off_this);
    }

    void lr2por::visit(ASTAxisStep &n)
    {
        childOffer off_cont, off_this;
        operation_info oi;

        oi = createOperationInfo(n);

        if (n.cont)
        {
            // we want to propagate pers_abspath property here
            setParentRequest(getParentRequest());
            n.cont->accept(*this);
            off_cont = getOffer();
        }
        else if (!pers_path_mode) // we don't need context when we create persistent relative path for index
        {
            off_cont = getContextOffer(oi);
        }

        // look if we can prolong PPAbsPath or start relative one for index
        if (!n.cont && pers_path_mode)
        {
            db_entity *dbe = new db_entity;
            dbe->type = dbe_document;
            dbe->name = NULL;
            off_cont.opin = PPOpIn(new PPAbsPath(dyn_cxt, createOperationInfo(n), NULL, counted_ptr<db_entity>(dbe)), 1);
            off_cont.open_abs_path = true;
        }

        if (n.isSuitableForAbsPath() && off_cont.open_abs_path) // if op == NULL then this axis starts a relative non-abspath XPath
        {
            PPAbsPath *apa = dynamic_cast<PPAbsPath *>(off_cont.opin.op);
            U_ASSERT(apa != NULL);

            std::string lr;

            lr = getlrForAxisStep(n);

            off_this.opin = off_cont.opin;
            off_this.lr_path = off_cont.lr_path + " " + lr;

            // last step: should finalize abspath
            if (n.isLast)
            {
                finalizeAbsPath(apa, off_this.lr_path.c_str(), pers_path_mode);
                off_this.lr_path = "";
                off_this.open_abs_path = false;
            }
            else
            {
                off_this.open_abs_path = true;
            }

            setOffer(off_this);

            return;
        }
        else if (n.cont && off_cont.open_abs_path) // need to close PPAbsPath
        {
            U_ASSERT(dynamic_cast<PPAbsPath *>(off_cont.opin.op) != NULL);
            finalizeAbsPath(dynamic_cast<PPAbsPath *>(off_cont.opin.op), off_cont.lr_path.c_str(), pers_path_mode);
            off_this.open_abs_path = false;
        }

        // determine if we need sequence checker
        bool need_checker = isStepNeedsChecker(n);

        if (need_checker) // check context
            off_cont.opin.op = new PPSeqChecker(dyn_cxt, oi, off_cont.opin, PPSeqChecker::CHECK_NODE);

        // now we need to construct qep for xpath axis step
        if (n.preds)
        {
            var_dsc axis_cxt = getVarNum(); // axis context
            arr_of_var_dsc vars;
            PPOpIn preds;

            preds.op = new PPVariable(dyn_cxt, oi, axis_cxt);
            preds.ts = 1;

            preds = getPPForAxis(n, preds, oi);

            for (unsigned int i = 0; i < n.preds->size(); i++)
            {
                childOffer off;
                var_dsc pred_cxt; // predicate context
                var_dsc pos_var, last_var;
                ASTPred *pred = dynamic_cast<ASTPred *>(n.preds->at(i));
                parentRequest req;

                pred_cxt = getVarNum();
                pos_var = last_var = INVALID_VAR_DSC;

                // bind context
                bound_vars.push_back(l2pVarInfo("$%v", pred_cxt));

                // bind pos if needed
                if (pred->usePosition())
                {
                    pos_var = getVarNum();
                    bound_vars.push_back(l2pVarInfo("$%pos", pos_var));
                }

                // bind last if needed
                if (pred->useLast())
                {
                    last_var = getVarNum();
                    bound_vars.push_back(l2pVarInfo("$%last", last_var));
                }

                req.pred_cxt = preds;
                setParentRequest(req);
                (*n.preds)[i]->accept(*this);
                off = getOffer();

                preds = off.opin;

                if (last_var != INVALID_VAR_DSC)
                    bound_vars.pop_back();

                if (pos_var != INVALID_VAR_DSC)
                    bound_vars.pop_back();

                bound_vars.pop_back();
            }

            vars.push_back(axis_cxt);

            off_this.opin.op = new PPReturn(dyn_cxt, oi, vars, off_cont.opin, preds, INVALID_VAR_DSC);
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
        try
        {
            dyn_cxt->get_static_context()->set_base_uri(n.uri->c_str());
        }
        catch (SednaUserException &e) // invalid uri exception
        {
            drv->error(e.get_code(), e.getDescription().c_str());
        }
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

            case ASTBop::UNION:
                n.lop->accept(*this);
                off_l = getOffer();

                n.rop->accept(*this);
                off_r = getOffer();

                off_this.opin = PPOpIn(new PPUnion(dyn_cxt, createOperationInfo(n), off_l.opin, off_r.opin, n.doc_order), 1);
                break;

            case ASTBop::INTERSECT:
                n.lop->accept(*this);
                off_l = getOffer();

                n.rop->accept(*this);
                off_r = getOffer();

                off_this.opin = PPOpIn(new PPIntersect(dyn_cxt, createOperationInfo(n), off_l.opin, off_r.opin, n.doc_order), 1);
                break;

            case ASTBop::EXCEPT:
                n.lop->accept(*this);
                off_l = getOffer();

                n.rop->accept(*this);
                off_r = getOffer();

                off_this.opin = PPOpIn(new PPExcept(dyn_cxt, createOperationInfo(n), off_l.opin, off_r.opin, n.doc_order), 1);
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
        dyn_cxt->get_static_context()->set_boundary_space((n.mod == ASTBoundSpaceDecl::STRIP) ? xq_boundary_space_strip : xq_boundary_space_preserve);
    }

    void lr2por::visit(ASTCase &n)
    {
        childOffer off_this;

        // first we need to bind our var if we've got it
        if (n.var)
        {
            // first get typeswitch main binding; it should be the last one in bound_vars
            U_ASSERT(bound_vars.back().first == "$%ts");
            var_dsc var = bound_vars.back().second;

            // then bind our var
            // this should be ASTVar
            U_ASSERT(dynamic_cast<const ASTVar *>(n.var));
            std::string name = dynamic_cast<const ASTVar *>(n.var)->getStandardName();

            bound_vars.push_back(l2pVarInfo(name, var));
        }

        n.expr->accept(*this);
        off_this.opin = getOffer().opin;

        if (n.type) // not default case
        {
            n.type->accept(*this);
            off_this.st = getOffer().st;
        }

        setOffer(off_this);

        // get rif of our binding if any
        if (n.var)
            bound_vars.pop_back();
    }

    void lr2por::visit(ASTCast &n)
    {
        childOffer off_e, off_t, off_this;

        n.expr->accept(*this);
        off_e = getOffer();

        n.type->accept(*this);
        off_t = getOffer();

        U_ASSERT(off_t.st.type.type == st_atomic_type);
        off_this.opin = PPOpIn(new PPCast(dyn_cxt, createOperationInfo(n), off_e.opin, off_t.st.type.info.single_type, off_t.st.oi == st_optional), 1);

        setOffer(off_this);
    }

    void lr2por::visit(ASTCastable &n)
    {
        childOffer off_e, off_t, off_this;

        n.expr->accept(*this);
        off_e = getOffer();

        n.type->accept(*this);
        off_t = getOffer();

        U_ASSERT(off_t.st.type.type == st_atomic_type);
        off_this.opin = PPOpIn(new PPCastable(dyn_cxt, createOperationInfo(n), off_e.opin, off_t.st.type.info.single_type, off_t.st.oi == st_optional), 1);

        setOffer(off_this);
    }

    void lr2por::visit(ASTCharCont &n)
    {
        childOffer off_this;

        if (n.orig == ASTCharCont::CDATA) {
            off_this.opin = PPOpIn(new PPTextConstructor(dyn_cxt, createOperationInfo(n), n.cont->c_str(), true, true), 1);
        } else {
            off_this.opin = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.cont, xs_string)), 1);
        }

        setOffer(off_this);
    }

    void lr2por::visit(ASTCommTest &n)
    {
        childOffer off_this;

        off_this.serialized_form = "(comment)";
        off_this.st.type.type = st_comment;

        setOffer(off_this);
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
        dyn_cxt->get_static_context()->set_construction_mode((n.mod == ASTConstDecl::PRESERVE) ? true : false);
    }

    void lr2por::visit(ASTCreateColl &n)
    {
        childOffer off_coll;

        n.coll->accept(*this);
        off_coll = getOffer();

        qep = new PPCreateCollection(off_coll.opin, dyn_cxt);
    }

    void lr2por::visit(ASTCreateDoc &n)
    {
        childOffer off_doc, off_coll;

        n.doc->accept(*this);
        off_doc = getOffer();

        if (n.coll)
        {
            n.coll->accept(*this);
            off_coll = getOffer();
        }

        if (n.coll)
            qep = new PPCreateDocumentInCollection(off_doc.opin, off_coll.opin, dyn_cxt);
        else
            qep = new PPCreateDocument(off_doc.opin, dyn_cxt);
    }

    void lr2por::visit(ASTCreateFtIndex &n)
    {
#ifdef SE_ENABLE_FTSEARCH

        childOffer off_name, off_path, off_options;
        PPAbsPath *pa;
        xpath::PathExpression *onp;
        counted_ptr<db_entity> dbe;
        PPOpIn comp_name;

        n.name->accept(*this);
        off_name = getOffer();

        pers_path_mode = true;
        n.path->accept(*this);
        off_path = getOffer();
        pers_path_mode = false;

		if (n.options != NULL)
		{
			n.options->accept(*this);
			off_options = getOffer();
		}

        // path will definitely be PPAbsPath
        pa = dynamic_cast<PPAbsPath *>(off_path.opin.op);
        U_ASSERT(pa);

        onp = pa->getPathExpr();
        dbe = pa->getDocColl();

        if (!dbe->name)
            comp_name = pa->getCompName();

        delete pa; // we don't need it anymore (note that this won't destroy onp)

        if (!onp || onp->size() == 0) {
            setDefaultSpace(catalog_space_base);
            onp = new xpath::PathExpression();
            popDefaultSpace();
        }

        xpath::PathExprRoot peroot(dbe);

        // computed name in doc/coll
        if (!dbe->name)
        {
            peroot.set_name(comp_name);
        }

        // make qe
        if (*n.type == "customized-value" || *n.type == "!customized-value")
        {
            childOffer off_cust;

            n.cust_expr->accept(*this);
            off_cust = getOffer();

			qep = new PPCreateFtIndex(onp, n.type->c_str(), peroot, off_name.opin, off_options.opin, off_cust.opin, dyn_cxt);
        }
        else
        {
			childOffer off_cust;
            qep = new PPCreateFtIndex(onp, n.type->c_str(), peroot, off_name.opin, off_options.opin, off_cust.opin, dyn_cxt);
        }
#else
        drv->error(SE1002, "full-text search support is disabled");
#endif
    }

    void lr2por::visit(ASTCreateIndex &n)
    {
        childOffer off_name, off_path, off_type;
        PPAbsPath *pa;
        xpath::PathExpression *onp, *byp;
        xmlscm_type xtype;
        counted_ptr<db_entity> dbe;
        PPOpIn comp_name;

        n.name->accept(*this);
        off_name = getOffer();

        pers_path_mode = true;
        n.on_path->accept(*this);
        off_path = getOffer();

        // on-path will definitely be PPAbsPath
        pa = dynamic_cast<PPAbsPath *>(off_path.opin.op);
        U_ASSERT(pa);

        onp = pa->getPathExpr();
        dbe = pa->getDocColl();

        if (!dbe->name)
            comp_name = pa->getCompName();

        delete pa; // we don't need it anymore (note that this won't destroy onp)

        setDefaultSpace(catalog_space_base);

        if (!onp || onp->size() == 0) { // should make it persistent (not-null path will be made persistent by ast-ops)
            onp = new xpath::PathExpression();
        }

        n.by_path->accept(*this);
        off_path = getOffer();
        pers_path_mode = false;

        // now by-path will definitely be PPAbsPath
        pa = dynamic_cast<PPAbsPath *>(off_path.opin.op);
        U_ASSERT(pa);

        byp = pa->getPathExpr();
        delete pa; // we don't need it anymore (note that this won't destroy on_path)

        if (!byp || byp->size() == 0) // should make it persistent (not-null path will be made persistent by ast-ops)
            byp = new xpath::PathExpression();

        popDefaultSpace();

        n.type->accept(*this);
        off_type = getOffer();
        xtype = off_type.st.type.info.single_type;

        xpath::PathExprRoot peroot(dbe);

        // computed name in doc/coll
        if (!dbe->name)
        {
            peroot.set_name(comp_name);
        }

        qep = new PPCreateIndex(off_name.opin, peroot, onp, byp, xtype, dyn_cxt, n.tree_type->c_str());
    }

    void lr2por::visit(ASTCreateRole &n)
    {
        qep = new PPCreateRole(PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.role, xs_string)), 1), dyn_cxt);
    }

#ifdef SE_ENABLE_TRIGGERS
    inline trigger_event trgmod2te(ASTCreateTrg::TrgMod tm, XQueryDriver *drv)
    {
        switch (tm)
        {
            case ASTCreateTrg::INSERT:
                return TRIGGER_INSERT_EVENT;
            case ASTCreateTrg::DEL:
                return TRIGGER_DELETE_EVENT;
            case ASTCreateTrg::REPLACE:
                return TRIGGER_REPLACE_EVENT;
            default:
                drv->error(SE4001, "unknown trigger event in ast");
                return TRIGGER_INSERT_EVENT;
        }
    }

    inline trigger_time trgmod2tt(ASTCreateTrg::TrgMod tm, XQueryDriver *drv)
    {
        switch (tm)
        {
            case ASTCreateTrg::BEFORE:
                return TRIGGER_BEFORE;
            case ASTCreateTrg::AFTER:
                return TRIGGER_AFTER;
            default:
                drv->error(SE4001, "unknown trigger time in ast");
                return TRIGGER_AFTER;
        }
    }

    inline trigger_granularity trgmod2tg(ASTCreateTrg::TrgMod tm, XQueryDriver *drv)
    {
        switch (tm)
        {
            case ASTCreateTrg::NODE:
                return TRIGGER_FOR_EACH_NODE;
            case ASTCreateTrg::STATEMENT:
                return TRIGGER_FOR_EACH_STATEMENT;
            default:
                drv->error(SE4001, "unknown trigger granularity in ast");
                return TRIGGER_FOR_EACH_NODE;
        }
    }
#endif /* SE_ENABLE_TRIGGERS */

    void lr2por::visit(ASTCreateTrg &n)
    {
#ifdef SE_ENABLE_TRIGGERS

        PPAbsPath *pa;
        xpath::PathExpression *onp;
        counted_ptr<db_entity> dbe;
        childOffer off_path;
        PPOpIn name, comp_name;
        tuple_cell tc;

        // create trigger name
        tc = string2tuple_cell(*n.name, xs_string);
        name = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), tc), 1);

        setDefaultSpace(catalog_space_base);

        // on-path will definitely be PPAbsPath
        pers_path_mode = true;
        n.path->accept(*this);
        off_path = getOffer();
        pers_path_mode = false;
        pa = dynamic_cast<PPAbsPath *>(off_path.opin.op);
        U_ASSERT(pa);

        onp = pa->getPathExpr();
        dbe = pa->getDocColl();

        if (!dbe->name)
            comp_name = pa->getCompName();

        delete pa; // we don't need it anymore (note that this won't destroy onp)
        xpath::PathExprRoot peroot(dbe);

        // computed name in doc/coll
        if (!dbe->name)
        {
            peroot.set_name(comp_name);
        }


        if (!onp || onp->size() == 0) { // should make it persistent (not-null path will be made persistent by ast-ops)
            onp = new xpath::PathExpression();
        }

        scheme_list *action = new scheme_list(n.do_exprs->size());

        // serialize trigger-query into scheme_list
        for (size_t i = 0; i < n.do_exprs->size(); i++)
        {
            ASTQuery *st_query = dynamic_cast<ASTQuery *>(n.do_exprs->at(i));

            st_query->is_trigger = true;

            // internal representation for a query
            std::string ir = mod->getIR(st_query);

            // trigger actions are pairs: (query: ir_string, is_query: bool)
            action->at(i).type = SCM_STRING;
            action->at(i).internal.str = new char[ir.size() + 1];

            strcpy(action->at(i).internal.str, ir.c_str());
        }

        if (n.t_mod == ASTCreateTrg::BEFORE && n.a_mod == ASTCreateTrg::INSERT && n.g_mod == ASTCreateTrg::NODE)
        {
            childOffer off_ipath;
            xpath::PathExpression *ip;

            pers_path_mode = true;
            n.trimmed_path->accept(*this);
            off_ipath = getOffer();
            pers_path_mode = false;

            pa = dynamic_cast<PPAbsPath *>(off_ipath.opin.op);
            U_ASSERT(pa);

            ip = pa->getPathExpr();
            delete pa; // we don't need it anymore (note that this won't destroy onp)

            if (!ip || ip->size() == 0) { // should make it persistent (not-null path will be made persistent by ast-ops)
                ip = new xpath::PathExpression();
            }

            inserting_node innode(n.leaf_name->c_str(), n.leaf_type == 0 ? element : attribute);

            qep = new PPCreateTrigger(dyn_cxt, //dynamic context
                                      peroot,  //PathExprRoot
                                      trgmod2te(n.a_mod, drv), //trigger event
                                      trgmod2tt(n.t_mod, drv), //trigger time
                                      trgmod2tg(n.g_mod, drv), //trigger granularity
                                      onp,     //trigger path
                                      action,  //action list in scheme
                                      name,    //trigger name operation
                                      ip,      //path to parent (inserting path)
                                      innode); //inserting node
        }
        else
        {
            qep = new PPCreateTrigger(dyn_cxt, //dynamic context
                                      peroot,  //PathExprRoot
                                      trgmod2te(n.a_mod, drv), //trigger event
                                      trgmod2tt(n.t_mod, drv), //trigger time
                                      trgmod2tg(n.g_mod, drv), //trigger granularity
                                      onp,     //trigger path
                                      action,  //action list in scheme
                                      name);   //trigger name operation
        }

        popDefaultSpace();

#else
        drv->error(SE1002, "Triggers support is disabled. Rebuild Sedna with enabled triggers.");
#endif
    }

    void lr2por::visit(ASTCreateUser &n)
    {
        qep = new PPCreateUser(PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.user, xs_string)), 1),
                               PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.psw, xs_string)), 1), dyn_cxt);
    }

    void lr2por::visit(ASTDDO &n)
    {
        childOffer off_this;

        n.expr->accept(*this);

        if (n.true_ddo)
            off_this.opin = PPOpIn(new PPDDO(dyn_cxt, createOperationInfo(n), getOffer().opin), 1);
        else
            off_this.opin = PPOpIn(new PPSXptr(dyn_cxt, createOperationInfo(n), getOffer().opin), 1);

        setOffer(off_this);
    }

    void lr2por::visit(ASTDeclareCopyNsp &n)
    {
        dyn_cxt->get_static_context()->set_namespace_preserve((n.pres_mod == ASTDeclareCopyNsp::PRESERVE) ? true : false);
        dyn_cxt->get_static_context()->set_namespace_inherit((n.pres_mod == ASTDeclareCopyNsp::INHERIT) ? true : false);
    }

    void lr2por::visit(ASTDefCollation &n)
    {
        try
        {
            dyn_cxt->get_static_context()->set_default_collation_uri(n.uri->c_str());
        }
        catch (SednaUserException &e) // invalid uri
        {
            drv->error(e.get_code(), e.getDescription().c_str());
        }
    }

    void lr2por::visit(ASTDefNamespaceDecl &n)
    {
        // we don't add default function namespace, since it is resolved in sema
        if (n.type == ASTDefNamespaceDecl::ELEMENT) {
            skn->setNamespace(xmlns_touch("", n.uri->c_str()));
        }
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
        // we ignore type and nillability from element here
        childOffer off_this;

        if (n.elem_test)
        {
            n.elem_test->accept(*this);
            off_this = getOffer();

            U_ASSERT(off_this.serialized_form.find("element") != std::string::npos);
            off_this.serialized_form.replace(off_this.serialized_form.find("element"), strlen("element"), "document-node");
            off_this.st.type.type = st_document_element;
        }
        else
        {
            off_this.serialized_form = "(document-node any)";
            off_this.st.type.type = st_document;
        }

        setOffer(off_this);
    }

    void lr2por::visit(ASTDropColl &n)
    {
        childOffer off_coll;

        n.coll->accept(*this);
        off_coll = getOffer();

        qep = new PPDropCollection(off_coll.opin, dyn_cxt);
    }

    void lr2por::visit(ASTDropDoc &n)
    {
        childOffer off_doc, off_coll;

        n.doc->accept(*this);
        off_doc = getOffer();

        if (n.coll)
        {
            n.coll->accept(*this);
            off_coll = getOffer();
        }

        if (n.coll)
            qep = new PPDropDocumentInCollection(off_doc.opin, off_coll.opin, dyn_cxt);
        else
            qep = new PPDropDocument(off_doc.opin, dyn_cxt);
    }

    void lr2por::visit(ASTDropFtIndex &n)
    {
#ifdef SE_ENABLE_FTSEARCH
        childOffer off_ind;

        n.index->accept(*this);

        off_ind = getOffer();

        qep = new PPDropFtIndex(off_ind.opin, dyn_cxt);
#else
        drv->error(SE1002, "full-text search support is disabled");
#endif
    }

    void lr2por::visit(ASTDropIndex &n)
    {
        childOffer off_ind;

        n.index->accept(*this);
        off_ind = getOffer();

        qep = new PPDropIndex(off_ind.opin, dyn_cxt);
    }

    void lr2por::visit(ASTDropMod &n)
    {
        PPOpIn mod = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.module, xs_string)), 1);
        qep = new PPDropModule(mod, dyn_cxt);
    }

    void lr2por::visit(ASTDropRole &n)
    {
        qep = new PPDropRole(PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.role, xs_string)), 1), dyn_cxt);
    }

    void lr2por::visit(ASTDropTrg &n)
    {
#ifdef SE_ENABLE_TRIGGERS
        PPOpIn name = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.trg, xs_string)), 1);
        qep = new PPDropTrigger(name, dyn_cxt);
#else
        drv->error(SE1002, "Triggers support is disabled. Rebuild Sedna with enabled triggers.");
#endif
    }

    void lr2por::visit(ASTDropUser &n)
    {
        qep = new PPDropUser(PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.user, xs_string)), 1), dyn_cxt);
    }

    void lr2por::visit(ASTElem &n)
    {
        arr_of_PPOpIn seq;
        size_t count = 0;
        PPOpIn content;
        childOffer off_this;
        PPOpIn ns;

        int sknMark = skn->mark();

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

        seq.reserve(count);

        while (count--) {
            childOffer offer = getOffer();
            seq.push_back(offer.opin);
        }

        if (seq.size() == 0) {
            content = PPOpIn(new PPNil(dyn_cxt, createOperationInfo(n)), 1);
        } else {
            std::reverse(seq.begin(), seq.end());

            content = PPOpIn(new PPSequence(dyn_cxt, createOperationInfo(n), seq), 1);
        }

        std::string name = n.pref->empty() ?
                           *n.local :
                           *n.pref + ":" + *n.local;

        off_this.opin.op = new PPElementConstructor(dyn_cxt, createOperationInfo(n), name.c_str(), content, n.deep_copy, skn->mark(), virtualizableConstructors);
        off_this.opin.ts = 1;

        setOffer(off_this);

        skn->rollbackToMark(sknMark);
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
            off_this.opin.op = new PPElementConstructor(dyn_cxt, createOperationInfo(n), off_name.opin, off_cont.opin, n.deep_copy, skn->mark(), virtualizableConstructors);
        }
        else
        {
            std::string name = n.pref->empty() ?
                               *n.local :
                               *n.pref + ":" + *n.local;

            off_this.opin.op = new PPElementConstructor(dyn_cxt, createOperationInfo(n), name.c_str(), off_cont.opin, n.deep_copy, skn->mark(), virtualizableConstructors);
        }

        off_this.opin.ts = 1;

        setOffer(off_this);
    }

    void lr2por::visit(ASTElementTest &n)
    {
        // we ignore type and nillability here for XPath and set it only for sequence types
        childOffer off_this;

        if (n.name)
        {
            n.name->accept(*this);
            off_this = getOffer();

            if (off_this.st.type.info.ea.nne == st_nne_name) {
                off_this.serialized_form = "(element " + off_this.serialized_form + ")";
            } else if (off_this.st.type.info.ea.nne == st_nne_wildcard) {
                off_this.serialized_form = "(element any)";
            } else {
              /* In this case --- do nothing. That means that it is not an element check. */
            }
        }
        else
        {
            off_this.serialized_form = "(element any)";
            off_this.st.type.info.ea.nne = st_nne_wildcard;
        }

        off_this.st.type.type = st_element;

        if (n.type)
        {
            childOffer off_type;

            n.type->accept(*this);
            off_type = getOffer();

            off_this.st.type.info.ea.type_name = off_type.st.type.info.single_type;
            off_this.st.type.info.ea.tne = (n.mod == ASTElementTest::ANY_NIL) ? st_tne_optional : st_tne_present;
        }
        else
        {
            off_this.st.type.info.ea.tne = st_tne_nothing;
        }

        setOffer(off_this);
    }

    void lr2por::visit(ASTEmptyTest &n)
    {
        childOffer off_this;

        off_this.st.type.type = st_item;

        setOffer(off_this);
    }

    void lr2por::visit(ASTError &n)
    {
        throw SYSTEM_EXCEPTION("If you see this, you are very unlucky. Anyway, this is an internal parser error.");
    }

    void lr2por::visit(ASTExtExpr &n)
    {
        // ignore pragmas
        childOffer off_this;

        n.expr->accept(*this);
        off_this = getOffer();

        setOffer(off_this);
    }

    void lr2por::visit(ASTFilterStep &n)
    {
        childOffer off_cont, off_this;
        PPOpIn expr;
        operation_info oi;

        oi = createOperationInfo(n);

        if (n.cont)
        {
            n.cont->accept(*this);
            off_cont = getOffer();
        }

        // look if we can create PPAbsPath
        if (!n.cont && pers_path_mode)
        {
            db_entity *dbe = new db_entity;
            dbe->type = dbe_document;
            dbe->name = new char[6];
            strcpy(dbe->name, "dummy");
            off_cont.opin = PPOpIn(new PPAbsPath(dyn_cxt, createOperationInfo(n), NULL, counted_ptr<db_entity>(dbe)), 1);
            off_cont.open_abs_path = true;
        }

        if (n.cont && (n.expr || n.preds || n.isLast) && off_cont.open_abs_path) // we need to close abs-path if we've got it as a context (exception, "." - expression)
        {
            finalizeAbsPath(dynamic_cast<PPAbsPath *>(off_cont.opin.op), off_cont.lr_path.c_str(), pers_path_mode);
            off_cont.open_abs_path = false;
        }

        // try to propagate abs-path via '.'-expression
        if (!n.expr && !n.preds && off_cont.opin.op)
        {
             if (off_cont.open_abs_path)
             {
                 off_this.opin = off_cont.opin;

                 if (!n.isLast)
                 {
                     off_this.lr_path = off_cont.lr_path;
                     off_this.open_abs_path = true;
                 }

                 setOffer(off_this);

                 return;
             }
        }

        // determine if we need sequence checker
        bool need_checker = isStepNeedsChecker(n);

        if (need_checker) // check context
            off_cont.opin.op = new PPSeqChecker(dyn_cxt, oi, off_cont.opin, PPSeqChecker::CHECK_NODE);

        // special case: just '.'
        if (!n.preds && !n.expr)
        {
            if (off_cont.opin.op)
                off_this.opin = off_cont.opin;
            else
                off_this.opin = getContextOffer(oi).opin;

            setOffer(off_this);

            return;
        }

        var_dsc cont_cxt; // step context
        var_dsc var_pos = INVALID_VAR_DSC, var_last = INVALID_VAR_DSC; // variables for last, positon to use in primary expression

        if (n.cont)
        {
            cont_cxt = getVarNum();

            bound_vars.push_back(l2pVarInfo("$%v", cont_cxt));
        }

        if (n.expr)
        {
            // set proper position and last variables if primary expression uses them
            // NOTE: if we've not got any context then we've got global context (which can only be one node).
            //       Since for now global context is not implemented I won't bother to provide implementation.
            //       However, it'll be straightforward -- check if we've got $%pos or/and $%last bound, anf if not, then assume '1' as result for
            //       these functions.
            if (n.use_pos && n.cont)
            {
                var_pos = getVarNum();
                bound_vars.push_back(l2pVarInfo("$%pos", var_pos));
            }

            if (n.use_last && n.cont)
            {
                var_last = getVarNum();
                bound_vars.push_back(l2pVarInfo("$%last", var_last));
            }

            childOffer off;
            n.expr->accept(*this);
            off = getOffer();

            expr = off.opin;
        }
        else
        {
            expr = getContextOffer(oi).opin;
        }


        // now we need to construct qep for XPath filter step
        if (n.preds)
        {
            PPOpIn preds = expr;

            for (unsigned int i = 0; i < n.preds->size(); i++)
            {
                childOffer off;
                var_dsc pred_cxt; // predicate context
                var_dsc pos_var, last_var;
                ASTPred *pred = dynamic_cast<ASTPred *>(n.preds->at(i));
                parentRequest req;

                pred_cxt = getVarNum();
                pos_var = last_var = INVALID_VAR_DSC;

                // bind context
                bound_vars.push_back(l2pVarInfo("$%v", pred_cxt));

                // bind pos if needed
                if (pred->usePosition())
                {
                    pos_var = getVarNum();
                    bound_vars.push_back(l2pVarInfo("$%pos", pos_var));
                }

                // bind last if needed
                if (pred->useLast())
                {
                    last_var = getVarNum();
                    bound_vars.push_back(l2pVarInfo("$%last", last_var));
                }

                req.pred_cxt = preds;
                setParentRequest(req);
                (*n.preds)[i]->accept(*this);
                off = getOffer();

                preds = off.opin;

                if (last_var != INVALID_VAR_DSC)
                    bound_vars.pop_back();

                if (pos_var != INVALID_VAR_DSC)
                    bound_vars.pop_back();

                bound_vars.pop_back();
            }

            // update expression
            expr = preds;
        }

        if (n.cont)
        {
            arr_of_var_dsc vars;
            vars.push_back(cont_cxt);

            if (var_last != INVALID_VAR_DSC)
            {
                off_cont.opin.op = new PPLast(dyn_cxt, oi, var_last, off_cont.opin);

                U_ASSERT(bound_vars.back().first == "$%last");
                bound_vars.pop_back();
            }

            // use PPStore to cache the expression
            if (n.expr && n.expr->isCached())
                expr.op = new PPStore(dyn_cxt, oi, expr);

            off_this.opin.op = new PPReturn(dyn_cxt, oi, vars, off_cont.opin, expr, var_pos);
            off_this.opin.ts = 1;

            if (var_pos != INVALID_VAR_DSC)
            {
                U_ASSERT(bound_vars.back().first == "$%pos");
                bound_vars.pop_back();
            }

            bound_vars.pop_back();
        }
        else
        {
            off_this.opin = expr;
        }

        if (n.isLast && !n.isFirstStep())
            off_this.opin.op = new PPSeqChecker(dyn_cxt, oi, off_this.opin, PPSeqChecker::CHECK_MIX);

        setOffer(off_this);
    }

    void lr2por::visit(ASTFLWOR &n)
    {
        ASTNodesVector::iterator it;
        size_t var_count = 0;
        std::vector<PPOpIn> fl_ops;
        childOffer off_ob, off_this, off_where;
        PPOpIn where, fl_close, nil;
        std::vector<l2pVarInfo> un_vars; // unique bindings to remedy (for $i ... for $i like situations)

        var_count = bound_vars.size();
        ASTVisitor::VisitNodesVector(n.fls, *this);
        var_count = bound_vars.size() - var_count;

        // if we've got order-by we'll process return-statement later with new bindings
        if (!n.order_by)
        {
            n.ret->accept(*this);
            off_this = getOffer();

            // try to PPStore-cache the node
            if (n.ret->isCached())
                off_this.opin.op = new PPStore(dyn_cxt, createOperationInfo(n), off_this.opin);
        }

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
        std::map<var_dsc, var_dsc> let_slet_vars; // maps let->slet variables in case of order-by

        for (size_t i = 0; i < n.fls->size(); i++)
        {
            size_t ind = n.fls->size() - i - 1;
            childOffer off = getOffer();
            arr_of_var_dsc vars;
            var_dsc pos_var = INVALID_VAR_DSC;
            bool use_position = false;

            // we use position only in 'for' with positional variable
            if (const ASTFor *f = dynamic_cast<const ASTFor *>((*n.fls)[ind]))
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

                    if (dynamic_cast<ASTLet *>((*n.fls)[ind])) // for let-clause remember the position
                        let_slet_vars[un_vars.back().second] = INVALID_VAR_DSC;
                }
            }
            bound_vars.pop_back();

            if (off.st.type.type == st_atomic_type && off.st.type.info.single_type == xs_anyType)
            {
                if (dynamic_cast<const ASTFor *>((*n.fls)[ind]))
                {
                    // check if we can cache previous for/let
                    if (ind < n.fls->size() - 1 && (*n.fls)[ind + 1]->isCached())
                        flop.op = new PPStore(dyn_cxt, createOperationInfo(*(*n.fls)[ind]), flop);

                    flop.op = new PPReturn(dyn_cxt, createOperationInfo(*(*n.fls)[ind]), vars, off.opin, flop, pos_var);
                }
                else
                {
                    flop.op = new PPLet(dyn_cxt, createOperationInfo(*(*n.fls)[ind]), vars, off.opin, flop);
                }
            }
            else
            {
                if (dynamic_cast<const ASTFor *>((*n.fls)[ind]))
                {
                    // check if we can cache previous for/let
                    if (ind < n.fls->size() - 1 && (*n.fls)[ind + 1]->isCached())
                        flop.op = new PPStore(dyn_cxt, createOperationInfo(*(*n.fls)[ind]), flop);

                    flop.op = new PPReturn(dyn_cxt, createOperationInfo(*(*n.fls)[ind]), vars, off.opin, flop, pos_var, off.st);
                }
                else
                {
                    flop.op = new PPLet(dyn_cxt, createOperationInfo(*(*n.fls)[ind]), vars, off.opin, flop, off.st);
                }
            }
        }

        if (n.order_by)
        {
            // here un_vars contains general PPReturn bindings (in reverse order)
            // since we introduce PPSLets here, return-statement must work with the same for-bindings and SLET-bindings (not un_vars let ones)
            PPOpIn ob, ret;
            arr_of_var_dsc vars(un_vars.size());
            bool isStable = dynamic_cast<const ASTOrderBy *>(n.order_by)->isStable();
            std::map<var_dsc, var_dsc>::iterator let_var;

            ob.op = new PPOrderBy(dyn_cxt, createOperationInfo(*n.order_by), isStable, flop, off_ob.orbs, un_vars.size());
            ob.ts = un_vars.size();

            // bind for-let variables with new bindings
            for (size_t i = 0; i < un_vars.size(); i++)
            {
                bound_vars.push_back(un_vars[i]);

                // check if we are dealing with a 'let' variable
                let_var = let_slet_vars.find(un_vars[i].second);
                if (let_var != let_slet_vars.end())
                {
                    var_dsc new_slet = getVarNum();
                    let_var->second = new_slet;
                    bound_vars.back().second = new_slet;
                }
            }

            n.ret->accept(*this);
            off_this = getOffer();

            // unbind variables
            bound_vars.erase(bound_vars.end() - un_vars.size(), bound_vars.end());

            ret = off_this.opin;

            // create slets and fill in order-by var descriptors
            for (size_t i = 0; i < un_vars.size(); i++)
            {
                // check if we are dealing with a 'let' variable
                let_var = let_slet_vars.find(un_vars[i].second);
                if (let_var != let_slet_vars.end())
                {
                    arr_of_var_dsc slet_vars;
                    U_ASSERT(let_var->second != INVALID_VAR_DSC);
                    slet_vars.push_back(let_var->second); // new binding for PPSLet

                    ret.op = new PPSLet(dyn_cxt, createOperationInfo(*n.ret), slet_vars,
                                    PPOpIn(new PPVariable(dyn_cxt, createOperationInfo(*n.ret), let_var->first), 1), ret);
                }

                vars[un_vars.size() - i - 1] = un_vars[i].second;
            }

            // Strange case for PPStore -- return-statement doesn't depend on all that for-let-order-by mess
            if (n.ret->isCached())
                ret.op = new PPStore(dyn_cxt, createOperationInfo(n), ret);

            off_this.opin.op = new PPReturn(dyn_cxt, createOperationInfo(n), vars, ob, ret, INVALID_VAR_DSC);
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
        childOffer off_this;
        bool stdFunc = (*n.int_name != "");
        size_t arity = (n.params ? n.params->size() : 0);

        if (stdFunc)
        {
            if (*n.int_name == "!fn!collection")
            {
                db_entity *dbe = new db_entity;
                dbe->type = dbe_collection;

                ASTLit *name = dynamic_cast<ASTLit *>((*n.params)[0]);

                if (name && name->type != ASTLit::STRING)
                {
                    drv->error(n.getLocation(), XPTY0004, "argument to fn:collection should be of xs:string");
                }

                setDefaultSpace(local_space_base);
                xpath::PathExpression *pe = new xpath::PathExpression();
                popDefaultSpace();

                if (name)
                {
                    dbe->name = new char[name->lit->size() + 1];
                    strcpy(dbe->name, name->lit->c_str());

                    off_this.opin = PPOpIn(new PPAbsPath(dyn_cxt, createOperationInfo(n), pe, counted_ptr<db_entity>(dbe)), 1);
                    off_this.open_abs_path = true;
                }
                else
                {
                    childOffer off;

                    (*n.params)[0]->accept(*this);
                    off = getOffer();

                    dbe->name = NULL;

                    off_this.opin = PPOpIn(new PPAbsPath(dyn_cxt, createOperationInfo(n), pe,
                            counted_ptr<db_entity>(dbe), off.opin), 1);
                    off_this.open_abs_path = true;
                }

                off_this.lr_path = "";
            }
            else if (*n.int_name == "!fn!document" && n.params->size() == 1)
            {
                db_entity *dbe = new db_entity;
                dbe->type = dbe_document;

                ASTLit *name = dynamic_cast<ASTLit *>((*n.params)[0]);

                if (name && name->type != ASTLit::STRING)
                {
                    drv->error(n.getLocation(), XPTY0004, "first argument to fn:document should be of xs:string");
                }

                setDefaultSpace(local_space_base);
                xpath::PathExpression *path_expr = new xpath::PathExpression();
                popDefaultSpace();

                if (name)
                {
                    dbe->name = new char[name->lit->size() + 1];
                    strcpy(dbe->name, name->lit->c_str());

                    off_this.opin = PPOpIn(new PPAbsPath(dyn_cxt, createOperationInfo(n), path_expr, counted_ptr<db_entity>(dbe)), 1);
                    off_this.open_abs_path = true;
                }
                else
                {
                    childOffer off;

                    (*n.params)[0]->accept(*this);
                    off = getOffer();

                    dbe->name = NULL;

                    off_this.opin = PPOpIn(new PPAbsPath(dyn_cxt, createOperationInfo(n), path_expr,
                            counted_ptr<db_entity>(dbe), off.opin), 1);
                    off_this.open_abs_path = true;
                }

                off_this.lr_path = "";
            }
            else if (*n.int_name == "!fn!index-scan")
            {
                childOffer off1, off2;
                PPOpIn dummy = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell("0", xs_integer)), 1);

                (*n.params)[0]->accept(*this);
                off1 = getOffer();

                (*n.params)[1]->accept(*this);
                off2 = getOffer();

                // last parameter is literal specifier (checked by sema)
                ASTLit *spec = dynamic_cast<ASTLit *>((*n.params)[2]);
                index_scan_condition isc;

                if (*spec->lit == "GT")
                    isc = isc_gt;
                else if (*spec->lit == "GE")
                    isc = isc_ge;
                else if (*spec->lit == "LT")
                    isc = isc_lt;
                else if (*spec->lit == "LE")
                    isc = isc_le;
                else // "EQ"
                    isc = isc_eq;

                off_this.opin = PPOpIn(new PPIndexScan(dyn_cxt, createOperationInfo(n), off1.opin, off2.opin, dummy, isc), 1);
            }
            else if (*n.int_name == "!fn!index-scan-between")
            {
                childOffer off1, off2, off3;

                (*n.params)[0]->accept(*this);
                off1 = getOffer();

                (*n.params)[1]->accept(*this);
                off2 = getOffer();

                (*n.params)[2]->accept(*this);
                off3 = getOffer();

                // last parameter is literal specifier (checked by sema)
                ASTLit *spec = dynamic_cast<ASTLit *>((*n.params)[3]);
                index_scan_condition isc;

                if (*spec->lit == "INT")
                    isc = isc_gt_lt;
                else if (*spec->lit == "SEG")
                    isc = isc_ge_le;
                else if (*spec->lit == "HINTL")
                    isc = isc_ge_lt;
                else // "HINTR"
                    isc = isc_gt_le;

                off_this.opin = PPOpIn(new PPIndexScan(dyn_cxt, createOperationInfo(n), off1.opin, off2.opin, off3.opin, isc), 1);
            }
            else if (*n.int_name == "!fn!position")
            {
                var_dsc pv = INVALID_VAR_DSC;

                // for position we need to find last bound pos var
                for (size_t i = 0; i < bound_vars.size(); i++)
                {
                    if (bound_vars[bound_vars.size() - i - 1].first == "$%pos")
                    {
                        pv = bound_vars[bound_vars.size() - i - 1].second;
                        break;
                    }
                }

                U_ASSERT(pv != INVALID_VAR_DSC);

                off_this.opin = PPOpIn(new PPVariable(dyn_cxt, createOperationInfo(n), pv), 1);
            }
            else if (*n.int_name == "!fn!last")
            {
                var_dsc lv = INVALID_VAR_DSC;

                // for position we need to find last bound pos var
                for (size_t i = 0; i < bound_vars.size(); i++)
                {
                    if (bound_vars[bound_vars.size() - i - 1].first == "$%last")
                    {
                        lv = bound_vars[bound_vars.size() - i - 1].second;
                        break;
                    }
                }

                U_ASSERT(lv != INVALID_VAR_DSC);

                off_this.opin = PPOpIn(new PPVariable(dyn_cxt, createOperationInfo(n), lv), 1);
            }
            else // all other standard functions are qeped in unified way
            {
                std::string name = "{" + *n.uri + "}" + *n.local;
                XQFunction *fu = drv->getStdFuncInfo(name);

                arr_of_PPOpIn para(n.params ? n.params->size() : 0);

                if (n.params)
                {
                    size_t count = n.params->size();
                    ASTVisitor::VisitNodesVector(n.params, *this);

                    while (count--)
                    {
                        childOffer off;

                        off = getOffer();
                        para[count] = off.opin;
                    }
                }

                try
                {
                    off_this.opin = fu->l2pGen(dyn_cxt, createOperationInfo(n), para);
                }
                catch (SednaUserException &e) // exceptions about turned off features
                {
                    for (size_t i = 0; i < para.size(); i++)
                        delete para[i].op;

                    off_this.opin = PPOpIn(NULL, 1);
                    drv->error(e.get_code(), e.getDescription().c_str());
                }
            }
        }
        else // user-defined function
        {
            std::string name = CREATE_INTNAME_FUN(*n.uri, *n.local, arity);

            // then find it in global functions
            function_id fid = getGlobalFunctionId(name);

            // fill out params
            arr_of_PPOpIn para(arity);

            if (n.params)
            {
                size_t count = arity;
                ASTVisitor::VisitNodesVector(n.params, *this);

                while (count--)
                {
                    childOffer off;

                    off = getOffer();
                    para[count] = off.opin;
                }
            }

            if (fid.second != INVALID_VAR_DSC)
            {
                off_this.opin = PPOpIn(new PPFunCall(dyn_cxt, createOperationInfo(n), para, fid), 1);
            }
            else // external function
            {
                try
                {
                    // NOTE: we ignore prefix-uri part for external functions
                    off_this.opin = PPOpIn(ext_function_manager.make_pp_ext_func(n.local->c_str(), dyn_cxt, createOperationInfo(n), para), 1);
                }
                catch (SednaUserException &e) // "external function not found"
                {
                    for (size_t i = 0; i < para.size(); i++)
                        delete para[i].op;

                    off_this.opin = PPOpIn(NULL, 1);
                    drv->error(n.getLocation(), e.get_code(), e.getDescription().c_str());
                }
            }
        }

        setOffer(off_this);
    }

    void lr2por::visit(ASTFuncDecl &n)
    {
        unsigned id;
        size_t arity = (n.params) ? n.params->size() : 0;
        XQFunction *xqf;
        std::string full_name = CREATE_INTNAME_FUN(*n.func_uri, *n.local, arity);

        mod->getFunctionInfo(full_name, &xqf);

        U_ASSERT(xqf);

        // if the function isn't needed then don't process it
        if (!xqf->is_used)
            return;

        // ignore external functions since they are treated only via fun-calls
        if (!n.body)
            return;

        // zero var-num since all functions use their own variable contexts
        dyn_cxt->reset_local_vars();

        // id was obtained earlier
        id = xqf->id.second;

        function_declaration fd;

        // some info for proper explain
        fd.func_name = (n.pref && *n.pref != "") ? *n.pref + ":" + *n.local : *n.local;
        fd.func_name_uri = *n.func_uri;

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

        n.body->accept(*this);
        fd.op = getOffer().opin.op;

        fd.dyn_cxt = dyn_cxt;
        fd.vars_total = dyn_cxt->get_local_vars_number();
        fd.num = arity;
        fd.var_map = dyn_cxt->get_var_map();

        // add function to context
        dyn_cxt->add_function(fd, id);

        // get rid of vars
        while (arity--)
            bound_vars.pop_back();
    }

    void lr2por::visit(ASTGrantPriv &n)
    {
        if (n.mod == ASTGrantPriv::DB)
        {
            qep = new PPGrantRevokePriv(PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.priv, xs_string)), 1),
                                        PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.user, xs_string)), 1), dyn_cxt, false);
        }
        else
        {
            qep = new PPGrantRevokePriv(PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.priv, xs_string)), 1),
                                            PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.obj, xs_string)), 1),
                                            PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.user, xs_string)), 1),
                                            (n.mod == ASTGrantPriv::DOCUMENT) ? "document" : "collection", dyn_cxt, false);
        }
    }

    void lr2por::visit(ASTGrantRole &n)
    {
        qep = new PPGrantRole(PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.role, xs_string)), 1),
                              PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.role_to, xs_string)), 1), dyn_cxt);
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
        childOffer off_e, off_t, off_this;

        n.expr->accept(*this);
        off_e = getOffer();

        n.type->accept(*this);
        off_t = getOffer();

        off_this.opin = PPOpIn(new PPInstanceOf(dyn_cxt, createOperationInfo(n), off_e.opin, off_t.st), 1);

        setOffer(off_this);
    }

    void lr2por::visit(ASTItemTest &n)
    {
        childOffer off_this;

        off_this.st.type.type = st_item;

        setOffer(off_this);
    }

    void lr2por::visit(ASTLet &n)
    {
        childOffer off_this;

        // we need to build expr before we bind variables since there could be same-name bindings
        n.expr->accept(*this);

        off_this = getOffer();

        setParamMode();
        n.tv->accept(*this);
        off_this.st = getOffer().st;
        unsetParamMode();

        setOffer(off_this);
    }

    void lr2por::visit(ASTLibModule &n)
    {
        n.prolog->accept(*this);
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
            default:
                type = xs_string;
                drv->error(SE4001, "unexpected literal node type");
        }

        // catch overflow exception
        try
        {
            tc = string2tuple_cell(*n.lit, type);
        }
        catch (SednaUserException &e)
        {
            drv->error(e.get_code(), e.getDescription().c_str());
            tc = string2tuple_cell("dummy", xs_string);
        }

        off_this.opin = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), tc), 1);

        setOffer(off_this);
    }

    void lr2por::visit(ASTLoadFile &n)
    {
        PPOpIn file, doc, coll;

        std::string *file_name = n.getFileName();

        file = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*file_name, xs_string)), 1);
        doc  = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.doc, xs_string)), 1);

        delete file_name;

        if (n.coll)
            coll = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.coll, xs_string)), 1);

        qep = new PPBulkLoad(file, doc, coll, dyn_cxt);
    }

    void lr2por::visit(ASTLoadModule &n)
    {
        arr_of_PPOpIn mods;
        ASTStringVector::iterator it;

        for (it = n.modules->begin(); it != n.modules->end(); it++)
        {
            mods.push_back(PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(**it, xs_string)), 1));
        }

        qep = new PPLoadModule(mods, n.mod == ASTLoadModule::REPLACE, dyn_cxt);
    }

    void lr2por::visit(ASTMainModule &n)
    {
        if (drv == NULL)
            throw SYSTEM_EXCEPTION("Driver is not set for lr2por analyzer!");

        n.prolog->accept(*this);
        n.query->accept(*this);
    }

    void lr2por::visit(ASTMetaCols &n)
    {
        PPOpIn coll; // dummy

        qep = new PPRetrieveMetadata(dbe_collection, coll, dyn_cxt, n.need_stats);
    }

    void lr2por::visit(ASTMetaDocs &n)
    {
        if (n.coll)
        {
            childOffer off_coll;

            n.coll->accept(*this);
            off_coll = getOffer();

            qep = new PPRetrieveMetadata(dbe_document, off_coll.opin, dyn_cxt, n.need_stats);
        }
        else
        {
            PPOpIn coll; // dummy

            qep = new PPRetrieveMetadata(dbe_document, coll, dyn_cxt, n.need_stats);
        }
    }

    void lr2por::visit(ASTMetaSchemaCol &n)
    {
        childOffer off_coll;

        n.coll->accept(*this);
        off_coll = getOffer();

        qep = new PPRetrieveDS(off_coll.opin, dyn_cxt, dbe_collection);
    }

    void lr2por::visit(ASTMetaSchemaDoc &n)
    {
        childOffer off_doc;

        n.doc->accept(*this);
        off_doc = getOffer();

        qep = new PPRetrieveDS(off_doc.opin, dyn_cxt, dbe_document);
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
        childOffer off_this;

        if (*n.pref == "*" && *n.local == "*")
        {
            off_this.serialized_form = "(wildcard_star)";
            off_this.st.type.info.ea.nne = st_nne_wildcard;
        }
        else if (*n.pref == "*")
        {
            off_this.serialized_form = "(wildcard_star_ncname \"" + *n.local + "\")";
            off_this.st.type.info.ea.nne = st_nne_other;
        }
        else if (*n.local == "*")
        {
            off_this.serialized_form = "(wildcard_ncname_star \"" + *n.uri + "\")";
            off_this.st.type.info.ea.nne = st_nne_other;
        }
        else
        {
            xsd::QName qname = xsd::QName::createUPL(n.uri->c_str(), n.pref->c_str(), n.local->c_str());

            // If this one is enclosed in another test, return
            ASTNode * parent = getParent();
            if (parent == NULL || (
                  dynamic_cast<ASTElementTest *>(parent) == NULL &&
                  dynamic_cast<ASTAttribTest *>(parent) == NULL &&
                  dynamic_cast<ASTSchemaElemTest *>(parent) == NULL &&
                  dynamic_cast<ASTSchemaAttrTest *>(parent) == NULL))
            {
                off_this.serialized_form = "(qname " + qname.toLRString() + ")";
            }
            else
            {
                // Warning: different semantics of returned expression here !
                off_this.serialized_form = qname.toLRString();
            }

            off_this.st.type.info.ea.nne = st_nne_name;
            off_this.st.type.info.ea.qname = qname.serialize(local_space_base);
        }

        setOffer(off_this);
    }

    void lr2por::visit(ASTNamespaceDecl &n)
    {
        xmlns_ptr ns = xmlns_touch(n.name->c_str(), n.uri->c_str());

        dyn_cxt->get_static_context()->addPrologueNamespace(ns);
        skn->setNamespace(ns);
    }

    void lr2por::visit(ASTNodeTest &n)
    {
        childOffer off_this;

        off_this.serialized_form = "(node)";

        off_this.st.type.type = st_node;

        setOffer(off_this);
    }

    void lr2por::visit(ASTNsp &n)
    {
        childOffer off_this;
        PPOpIn cont;
        tuple_cell tc;

        tc = string2tuple_cell(n.cont ? *n.cont : "", xs_string);
        cont = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), tc), 1);

        U_ASSERT(dynamic_cast<ASTElem *>(getParent()) != NULL);

        U_ASSERT(n.name != NULL);
        skn->setNamespace(xmlns_touch(n.name->c_str(), n.cont != NULL ? n.cont->c_str() : ""));

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
                    dyn_cxt->get_static_context()->set_output_method(se_output_method_xml);
                else if (it->first == "indent" && it->second == "yes")
                    dyn_cxt->get_static_context()->set_output_indent(true);
                else if (it->first == "indent" && it->second == "no")
                    dyn_cxt->get_static_context()->set_output_indent(false);
                else if (it->first == "cdata-section-elements") {
                    size_t i = 0;

                    const std::string & value = it->second;
                    std::string::size_type start = 0, end = 0;

                    end = it->second.find_first_not_of(' ', start);

                    while (end != std::string::npos) {
                        std::string::size_type len;

                        start = it->second.find_first_not_of(' ', end);
                        end = it->second.find_first_of(' ', start);

                        if (end == std::string::npos) {
                            len = value.length() - start;
                        } else {
                            len = end - start;
                        }

                        dyn_cxt->add_cdata_section_element(xsd::QName::createResolve(value.substr(start, len).c_str(), skn));
                    }
                }
            }
        }
        else if (*n.local == "bulk-load")
        {
            for (it = n.options->begin(); it != n.options->end(); it++) {
                dyn_cxt->sc()->setLocalOption("bulk-load-" + it->first, it->second);
            }
        }
        else if (*n.local == "character-map")
        {
            for (it = n.options->begin(); it != n.options->end(); it++)
            {
                dyn_cxt->add_char_mapping(it->first, it->second);
            }
        }
    }

    void lr2por::visit(ASTOrdExpr &n)
    {
        childOffer off_this;

        n.expr->accept(*this);
        off_this = getOffer();

        setOffer(off_this);
    }

    void lr2por::visit(ASTOrder &n)
    {
        dyn_cxt->get_static_context()->set_ordering_mode((n.mod == ASTOrder::ORDERED) ? xq_ordering_mode_ordered : xq_ordering_mode_unordered);
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

    void lr2por::visit(ASTOrderEmpty &n)
    {
        dyn_cxt->get_static_context()->set_empty_order ((n.mod == ASTOrderEmpty::EMPTY_GREATEST) ? xq_empty_order_greatest : xq_empty_order_least);
    }

    void lr2por::visit(ASTOrderMod &n)
    {
        childOffer off_this;
        orb_modifier orb;

        off_this.orbs = arr_of_orb_modifier(1);

        if (n.ad_mod)
        {
            n.ad_mod->accept(*this);
            off_this = getOffer();

            orb.order = off_this.orbs[0].order;
        }
        else
        {
            orb.order = orb_modifier::ORB_ASCENDING;
        }

        if (n.em_mod)
        {
            n.em_mod->accept(*this);
            off_this = getOffer();

            orb.status = off_this.orbs[0].status;
        }
        else
        {
            orb.status = dyn_cxt->get_static_context()->get_empty_order() == xq_empty_order_least ?
                                                      orb_modifier::ORB_EMPTY_LEAST :
                                                      orb_modifier::ORB_EMPTY_GREATEST;
        }

        if (n.col_mod)
        {
            n.col_mod->accept(*this);
            off_this = getOffer();

            orb.collation = off_this.orbs[0].collation;
        }
        else
        {
            orb.collation = dyn_cxt->get_static_context()->get_default_collation();
        }

        off_this.orbs[0] = orb;

        setOffer(off_this);
    }

    void lr2por::visit(ASTOrderModInt &n)
    {
        childOffer off_this;

        off_this.orbs = arr_of_orb_modifier(1);

        switch (n.mod)
        {
            case ASTOrderModInt::ASCENDING:
                off_this.orbs[0].order = orb_modifier::ORB_ASCENDING;
                break;

            case ASTOrderModInt::DESCENDING:
                off_this.orbs[0].order = orb_modifier::ORB_DESCENDING;
                break;

            case ASTOrderModInt::EMPTY_GREATEST:
                off_this.orbs[0].status = orb_modifier::ORB_EMPTY_GREATEST;
                break;

            case ASTOrderModInt::EMPTY_LEAST:
                off_this.orbs[0].status = orb_modifier::ORB_EMPTY_LEAST;
                break;

            case ASTOrderModInt::COLLATION:
                int res = dyn_cxt->get_static_context()->get_collation(n.uri->c_str(), &(off_this.orbs[0].collation));

                if(res != 0)
                {
                    // Given URI is invalid
                    if (res == COLLATION_INVALID_URI)
                        drv->error(n.getLocation(), XQST0046, n.uri->c_str());
                    else // There is no such collation, or it could not be properly resolved
                        drv->error(n.getLocation(), XQST0076, n.uri->c_str());
                }
                break;
        }

        setOffer(off_this);
    }

    void lr2por::visit(ASTOrderSpec &n)
    {
        childOffer off, off_this;

        n.expr->accept(*this);
        off = getOffer();

        off_this.opin = off.opin;

        if (n.mod)
        {
            n.mod->accept(*this);
            off = getOffer();

            off_this.orbs = off.orbs;
        }
        else
        {
            orb_modifier orb;

            orb.order = orb_modifier::ORB_ASCENDING;
            orb.status = dyn_cxt->get_static_context()->get_empty_order() == xq_empty_order_least ?
                                                      orb_modifier::ORB_EMPTY_LEAST :
                                                      orb_modifier::ORB_EMPTY_GREATEST;
            orb.collation = dyn_cxt->get_static_context()->get_default_collation();

            off_this.orbs = arr_of_orb_modifier(1);
            off_this.orbs[0] = orb;
        }

        setOffer(off_this);
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
        childOffer off_this;

        off_this.st.type.type = st_pi;

        if (n.type == ASTPiTest::NONE)
        {
            off_this.serialized_form = "(processing-instruction any)";
            off_this.st.type.info.ncname = NULL;
        }
        else
        {
            xsd::NCName name = xsd::NCName::check(n.test->c_str(), false);
            off_this.serialized_form = "(processing-instruction " + name.toLRString() + ")";
            off_this.st.type.info.ncname = name.serialize(local_space_base);
        }

        setOffer(off_this);
    }

    void lr2por::visit(ASTPosVar &n)
    {
        // no childOffer here
        if (param_mode)
        {
            n.var->accept(*this);
        }
    }

    void lr2por::visit(ASTPragma &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTPred &n)
    {
        bool use_last = n.useLast();
        bool use_pos = n.usePosition();
        var_dsc pos_var = INVALID_VAR_DSC, last_var = INVALID_VAR_DSC, cxt_var = INVALID_VAR_DSC;
        childOffer off, off_this;
        operation_info oip;

        oip = createOperationInfo(n);

        for (size_t i = 0; i < bound_vars.size(); i++)
        {
            size_t ind = bound_vars.size() - i - 1;

            if (last_var == INVALID_VAR_DSC && bound_vars[ind].first == "$%last")
            {
                last_var = bound_vars[ind].second;
            }
            else if (pos_var == INVALID_VAR_DSC && bound_vars[ind].first == "$%pos")
            {
                pos_var = bound_vars[ind].second;
            }
            else if (cxt_var == INVALID_VAR_DSC && bound_vars[ind].first == "$%v")
            {
                cxt_var = bound_vars[ind].second;
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
                        cond, off.opin, (n.others.size() == 1) ? !n.others[0].use_cxt : true,  last_var);
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
        ASTVisitor::VisitNodesVector(n.decls, *this);
    }

    void lr2por::visit(ASTQName &n)
    {
        childOffer off_this;

        off_this.opin.op = new PPConst(dyn_cxt, createOperationInfo(n),
            tuple_cell::atomic(xsd::QName::createUPL(n.uri->c_str(), n.pref->c_str(), n.local->c_str())));
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

        if (n.type == ASTQuantExpr::SOME)
        {
            off_this.opin = PPOpIn(new PPFnExists(dyn_cxt, createOperationInfo(n), op), 1);
        }
        else /* EVERY */
        {
            off_this.opin = PPOpIn(new PPFnEmpty(dyn_cxt, createOperationInfo(n), op), 1);
        }

        setOffer(off_this);

        bound_vars.pop_back();
    }

    void lr2por::visit(ASTQuery &n)
    {
        // local variable context for the body
        dyn_cxt->reset_local_vars();

        virtualizableConstructors = true;

        // if we deserealize trigger statement then bind special vars
        if (n.is_trigger)
        {
            bound_vars.push_back(l2pVarInfo("NEW", 0));
            bound_vars.push_back(l2pVarInfo("OLD", 0));
            bound_vars.push_back(l2pVarInfo("WHERE", 0));
        }

        n.query->accept(*this);

        if (n.type == ASTQuery::QUERY)
        {
            childOffer off = getOffer();

            if (is_subquery)
                qep = new PPSubQuery(dyn_cxt, off.opin);
            else
                qep = new PPQueryRoot(dyn_cxt, off.opin);
        }

        // finalize producers
        dyn_cxt->set_producers();

        if (mod->turnedExplain()) // explain feature
        {
            dynamic_context *old_dyn_cxt = dyn_cxt;

            // first, we need new dynamic context since we will use two root operations
            dyn_cxt = new dynamic_context(new static_context());

            // then, we build PPQueryRoot->PPExplain on top of actual query
            PPOpIn expl = PPOpIn(new PPExplain(dyn_cxt, createOperationInfo(n), qep,
                                 old_dyn_cxt), 1);
            qep = new PPQueryRoot(dyn_cxt, expl);
        }
        else if (mod->turnedProfile()) // profile feature
        {
            dynamic_context *old_dyn_cxt = dyn_cxt;

            // first, we need new dynamic context since we will use two root operations
            dyn_cxt = new dynamic_context(new static_context());

            // then, we build PPQueryRoot->PPExplain in profile mode on top of actual query
            PPOpIn expl = PPOpIn(new PPExplain(dyn_cxt, createOperationInfo(n), qep,
                                 old_dyn_cxt, true), 1);
            qep = new PPQueryRoot(dyn_cxt, expl);
        }
    }

    void lr2por::visit(ASTRenameColl &n)
    {
        childOffer off_old, off_new;

        n.name_old->accept(*this);
        off_old = getOffer();

        n.name_new->accept(*this);
        off_new = getOffer();

        qep = new PPRename(off_old.opin, off_new.opin, dyn_cxt);
    }

    void lr2por::visit(ASTRevokePriv &n)
    {
        if (n.mod == ASTRevokePriv::DB)
        {
            qep = new PPGrantRevokePriv(PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.priv, xs_string)), 1),
                                        PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.user, xs_string)), 1), dyn_cxt, true);
        }
        else
        {
            qep = new PPGrantRevokePriv(PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.priv, xs_string)), 1),
                                            PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.obj, xs_string)), 1),
                                            PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.user, xs_string)), 1),
                                            (n.mod == ASTRevokePriv::DOCUMENT) ? "document" : "collection", dyn_cxt, true);
        }
    }

    void lr2por::visit(ASTRevokeRole &n)
    {
        qep = new PPRevokeRole(PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.role, xs_string)), 1),
                               PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.role_from, xs_string)), 1), dyn_cxt);
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
        childOffer off_this;
        size_t count = n.exprs->size();

        if (count == 0)
        {
            off_this.opin = PPOpIn(new PPNil(dyn_cxt, createOperationInfo(n)), 1);
        }
        else
        {
            ASTVisitor::VisitNodesVector(n.exprs, *this);

            arr_of_PPOpIn ops(count);

            while (count--)
                ops[count] = getOffer().opin;

            off_this.opin = PPOpIn(new PPSequence(dyn_cxt, createOperationInfo(n), ops), ops[0].ts);
        }

        setOffer(off_this);
    }

    void lr2por::visit(ASTSpaceSeq &n)
    {
        childOffer off_this;

        n.expr->accept(*this);

        arr_of_PPOpIn ops;

        ops.push_back(getOffer().opin);

        off_this.opin = PPOpIn(new PPSpaceSequence(dyn_cxt, createOperationInfo(n), ops, n.atomize), ops[0].ts);

        setOffer(off_this);
    }

    void lr2por::visit(ASTTextConst &n)
    {
        childOffer off_this, off_cont;

        n.expr->accept(*this);
        off_cont = getOffer();

        off_this.opin.op = new PPTextConstructor(dyn_cxt, createOperationInfo(n), off_cont.opin, n.deep_copy, false);
        off_this.opin.ts = 1;

        setOffer(off_this);
    }

    void lr2por::visit(ASTTextTest &n)
    {
        childOffer off_this;

        off_this.serialized_form = "(text)";
        off_this.st.type.type = st_text;

        setOffer(off_this);
    }

    void lr2por::visit(ASTTreat &n)
    {
        childOffer off_e, off_t, off_this;

        n.expr->accept(*this);
        off_e = getOffer();

        n.type->accept(*this);
        off_t = getOffer();

        off_this.opin = PPOpIn(new PPTreat(dyn_cxt, createOperationInfo(n), off_e.opin, off_t.st), 1);

        setOffer(off_this);
    }

    void lr2por::visit(ASTType &n)
    {
        childOffer off_this;
        std::string *pref, *loc;

        ASTParseQName(n.name, &pref, &loc);

        off_this.st.type.type = st_atomic_type;
        off_this.st.type.info.single_type = drv->getXsType(loc->c_str());

        delete pref;
        delete loc;

        setOffer(off_this);
    }

    void lr2por::visit(ASTTypeSeq &n)
    {
        childOffer off_this;

        n.type_test->accept(*this);
        off_this = getOffer();

        switch (n.mod)
        {
            case ASTTypeSeq::EMPTY:
                off_this.st.oi = st_empty;
                break;
            case ASTTypeSeq::ONE:
                off_this.st.oi = st_one;
                break;
            case ASTTypeSeq::OPT:
                off_this.st.oi = st_optional;
                break;
            case ASTTypeSeq::ZERO_OR_MORE:
                off_this.st.oi = st_zero_or_more;
                break;
            case ASTTypeSeq::ONE_OR_MORE:
                off_this.st.oi = st_one_or_more;
                break;
        }

        setOffer(off_this);
    }

    void lr2por::visit(ASTTypeSingle &n)
    {
        childOffer off_this;

        n.type->accept(*this);
        off_this = getOffer();

        off_this.st.oi = (n.mod == ASTTypeSingle::ONE ? st_one : st_optional);

        setOffer(off_this);
    }

    void lr2por::visit(ASTTypeSwitch &n)
    {
        childOffer off_this, off;
        size_t count = n.cases->size();

        // first, we need to bind the expression
        n.expr->accept(*this);
        PPOpIn main_e = getOffer().opin;

        arr_of_var_dsc vars;

        var_dsc main_var = getVarNum();

        bound_vars.push_back(l2pVarInfo("$%ts", main_var));
        vars.push_back(main_var);

        ASTVisitor::VisitNodesVector(n.cases, *this);

        arr_of_PPOpIn cases(count);
        arr_of_sequence_type types(count);

        // cases are now in stack in reverse order
        while (count--)
        {
            off = getOffer();

            cases[count] = off.opin;
            types[count] = off.st;
        }

        // default case
        n.def_case->accept(*this);
        off = getOffer();

        // NOTE on tuple size; Scheme part states that typeswitch tuple size equals main expression tuple size
        // this seems weird since typeswitch returns effective case; since cases always return ExprSingle I make tuple size to be 1
        off_this.opin = PPOpIn(new PPTypeswitch(dyn_cxt, createOperationInfo(n), vars, main_e, types, cases, off.opin), 1);

        setOffer(off_this);

        // get rid of main binding
        U_ASSERT(bound_vars.back().first == "$%ts");
        bound_vars.pop_back();
    }

    void lr2por::visit(ASTTypeVar &n)
    {
        childOffer off_this;

        if (param_mode)
        {
            n.var->accept(*this);
            n.type->accept(*this);

            off_this = getOffer();
        }

        setOffer(off_this);
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
        childOffer off_what;

        n.what->accept(*this);
        off_what = getOffer();

        if (n.type == ASTUpdDel::DEEP)
            qep = new PPDeleteDeep(off_what.opin, dyn_cxt);
        else
            qep = new PPDeleteUndeep(off_what.opin, dyn_cxt);
    }

    void lr2por::visit(ASTUpdInsert &n)
    {
        childOffer off_where, off_what;

        virtualizableConstructors = false;

        n.what->accept(*this);
        off_what = getOffer();

        n.where->accept(*this);
        off_where = getOffer();

        switch (n.type)
        {
            case ASTUpdInsert::INTO:
                qep = new PPInsertTo(off_what.opin, off_where.opin, dyn_cxt);
                break;
            case ASTUpdInsert::PRECEDING:
                qep = new PPInsertBefore(off_what.opin, off_where.opin, dyn_cxt);
                break;
            case ASTUpdInsert::FOLLOWING:
                qep = new PPInsertFollowing(off_what.opin, off_where.opin, dyn_cxt);
                break;
        }
    }

    void lr2por::visit(ASTUpdMove &n)
    {
        drv->error(SE4001, "update move is not supported");
    }

    void lr2por::visit(ASTUpdRename &n)
    {
        childOffer off_what;

        n.what->accept(*this);
        off_what = getOffer();

        setDefaultSpace(local_space_base);

        xsd::NCName prefix(xsd::materialize(n.pref->c_str()));
        xsd::NCName local(xsd::materialize(n.local->c_str()));

        popDefaultSpace();

        qep = new PPRename(off_what.opin, dyn_cxt, prefix, local);
    }

    void lr2por::visit(ASTUpdReplace &n)
    {
        childOffer off_what, off_new, off_var;
        arr_of_var_dsc retv;
        arr_of_PPOpIn new_seq;
        PPOpIn newop;

        setParamMode();
        n.var->accept(*this);
        off_var = getOffer();
        unsetParamMode();

        bool got_type = !(off_var.st.type.type == st_atomic_type && off_var.st.type.info.single_type == xs_anyType);

        virtualizableConstructors = false;

        n.what->accept(*this);
        off_what = getOffer();

        n.new_expr->accept(*this);
        off_new = getOffer();

        // main return variable
        retv.push_back(bound_vars.back().second);

        // check if we can PPStore-cache new-expression
        if (n.new_expr->isCached())
            off_new.opin.op = new PPStore(dyn_cxt, createOperationInfo(n), off_new.opin);

        // sequence on each updated node (data_child for return)
        new_seq.push_back(PPOpIn(new PPVariable(dyn_cxt, createOperationInfo(n), retv.back()), 1));
        new_seq.push_back(off_new.opin);
        new_seq.push_back(PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell("1", se_separator)), 1));

        newop = PPOpIn(new PPSequence(dyn_cxt, createOperationInfo(n), new_seq), 1);

        if (got_type)
            qep = new PPReplace(PPOpIn(new PPReturn(dyn_cxt, createOperationInfo(n), retv, off_what.opin, newop, INVALID_VAR_DSC, off_var.st), 1), dyn_cxt);
        else
            qep = new PPReplace(PPOpIn(new PPReturn(dyn_cxt, createOperationInfo(n), retv, off_what.opin, newop, INVALID_VAR_DSC), 1), dyn_cxt);

        bound_vars.pop_back();
    }

    void lr2por::visit(ASTVar &n)
    {
        std::string name = CREATE_INTNAME(*n.uri, *n.local);
        childOffer off_this;
        operation_info oi;

        if (param_mode)
        {
            var_dsc vid = getVarNum();
            std::string exp_name = (n.pref && *(n.pref) != "") ? *(n.pref) + ":" + *(n.local) : *(n.local);

            dyn_cxt->add_to_var_map(vid, var_name_exp(exp_name, *n.uri));

            bound_vars.push_back(l2pVarInfo(name, vid));

            return;
        }

        oi = createOperationInfo(n);

        // in usual mode we must resolve name to the id given
        // first, check if variable is bound
        if (bound_vars.size() > 0)
        {
            for (size_t i = 0; i < bound_vars.size(); i++)
            {
                size_t ind = bound_vars.size() - i - 1;

                if (bound_vars[ind].first == name)
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
                        off_this.opin.op = new PPVariable(dyn_cxt, oi, bound_vars[ind].second);
                        off_this.opin.ts = 1;
                    }

                    setOffer(off_this);

                    return;
                }
            }
        }

        // then find it in globals
        global_var_dsc id = getGlobalVariableId(name);

        PPGlobalVariable *pgv = new PPGlobalVariable(dyn_cxt, oi, id);
        off_this.opin.op = pgv;
        off_this.opin.ts = 1;

        setOffer(off_this);
    }

    void lr2por::visit(ASTVarDecl &n)
    {
        childOffer off, off_this, off_type;
        var_dsc id;
        PPVarIterator *var;
        operation_info oi;
        global_producer var_prod;
        XQVariable *xqv;
        const ASTVar *var_info = dynamic_cast<const ASTVar *>(n.var);

        U_ASSERT(var_info);
        std::string full_name = CREATE_INTNAME(*var_info->uri, *var_info->local);

        mod->getVariableInfo(full_name, &xqv);

        U_ASSERT(xqv);

        // if the variable isn't needed then don't process it
        if (!xqv->is_used)
            return;

        // create new variable context for global variable
        dyn_cxt->reset_local_vars();

        // analyze the type
        if (n.type)
        {
            n.type->accept(*this);
            off_type = getOffer();
        }

        // analyze the body
        n.expr->accept(*this);
        off = getOffer();

        id = xqv->id.second;
        oi = createOperationInfo(n);

        if (n.type)
            var = new PPVarDecl(dyn_cxt, oi, id, off.opin, off_type.st);
        else
            var = new PPVarDecl(dyn_cxt, oi, id, off.opin);

        var_prod.op = var;
        var_prod.cxt = dyn_cxt;

        // some info for proper explain
        var_prod.var_name = (var_info->pref && *(var_info->pref) != "") ?
                                                                *(var_info->pref) + ":" + *(var_info->local) :
                                                                *(var_info->local);
        var_prod.var_name_uri = *(var_info->uri);

        dyn_cxt->add_global_var(var_prod, id);
        dyn_cxt->set_producers();
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

    global_var_dsc lr2por::getGlobalVariableId(const std::string &name)
    {
        varInfo::iterator it;
        global_var_dsc id;
        XQVariable *xqv;

        // first,look in the cache
        it = varCache.find(name);

        if (it != varCache.end())
            return it->second;

        // then, try to process it as a local one
        if (mod->getVariableInfo(name, &xqv))
        {
            varCache[name] = xqv->id;
            return xqv->id;
        }

        // else, the variable is defined in some of the library modules
        id = drv->getGlobalVariableId(name);

        // since we've obtained this info from driver we should locally cache it
        varCache[name] = id;

        return id;
    }

    function_id lr2por::getGlobalFunctionId(const std::string &name)
    {
        XQFunction *xqf;
        funcInfo::iterator it;
        function_id id;

        // first,look in cache
        it = funcCache.find(name);

        if (it != funcCache.end())
            return it->second;

        // then, try to process it as a local one
        if (mod->getFunctionInfo(name, &xqf))
        {
            funcCache[name] = xqf->id;
            return xqf->id;
        }

        // else, the function is defined in some of the library modules
        id = drv->getGlobalFunctionId(name);

        // since we've obtained this info from driver we should locally cache it
        funcCache[name] = id;

        return id;
    }

    lr2por::childOffer lr2por::getContextOffer(operation_info oi) const
    {
        var_dsc id = INVALID_VAR_DSC;
        childOffer off_this;

        for (size_t i = 0; i < bound_vars.size(); i++)
        {
            if (bound_vars[bound_vars.size() - i - 1].first == "$%v")
            {
                id = bound_vars[bound_vars.size() - i - 1].second;
                break;
            }
        }

        U_ASSERT(id != INVALID_VAR_DSC);

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

                if (fs && !fs->expr) // don't propagate sequence checking for contex-item steps
                    need_checker = false;
                else // here we've got either some expression or filter-step (not context one)
                    need_checker = true;
            }
        }

        return need_checker;
    }

    std::string lr2por::getlrForAxisStep(const ASTAxisStep &s)
    {
        std::string res = "(test ";
        childOffer off;

        switch (s.axis) {
            case ASTAxisStep::CHILD: res += "child"; break;
            case ASTAxisStep::ATTRIBUTE: res += "attribute"; break;

            case ASTAxisStep::SELF:  res += "self"; break;
            case ASTAxisStep::PARENT:  res += "parent"; break;

            case ASTAxisStep::DESCENDANT: res += "descendant"; break;
            case ASTAxisStep::DESCENDANT_OR_SELF: res += "descendant-or-self"; break;
            case ASTAxisStep::DESCENDANT_ATTRIBUTE: res += "descendant-attribute"; break;

            case ASTAxisStep::ANCESTOR:  res += "ancestor"; break;
            case ASTAxisStep::ANCESTOR_OR_SELF:  res += "ancestor-or-self"; break;

            case ASTAxisStep::PRECEDING:  res += "preceding"; break;
            case ASTAxisStep::PRECEDING_SIBLING:  res += "preceding-sibling"; break;
            case ASTAxisStep::FOLLOWING: res += "following"; break;
            case ASTAxisStep::FOLLOWING_SIBLING: res += "following-sibling"; break;
        }

        s.test->accept(*this);
        off = getOffer();

        res += " ";
        res += off.serialized_form;
        res += ")";

        return res;
    }

    void lr2por::finalizeAbsPath(PPAbsPath *pap, const char *lr, bool pers)
    {
        // null-abspath -- ignore
        if (!strlen(lr))
            return;

        // make it list-like
        std::string lr_list = std::string("(xpath ") + lr + ")";

        setDefaultSpace(pers ? catalog_space_base : local_space_base);
        xpath::PathExpression * pe = new xpath::PathExpression(lr_list.c_str(), dyn_cxt);
        popDefaultSpace();
        pap->setPathExpr(pe);
    }

    PPOpIn lr2por::getPPForAxis(const ASTAxisStep &s, PPOpIn cont, operation_info oi)
    {
        childOffer off;
        PPOpIn op;

        setDefaultSpace(local_space_base);
        xpath::NodeTest nodeTest(getlrForAxisStep(s).c_str());
        popDefaultSpace();

        op.op = new PPAxisStep(dyn_cxt, oi, cont, nodeTest);

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

    var_dsc lr2por::getVarNum()
    {
        return dyn_cxt->get_new_var_id();
    }

    CalcOp *lr2por::make_CalcOp(ASTNode *n, bool logical)
    {
        CalcOp *res;
        ASTBop *b;

        if ((b = dynamic_cast<ASTBop *>(n)) && (b->op >= ASTBop::OR && b->op <= ASTBop::GE_V && b->op != ASTBop::TO))
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
        xq_binary_op_type op_type;

        switch (n.op)
        {
            case ASTBop::AND:
                op_tree = new BinaryOpAnd(lop, rop);
                return;
            case ASTBop::OR:
                op_tree = new BinaryOpOr(lop, rop);
                return;

            case ASTBop::EQ_V:
                op_type = xqbop_eq;
                break;
            case ASTBop::NE_V:
                op_type = xqbop_ne;
                break;
            case ASTBop::LT_V:
                op_type = xqbop_lt;
                break;
            case ASTBop::LE_V:
                op_type = xqbop_le;
                break;
            case ASTBop::GT_V:
                op_type = xqbop_gt;
                break;
            case ASTBop::GE_V:
                op_type = xqbop_ge;
                break;
            case ASTBop::PLUS:
                op_type = xqbop_add;
                break;
            case ASTBop::MINUS:
                op_type = xqbop_sub;
                break;
            case ASTBop::MULT:
                op_type = xqbop_mul;
                break;
            case ASTBop::DIV:
                op_type = xqbop_div;
                break;
            case ASTBop::IDIV:
                op_type = xqbop_idiv;
                break;
            case ASTBop::MOD:
                op_type = xqbop_mod;
                break;

            default:
                op_type = xqbop_eq;
                drv->error(SE4001, "make_binary_op cannot process the operation");
        }

        r = get_binary_op(op_type, lt, rt);

        if (r.collation)
            op_tree = new BinaryOpCollation(lop, rop, r.f.bf_c, op_type);
        else
            op_tree = new BinaryOp(lop, rop, r.f.bf, op_type);
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
                op_tree = new UnaryOp(lop, get_unary_op(xquop_plus, lt), xquop_plus);
                break;

            case ASTUop::MINUS:
                op_tree = new UnaryOp(lop, get_unary_op(xquop_minus, lt), xquop_minus);
                break;
        }
    }
}
