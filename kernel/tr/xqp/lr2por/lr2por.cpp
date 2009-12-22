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
        dyn_cxt = new dynamic_context(st_cxt, 0);
        dyn_cxt->set_producers(1);

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
        // we ignore type here except for sequence types
        childOffer off_this;

        if (n.name)
        {
            n.name->accept(*this);
            off_this = getOffer();
        }
        else
        {
            off_this.test_data = "()";
            off_this.test_type = "wildcard_star";

            off_this.st.type.info.ea.nne = st_nne_wildcard;
        }

        off_this.st.type.type = st_attribute;

        if (off_this.test_type == "wildcard_star" || off_this.test_type == "qname")
            off_this.test_type = "attribute";

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
            dbe->name = new char[6];
            strcpy(dbe->name, "dummy");
            off_cont.opin = PPOpIn(new PPAbsPath(dyn_cxt, createOperationInfo(n), NULL, counted_ptr<db_entity>(dbe)), 1);
        }

        // check if we've got processing-instruction test
        ASTPiTest *pit = dynamic_cast<ASTPiTest *>(n.test);

        if (n.axis <= ASTAxisStep::DESCENDANT_ATTRIBUTE && !n.preds && off_cont.opin.op && (!pit || pit->type == ASTPiTest::NONE))
        {
            if (PPAbsPath *apa = dynamic_cast<PPAbsPath *>(off_cont.opin.op))
            {
                std::string lr;

                lr = getlrForAxisStep(n);

                off_this.opin = off_cont.opin;
                off_this.lr_path = (off_cont.lr_path + lr);

                // last step: should finalize abspath
                if (n.isLast)
                {
                    finalizeAbsPath(apa, off_this.lr_path.c_str(), pers_path_mode);
                    off_this.lr_path = "";
                }

                setOffer(off_this);

                return;
            }
        }
        else if (n.cont)
        {
             if (PPAbsPath *apa = dynamic_cast<PPAbsPath *>(off_cont.opin.op)) // need to close PPAbsPath
             {
                 finalizeAbsPath(apa, off_cont.lr_path.c_str(), pers_path_mode);
             }
        }

        // determine if we need sequence checker
        bool need_checker = isStepNeedsChecker(n);

        if (need_checker) // check context
            off_cont.opin.op = new PPSeqChecker(dyn_cxt, oi, off_cont.opin, PPSeqChecker::CHECK_NODE);

        // now we need to construct qep for xpath axis step
        if (n.preds)
        {
            var_id axis_cxt = getVarNum(); // axis context
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

                pred_cxt = getVarNum();
                pos_var = last_var = -1;

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

                if (last_var != -1)
                    bound_vars.pop_back();

                if (pos_var != -1)
                    bound_vars.pop_back();

                bound_vars.pop_back();
            }

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
        st_cxt->boundary_space = (n.mod == ASTBoundSpaceDecl::STRIP) ? xq_boundary_space_strip : xq_boundary_space_preserve;
    }

    void lr2por::visit(ASTCase &n)
    {
        childOffer off_this;

        // first we need to bind our var if we've got it
        if (n.var)
        {
            // first get typeswitch main binding; it should be the last one in bound_vars
            U_ASSERT(bound_vars.back().first == "$%ts");
            var_id var = bound_vars.back().second;

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
        tuple_cell tc;

        tc = string2tuple_cell(*n.cont, xs_string);

        off_this.opin = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), tc), 1);

        setOffer(off_this);
    }

    void lr2por::visit(ASTCommTest &n)
    {
        childOffer off_this;

        off_this.test_data = "()";
        off_this.test_type = "comment";
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
        st_cxt->preserve_type = (n.mod == ASTConstDecl::PRESERVE) ? true : false;
    }

    void lr2por::visit(ASTCreateColl &n)
    {
        childOffer off_coll;

        var_num = 0;
        dyn_cxt = new dynamic_context(st_cxt, 0);
        n.coll->accept(*this);

        off_coll = getOffer();
        dyn_cxt->set_producers((var_num) ? (var_num + 1) : 0);

        qep = new PPCreateCollection(off_coll.opin, dyn_cxt);
    }

    void lr2por::visit(ASTCreateDoc &n)
    {
        childOffer off_doc, off_coll;
        dynamic_context *cxt_doc, *cxt_coll;

        var_num = 0;
        dyn_cxt = cxt_doc = new dynamic_context(st_cxt, 0);
        n.doc->accept(*this);

        off_doc = getOffer();
        cxt_doc->set_producers((var_num) ? (var_num + 1) : 0);

        if (n.coll)
        {
            var_num = 0;
            dyn_cxt = cxt_coll = new dynamic_context(st_cxt, (var_num) ? (var_num + 1) : 0);
            n.coll->accept(*this);

            off_coll = getOffer();
            cxt_coll->set_producers((var_num) ? (var_num + 1) : 0);
        }

        if (n.coll)
            qep = new PPCreateDocumentInCollection(off_doc.opin, cxt_doc, off_coll.opin, cxt_coll);
        else
            qep = new PPCreateDocument(off_doc.opin, cxt_doc);
    }

    void lr2por::visit(ASTCreateFtIndex &n)
    {
#ifdef SE_ENABLE_FTSEARCH

        if (tr_globals::is_ft_disabled)
            throw USER_EXCEPTION2(SE1002, "full-text search support is disabled in RO-mode");

        childOffer off_name, off_path;
        PPAbsPath *pa;
        PathExpr *onp;
        counted_ptr<db_entity> dbe;

        var_num = 0;
        dyn_cxt = new dynamic_context(st_cxt, 0);
        n.name->accept(*this);
        off_name = getOffer();

        pers_path_mode = true;
        n.path->accept(*this);
        off_path = getOffer();
        pers_path_mode = false;

        // path will definitely be PPAbsPath
        pa = dynamic_cast<PPAbsPath *>(off_path.opin.op);
        U_ASSERT(pa);

        onp = pa->getPathExpr();
        dbe = pa->getDocColl();
        delete pa; // we don't need it anymore (note that this won't destroy onp)

        if (!onp || onp->s == 0) // should make it persistent (not-null path will be made persistent by ast-ops)
            onp = lr2PathExpr(dyn_cxt, "()", pe_catalog_aspace);

        // set context
        dyn_cxt->set_producers((var_num) ? (var_num + 1) : 0);

        // make qe
        if (*n.type == "customized-value" || *n.type == "!customized-value")
        {
            childOffer off_cust;

            n.cust_expr->accept(*this);
            off_cust = getOffer();

            qep = new PPCreateFtIndex(onp, n.type->c_str(), dbe, off_name.opin, off_cust.opin, dyn_cxt);
        }
        else
        {
            qep = new PPCreateFtIndex(onp, n.type->c_str(), dbe, off_name.opin, dyn_cxt);
        }
#else
        throw USER_EXCEPTION2(SE1002, "full-text search support is disabled");
#endif
    }

    void lr2por::visit(ASTCreateIndex &n)
    {
        childOffer off_name, off_path, off_type;
        PPAbsPath *pa;
        PathExpr *onp, *byp;
        xmlscm_type xtype;
        counted_ptr<db_entity> dbe;

        var_num = 0;
        dyn_cxt = new dynamic_context(st_cxt, 0);
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
        delete pa; // we don't need it anymore (note that this won't destroy onp)

        if (!onp || onp->s == 0) // should make it persistent (not-null path will be made persistent by ast-ops)
            onp = lr2PathExpr(dyn_cxt, "()", pe_catalog_aspace);

        n.by_path->accept(*this);
        off_path = getOffer();
        pers_path_mode = false;

        // now by-path will definitely be PPAbsPath
        pa = dynamic_cast<PPAbsPath *>(off_path.opin.op);
        U_ASSERT(pa);

        byp = pa->getPathExpr();
        delete pa; // we don't need it anymore (note that this won't destroy on_path)

        if (!byp || byp->s == 0) // should make it persistent (not-null path will be made persistent by ast-ops)
            byp = lr2PathExpr(dyn_cxt, "()", pe_catalog_aspace);

        n.type->accept(*this);
        off_type = getOffer();
        xtype = off_type.st.type.info.single_type;

        // set context
        dyn_cxt->set_producers((var_num) ? (var_num + 1) : 0);

        qep = new PPCreateIndex(onp, byp, xtype, dbe, off_name.opin, dyn_cxt);
    }

    void lr2por::visit(ASTCreateRole &n)
    {
        dyn_cxt = new dynamic_context(st_cxt, 0);
        dyn_cxt->set_producers(1);

        qep = new PPCreateRole(PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.role, xs_string)), 1), dyn_cxt);
    }

    void lr2por::visit(ASTCreateTrg &n)
    {
#ifdef SE_ENABLE_TRIGGERS
        static const char *trg2str[] =
        {
            "BEFORE",
            "AFTER",

            "INSERT",
            "DELETE",
            "REPLACE",

            "NODE",
            "STATEMENT"
        };

        PPAbsPath *pa;
        PathExpr *onp;
        counted_ptr<db_entity> dbe;
        childOffer off_path;
        PPOpIn name;
        tuple_cell tc;

        var_num = 0;
        dyn_cxt = new dynamic_context(st_cxt, 0);

        // create trigger name
        tc = string2tuple_cell(*n.name, xs_string);
        name = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), tc), 1);

        // on-path will definitely be PPAbsPath
        pers_path_mode = true;
        n.path->accept(*this);
        off_path = getOffer();
        pers_path_mode = false;
        pa = dynamic_cast<PPAbsPath *>(off_path.opin.op);
        U_ASSERT(pa);

        onp = pa->getPathExpr();
        dbe = pa->getDocColl();
        delete pa; // we don't need it anymore (note that this won't destroy onp)

        if (!onp || onp->s == 0) // should make it persistent (not-null path will be made persistent by ast-ops)
            onp = lr2PathExpr(dyn_cxt, "()", pe_catalog_aspace);


        scheme_list *action = new scheme_list(n.do_exprs->size() * 2);

        for (unsigned int i = 0; i < n.do_exprs->size(); i++)
        {
            ASTQuery *st_query = dynamic_cast<ASTQuery *>(n.do_exprs->at(i));
            bool is_query = (st_query->type == ASTQuery::QUERY);

            st_query->is_trigger = true;

            std::string ir = mod->getIR(st_query);

            action->at(2*i).type = SCM_STRING;
            action->at(2*i).internal.str = new char[ir.size() + 1];
            action->at(2*i+1).type = SCM_BOOL;
            action->at(2*i+1).internal.b = is_query;

            strcpy(action->at(2*i).internal.str, ir.c_str());
        }

        if (n.t_mod == ASTCreateTrg::BEFORE && n.a_mod == ASTCreateTrg::INSERT && n.g_mod == ASTCreateTrg::NODE)
        {
            childOffer off_ipath;
            PathExpr *ip;

            pers_path_mode = true;
            n.trimmed_path->accept(*this);
            off_ipath = getOffer();
            pers_path_mode = false;

            pa = dynamic_cast<PPAbsPath *>(off_ipath.opin.op);
            U_ASSERT(pa);

            ip = pa->getPathExpr();
            delete pa; // we don't need it anymore (note that this won't destroy onp)

            if (!ip || ip->s == 0) // should make it persistent (not-null path will be made persistent by ast-ops)
                ip = lr2PathExpr(dyn_cxt, "()", pe_catalog_aspace);

            qep = new PPCreateTrigger(trg2str[n.t_mod], trg2str[n.a_mod], dbe, onp, trg2str[n.g_mod], action,
                    n.leaf_name->c_str(), n.leaf_type, ip, name, dyn_cxt);
        }
        else
        {
            qep = new PPCreateTrigger(trg2str[n.t_mod], trg2str[n.a_mod], dbe, onp, trg2str[n.g_mod], action, name, dyn_cxt);
        }

        dyn_cxt->set_producers((var_num) ? (var_num + 1) : 0);
#else
        throw USER_EXCEPTION2(SE1002, "Triggers support is disabled. Compile Sedna with ENABLE_TRIGGERS=1 if you want to turn this feature on.");
#endif
    }

    void lr2por::visit(ASTCreateUser &n)
    {
        dyn_cxt = new dynamic_context(st_cxt, 0);
        dyn_cxt->set_producers(1);

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
        // we ignore type and nillability from element here
        childOffer off_this;

        if (n.elem_test)
        {
            n.elem_test->accept(*this);
            off_this = getOffer();

            off_this.st.type.type = st_document_element;
        }
        else
        {
            off_this.test_data = "()";
            off_this.st.type.type = st_document;
        }

        off_this.test_type = "document";

        setOffer(off_this);
    }

    void lr2por::visit(ASTDropColl &n)
    {
        childOffer off_coll;

        var_num = 0;
        dyn_cxt = new dynamic_context(st_cxt, 0);
        n.coll->accept(*this);

        off_coll = getOffer();
        dyn_cxt->set_producers((var_num) ? (var_num + 1) : 0);

        qep = new PPDropCollection(off_coll.opin, dyn_cxt);
    }

    void lr2por::visit(ASTDropDoc &n)
    {
        childOffer off_doc, off_coll;
        dynamic_context *cxt_doc, *cxt_coll;

        var_num = 0;
        dyn_cxt = cxt_doc = new dynamic_context(st_cxt, 0);
        n.doc->accept(*this);

        off_doc = getOffer();
        cxt_doc->set_producers((var_num) ? (var_num + 1) : 0);

        if (n.coll)
        {
            var_num = 0;
            dyn_cxt = cxt_coll = new dynamic_context(st_cxt, (var_num) ? (var_num + 1) : 0);
            n.coll->accept(*this);

            off_coll = getOffer();
            cxt_coll->set_producers((var_num) ? (var_num + 1) : 0);
        }

        if (n.coll)
            qep = new PPDropDocumentInCollection(off_doc.opin, cxt_doc, off_coll.opin, cxt_coll);
        else
            qep = new PPDropDocument(off_doc.opin, cxt_doc);
    }

    void lr2por::visit(ASTDropFtIndex &n)
    {
#ifdef SE_ENABLE_FTSEARCH
        if (tr_globals::is_ft_disabled)
            throw USER_EXCEPTION2(SE1002, "full-text search support is disabled in RO-mode");

        childOffer off_ind;

        var_num = 0;
        dyn_cxt = new dynamic_context(st_cxt, 0);
        n.index->accept(*this);

        off_ind = getOffer();
        dyn_cxt->set_producers((var_num) ? (var_num + 1) : 0);

        qep = new PPDropFtIndex(off_ind.opin, dyn_cxt);
#else
        throw USER_EXCEPTION2(SE1002, "full-text search support is disabled");
#endif
    }

    void lr2por::visit(ASTDropIndex &n)
    {
        childOffer off_ind;

        var_num = 0;
        dyn_cxt = new dynamic_context(st_cxt, 0);
        n.index->accept(*this);

        off_ind = getOffer();
        dyn_cxt->set_producers((var_num) ? (var_num + 1) : 0);

        qep = new PPDropIndex(off_ind.opin, dyn_cxt);
    }

    void lr2por::visit(ASTDropMod &n)
    {
        dyn_cxt = new dynamic_context(st_cxt, 0);

        PPOpIn mod = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.module, xs_string)), 1);

        qep = new PPDropModule(mod);
    }

    void lr2por::visit(ASTDropRole &n)
    {
        // nothing to do
    }

    void lr2por::visit(ASTDropTrg &n)
    {
#ifdef SE_ENABLE_TRIGGERS
        dyn_cxt = new dynamic_context(st_cxt, 0);

        PPOpIn name = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.trg, xs_string)), 1);

        qep = new PPDropTrigger(name, dyn_cxt);
#else
        throw USER_EXCEPTION2(SE1002, "Triggers support disabled. Compile Sedna with ENABLE_TRIGGERS=1 if you want to turn this feature on.");
#endif
    }

    void lr2por::visit(ASTDropUser &n)
    {
        dyn_cxt = new dynamic_context(st_cxt, 0);
        dyn_cxt->set_producers(1);

        qep = new PPDropUser(PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(*n.user, xs_string)), 1), dyn_cxt);
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
        // we ignore type and nillability here for XPath and set it only for sequence types
        childOffer off_this;

        if (n.name)
        {
            n.name->accept(*this);
            off_this = getOffer();
        }
        else
        {
            off_this.test_data = "()";
            off_this.test_type = "wildcard_star";

            off_this.st.type.info.ea.nne = st_nne_wildcard;
        }

        off_this.st.type.type = st_element;

        if (off_this.test_type == "wildcard_star" || off_this.test_type == "qname")
            off_this.test_type = "element";

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
        }

        if (n.cont && (n.expr || n.preds || n.isLast)) // we need to close abs-path if we've got it as a context (exception, "." - expression)
        {
             if (PPAbsPath *apa = dynamic_cast<PPAbsPath *>(off_cont.opin.op)) // need to close PPAbsPath
                     finalizeAbsPath(apa, off_cont.lr_path.c_str(), pers_path_mode);
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

        var_id cont_cxt; // step context
        var_id var_pos = -1, var_last = -1; // variables for last, positon to use in primary expression

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
                var_id pred_cxt; // predicate context
                var_id pos_var, last_var;
                ASTPred *pred = dynamic_cast<ASTPred *>(n.preds->at(i));
                parentRequest req;

                pred_cxt = getVarNum();
                pos_var = last_var = -1;

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

                if (last_var != -1)
                    bound_vars.pop_back();

                if (pos_var != -1)
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

            if (var_last != -1)
            {
                off_cont.opin.op = new PPLast(dyn_cxt, oi, var_last, off_cont.opin);

                U_ASSERT(bound_vars.back().first == "$%last");
                bound_vars.pop_back();
            }

            off_this.opin.op = new PPReturn(dyn_cxt, oi, vars, off_cont.opin, expr, var_pos);
            off_this.opin.ts = 1;

            if (var_pos != -1)
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
        unsigned int var_count = 0;
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
        size_t let_num = 0; // number of let-vars in un_vars

        for (int i = n.fls->size() - 1; i >= 0; i--)
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

                    if (dynamic_cast<ASTLet *>((*n.fls)[i])) // for let-clause remember the position
                        let_num++;
                }
            }
            bound_vars.pop_back();

            if (off.st.type.type == st_atomic_type && off.st.type.info.single_type == xs_anyType)
            {
                if (dynamic_cast<const ASTFor *>((*n.fls)[i]))
                    flop.op = new PPReturn(dyn_cxt, createOperationInfo(*(*n.fls)[i]), vars, off.opin, flop, pos_var);
                else
                    flop.op = new PPLet(dyn_cxt, createOperationInfo(*(*n.fls)[i]), vars, off.opin, flop);
            }
            else
            {
                if (dynamic_cast<const ASTFor *>((*n.fls)[i]))
                    flop.op = new PPReturn(dyn_cxt, createOperationInfo(*(*n.fls)[i]), vars, off.opin, flop, pos_var, off.st);
                else
                    flop.op = new PPLet(dyn_cxt, createOperationInfo(*(*n.fls)[i]), vars, off.opin, flop, off.st);
            }
        }

        if (n.order_by)
        {
            // here un_vars contains general PPReturn bindings (in reverse order)
            // since we introduce PPSLets here, return-statement must work with the same for-bindings and SLET-bindings (not un_vars let ones)
            PPOpIn ob, ret;
            arr_of_var_dsc vars(un_vars.size());
            std::vector<var_id> new_slet_bindings;
            bool isStable = dynamic_cast<const ASTOrderBy *>(n.order_by)->isStable();

            ob.op = new PPOrderBy(dyn_cxt, createOperationInfo(*n.order_by), isStable, flop, off_ob.orbs, un_vars.size());
            ob.ts = un_vars.size();

            // bind for-let variables with new bindings
            for (size_t i = 0; i < un_vars.size(); i++)
            {
                bound_vars.push_back(un_vars[i]);

                // for let introduce new binding
                if (i < let_num)
                {
                    var_id new_slet = getVarNum();
                    new_slet_bindings.push_back(new_slet);
                    bound_vars.back().second = new_slet;
                }
            }

            n.ret->accept(*this);
            off_this = getOffer();

            // unbind variables
            bound_vars.erase(bound_vars.end() - un_vars.size(), bound_vars.end());

            ret = off_this.opin;

            // create slets
            for (size_t i = 0; i < un_vars.size(); i++)
            {
                if (i < let_num)
                {
                    arr_of_var_dsc slet_vars;
                    slet_vars.push_back(new_slet_bindings[i]); // new binding for PPSLet

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

                PathExpr *path_expr = lr2PathExpr(dyn_cxt, "()", pe_local_aspace);

                if (name)
                {
                    dbe->name = new char[name->lit->size() + 1];
                    strcpy(dbe->name, name->lit->c_str());

                    off_this.opin = PPOpIn(new PPAbsPath(dyn_cxt, createOperationInfo(n), path_expr, counted_ptr<db_entity>(dbe)), 1);
                }
                else
                {
                    childOffer off;

                    (*n.params)[0]->accept(*this);
                    off = getOffer();

                    dbe->name = NULL;

                    off_this.opin = PPOpIn(new PPAbsPath(dyn_cxt, createOperationInfo(n), path_expr,
                            counted_ptr<db_entity>(dbe), off.opin), 1);
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

                PathExpr *path_expr = lr2PathExpr(dyn_cxt, "()", pe_local_aspace);

                if (name)
                {
                    dbe->name = new char[name->lit->size() + 1];
                    strcpy(dbe->name, name->lit->c_str());

                    off_this.opin = PPOpIn(new PPAbsPath(dyn_cxt, createOperationInfo(n), path_expr, counted_ptr<db_entity>(dbe)), 1);
                }
                else
                {
                    childOffer off;

                    (*n.params)[0]->accept(*this);
                    off = getOffer();

                    dbe->name = NULL;

                    off_this.opin = PPOpIn(new PPAbsPath(dyn_cxt, createOperationInfo(n), path_expr,
                            counted_ptr<db_entity>(dbe), off.opin), 1);
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
                var_id pv = -1;

                // for position we need to find last bound pos var
                for (int i = bound_vars.size() - 1; i >= 0; i--)
                {
                    if (bound_vars[i].first == "$%pos")
                    {
                        pv = bound_vars[i].second;
                        break;
                    }
                }

                U_ASSERT(pv != -1);

                off_this.opin = PPOpIn(new PPVariable(dyn_cxt, createOperationInfo(n), pv), 1);
            }
            else if (*n.int_name == "!fn!last")
            {
                var_id lv = -1;

                // for position we need to find last bound pos var
                for (int i = bound_vars.size() - 1; i >= 0; i--)
                {
                    if (bound_vars[i].first == "$%last")
                    {
                        lv = bound_vars[i].second;
                        break;
                    }
                }

                U_ASSERT(lv != -1);

                off_this.opin = PPOpIn(new PPVariable(dyn_cxt, createOperationInfo(n), lv), 1);
            }
            else // all other standard functions are qeped in unified way
            {
                std::string name = "{" + *n.uri + "}" + *n.local;
                XQFunction fu = drv->getStdFuncInfo(name);

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

                off_this.opin = fu.l2pGen(dyn_cxt, createOperationInfo(n), para);
            }
        }
        else
        {
            std::string name = CREATE_INTNAME_FUN(*n.uri, *n.local, arity);

            // then find it in global functions
            int fid = getGlobalFunctionId(name);

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

            if (fid != -1)
            {
                off_this.opin = PPOpIn(new PPFunCall(dyn_cxt, createOperationInfo(n), para, fid), 1);
            }
            else // external function
            {
                // NOTE: we ignore prefix-uri part for external functions
                off_this.opin = PPOpIn(ext_function_manager.make_pp_ext_func(n.local->c_str(), dyn_cxt, createOperationInfo(n), para), 1);
            }
        }

        setOffer(off_this);
    }

    void lr2por::visit(ASTFuncDecl &n)
    {
        int id;
        unsigned int arity = (n.params) ? n.params->size() : 0;

        // ignore external functions since they are treated only via fun-calls
        if (!n.body)
            return;

        var_num = 0;
        id = n.getId();
        function_declaration &fd = dynamic_context::funct_cxt.fun_decls[id];

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
                                       // during evaluation it will not be a problem (yuck!!!, but moved from por2qep "as-is")
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
        }

        tc = string2tuple_cell(*n.lit, type);

        off_this.opin = PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), tc), 1);

        setOffer(off_this);
    }

    void lr2por::visit(ASTLoadFile &n)
    {
        PPOpIn file, doc, coll;
        dynamic_context *dc1, *dc2, *dc3 = NULL;

        dc1 = new dynamic_context(st_cxt, 0);
        dc2 = new dynamic_context(st_cxt, 0);

        if (n.coll)
            dc3 = new dynamic_context(st_cxt, 0);

        std::string *file_name = n.getFileName();

        file = PPOpIn(new PPConst(dc1, createOperationInfo(n), string2tuple_cell(*file_name, xs_string)), 1);
        doc  = PPOpIn(new PPConst(dc2, createOperationInfo(n), string2tuple_cell(*n.doc, xs_string)), 1);

        delete file_name;

        if (n.coll)
            coll = PPOpIn(new PPConst(dc3, createOperationInfo(n), string2tuple_cell(*n.coll, xs_string)), 1);

        qep = new PPBulkLoad(file, dc1, doc, dc2, coll, dc3);
    }

    void lr2por::visit(ASTLoadModule &n)
    {
        arr_of_PPOpIn mods;
        ASTStringVector::iterator it;

        for (it = n.modules->begin(); it != n.modules->end(); it++)
        {
            dyn_cxt = new dynamic_context(st_cxt, 0);

            mods.push_back(PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell(**it, xs_string)), 1));
        }

        qep = new PPLoadModule(mods, n.mod == ASTLoadModule::REPLACE);
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

        qep = new PPRetrieveMetadata(dbe_collection, coll, NULL, n.need_stats);
    }

    void lr2por::visit(ASTMetaDocs &n)
    {
        if (n.coll)
        {
            childOffer off_coll;

            dyn_cxt = new dynamic_context(st_cxt, 0);

            n.coll->accept(*this);
            off_coll = getOffer();

            dyn_cxt->set_producers((var_num) ? (var_num + 1) : 0);

            qep = new PPRetrieveMetadata(dbe_document, off_coll.opin, dyn_cxt, n.need_stats);
        }
        else
        {
            PPOpIn coll; // dummy

            qep = new PPRetrieveMetadata(dbe_document, coll, NULL, n.need_stats);
        }
    }

    void lr2por::visit(ASTMetaSchemaCol &n)
    {
        childOffer off_coll;

        dyn_cxt = new dynamic_context(st_cxt, 0);

        n.coll->accept(*this);
        off_coll = getOffer();

        dyn_cxt->set_producers((var_num) ? (var_num + 1) : 0);

        qep = new PPRetrieveDS(off_coll.opin, dyn_cxt, dbe_collection);
    }

    void lr2por::visit(ASTMetaSchemaDoc &n)
    {
        childOffer off_doc;

        dyn_cxt = new dynamic_context(st_cxt, 0);

        n.doc->accept(*this);
        off_doc = getOffer();

        dyn_cxt->set_producers((var_num) ? (var_num + 1) : 0);

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
            off_this.test_data = "()";
            off_this.test_type = "wildcard_star";
            off_this.st.type.info.ea.nne = st_nne_wildcard;
        }
        else if (*n.pref == "*")
        {
            off_this.test_data = "\"" + *n.local + "\"";
            off_this.test_type = "wildcard_star_ncname";
        }
        else if (*n.local == "*")
        {
            off_this.test_data = "\"" + *n.uri + "\"";
            off_this.test_type = "wildcard_ncname_star";
        }
        else
        {
            off_this.test_data = "(\"" + ((n.uri) ? *n.uri : "") + "\"";
            off_this.test_data += " \"" + *n.local + "\" \"" + *n.pref + "\")";
            off_this.test_type = "qname";

            off_this.st.type.info.ea.nne = st_nne_name;
            off_this.st.type.info.ea.node_name_uri = (!n.uri || *n.uri == "") ? NULL : xs_NCName_create(n.uri->c_str(), pe_local_aspace->alloc);
            off_this.st.type.info.ea.node_name_local = xs_NCName_create(n.local->c_str(), pe_local_aspace->alloc);
        }

        setOffer(off_this);
    }

    void lr2por::visit(ASTNamespaceDecl &n)
    {
        st_cxt->add_to_context(n.name->c_str(), n.uri->c_str());
    }

    void lr2por::visit(ASTNodeTest &n)
    {
        childOffer off_this;

        off_this.test_data = "()";
        off_this.test_type = "node";

        off_this.st.type.type = st_node;

        setOffer(off_this);
    }

    void lr2por::visit(ASTNsp &n)
    {
        PPOpIn cont;
        tuple_cell tc;
        childOffer off_this;

        tc = string2tuple_cell(n.cont ? *n.cont : "", xs_string);
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
        childOffer off_this;

        n.expr->accept(*this);
        off_this = getOffer();

        setOffer(off_this);
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

    void lr2por::visit(ASTOrderEmpty &n)
    {
        st_cxt->empty_order = (n.mod == ASTOrderEmpty::EMPTY_GREATEST) ? xq_empty_order_greatest : xq_empty_order_least;
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
            orb.order = ORB_ASCENDING;
        }

        if (n.em_mod)
        {
            n.em_mod->accept(*this);
            off_this = getOffer();

            orb.status = off_this.orbs[0].status;
        }
        else
        {
            orb.status = st_cxt->empty_order == xq_empty_order_least ? ORB_EMPTY_LEAST : ORB_EMPTY_GREATEST;
        }

        if (n.col_mod)
        {
            n.col_mod->accept(*this);
            off_this = getOffer();

            orb.collation = off_this.orbs[0].collation;
        }
        else
        {
            orb.collation = st_cxt->get_default_collation();
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
                off_this.orbs[0].order = ORB_ASCENDING;
                break;

            case ASTOrderModInt::DESCENDING:
                off_this.orbs[0].order = ORB_DESCENDING;
                break;

            case ASTOrderModInt::EMPTY_GREATEST:
                off_this.orbs[0].status = ORB_EMPTY_GREATEST;
                break;

            case ASTOrderModInt::EMPTY_LEAST:
                off_this.orbs[0].status = ORB_EMPTY_LEAST;
                break;

            case ASTOrderModInt::COLLATION:
                int res = st_cxt->get_collation(n.uri->c_str(), &(off_this.orbs[0].collation));

                if(res != 0)
                {
                    // Given URI is invalid
                    if (res == COLLATION_INVALID_URI)
                        throw USER_EXCEPTION2(XQST0046, n.uri->c_str());
                    else // There is no such collation, or it could not be properly resolved
                        throw USER_EXCEPTION2(XQST0076, n.uri->c_str());
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

            orb.order = ORB_ASCENDING;
            orb.status = st_cxt->empty_order == xq_empty_order_least ? ORB_EMPTY_LEAST : ORB_EMPTY_GREATEST;
            orb.collation = st_cxt->get_default_collation();

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
            off_this.test_data = "()";
            off_this.st.type.info.ncname = NULL;
        }
        else
        {
            off_this.test_data = "\"" + *n.test + "\"";
            off_this.st.type.info.ncname = xs_NCName_create(n.test->c_str(), pe_local_aspace->alloc);
        }

        off_this.test_type = "processing-instruction";

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
        if (n.type == ASTQuery::QUERY)
        {
            var_num = 0;
            dyn_cxt = new dynamic_context(st_cxt, 0);
        }

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

            dyn_cxt->set_producers((var_num) ? (var_num + 1) : 0);

            qep = new PPQueryRoot(dyn_cxt, off.opin);
        }
    }

    void lr2por::visit(ASTRenameColl &n)
    {
        childOffer off_old, off_new;

        var_num = 0;
        dyn_cxt = new dynamic_context(st_cxt, 0);
        n.name_old->accept(*this);

        off_old = getOffer();

        n.name_new->accept(*this);

        off_new = getOffer();
        dyn_cxt->set_producers((var_num) ? (var_num + 1) : 0);

        qep = new PPRename(off_old.opin, off_new.opin, dyn_cxt);
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

        off_this.opin.op = new PPTextConstructor(dyn_cxt, createOperationInfo(n), off_cont.opin, n.deep_copy);
        off_this.opin.ts = 1;

        setOffer(off_this);
    }

    void lr2por::visit(ASTTextTest &n)
    {
        childOffer off_this;

        off_this.test_data = "()";
        off_this.test_type = "text";
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

        var_id main_var = getVarNum();

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

        var_num = 0;
        dyn_cxt = new dynamic_context(st_cxt, 0);
        n.what->accept(*this);

        off_what = getOffer();
        dyn_cxt->set_producers((var_num) ? (var_num + 1) : 0);

        if (n.type == ASTUpdDel::DEEP)
            qep = new PPDeleteDeep(off_what.opin, dyn_cxt);
        else
            qep = new PPDeleteUndeep(off_what.opin, dyn_cxt);
    }

    void lr2por::visit(ASTUpdInsert &n)
    {
        childOffer off_where, off_what;
        dynamic_context *cxt_what, *cxt_where;

        var_num = 0;
        dyn_cxt = cxt_what = new dynamic_context(st_cxt, 0);
        n.what->accept(*this);

        off_what = getOffer();
        cxt_what->set_producers((var_num) ? (var_num + 1) : 0);

        var_num = 0;
        dyn_cxt = cxt_where = new dynamic_context(st_cxt, (var_num) ? (var_num + 1) : 0);
        n.where->accept(*this);

        off_where = getOffer();
        cxt_where->set_producers((var_num) ? (var_num + 1) : 0);

        switch (n.type)
        {
            case ASTUpdInsert::INTO:
                qep = new PPInsertTo(off_what.opin, cxt_what, off_where.opin, cxt_where);
                break;
            case ASTUpdInsert::PRECEDING:
                qep = new PPInsertBefore(off_what.opin, cxt_what, off_where.opin, cxt_where);
                break;
            case ASTUpdInsert::FOLLOWING:
                qep = new PPInsertFollowing(off_what.opin, cxt_what, off_where.opin, cxt_where);
                break;
        }
    }

    void lr2por::visit(ASTUpdMove &n)
    {
        throw USER_EXCEPTION2(SE4001, "update move is not supported");
    }

    void lr2por::visit(ASTUpdRename &n)
    {
        childOffer off_what;

        var_num = 0;
        dyn_cxt = new dynamic_context(st_cxt, 0);
        n.what->accept(*this);

        off_what = getOffer();
        dyn_cxt->set_producers((var_num) ? (var_num + 1) : 0);

        char *ncname_prefix = xs_NCName_create(n.pref->c_str(), pe_local_aspace->alloc);
        char *ncname_local  = xs_NCName_create(n.local->c_str(), pe_local_aspace->alloc);

        qep = new PPRename(off_what.opin, dyn_cxt, ncname_prefix, ncname_local);
    }

    void lr2por::visit(ASTUpdReplace &n)
    {
        childOffer off_what, off_new, off_var;
        arr_of_var_dsc retv;
        arr_of_PPOpIn new_seq;
        PPOpIn newop;

        var_num = 0;
        dyn_cxt = new dynamic_context(st_cxt, 0);

        setParamMode();
        n.var->accept(*this);
        off_var = getOffer();
        unsetParamMode();

        bool got_type = !(off_var.st.type.type == st_atomic_type && off_var.st.type.info.single_type == xs_anyType);

        n.what->accept(*this);
        off_what = getOffer();

        n.new_expr->accept(*this);
        off_new = getOffer();

        // main return variable
        retv.push_back(bound_vars.back().second);

        // sequence on each updated node (data_child for return)
        new_seq.push_back(PPOpIn(new PPVariable(dyn_cxt, createOperationInfo(n), retv.back()), 1));
        new_seq.push_back(off_new.opin);
        new_seq.push_back(PPOpIn(new PPConst(dyn_cxt, createOperationInfo(n), string2tuple_cell("1", se_separator)), 1));

        newop = PPOpIn(new PPSequence(dyn_cxt, createOperationInfo(n), new_seq), 1);

        dyn_cxt->set_producers((var_num) ? (var_num + 1) : 0);

        if (got_type)
            qep = new PPReplace(PPOpIn(new PPReturn(dyn_cxt, createOperationInfo(n), retv, off_what.opin, newop, -1, off_var.st), 1), dyn_cxt);
        else
            qep = new PPReplace(PPOpIn(new PPReturn(dyn_cxt, createOperationInfo(n), retv, off_what.opin, newop, -1), 1), dyn_cxt);

        bound_vars.pop_back();
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

        setOffer(off_this);
    }

    void lr2por::visit(ASTVarDecl &n)
    {
        childOffer off, off_this, off_type;
        int id;
        PPVarIterator *var;
        operation_info oi;

        // analyze the type
        if (n.type)
        {
            n.type->accept(*this);
            off_type = getOffer();
        }

        dyn_cxt = new dynamic_context(st_cxt, 0);

        // analyze the body
        var_num = 0;
        n.expr->accept(*this);
        off = getOffer();

        dyn_cxt->set_producers((var_num) ? (var_num + 1) : 0);

        id = n.getId();

        oi = createOperationInfo(n);

        if (n.type)
            var = new PPVarDecl(dyn_cxt, oi, id, off.opin, off_type.st);
        else
            var = new PPVarDecl(dyn_cxt, oi, id, off.opin);

        dynamic_context::glb_var_cxt.producers[id].op = var;
        dynamic_context::glb_var_cxt.producers[id].cxt = dyn_cxt;

        dyn_cxt = NULL;
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

        // first,look in the cache
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
        std::string res = "((";
        childOffer off;

        res += axis_str[s.axis];

        s.test->accept(*this);
        off = getOffer();

        res += " ";
        res += off.test_type;
        res += " ";
        res += off.test_data;
        res += "))";

        return res;
    }

    void lr2por::finalizeAbsPath(PPAbsPath *pap, const char *lr, bool pers)
    {
        // null-abspath -- ignore
        if (!strlen(lr))
            return;

        // make it list-like
        std::string lr_list = std::string("(") + lr + ")";

        PathExpr *pe = lr2PathExpr(dyn_cxt, lr_list.c_str(), pers ? pe_catalog_aspace : pe_local_aspace);
        pap->setPathExpr(pe);
    }

    PPOpIn lr2por::getPPForAxis(const ASTAxisStep &s, PPOpIn cont, operation_info oi)
    {
        NodeTestType ntype;
        NodeTestData ndata;
        childOffer off;
        PPOpIn op;

        scheme_list *sl = make_tree_from_scheme_list(getlrForAxisStep(s).c_str());
        set_node_test_type_and_data(sl->at(0).internal.list, ntype, ndata, pe_local_aspace);
        delete_scheme_list(sl);

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
            case ASTBop::MULT:
                r = get_binary_op(xqbop_mul, lt, rt);
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
