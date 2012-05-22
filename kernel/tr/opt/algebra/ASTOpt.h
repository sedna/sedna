/*
 * File:  lr2opt.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _LR2RQP_VISITOR_H_
#define _LR2RQP_VISITOR_H_

#include "tr/xqp/lr2por/lr2por.h"
#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/opt/path/XPathTypes.h"

#include <stack>

/* Reduced query plan generation */

namespace sedna
{

class lr2opt : public lr2por
{
  private:

    enum var_context_t {
        vvc_none,
        vvc_get_value,
        vvc_declare,
    } varVisitContext;
    
    struct ResultInfo {
        rqp::RPBase * op;
        opt::TupleId contextItem;
        int opid;
        rqp::TupleVarDescriptor * varDesc;

        explicit ResultInfo(rqp::RPBase * _op)
          : op(_op), contextItem(0), opid(0), varDesc(NULL) { };
    };

    struct StepInfo {
        pe::axis_t axis;
        pe::node_test_t nodeTest;
        xsd::TemplateQName tqname;
    };

    std::stack<ResultInfo> resultStack;
    std::stack<StepInfo> stepStack;

    StaticallyKnownNamespaces * skn;
  public:
    lr2opt(sedna::XQueryDriver *drv_, sedna::XQueryModule *mod_, dynamic_context *dyn_cxt_, bool is_subquery_)
      : lr2por(drv_, mod_, dyn_cxt_, is_subquery_), varVisitContext(vvc_none)
    {
        skn = dyn_cxt_->get_static_context()->getStaticallyKnownNamespaces();
    }

    ~lr2opt() {
    }

    rqp::RPBase * getReducedPlan() const { return resultStack.top().op; };

    /* a11n and a12n */

    void visit(ASTAlterUser &n) {};
    void visit(ASTGrantPriv &n) {};
    void visit(ASTGrantRole &n) {};
    void visit(ASTRevokePriv &n) {};
    void visit(ASTRevokeRole &n) {};

    /* Data definition language */

    void visit(ASTRenameColl &n) {};

    void visit(ASTCreateColl &n) {};
    void visit(ASTCreateDoc &n) {};
    void visit(ASTCreateFtIndex &n) {};
    void visit(ASTCreateIndex &n) {};
    void visit(ASTCreateRole &n) {};
    void visit(ASTCreateTrg &n) {};
    void visit(ASTCreateUser &n) {};

    void visit(ASTDropColl &n) {};
    void visit(ASTDropDoc &n) {};
    void visit(ASTDropFtIndex &n) {};
    void visit(ASTDropIndex &n) {};
    void visit(ASTDropMod &n) {};
    void visit(ASTDropRole &n) {};
    void visit(ASTDropTrg &n) {};
    void visit(ASTDropUser &n) {};

    void visit(ASTMetaCols &n) {};
    void visit(ASTMetaDocs &n) {};
    void visit(ASTMetaSchemaCol &n) {};
    void visit(ASTMetaSchemaDoc &n) {};

    void visit(ASTLoadFile &n) {};

    /* Sedna update language updates */

    void visit(ASTUpdDel &n) {};
    void visit(ASTUpdInsert &n) {};
    void visit(ASTUpdMove &n) {};
    void visit(ASTUpdRename &n) {};
    void visit(ASTUpdReplace &n) {};

    /* Modules */

    void visit(ASTLoadModule &n) {};
    void visit(ASTMainModule &n);
    void visit(ASTModImport &n) {};
    void visit(ASTModuleDecl &n) {};
    void visit(ASTLibModule &n) {};

    /* XQuery language */

    void visit(ASTQuery &n);

    /* XQuery language - Prolog and other declarations */

    void visit(ASTProlog &n) {};
    void visit(ASTBaseURI &n) {};
    void visit(ASTBoundSpaceDecl &n) {};
    void visit(ASTConstDecl &n) {}; // declare construction strip/preserve
    void visit(ASTDeclareCopyNsp &n) {};
    void visit(ASTDefCollation &n) {};
    void visit(ASTDefNamespaceDecl &n) {};
    void visit(ASTFuncDecl &n) {};
    void visit(ASTVarDecl &n) {};
    void visit(ASTVersionDecl &n) {};
    void visit(ASTOption &n) {};
    void visit(ASTNamespaceDecl &n) {};
    void visit(ASTOrderEmpty &n) {};
    void visit(ASTItemTest &n) {};
    void visit(ASTTypeSingle &n) {};
    void visit(ASTPragma &n) {};
    void visit(ASTExtExpr &n) {};

    /* XQuery language - XPath */

    void visit(ASTAxisStep &n);
    void visit(ASTFilterStep &n);
    void visit(ASTPred &n);

    void visit(ASTDocTest &n);
    void visit(ASTAttribTest &n);
    void visit(ASTElementTest &n);
    void visit(ASTEmptyTest &n);
    void visit(ASTTextTest &n);
    void visit(ASTCommTest &n);
    void visit(ASTPiTest &n);
    void visit(ASTSchemaAttrTest &n);
    void visit(ASTSchemaElemTest &n);
    void visit(ASTNameTest &n);
    void visit(ASTNodeTest &n);

    /* XQuery language - Constructors */

    void visit(ASTDocConst &n);
    void visit(ASTAttr &n); // Direct attribute constructor
    void visit(ASTAttrConst &n); // Computed attribute constructor
    void visit(ASTElem &n); // Direct element constructor
    void visit(ASTElemConst &n); // Computed element constructor
    void visit(ASTXMLComm &n); // Direct comment constructor
    void visit(ASTCommentConst &n); // Computed comment constructor
    void visit(ASTPi &n); // Direct PI constructor
    void visit(ASTPIConst &n); // Computed PI constructor

    void visit(ASTTextConst &n);
    void visit(ASTCharCont &n); // CDATA section
    void visit(ASTSpaceSeq &n);
    void visit(ASTNsp &n);

    /* XQuery language - Functions and operators */

    void visit(ASTFunCall &n);
    void visit(ASTUop &n);
    void visit(ASTBop &n);
    void visit(ASTCast &n);
    void visit(ASTCastable &n);
    void visit(ASTInstOf &n);
    void visit(ASTTreat &n);

    /* XQuery language - Statements */

    void visit(ASTCase &n);
    void visit(ASTIf &n);
    void visit(ASTQuantExpr &n);
    void visit(ASTTypeSwitch &n);
    void visit(ASTSeq &n);

    /* XQuery language - FLWOR and its parts */

    void visit(ASTFLWOR &n);
    void visit(ASTFor &n);
    void visit(ASTLet &n);
    void visit(ASTOrder &n);
    void visit(ASTOrderBy &n);
    void visit(ASTOrderMod &n);
    void visit(ASTOrderModInt &n);
    void visit(ASTOrderSpec &n);
    void visit(ASTPosVar &n);

    /* XQuery language - Atoms */

    void visit(ASTTypeSeq &n);
    void visit(ASTTypeVar &n);
    void visit(ASTType &n);
    void visit(ASTVar &n); // ANY var reference
    void visit(ASTLit &n);
    void visit(ASTQName &n);
    void visit(ASTOrdExpr &n);

    /* Xquery PP helpers */

    void visit(ASTDDO &n);

    /* Errornous expressions */
    void visit(ASTError &n) {};
};

};

#endif /* _LR2RQP_VISITOR_H_ */
