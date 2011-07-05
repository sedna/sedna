/*
 * File:  lreturn.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _LR2POR_VISITOR_H_
#define _LR2POR_VISITOR_H_

#include <string>
#include <map>
#include <set>

#include "tr/xqp/visitor/ASTVisitor.h"
#include "tr/xqp/XQueryDriver.h"
#include "tr/xqp/XQueryModule.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/xqops/PPOrderBy.h"
#include "tr/executor/xqops/PPCalculate.h"
#include "tr/executor/xqops/PPAbsPath.h"

namespace sedna
{
    class lr2por : public ASTVisitor
    {
    private:

        struct parentRequest
        {
            PPOpIn pred_cxt; // context for predicate evaluation (propagated from step)
            size_t var_count; // number of vars bound by for-let (for order-by PPSTuple)
            bool copy_constructor; // true, if constructed node will be deep-copied in place (see PPElementConstructor for example)
            bool pers_abspath; // true, if we need to start abs-path from relatieve path and we need it persistent(take create-index as an example)
            // NOTE: true is always safe, false allows small optimization when node is inserted in its intended place

            parentRequest()
            {
                pred_cxt.op = NULL;
                var_count = 0;
                copy_constructor = true;
            }
        };

        struct childOffer
        {
            PPOpIn opin;      // subtree for the expression
            arr_of_orb_modifier orbs; // order-by modifiers

            sequence_type st;  // type for typed vars
            bool open_abs_path; // Whether or not can we continue abs path
            std::string lr_path; // for indexes, triggers, abs_path expressions
            std::string serialized_form; // for node-test in axis steps (look XPath.cpp for serialization rules)

            bool special_node; // Currently used to indicate that the constructed branch is NSP node with element's prefix

            childOffer() : open_abs_path(false), special_node(false)
            {
                opin.op = NULL;
            }
        };

        bool virtualizableConstructors;

        bool param_mode; // true, if we are checking function params now (ASTVar sema analysis)
        unsigned int param_count; // number of parameters found in param_mode
        bool pers_path_mode; // if true, then we the next xpath will be persistent (use only for craete indexe/ft-index/trigger, where path is already checked)

        typedef std::pair<std::string, var_dsc> l2pVarInfo; // var info int_name+id

        std::vector<l2pVarInfo> bound_vars; // vector of variables bound in the current expression (we need only names there)
        std::vector<childOffer> offers; // offers from children go in this sequence

        typedef std::map<std::string, function_id> funcInfo;
        typedef std::map<std::string, global_var_dsc> varInfo;

        funcInfo funcCache; // cache containing info about processed functions
        varInfo varCache; // cache containg info about processed global and lib variables

        dynamic_context *dyn_cxt; // current context for ops (different for every function-variable)

        PPQueryEssence *qep; // result tree
        bool is_subquery; // true if we're building a subquery

        // some special stuff for PPCalculate
        int var_op_num; // leaf nums for ppcalculate operations
        CalcOp *op_tree; // tree of CalcOps for ppcalculate
        arr_of_PPOpIn *calc_ops; // operations for ppcalculate

        void setParamMode();
        void unsetParamMode();

        childOffer getOffer();
        void setOffer(const childOffer &off);

        void VisitNodesVector(ASTNodesVector *nodes, ASTVisitor &v, parentRequest req);

        parentRequest parentReq; // request from parent to child
        std::vector<parentRequest> pareqs; // parent requests

        const parentRequest &getParentRequest() const;
        void setParentRequest(const parentRequest &preq);

        var_dsc getVarNum();
        CalcOp *make_CalcOp(ASTNode *n, bool logical);
        void make_binary_op(ASTBop &n);
        void make_unary_op(ASTUop &n);

        static operation_info createOperationInfo(const ASTNode &n);
        static void alterOffer(childOffer &off_this, const childOffer &off);
        static bool checkAndAddIfUnique(std::vector<l2pVarInfo> &un_vars, const l2pVarInfo &var);

    public:
        lr2por(sedna::XQueryDriver *drv_, sedna::XQueryModule *mod_, dynamic_context *dyn_cxt_, bool is_subquery_) : ASTVisitor(drv_, mod_)
        {
            param_mode = false;
            param_count = 0;
            pers_path_mode = false;
            pareqs.push_back(parentRequest());

            dyn_cxt = dyn_cxt_;
            qep = NULL;
            var_op_num = -1;
            virtualizableConstructors = false;

            is_subquery = is_subquery_;
        }

        ~lr2por()
        {
        }

        PPQueryEssence *getResult()
        {
            return qep;
        }

        virtual void addToPath(ASTNode *nod);
        virtual void removeFromPath(ASTNode *nod);

        function_id getGlobalFunctionId(const std::string &name);
        global_var_dsc getGlobalVariableId(const std::string &name);
        childOffer getContextOffer(operation_info oi) const;
        bool isStepNeedsChecker(const ASTStep &st) const;
        std::string getlrForAxisStep(const ASTAxisStep &s);
        void finalizeAbsPath(PPAbsPath *pap, const char *lr, bool pers);
        PPOpIn getPPForAxis(const ASTAxisStep &s, PPOpIn cont, operation_info oi);

        // visiting functions
        void visit(ASTAlterUser &n);
        void visit(ASTAttr &n);
        void visit(ASTAttrConst &n);
        void visit(ASTAttribTest &n);
        void visit(ASTAxisStep &n);
        void visit(ASTBaseURI &n);
        void visit(ASTBop &n);
        void visit(ASTBoundSpaceDecl &n);
        void visit(ASTCase &n);
        void visit(ASTCast &n);
        void visit(ASTCastable &n);
        void visit(ASTCharCont &n);
        void visit(ASTCommTest &n);
        void visit(ASTCommentConst &n);
        void visit(ASTConstDecl &n);
        void visit(ASTCreateColl &n);
        void visit(ASTCreateDoc &n);
        void visit(ASTCreateFtIndex &n);
        void visit(ASTCreateIndex &n);
        void visit(ASTCreateRole &n);
        void visit(ASTCreateTrg &n);
        void visit(ASTCreateUser &n);
        void visit(ASTDDO &n);
        void visit(ASTDeclareCopyNsp &n);
        void visit(ASTDefCollation &n);
        void visit(ASTDefNamespaceDecl &n);
        void visit(ASTDocConst &n);
        void visit(ASTDocTest &n);
        void visit(ASTDropColl &n);
        void visit(ASTDropDoc &n);
        void visit(ASTDropFtIndex &n);
        void visit(ASTDropIndex &n);
        void visit(ASTDropMod &n);
        void visit(ASTDropRole &n);
        void visit(ASTDropTrg &n);
        void visit(ASTDropUser &n);
        void visit(ASTElem &n);
        void visit(ASTElemConst &n);
        void visit(ASTElementTest &n);
        void visit(ASTEmptyTest &n);
        void visit(ASTError &n);
        void visit(ASTExtExpr &n);
        void visit(ASTFilterStep &n);
        void visit(ASTFLWOR &n);
        void visit(ASTFor &n);
        void visit(ASTFunCall &n);
        void visit(ASTFuncDecl &n);
        void visit(ASTGrantPriv &n);
        void visit(ASTGrantRole &n);
        void visit(ASTIf &n);
        void visit(ASTInstOf &n);
        void visit(ASTItemTest &n);
        void visit(ASTLet &n);
        void visit(ASTLibModule &n);
        void visit(ASTLit &n);
        void visit(ASTLoadFile &n);
        void visit(ASTLoadModule &n);
        void visit(ASTMainModule &n);
        void visit(ASTMetaCols &n);
        void visit(ASTMetaDocs &n);
        void visit(ASTMetaSchemaCol &n);
        void visit(ASTMetaSchemaDoc &n);
        void visit(ASTModImport &n);
        void visit(ASTModuleDecl &n);
        void visit(ASTNameTest &n);
        void visit(ASTNamespaceDecl &n);
        void visit(ASTNodeTest &n);
        void visit(ASTNsp &n);
        void visit(ASTOption &n);
        void visit(ASTOrdExpr &n);
        void visit(ASTOrder &n);
        void visit(ASTOrderBy &n);
        void visit(ASTOrderEmpty &n);
        void visit(ASTOrderMod &n);
        void visit(ASTOrderModInt &n);
        void visit(ASTOrderSpec &n);
        void visit(ASTPIConst &n);
        void visit(ASTPi &n);
        void visit(ASTPiTest &n);
        void visit(ASTPosVar &n);
        void visit(ASTPragma &n);
        void visit(ASTPred &n);
        void visit(ASTProlog &n);
        void visit(ASTQName &n);
        void visit(ASTQuantExpr &n);
        void visit(ASTQuery &n);
        void visit(ASTRenameColl &n);
        void visit(ASTRevokePriv &n);
        void visit(ASTRevokeRole &n);
        void visit(ASTSchemaAttrTest &n);
        void visit(ASTSchemaElemTest &n);
        void visit(ASTSeq &n);
        void visit(ASTSpaceSeq &n);
        void visit(ASTTextConst &n);
        void visit(ASTTextTest &n);
        void visit(ASTTreat &n);
        void visit(ASTType &n);
        void visit(ASTTypeSeq &n);
        void visit(ASTTypeSingle &n);
        void visit(ASTTypeSwitch &n);
        void visit(ASTTypeVar &n);
        void visit(ASTUop &n);
        void visit(ASTUpdDel &n);
        void visit(ASTUpdInsert &n);
        void visit(ASTUpdMove &n);
        void visit(ASTUpdRename &n);
        void visit(ASTUpdReplace &n);
        void visit(ASTVar &n);
        void visit(ASTVarDecl &n);
        void visit(ASTVersionDecl &n);
        void visit(ASTXMLComm &n);
    };
}

#endif
