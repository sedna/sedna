/*
 * File:  lreturn.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _LRETURN_VISITOR_H_
#define _LRETURN_VISITOR_H_

#include "tr/xqp/visitor/ASTVisitor.h"
#include "tr/xqp/XQueryDriver.h"
#include "tr/xqp/XQueryModule.h"
#include <string>
#include <map>
#include <set>

namespace sedna
{
    class LReturn : public ASTVisitor
    {
    private:

        struct parentRequest
        {
            bool distinctOnly; // require only distinct from child
            bool calledOnce;   // child will be called only once
            bool deep_copy; // true, if constructed node will be deep-copied in place (see PPElementConstructor for example)
            // NOTE: true is always safe, false allows small optimization when node is inserted in its intended place

            bool atomize; // true, if nodes in fact atomize on their insertion (think of an attribute for example); needed by space-seq
            // NOTE: don't need to propagate it deep, only through ASTSeq, since ASTSpaceSeq is always direct
            //       child of comp. constructor or of sequence in usual one
            
            bool start_abspath; // if true -- try to start AbsPath optimization (valid for AxisTest mostly)
            // NOTE: false doesn't necessarily mean that we won't use abs-path; true forces even relative XPath to consider the
            //       opportunity; for indexes and triggers, relative abs-path is crucial, so "true" here means "force"

            std::string * parentPrefix; /* Namespace prefix of parent element. Namespace is not added to child sequence if it
                      * is needed to resolve element's prefix, but returned in child offer */

            parentRequest()
            {
                calledOnce = true;
                distinctOnly = false;
                deep_copy = true;
                atomize = false;
                start_abspath = false;
                parentPrefix = NULL;
            }
        };

        struct childOffer
        {
            xqExprInfo exi;
            bool isCached;                  // true, if child has been cached
            std::set<std::string> usedVars; // contains bound variables used in subexpression
            bool use_last;                  // true, if child uses fn:last()
            bool use_position;              // true, if child uses fn:position()
            bool in_abs_path;               // true, if we continue AbsPath chain -- see PPAbsPath execution logic

            childOffer()
            {
                exi.isOrdered = true;
                exi.isDistincted = true;
                exi.isMax1 = true;
                exi.isSingleLevel = true;
                exi.useConstructors = false;

                use_last = false;
                use_position = false;
                
                in_abs_path = false;

                isCached = false;
            }
        };

        bool param_mode; // true, if we are checking function params now (ASTVar sema analysis)
        unsigned int param_count; // number of parameters found in param_mode
        bool isModeOrdered;     // cuurent mode of operation (global + may change on ordered-unordered expressions)

        std::vector<XQVariable> bound_vars; // vector of variables bound in the current expression (we need only names there)

        std::vector<childOffer> offers; // offers from children go in this sequence

        typedef std::map<std::string, XQFunction *> funcInfo;
        typedef std::map<std::string, XQVariable *> varInfo;

        funcInfo funcCache; // cache containing info about processed functions
        varInfo varCache; // cache containg info about processed global and lib variables

        void setParamMode();
        void unsetParamMode();

        bool isOfferCorrect(const childOffer &off);
        childOffer getOffer();
        void setOffer(const childOffer &off);

        void VisitNodesVector(ASTNodesVector *nodes, ASTVisitor &v, parentRequest req);

        childOffer mergeOffers(unsigned int count);
        childOffer mergeOffersSwitch(unsigned int count);

        parentRequest parentReq; // request from parent to child
        std::vector<parentRequest> pareqs; // parent requests
        const parentRequest &getParentRequest() const;
        void setParentRequest(const parentRequest &preq);

        void cacheTheNode(ASTNode *nod, childOffer &off) const;
        void ignoreVariables(LReturn::childOffer &coff, unsigned int count);

    public:
        LReturn(sedna::XQueryDriver *drv_, sedna::XQueryModule *mod_) : ASTVisitor(drv_, mod_)
        {
            param_mode = false;
            param_count = 0;
            isModeOrdered = mod->getOrderedMode();
            pareqs.push_back(parentRequest());
        }

        ~LReturn()
        {
        }

        void setOrderedMode(bool mode)
        {
            isModeOrdered = mode;
        }

        virtual void addToPath(ASTNode *nod);
        virtual void removeFromPath(ASTNode *nod);

        XQFunction *getFunctionInfo(const std::string &name);
        XQVariable *getVariableInfo(const std::string &name);

        ASTNode *checkIfPosConjunct(const ASTNode *n);

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
