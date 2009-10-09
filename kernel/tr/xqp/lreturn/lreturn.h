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

            parentRequest()
            {
                calledOnce = true;
                distinctOnly = false;
            }
        };

        struct childOffer
        {
            bool isOrdered;     // child is ordered
            bool isDistincted;  // child contains distincted values
            bool isMax1;        // child emits singleton or empty sequence
            bool isSingleLevel; // all nodes are on the same level in node-sequence

            bool isCached;                  // true, if child has been cached
            bool useConstructors;           // true, if child subexpression uses constructor (direct or computed)
            std::set<std::string> usedVars; // contains bound variables used in subexpression

            childOffer()
            {
                isOrdered = true;
                isDistincted = true;
                isMax1 = true;
                isSingleLevel = true;

                isCached = false;
                useConstructors = false;
            }
        };

        bool param_mode; // true, if we are checking function params now (ASTVar sema analysis)
        unsigned int param_count; // number of parameters found in param_mode
        bool isModeOrdered;     // cuurent mode of operation (global + may change on ordered-unordered expressions)

        std::vector<std::string> bound_vars; // vector of variables bound in the current expression (we need only names there)

        std::vector<childOffer> offers; // offers from children go in this sequence

        void setParamMode();
        void unsetParamMode();

        bool isOfferCorrect(const childOffer &off);
        childOffer getOffer();
        void setOffer(const childOffer &off);

        void VisitNodesVector(ASTNodesVector *nodes, ASTVisitor &v, parentRequest req);

        childOffer mergeOffers(unsigned int count);

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
            isModeOrdered = true; // ddo for default
            pareqs.push_back(parentRequest());
        }

        ~LReturn()
        {
        }

        virtual void addToPath(ASTNode *nod);
        virtual void removeFromPath(ASTNode *nod);

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
        void visit(ASTOrderByRet &n);
        void visit(ASTOrderEmpty &n);
        void visit(ASTOrderMod &n);
        void visit(ASTOrderModInt &n);
        void visit(ASTOrderSpec &n);
        void visit(ASTPIConst &n);
        void visit(ASTPi &n);
        void visit(ASTPiTest &n);
        void visit(ASTPosVar &n);
        void visit(ASTPragma &n);
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
        void visit(ASTUnio &n);
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
