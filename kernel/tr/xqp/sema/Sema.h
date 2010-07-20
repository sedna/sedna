/*
 * File:  Sema.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _SEMA_VISITOR_H_
#define _SEMA_VISITOR_H_

#include "tr/xqp/visitor/ASTVisitor.h"
#include "tr/xqp/XQueryDriver.h"
#include "tr/xqp/XQueryModule.h"
#include <string>
#include <map>
#include <set>

namespace sedna
{
    class Sema : public ASTVisitor
    {
    private:
        enum DuplicatePrologsDecls
        {
            PrologBoundSpace = 0,
            PrologColl,
            PrologBaseURI,
            PrologDeclConst,
            PrologOrder,
            PrologOrderEmpty,
            PrologCopyNsp,

            PrologDummyEnd // to evaluate the size of the dupLocations
        };

        const ASTLocation *dupLocations[PrologDummyEnd]; // locations to diagnoze duplicate prolog elements

        bool is_postload; // true, if we import internal module; if so, we can skip some of the checks
        bool param_mode; // true, if we are checking function params now (ASTVar sema analysis)
        unsigned int param_count; // number of parameters found in param_mode
        bool casting_mode; // true, if we analyze types for cast or castable
        bool att_test; // true, if name test uri should be resolved as for attribute (default namespace uri issues)
        bool has_prolog; // true, if we've got prolog declarations

        typedef std::pair<nsBindType, nsPair> elNspInfo;
        std::vector<elNspInfo> elemNsps; // stack of pairs (namespaces, def.namespaces) overriden in direct elem constructor

        std::vector<XQVariable> bound_vars; // vector of variables bound in the current expression

        bool checkXQueryEncoding(const char *enc);
        const char *resolveQName(const ASTLocation &loc, const char *pref, const char *def_uri, int err_code = XPST0081);

        void parseOption(const ASTLocation &loc, const std::string &opt,
                         std::vector<std::pair<std::string, std::string> > &opts, const char delim);

        void rewriteStdFunCall(ASTFunCall &n, std::string name);
        ASTNode *getDocCollFromAbsXPathAndCheck(ASTNode *path, bool relative);
        void getLeafAndTrimmedPath(ASTNode *path, std::string **ln, int *lt, ASTNode **t_path);

        void setParamMode();
        void unsetParamMode();

    public:
        Sema(sedna::XQueryDriver *drv_, sedna::XQueryModule *mod_) : ASTVisitor(drv_, mod_)
        {
            for (unsigned int i = 0; i < PrologDummyEnd; i++)
                dupLocations[i] = NULL;

            is_postload = false;
            param_mode = false;
            param_count = 0;
            casting_mode = false;
            att_test = false;
        }

        ~Sema()
        {
        }

        static XQFunction *findFunction(std::string name, unsigned int arity, XQueryModule *mod, XQueryDriver *drv);
        static std::string uriFromGeneralName(const std::string &name);

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
