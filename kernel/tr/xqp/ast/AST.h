/*
 * File:  AST.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _AST_NODES_
#define _AST_NODES_

#include "tr/xqp/ast/ASTAlterUser.h"
#include "tr/xqp/ast/ASTAttr.h"
#include "tr/xqp/ast/ASTAttrConst.h"
#include "tr/xqp/ast/ASTAttribTest.h"
#include "tr/xqp/ast/ASTBaseURI.h"
#include "tr/xqp/ast/ASTBop.h"
#include "tr/xqp/ast/ASTBoundSpaceDecl.h"
#include "tr/xqp/ast/ASTCase.h"
#include "tr/xqp/ast/ASTCast.h"
#include "tr/xqp/ast/ASTCastable.h"
#include "tr/xqp/ast/ASTCharCont.h"
#include "tr/xqp/ast/ASTCommentConst.h"
#include "tr/xqp/ast/ASTCommTest.h"
#include "tr/xqp/ast/ASTConstDecl.h"
#include "tr/xqp/ast/ASTCreateColl.h"
#include "tr/xqp/ast/ASTCreateDoc.h"
#include "tr/xqp/ast/ASTCreateFtIndex.h"
#include "tr/xqp/ast/ASTCreateIndex.h"
#include "tr/xqp/ast/ASTCreateRole.h"
#include "tr/xqp/ast/ASTCreateTrg.h"
#include "tr/xqp/ast/ASTCreateUser.h"
#include "tr/xqp/ast/ASTDDO.h"
#include "tr/xqp/ast/ASTDeclareCopyNsp.h"
#include "tr/xqp/ast/ASTDefCollation.h"
#include "tr/xqp/ast/ASTDefNamespaceDecl.h"
#include "tr/xqp/ast/ASTDocConst.h"
#include "tr/xqp/ast/ASTDocTest.h"
#include "tr/xqp/ast/ASTDropColl.h"
#include "tr/xqp/ast/ASTDropDoc.h"
#include "tr/xqp/ast/ASTDropFtIndex.h"
#include "tr/xqp/ast/ASTDropIndex.h"
#include "tr/xqp/ast/ASTDropMod.h"
#include "tr/xqp/ast/ASTDropRole.h"
#include "tr/xqp/ast/ASTDropTrg.h"
#include "tr/xqp/ast/ASTDropUser.h"
#include "tr/xqp/ast/ASTElem.h"
#include "tr/xqp/ast/ASTElemConst.h"
#include "tr/xqp/ast/ASTElementTest.h"
#include "tr/xqp/ast/ASTEmptyTest.h"
#include "tr/xqp/ast/ASTError.h"
#include "tr/xqp/ast/ASTExtExpr.h"
#include "tr/xqp/ast/ASTFLWOR.h"
#include "tr/xqp/ast/ASTFor.h"
#include "tr/xqp/ast/ASTFunCall.h"
#include "tr/xqp/ast/ASTFuncDecl.h"
#include "tr/xqp/ast/ASTGrantPriv.h"
#include "tr/xqp/ast/ASTGrantRole.h"
#include "tr/xqp/ast/ASTIf.h"
#include "tr/xqp/ast/ASTInstOf.h"
#include "tr/xqp/ast/ASTItemTest.h"
#include "tr/xqp/ast/ASTLet.h"
#include "tr/xqp/ast/ASTLibModule.h"
#include "tr/xqp/ast/ASTLit.h"
#include "tr/xqp/ast/ASTLoadFile.h"
#include "tr/xqp/ast/ASTLoadModule.h"
#include "tr/xqp/ast/ASTMainModule.h"
#include "tr/xqp/ast/ASTMetaCols.h"
#include "tr/xqp/ast/ASTMetaDocs.h"
#include "tr/xqp/ast/ASTMetaSchemaDoc.h"
#include "tr/xqp/ast/ASTMetaSchemaCol.h"
#include "tr/xqp/ast/ASTModImport.h"
#include "tr/xqp/ast/ASTModuleDecl.h"
#include "tr/xqp/ast/ASTNamespaceDecl.h"
#include "tr/xqp/ast/ASTNameTest.h"
#include "tr/xqp/ast/ASTNodeTest.h"
#include "tr/xqp/ast/ASTNsp.h"
#include "tr/xqp/ast/ASTOption.h"
#include "tr/xqp/ast/ASTOrdExpr.h"
#include "tr/xqp/ast/ASTOrder.h"
#include "tr/xqp/ast/ASTOrderBy.h"
#include "tr/xqp/ast/ASTOrderByRet.h"
#include "tr/xqp/ast/ASTOrderEmpty.h"
#include "tr/xqp/ast/ASTOrderMod.h"
#include "tr/xqp/ast/ASTOrderModInt.h"
#include "tr/xqp/ast/ASTOrderSpec.h"
#include "tr/xqp/ast/ASTPi.h"
#include "tr/xqp/ast/ASTPiTest.h"
#include "tr/xqp/ast/ASTPIConst.h"
#include "tr/xqp/ast/ASTPosVar.h"
#include "tr/xqp/ast/ASTPragma.h"
#include "tr/xqp/ast/ASTPred.h"
#include "tr/xqp/ast/ASTProlog.h"
#include "tr/xqp/ast/ASTQName.h"
#include "tr/xqp/ast/ASTQuantExpr.h"
#include "tr/xqp/ast/ASTQuery.h"
#include "tr/xqp/ast/ASTRenameColl.h"
#include "tr/xqp/ast/ASTRevokePriv.h"
#include "tr/xqp/ast/ASTRevokeRole.h"
#include "tr/xqp/ast/ASTSchemaAttrTest.h"
#include "tr/xqp/ast/ASTSchemaElemTest.h"
#include "tr/xqp/ast/ASTSeq.h"
#include "tr/xqp/ast/ASTSpaceSeq.h"
#include "tr/xqp/ast/ASTStep.h"
#include "tr/xqp/ast/ASTTextConst.h"
#include "tr/xqp/ast/ASTTextTest.h"
#include "tr/xqp/ast/ASTTreat.h"
#include "tr/xqp/ast/ASTType.h"
#include "tr/xqp/ast/ASTTypeSeq.h"
#include "tr/xqp/ast/ASTTypeSingle.h"
#include "tr/xqp/ast/ASTTypeSwitch.h"
#include "tr/xqp/ast/ASTTypeVar.h"
#include "tr/xqp/ast/ASTUop.h"
#include "tr/xqp/ast/ASTUnio.h"
#include "tr/xqp/ast/ASTUpdInsert.h"
#include "tr/xqp/ast/ASTUpdDel.h"
#include "tr/xqp/ast/ASTUpdMove.h"
#include "tr/xqp/ast/ASTUpdReplace.h"
#include "tr/xqp/ast/ASTUpdRename.h"
#include "tr/xqp/ast/ASTVar.h"
#include "tr/xqp/ast/ASTVarDecl.h"
#include "tr/xqp/ast/ASTVersionDecl.h"
#include "tr/xqp/ast/ASTXMLComm.h"

// All available ast node types
enum ASTNodeType
{
    AST_ALTERUSER = 1,
    AST_ATTRCONST,
    AST_ATTR,
    AST_ATTRIBTEST,
    AST_AXISSTEP,
    AST_BASEURI,
    AST_BOP,
    AST_BOUNDSPACEDECL,
    AST_CASE,
    AST_CASTABLE,
    AST_CAST,
    AST_CHARCONT,
    AST_COMMENTCONST,
    AST_COMMTEST,
    AST_CONSTDECL,
    AST_CREATECOLL,
    AST_CREATEDOC,
    AST_CREATEFTINDEX,
    AST_CREATEINDEX,
    AST_CREATEROLE,
    AST_CREATETRG,
    AST_CREATEUSER,
    AST_DDO,
    AST_DECLARECOPYNSP,
    AST_DEFCOLLATION,
    AST_DEFNAMESPACEDECL,
    AST_DOCCONST,
    AST_DOCTEST,
    AST_DROPCOLL,
    AST_DROPDOC,
    AST_DROPFTINDEX,
    AST_DROPINDEX,
    AST_DROPMOD,
    AST_DROPROLE,
    AST_DROPTRG,
    AST_DROPUSER,
    AST_ELEMCONST,
    AST_ELEMENTTEST,
    AST_ELEM,
    AST_EMPTYTEST,
    AST_ERROR,
    AST_EXTEXPR,
    AST_FILTERSTEP,
    AST_FLWOR,
    AST_FOR,
    AST_FUNCALL,
    AST_FUNCDECL,
    AST_GRANTPRIV,
    AST_GRANTROLE,
    AST_IF,
    AST_INSTOF,
    AST_ITEMTEST,
    AST_LET,
    AST_LIBMODULE,
    AST_LIT,
    AST_LOADFILE,
    AST_LOADMODULE,
    AST_MAINMODULE,
    AST_METACOLS,
    AST_METADOCS,
    AST_METASCHEMACOL,
    AST_METASCHEMADOC,
    AST_MODIMPORT,
    AST_MODULEDECL,
    AST_NAMESPACEDECL,
    AST_NAMETEST,
    AST_NODE,
    AST_NODETEST,
    AST_NSP,
    AST_OPTION,
    AST_ORDERBY,
    AST_ORDERBYRET,
    AST_ORDEREMPTY,
    AST_ORDER,
    AST_ORDERMOD,
    AST_ORDERMODINT,
    AST_ORDERSPEC,
    AST_ORDEXPR,
    AST_PICONST,
    AST_PI,
    AST_PITEST,
    AST_POSVAR,
    AST_PRAGMA,
    AST_PRED,
    AST_PROLOG,
    AST_QNAME,
    AST_QUANTEXPR,
    AST_QUERY,
    AST_RENAMECOLL,
    AST_REVOKEPRIV,
    AST_REVOKEROLE,
    AST_SCHEMAATTRTEST,
    AST_SCHEMAELEMTEST,
    AST_SEQ,
    AST_SPACESEQ,
    AST_TEXTCONST,
    AST_TEXTTEST,
    AST_TREAT,
    AST_TYPE,
    AST_TYPESEQ,
    AST_TYPESINGLE,
    AST_TYPESWITCH,
    AST_TYPEVAR,
    AST_UNIO,
    AST_UOP,
    AST_UPDDEL,
    AST_UPDINSERT,
    AST_UPDMOVE,
    AST_UPDRENAME,
    AST_UPDREPLACE,
    AST_VARDECL,
    AST_VAR,
    AST_VERSIONDECL,
    AST_XMLCOMM,

    AST_DUMMY
};

#endif
