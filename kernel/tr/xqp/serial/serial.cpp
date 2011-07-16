/*
 * File:  serial.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "serial.h"
#include "common/errdbg/exceptions.h"

#define DUMP_STR(s) \
    do\
    {\
        sc = "";\
        for (unsigned int ii = 0; ii < (s).size(); ii++)\
            if ((s)[ii] == '\\')\
                sc += "\\\\";\
            else if ((s)[ii] == '\"')\
                sc += "\\\"";\
            else\
                sc.push_back((s)[ii]);\
        int_str.append(std::string(" \"") + sc + "\" ");\
    }\
    while (0)

#define DUMP_INT(i) do { int_str.append(std::string(" ") + int2string(i) + " "); } while (0)
#define DUMP_NULL ("(0)")
#define DUMP_BOOL(b) (int_str.append((b) ? (" #t ") : (" #f ")))

static std::string sc;

inline std::string IntVisitor::dumpCommonData(const ASTNodeCommonData &cd)
{
    return "(" + int2string(cd.loc.begin.line) + " " + int2string(cd.loc.begin.column) + " " +
            int2string(cd.loc.end.line) + " " + int2string(cd.loc.end.column) + (cd.isCached ? " #t" : " #f") + ")";
}

inline void IntVisitor::dumpASTNodesVector(ASTNodesVector *vec)
{
    if (vec == NULL)
    {
        int_str.append("()");
    }
    else
    {
        int_str.append("(");
        VisitNodesVector(vec, *this);
        int_str.append(")");
    }
}

inline void IntVisitor::dumpASTStrings(const ASTStringVector *vec)
{
    if (vec == NULL)
    {
        int_str.append("()");
    }
    else
    {
        int_str.append("(");

        for (unsigned int i = 0; i < vec->size(); i++)
            DUMP_STR(*((*vec)[i]));

        int_str.append(")");
    }
}

inline void IntVisitor::dumpASTNode(ASTNode *node)
{
    if (node == NULL)
    {
        int_str.append(DUMP_NULL);
    }
    else
    {
        node->accept(*this);
    }
}

void IntVisitor::visit(ASTAlterUser &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_ALTERUSER));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.user);
    DUMP_STR(*n.psw);
    int_str.append(") ");
}

void IntVisitor::visit(ASTAttr &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_ATTR));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.pref);
    DUMP_STR(*n.local);
    dumpASTNodesVector(n.cont);
    DUMP_BOOL(n.deep_copy);
    if (n.uri)
        DUMP_STR(*n.uri);
    int_str.append(")");
}

void IntVisitor::visit(ASTAttrConst &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_ATTRCONST));
    int_str.append(dumpCommonData(n.cd));

    if (n.name)
    {
        dumpASTNode(n.name);
    }
    else
    {
        DUMP_STR(*n.pref);
        DUMP_STR(*n.local);
    }

    dumpASTNode(n.expr);
    DUMP_BOOL(n.deep_copy);
    int_str.append(")");
}

void IntVisitor::visit(ASTAttribTest &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_ATTRIBTEST));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.name);
    dumpASTNode(n.type);
    int_str.append(")");
}

void IntVisitor::visit(ASTAxisStep &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_AXISSTEP));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.cont);
    int_str.append(int2string(n.axis));
    dumpASTNode(n.test);
    dumpASTNodesVector(n.preds);

    if (n.isLast)
        int_str.append(" #t");
    else
        int_str.append(" #f");

    int_str.append(")");
}

void IntVisitor::visit(ASTBaseURI &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_BASEURI));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.uri);
    int_str.append(")");
}

void IntVisitor::visit(ASTBop &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_BOP));
    int_str.append(dumpCommonData(n.cd));
    int_str.append(int2string(n.op));
    dumpASTNode(n.lop);
    dumpASTNode(n.rop);
    DUMP_BOOL(n.doc_order);
    int_str.append(")");
}

void IntVisitor::visit(ASTBoundSpaceDecl &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_BOUNDSPACEDECL));
    int_str.append(dumpCommonData(n.cd));
    int_str.append(int2string(n.mod));
    int_str.append(")");
}

void IntVisitor::visit(ASTCase &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_CASE));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.var);
    dumpASTNode(n.type);
    dumpASTNode(n.expr);
    int_str.append(")");
}

void IntVisitor::visit(ASTCast &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_CAST));
    int_str.append(dumpCommonData(n.cd));

    dumpASTNode(n.expr);
    dumpASTNode(n.type);

    int_str.append(")");
}

void IntVisitor::visit(ASTCastable &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_CASTABLE));
    int_str.append(dumpCommonData(n.cd));

    dumpASTNode(n.expr);
    dumpASTNode(n.type);

    int_str.append(")");
}

void IntVisitor::visit(ASTCharCont &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_CHARCONT));
    int_str.append(dumpCommonData(n.cd));

    DUMP_STR(*n.cont);
    int_str.append(int2string(n.orig));
    int_str.append(")");
}

void IntVisitor::visit(ASTCommTest &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_COMMTEST));
    int_str.append(dumpCommonData(n.cd));
    int_str.append(")");
}

void IntVisitor::visit(ASTCommentConst &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_COMMENTCONST));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.expr);
    DUMP_BOOL(n.deep_copy);
    int_str.append(")");
}

void IntVisitor::visit(ASTConstDecl &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_CONSTDECL));
    int_str.append(dumpCommonData(n.cd));
    int_str.append(int2string(n.mod));
    int_str.append(")");
}

void IntVisitor::visit(ASTCreateColl &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_CREATECOLL));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.coll);
    int_str.append(")");
}

void IntVisitor::visit(ASTCreateDoc &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_CREATEDOC));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.doc);
    dumpASTNode(n.coll);
    int_str.append(")");
}

void IntVisitor::visit(ASTCreateFtIndex &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_CREATEFTINDEX));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.name);
    dumpASTNode(n.path);
    DUMP_STR(*n.type);
    dumpASTNode(n.cust_expr);
	dumpASTNode(n.options);
    int_str.append(")");
}

void IntVisitor::visit(ASTCreateIndex &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_CREATEINDEX));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.name);
    dumpASTNode(n.on_path);
    dumpASTNode(n.by_path);
    dumpASTNode(n.type);
    DUMP_STR(*n.tree_type);
    int_str.append(")");
}

void IntVisitor::visit(ASTCreateRole &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_CREATEROLE));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.role);
    int_str.append(")");
}

void IntVisitor::visit(ASTCreateTrg &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_CREATETRG));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.name);
    int_str.append(int2string(n.t_mod));
    int_str.append(" ");
    int_str.append(int2string(n.a_mod));
    dumpASTNode(n.path);
    int_str.append(int2string(n.g_mod));
    dumpASTNodesVector(n.do_exprs);

    if (n.leaf_name)
    {
        U_ASSERT(n.leaf_type >= 0 && n.trimmed_path);

        DUMP_STR(*n.leaf_name);
        int_str.append(" ");
        int_str.append(int2string(n.leaf_type));
        dumpASTNode(n.trimmed_path);
    }

    int_str.append(")");
}

void IntVisitor::visit(ASTCreateUser &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_CREATEUSER));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.user);
    DUMP_STR(*n.psw);
    int_str.append(")");
}

void IntVisitor::visit(ASTDDO &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_DDO));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.expr);
    if (n.true_ddo)
        int_str.append(" #t");
    else
        int_str.append(" #f");
    int_str.append(")");
}

void IntVisitor::visit(ASTDeclareCopyNsp &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_DECLARECOPYNSP));
    int_str.append(dumpCommonData(n.cd));
    int_str.append(int2string(n.pres_mod));
    int_str.append(" ");
    int_str.append(int2string(n.inh_mod));
    int_str.append(")");
}

void IntVisitor::visit(ASTDefCollation &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_DEFCOLLATION));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.uri);
    int_str.append(")");
}

void IntVisitor::visit(ASTDefNamespaceDecl &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_DEFNAMESPACEDECL));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.uri);
    int_str.append(int2string(n.type));
    int_str.append(")");
}

void IntVisitor::visit(ASTDocConst &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_DOCCONST));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.expr);
    int_str.append(")");
}

void IntVisitor::visit(ASTDocTest &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_DOCTEST));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.elem_test);
    int_str.append(")");
}

void IntVisitor::visit(ASTDropColl &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_DROPCOLL));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.coll);
    int_str.append(")");
}

void IntVisitor::visit(ASTDropDoc &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_DROPDOC));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.doc);
    dumpASTNode(n.coll);
    int_str.append(")");
}

void IntVisitor::visit(ASTDropFtIndex &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_DROPFTINDEX));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.index);
    int_str.append(")");
}

void IntVisitor::visit(ASTDropIndex &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_DROPINDEX));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.index);
    int_str.append(")");
}

void IntVisitor::visit(ASTDropMod &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_DROPMOD));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.module);
    int_str.append(")");
}

void IntVisitor::visit(ASTDropRole &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_DROPROLE));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.role);
    int_str.append(")");
}

void IntVisitor::visit(ASTDropTrg &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_DROPTRG));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.trg);
    int_str.append(")");
}

void IntVisitor::visit(ASTDropUser &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_DROPUSER));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.user);
    int_str.append(")");
}

void IntVisitor::visit(ASTElem &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_ELEM));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.pref);
    DUMP_STR(*n.local);
    dumpASTNodesVector(n.attrs);
    dumpASTNodesVector(n.cont);
    DUMP_BOOL(n.deep_copy);
    int_str.append(")");
}

void IntVisitor::visit(ASTElemConst &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_ELEMCONST));
    int_str.append(dumpCommonData(n.cd));

    if (n.name)
    {
        dumpASTNode(n.name);
    }
    else
    {
        DUMP_STR(*n.pref);
        DUMP_STR(*n.local);
    }

    dumpASTNode(n.expr);
    DUMP_BOOL(n.deep_copy);
    int_str.append(")");
}

void IntVisitor::visit(ASTElementTest &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_ELEMENTTEST));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.name);
    dumpASTNode(n.type);
    int_str.append(int2string(n.mod));
    int_str.append(")");
}

void IntVisitor::visit(ASTEmptyTest &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_EMPTYTEST));
    int_str.append(dumpCommonData(n.cd));
    int_str.append(")");
}

void IntVisitor::visit(ASTError &n)
{
    throw SYSTEM_EXCEPTION("If you see this, you are very unlucky. Anyway, this is an internal parser error.");
}

void IntVisitor::visit(ASTExtExpr &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_EXTEXPR));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNodesVector(n.pragmas);
    dumpASTNode(n.expr);
    int_str.append(")");
}

void IntVisitor::visit(ASTFilterStep &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_FILTERSTEP));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.cont);
    dumpASTNode(n.expr);
    dumpASTNodesVector(n.preds);

    DUMP_BOOL(n.isLast);
    DUMP_BOOL(n.use_last);
    DUMP_BOOL(n.use_pos);

    int_str.append(")");
}

void IntVisitor::visit(ASTFLWOR &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_FLWOR));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNodesVector(n.fls);
    dumpASTNode(n.where);
    dumpASTNode(n.order_by);
    dumpASTNode(n.ret);
    int_str.append(")");
}

void IntVisitor::visit(ASTFor &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_FOR));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.tv);
    dumpASTNode(n.pv);
    dumpASTNode(n.expr);
    int_str.append(")");
}

void IntVisitor::visit(ASTFunCall &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_FUNCALL));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.pref);
    DUMP_STR(*n.local);
    dumpASTNodesVector(n.params);

    if (n.uri)
    {
        DUMP_STR(*n.uri);

        if (n.int_name)
        {
            DUMP_STR(*n.int_name);
        }
    }

    int_str.append(")");
}

void IntVisitor::visit(ASTFuncDecl &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_FUNCDECL));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.pref);
    DUMP_STR(*n.local);
    dumpASTNodesVector(n.params);
    dumpASTNode(n.ret);
    dumpASTNode(n.body);

    if (n.func_uri)
        DUMP_STR(*n.func_uri);

    int_str.append(")");
}

void IntVisitor::visit(ASTGrantPriv &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_GRANTPRIV));
    int_str.append(dumpCommonData(n.cd));
    int_str.append(int2string(n.mod));
    DUMP_STR(*n.user);
    DUMP_STR(*n.priv);

    if (n.obj)
        DUMP_STR(*n.obj);

    int_str.append(")");
}

void IntVisitor::visit(ASTGrantRole &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_GRANTROLE));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.role);
    DUMP_STR(*n.role_to);
    int_str.append(")");
}

void IntVisitor::visit(ASTIf &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_IF));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.i_expr);
    dumpASTNode(n.t_expr);
    dumpASTNode(n.e_expr);
    int_str.append(")");
}

void IntVisitor::visit(ASTInstOf &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_INSTOF));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.expr);
    dumpASTNode(n.type);
    int_str.append(")");
}

void IntVisitor::visit(ASTItemTest &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_ITEMTEST));
    int_str.append(dumpCommonData(n.cd));
    int_str.append(")");
}

void IntVisitor::visit(ASTLet &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_LET));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.tv);
    dumpASTNode(n.expr);
    int_str.append(")");
}

void IntVisitor::visit(ASTLibModule &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_LIBMODULE));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.moduleDecl);
    dumpASTNode(n.prolog);
    int_str.append(")");
}

void IntVisitor::visit(ASTLit &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_LIT));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.lit);
    int_str.append(int2string(n.type));
    int_str.append(")");
}

void IntVisitor::visit(ASTLoadFile &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_LOADFILE));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.file);
    DUMP_STR(*n.doc);

    if (n.coll)
        DUMP_STR(*n.coll);

    int_str.append(")");
}

void IntVisitor::visit(ASTLoadModule &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_LOADMODULE));
    int_str.append(dumpCommonData(n.cd));
    dumpASTStrings(n.modules);
    int_str.append(int2string(n.mod));
    int_str.append(")");
}

void IntVisitor::visit(ASTMainModule &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_MAINMODULE));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.prolog);
    dumpASTNode(n.query);
    int_str.append(")");
}

void IntVisitor::visit(ASTMetaCols &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_METACOLS));
    int_str.append(dumpCommonData(n.cd));

    if (n.need_stats)
        int_str.append(" #t");
    else
        int_str.append(" #f");

    int_str.append(")");
}

void IntVisitor::visit(ASTMetaDocs &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_METADOCS));
    int_str.append(dumpCommonData(n.cd));

    if (n.need_stats)
        int_str.append(" #t");
    else
        int_str.append(" #f");

    dumpASTNode(n.coll);

    int_str.append(")");
}

void IntVisitor::visit(ASTMetaSchemaCol &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_METASCHEMACOL));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.coll);
    int_str.append(")");
}

void IntVisitor::visit(ASTMetaSchemaDoc &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_METASCHEMADOC));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.doc);
    dumpASTNode(n.coll);
    int_str.append(")");
}

void IntVisitor::visit(ASTModImport &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_MODIMPORT));
    int_str.append(dumpCommonData(n.cd));

    if (n.name)
        DUMP_STR(*n.name);
    else
        int_str.append("\"\"");

    DUMP_STR(*n.uri);
    dumpASTStrings(n.hints);
    int_str.append(")");
}


void IntVisitor::visit(ASTModuleDecl &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_MODULEDECL));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.name);
    DUMP_STR(*n.uri);
    int_str.append(")");
}


void IntVisitor::visit(ASTNameTest &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_NAMETEST));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.pref);
    DUMP_STR(*n.local);

    if (n.uri)
        DUMP_STR(*n.uri);

    int_str.append(")");
}

void IntVisitor::visit(ASTNamespaceDecl &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_NAMESPACEDECL));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.name);
    DUMP_STR(*n.uri);
    int_str.append(")");
}

void IntVisitor::visit(ASTNodeTest &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_NODETEST));
    int_str.append(dumpCommonData(n.cd));
    int_str.append(")");
}

void IntVisitor::visit(ASTNsp &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_NSP));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.name);

    if (n.cont)
        DUMP_STR(*n.cont);

    int_str.append(")");
}

void IntVisitor::visit(ASTOption &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_OPTION));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.pref);
    DUMP_STR(*n.local);
    DUMP_STR(*n.opt);

    if (n.uri)
    {
        DUMP_STR(*n.uri);

        U_ASSERT(n.options != NULL);
        int_str.append("(");
        for (unsigned int i = 0; i < n.options->size(); i++)
        {
            int_str.append("(");
            DUMP_STR((*n.options)[i].first);
            DUMP_STR((*n.options)[i].second);
            int_str.append(")");
        }
        int_str.append(")");
    }

    int_str.append(")");
}

void IntVisitor::visit(ASTOrdExpr &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_ORDEXPR));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.expr);
    int_str.append(int2string(n.type));
    int_str.append(")");
}

void IntVisitor::visit(ASTOrder &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_ORDER));
    int_str.append(dumpCommonData(n.cd));
    int_str.append(int2string(n.mod));
    int_str.append(")");
}

void IntVisitor::visit(ASTOrderBy &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_ORDERBY));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNodesVector(n.specs);
    int_str.append(int2string(n.type));
    int_str.append(")");
}

void IntVisitor::visit(ASTOrderEmpty &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_ORDEREMPTY));
    int_str.append(dumpCommonData(n.cd));
    int_str.append(int2string(n.mod));
    int_str.append(")");
}

void IntVisitor::visit(ASTOrderMod &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_ORDERMOD));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.ad_mod);
    dumpASTNode(n.em_mod);
    dumpASTNode(n.col_mod);
    int_str.append(")");
}

void IntVisitor::visit(ASTOrderModInt &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_ORDERMODINT));
    int_str.append(dumpCommonData(n.cd));
    int_str.append(int2string(n.mod));

    if (n.uri)
        DUMP_STR(*n.uri);

    int_str.append(")");
}

void IntVisitor::visit(ASTOrderSpec &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_ORDERSPEC));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.expr);
    dumpASTNode(n.mod);
    int_str.append(")");
}

void IntVisitor::visit(ASTPIConst &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_PICONST));
    int_str.append(dumpCommonData(n.cd));

    if (n.name)
        dumpASTNode(n.name);
    else
        DUMP_STR(*n.ncname);

    dumpASTNode(n.expr);
    DUMP_BOOL(n.deep_copy);
    int_str.append(")");
}

void IntVisitor::visit(ASTPi &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_PI));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.name);
    DUMP_STR(*n.cont);
    DUMP_BOOL(n.deep_copy);
    int_str.append(")");
}

void IntVisitor::visit(ASTPiTest &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_PITEST));
    int_str.append(dumpCommonData(n.cd));
    int_str.append(int2string(n.type));

    if (n.test)
        DUMP_STR(*n.test);

    int_str.append(")");
}

void IntVisitor::visit(ASTPosVar &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_POSVAR));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.var);
    int_str.append(")");
}

void IntVisitor::visit(ASTPragma &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_PRAGMA));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.pref);
    DUMP_STR(*n.local);
    DUMP_STR(*n.cont);
    int_str.append(")");
}

void IntVisitor::visit(ASTPred &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_PRED));
    int_str.append(dumpCommonData(n.cd));
    n.seriliazeConjuncts(int_str, *this);
    int_str.append(")");
}

void IntVisitor::visit(ASTProlog &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_PROLOG));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNodesVector(n.decls);
    int_str.append(")");
}

void IntVisitor::visit(ASTQName &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_QNAME));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.uri);
    DUMP_STR(*n.pref);
    DUMP_STR(*n.local);
    int_str.append(")");
}

void IntVisitor::visit(ASTQuantExpr &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_QUANTEXPR));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.var);
    dumpASTNode(n.expr);
    dumpASTNode(n.sat);
    int_str.append(int2string(n.type));
    int_str.append(")");
}

void IntVisitor::visit(ASTQuery &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_QUERY));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.query);
    int_str.append(int2string(n.type));
    DUMP_BOOL(n.is_trigger);
    int_str.append(")");
}

void IntVisitor::visit(ASTRenameColl &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_RENAMECOLL));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.name_old);
    dumpASTNode(n.name_new);
    int_str.append(")");
}

void IntVisitor::visit(ASTRevokePriv &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_REVOKEPRIV));
    int_str.append(dumpCommonData(n.cd));
    int_str.append(int2string(n.mod));
    DUMP_STR(*n.user);
    DUMP_STR(*n.priv);

    if (n.obj)
        DUMP_STR(*n.obj);

    int_str.append(")");
}

void IntVisitor::visit(ASTRevokeRole &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_REVOKEROLE));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.role);
    DUMP_STR(*n.role_from);
    int_str.append(")");
}

void IntVisitor::visit(ASTSchemaAttrTest &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_SCHEMAATTRTEST));
    int_str.append(dumpCommonData(n.cd));
    n.name->accept(*this);
    int_str.append(")");
}

void IntVisitor::visit(ASTSchemaElemTest &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_SCHEMAELEMTEST));
    int_str.append(dumpCommonData(n.cd));
    n.name->accept(*this);
    int_str.append(")");
}

void IntVisitor::visit(ASTSeq &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_SEQ));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNodesVector(n.exprs);
    int_str.append(")");
}

void IntVisitor::visit(ASTSpaceSeq &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_SPACESEQ));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.expr);
    DUMP_BOOL(n.atomize);
    int_str.append(")");
}

void IntVisitor::visit(ASTTextConst &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_TEXTCONST));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.expr);
    DUMP_BOOL(n.deep_copy);
    int_str.append(")");
}

void IntVisitor::visit(ASTTextTest &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_TEXTTEST));
    int_str.append(dumpCommonData(n.cd));
    int_str.append(")");
}

void IntVisitor::visit(ASTTreat &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_TREAT));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.expr);
    dumpASTNode(n.type);
    int_str.append(")");
}

void IntVisitor::visit(ASTType &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_TYPE));
    int_str.append(dumpCommonData(n.cd));
    int_str.append(int2string(n.type));
    DUMP_STR(*n.name);
    int_str.append(")");
}

void IntVisitor::visit(ASTTypeSeq &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_TYPESEQ));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.type_test);
    int_str.append(int2string(n.mod));
    int_str.append(")");
}

void IntVisitor::visit(ASTTypeSingle &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_TYPESINGLE));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.type);
    int_str.append(int2string(n.mod));
    int_str.append(")");
}

void IntVisitor::visit(ASTTypeSwitch &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_TYPESWITCH));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.expr);
    dumpASTNodesVector(n.cases);
    dumpASTNode(n.def_case);
    int_str.append(")");
}

void IntVisitor::visit(ASTTypeVar &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_TYPEVAR));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.type);
    dumpASTNode(n.var);
    int_str.append(")");
}

void IntVisitor::visit(ASTUop &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_UOP));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.expr);
    int_str.append(int2string(n.op));
    int_str.append(")");
}

void IntVisitor::visit(ASTUpdDel &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_UPDDEL));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.what);
    int_str.append(int2string(n.type));
    int_str.append(")");
}

void IntVisitor::visit(ASTUpdInsert &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_UPDINSERT));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.what);
    dumpASTNode(n.where);
    int_str.append(int2string(n.type));
    int_str.append(")");
}

void IntVisitor::visit(ASTUpdMove &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_UPDMOVE));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.var);
    dumpASTNode(n.what);
    dumpASTNode(n.where);
    int_str.append(int2string(n.type));
    int_str.append(")");
}

void IntVisitor::visit(ASTUpdRename &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_UPDRENAME));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.what);
    DUMP_STR(*n.pref);
    DUMP_STR(*n.local);
    int_str.append(")");
}

void IntVisitor::visit(ASTUpdReplace &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_UPDREPLACE));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.var);
    dumpASTNode(n.what);
    dumpASTNode(n.new_expr);
    int_str.append(")");
}

void IntVisitor::visit(ASTVar &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_VAR));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.pref);
    DUMP_STR(*n.local);

    if (n.uri)
        DUMP_STR(*n.uri);

    int_str.append(")");
}

void IntVisitor::visit(ASTVarDecl &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_VARDECL));
    int_str.append(dumpCommonData(n.cd));
    dumpASTNode(n.var);
    dumpASTNode(n.type);
    dumpASTNode(n.expr);
    int_str.append(")");
}

void IntVisitor::visit(ASTVersionDecl &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_VERSIONDECL));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.xq_version);

    if (n.encoding)
        DUMP_STR(*n.encoding);

    int_str.append(")");
}

void IntVisitor::visit(ASTXMLComm &n)
{
    int_str.append("(");
    int_str.append(int2string(AST_XMLCOMM));
    int_str.append(dumpCommonData(n.cd));
    DUMP_STR(*n.cont);
    DUMP_BOOL(n.deep_copy);
    int_str.append(")");
}
