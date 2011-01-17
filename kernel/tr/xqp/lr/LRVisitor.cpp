/*
 * File:  LRVisitor.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "LRVisitor.h"
#include "common/errdbg/exceptions.h"
#include "tr/executor/xqops/PPSeqChecker.h"

#define LR_STR(s)\
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
        lr_str.append(std::string("(const (type !xs!string) ") + "\"" + sc + "\") ");\
    }\
    while (0)

#define LR_QNAME(p, l) (lr_str.append(std::string("(const (type !xs!QName) (\"") + *(p) + "\" \"" + *(l) + "\" )) "))
#define LR_QNAME_URI(p, l, u) (lr_str.append(std::string("(const (type !xs!QName) (\"") + *(u) + "\" \"" + *(l) + "\" \"" + *(p) + "\" )) "))
#define LR_NCNAME(n) (lr_str.append(std::string("(const (type !xs!NCName) ") + *(n) + ")"))
#define LR_NCNAME_QUOT(n) (lr_str.append(std::string("(const (type !xs!NCName) \"") + *(n) + "\")"))
#define LR_SINGLE(p) (lr_str.append(std::string("\"") + *(p) + "\""))
#define LR_PAIR(p1, p2) (lr_str.append(std::string("(\"") + *(p1) + "\" \"" + *(p2) + "\" ) "))

static const char *axis_str[] = {
    "child ",
    "descendant ",
    "attr-axis ",
    "self ",
    "descendant-or-self ",
    "descendant-attr ",
    "following-sibling ",
    "following ",
    "parent ",
    "ancestor ",
    "preceding-sibling ",
    "preceding ",
    "ancestor-or-self ",
 };

static const char *bops_str[] = {
    "or@ ",
    "and@ ",

    "to@ ",
    "+@ ",
    "-@ ",
    "*@ ",
    "/@ ",
    "idiv@ ",
    "mod@ ",

    "eq@ ",
    "ne@ ",
    "lt@ ",
    "le@ ",
    "gt@ ",
    "ge@ ",

    "is@ ",
    "<<@ ",
    ">>@ ",

    "=@ ",
    "!=@ ",
    "<@ ",
    "<=@ ",
    ">@ ",
    ">=@ ",

    "union@ ",
    "intersect@ ",
    "except@ ",
};

static const char *trg_str[] =
{
    "BEFORE",
    "AFTER",

    "INSERT",
    "DELETE",
    "REPLACE",

    "NODE",
    "STATEMENT"
};

static const char *query_str[] =
{
    "query ",
    "manage ",
    "update ",
    "retrieve-metadata "
};

static const char *ordmod_str[] =
{
    "asc",
    "desc",

    "empty-greatest",
    "empty-least",

    "collation "
};

static const char *occur_str[] =
{
    "one ",
    "optional ",
    "zero-or-more ",
    "one-or-more "
};

void LRVisitor::visit(ASTAlterUser &n)
{
    lr_str.append("(alter-user ");
    LR_STR(*n.user);
    LR_STR(*n.psw);
    lr_str.append(") ");
}

void LRVisitor::visit(ASTAttr &n)
{
    lr_str.append("(attribute  ");
    LR_QNAME(n.pref, n.local);

    if (n.cont == NULL || n.cont->size() > 1)
        lr_str.append("(sequence ");

    VisitNodesVector(n.cont, *this);

    if (n.cont == NULL || n.cont->size() > 1)
        lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTAttrConst &n)
{
    lr_str.append("(attribute  ");

    if (n.name == NULL)
        LR_QNAME(n.pref, n.local);
    else
        n.name->accept(*this);

    if (n.expr == NULL)
        lr_str.append("(sequence)");
    else
        n.expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTAttribTest &n)
{
    lr_str.append("(attr-test  ");

    if (n.name == NULL)
        lr_str.append("(ename (const (type !xs!QName) unspecified) ");
    else
        n.name->accept(*this);

    if (n.type == NULL)
        lr_str.append("(type unspecified)");
    else
    {
        lr_str.append("(type ");
        n.type->accept(*this);
        lr_str.append(")");
    }

    lr_str.append("(const (type !xs!string) \"non-nil\")");

    lr_str.append(")) ");
}

void LRVisitor::visit(ASTAxisStep &n)
{
    std::string cont = " (var (\"\" \"$%v\")) ";
    std::string lr_save;
    ASTFilterStep *fs;
    bool need_checker = !n.isFirstStep() && (dynamic_cast<ASTAxisStep *>(n.cont) == NULL);

    if (need_checker && (fs = dynamic_cast<ASTFilterStep*>(n.cont)))
    {
        if (!fs->expr)
            need_checker = false;
    }


//    lr_str.append("(ddo ");
    if (n.preds)
    {
        lr_str.append("(return ");

        if (n.cont)
        {
            if (need_checker)
            {
                lr_str.append("(seq-checker ");
            }
            n.cont->accept(*this);
            if (need_checker)
            {
                lr_str.append(int2string(PPSeqChecker::CHECK_NODE));
                lr_str.append(")");
            }
        }
        else
        {
            lr_str += cont;
        }

        lr_str.append("(fun-def ((!xs!anyType (var (\"\" \"$%v\")))) ");

        lr_save = lr_str;
        lr_str = "";

        lr_str.append("(");
        lr_str.append(axis_str[n.axis]);
        lr_str.append(cont);

        lr_str.append("(type ");
        n.test->accept(*this);
        lr_str.append("))");

        for (unsigned int i = 0; i < n.preds->size(); i++)
        {
            (*n.preds)[i]->accept(*this);
        }

        lr_str = lr_save + lr_str;
        lr_str.append("))");
    }
    else
    {
        lr_str.append("(");
        lr_str.append(axis_str[n.axis]);

        if (n.cont)
        {
            if (need_checker)
            {
                lr_str.append("(seq-checker ");
            }
            n.cont->accept(*this);
            if (need_checker)
            {
                lr_str.append(int2string(PPSeqChecker::CHECK_NODE));
                lr_str.append(")");
            }
        }
        else
        {
            lr_str += cont;
        }

        lr_str.append("(type ");
        n.test->accept(*this);
        lr_str.append("))");
    }
  //  lr_str.append(")");
}

void LRVisitor::visit(ASTBaseURI &n)
{
    lr_str.append("(declare-base-uri  ");
    LR_STR(*n.uri);
    lr_str.append(") ");
}

void LRVisitor::visit(ASTBop &n)
{
    lr_str.append("(");
    lr_str.append(bops_str[n.op]);

    n.lop->accept(*this);
    n.rop->accept(*this);

    if (n.op == ASTBop::INTERSECT || n.op == ASTBop::UNION || n.op == ASTBop::EXCEPT)
    {
        lr_str.append((n.doc_order) ? "true" : "false");
    }

    lr_str.append(") ");
}

void LRVisitor::visit(ASTBoundSpaceDecl &n)
{
    lr_str.append("(boundary-space-decl ");

    if (n.mod == ASTBoundSpaceDecl::STRIP)
        lr_str.append("(const (type !xs!string) \"strip\")");
    else
        lr_str.append("(const (type !xs!string) \"preserve\")");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTCase &n)
{
    if (n.type == NULL)
        lr_str.append("(default ");
    else
        lr_str.append("(case ");

    if (n.type)
    {
        lr_str.append("(type ");
        n.type->accept(*this);
        lr_str.append(") ");
    }

    lr_str.append("(fun-def (");

    if (n.var)
    {
        lr_str.append("(!xs!anyType ");
        n.var->accept(*this);
        lr_str.append(")");
    }
    else
    {
        lr_str.append("(!xs!anyType (var (\"\" \"%v\")))");
    }

    lr_str.append(")");

    n.expr->accept(*this);

    lr_str.append("))");
}

void LRVisitor::visit(ASTCast &n)
{
    lr_str.append("(cast ");

    n.expr->accept(*this);

    lr_str.append("(type  ");
    n.type ->accept(*this);
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTCastable &n)
{
    lr_str.append("(castable ");

    n.expr->accept(*this);

    lr_str.append("(type  ");
    n.type ->accept(*this);
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTCharCont &n)
{
    LR_STR(*n.cont);
}

void LRVisitor::visit(ASTCommTest &n)
{
    lr_str.append("(comment-test)");
}

void LRVisitor::visit(ASTCommentConst &n)
{
    lr_str.append("(comment ");
    n.expr->accept(*this);
    lr_str.append(") ");
}

void LRVisitor::visit(ASTConstDecl &n)
{
    lr_str.append("(declare-construction ");

    if (n.mod == ASTConstDecl::STRIP)
        lr_str.append("(const (type !xs!string) \"strip\")");
    else
        lr_str.append("(const (type !xs!string) \"preserve\")");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTCreateColl &n)
{
    lr_str.append("(create-collection ");
    n.coll->accept(*this);
    lr_str.append(") ");
}

void LRVisitor::visit(ASTCreateDoc &n)
{
    lr_str.append("(create-document ");
    n.doc->accept(*this);

    if (n.coll)
        n.coll->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTCreateFtIndex &n)
{
    lr_str.append("(create-fulltext-index ");
    n.name->accept(*this);
    n.path->accept(*this);
    LR_STR(*n.type);

    if (n.cust_expr)
        n.cust_expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTCreateIndex &n)
{
    lr_str.append("(create-index ");

    n.name->accept(*this);
    n.on_path->accept(*this);
    n.by_path->accept(*this);
    n.type->accept(*this);
    LR_STR(*n.tree_type);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTCreateRole &n)
{
    lr_str.append("(create-role ");
    LR_STR(*n.role);
    lr_str.append(") ");
}

void LRVisitor::visit(ASTCreateTrg &n)
{
    std::string *s = new std::string();

    lr_str.append("(create-trigger ");

    LR_STR(*n.name);

    *s = trg_str[n.t_mod];
    LR_STR(*s);

    *s = trg_str[n.a_mod];
    LR_STR(*s);

    n.path->accept(*this);

    *s = trg_str[n.g_mod];
    LR_STR(*s);

    lr_str.append("( ");
    VisitNodesVector(n.do_exprs, *this);
    lr_str.append(") ");

    if (n.leaf_name)
        lr_str.append(std::string("\"") + *n.leaf_name + "\"");

    lr_str.append(" ");

    if (n.leaf_type >= 0)
        lr_str.append(int2string(n.leaf_type));

    if (n.trimmed_path)
        n.trimmed_path->accept(*this);

    lr_str.append(") ");

    delete s;
}

void LRVisitor::visit(ASTCreateUser &n)
{
    lr_str.append("(create-user ");
    LR_STR(*n.user);
    LR_STR(*n.psw);
    lr_str.append(") ");
}

void LRVisitor::visit(ASTDDO &n)
{
    if (!n.true_ddo)
        lr_str.append("(distinct ");
    else
        lr_str.append("(ddo ");

    n.expr->accept(*this);
    lr_str.append(") ");
}

void LRVisitor::visit(ASTDeclareCopyNsp &n)
{
    lr_str.append("(declare-copy-namespaces ");

    if (n.pres_mod == ASTDeclareCopyNsp::PRESERVE)
        lr_str.append("(const (type !xs!string) \"preserve\")");
    else
        lr_str.append("(const (type !xs!string) \"no-preserve\")");

    if (n.inh_mod == ASTDeclareCopyNsp::INHERIT)
        lr_str.append("(const (type !xs!string) \"inherit\")");
    else
        lr_str.append("(const (type !xs!string) \"no-inherit\")");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTDefCollation &n)
{
    lr_str.append("(declare-default-collation ");
    LR_STR(*n.uri);
    lr_str.append(") ");
}

void LRVisitor::visit(ASTDefNamespaceDecl &n)
{
    if (n.type == ASTDefNamespaceDecl::ELEMENT)
        lr_str.append("(declare-default-element-namespace ");
    else
        lr_str.append("(declare-default-function-namespace ");

    LR_STR(*n.uri);
    lr_str.append(") ");
}

void LRVisitor::visit(ASTDocConst &n)
{
    lr_str.append("(document ");
    n.expr->accept(*this);
    lr_str.append(") ");
}

void LRVisitor::visit(ASTDocTest &n)
{
    if (!n.elem_test)
    {
        lr_str.append("(doc-test) ");
    }
    else
    {
        lr_str.append("(doc-test ");

        if (n.elem_test)
            n.elem_test->accept(*this);

        lr_str.append(")");
    }
}

void LRVisitor::visit(ASTDropColl &n)
{
    lr_str.append("(drop-collection ");
    n.coll->accept(*this);
    lr_str.append(") ");
}

void LRVisitor::visit(ASTDropDoc &n)
{
    lr_str.append("(drop-document ");
    n.doc->accept(*this);

    if (n.coll)
        n.coll->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTDropFtIndex &n)
{
    lr_str.append("(drop-fulltext-index ");
    n.index->accept(*this);
    lr_str.append(") ");
}

void LRVisitor::visit(ASTDropIndex &n)
{
    lr_str.append("(drop-index ");
    n.index->accept(*this);
    lr_str.append(") ");
}

void LRVisitor::visit(ASTDropMod &n)
{
    lr_str.append("(drop-module ");
    LR_STR(*n.module);
    lr_str.append(") ");
}

void LRVisitor::visit(ASTDropRole &n)
{
    lr_str.append("(drop-role ");
    LR_STR(*n.role);
    lr_str.append(") ");
}

void LRVisitor::visit(ASTDropTrg &n)
{
    lr_str.append("(drop-trigger ");
    LR_STR(*n.trg);
    lr_str.append(") ");
}

void LRVisitor::visit(ASTDropUser &n)
{
    lr_str.append("(drop-user ");
    LR_STR(*n.user);
    lr_str.append(") ");
}

void LRVisitor::visit(ASTElem &n)
{
    unsigned int count = 0;

    lr_str.append("(element ");
    LR_QNAME(n.pref, n.local);

    if (n.attrs) count += n.attrs->size();
    if (n.cont) count += n.cont->size();

    if (count > 1 || count == 0)
        lr_str.append("(sequence ");

    if (n.attrs)
        VisitNodesVector(n.attrs, *this);

    if (n.cont)
        VisitNodesVector(n.cont, *this);

    if (count > 1 || count == 0)
        lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTElemConst &n)
{
    lr_str.append("(element ");

    if (n.name == NULL)
        LR_QNAME(n.pref, n.local);
    else
        n.name->accept(*this);

    if (n.expr == NULL)
        lr_str.append("(sequence)");
    else
        n.expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTElementTest &n)
{
    lr_str.append("(elem-test ");

    if (n.name == NULL)
        lr_str.append("(ename (const (type !xs!QName) unspecified) ");
    else
        n.name->accept(*this);

    if (n.type == NULL)
        lr_str.append("(type unspecified) ");
    else
    {
        lr_str.append("(type ");
        n.type->accept(*this);
        lr_str.append(")");
    }

    if (n.mod == ASTElementTest::NON_NIL)
        lr_str.append("(const (type !xs!string) \"non-nil\"))");
    else
        lr_str.append("(const (type !xs!string) qmark))");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTEmptyTest &n)
{
    lr_str.append("(empty-test)");
}

void LRVisitor::visit(ASTError &n)
{
    throw SYSTEM_EXCEPTION("If you see this, you are very unlucky. Anyway, this is an internal parser error.");
}

void LRVisitor::visit(ASTExtExpr &n)
{
//     lr_str.append("(extension-expr (pragmas ");
//     VisitNodesVector(n.pragmas, *this);
//     lr_str.append(") ");

    if (n.expr)
        n.expr->accept(*this);

//     lr_str.append(") ");
}

void LRVisitor::visit(ASTFilterStep &n)
{
    std::string cont = " (var (\"\" \"$%v\")) ";
    std::string lr_save;
    bool need_checker = !n.isFirstStep() && (dynamic_cast<ASTAxisStep *>(n.cont) == NULL);
    ASTFilterStep *fs;

    if (need_checker && (fs = dynamic_cast<ASTFilterStep*>(n.cont)))
    {
        if (!fs->expr)
            need_checker = false;
    }

    if (n.isLast && !n.isFirstStep())
    {
        lr_str.append("(seq-checker ");
    }

    if (!n.expr)
    {
        if (n.preds)
        {
            lr_str.append("(return ");

            if (n.cont)
            {
                if (need_checker)
                {
                    lr_str.append("(seq-checker ");
                }
                n.cont->accept(*this);
                if (need_checker)
                {
                    lr_str.append(int2string(PPSeqChecker::CHECK_NODE));
                    lr_str.append(")");
                }
            }
            else
                lr_str += cont;

            lr_str.append("(fun-def ((!xs!anyType (var (\"\" \"$%v\")))) ");

            lr_save = lr_str;
            lr_str = cont;

            for (unsigned int i = 0; i < n.preds->size(); i++)
            {
                (*n.preds)[i]->accept(*this);
            }

            lr_str = lr_save + lr_str;

            lr_str += "))";
        }
        else
        {
            if (n.cont)
            {
                if (need_checker)
                {
                    lr_str.append("(seq-checker ");
                }
                n.cont->accept(*this);
                if (need_checker)
                {
                    lr_str.append(int2string(PPSeqChecker::CHECK_NODE));
                    lr_str.append(")");
                }
            }
            else
            {
                lr_str += cont;
            }
        }
    }
    else
    {
        if (n.cont)
        {
            if (n.expr->isCached())
            {
                lr_str.append("(lreturn ");
            }
            else
            {
                lr_str.append("(return ");
            }

            if (need_checker)
            {
                lr_str.append("(seq-checker ");
            }
            n.cont->accept(*this);
            if (need_checker)
            {
                lr_str.append(int2string(PPSeqChecker::CHECK_NODE));
                lr_str.append(")");
            }

            lr_str.append("(fun-def ((!xs!anyType (var (\"\" \"$%v\")))) ");
        }

        if (n.preds)
        {
            lr_save = lr_str;
            lr_str = "";
            n.expr->accept(*this);

            for (unsigned int i = 0; i < n.preds->size(); i++)
            {
                (*n.preds)[i]->accept(*this);
            }

            lr_str = lr_save + lr_str;
        }
        else
        {
            n.expr->accept(*this);
        }

        if (n.cont)
        {
            lr_str += "))";
        }
    }

    if (n.isLast && !n.isFirstStep())
    {
        lr_str.append(int2string(PPSeqChecker::CHECK_MIX));
        lr_str.append(")");
    }
}

void LRVisitor::visit(ASTFLWOR &n)
{
//    if (n.order_by)
//    {
//        lr_str += "(return (order-by ";
//
//        for (unsigned int i = 0; i < n.fls->size(); i++)
//        {
//            n.fls->at(i)->accept(*this);
//        }
//
//        lr_str += "(unio ";
//
//    }
}

void LRVisitor::visit(ASTFor &n)
{
//    lr_str.append("(return ");
//
//    n.expr->accept(*this);
//
//    lr_str.append("(fun-def (");
//    n.tv->accept(*this);
//
//    if (n.pv)
//        n.pv->accept(*this);
//    lr_str.append(")");
//
//    n.fd->accept(*this);
//    lr_str.append(")) ");
}

void LRVisitor::visit(ASTFunCall &n)
{
    if (n.int_name && *n.int_name != "")
    {
        lr_str.append("(");

        if (*n.int_name == "!fn!ftwindex-scan")
            lr_str.append("!fn!ftindex-scan2");
        else if (*n.int_name == "!fn!fthighlight-blocks")
            lr_str.append("!fn!fthighlight2");
        else
            lr_str.append(*n.int_name);
    }
    else if (n.uri)
    {
        lr_str.append("(fun-call ");
        LR_QNAME_URI(n.pref, n.local, n.uri);
    }
    else
    {
        lr_str.append("(fun-call ");
        LR_QNAME(n.pref, n.local);
    }

    VisitNodesVector(n.params, *this);

    lr_str.append(" ");
    lr_str.append(int2string(n.getFirstLine()));

    lr_str.append(") ");
}

void LRVisitor::visit(ASTFuncDecl &n)
{
    if (n.body == NULL)
        lr_str.append("(declare-external-function ");
    else
        lr_str.append("(declare-function ");

    if (!n.func_uri)
        LR_QNAME(n.pref, n.local);
    else
        LR_QNAME_URI(n.pref, n.local, n.func_uri);

    lr_str.append("(");
    VisitNodesVector(n.params, *this);
    lr_str.append(") ");

    lr_str.append("(result-type ");
    n.ret->accept(*this);
    lr_str.append(") ");

    if (n.body)
    {
        lr_str.append("(body ");
        n.body->accept(*this);
        lr_str.append(")");
    }

    lr_str.append(") ");
}

void LRVisitor::visit(ASTGrantPriv &n)
{
    if (n.mod == ASTGrantPriv::DOCUMENT)
        lr_str.append("(grant-priv-on-doc ");
    else if (n.mod == ASTGrantPriv::COLLECTION)
        lr_str.append("(grant-priv-on-col ");
    else
        lr_str.append("(grant-priv ");

    lr_str.append("(");
    LR_STR(*n.priv);
    lr_str.append(") ");

    if (n.obj)
        LR_STR(*n.obj);

    lr_str.append("(");
    LR_STR(*n.user);
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTGrantRole &n)
{
    lr_str.append("(grant-role ");

    lr_str.append("(");
    LR_STR(*n.role);
    lr_str.append(") ");

    lr_str.append("(");
    LR_STR(*n.role_to);
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTIf &n)
{
    lr_str.append("(if@ ");

    n.i_expr->accept(*this);
    n.t_expr->accept(*this);
    n.e_expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTInstOf &n)
{
    lr_str.append("(instance-of ");

    n.expr->accept(*this);

    lr_str.append("(type ");
    n.type->accept(*this);
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTItemTest &n)
{
    lr_str.append("(item-test) ");
}

void LRVisitor::visit(ASTLet &n)
{
//    lr_str.append("(let@ ");
//
//    n.expr->accept(*this);
//
//    lr_str.append("(fun-def (");
//    n.tv->accept(*this);
//    lr_str.append(")");
//    n.fd->accept(*this);
//
//    lr_str.append("))");
}

void LRVisitor::visit(ASTLibModule &n)
{
    is_libmodule = true;

    lr_str.append("(module ");

    n.moduleDecl->accept(*this);
    n.prolog->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTLit &n)
{
    if (n.type != ASTLit::STRING)
        lr_str.append("(const (type ");

    if (n.type == ASTLit::INTEGER)
    {
        lr_str.append("!xs!integer) ");
        LR_SINGLE(n.lit);
    }
    else if (n.type == ASTLit::DECIMAL)
    {
        lr_str.append("!xs!decimal) ");
        LR_SINGLE(n.lit);
    }
    else if (n.type == ASTLit::DOUBLE)
    {
        lr_str.append("!xs!double) ");
        LR_SINGLE(n.lit);
    }
    else
    {
        LR_STR(*n.lit); // special treatment for string because of scheme part
    }

    if (n.type != ASTLit::STRING)
        lr_str.append(") ");
}

void LRVisitor::visit(ASTLoadFile &n)
{
    std::string *file = n.getFileName();

    lr_str.append("(load ");

    LR_STR(*file);
    LR_STR(*n.doc);

    if (n.coll)
        LR_STR(*n.coll);

    lr_str.append(") ");

    delete file;
}

void LRVisitor::visit(ASTLoadModule &n)
{
    ASTStringVector::const_iterator it;

    if (n.mod == ASTLoadModule::LOAD)
        lr_str.append("(load-module ");
    else
        lr_str.append("(load-or-replace-module ");

//    lr_str.append("(const (type !xs!string) \"dummy-for-compatibility\")");

    for (it = n.modules->begin(); it != n.modules->end(); it++)
        LR_STR(**it);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTMainModule &n)
{
    lr_str = "(";
    lr_str.append(query_str[dynamic_cast<ASTQuery *>(n.query)->type]);
    //lr_str.append(drv->getLRForModules(mod));

    purge_imports = true;
    n.prolog->accept(*this);
    n.query->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTMetaCols &n)
{
    lr_str.append("(retrieve-metadata-collections ");

    if (n.need_stats)
        lr_str.append("(const (type !xs!boolean) true)");
    else
        lr_str.append("(const (type !xs!boolean) false)");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTMetaDocs &n)
{
    lr_str.append("(retrieve-metadata-documents ");

    if (n.coll)
        n.coll->accept(*this);

    if (n.need_stats)
        lr_str.append("(const (type !xs!boolean) true)");
    else
        lr_str.append("(const (type !xs!boolean) false)");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTMetaSchemaCol &n)
{
    lr_str.append("(retrieve-descr-scheme-collection ");

    n.coll->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTMetaSchemaDoc &n)
{
    lr_str.append("(retrieve-descr-scheme ");

    n.doc->accept(*this);

    if (n.coll)
        n.coll->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTModImport &n)
{
    if (purge_imports)
    {
        return;
    }

    if (is_libmodule && n.name)
    {
        lr_str.append("(declare-namespace ");

        lr_str.append(*n.name);
        LR_STR(*n.uri);

        lr_str.append(") ");
        return;
    }
    else if (is_libmodule) // in libary modules ignore "just-uri" declarations
    {
        return;
    }

    ASTStringVector::const_iterator it;

    lr_str.append("(import-module ");

    if (n.name)
        LR_NCNAME(n.name);

    LR_STR(*n.uri);

    if (n.hints)
        for (it = n.hints->begin(); it != n.hints->end(); it++)
            LR_STR(**it);

    lr_str.append(") ");
}


void LRVisitor::visit(ASTModuleDecl &n)
{
//    lr_str.append("(module-decl ");
    lr_str.append("(declare-namespace ");

    lr_str.append(*n.name);
    LR_STR(*n.uri);

//    LR_NCNAME(n.name);
//    LR_STR(*n.uri);

    lr_str.append(") ");
}


void LRVisitor::visit(ASTNameTest &n)
{
    lr_str.append("(ename ");

    lr_str.append("(const (type !xs!QName) ");

    if (*n.pref == "*" && *n.local == "*")
    {
        lr_str.append("*");
    }
    else
    {
        lr_str.append("(");

        if (n.uri && *n.uri == "*")
            lr_str.append("* ");
        else if (n.uri)
            LR_SINGLE(n.uri);

        if (*n.local == "*")
            lr_str.append("*");
        else
            LR_SINGLE(n.local);

        if (*n.pref == "*")
            lr_str.append("* ");
        else
            LR_SINGLE(n.pref);

        lr_str.append(")");
    }

    lr_str.append(") ");
}

void LRVisitor::visit(ASTNamespaceDecl &n)
{
    lr_str.append("(declare-namespace ");

    lr_str.append(*n.name);
    LR_STR(*n.uri);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTNodeTest &n)
{
    lr_str.append("(node-test) ");
}

void LRVisitor::visit(ASTNsp &n)
{
    lr_str.append("(namespace ");
    LR_NCNAME_QUOT(n.name);

    if (n.cont)
        LR_STR(*n.cont);
    else
        lr_str.append("(const (type !xs!string) \"\")");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTOption &n)
{
    lr_str.append("(declare-option ");

    if (!n.uri)
        LR_QNAME(n.pref, n.local);
    else
        LR_QNAME_URI(n.pref, n.local, n.uri);

    if (!n.options)
        LR_STR(*n.opt);
    else // options already parsed
    {
        for (unsigned int i = 0; i < n.options->size(); i++)
        {
            lr_str.append("(");
            LR_STR((*n.options)[i].first);
            LR_STR((*n.options)[i].second);
            lr_str.append(")");
        }
    }

    lr_str.append(") ");
}

void LRVisitor::visit(ASTOrdExpr &n)
{
    if (n.type == ASTOrdExpr::ORDERED)
        lr_str.append("(ordered ");
    else
        lr_str.append("(unordered ");

    n.expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTOrder &n)
{
    lr_str.append("(declare-order ");

    if (n.mod == ASTOrder::ORDERED)
        lr_str.append("(const (type !xs!string) \"ordered\")");
    else
        lr_str.append("(const (type !xs!string) \"unordered\")");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTOrderBy &n)
{
    lr_str.append("(orderspecs ");

    if (n.type == ASTOrderBy::STABLE)
        lr_str.append("(const (type !xs!string) \"stable\")");
    else
        lr_str.append("(const (type !xs!string) \"non-stable\")");

    VisitNodesVector(n.specs, *this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTOrderEmpty &n)
{
    lr_str.append("(declare-default-order ");

    if (n.mod == ASTOrderEmpty::EMPTY_GREATEST)
        lr_str.append("(const (type !xs!string) \"empty-greatest\")");
    else
        lr_str.append("(const (type !xs!string) \"empty-least\")");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTOrderMod &n)
{
    lr_str.append("(ordermodifier ");

    if (n.ad_mod)
        n.ad_mod->accept(*this);
    else
        lr_str.append("(const (type !xs!string) \"asc\")");

    if (n.em_mod)
        n.em_mod->accept(*this);
    else
        lr_str.append("(const (type !xs!string) \"default\")");

    if (n.col_mod)
        n.col_mod->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTOrderModInt &n)
{
    if (n.mod != ASTOrderModInt::COLLATION)
    {
        lr_str.append("(const (type !xs!string) \"");
        lr_str.append(ordmod_str[n.mod]);
        lr_str.append("\") ");
    }
    else
    {
        lr_str.append("(collation ");
        LR_STR(*n.uri);
        lr_str.append(")");
    }
}

void LRVisitor::visit(ASTOrderSpec &n)
{
    lr_str.append("(orderspec ");

    if (n.mod)
        n.mod->accept(*this);
    else
        lr_str.append("(ordermodifier)");

    n.expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTPIConst &n)
{
    lr_str.append("(pi ");

    if (n.name)
        n.name->accept(*this);
    else
        LR_NCNAME(n.ncname);

    if (n.expr)
        n.expr->accept(*this);
    else
        lr_str.append("(space-sequence)");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTPi &n)
{
    lr_str.append("(pi ");

    LR_NCNAME(n.name);

    if (*n.cont != "")
    {
        LR_STR(*n.cont);
    }

    lr_str.append(") ");
}

void LRVisitor::visit(ASTPiTest &n)
{
    lr_str.append("(pi-test ");

    if (n.type == ASTPiTest::NCNAME)
        LR_NCNAME_QUOT(n.test);
    else if (n.type == ASTPiTest::STRING)
        LR_STR(*n.test);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTPosVar &n)
{
    lr_str.append("(se:positional-var ");

    n.var->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTPragma &n)
{
    lr_str.append("(pragma ");

    LR_QNAME(n.pref, n.local);

    LR_STR(*n.cont);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTPred &n)
{
    unsigned int sz = n.others.size(), and_cnt = 0;
    lr_str = "(predicate " + lr_str + "(fun-def ((!xs!anyType (var (\"\" \"$%v\")))) ";

    if (sz > 1)
    {
        lr_str.append("(and@ ");
        and_cnt++;
    }

    for (unsigned int i = 0; i < sz; i++)
    {
        n.others[i].expr->accept(*this);

        if (sz - i - 1 > 1)
        {
            lr_str.append("(and@ ");
            and_cnt++;
        }
    }
    while (and_cnt--)
    {
        lr_str.append(")");
    }
    lr_str += " ))";
}

void LRVisitor::visit(ASTProlog &n)
{
    if (!is_libmodule)
        lr_str.append("(prolog  ");

    VisitNodesVector(n.decls, *this);

    if (!is_libmodule)
        lr_str.append(") ");
}

void LRVisitor::visit(ASTQName &n)
{
    LR_QNAME_URI(n.pref, n.local, n.uri);
}

void LRVisitor::visit(ASTQuantExpr &n)
{
    if (n.type == ASTQuantExpr::SOME)
        lr_str.append("(some ");
    else
        lr_str.append("(every ");

    n.expr->accept(*this);

    lr_str.append("(fun-def (");
    n.var->accept(*this);
    lr_str.append(")");

    n.sat->accept(*this);

    lr_str.append("))");
}

void LRVisitor::visit(ASTQuery &n)
{
    // query type is exploited in visit ASTMainModule (except usual query)
    if (n.type == ASTQuery::QUERY)
        lr_str.append("(query-body ");

    n.query->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTRenameColl &n)
{
    lr_str.append("(rename-collection ");

    n.name_old->accept(*this);
    n.name_new->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTRevokePriv &n)
{
    if (n.mod == ASTRevokePriv::DOCUMENT)
        lr_str.append("(revoke-priv-from-doc ");
    else if (n.mod == ASTRevokePriv::COLLECTION)
        lr_str.append("(revoke-priv-from-col ");
    else
        lr_str.append("(revoke-priv ");

    lr_str.append("(");
    LR_STR(*n.priv);
    lr_str.append(") ");

    if (n.obj)
        LR_STR(*n.obj);

    lr_str.append("(");
    LR_STR(*n.user);
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTRevokeRole &n)
{
    lr_str.append("(revoke-role ");

    lr_str.append("(");
    LR_STR(*n.role);
    lr_str.append(") ");

    lr_str.append("(");
    LR_STR(*n.role_from);
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTSchemaAttrTest &n)
{
    lr_str.append("(schema-attr-test ");
    n.name->accept(*this);
    lr_str.append(") ");
}

void LRVisitor::visit(ASTSchemaElemTest &n)
{
    lr_str.append("(schema-elem-test ");
    n.name->accept(*this);
    lr_str.append(") ");
}

void LRVisitor::visit(ASTSeq &n)
{
    lr_str.append("(sequence ");

    VisitNodesVector(n.exprs, *this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTSpaceSeq &n)
{
    lr_str.append("(space-sequence ");

    n.expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTTextConst &n)
{
    lr_str.append("(text ");

    n.expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTTextTest &n)
{
    lr_str.append("(text-test)");
}

void LRVisitor::visit(ASTTreat &n)
{
    lr_str.append("(treat ");

    n.expr->accept(*this);

    lr_str.append("(type ");
    n.type->accept(*this);
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTType &n)
{
    std::string *pref, *loc;

    ASTParseQName(n.name, &pref, &loc);
    //LR_PAIR(pref, loc);
    lr_str.append(std::string("!xs!") + *loc);
    delete pref;
    delete loc;
}

void LRVisitor::visit(ASTTypeSeq &n)
{
    if (dynamic_cast<ASTEmptyTest *>(n.type_test))
    {
        n.type_test->accept(*this);
    }
    else
    {
        lr_str.append("(");

        lr_str.append(occur_str[n.mod]);

        n.type_test->accept(*this);

        lr_str.append(") ");
    }
}

void LRVisitor::visit(ASTTypeSingle &n)
{
    lr_str.append("(");
    lr_str.append(occur_str[n.mod]);

    n.type->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTTypeSwitch &n)
{
    lr_str.append("(ts ");

    n.expr->accept(*this);

    lr_str.append("(cases ");
    VisitNodesVector(n.cases, *this);
    n.def_case->accept(*this);
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTTypeVar &n)
{
    lr_str.append("(");

    n.type->accept(*this);

    n.var->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTUop &n)
{
    if (n.op == ASTUop::MINUS)
        lr_str.append("(unary-@ ");
    else
        lr_str.append("(unary+@ ");

    n.expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTUpdDel &n)
{
    if (n.type == ASTUpdDel::DEEP)
        lr_str.append("(delete ");
    else
        lr_str.append("(delete_undeep ");

    n.what->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTUpdInsert &n)
{
    if (n.type == ASTUpdInsert::INTO)
        lr_str.append("(insert-into ");
    else if (n.type == ASTUpdInsert::FOLLOWING)
        lr_str.append("(insert-following ");
    else
        lr_str.append("(insert-preceding ");

    n.what->accept(*this);
    n.where->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTUpdMove &n)
{
    if (n.type == ASTUpdMove::INTO)
        lr_str.append("(move-into ");
    else if (n.type == ASTUpdMove::FOLLOWING)
        lr_str.append("(move-following ");
    else
        lr_str.append("(move-preceding ");

    n.what->accept(*this);

    lr_str.append("(fun-def (");
    n.var->accept(*this);
    lr_str.append(")");

    n.where->accept(*this);

    lr_str.append("))");
}

void LRVisitor::visit(ASTUpdRename &n)
{
    lr_str.append("(rename ");

    n.what->accept(*this);

    LR_PAIR(n.pref, n.local);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTUpdReplace &n)
{
    lr_str.append("(replace (return");

    n.what->accept(*this);

    lr_str.append("(fun-def (");
    n.var->accept(*this);
    lr_str.append(")");

    lr_str.append("(sequence ");
    dynamic_cast<ASTTypeVar *>(n.var)->var->accept(*this);
    n.new_expr->accept(*this);
    lr_str.append("(const (type !se!separator) 1)");

    lr_str.append("))))");
}

void LRVisitor::visit(ASTVar &n)
{
    lr_str.append("(var ");

    if (!n.uri)
        LR_PAIR(n.pref, n.local);
    else
        LR_PAIR(n.uri, n.local);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTVarDecl &n)
{
    if (n.expr == NULL)
        lr_str.append("(declare-external-var ");
    else
        lr_str.append("(declare-global-var ");

    n.var->accept(*this);

    if (n.expr)
        n.expr->accept(*this);

    if (n.type)
        n.type->accept(*this);
    else
        lr_str.append(" (zero-or-more (item-test))");

    lr_str.append(") ");
}

void LRVisitor::visit(ASTVersionDecl &n)
{
    return; // we don't want this in lropt

    lr_str.append("(version-declaration ");

    LR_STR(*n.xq_version);

    if (n.encoding)
        LR_STR(*n.encoding);

    lr_str.append(") ");
}

void LRVisitor::visit(ASTXMLComm &n)
{
    lr_str.append("(comment ");

    LR_STR(*n.cont);

    lr_str.append(") ");
}
