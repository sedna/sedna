#include "LRVisitor.h"
#include "common/errdbg/exceptions.h"

//#define LR_STR(s) (std::string("(const (type !xs!string) \"") + *s + "\") ")
#define LR_STR(s) (std::string("(const (type !xs!string) ") + *s + ") ")
#define LR_QNAME(p, l) (std::string("(const (type !xs!QName) (\"") + *p + "\" \"" + *l + "\" )) ")
//#define LR_NCNAME(n) (std::string("(const (type !xs!NCName) \"") + *n + "\")")
#define LR_NCNAME(n) (std::string("(const (type !xs!NCName) ") + *n + ")")
#define LR_SINGLE(p) (std::string("\"") + *p + "\"")
#define LR_PAIR(p1, p2) (std::string("(\"") + *p1 + "\" \"" + *p2 + "\" ) ")

static const char *axis_str[] = {
    "child ",
    "descendant ",
    "attr-axis ",
    "self ",
    "descendant-or-self ",
    "following-sibling ",
    "following ",
    "parent ",
    "ancestor ",
    "preceding-sibling ",
    "preceding ",
    "ancestor-or-self ",
    "context-item "
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
    "union@ ",
    "intersect@ ",
    "except@ ",

    "=@ ",
    "!=@ ",
    "<@ ",
    "<=@ ",
    ">@ ",
    ">=@ ",

    "eq@ ",
    "ne@ ",
    "lt@ ",
    "le@ ",
    "gt@ ",
    "ge@ ",

    "is@ ",
    "<<@ ",
    ">>@ "
};

static const char *trg_str[] =
{
    "\"BEFORE\" ",
    "\"AFTER\" ",

    "\"INSERT\" ",
    "\"DELETE\" ",
    "\"REPLACE\" ",

    "\"NODE\" ",
    "\"STATEMENT\" "
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
void LRVisitor::visit(const ASTAlterUser &n)
{
    lr_str.append("(alter-user ");
    lr_str.append(LR_STR(n.user));
    lr_str.append(LR_STR(n.psw));
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTAtomicTest &n)
{
    std::string *pref, *loc;

    if (n.name->substr(0, 4) == "!xs!") // standard type
        lr_str.append(*n.name);
    else
    {
        ASTParseQName(n.name, &pref, &loc);
        lr_str.append(LR_PAIR(pref, loc));
        delete pref;
        delete loc;
    }
}

void LRVisitor::visit(const ASTAttr &n)
{
    lr_str.append("(attribute  ");
    lr_str.append(LR_QNAME(n.pref, n.local));

    if (n.cont == NULL || n.cont->size() > 1)
        lr_str.append("(sequence ");

    VisitNodesVector(n.cont, *this);

    if (n.cont == NULL || n.cont->size() > 1)
        lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTAttrConst &n)
{
    lr_str.append("(attribute  ");

    if (n.name == NULL)
        lr_str.append(LR_QNAME(n.pref, n.local));
    else
        n.name->accept(*this);

    if (n.expr == NULL)
        lr_str.append("(sequence)");
    else
        n.expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTAttribTest &n)
{
    lr_str.append("(attr-test  ");

    if (n.npref == NULL)
        lr_str.append("(ename (const (type !xs!QName) unspecified) ");
    else if (*(n.nloc) == "*")
        lr_str.append("(ename (const (type !xs!QName) *) ");
    else
    {
        lr_str.append("(ename ");
        lr_str.append(LR_QNAME(n.npref, n.nloc));
    }

    if (n.tpref == NULL)
        lr_str.append("(type unspecified)");
    else
        lr_str.append("(type " + LR_PAIR(n.tpref, n.tloc) + ")");

    lr_str.append(")) ");
}

void LRVisitor::visit(const ASTAxis &n)
{
    lr_str.append("(");
    lr_str.append(axis_str[n.axis]);
    n.expr->accept(*this);

    lr_str.append("(type ");
    if (dynamic_cast<ASTNameTest *>(n.test))
    {
        if (n.axis == ASTAxisStep::ATTRIBUTE)
            lr_str.append("(attr-test ");
        else
            lr_str.append("(elem-test ");

        n.test->accept(*this);
        lr_str.append(")");
    }
    else
    {
        n.test->accept(*this);
    }
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTAxisStep &n)
{
    throw SYSTEM_EXCEPTION("If you see this, you are very unlucky. Anyway, this is an internal parser error.");
}

void LRVisitor::visit(const ASTBaseURI &n)
{
    lr_str.append("(declare-base-uri  ");
    lr_str.append(LR_STR(n.uri));
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTBop &n)
{
    lr_str.append("(");
    lr_str.append(bops_str[n.op]);

    n.lop->accept(*this);
    n.rop->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTBoundSpaceDecl &n)
{
    lr_str.append("(boundary-space-decl ");

    if (n.mod == ASTBoundSpaceDecl::STRIP)
        lr_str.append("(const (type !xs!string) \"strip\")");
    else
        lr_str.append("(const (type !xs!string) \"preserve\")");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTCase &n)
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

    n.fd->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTCast &n)
{
    lr_str.append("(cast ");

    n.expr->accept(*this);

    lr_str.append("(type  ");
    n.type ->accept(*this);
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTCastable &n)
{
    lr_str.append("(castable ");

    n.expr->accept(*this);

    lr_str.append("(type  ");
    n.type ->accept(*this);
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTCharCont &n)
{
    lr_str.append("(const (type !xs!string) ");
    lr_str.append(LR_SINGLE(n.cont));
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTCommTest &n)
{
    lr_str.append("(comment-test)");
}

void LRVisitor::visit(const ASTCommentConst &n)
{
    lr_str.append("(comment ");
    n.expr->accept(*this);
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTConstDecl &n)
{
    lr_str.append("(declare-construction ");

    if (n.mod == ASTConstDecl::STRIP)
        lr_str.append("(const (type !xs!string) \"strip\")");
    else
        lr_str.append("(const (type !xs!string) \"preserve\")");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTCreateColl &n)
{
    lr_str.append("(create-collection ");
    n.coll->accept(*this);
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTCreateDoc &n)
{
    lr_str.append("(create-document ");
    n.doc->accept(*this);

    if (n.coll)
        n.coll->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTCreateFtIndex &n)
{
    lr_str.append("(create-fulltext-index ");
    n.name->accept(*this);
    n.path->accept(*this);
    lr_str.append(LR_STR(n.type));

    if (n.cust_expr)
        n.cust_expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTCreateIndex &n)
{
    lr_str.append("(create-index ");

    n.name->accept(*this);
    n.on_path->accept(*this);
    n.by_path->accept(*this);
    n.type->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTCreateRole &n)
{
    lr_str.append("(create-role ");
    lr_str.append(LR_STR(n.role));
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTCreateTrg &n)
{
    std::string *s = new std::string();

    lr_str.append("(create-trigger ");

    lr_str.append(LR_STR(n.name));

    *s = trg_str[n.t_mod];
    lr_str.append(LR_STR(s));

    *s = trg_str[n.a_mod];
    lr_str.append(LR_STR(s));

    n.path->accept(*this);

    *s = trg_str[n.g_mod];
    lr_str.append(LR_STR(s));

    lr_str.append("( ");
    VisitNodesVector(n.do_exprs, *this);
    lr_str.append(") ");

//    n.do_expr->accept(*this);

    lr_str.append(") ");

    delete s;
}

void LRVisitor::visit(const ASTCreateUser &n)
{
    lr_str.append("(create-user ");
    lr_str.append(LR_STR(n.user));
    lr_str.append(LR_STR(n.psw));
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTDDO &n)
{
    lr_str.append("(ddo ");
    n.expr->accept(*this);
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTDeclareCopyNsp &n)
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

void LRVisitor::visit(const ASTDefCollation &n)
{
    lr_str.append("(declare-default-collation ");
    lr_str.append(LR_STR(n.uri));
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTDefNamespaceDecl &n)
{
    if (n.type == ASTDefNamespaceDecl::ELEMENT)
        lr_str.append("(declare-default-element-namespace ");
    else
        lr_str.append("(declare-default-function-namespace ");

    lr_str.append(LR_STR(n.uri));
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTDocConst &n)
{
    lr_str.append("(document ");
    n.expr->accept(*this);
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTDocTest &n)
{
    if (!n.elem_test)
    {
        lr_str.append("doc-test ");
    }
    else
    {
        lr_str.append("(doc-test ");

        if (n.elem_test)
            n.elem_test->accept(*this);

        lr_str.append(")");
    }
}

void LRVisitor::visit(const ASTDropColl &n)
{
    lr_str.append("(drop-collection ");
    n.coll->accept(*this);
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTDropDoc &n)
{
    lr_str.append("(drop-document ");
    n.doc->accept(*this);

    if (n.coll)
        n.coll->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTDropFtIndex &n)
{
    lr_str.append("(drop-fulltext-index ");
    n.index->accept(*this);
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTDropIndex &n)
{
    lr_str.append("(drop-index ");
    n.index->accept(*this);
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTDropMod &n)
{
    lr_str.append("(drop-module ");
    lr_str.append(LR_STR(n.module));
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTDropRole &n)
{
    lr_str.append("(drop-role ");
    lr_str.append(LR_STR(n.role));
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTDropTrg &n)
{
    lr_str.append("(drop-trigger ");
    lr_str.append(LR_STR(n.trg));
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTDropUser &n)
{
    lr_str.append("(drop-user ");
    lr_str.append(LR_STR(n.user));
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTElem &n)
{
    lr_str.append("(element ");
    lr_str.append(LR_QNAME(n.pref, n.local));

    if (n.attrs)
    {
        lr_str.append("(sequence ");
        VisitNodesVector(n.attrs, *this);
        VisitNodesVector(n.cont, *this);
        lr_str.append(")");
    }
    else if (!n.cont || n.cont->size() > 1)
    {
        lr_str.append("(sequence ");
        VisitNodesVector(n.cont, *this);
        lr_str.append(")");
    }
    else // cont && cont->size()==1
        VisitNodesVector(n.cont, *this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTElemConst &n)
{
    lr_str.append("(element ");

    if (n.name == NULL)
        lr_str.append(LR_QNAME(n.pref, n.local));
    else
        n.name->accept(*this);

    if (n.expr == NULL)
        lr_str.append("(sequence)");
    else
        n.expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTElementTest &n)
{
    lr_str.append("(elem-test ");

    if (n.npref == NULL)
        lr_str.append("(ename (const (type !xs!QName) unspecified) ");
    else if (*n.nloc == "*")
        lr_str.append("(ename (const (type !xs!QName) *) ");
    else
    {
        lr_str.append("(ename ");
        lr_str.append(LR_QNAME(n.npref, n.nloc));
    }

    if (n.tpref == NULL)
        lr_str.append("(type unspecified) ");
    else
        lr_str.append("(type " + LR_PAIR(n.tpref, n.tloc) + ") ");

    if (n.mod == ASTElementTest::NON_NIL)
        lr_str.append("(const (type !xs!string) \"non-nil\"))");
    else
        lr_str.append("(const (type !xs!string) qmark))");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTEmptyTest &n)
{
    lr_str.append("(empty-test)");
}

void LRVisitor::visit(const ASTError &n)
{
    throw SYSTEM_EXCEPTION("If you see this, you are very unlucky. Anyway, this is an internal parser error.");
}

void LRVisitor::visit(const ASTExtExpr &n)
{
    lr_str.append("(extension-expr (pragmas ");
    VisitNodesVector(n.pragmas, *this);
    lr_str.append(") ");

    if (n.expr)
        n.expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTFilterStep &n)
{
    throw SYSTEM_EXCEPTION("If you see this, you are very unlucky. Anyway, this is an internal parser error.");
}

void LRVisitor::visit(const ASTFor &n)
{
    lr_str.append("(return ");
    n.expr->accept(*this);
    n.fd->accept(*this);
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTFunCall &n)
{
    lr_str.append("(fun-call ");
    lr_str.append(LR_QNAME(n.pref, n.local));

    VisitNodesVector(n.params, *this);

    lr_str.append(int2string(n.getFirstLine()));
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTFunDef &n)
{
    lr_str.append("(fun-def (");
    VisitNodesVector(n.vars, *this);
    lr_str.append(") ");

    n.fun->accept(*this);
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTFuncDecl &n)
{
    if (n.body == NULL)
        lr_str.append("(declare-external-function ");
    else
        lr_str.append("(declare-function ");

    lr_str.append(LR_QNAME(n.pref, n.local));

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

void LRVisitor::visit(const ASTGrantPriv &n)
{
    if (n.mod == ASTGrantPriv::DOCUMENT)
        lr_str.append("(grant-priv-on-doc ");
    else if (n.mod == ASTGrantPriv::COLLECTION)
        lr_str.append("(grant-priv-on-col ");
    else
        lr_str.append("(grant-priv ");

    lr_str.append("(");
    lr_str.append(LR_STR(n.priv));
    lr_str.append(") ");

    if (n.obj)
        lr_str.append(LR_STR(n.obj));

    lr_str.append("(");
    lr_str.append(LR_STR(n.user));
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTGrantRole &n)
{
    lr_str.append("(grant-role ");

    lr_str.append("(");
    lr_str.append(LR_STR(n.role));
    lr_str.append(") ");

    lr_str.append("(");
    lr_str.append(LR_STR(n.role_to));
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTIf &n)
{
    lr_str.append("(if@ ");

    n.i_expr->accept(*this);
    n.t_expr->accept(*this);
    n.e_expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTInstOf &n)
{
    lr_str.append("(instance-of ");

    n.expr->accept(*this);

    lr_str.append("(type ");
    n.type->accept(*this);
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTItemTest &n)
{
    lr_str.append("(item-test) ");
}

void LRVisitor::visit(const ASTLet &n)
{
    lr_str.append("(let@ ");

    n.expr->accept(*this);
    n.fd->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTLibModule &n)
{
    lr_str.append("(lib-module ");

    n.moduleDecl->accept(*this);
    n.prolog->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTLit &n)
{
    lr_str.append("(const (type ");

    if (n.type == ASTLit::INTEGER)
    {
        lr_str.append("!xs!integer) ");
        lr_str.append(LR_SINGLE(n.lit));
    }
    else if (n.type == ASTLit::DECIMAL)
    {
        lr_str.append("!xs!decimal) ");
        lr_str.append(LR_SINGLE(n.lit));
    }
    else if (n.type == ASTLit::DOUBLE)
    {
        lr_str.append("!xs!double) ");
        lr_str.append(LR_SINGLE(n.lit));
    }
    else
    {
        lr_str.append("!xs!string) ");
        lr_str.append(*n.lit);
    }

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTLoadFile &n)
{
    std::string *file = n.getFileName();

    lr_str.append("(load ");

    lr_str.append(LR_STR(file));
    lr_str.append(LR_STR(n.doc));

    if (n.coll)
        lr_str.append(LR_STR(n.coll));

    lr_str.append(") ");

    delete file;
}

void LRVisitor::visit(const ASTLoadModule &n)
{
    ASTStringVector::const_iterator it;

    if (n.mod == ASTLoadModule::LOAD)
        lr_str.append("(load-module ");
    else
        lr_str.append("(load-or-replace-module ");

    lr_str.append("(const (type !xs!string) \"dummy-for-compatibility\")");

    for (it = n.modules->begin(); it != n.modules->end(); it++)
        lr_str.append(LR_STR(*it));

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTMainModule &n)
{
    lr_str.append("( ");
    lr_str.append(query_str[n.query->type]);

    n.prolog->accept(*this);
    n.query->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTMetaCols &n)
{
    lr_str.append("(retrieve-metadata-collections ");

    if (n.need_stats)
        lr_str.append("(const (type !xs!boolean) true)");
    else
        lr_str.append("(const (type !xs!boolean) false)");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTMetaDocs &n)
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

void LRVisitor::visit(const ASTMetaSchemaCol &n)
{
    lr_str.append("(retrieve-descr-scheme-collection ");

    n.coll->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTMetaSchemaDoc &n)
{
    lr_str.append("(retrieve-descr-scheme ");

    n.doc->accept(*this);

    if (n.coll)
        n.coll->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTModImport &n)
{
    ASTStringVector::const_iterator it;

    lr_str.append("(import-module ");

    if (n.name)
        lr_str.append(LR_NCNAME(n.name));

    lr_str.append(LR_STR(n.uri));

    if (n.hints)
        for (it = n.hints->begin(); it != n.hints->end(); it++)
            lr_str.append(LR_STR(*it));

    lr_str.append(") ");
}


void LRVisitor::visit(const ASTModuleDecl &n)
{
    lr_str.append("(module-decl ");

    lr_str.append(LR_NCNAME(n.name));
    lr_str.append(LR_STR(n.uri));

    lr_str.append(") ");
}


void LRVisitor::visit(const ASTNameTest &n)
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

        if (*n.pref == "*")
            lr_str.append("* ");
        else
            lr_str.append(LR_SINGLE(n.pref));

        if (*n.local == "*")
            lr_str.append("*");
        else
            lr_str.append(LR_SINGLE(n.local));

        lr_str.append(")");
    }

    lr_str.append(") ");

    lr_str.append("(type *) ");

    lr_str.append("(const (type !xs!string) \"non-nil\"))");
}

void LRVisitor::visit(const ASTNamespaceDecl &n)
{
    lr_str.append("(declare-namespace ");

    lr_str.append(*n.name);
    lr_str.append(LR_STR(n.uri));

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTNodeTest &n)
{
    lr_str.append("(node-test) ");
}

void LRVisitor::visit(const ASTNsp &n)
{
    lr_str.append("(namespace ");
    lr_str.append(LR_NCNAME(n.name));

    if (n.cont == NULL || n.cont->size() > 1)
        lr_str.append("(sequence ");

    VisitNodesVector(n.cont, *this);

    if (n.cont == NULL || n.cont->size() > 1)
        lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTOption &n)
{
    lr_str.append("(declare-option ");

    lr_str.append(LR_QNAME(n.pref, n.local));
    lr_str.append(LR_STR(n.opt));

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTOrdExpr &n)
{
    if (n.type == ASTOrdExpr::ORDERED)
        lr_str.append("(ordered ");
    else
        lr_str.append("(unordered ");

    n.expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTOrder &n)
{
    lr_str.append("(declare-order ");

    if (n.mod == ASTOrder::ORDERED)
        lr_str.append("(const (type !xs!string) \"ordered\")");
    else
        lr_str.append("(const (type !xs!string) \"unordered\")");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTOrderBy &n)
{
    lr_str.append("(orderspecs ");

    if (n.type == ASTOrderBy::STABLE)
        lr_str.append("(const (type !xs!string) \"stable\")");
    else
        lr_str.append("(const (type !xs!string) \"non-stable\")");

    VisitNodesVector(n.specs, *this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTOrderByRet &n)
{
    lr_str.append("(order-by ");

    n.iter_expr->accept(*this);
    n.ret_expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTOrderEmpty &n)
{
    lr_str.append("(declare-default-order ");

    if (n.mod == ASTOrderEmpty::EMPTY_GREATEST)
        lr_str.append("(const (type !xs!string) \"empty-greatest\")");
    else
        lr_str.append("(const (type !xs!string) \"empty-least\")");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTOrderMod &n)
{
    lr_str.append("(ordermodifier ");

    if (n.ad_mod)
        n.ad_mod->accept(*this);

    if (n.em_mod)
        n.em_mod->accept(*this);

    if (n.col_mod)
        n.col_mod->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTOrderModInt &n)
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
        lr_str.append(LR_STR(n.uri));
        lr_str.append(")");
    }
}

void LRVisitor::visit(const ASTOrderSpec &n)
{
    lr_str.append("(orderspec ");

    if (n.mod)
        n.mod->accept(*this);
    else
        lr_str.append("(ordermodifier)");

    n.expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTPIConst &n)
{
    lr_str.append("(pi ");

    if (n.name)
        n.name->accept(*this);
    else
        lr_str.append(LR_NCNAME(n.ncname));

    if (n.expr)
        n.expr->accept(*this);
    else
        lr_str.append("(space-sequence)");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTPi &n)
{
    lr_str.append("(pi ");

    lr_str.append(LR_NCNAME(n.name));

    if (*n.cont != "")
    {
        lr_str.append("(const (type !xs!string) ");
        lr_str.append(LR_SINGLE(n.cont));
        lr_str.append(")");
    }

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTPiTest &n)
{
    lr_str.append("(pi-test ");

    if (n.type == ASTPiTest::NCNAME)
        lr_str.append(LR_NCNAME(n.test));
    else if (n.type == ASTPiTest::STRING)
        lr_str.append(LR_STR(n.test));

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTPosVar &n)
{
    lr_str.append("(se:positional-var ");

    n.var->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTPragma &n)
{
    lr_str.append("(pragma ");

    lr_str.append(LR_QNAME(n.pref, n.local));

    lr_str.append("(const (type !xs!string) ");
    lr_str.append(LR_SINGLE(n.cont));
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTPred &n)
{
    lr_str.append("(predicate ");

    n.iter_expr->accept(*this);
    n.pred_expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTProlog &n)
{
    lr_str.append("(prolog  ");

    VisitNodesVector(n.decls, *this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTQuantExpr &n)
{
    if (n.type == ASTQuantExpr::SOME)
        lr_str.append("(some ");
    else
        lr_str.append("(every ");

    n.expr->accept(*this);
    n.fd->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTQuery &n)
{
    // query type is exploited in visit ASTMainModule (except usual query)
    if (n.type == ASTQuery::QUERY)
        lr_str.append("(query-body ");

    n.query->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTRenameColl &n)
{
    lr_str.append("(rename-collection ");

    n.name_old->accept(*this);
    n.name_new->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTRet &n)
{
    lr_str.append("(return ");

    n.iter_expr->accept(*this);
    n.ret_expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTRevokePriv &n)
{
    if (n.mod == ASTRevokePriv::DOCUMENT)
        lr_str.append("(revoke-priv-from-doc ");
    else if (n.mod == ASTRevokePriv::COLLECTION)
        lr_str.append("(revoke-priv-from-col ");
    else
        lr_str.append("(revoke-priv ");

    lr_str.append("(");
    lr_str.append(LR_STR(n.priv));
    lr_str.append(") ");

    if (n.obj)
        lr_str.append(LR_STR(n.obj));

    lr_str.append("(");
    lr_str.append(LR_STR(n.user));
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTRevokeRole &n)
{
    lr_str.append("(revoke-role ");

    lr_str.append("(");
    lr_str.append(LR_STR(n.role));
    lr_str.append(") ");

    lr_str.append("(");
    lr_str.append(LR_STR(n.role_from));
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTSchemaAttrTest &n)
{
    throw SYSTEM_EXCEPTION("If you see this, you are very unlucky. Anyway, this is an internal parser error.");
}

void LRVisitor::visit(const ASTSchemaElemTest &n)
{
    throw SYSTEM_EXCEPTION("If you see this, you are very unlucky. Anyway, this is an internal parser error.");
}

void LRVisitor::visit(const ASTScript &n)
{
    // we shouldn't visit this since we want to obtain different strings for different modules
    // to visit script use getScriptLRs() instead
    lr_str.append("(script ");

    VisitNodesVector(n.modules, *this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTSeq &n)
{
    if (n.exprs->size() > 1 || n.exprs->size() == 0)
    {
        lr_str.append("(sequence ");
    }

    VisitNodesVector(n.exprs, *this);

    if (n.exprs->size() > 1 || n.exprs->size() == 0)
    {
        lr_str.append(") ");
    }
}

void LRVisitor::visit(const ASTSpaceSeq &n)
{
    lr_str.append("(space-sequence ");

    n.expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTTextConst &n)
{
    lr_str.append("(text ");

    n.expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTTextTest &n)
{
    lr_str.append("(text-test)");
}

void LRVisitor::visit(const ASTTreat &n)
{
    lr_str.append("(treat ");

    n.expr->accept(*this);

    lr_str.append("(type ");
    n.type->accept(*this);
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTTypeSeq &n)
{
    if (dynamic_cast<ASTEmptyTest *>(n.type_test))
    {
        n.type_test->accept(*this);
    }
    else
    {
        lr_str.append("(");

        if (!dynamic_cast<ASTEmptyTest *>(n.type_test))
            lr_str.append(occur_str[n.mod]);

        if (n.type_name)
            lr_str.append(*n.type_name);
        else
            n.type_test->accept(*this);

        lr_str.append(") ");
    }
}

void LRVisitor::visit(const ASTTypeSingle &n)
{
    lr_str.append("(");
    lr_str.append(occur_str[n.mod]);

    n.type->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTTypeSwitch &n)
{
    lr_str.append("(ts ");

    n.expr->accept(*this);

    lr_str.append("(cases ");
    VisitNodesVector(n.cases, *this);
    n.def_case->accept(*this);
    lr_str.append(")");

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTTypeVar &n)
{
    lr_str.append("(");

    if (n.type_name)
        lr_str.append(*n.type_name);
    else
        n.type_seq->accept(*this);

    n.var->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTUnio &n)
{
    lr_str.append("(unio ");

    VisitNodesVector(n.vars, *this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTUop &n)
{
    if (n.op == ASTUop::MINUS)
        lr_str.append("(unary-@ ");
    else
        lr_str.append("(unary+@ ");

    n.expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTUpdDel &n)
{
    if (n.type == ASTUpdDel::DEEP)
        lr_str.append("(delete ");
    else
        lr_str.append("(delete_undeep ");

    n.what->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTUpdInsert &n)
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

void LRVisitor::visit(const ASTUpdMove &n)
{
    if (n.type == ASTUpdMove::INTO)
        lr_str.append("(move-into ");
    else if (n.type == ASTUpdMove::FOLLOWING)
        lr_str.append("(move-following ");
    else
        lr_str.append("(move-preceding ");

    n.what->accept(*this);
    n.where->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTUpdRename &n)
{
    lr_str.append("(rename ");

    n.what->accept(*this);

    lr_str.append(LR_PAIR(n.pref, n.local));

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTUpdReplace &n)
{
    lr_str.append("(replace ");

    n.what->accept(*this);
    n.new_expr->accept(*this);

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTVar &n)
{
    lr_str.append("(var ");
    lr_str.append(LR_PAIR(n.pref, n.local));
    lr_str.append(") ");
}

void LRVisitor::visit(const ASTVarDecl &n)
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

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTVersionDecl &n)
{
    lr_str.append("(version-declaration ");

    lr_str.append(LR_STR(n.xq_version));

    if (n.encoding)
        lr_str.append(LR_STR(n.encoding));

    lr_str.append(") ");
}

void LRVisitor::visit(const ASTXMLComm &n)
{
    lr_str.append("(comment ");

    lr_str.append("(const (type !xs!string) ");
    lr_str.append(LR_SINGLE(n.cont));
    lr_str.append(")");

    lr_str.append(") ");
}

