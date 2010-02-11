#include "XQueryModule.h"
#include "tr/xqp/sema/Sema.h"
#include "tr/xqp/lr/LRVisitor.h"
#include "tr/xqp/serial/serial.h"
#include "tr/xqp/serial/deser.h"
#include "tr/xqp/sema/cycle.h"
#include "tr/xqp/lreturn/lreturn.h"
#include "tr/xqp/lr2por/lr2por.h"

namespace sedna
{
    XQueryModule::XQueryModule(ASTNode *ast_tree, XQueryDriver *driver)
    {
        ast = ast_tree;
        qep = NULL;
        drv = driver;
        lr = NULL;
        qep_sx = NULL;

        isExplainOn = false;
        isProfileOn = false;

#if (defined(DEBUGI) && (DEBUGI == 1))
        if (drv->getErrorCode() == -1)
            testDupAndSerial(drv);
#endif

        initXQueryInfo();

        xpdy0002 = NULL;
    }

    XQueryModule::XQueryModule(const char *ast_str, XQueryDriver *driver)
    {
        ast = dsGetASTFromString(ast_str);
        qep = NULL;
        drv = driver;
        lr = NULL;
        qep_sx = NULL;

        isExplainOn = false;
        isProfileOn = false;

#if (defined(DEBUGI) && (DEBUGI == 1))
        if (drv->getErrorCode() == -1)
            testDupAndSerial(drv);
#endif

        initXQueryInfo();

        xpdy0002 = NULL;
    }

    XQueryModule::~XQueryModule()
    {
        delete ast;
        delete module_uri;
        delete lr;
    }

    void XQueryModule::doLReturnAnalysis()
    {
        LReturn lr(drv, this);

        ast->accept(lr);

#if (defined(DEBUGI) && (DEBUGI == 1))
        testDupAndSerial(drv);
#endif
    }

    void XQueryModule::doSemanticAnalysis()
    {
        Sema sema(drv, this);

        ast->accept(sema);

#if (defined(DEBUGI) && (DEBUGI == 1))
        if (drv->getErrorCode() == -1)
            testDupAndSerial(drv);
#endif
    }

    void XQueryModule::doPostSemanticAnalysis(const XQVariablesInfo &libVars, bool check_cycles)
    {
        // first, we should check all unresolved vars again
        XQStringHash::const_iterator it;
        XQFunctionInfo::const_iterator itf;
        std::string name;

        ASTVar *var;

        for (it = unres_vars.begin(); it != unres_vars.end(); it++)
        {
            U_ASSERT(dynamic_cast<ASTVar *>(it->second) != NULL);

            var = static_cast<ASTVar *>(it->second);

            // we should check only library variables since it is the only case variable reference can be unresolved
            if (imported.find(*var->uri) != imported.end())
                if (libVars.find(it->first) == libVars.end())
                    drv->error(var->getLocation(), XPST0008, it->first);
        }
        unres_vars.clear(); // we've emitted all errors already

        // second, we should try to resolve all remaining functions
        for (itf = unres_funcs.begin(); itf != unres_funcs.end(); itf++)
        {
            name = CREATE_INTNAME(itf->second.uri, itf->second.local);

            // check prolog
            if (!Sema::findFunction(name, itf->second.min_arg, this, drv))
                drv->error(*itf->second.loc, XPST0017, std::string("unknown function ") + name + "/" + int2string(itf->second.min_arg));
        }
        unres_funcs.clear(); // we've emitted all errors already

        // the we should check for cycles between variables
        if (check_cycles)
        {
            Cycle ca(drv, this);
            ast->accept(ca);
        }

        // if we've encountered xpdy error then emit it after all static analysis has been done
        if (xpdy0002)
            drv->error(*xpdy0002, XPDY0002, NULL);
    }

    std::string XQueryModule::getLR()
    {
        LRVisitor lr(drv, this);

        ast->accept(lr);

        return lr.getResult();
    }

    std::string XQueryModule::getIR(ASTNode *ast)
    {
        IntVisitor iv(drv, this);

        ast->accept(iv);

        return iv.getResult();
    }

    std::string XQueryModule::getIR()
    {
        return getIR(ast);
    }

    std::string XQueryModule::getModuleURI()
    {
        if (!module_uri)
            return "";

        return *module_uri;
    }

    void XQueryModule::testDupAndSerial(XQueryDriver *drv)
    {
        ASTNode *dupt;
        std::string dups, origs, s;
        IntVisitor dv(drv, this);
        LRVisitor lr1(drv, this), lr2(drv, this);

        // check dup-function
        dupt = ast->dup();
        dupt->accept(lr1);
        dups = lr1.getResult();

        ast->accept(lr2);
        origs = lr2.getResult();

        U_ASSERT(dups == origs);
        delete dupt;

        ast->accept(dv);
        s = dv.getResult();
        dupt = dsGetASTFromString(s.c_str());

        lr1.resetVisitor();
        dupt->accept(lr1);
        dups = lr1.getResult();

        U_ASSERT(dups == origs);
        delete dupt;
    }

    void XQueryModule::initXQueryInfo()
    {
        module_uri = NULL;
        isDefaultOrdered = true;

        // must be by XQuery specs
        nsBinds["xml"] = nsPair("http://www.w3.org/XML/1998/namespace", NULL);
        nsBinds["xs"] = nsPair("http://www.w3.org/2001/XMLSchema", NULL);
        nsBinds["xsi"] = nsPair("http://www.w3.org/2001/XMLSchema-instance", NULL);
        nsBinds["fn"] = nsPair("http://www.w3.org/2005/xpath-functions", NULL);
        nsBinds["local"] = nsPair("http://www.w3.org/2005/xquery-local-functions", NULL);

        // predefined Sedna namespace
        nsBinds["se"] = nsPair("http://www.modis.ispras.ru/sedna", NULL);

        // default element/function namespaces
        defElemNsp = nsPair("", NULL);
        defFuncNsp = nsPair("http://www.w3.org/2005/xpath-functions", NULL);

        // internal namespace to specify types as xs:anyType without worrying about original xs redeclaration
        // notice that it cannot be redefined since !xs is not a valid NCName
        nsBinds["!xs"] = nsPair("http://www.w3.org/2001/XMLSchema", NULL);

    }

    std::vector<std::string> XQueryModule::getImportedModules() const
    {
        std::vector<std::string> res;

        for (XQStringHash::const_iterator it = imported.begin(); it != imported.end(); it++)
            res.push_back(it->first);

        return res;
    }

    ASTNode* XQueryModule::getTree()
    {
        return ast;
    }

    bool XQueryModule::getFunctionInfo(const std::string &name, XQFunction &xqf) const
    {
        XQFunctionInfo::const_iterator it = funcs.find(name);

        if (it == funcs.end())
            return false;

        xqf = it->second;
        return true;
    }

    ASTVarDecl *XQueryModule::getVariableInfo(const std::string &name) const
    {
        XQVariablesInfo::const_iterator it = vars.find(name);

        if (it == vars.end())
            return NULL;

        return it->second;
    }

    bool XQueryModule::getLReturnFunctionInfo(const std::string &name, XQFunction &xqf)
    {
        XQFunctionInfo::const_iterator it = funcs.find(name);

        if (it == funcs.end())
            return false;

        // create new lreturn visitor to process variables/functions
        if (!lr)
            lr = new LReturn(this->drv, this);

        xqf = lr->getFunctionInfo(name);

        return true;
    }

    bool XQueryModule::getLReturnVariableInfo(const std::string &name, XQVariable &xqv)
    {
        XQVariablesInfo::const_iterator it = vars.find(name);

        if (it == vars.end())
            return false;

        // create new lreturn visitor to process variables/functions
        if (!lr)
            lr = new LReturn(this->drv, this);

        xqv = lr->getVariableInfo(name);

        return true;
    }

    PPQueryEssence *XQueryModule::getQEP()
    {
        size_t mod_num, libvar_num, libfun_num;
        size_t var_num, fun_num;
        static_context *st_cxt;
        lr2por *l2p;

        if (imported.size())
            mod_num = drv->getLibModCount();
        else
            mod_num = 0;

        libvar_num = drv->getVarCount();
        libfun_num = drv->getFuncCount();
        var_num = vars.size();
        fun_num = funcs.size();

        // enumerate local vars and funcs
        XQVariablesInfo::iterator vit;

        for (vit = vars.begin(); vit != vars.end(); vit++)
            vit->second->setId(drv->getNewGlobVarId());

        XQFunctionInfo::iterator fit;

        for (fit = funcs.begin(); fit != funcs.end(); fit++)
        {
            if (!fit->second.decl->body) // external function
            {
                fit->second.decl->setId(-1);
            }
            else
            {
                fit->second.decl->setId(drv->getNewGlobFunId());
            }
        }

        if (!qep_sx) // we haven't told about some specific static context
            dynamic_context::static_set(libfun_num + fun_num, libvar_num + var_num, mod_num + 1);

        if (mod_num)
            drv->porLibModules();

        // create our own context
        if (!qep_sx) // we haven't told about some specific static context
            st_cxt = dynamic_context::create_static_context();
        else
            st_cxt = qep_sx;

        l2p = new lr2por(this->drv, this, st_cxt);

        ast->accept(*l2p);

        delete ast;
        ast = NULL;

        qep = l2p->getResult();

        delete l2p;

        return qep;
    }

    void XQueryModule::porLibModule()
    {
        U_ASSERT(module_uri != NULL);

        lr2por *l2p;
        static_context *st_cxt;

        st_cxt = dynamic_context::create_static_context();
        l2p = new lr2por(this->drv, this, st_cxt);

        ast->accept(*l2p);

        // we don't need any result from l2p for library modules since static_context will be populated with global vars/funcs
        delete l2p;
    }

    void XQueryModule::addToUnresolvedPor(const std::string &name, PPGlobalVariable *var)
    {
        unresPorVars.insert(unresPorVarInfo(name, var));
    }

    void XQueryModule::setStaticContextForQEP(static_context *sx)
    {
        qep_sx = sx;
    }
}
