#include "XQueryModule.h"
#include "tr/xqp/sema/Sema.h"
#include "tr/xqp/lr/LRVisitor.h"
#include "tr/xqp/serial/serial.h"
#include "tr/xqp/serial/deser.h"
#include "tr/xqp/sema/cycle.h"
#include "tr/xqp/lreturn/lreturn.h"
#include "tr/xqp/lr2por/lr2por.h"
#include "tr/opt/algebra/ASTOpt.h"

namespace sedna
{
    XQueryModule::XQueryModule(ASTNode *ast_tree, XQueryDriver *driver)
    {
        ast = ast_tree;
        qep = NULL;
        drv = driver;
        lr = NULL;
        dyn_cxt = NULL;
        is_used = false;

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
        dyn_cxt = NULL;
        is_used = false;

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

        for (XQVariablesInfo::iterator it = vars.begin(); it != vars.end(); it++)
            delete it->second;

        for (XQFunctionInfo::iterator it = funcs.begin(); it != funcs.end(); it++)
            delete it->second;

        for (XQFunctionInfo::iterator it = unres_funcs.begin(); it != unres_funcs.end(); it++)
            delete it->second;
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

    void XQueryModule::doPostSemanticAnalysis(XQVariablesInfo &libVars, bool check_cycles)
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
            {
                if (libVars.find(it->first) == libVars.end())
                {
                    drv->error(var->getLocation(), XPST0008, it->first);
                }
            }
        }
        unres_vars.clear(); // we've emitted all errors already

        // second, we should try to resolve all remaining functions
        for (itf = unres_funcs.begin(); itf != unres_funcs.end(); itf++)
        {
            name = CREATE_INTNAME(itf->second->uri, itf->second->local);

            // check prolog
            XQFunction *xqf = Sema::findFunction(name, itf->second->min_arg, this, drv);
            if (!xqf)
            {
                drv->error(*itf->second->loc, XPST0017, std::string("unknown function ") + name + "/" + int2string(itf->second->min_arg));
            }

            delete itf->second;
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

        //Note: VS2010 bug require (sedna::location*)NULL hack
        // must be by XQuery specs
        nsBinds["xml"] = nsPair("http://www.w3.org/XML/1998/namespace", (sedna::location*)NULL);
        nsBinds["xs"] = nsPair("http://www.w3.org/2001/XMLSchema", (sedna::location*)NULL);
        nsBinds["xsi"] = nsPair("http://www.w3.org/2001/XMLSchema-instance", (sedna::location*)NULL);
        nsBinds["fn"] = nsPair("http://www.w3.org/2005/xpath-functions", (sedna::location*)NULL);
        nsBinds["local"] = nsPair("http://www.w3.org/2005/xquery-local-functions", (sedna::location*)NULL);

        //Note: VS2010 bug require (sedna::location*)NULL hack
        // predefined Sedna namespace
        nsBinds["se"] = nsPair("http://www.modis.ispras.ru/sedna", (sedna::location*)NULL);

        //Note: VS2010 bug require (sedna::location*)NULL hack
        // default element/function namespaces
        defElemNsp = nsPair("", (sedna::location*)NULL);
        defFuncNsp = nsPair("http://www.w3.org/2005/xpath-functions", (sedna::location*)NULL);

        //Note: VS2010 bug require (sedna::location*)NULL hack
        // internal namespace to specify types as xs:anyType without worrying about original xs redeclaration
        // notice that it cannot be redefined since !xs is not a valid NCName
        nsBinds["!xs"] = nsPair("http://www.w3.org/2001/XMLSchema", (sedna::location*)NULL);

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

    bool XQueryModule::getFunctionInfo(const std::string &name, XQFunction **xqf)
    {
        XQFunctionInfo::const_iterator it = funcs.find(name);

        if (it == funcs.end())
            return false;

        if (xqf)
            *xqf = it->second;

        return true;
    }

    bool XQueryModule::getVariableInfo(const std::string &name, XQVariable **xqv)
    {
        XQVariablesInfo::const_iterator it = vars.find(name);

        if (it == vars.end())
            return false;

        if (xqv)
            *xqv = it->second;

        return true;
    }

    void XQueryModule::getLReturnFunctionInfo(const std::string &name, XQFunction **xqf)
    {
        U_ASSERT(funcs.find(name) != funcs.end());

        // create new lreturn visitor to process variables/functions
        if (!lr)
            lr = new LReturn(this->drv, this);

        *xqf = lr->getFunctionInfo(name);
    }

    void XQueryModule::getLReturnVariableInfo(const std::string &name, XQVariable **xqv)
    {
        U_ASSERT(vars.find(name) != vars.end());

        // create new lreturn visitor to process variables/functions
        if (!lr)
            lr = new LReturn(this->drv, this);

        *xqv = lr->getVariableInfo(name);
    }

    rqp::RPBase * XQueryModule::getOPT(bool is_subquery)
    {
        U_ASSERT(module_uri == NULL);

        lr2opt *l2p;

        // create module contexts
        dyn_cxt = new dynamic_context(new static_context());

        // this will resolve ids for all imported functions and vars
        drv->porLibModules(dyn_cxt);

        // this will resolve ids for our funcs and vars
        enumerate_vars_funcs();

        l2p = new lr2opt(this->drv, this, dyn_cxt, is_subquery);

        ast->accept(*l2p);

        rqp::RPBase * opt = l2p->getPlan();

        delete l2p;

        return opt;
    }
    
    PPQueryEssence *XQueryModule::getQEP(bool is_subquery)
    {
        U_ASSERT(module_uri == NULL);

        lr2por *l2p;

        // create module contexts
        dyn_cxt = new dynamic_context(new static_context());

        // this will resolve ids for all imported functions and vars
        drv->porLibModules(dyn_cxt);

        // this will resolve ids for our funcs and vars
        enumerate_vars_funcs();

        l2p = new lr2por(this->drv, this, dyn_cxt, is_subquery);

        ast->accept(*l2p);

        qep = l2p->getResult();

        delete l2p;

        return qep;
    }

    void XQueryModule::enumerate_vars_funcs()
    {
        for (XQVariablesInfo::iterator it = vars.begin(); it != vars.end(); it++)
        {
            if (it->second->is_used)
            {
                it->second->id.first = dyn_cxt;
                it->second->id.second = dyn_cxt->get_new_global_var_id();
            }
        }

        for (XQFunctionInfo::iterator it = funcs.begin(); it != funcs.end(); it++)
        {
            if (it->second->is_used && !it->second->is_external())
            {
                it->second->id.first = dyn_cxt;
                it->second->id.second = dyn_cxt->get_new_func_id();
            }
        }
    }

    void XQueryModule::porLibModule()
    {
        U_ASSERT(module_uri != NULL);

        lr2por *l2p;

        l2p = new lr2por(this->drv, this, dyn_cxt, false);
        ast->accept(*l2p);

        // we don't need any result from l2p for library modules since static/dynamic contexts will be populated with global vars/funcs
        delete l2p;
    }
}
