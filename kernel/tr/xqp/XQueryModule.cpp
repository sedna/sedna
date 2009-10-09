#include "XQueryModule.h"
#include "tr/xqp/sema/Sema.h"
#include "tr/xqp/lr/LRVisitor.h"
#include "tr/xqp/serial/serial.h"
#include "tr/xqp/serial/deser.h"
#include "tr/xqp/sema/cycle.h"

namespace sedna
{
    XQueryModule::XQueryModule(ASTNode *ast_tree, XQueryDriver *driver)
    {
        ast = ast_tree;
        drv = driver;

#if (defined(DEBUGI) && (DEBUGI == 1))
        if (drv->getErrorCode() == -1)
            testDupAndSerial(drv);
#endif

        initXQueryInfo();

        xpdy0002 = NULL;
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

    std::string XQueryModule::getIR()
    {
        IntVisitor iv(drv, this);

        ast->accept(iv);

        return iv.getResult();
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
        // notice that it cannot be redefenid since !xs is not a valid NCName
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
}
