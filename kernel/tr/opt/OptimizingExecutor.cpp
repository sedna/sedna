#include "OptimizingExecutor.h"

#include "tr/opt/OptTypes.h"
#include "tr/opt/graphs/GraphCompiler.h"
#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/opt/algebra/PlanAlgorithms.h"
#include "tr/opt/cost/Statistics.h"
#include "tr/xqp/XQuerytoLR.h"

#include "tr/opt/evaluation/DynamicContext.h"
#include "tr/crmutils/serialization.h"

using namespace opt;
using namespace rqp;

OptimizingExecutor optimizerInstance;
OptimizingExecutor * optimizer = &optimizerInstance;

void opt::OptimizingExecutor::onSessionBegin()
{

}

void opt::OptimizingExecutor::onSessionEnd()
{

}

void opt::OptimizingExecutor::onTransactionBegin()
{
    _gcmpler = new GraphCompiler();
    _dgm = new VariableUsageGraph();
    _planContext = new PlanContext();
    _costModel = new CostModel();
}

void opt::OptimizingExecutor::onTransactionEnd()
{
    delete _costModel;
    delete _planContext;
    delete _dgm;
    delete _gcmpler;

    elog(EL_LOG, ("Optimizer used : %llu / %llu", planGenerationPool.totalAllocated(), planGenerationPool.total()));
    elog(EL_LOG, ("Cost model used : %llu / %llu", costModelPool.totalAllocated(), costModelPool.total()));

//    ptrs.destroyAll<opt::IPlanDisposable>(NULL);
//    ptrs.clear();

    planGenerationPool.clear();
    costModelPool.clear();
}

void OptimizedStatement::execute()
{
    xs_decimal_t::init();
    plan = driver->getRQPForModule(driver->getModulesCount() - 1, false);
    executionContext = new executor::DynamicContext();
    plan->evaluateTo(executionContext);
}

void OptimizedStatement::next()
{
    tuple valueTuple(1);
    tuple_cell & value = valueTuple.cells[0];
    t_item node_type = (t_item) 0;

    value = executionContext->stack->next();

    if (value.is_eos()) {
        client->end_item(se_no_next_item);
    };

    if (value.is_node()) {
        node_type = Node(value.get_node()).getNodeType();
    };

    client->begin_item(value.is_atomic(), value.get_atomic_type(), node_type, NULL);
    serialier->serialize(valueTuple);
    client->end_item(se_next_item_exists);
}

void OptimizedStatement::prepare(QueryType queryType, const char* query_str)
{
    if (driver != NULL) {
        delete driver;
        driver = NULL;
    };

    driver = new sedna::XQueryDriver();
    parse_batch(driver, queryType, query_str, NULL);
    serialier->prepare(client->get_se_ostream(), serializationOptions);
}

OptimizedStatement::OptimizedStatement(client_core* _client)
    : client(_client), driver(NULL), plan(NULL), executionContext(NULL),
      serialier(NULL), serializationOptions(NULL)
{
    serializationOptions = new GlobalSerializationOptions();
    serialier = Serializer::createSerializer(client->get_result_type());
}

OptimizedStatement::~OptimizedStatement()
{
    delete serialier;
    delete driver;
    delete plan;
    delete executionContext;
}

void ExplainStatement::execute()
{
    xs_decimal_t::init();
    plan = driver->getRQPForModule(driver->getModulesCount() - 1, false);
}

void ExplainStatement::next()
{
    tuple valueTuple(1);
    tuple_cell & value = valueTuple.cells[0];

    try {
        plan = VarGraphRewriting::rewrite(plan);
        plan = RewritingContext::rewrite(plan);
        optimizer->planContext()->varGraph.cleanup();

        XmlConstructor xmlConstructor(VirtualRootConstructor(0));
        xmlConstructor.openElement(SE_EL_NAME("plan"));
        plan->toXML(xmlConstructor);
        optimizer->planContext()->varGraph.toXML(xmlConstructor);
        xmlConstructor.closeElement();

        value = xmlConstructor.getLastChild();
    } catch (std::exception & e) {
        U_ASSERT(false);
    };

    client->begin_item(value.is_atomic(), value.get_atomic_type(), element, NULL);
    serialier->serialize(valueTuple);
    client->end_item(se_no_next_item);
}


