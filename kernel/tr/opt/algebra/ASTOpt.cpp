#include "ASTOpt.h"

#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/executor/base/XPath.h"

using namespace sedna;
using namespace rqp;

/*
 * NOTE: Context item can be ONLY overwritten
 * during evaluation of path expressions and predicates!!!
 */

void lr2opt::visit(ASTMainModule &n)
{
//    n.prolog->accept(*this);
    // TODO : remove

    if (PlanContext::current == NULL) {
        PlanContext::current = new PlanContext();
    }

    varVisitContext = vvc_get_value;
    n.query->accept(*this);
}

void lr2opt::visit(ASTQuery &n)
{
    if (n.type == ASTQuery::QUERY) {
        contextVariable = opt::invalidTupleId;
        n.query->accept(*this);
    } else {
      //
    }
}

void lr2opt::visit(ASTAxisStep &n) {
    stepStack.push(StepInfo());
    StepInfo & info = stepStack.top();

    info.tqname = xsd::QNameAny;

    switch (n.axis) {
      case ASTAxisStep::CHILD :
        info.axis = pe::axis_child;
        break;
      case ASTAxisStep::DESCENDANT :
        info.axis = pe::axis_descendant;
        break;
      case ASTAxisStep::DESCENDANT_OR_SELF :
        info.axis = pe::axis_descendant_or_self;
        break;
      case ASTAxisStep::ATTRIBUTE :
        info.axis = pe::axis_attribute;
        break;
      default:
        throw USER_EXCEPTION(2902);
    }

    RPBase * resultOp = null_op;

    if (n.cont != NULL) {
        n.cont->accept(*this);
        resultOp = resultStack.top().op;
        resultStack.pop();
    } else {
        resultOp = new rqp::VarIn(contextVariable);
    };

    if (n.test == NULL) {
        throw USER_EXCEPTION(2902);
    } else {
        n.test->accept(*this);
    }

    resultOp = new XPathStep(resultOp, pe::Step(info.axis, info.nodeTest, info.tqname));

    stepStack.pop();

    if (n.preds != NULL) {
        opt::TupleId saveContextVariable = contextVariable;

        for (ASTNodesVector::iterator pred = n.preds->begin(); pred != n.preds->end(); pred++) {
            contextVariable = PlanContext::current->generateTupleId();
            (*pred)->accept(*this);
            resultOp = new Select(resultOp, resultStack.top().op, contextVariable);
            resultStack.pop();
        }

        contextVariable = saveContextVariable;
    }

    resultStack.push(ResultInfo(resultOp));
}

void lr2opt::visit(ASTFilterStep &n) {
    // TODO: context variable
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTPred &n) {
    U_ASSERT(n.others.size() == 1);

    n.others[0].expr->accept(*this);
}

void lr2opt::visit(ASTDocTest &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTAttribTest &n) {
    U_ASSERT(dynamic_cast<ASTAxisStep*>(*(vis_path.rbegin()+1)) != NULL);

    stepStack.top().nodeTest = pe::nt_attribute;
    
    n.name->accept(*this);
    // TODO!
}

void lr2opt::visit(ASTElementTest &n) {
    U_ASSERT(dynamic_cast<ASTAxisStep*>(*(vis_path.rbegin()+1)) != NULL);

    stepStack.top().nodeTest = pe::nt_element;
    
    n.name->accept(*this);
    // TODO!
}

void lr2opt::visit(ASTEmptyTest &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTTextTest &n) {
    U_ASSERT(dynamic_cast<ASTAxisStep*>(*(vis_path.rbegin()+1)) != NULL);

    stepStack.top().nodeTest = pe::nt_text;
    // TODO!
}

void lr2opt::visit(ASTCommTest &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTPiTest &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTSchemaAttrTest &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTSchemaElemTest &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTNameTest &n) {
    U_ASSERT(
       dynamic_cast<ASTAttribTest*>(*(vis_path.rbegin()+1)) != NULL ||
       dynamic_cast<ASTElementTest*>(*(vis_path.rbegin()+1)) != NULL
            );
  
    xmlns_ptr ns = NULL_XMLNS;
    const char * local = xsd::QNameWildcard;

    if (n.pref != NULL) {
        ns = skn->resolvePrefix(n.pref->c_str());
    };

    if (n.local != NULL) {
        local = n.local->c_str();
    };

    stepStack.top().tqname = xsd::TemplateQName(ns == NULL_XMLNS ? NULL : ns->get_uri(), local);
}

void lr2opt::visit(ASTNodeTest &n) {
    throw USER_EXCEPTION(2902);
}

/* XQuery language - Constructors */

void lr2opt::visit(ASTDocConst &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTAttr &n){
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTAttrConst &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTElem &n)
{
    xmlns_ptr ns = NULL_XMLNS;
    unsigned count = 0;

    U_ASSERT(n.local != NULL);

    int sknMark = skn->mark();

    if (n.attrs)
    {
        ASTVisitor::VisitNodesVector(n.attrs, *this);
        count += n.attrs->size();
    }

    if (n.cont)
    {
        ASTVisitor::VisitNodesVector(n.cont, *this);
        count += n.cont->size();
    }

    OperationList oplist;
    oplist.reserve(count);

    while (count--) {
        oplist.push_back(resultStack.top().op);
        resultStack.pop();
    }

    std::reverse(oplist.begin(), oplist.end());

    xsd::QName qname = xsd::QName::resolve(
        n.pref != NULL ? n.pref->c_str() : NULL,
        n.local->c_str(), skn);
    
    resultStack.push(ResultInfo(
        new Construct(element,
            new Const(tuple_cell::atomic(qname)),
            new Sequence(oplist)
        )));

    skn->rollbackToMark(sknMark);
}

void lr2opt::visit(ASTElemConst &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTXMLComm &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTCommentConst &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTPi &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTPIConst &n) {
    throw USER_EXCEPTION(2902);
}


void lr2opt::visit(ASTTextConst &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTCharCont &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTSpaceSeq &n) {
    n.expr->accept(*this);

    Sequence * op = new Sequence(resultStack.top().op);
    resultStack.top().op = op;

    if (n.atomize) {
        op->setSpaces(Sequence::all_spaces);
    } else {
        op->setSpaces(Sequence::atomic_spaces);
    };
}

void lr2opt::visit(ASTNsp &n) {
    throw USER_EXCEPTION(2902);
}


/* XQuery language - Functions and operators */

void lr2opt::visit(ASTFunCall &n) {
    if (*n.int_name == "!fn!document") {
        // TODO: make strict param clearness;

        OperationList oplist;
        n.params->at(0)->accept(*this);
        oplist.push_back(resultStack.top().op);
        resultStack.pop();

        xmlns_ptr ns;

        if (!n.pref->empty()) {
             ns = skn->resolvePrefix(n.pref->c_str());
        } else {
             ns = skn->resolvePrefix("fn");
        };

        resultStack.push(ResultInfo(
            new FunCall(xsd::constQName(ns, n.local->c_str()), oplist)));
    } else {
        throw USER_EXCEPTION(2902);
    }
}

void lr2opt::visit(ASTUop &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTBop &n) {
    n.lop->accept(*this);
    RPBase * leftSequence = resultStack.top().op;
    resultStack.pop();

    n.rop->accept(*this);
    RPBase * rightSequence = resultStack.top().op;
    resultStack.pop();
    
    if (n.op >= ASTBop::IS && n.op <= ASTBop::GE_G) {
        opt::Comparison cmp;

        switch (n.op) {
          case ASTBop::PREC :
            cmp.op = opt::Comparison::do_before;
            break;
          case ASTBop::FOLLOW :
            cmp.op = opt::Comparison::do_after;
            break;
          case ASTBop::GT_G :
            cmp.op = opt::Comparison::g_gt;
            break;
          case ASTBop::EQ_G :
            cmp.op = opt::Comparison::g_eq;
            break;
          default:
           throw USER_EXCEPTION(2902);
        };

        resultStack.push(ResultInfo(new ComparisonExpression(leftSequence, rightSequence, cmp)));
    } else {
        U_ASSERT(false);
    };
}

void lr2opt::visit(ASTCast &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTCastable &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTInstOf &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTTreat &n) {
    throw USER_EXCEPTION(2902);
}


/* XQuery language - Statements */

void lr2opt::visit(ASTCase &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTIf &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTQuantExpr &n)
{
    ScopeMarker scopeMarker = PlanContext::current->setScopeMarker();

    n.expr->accept(*this);

    varVisitContext = vvc_declare;
    n.var->accept(*this);
    varVisitContext = vvc_get_value;

    // TODO : process type info, important step

    opt::TupleId varBinding = PlanContext::current->generateTupleIdVarScoped(resultStack.top().variableName);
    resultStack.pop();

    rqp::RPBase * expression = resultStack.top().op;
    resultStack.pop();

    n.sat->accept(*this);

    rqp::RPBase * predicate = resultStack.top().op;
    resultStack.pop();

    xsd::QName aggrFunction;

    // TODO: not fair! should be an aggr function
    if (n.type == ASTQuantExpr::SOME)
    {
        aggrFunction = xsd::constQName(skn->resolvePrefix("fn"), "opt_not_empty");
//        predicate = new rqp::If(predicate, new rqp::Const(fn_true()), new rqp::Const(EmptySequenceConst()));
    }
    else /* EVERY */
    {
        aggrFunction = xsd::constQName(skn->resolvePrefix("fn"), "empty");
        predicate = new rqp::If(predicate, new rqp::Const(EmptySequenceConst()), new rqp::Const(fn_true()));
    }

    resultStack.push(ResultInfo(
        new rqp::MapConcat(predicate, expression, varBinding)));
/*    
    resultStack.push(ResultInfo(
        new rqp::FunCall(aggrFunction,
            new rqp::MapConcat(predicate, expression, varBinding))));
*/
    PlanContext::current->clearScopesToMarker(scopeMarker);
}

void lr2opt::visit(ASTTypeSwitch &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTSeq &n) {
    throw USER_EXCEPTION(2902);
}


/* XQuery language - FLWOR and its parts */

void lr2opt::visit(ASTFLWOR &n) {
    std::stack<ResultInfo> operationStack;

    ScopeMarker scopeMarker = PlanContext::current->setScopeMarker();
    
    for (ASTNodesVector::const_iterator fl = n.fls->begin(); fl != n.fls->end(); fl++) {
        PlanContext::current->newScope();
        (*fl)->accept(*this);

        operationStack.push(resultStack.top());
        resultStack.pop();
    }

    if (n.where) {
        n.where->accept(*this);

        operationStack.push(resultStack.top());
        resultStack.pop();
        operationStack.top().opid = If::opid;
    }

    if (n.order_by) {
        throw USER_EXCEPTION(2902);
    }

    n.ret->accept(*this);

    while (!operationStack.empty()) {
        switch (operationStack.top().opid) {
          case MapConcat::opid :
            resultStack.top().op = new MapConcat(resultStack.top().op, operationStack.top().op, operationStack.top().variableId);
            break;
          case SequenceConcat::opid :
            resultStack.top().op = new SequenceConcat(resultStack.top().op, operationStack.top().op, operationStack.top().variableId);
            break;
          case If::opid :
            resultStack.top().op = new If(operationStack.top().op, resultStack.top().op, null_op);
            break;
          default:
            U_ASSERT(false);
            throw USER_EXCEPTION(2902);
        };
        
        operationStack.pop();
    };
    
    PlanContext::current->clearScopesToMarker(scopeMarker);
}

void lr2opt::visit(ASTFor &n) {
    n.expr->accept(*this);

    varVisitContext = vvc_declare;
    n.tv->accept(*this);
    varVisitContext = vvc_get_value;

    // TODO : process type info, important step, make type assert
    // TODO : process pos var

    opt::TupleId varBinding = PlanContext::current->generateTupleIdVarScoped(resultStack.top().variableName);

    resultStack.pop();
    resultStack.top().variableId = varBinding;
    resultStack.top().opid = MapConcat::opid;

    if (n.usesPosVar()) {
        U_ASSERT(false);
    }
}

void lr2opt::visit(ASTLet &n) {
    n.expr->accept(*this);

    varVisitContext = vvc_declare;
    n.tv->accept(*this);
    varVisitContext = vvc_get_value;

    // TODO : process type info, important step, make type assert
    
    opt::TupleId varBinding = PlanContext::current->generateTupleIdVarScoped(resultStack.top().variableName);

    resultStack.pop();
    resultStack.top().variableId = varBinding;
    resultStack.top().opid = SequenceConcat::opid;
}

void lr2opt::visit(ASTOrder &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTOrderBy &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTOrderMod &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTOrderModInt &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTOrderSpec &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTPosVar &n) {
    n.var->accept(*this);
}


/* XQuery language - Atoms */

void lr2opt::visit(ASTTypeSeq &n) {
    lr2por::visit(n);
}


void lr2opt::visit(ASTTypeVar &n) {
    U_ASSERT(varVisitContext == vvc_declare);
    resultStack.push(ResultInfo(null_op));

    lr2por::visit(n);
    childOffer offer = lr2por::getOffer();
    
    resultStack.top().variableName = static_cast<ASTVar *>(n.var)->getStandardName();
}

void lr2opt::visit(ASTType &n) {
    lr2por::visit(n);
}

void lr2opt::visit(ASTVar &n) {
    switch (varVisitContext) {
      case vvc_get_value :
        resultStack.push(ResultInfo(
            new VarIn(PlanContext::current->getVarTupleInScope(n.getStandardName()))));
      break;
      case vvc_declare :
      default:
        U_ASSERT(false);
      break;
    };
}

void lr2opt::visit(ASTLit &n) {
    opt::MemoryTupleSequence * mts = new opt::MemoryTupleSequence;
    resultStack.push(ResultInfo(new Const(mts)));

    switch (n.type) {
      case ASTLit::STRING:
        mts->push_back(tuple_cell::atomic_deep(xs_string, n.lit->c_str()));
      break;
      default:
        U_ASSERT(false);
    };
}

void lr2opt::visit(ASTQName &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTOrdExpr &n) {
    throw USER_EXCEPTION(2902);
}


/* Xquery PP helpers */

void lr2opt::visit(ASTDDO &n) {
    throw USER_EXCEPTION(2902);
}

