#include "ASTOpt.h"

#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/executor/base/XPath.h"

using namespace sedna;
using namespace rqp;

void lr2opt::visit(ASTMainModule &n)
{
//    n.prolog->accept(*this);
    // TODO : remove
    if (PlanContext::current == NULL) {
        PlanContext::current = new PlanContext();
    }

    n.query->accept(*this);
}

void lr2opt::visit(ASTQuery &n)
{
    typeVarMode = false;
    contextStack.push(OpContextInfo(null_op, worldDataTupleId));
    
    if (n.type == ASTQuery::QUERY) {
        n.query->accept(*this);
        OpContextInfo expressionResult = resultStack.top();

        OpContextInfo result(
            new ItemReduce(expressionResult.op, expressionResult.contextItem),
            expressionResult.contextItem
                      );
    } else {
      //
    }
}

void lr2opt::visit(ASTAxisStep &n) {
    contextStack.push(OpContextInfo(&n));
    OpContextInfo & info = contextStack.top();

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
    
    if (n.cont != NULL) {
        n.cont->accept(*this);
    }

    if (n.test == NULL) {
        throw USER_EXCEPTION(2902);
    } else {
        n.test->accept(*this);
    }

    info.op = new XPathStep(resultStack.top().op, pe::Step(info.axis, info.nodeTest, info.qname));
    resultStack.pop();

    if (n.preds != NULL) {
        for (ASTNodesVector::iterator pred = n.preds->begin(); pred != n.preds->end(); pred++) {
            (*pred)->accept(*this);
            info.op = new Select(info.op, resultStack.top().op);
            resultStack.pop();
            /* Context item is preserved */
        }
    }

    resultStack.push(contextStack.top());
    contextStack.pop();
}

void lr2opt::visit(ASTFilterStep &n) {
    throw USER_EXCEPTION(2902);

    if (n.cont != NULL) {
        n.cont->accept(*this);

        contextStack.push(resultStack.top());
        resultStack.pop();
    }
}

void lr2opt::visit(ASTPred &n) {
    U_ASSERT(n.others.size() == 1);

    n.others[0].expr->accept(*this);
}

void lr2opt::visit(ASTDocTest &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTAttribTest &n) {
    contextStack.top().nodeTest = pe::nt_attribute;
    contextStack.top().qname = xsd::QNameAny;
    n.name->accept(*this);
    // TODO!
}

void lr2opt::visit(ASTElementTest &n) {
    contextStack.top().nodeTest = pe::nt_element;
    contextStack.top().qname = xsd::QNameAny;
    n.name->accept(*this);
    // TODO!
}

void lr2opt::visit(ASTEmptyTest &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTTextTest &n) {
    contextStack.top().nodeTest = pe::nt_text;
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
    throw USER_EXCEPTION(2902);
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

void lr2opt::visit(ASTElem &n) {
    throw USER_EXCEPTION(2902);
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
    throw USER_EXCEPTION(2902);
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
        
        resultStack.push(OpContextInfo(new FunCall(*n.int_name, oplist), invalidTupleId));
    } else {
        throw USER_EXCEPTION(2902);
    }
}

void lr2opt::visit(ASTUop &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTBop &n) {
    contextStack.push(OpContextInfo(&n));

    n.lop->accept(*this);
    RPBase * leftSequence = resultStack.top().op;
    resultStack.pop();

    n.rop->accept(*this);
    RPBase * rightSequence = resultStack.top().op;
    resultStack.pop();
    
    if (n.op >= ASTBop::EQ_G && n.op <= ASTBop::GE_G) {
        opt::Comparison cmp;

        switch (n.op) {
          case ASTBop::GT_G :
            cmp.op = opt::Comparison::g_gt;
            break;
          case ASTBop::EQ_G :
            cmp.op = opt::Comparison::g_eq;
            break;
          default:
           throw USER_EXCEPTION(2902);
        };

        resultStack.push(OpContextInfo(new ComparisonExpression(leftSequence, rightSequence, cmp), invalidTupleId));
    } else {
        U_ASSERT(false);
    };

    contextStack.pop();
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

void lr2opt::visit(ASTQuantExpr &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTTypeSwitch &n) {
    throw USER_EXCEPTION(2902);
}

void lr2opt::visit(ASTSeq &n) {
    throw USER_EXCEPTION(2902);
}


/* XQuery language - FLWOR and its parts */

void lr2opt::visit(ASTFLWOR &n) {
    contextStack.push(OpContextInfo(&n));

    std::stack<OpContextInfo> operationStack;

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
            new MapConcat(resultStack.top().op, operationStack.top().op, operationStack.top().contextItem);
          case SequenceConcat::opid :
            new SequenceConcat(resultStack.top().op, operationStack.top().op, operationStack.top().contextItem);
          case If::opid :
            new If(operationStack.top().op, resultStack.top().op, null_op);
          default:
            U_ASSERT(false);
            throw USER_EXCEPTION(2902);
        };
        
        operationStack.pop();
    };
    
    PlanContext::current->clearScopesToMarker(scopeMarker);

    contextStack.pop();
}

void lr2opt::visit(ASTFor &n) {
    contextStack.push(OpContextInfo(&n));
    
    n.expr->accept(*this);
    n.tv->accept(*this);

    // TODO : process type info, important step

    opt::TupleId varBinding = PlanContext::current->generateTupleIdVarScoped(resultStack.top().varDesc);

    resultStack.pop();
    resultStack.top().contextItem = varBinding;
    resultStack.top().opid = MapConcat::opid;

    if (n.usesPosVar()) {
        U_ASSERT(false);
    }

    contextStack.pop();
}

void lr2opt::visit(ASTLet &n) {
    contextStack.push(OpContextInfo(&n));

    n.expr->accept(*this);
    n.tv->accept(*this);

    opt::TupleId varBinding = PlanContext::current->generateTupleIdVarScoped(resultStack.top().varDesc);

    resultStack.pop();
    resultStack.top().contextItem = varBinding;
    resultStack.top().opid = SequenceConcat::opid;
    U_ASSERT(false);

    contextStack.pop();
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
    resultStack.push(OpContextInfo(&n));

    TupleVarDescriptor * vd = new TupleVarDescriptor();
    
    lr2por::visit(n);
    childOffer offer = lr2por::getOffer();
    
    vd->t = offer.st.type.info.single_type;
    vd->name = static_cast<ASTVar *>(n.var)->getStandardName();

    resultStack.top().varDesc = vd;
}

void lr2opt::visit(ASTType &n) {
    lr2por::visit(n);
}

void lr2opt::visit(ASTVar &n) {
    if (dynamic_cast<ASTFor *>(contextStack.top().astNode) != NULL) {
    } else if (dynamic_cast<ASTLet *>(contextStack.top().astNode) != NULL) {
    } else {
        resultStack.push(
          OpContextInfo(
            new VarIn(PlanContext::current->getVarTupleInScope(n.getStandardName())), invalidTupleId));
    }
}

void lr2opt::visit(ASTLit &n) {
    opt::MemoryTupleSequence * mts = new opt::MemoryTupleSequence;
    resultStack.push(OpContextInfo(new Const(mts), invalidTupleId));

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

