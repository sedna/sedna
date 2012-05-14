/*
 * File:  OptimizablePlan.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _OPT_PLAN_H_
#define _OPT_PLAN_H_

#include <map>
#include <vector>
#include <set>
#include <string>
#include <sstream>
#include <stack>

#include "tr/structures/xmlns.h"

#include "tr/opt/algebra/PlanOperations.h"
#include "tr/opt/algebra/TupleScheme.h"
#include "tr/opt/algebra/DataGraph.h"

/* Look PlanOperationTypes for description of all operators */

class DataGraph;

namespace xsd {
  class AnyURI;
}

namespace xpath {
  struct NodeTest;
}

namespace rqp {
    class Optimizer;
    class Normalizer;

    struct opdesc_t {
        const char * opname;
        int opType;
        operation_result_type_t t;
    };

    class RPBase {
      protected:
        const opdesc_t * opdesc;
      public:
        virtual ~RPBase() {};

        // This MUST be implemented in every non-abstract class;
        inline const opdesc_t * getDesc() { return opdesc; };
        inline std::string getOpName() const { return opdesc->opname; }; 
        inline int getOpType() const { return opdesc->opType; }; 
        inline bool isType(int t) const { return opdesc->opType == t; }; 
        inline operation_result_type_t getOperationResultType() const { return opdesc->t; }; 
      protected:
        PlanContext * context;
        static int opids;
        int opid;
      public:
        RPBase(PlanContext * context_) : context(context_), opid(opids++) {};

        PlanContext * getContext() const { return context; };
    };

    template<class T>
    inline static
    bool instanceof(RPBase * op) { return op != null_op && op->getDesc() == &(T::sopdesc); };

    typedef int ScopeMarker;
    typedef std::multimap<TupleId, RPBase *> DependancyMap;
    typedef std::pair<DependancyMap::const_iterator, DependancyMap::const_iterator> DependancyMapRange;
    typedef std::multimap<RPBase *, TupleId> DependancyBackMap;
    typedef std::vector<TupleScheme *> TupleSchemeStorage;
    typedef std::vector<RPBase *> OperationList;
    typedef std::pair<TupleId, TupleDefinition> GreatMapRecord;
    typedef std::stack<TupleId> ScopeStack;
    typedef std::map<std::string, TupleId> TupleVarMap;
    typedef std::pair<std::string, TupleId> VarMapRecord;
    
    class PlanContext {
      private:
        ScopeMarker lastScopeMarker;
        TupleId currentTupleId;
        TupleSchemeMap greatTupleScheme;
        
        ScopeStack scopeStack;
        TupleVarMap scope;
        
        std::vector<void *> temporaryData;
      public:
        DataGraphMaster dataGraphFactory;
        
        PlanContext();
        ~PlanContext();
       
        DependancyMap tupleDependencyMap;
        DependancyBackMap tupleDependencyBackMap;

        TupleSchemeStorage tupleSchemeStorage;
        OperationList subplans;
        
        TupleScheme * newMapExtend(TupleScheme * in, const TupleScheme * with);
        TupleScheme * newMapExtendSingle(TupleScheme * in, TupleId with);
        
        const TupleDefinition * getVarDef(TupleId tid) const { return &(greatTupleScheme.at(tid)); };

        TupleId generateTupleId();
        TupleId generateTupleIdVarScoped(TupleVarDescriptor * var);
        TupleId getVarTupleInScope(const std::string & canonicalName);
        
        ScopeMarker setScopeMarker();
        void newScope();
        void clearScope();
        void clearScopesToMarker(ScopeMarker marker);

        RPBase * getExpressionResult(RPBase * tree);

        void deleteFromPlan(RPBase * item);
        void deleteSubtree(RPBase * item);

        void addDependancy(TupleId tid, RPBase * item) {
            tupleDependencyMap.insert(std::pair<TupleId, RPBase *>(tid, item));
            tupleDependencyBackMap.insert(std::pair<RPBase *, TupleId>(item, tid));
        };

        void * temporaryAlloc(size_t size) { void * x = malloc(size); temporaryData.push_back(x); return x; };
    };

/* YES! I know about typeid operator existance. */

#define ABSTRACT_OPERATION 

#define OPERATION \
  public:\
    static const opdesc_t sopdesc; \
  protected: \
    void __init();\
  private:
    
#define PROPERTY(name, t, member) t get##name() const { return member; } void set##name(t value) { member = value; }
#define PROPERTY_RO(name, t, member) t get##name() const { return member; }
#define PROPERTY_WO(name, t, member) void set##name(t value) { member = value; }

/* Basic operation classes, devided by arity */

    /* 0r-operations */
    class ConstantOperation : public RPBase {
        ABSTRACT_OPERATION
      public:
        ConstantOperation(PlanContext * context_);
    };

    /* 1r-operations */
    class ListOperation : public RPBase {
        ABSTRACT_OPERATION
      protected:
        RPBase * list;
      public:
        ListOperation(PlanContext * context_, RPBase * list_);

        PROPERTY_RO(List, RPBase *, list)
    };

    /* 1r-operations with independent nested operation plan */
    class NestedOperation : public ListOperation {
        ABSTRACT_OPERATION
      protected:
        RPBase * subplan;
      public:
        NestedOperation(PlanContext * context_, RPBase * list_, RPBase * subplan_);

        PROPERTY_RO(Subplan, RPBase *, subplan)
    };

    /* 2r-operations */
    class BinaryOperation : public RPBase {
        ABSTRACT_OPERATION
      private:
        RPBase * leftList;
        RPBase * rightList;
      public:
        BinaryOperation(PlanContext * context_, RPBase * ltItem_, RPBase * rtItem_);
      public:
        PROPERTY_RO(Left, RPBase *, leftList)
        PROPERTY_RO(Right, RPBase *, rightList)
    };

/*    
    class Let : public ListOperation { 
        OPERATION
      protected:
        TupleId mapsTo;
      public:
        Let(PlanContext * context_, TupleId tupleid, RPBase * list_);
      public:
        PROPERTY_RO(Var, TupleId, mapsTo)
    };
*/

    static inline 
    TupleScheme singleTupleScheme(TupleId tid) { TupleScheme a; a.insert(tid); return a; }

    class Map : public NestedOperation {
        OPERATION
      protected:
        TupleId mapsTo;
      public:
        Map(PlanContext * context_, RPBase * opin_, RPBase * function_, TupleId iterateTuple, TupleId resultTuple);
      public:
        PROPERTY_RO(Var, TupleId, mapsTo)
    };

    class MapAll : public Map {
        OPERATION
      public:
        MapAll(PlanContext * context_, RPBase * opin_, RPBase * function_, TupleId iterateTuple, TupleId resultTuple);
    };

    class Functional : public RPBase {
        OPERATION
      protected:
        DataGraph * func;
      public:
        Functional(PlanContext * context_, DataGraph * function_);
    };

    class Serialize : public RPBase {
        OPERATION
      protected:
        RPBase * opin_;
      public:
        Serialize(PlanContext * context_, TupleId iterateTuple, RPBase * opin_);
    };

#undef OPERATION
#undef PROPERTY
#undef PROPERTY_RO
#undef PROPERTY_WO

}

#endif /* _OPT_PLAN_H_ */
