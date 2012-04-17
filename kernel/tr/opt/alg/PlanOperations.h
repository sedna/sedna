/*
 * File:  PlanOperationTypes.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PLAN_OP_MINIMAL_H_
#define _PLAN_OP_MINIMAL_H_

#include "common/sedna.h"

namespace rqp {
    class PlanContext;

    struct TupleDefinition;

    class RPBase;

    class ConstantOperation;
    class NestedOperation;
    class ListOperation;
    class BinaryOperation;

    static RPBase * const null_op = (RPBase *)(NULL);

/**** Logical operations ****/

    class Map;
    class MapAll;

/**** Physical operations ****/

    class Functional;
    class Serialize;

//    class AtomicConstructor;
//    class GeneralComparison;
//    class FNDocumentConst; /* Get document */ // item => sequence-of-items
    

    /* Let converts item to tuple by assigning input to a variable */
  
//    class Let; // sequence-of-items => sequence-of-tuples

    /* Map operation produces a tuple with extended 
       tuple scheme for every tuple in input list, by applying 
       operation "function" to it. */
  
//    class Map; // (sequence-of-items, sequence-of-tuples) => sequence-of-tuples

    /* MapProduct operation produces a tuple with extended 
       tuple scheme for every tuple in input list, and every tuple
       returned by operation "function". Similar to cartesian product. */

//    class MapProduct; // (sequence-of-items, sequence-of-tuples) => sequence-of-tuples

//    class Product; // (sequence-of-tuples, sequence-of-tuples) => sequence-of-tuples

    /* SemiJoin operation produces a tuple with extended 
       tuple scheme for every tuple in input list, and every tuple
       returned by operation "function". If no tuple is returned by
       "function" the tuple from input list is still returned.
       Similar to semijoin operation. */

//    class SemiJoin; // (sequence-of-items, sequence-of-tuples) => sequence-of-tuples
    
    /* Filter produces only tuples from input list if
       expression "predicate" on that tuple does not return null */
    
//    class Select; // sequence-of-tuples => sequence-of-tuples
    
    /* Reduce operation removes items from scema definition */
    
//    class Reduce; // sequence-of-tuples => sequence-of-tuples

    /* ItemReduce the same as Reduce, but always reduces to single-scheme-tuple and converts to item */
    
//    class ItemReduce; // sequence-of-tuples => sequence-of-items
    
    /* Collect (Group by) */
    
//    class Collect; // sequence-of-tuples => sequence-of-tuples

    /* Enumerate creates a counter variable and numerates tuples from input list to this variable  */
    
//    class Enumerate; // sequence-of-tuples => sequence-of-tuples
    
};

#endif /* _PLAN_OP_MINIMAL_H_ */
