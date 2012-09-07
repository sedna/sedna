#ifndef _FN_OPERATIONS_H_
#define _FN_OPERATIONS_H_

#include "Functions.h"
#include "tr/opt/algorithms/ValueFunction.h"

class CollationHandler;

struct BinaryTupleCellOpData : public IFunctionData {
    bin_op_tuple_cell_tuple_cell bf;
    bin_op_tuple_cell_tuple_cell_collation bfc;

    xq_binary_op_type ftype;
    CollationHandler * collation;

    BinaryTupleCellOpData(
      xq_binary_op_type _ftype
    ) : ftype(_ftype), collation(NULL) { };

    BinaryTupleCellOpData(
      xq_binary_op_type _ftype,
      CollationHandler * _collation
    ) : ftype(_ftype), collation(_collation) {};

    BinaryTupleCellOpData(
      bin_op_tuple_cell_tuple_cell _bf
    ) : bf(_bf), ftype(xqbop_le), collation(NULL) {};

    BinaryTupleCellOpData(
      bin_op_tuple_cell_tuple_cell_collation _bfc,
      CollationHandler * _collation
    ) : bfc(_bfc), ftype(xqbop_le), collation(_collation) {};

    void init(xmlscm_type t1, xmlscm_type t2);
    
    virtual XmlConstructor& toXML(XmlConstructor& constructor) const;
};

extern phop::FunctionInfo * binaryOperationFunction;

#endif /* _FN_OPERATIONS_H_ */
