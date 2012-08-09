#ifndef _VIRTUAL_SEQUENCE_H_
#define _VIRTUAL_SEQUENCE_H_

#include "tr/opt/OptTypes.h"
#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/opt/algorithms/SequenceModel.h"

namespace executor {

//typedef void (* ExecutorProc)(void * object, VirtualSequence * executor);

/** @brief Interface for an object, that can continue a lazy sequence
 */
class IExecuteProc
{
public:
    virtual ~IExecuteProc() {};
    virtual void execute(VirtualSequence * sequence) = 0;
};

/** @brief Item of virtual sequence
 */
struct Result
{
    IExecuteProc * next;
    tuple_cell value;

    explicit Result(const tuple_cell & _tc) : next(NULL), value(_tc) {};
    explicit Result(IExecuteProc * _next) : next(_next) {};
};

// TODO : optimize result stack as it will be one of the most critical elements

typedef std::list<Result> ResultStack;

/** @brief Lazy evaluation implementation sequence
 */
struct VirtualSequence
{
private:
    ResultStack result;
    ResultStack::iterator position;
public:
    /** @brief Dynamic context of the sequence.
     * It is assigned only when sequence is assigned to the context,
     * so it is not mandatory
     */

    executor::DynamicContext * context;

    void push(const Result& _result) { position = result.insert(position, _result); };

    VirtualSequence() : context(NULL) { position = result.begin(); };

    inline void clear()
    {
        result.clear();
        position = result.begin();
    };

    inline
    tuple_cell next()
    {
        U_ASSERT(position == result.begin());

        while (position != result.end()) {
            if (NULL != position->next) {
                IExecuteProc * proc = position->next;
                position = result.erase(position);
                proc->execute(this);
                delete proc;
            } else if (position->value.is_eos()) {
                position = result.erase(position);
            } else {
                tuple_cell r = position->value;
                position = result.erase(position);
                return r;
            };
        };

        return tuple_cell::eos();
    };
};

};

#endif /* _VIRTUAL_SEQUENCE_ */
