#ifndef _VIRTUAL_SEQUENCE_H_
#define _VIRTUAL_SEQUENCE_H_

#include "tr/opt/OptTypes.h"
#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/opt/algorithms/SequenceModel.h"
#include "tr/opt/evaluation/DynamicContext.h"

#include <vector>

namespace executor {

class DynamicContext;

//typedef void (* ExecutorProc)(void * object, VirtualSequence * executor);

/** @brief Interface for an object, that can continue a lazy sequence
 */
class IExecuteProc
{
public:
    virtual ~IExecuteProc() {};
    virtual void execute(ResultSequence * sequence) = 0;
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

typedef std::vector<Result> ResultStack;

struct ResultSequence
{
protected:
    ResultStack result;
public:
    /** @brief Dynamic context of the sequence.
     * It is assigned only when sequence is assigned to the context, so it is not mandatory
     */
    executor::DynamicContext * context;

    ResultSequence() : context(NULL) {};

    inline void push(const Result& _result) { result.push_back(_result); };
    inline void clear() { result.clear(); };
    inline bool empty() const { return result.empty(); };
};

/** @brief Update primitive sequence
 */
struct UpdateSequence : public ResultSequence
{
    inline
    void execute()
    {
        while (!result.empty()) {
            U_ASSERT(NULL != result.back().next);

            IExecuteProc * proc = result.back().next;
            result.pop_back();
            proc->execute(this);
            delete proc;
        };
    };
};

struct SetContext {
    DynamicContext * dc;
    VirtualSequence * oldvs;

    inline
    SetContext(DynamicContext * _dc, VirtualSequence * vs)
        : dc(_dc)
    {
        if (dc->stack != vs) {
            oldvs = dc->stack;
            dc->stack = vs;
        } else {
            dc = NULL;
        };
    }

    inline
    ~SetContext() { if (dc != NULL) { dc->stack = oldvs; } };
};

/** @brief Lazy evaluation implementation sequence
 */
struct VirtualSequence : public ResultSequence
{
    inline
    tuple_cell next()
    {
        U_ASSERT(context != NULL);
        SetContext _setContext(context, this);

        while (!result.empty()) {
            if (NULL != result.back().next) {
                IExecuteProc * proc = result.back().next;
                result.pop_back();
                proc->execute(this);
                delete proc;
            } else if (result.back().value.is_eos()) {
                result.pop_back();
            } else {
                tuple_cell r = result.back().value;
                result.pop_back();
                return r;
            };
        };

        return tuple_cell::eos();
    };
};

};

#endif /* _VIRTUAL_SEQUENCE_ */
