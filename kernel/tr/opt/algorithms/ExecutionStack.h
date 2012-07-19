#ifndef _EXECUTION_STACK_H_
#define _EXECUTION_STACK_H_

#include "tr/opt/OptTypes.h"
#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/opt/SequenceModel.h"

namespace executor {

struct ResultSequence;
struct DynamicContext;

typedef void (* ExecutorProc)(void * object, ResultSequence * executor);
  
struct NextResultProc
{
    ExecutorProc proc;
    void * object;

    inline
    void operator()(ResultSequence * executor)
    {
        proc(object, executor);
    };

    NextResultProc() : proc(NULL) {};

    NextResultProc(ExecutorProc _proc, void * _object)
      : proc(_proc), object(_object) {};

    inline bool is_null() const { return proc == NULL; }
};
  
struct Result
{
    NextResultProc next;
    tuple_cell value;

    explicit Result(const tuple_cell & _tc) : value(_tc) {};
    explicit Result(const NextResultProc & _next) : next(_next) {};
};

typedef std::list<Result> ResultStack;

struct DynamicContext;

struct ResultSequence
{
private:
    ResultStack result;
    ResultStack::iterator position;
public:
    executor::DynamicContext * context;

    ResultSequence(executor::DynamicContext * _context) : context(_context)
    {
    };

    void push(const Result& _result) { position = result.insert(position, _result); };

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
            if (!position->next.is_null()) {
                NextResultProc proc = position->next;
                position = result.erase(position);
                proc(this);
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

#endif /* _EXECUTION_STACK_H_ */
