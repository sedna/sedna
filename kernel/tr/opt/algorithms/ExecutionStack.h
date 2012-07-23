#ifndef _EXECUTION_STACK_H_
#define _EXECUTION_STACK_H_

#include "tr/opt/OptTypes.h"
#include "tr/opt/algebra/IndependentPlan.h"
#include "tr/opt/SequenceModel.h"

namespace executor {

struct ExecutionStack;
struct DynamicContext;

//typedef void (* ExecutorProc)(void * object, ExecutionStack * executor);

class IExecuteProc
{
public:
    virtual ~IExecuteProc() {};
    virtual void execute(ExecutionStack * executor) = 0;
};

/*
struct NextResultProc
{
    ExecutorProc proc;
    void * object;

    inline
    void operator()(ExecutionStack * executor)
    {
        proc(object, executor);
    };

    NextResultProc() : proc(NULL) {};

    NextResultProc(ExecutorProc _proc, void * _object)
      : proc(_proc), object(_object) {};

    inline bool is_null() const { return proc == NULL; }
};
*/

struct Result
{
    IExecuteProc * next;
    tuple_cell value;

    explicit Result(const tuple_cell & _tc) : next(NULL), value(_tc) {};
    explicit Result(IExecuteProc * _next) : next(_next) {};

//    ~Result() { delete next; } TODO : should somehow delete itself
};

// TODO : optimize result stack as it will be one of the most critical elements

typedef std::list<Result> ResultStack;

struct ExecutionStack
{
private:
    ResultStack result;
    ResultStack::iterator position;
public:
    void push(const Result& _result) { position = result.insert(position, _result); };

    ExecutionStack() { position = result.begin(); };
    
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

#endif /* _EXECUTION_STACK_H_ */
