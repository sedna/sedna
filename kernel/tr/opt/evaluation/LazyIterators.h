#ifndef _LAZY_ITERATORS_H_
#define _LAZY_ITERATORS_H_

#include "tr/opt/OptForewards.h"
#include "tr/opt/evaluation/VirtualSequence.h"

namespace executor {

class NextWindow : public IExecuteProc
{
public:
    VariableProducer * producer;
    rqp::RPBase * nextOp;
    uint64_t restrictMask;

    explicit NextWindow(VariableProducer * _producer, rqp::RPBase * _nextOp, uint64_t _restrictMask)
      : producer(_producer), nextOp(_nextOp), restrictMask(_restrictMask) {};

    virtual void execute(executor::VirtualSequence* sequence);
};

};

#endif /* _LAZY_ITERATORS_H_ */
