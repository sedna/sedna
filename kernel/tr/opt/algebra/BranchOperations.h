#ifndef _BRANCH_OPERATIONS_H_
#define _BRANCH_OPERATIONS_H_

#include "IndependentPlan.h"

namespace rqp {

class If : public RPBase {
    OPERATION(0x015)
private:
public:
    If(RPBase* _condition, RPBase* _then, RPBase* _else)
      : RPBase(&sopdesc) {
        children.push_back(_condition);
        children.push_back(_then);
        children.push_back(_else);

        PlanContext::current->registerLink(this, _condition, &(children[0]));
        PlanContext::current->registerLink(this, _then, &(children[1]));
        PlanContext::current->registerLink(this, _else, &(children[2]));
    };

    PROPERTY_RO(Condition, RPBase *, children[0])
    PROPERTY_RO(Then, RPBase *, children[1])
    PROPERTY_RO(Else, RPBase *, children[2])
};


}

#endif /* _BRANCH_OPERATIONS_H_ */
