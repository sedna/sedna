#ifndef _BRANCH_OPERATIONS_H_
#define _BRANCH_OPERATIONS_H_

#include "IndependentPlan.h"

namespace rqp {

class If : public RPBase {
    RTTI_DECL(plan_operation_If, RPBase)
private:
    virtual XmlConstructor& __toXML ( XmlConstructor& ) const;

    unsigned thenBranch;
    unsigned elseBranch;
public:
    bool singleBranch;
  
    If(RPBase* _condition, RPBase* _then, RPBase* _else)
      : RPBase(SELF_RTTI_REF), thenBranch(0), elseBranch(0), singleBranch(false)
    {
        children.push_back(_condition);
        dependantVariables.insert(_condition->dependsOn().begin(), _condition->dependsOn().end());

        if (_then != null_obj) {
            thenBranch = children.size();
            children.push_back(_then);
            dependantVariables.insert(_then->dependsOn().begin(), _then->dependsOn().end());
        }

        if (_else != null_obj) {
            elseBranch = children.size();
            children.push_back(_else);
            dependantVariables.insert(_else->dependsOn().begin(), _else->dependsOn().end());
        }

        if (_else == null_obj) {
            resultChild = thenBranch;
            singleBranch = true;
        } else if (_then == null_obj) {
            resultChild = elseBranch;
            singleBranch = true;
        };
    };

    PROPERTY_RO(Condition, RPBase *, children[0])

    RPBase * getThen() const { return (thenBranch != 0) ? children[thenBranch] : null_op; };
    RPBase * getElse() const { return (elseBranch != 0) ? children[elseBranch] : null_op; };
};


}

#endif /* _BRANCH_OPERATIONS_H_ */
