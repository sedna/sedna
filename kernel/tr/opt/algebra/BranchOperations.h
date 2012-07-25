#ifndef _BRANCH_OPERATIONS_H_
#define _BRANCH_OPERATIONS_H_

#include "IndependentPlan.h"

namespace rqp {

class If : public RPBase {
    RTTI_DECL(op_if, RPBase)
private:
    virtual XmlConstructor& __toXML ( XmlConstructor& ) const;
public:
    If(RPBase* _condition, RPBase* _then, RPBase* _else)
      : RPBase(SELF_RTTI_REF)
    {
        children.push_back(_condition);
        children.push_back(_then);
        children.push_back(_else);

        if (_else == null_obj) {
            resultChild = 1;
        } else if (_then == null_obj) {
            resultChild = 2;
        };
    };

    PROPERTY_RO(Condition, RPBase *, children[0])
    PROPERTY_RO(Then, RPBase *, children[1])
    PROPERTY_RO(Else, RPBase *, children[2])
};


}

#endif /* _BRANCH_OPERATIONS_H_ */
