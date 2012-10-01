#ifndef _XML_TYPE_H_
#define _XML_TYPE_H_

//#include "u/u.h"
//#include "common/xptr/xptr.h"

// Total size should be equal to 4+4+16 = 24

class Value {
  
    Value() {};

    explicit Value(const value_t & _x) : x(_x) {
        acquireObject();
    };
    
    Value(const Value& _x) : x(_x.x) {
        acquireObject();
    };

    ~Value() {
        releaseObject();
    };

    Value& operator=(const Value& _x) {
        if (&_x != this) {
            releaseObject();
            x = _x.x;
            acquireObject();
        };

        return *this;
    };
};

#endif /* _XML_TYPE_H_ */
