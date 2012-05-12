#ifndef _XML_TYPE_H_
#define _XML_TYPE_H_

//#include "common/u/u.h"
//#include "common/xptr.h"

// Total size should be equal to 4+4+16 = 24

union variant_t {
    void * _ptr;
    bool _bool;
    double _double;
    float _float;
    int64_t _int;
    uint64_t _uint;
//    xs_decimal_t _decimal;
//    xptr _xptr;
    IObjectDisposable * _object;
};

struct value_t {
    uint16_t _physicalFlag;
    uint16_t _logicalType;

    variant_t val;
};

class Value {
    value_t x;

    inline void releaseObject() {
        if ((x._physicalFlag & st_object) > 0) {
//            U_ASSERT(x.val._object != NULL);
            if (x.val._object->decRef() == 0) {
                delete x.val._object;
            };
        };
    };

    inline void acquireObject() {
        if ((x._physicalFlag & st_object) > 0) {
//            U_ASSERT(x.val._object != NULL);
            x.val._object->incRef();
        }
    };
public:
    const value_t & __value() const { return x; };
  
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
