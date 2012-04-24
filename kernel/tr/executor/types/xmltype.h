#ifndef _XML_TYPE_H_
#define _XML_TYPE_H_

#include "common/u/u.h"
#include "common/xptr.h"

enum storage_type_t {
    st_node = 0x0001,
    st_xptr = 0x0002,
    st_string = 0x0004,
    st_pointer = 0x0008,
    st_object = 0x0010,
    st_node = 0x0020,
    st_atomic = 0x0040,
    st_in_sequence = 0x0080,
    st_external = 0x0100,
};

class IXmlObjectDisposable {
private:
    int referenceCount;
public:
    IXmlTypeWrapper() : referenceCount(0) {};
    virtual ~IXmlTypeWrapper() {};

    inline int decRef() { return --referenceCount; };
    inline int incRef() { return ++referenceCount; };
};

// Total size should be equal to 4+4+8 = 16

union variant_t {
    void * _ptr;
    int64_t _int;
    uint64_t _uint;
    xptr _xptr;
    IXmlObjectDisposable * _object;
};

struct xml_type_t {
    uint32_t _type;
    uint32_t _xmlType;

    variant_t val;
};

class XmlType {
    xml_type_t x;

    inline void releaseObject() {
        if ((x._type & st_object) > 0) {
            U_ASSERT(x.val._object != NULL);
            if (x.val._object->decRef() == 0) {
                delete x.val._object;
            };
        };
    };

    inline void acquireObject() {
        if ((x._type & st_object) > 0) {
            U_ASSERT(x.val._object != NULL);
            x.val._object->incRef();
        }
    };
public:
    XmlType() {};

    explicit XmlType(xml_type_t _x) : x(_x.x) {
        acquireObject();
    };
    
    XmlType(const XmlType& _x) : x(_x.x) {
        acquireObject();
    };

    ~XmlType() {
        releaseObject();
    };

    XmlType& operator=(const XmlType& _x) {
        if (&_x != this) {
            releaseObject();
            x = _x.x;
            acquireObject();
        };

        return *this;
    };
};

#endif /* _XML_TYPE_H_ */
