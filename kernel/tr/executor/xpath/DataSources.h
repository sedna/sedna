#ifndef _DATA_SOURCES_H_
#define _DATA_SOURCES_H_

#include <stdint.h>

#include "common/sedna.h"
#include "tr/executor/xpath/XPathTypes.h"

class DataRoot {
public:
    enum data_root_type_t {
        drt_null,
        drt_document,
        drt_collection,
        drt_external,
    };
private:
    data_root_type_t type;
    counted_ptr<std::string> name;
public:
    DataRoot() : type(drt_null), name(NULL) {};
    DataRoot(data_root_type_t type, const char * name);
    DataRoot(const counted_ptr<db_entity> dbe);
    DataRoot(const scheme_list * x);

    counted_ptr<db_entity> toDBEntity() const;

    DataRoot(const DataRoot & x) : type(x.type), name(x.name) {};
    const DataRoot& operator=(const DataRoot& x) {
        if (&x != this) {
            type = x.type;
            name = x.name;
        }
        return *this;
    }

    std::string toLRString() const;
};

#endif /* _DATA_SOURCES_H_ */
