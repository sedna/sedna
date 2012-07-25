#ifndef _DATA_SOURCES_H_
#define _DATA_SOURCES_H_

#include <stdint.h>

#include "common/sedna.h"
#include "tr/opt/path/XPathTypes.h"

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
    mutable schema_node_xptr snode;

    schema_node_xptr _getSchemaNode() const;
public:
    DataRoot() : type(drt_null), name(NULL), snode(XNULL) {};
    DataRoot(data_root_type_t type, const char * name);
    DataRoot(const scheme_list * x);

    inline bool empty() const { return type == drt_null; };
    
    inline schema_node_cptr getSchemaNode() const {
        if (snode == XNULL) {
            snode = _getSchemaNode();
        };

        return snode;
    };

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
