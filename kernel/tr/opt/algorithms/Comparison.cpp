#include "Comparison.h"

#include "tr/executor/fo/op_map.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/nid/nidstring.h"

tuple_cell op_doc_order_lt (const tuple_cell &a1, const tuple_cell &a2, CollationHandler* handler)
{
    if (!a1.is_node() || !a2.is_node()) {
        U_ASSERT(false);
//        throw
    };
    
    return tuple_cell::atomic(nid_cmp_effective(a1.get_node(), a2.get_node()) < 0);
};

tuple_cell op_doc_order_gt (const tuple_cell &a1, const tuple_cell &a2, CollationHandler* handler)
{
    if (!a1.is_node() || !a2.is_node()) {
        U_ASSERT(false);
//        throw
    };

    return tuple_cell::atomic(nid_cmp_effective(a1.get_node(), a2.get_node()) > 0);
};

tuple_cell op_doc_order_ancestor(const tuple_cell& a1, const tuple_cell& a2, CollationHandler* handler)
{
    if (!a1.is_node() || !a2.is_node()) {
        U_ASSERT(false);
//        throw
    };

    return tuple_cell::atomic(nid_cmp_effective(a1.get_node(), a2.get_node()) == -2);
}

tuple_cell op_doc_order_descendant(const tuple_cell& a1, const tuple_cell& a2, CollationHandler* handler)
{
    if (!a1.is_node() || !a2.is_node()) {
        U_ASSERT(false);
//        throw
    };

    return tuple_cell::atomic(nid_cmp_effective(a1.get_node(), a2.get_node()) == +2);
}


inline
size_t write_cell(const tuple_cell& t, char * c) {
    char * tcbuf = c;
    uint32_t type = t.__type();

    memcpy(c, &type, sizeof(type));
    c += sizeof(type);

    if ((type & tc_light_atomic_var_size) == 0) {
        memcpy(c, &t.__data(), sizeof(tcdata));
        c += sizeof(tcdata);
    } else {
        size_t strlen = t.get_strlen();
        const char * str = t.get_str_mem();

        memcpy(c, &strlen, sizeof(size_t));
        c += sizeof(size_t);

        memcpy(c, str, strlen);
        c += strlen;
    };

    return c - tcbuf;
};

inline
size_t read_cell(tuple_cell& t, char * c) {
    char * tcbuf = c;
    uint32_t type;
    tcdata data;

    memcpy(&type, c, sizeof(type));
    c += sizeof(type);

    if ((type & tc_light_atomic_var_size) == 0) {
        memcpy(&data, c, sizeof(tcdata));
        c += sizeof(tcdata);

        t = tuple_cell(type, data);
    } else {
        size_t strlen;

        memcpy(&strlen, c, sizeof(size_t));
        c += sizeof(size_t);

        char * str = new char[strlen + 1];

        memcpy(str, c, strlen);
        c += strlen;
        c[strlen] = '\0';

        t = tuple_cell(type, str, 0, 0);
    };

    return c - tcbuf;
};

inline
size_t skip_cell(char * c) {
    char * tcbuf = c;
    uint32_t type;

    memcpy(&type, c, sizeof(type));
    c += sizeof(type);

    if ((type & tc_light_atomic_var_size) == 0) {
        c += sizeof(tcdata);
    } else {
        size_t strlen;
        memcpy(&strlen, c, sizeof(size_t));
        c += sizeof(size_t);
        c += strlen;
    };

    return c - tcbuf;
};

size_t GeneralCollationSerializer::serialize(const tuple& t, void* buf)
{
    // TODO: CHECK maximum buffer length
  
    char * c = (char *) buf;

    c += write_cell(atomize(t.cells[idx]), c);

    for (unsigned i = 0; i != t.size(); ++i) {
        c += write_cell(t.cells[i], c);
    };

    return c - (char *) buf;
}

int GeneralCollationSerializer::compare(void* buf1, size_t size1, void* buf2, size_t size2)
{
    tuple_cell t1, t2;

    read_cell(t1, (char *) buf1);
    read_cell(t2, (char *) buf2);

    if (op_lt(t1, t2, collation).get_xs_boolean()) {
        return -1;
    } else {
        return 1;
    };
}

void GeneralCollationSerializer::deserialize(tuple& t, void* buf, size_t size)
{
    char * c = (char *) buf;

    c += skip_cell(c);

    for (unsigned i = 0; i != t.size(); ++i) {
        c += read_cell(t.cells[i], c);
    };

    U_ASSERT(size == (c - (char *) buf));
}




size_t DocOrderSerializer::serialize(const tuple& t, void* buf)
{
    char * c = (char *) buf;

    U_ASSERT(t.cells[idx].is_node());

    Node node = t.cells[idx].get_node();
    size_t strlen = 0;
    
    if (!node.isNull()) {
        NidString nid(node.checkp().getPtr());

        strlen = nid.size();
        const unsigned char * str = nid.data();

        memcpy(c, &strlen, sizeof(size_t));
        c += sizeof(size_t);

        memcpy(c, str, strlen);
        c += strlen;
    } else {
        memcpy(c, &strlen, sizeof(size_t));
        c += sizeof(size_t);
    };

    for (unsigned i = 0; i != t.size(); ++i) {
        c += write_cell(t.cells[i], c);
    };

    return c - (char *) buf;
}

void DocOrderSerializer::deserialize(tuple& t, void* buf, size_t size)
{
    char * c = (char *) buf;
    size_t strlen = 0;
    memcpy(&strlen, c, sizeof(size_t));
    c += sizeof(size_t);
    c += strlen;
    
    for (unsigned i = 0; i != t.size(); ++i) {
        c += read_cell(t.cells[i], c);
    };

    U_ASSERT(size == (c - (char *) buf));
}

int DocOrderSerializer::compare(void* buf1, size_t size1, void* buf2, size_t size2)
{
    const unsigned char * s1 = (const unsigned char *) buf1;
    const unsigned char * s2 = (const unsigned char *) buf2;
  
    size_t strlen1 = 0;
    size_t strlen2 = 0;

    memcpy(&strlen1, s1, sizeof(size_t));
    s1 += sizeof(size_t);

    memcpy(&strlen2, s2, sizeof(size_t));
    s2 += sizeof(size_t);

    return memcmp(s1, s2, std::min(strlen1, strlen2));
}
