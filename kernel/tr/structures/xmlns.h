/*
 * File:  xmlns.h
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef XMLNS_H_
#define XMLNS_H_

#include "common/sedna.h"
#include "common/base.h"

#include "tr/cat/catalog.h"
#include "tr/cat/catptr.h"

/******************************
 * Namespace handling routines
 */

struct xmlns_local_object {
public:
    char* prefix; /* persistent string */
    char* uri; /* persistent string */

    inline bool same_prefix(const char * _prefix) const { return strcmpex(prefix, _prefix) == 0; };
    inline bool same_uri(const char * _uri) const { return strcmpex(uri, _uri) == 0; };

    inline const char * get_uri() const { return uri; }
    inline const char * get_prefix() const { return prefix; }
    inline bool has_prefix() const { return prefix != NULL && prefix[0] != '\0'; }
    inline bool empty_uri() const { return uri == NULL || uri[0] == '\0'; }
    inline bool is_reserved_prefix() const { return prefix != NULL && ((strcmp(prefix, "xml") == 0) || (strcmp(prefix, "xmlns") == 0)); }
};

struct xmlns_indb_object : public catalog_object {
    char* prefix; /* persistent string */
    char* uri; /* persistent string */

    xptr root; /* where to save this object in catalog */
    xmlns_ptr_pers next_xmlns; /* xmlns list */

/* Common catalog object interface */

    static const int magic = 0x008;
    int get_magic() { return magic; };
    void serialize_data(se_simplestream &stream);
    void deserialize_data(se_simplestream &stream);
    void drop();

    inline xmlns_indb_object() :
        prefix(NULL), uri(NULL), root(XNULL), next_xmlns(XNULL) {};
    inline xmlns_indb_object(const char* _prefix, const char* _uri, const xptr _root, const xmlns_ptr_pers _next_xmlns) :
        prefix(NULL), uri(NULL), root(_root), next_xmlns(_next_xmlns) {
        prefix = cat_strcpy(this, _prefix);
        uri = cat_strcpy(this, _uri);
    };
    static catalog_object_header * create(const char* prefix, const char* uri, const xptr root, const xmlns_ptr_pers next_xmlns);
};


typedef xmlns_local_object * xmlns_ptr;

static const xmlns_ptr NULL_XMLNS = (xmlns_ptr) NULL;

void free_xmlns_hash();

xmlns_ptr xmlns_touch(const char * prefix, const char * uri);

inline
xmlns_ptr xmlns_touch(xmlns_ptr_pers xmlns) {
    catalog_cptr_template<xmlns_indb_object> a = xmlns;
    return xmlns_touch(a->prefix, a->uri);
}



/*
 * Returns true for <a xmlns=""/> case.
 * The attribute value in a default namespace declaration MAY be empty.
 * This has the same effect, within the scope of the declaration,
 * of there being no default namespace
 * Returns false for NULL_XMLNS.
 */

inline
bool same_xmlns_uri(xmlns_ptr ns1, xmlns_ptr ns2) {
    if (ns1 == NULL_XMLNS && ns2 == NULL_XMLNS) {
        return true;
    }

    if (ns1 == NULL_XMLNS || ns2 == NULL_XMLNS) {
        return false;
    }

    return strcmpex(ns1->uri, ns2->uri) == 0;
}

inline
bool same_xmlns_uri(xmlns_ptr ns1, const char * uri) {
    if (ns1 == NULL_XMLNS && uri == NULL) {
        return true;
    }

    if (ns1 == NULL_XMLNS || uri == NULL) {
        return false;
    }

    return strcmpex(ns1->uri, uri) == 0;
}

inline
bool is_empty_ns_declaration(xmlns_ptr ns)
{
    U_ASSERT(ns == NULL_XMLNS || (ns->prefix != NULL && ns->uri != NULL));
    return ns != NULL_XMLNS && strlen(ns->prefix)==0 && strlen(ns->uri)==0;
}

class dynamic_context;

xmlns_ptr generate_prefix(int ctr, const char* uri, dynamic_context *cxt);
xmlns_ptr generate_prefix(const char * prefix, const char * uri);


#endif /* XMLNS_H_ */
