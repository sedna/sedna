/*
 * File:  xsd.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/base/xsd.h"
#include "tr/structures/schema.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/xs_names.h"
#include "tr/structures/nodeinterface.h"
#include "tr/executor/base/dm_accessors.h"


using namespace xsd;

void Name::toLR(std::ostream& os) const
{
    os << "\"";
    if (NULL != name) { os << name; }
    os << "\"";
}

std::string Name::toString() const
{
    if (name != NULL && *name) {
        return name;
    } else {
        return std::string();
    }
}

void Name::own() {
    if (!ownsName && name != NULL) {
        char * newname = (char *) malloc(strlen(name) + 1);
        strcpy(newname, name);
        name = newname;
    }
}

const char * Name::serialize(void * parent)
{
    return valid() ? cat_strcpy(parent, name) : NULL;
}

NCName NCName::check(const char* name, bool quietly)
{
    if (check_constraints_for_xs_NCName(name)) {
        return NCName(name);
    } else if (!quietly) {
        throw XQUERY_EXCEPTION(XQDY0074);
    } else {
        return NCName();
    }
}

AnyURI AnyURI::check(const char* uri, bool quietly)
{
    return AnyURI(uri);
}

QName::QName() : ns(NULL_XMLNS), localName(NULL) { }

QName::QName(xmlns_ptr ns, const char* aLocalName) : ns(ns), localName(NULL)
{
    localName = (char *) malloc(strlen(aLocalName) + 1);
    strcpy(localName.get(), aLocalName);
}

QName::QName(xmlns_ptr ns, const char* aLocalName, size_t len) : ns(ns), localName(NULL)
{
    localName = (char *) malloc(len + 1);
    strncpy(localName.get(), aLocalName, len);
    localName[len] = 0;
}


char* QName::serializeTo(char* str) const
{
    U_ASSERT(sizeof(xmlns_ptr) == sizeof(uintptr_t));

    size_t localNameLen = strlen(localName.get());

    uintptr_t a = ~ (uintptr_t) 0;
    uintptr_t b = ~ (uintptr_t) ns;

    memcpy(str, &a, sizeof(uintptr_t)); str += sizeof(uintptr_t);
    char * c = str;
    memcpy(str, &b, sizeof(uintptr_t)); str += sizeof(uintptr_t);
    memcpy(str, localName.get(), localNameLen); str += localNameLen;
    *str = '\0';

    for (int i = 0; i < ((int) sizeof(uintptr_t)); i++) {
        if (c[i] == '\0') {
            * (uint8_t *) (c + i) = 0xf0;
            * (uint8_t *) (c + i - sizeof(uintptr_t)) = 0x0f;
        }
    }

    return str;
}


const char* QName::serialize(void* parent) const
{
    if (!valid()) { return NULL; }
    char * str = (char *) cat_malloc(parent, getLen());
    return serializeTo(str);
}

QName QName::deserialize(const char* serializedForm)
{
    U_ASSERT(sizeof(xmlns_ptr) == sizeof(uintptr_t));

    size_t len = strlen(serializedForm);
    uintptr_t a, b;
    xmlns_ptr ns;

    memcpy(&a, serializedForm, sizeof(uintptr_t));
    memcpy(&b, serializedForm + sizeof(uintptr_t), sizeof(uintptr_t));

    ns = (xmlns_ptr) (a ^ b);

    return QName(ns, serializedForm + 2 * sizeof(uintptr_t));
}

std::string QName::getColonizedName() const
{
    if (!valid()) {
        return std::string();
    } else {
        return std::string(getPrefix()) + ":" + getLocalName();
    }
}

std::string QName::getColonizedName(NCName prefix, NCName local)
{
    if (!local.valid()) {
        return std::string();
    } else {
        return prefix.toString() + ":" + local.toString();
    }
}

QName QName::createUL(const char* uri, const char* localName, bool quietly)
{
    xmlns_ptr ns;

    if (uri != NULL && *uri != '\0') {
        ns = xmlns_touch("", uri);
    } else {
        ns = NULL_XMLNS;
    }

    if (!check_constraints_for_xs_NCName(localName)) {
        if (quietly) { return QName(); } else {
            throw XQUERY_EXCEPTION2(XPTY0004, "Error in functions xs:QName");
        }
    }

    return QName(ns, localName);
}


QName QName::createUPL(const char* uri, const char* prefix, const char* localName, bool quietly)
{
    xmlns_ptr ns;

    if (uri != NULL && *uri != '\0') {
        if (prefix != NULL && (*prefix != '\0') && !check_constraints_for_xs_NCName(prefix)) {
            if (quietly) { return QName(); } else {
                throw XQUERY_EXCEPTION2(XPTY0004, "Error in functions xs:QName");
            }
        }

        ns = xmlns_touch(prefix, uri);
    } else {
        ns = NULL_XMLNS;
    }

    if (!check_constraints_for_xs_NCName(localName)) {
        if (quietly) { return QName(); } else {
            throw XQUERY_EXCEPTION2(XPTY0004, "Error in functions xs:QName");
        }
    }

    return QName(ns, localName);
}

QName QName::createNsCn(xmlns_ptr ns, const char* prefixAndLocal, bool quietly)
{
    const char * localName = strchr(prefixAndLocal, ':') + 1;


    if ((ns == NULL_XMLNS && localName != NULL) || (ns != NULL_XMLNS && localName == NULL)) // Logical XOR =)
    {
        if (quietly) { return QName(); } else {
            throw XQUERY_EXCEPTION2(XPTY0004, "Error in functions xs:QName");
        }
    }

    if (ns != NULL_XMLNS && strncmp(ns->get_prefix(), prefixAndLocal, strlen(prefixAndLocal)) != 0) {
        if (quietly) { return QName(); } else {
            throw XQUERY_EXCEPTION2(XPTY0004, "Error in functions xs:QName");
        }
    }

    return QName(ns, localName);
}

QName QName::createNsN(xmlns_ptr ns, const char* local, bool quietly)
{
    if (!quietly) {
        if (!check_constraints_for_xs_NCName(local)) {
            throw XQUERY_EXCEPTION(XQDY0074);
        }

        if  (ns != NULL_XMLNS && ns->has_prefix()) {
            if (strcmpex(ns->get_prefix(), "xmlns") == 0) {
                throw XQUERY_EXCEPTION(XQDY0096);
            }

            if(!check_constraints_for_xs_NCName(ns->get_prefix())) {
                throw XQUERY_EXCEPTION(XQDY0074);
            }
        }
    }

    return QName(ns, local);
}


QName QName::createUCn(const char* uri, const char* prefixAndLocal, bool quietly)
{
    const char * localName = strchr(prefixAndLocal, ':');

    if (uri == NULL || *uri == '\0') {
        if (localName != NULL) {
            if (quietly) { return QName(); } else {
                throw XQUERY_EXCEPTION2(XPTY0004, "Error in functions xs:QName");
            }
        }

        return QName(NULL_XMLNS, prefixAndLocal);
    } else if (localName == NULL) {
        return QName(xmlns_touch("", uri), prefixAndLocal);
    } else {
        size_t prefixLen = localName - prefixAndLocal;
        std::string prefix(prefixAndLocal, prefixLen);

        return createUPL(uri, prefix.c_str(), localName + 1);
    }
}

// TODO : reimplement

static inline
xmlns_ptr se_resolve_prefix(const char * prefix, Node node, dynamic_context* cxt)
{
    std::vector<xmlns_ptr> xmlns;
    se_get_in_scope_namespaces(node, xmlns, cxt);

    for (size_t i = 0; i < xmlns.size(); i++) {
        if (strcmp(xmlns[i]->prefix, prefix) == 0) {
            return xmlns[i];
        }
    }

    return NULL_XMLNS;
}



QName QName::createResolveContext(const char* prefixAndLocal, dynamic_context* cxt, bool quietly)
{
    const char * localName = strchr(prefixAndLocal, ':');

    if (localName == NULL) {
        return QName::createNsN(cxt->get_default_namespace(), prefixAndLocal, quietly);
    } else {
        size_t prefixLen = localName - prefixAndLocal;
        std::string prefix(prefixAndLocal, prefixLen);

        xmlns_ptr ns = cxt->get_xmlns_by_prefix(prefix.c_str());

        if (ns == NULL_XMLNS) {
            if (quietly) { return QName(); } else {
                throw XQUERY_EXCEPTION2(FONS0004, "Error in functions fn:resolve-QName");
            }
        }

        return QName::createNsN(ns, localName + 1, quietly);
    }
}

QName QName::createResolveNode(const char* prefixAndLocal, xptr node, dynamic_context* cxt, bool quietly)
{
    const char * localName = strchr(prefixAndLocal, ':');

    if (localName == NULL) {
        return QName(cxt->get_default_namespace() , prefixAndLocal);
    } else {
        size_t prefixLen = localName - prefixAndLocal;
        std::string prefix(prefixAndLocal, prefixLen);

        xmlns_ptr ns = se_resolve_prefix(prefix.c_str(), node, cxt);

        if (ns == NULL_XMLNS) {
            if (quietly) { return QName(); } else {
                throw XQUERY_EXCEPTION2(FONS0004, "Error in functions fn:resolve-QName");
            }
        }

        return QName(ns, localName + 1);
    }
}


void QName::toLR(std::ostream& os, const xsd::AnyURI uri, const xsd::NCName prefix, const xsd::NCName local)
{
    uri.toLR(os);
    os << " ";
    local.toLR(os);
    os << " ";
    prefix.toLR(os);
}


void QName::toLR(std::ostream& os) const
{
    QName::toLR(os, getUri(), getPrefix(), getLocalName());
}
