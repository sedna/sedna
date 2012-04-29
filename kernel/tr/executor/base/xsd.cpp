/*
 * File:  xsd.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "common/internstr.h"

#include "tr/executor/base/xsd.h"
#include "tr/structures/schema.h"
#include "tr/executor/base/PPBase.h"
#include "tr/executor/base/xs_names.h"
#include "tr/structures/nodeinterface.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/executor/xpath/SchemaTests.h"

using namespace xsd;

const char * xsd::QNameWildcard = "*";

/* Storage of strings, used as QName
  WARNING: name storage's main goal is to save char array even after qname is destroyed
*/

sedna::StringStorage nameStorage(128);

void xsd::clearQNameCache() {
    nameStorage.clear();
}

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

std::string Name::toLRString() const
{
    std::ostringstream oss(std::ios::out | std::ios::binary);
    toLR(oss);
    return oss.str();
}

const char * Name::serialize(void * parent)
{
    return valid() ? cat_strcpy(parent, name) : NULL;
}

NCName NCName::check(const char* name, bool quietly)
{
    if (check_constraints_for_xs_NCName(name)) {
        return NCName(nameStorage.intern(name));
    } else if (quietly) {
        return NCName();
    } else {
        throw XQUERY_EXCEPTION(FOCA0002);
    }
}

AnyURI AnyURI::check(const char* uri)
{
    return AnyURI(nameStorage.intern(uri));
}

QName::QName(xmlns_ptr ns, const char* aLocalName) : ns(ns), localName(NULL)
{
    U_ASSERT(aLocalName != NULL);
    localName = nameStorage.intern(aLocalName);
}

QName::QName(xmlns_ptr ns, const char* aLocalName, size_t len) : ns(ns), localName(NULL)
{
    U_ASSERT(aLocalName != NULL);
    localName = nameStorage.intern(aLocalName, len);
}

QName QName::createUnchecked(xmlns_ptr ns, const char* local)
{
    return QName(ns, local);
}

static
char * encodePtr(char * c, uintptr_t p) {
    uintptr_t a = ~ (uintptr_t) 0;
    uintptr_t b = ~ p;

    char * ca = c;
    memcpy(c, &a, sizeof(uintptr_t)); c += sizeof(uintptr_t);
    char * cb = c;
    memcpy(c, &b, sizeof(uintptr_t)); c += sizeof(uintptr_t);

    for (int i = 0; i < ((int) sizeof(uintptr_t)); i++, ca++, cb++) {
        if (cb == '\0') {
            * (uint8_t *) cb = 0xf0;
            * (uint8_t *) ca = 0x0f;
        }
    }

    return c;
}

static
uintptr_t decodePtr(const char * c) {
    uintptr_t a;
    uintptr_t b;

    memcpy(&a, c, sizeof(uintptr_t)); c += sizeof(uintptr_t);
    memcpy(&b, c, sizeof(uintptr_t));

    return (a ^ b);
}

char* QName::serializeTo(char* str) const
{
    U_ASSERT(sizeof(xmlns_ptr) == sizeof(uintptr_t));
    char * c = str;
    size_t localNameLen = strlen(localName);

    c = encodePtr(c, (uintptr_t) ns);

    memcpy(c, localName, localNameLen); c += localNameLen;

    *c = '\0';

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

    if (serializedForm == NULL) {
        return QName();
    }

    xmlns_ptr ns = (xmlns_ptr) decodePtr(serializedForm);

    return QName(ns, serializedForm + 2 * sizeof(uintptr_t));
}

std::string QName::getColonizedName() const
{
    if (!valid()) {
        return std::string();
    } else if (*getPrefix() != '\0') {
        return std::string(getPrefix()) + ":" + getLocalName();
    } else {
        return getLocalName();
    }
}

std::string QName::getColonizedName(NCName prefix, NCName local)
{
    if (!local.valid()) {
        return std::string();
    } else if (prefix.valid()) {
        return prefix.toString() + ":" + local.toString();
    } else {
        return local.toString();
    }
}

QName QName::createUPL(const char* uri, const char* prefix, const char* localName, bool quietly)
{
    xmlns_ptr ns;

    if (uri != NULL && *uri != '\0') {
        if (prefix != NULL && *prefix != '\0') {
            if (!NCName::check(prefix, quietly).valid()) {
                return QName();
            }
        }

        ns = xmlns_touch(prefix, uri);
    } else {
        ns = NULL_XMLNS;
    }

    return createNsN(ns, localName, quietly);
}

typedef std::pair<xsd::NCName, xsd::NCName> ColonizedName;

ColonizedName resolveColonizedName(const char* prefixAndLocal, bool quietly)
{
    ColonizedName result;
    const char * localName = strchr(prefixAndLocal, ':');

    if (localName == NULL) {
        result.first = NCName();
        result.second = NCName::check(prefixAndLocal, quietly);

        if (!result.second.valid()) {
            return ColonizedName(NCName(), NCName());
        }
    } else {
        size_t prefixLen = localName - prefixAndLocal;

        result.first = NCName::check(std::string(prefixAndLocal, prefixLen).c_str(), quietly);
        result.second = NCName::check(localName + 1, quietly);

        if (!result.first.valid() || !result.second.valid()) {
            return ColonizedName(NCName(), NCName());
        }
    }

    return result;
}

QName QName::createNsN(xmlns_ptr ns, const char* local, bool quietly)
{
    if (!NCName::check(local, quietly).valid()) {
        return QName();
    }

    if  (ns != NULL_XMLNS && ns->has_prefix() &&
          !NCName::check(ns->get_prefix(), quietly).valid()) {
        return QName();
    }

    return QName(ns, local);
}


QName QName::createUCn(const char* uri, const char* prefixAndLocal, bool quietly)
{
    ColonizedName cn = resolveColonizedName(prefixAndLocal, quietly);

    if (!cn.second.valid()) {
        return QName();
    }

    if (uri != NULL && *uri == '\0') { uri = NULL; }

    /* Prefix exists, but uri is NULL */
    if (cn.first.valid() && uri == NULL) {
        if (quietly) {
            return QName();
        } else {
            throw XQUERY_EXCEPTION2(FOCA0002, "Error in functions xs:QName");
        }
    }

    return QName(xmlns_touch(cn.first.getValue(), uri), cn.second.getValue());
}

QName QName::createResolve(const char* prefixAndLocal, INamespaceMap* namespaces, bool quietly)
{
    ColonizedName cn = resolveColonizedName(prefixAndLocal, quietly);

    if (!cn.second.valid()) {
        return QName();
    }

    if (!cn.first.valid()) {
        return QName(namespaces->getDefaultNamespace(), cn.second.getValue());
    } else {
        xmlns_ptr ns;
        /* TRICKY: THAT IS DAMN XQUERY.
          We should check for xmlns prefix before resolving it, it's kinda hack.
          The worst thing is that there are different errors for attrbutes and for elements  */

        if (strcmp(cn.first.getValue(), "xmlns") == 0) {
            ns = xmlns_touch("xmlns", "http://www.w3.org/2000/xmlns/");
        } else {
            ns = namespaces->resolvePrefix(cn.first.getValue());
        }

        /* Name not found */

        if (ns == NULL_XMLNS) {
            if (quietly) {
                return QName();
            } else {
                throw XQUERY_EXCEPTION2(FONS0004, "Error in functions fn:resolve-QName");
            }
        }

        return QName(ns, cn.second.getValue());
    }
}

#define IS_WILDCARD(x) ((x) == NULL || (x) == QNameWildcard || strcmp((x), QNameWildcard) == 0)

TemplateQName::TemplateQName(const char* _uri, const char* _localName)
  : uri(NULL), localName(NULL)
{
    if (IS_WILDCARD(_uri)) {
        uri = QNameWildcard;
    } else {
        uri = nameStorage.intern(_uri);
    };

    if (IS_WILDCARD(_localName) || _localName[0] == '\0') {
        localName = QNameWildcard;
    } else {
        localName = nameStorage.intern(_localName);
    };
}

std::string TemplateQName::getColonizedName() const
{
    std::stringstream ss;

    if (uri == QNameWildcard) {
      ss << QNameWildcard;
    } else {
      ss << "{" << uri << "}";
    }

    ss << ":" << localName;
    return ss.str();
}

std::string TemplateQName::getXPathName() const
{
    std::stringstream ss;

    ss << "(";
    
    if (uri == QNameWildcard) {
      ss << QNameWildcard << ":";
    } else if (uri[0] == '\0') {
    } else {
      ss << "{" << uri << "}" << ":";
    }

    ss << localName << ")";

    return ss.str();
}

SchemaTestData* TemplateQName::getTestData(SchemaTestData* _td) const
{
    _td->m_uri = uri;
    _td->m_local = localName;
    return _td;
}


QName QName::bulkloadParse(const char* triplet)
{
    const char * c = triplet;

    c = strchr(c, '>');

    if (c == NULL) {
        return QName(NULL_XMLNS, triplet);
    }

    const char * uri = triplet;
    size_t uri_len = c - uri;
    const char * name = c + 1;
    c = strchr(name, '>');

    if (c == NULL) {
        return xsd::QName(xmlns_touch_len(NULL, uri, uri_len), name);
    }

    size_t name_len = c - name;
    const char * ns = c + 1;

    return xsd::QName(xmlns_touch_len(ns, uri, uri_len), name, name_len);
}

/* QName is stored as (qname PREFIX URI LOCAL_NAME) */

void QName::toLR(std::ostream& os, const xsd::AnyURI uri, const xsd::NCName prefix, const xsd::NCName local)
{
    os << "(qname ";
    prefix.toLR(os);
    os << " ";
    uri.toLR(os);
    os << " ";
    local.toLR(os);
    os << ")";
}

QName QName::fromLR(scheme_list* lst)
{
    xmlns_ptr ns = NULL_XMLNS;
    char * local = NULL;

    if (lst->size() != 4 ||
          lst->at(0).type != SCM_SYMBOL ||
          lst->at(1).type != SCM_STRING ||
          lst->at(2).type != SCM_STRING ||
          lst->at(3).type != SCM_STRING ) {
        throw USER_EXCEPTION2(SE1004, "Path expression");
    }

    U_ASSERT(strcmpex(lst->at(0).internal.symb, "qname") == 0);

    if (*(lst->at(1).internal.str) != '\0' || *(lst->at(2).internal.str) != '\0') {
        ns = xmlns_touch(lst->at(1).internal.str, lst->at(2).internal.str);
    }

    local = lst->at(3).internal.str;

    U_ASSERT((QName::createNsN(ns, local).valid()));

    return QName(ns, local);
}

std::string QName::toLRString() const
{
    std::ostringstream oss(std::ios::out | std::ios::binary);
    toLR(oss);
    return oss.str();
}

void QName::toLR(std::ostream& os) const
{
    QName::toLR(os, getUri(), getPrefix(), getLocalName());
}
