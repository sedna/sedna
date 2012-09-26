/*
 * File:  xsd.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __XSD_H
#define __XSD_H

#include <iostream>
#include <string>

#include "common/sedna.h"
#include "common/xptr/xptr.h"

#include "tr/structures/xmlns.h"
#include "tr/executor/por2qep/scheme_tree.h"

struct SchemaTestData;
class INamespaceMap;
class dynamic_context;

/**
XML Schema Part 2 Datatypes to C++ Types Mapping
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xs:string               variable            char*
xs:boolean              1                   bool
xs:decimal              8                   xs_decimal
xs:float                4                   float
xs:double               8                   double
xs:duration
xs:dateTime
xs:time
xs:date
xs:gYearMonth
xs:gYear
xs:gMonthDay
xs:gDay
xs:gMonth
xs:hexBinary            variable            char* or xs_hexBinary
xs:base64Binary         variable            char* or xs_base64Binary

xs:integer              8                   int64_t
xs:nonPositiveInteger   8                   int64_t
xs:negativeInteger      8                   int64_t
xs:long                 8                   int64_t
xs:int                  4                   int32_t
xs:short                2                   int16_t
xs:byte                 1                   int8_t
xs:nonNegativeInteger   8                   int64_t
xs:unsignedLong         8                   uint64_t
xs:unsignedInt          4                   uint32_t
xs:unsignedShort        2                   uint16_t
xs:unsignedByte         1                   uint8_t
xs:positiveInteger      8                   uint64_t
**/


/*
  XML Schema Datatypes
*/

namespace xsd {

/*
 * Serialize returns the serialized form of NCName within the memory context, defined by parent.
 * If no memory context defined, uses malloc.
 */

typedef counted_ptr<char, de_free<char> > counted_cstr;

/* Frees internal strings for storing localnames */
void clearQNameCache();

class Name {
  private:
    const char * name;
  public:
    Name() : name(NULL) {};

    /* deserializes name */
    Name(const char * value) : name(value) { };
    Name(const Name & from) { this->name = from.name; };

    Name & operator=(const Name & to) {
        this->name = to.name;
        return *this;
    };

    void toLR(std::ostream& os) const;
    std::string toLRString() const;
    std::string toString() const;
    const char * serialize(void * parent);

    inline const char * getValue() const { return name; };
    inline bool valid() const { return name != NULL; };
};

class NCName : public Name {
  public:
    NCName() : Name() {};
    NCName(const char * value) : Name(value) {};
    NCName(const NCName & from) : Name(from) {};

    static NCName check(const char * name, bool quietly);
};

inline static char * materialize(const char * in) { return cat_strcpy(default_context_space, in); };

class AnyURI : public Name {
  private:
    AnyURI() : Name() {};
  public:
    AnyURI(const char * value) : Name(value) {};
    AnyURI(const AnyURI & from) : Name(from) {};

    static AnyURI check(const char * uri);
};

/// XML Schema Part 2 QName (qualified name from Namespaces in XML standard)

struct const_qname_t {
    xmlns_ptr ns;
    const char * localName;
};

class QName {
  private:
    xmlns_ptr ns;
    const char * localName; /* This always should be internal saved string, we need not to free it or allocate it. */

    QName(xmlns_ptr ns, const char * localName);
    QName(xmlns_ptr ns, const char * localName, size_t len);
  public:
    QName() : ns(NULL_XMLNS), localName(NULL) { }; // invalid!
    QName(const QName & from) : ns(from.ns), localName(from.localName) {};

    explicit QName(const const_qname_t & x) : ns(x.ns), localName(x.localName) {};
    const_qname_t toConst() { const_qname_t result = {ns, localName}; return result; };

    static void toLR(std::ostream& os, const AnyURI uri, const NCName prefix, const NCName local);
    void toLR(std::ostream& os) const;
    std::string toLRString() const;

    /* Serializes qname, allocates memory cat_malloc */
    const char * serialize(void * parent) const;
    char * serializeTo(char * tmp) const;

    size_t getLen() const { return strlen(localName) + 2 * sizeof(uintptr_t) + 1; };

    std::string getColonizedName() const;
    static std::string getColonizedName(NCName prefix, NCName local);

    inline xmlns_ptr getXmlNs() const { return ns; };

    inline const char * getUri() const { return ns == NULL_XMLNS ? NULL : ns->get_uri(); };
    inline const char * getPrefix() const { return ns == NULL_XMLNS ? "" : ns->get_prefix(); };
    inline const char * getLocalName() const { return localName; };

    inline bool emptyUri() const { return ns == NULL_XMLNS || ns->empty_uri(); };

    inline bool valid() const { return localName != NULL; };

    inline bool equals(const QName & to) const {
        /* We can compare just pointers to strings since they are internalized */
        return same_xmlns_uri(ns, to.ns) && (localName == to.localName);
    };

    /* operators are overloaded to store class in set */

    inline bool operator==(const QName &against) const { return equals(against); };

    inline bool operator<(const QName &against) const {
        int cmp;

        // This comparison should at first check validity, then
        // compare by ns (if they exists) then by localname

        cmp = (valid() ? 1 : 0) - (against.valid() ? 1 : 0);

        if (cmp != 0 || !valid()) {
          // If both qnames are invalid, return false (we consider them equal in this case)
            return cmp < 0 || valid();
        }

        cmp = (emptyUri() ? 1 : 0) - (against.emptyUri() ? 1 : 0);

        if (cmp != 0) {
            return cmp < 0;
        }

        // QNames ns's are either both null or not
        if (!emptyUri()) {
            cmp = strcmp(getUri(), against.getUri());

            if (cmp != 0) {
                return cmp < 0;
            }
        }

        U_ASSERT(valid() && against.valid()); // to make sure localnames are not NULL

        cmp = strcmp(localName, against.localName);

        return cmp < 0;
    };

    static QName deserialize(const char* serializedForm);

    static QName fromLR(scheme_list* lst);

// TODO: replace all exceptions with std::exception
    static QName createResolve(const char * prefixAndLocal, INamespaceMap * namespaces, bool quietly = false);
    static QName resolve(const char* prefix, const char* local, INamespaceMap* namespaces);

    static QName createNsN(xmlns_ptr ns, const char * local, bool quietly = false);
    static QName createUCn(const char * uri, const char * prefixAndLocal, bool quietly = false);
    static QName createUPL(const char * uri, const char * prefix, const char * localName, bool quietly = false);
    static QName createUnchecked(xmlns_ptr ns, const char * local);

    static QName bulkloadParse(const char * triplet);

    static inline QName getConstantQName(xmlns_ptr ns, const char * name) { return QName(ns, name); };

    struct FastCompare {
        /* The idea of this comparison: since localName is always internated (or NULL),
          their pointers are somehow duplicate-safe ordered, it is valid for namespaces too */

        bool operator()(const QName &qname1, const QName &qname2) const {
            if (qname1.ns == qname2.ns) {
                return (qname1.localName - qname2.localName) > 0;
            } else {
                return (qname1.ns - qname2.ns) > 0;
            }
        }
    };
};

inline
QName constQName(xmlns_ptr ns, const char * local)
{
    return xsd::QName::getConstantQName(ns, local);
};

extern const char * QNameWildcard;

class TemplateQName {
  private:
    const char * uri;
    const char * localName;

  public:
    TemplateQName() : uri(NULL), localName(NULL) {}; // invalid;
    TemplateQName(const TemplateQName & _from) : uri(_from.uri), localName(_from.localName) {};

    TemplateQName(const char * _uri, const char * localName);

    inline const char * getUri() const { return uri; };
    inline const char * getLocalName() const { return localName; };

    std::string getColonizedName() const;
    std::string getXPathName() const;

    QName toQName(INamespaceMap * namespaces) const;

    SchemaTestData * getTestData(SchemaTestData *) const;

    struct __const_wildcard {};
    explicit TemplateQName(const __const_wildcard & cwldr) : uri(QNameWildcard), localName(QNameWildcard) {}; // invalid;
};

static const TemplateQName QNameAny = TemplateQName(TemplateQName::__const_wildcard());

}

#endif /* __XSD_H */
