/*
 * File:  xsd.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __XSD_H
#define __XSD_H

#include <iostream>
#include <string>

#include "common/sedna.h"
#include "common/xptr.h"

#include "tr/structures/xmlns.h"

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
 *
 * Serialize returns the serialized form of NCName within the memory context, defined by parent.
 * If no memory context defined, uses malloc.
 */

typedef counted_ptr<char, de_free<char> > counted_cstr;

class Name {
  private:
    char * name;
  public:
    ~Name() {};
    /* deserializes name */
    Name(const char * value) : name(value) { };
    /* creates new name */
    Name(const char * value, void * memory_parent);

    Name(const Name & from) { this->name = from.name; };

    Name & operator=(Name & to);

    void toLR(std::ostream& os) const;
    std::string toString() const;
    char const * serialize(void * parent);

    inline const char * getValue() const { return name.get(); };
    inline bool valid() const { return name.get() != NULL; };
};

/// XML Schema Part 2 NCName

class NCName : public Name {
  public:
    static void * defaultContext;

    NCName(const char * value) : Name(value) {};
//    NCName(const char * value, void * memory_parent) : Name(value) {};
    NCName(const NCName & from) : Name(from) {};

    static NCName checkQuietly(const char * name);
    static NCName check(const char * name);
};

/// XML Schema Part 2 anyURI

class AnyURI : public Name {
  public:
    static void * defaultContext;

    AnyURI(const char * value) : Name(value) {};
    AnyURI(const AnyURI & from) : Name(from) {};

    static AnyURI checkQuietly(const char * uri);
    static AnyURI check(const char * uri);
};

/// XML Schema Part 2 QName (qualified name from Namespaces in XML standard)

class QName {
  private:
    xmlns_ptr ns;
    counted_cstr localName;

    QName(); // invalid!
    QName(xmlns_ptr ns, const char * localName);
    QName(xmlns_ptr ns, const char * localName, size_t len);
  public:
    static void * defaultContext;

    ~QName() {};
    QName(const QName & from) { this->ns = from.ns; this->localName = from.localName; }

    void toLR(std::ostream& os) const;
    const char * serialize(void * parent) const;
    std::string getColonizedName() const;

    inline xmlns_ptr getXmlNs() const { return ns; };
    inline const char * getUri() const { return ns == NULL_XMLNS ? "" : ns->get_uri(); };
    inline const char * getPrefix() const { return ns == NULL_XMLNS ? "" : ns->get_prefix(); };
    inline const char * getLocalName() const { return localName.get(); };

    inline bool emptyUri() const { return ns == NULL_XMLNS || ns->empty_uri(); };

    inline bool valid() const { return localName.get() != NULL; };

    inline bool equals(const QName & to) const {
        return same_xmlns_uri(ns, to.ns) && strcmp(localName.get(), to.localName.get());
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

        cmp = strcmp(localName.get(), against.localName.get());

        return cmp < 0;
    };

    static QName deserialize(const char* serializedForm);
    static QName createNsCn(xmlns_ptr ns, const char * prefixAndLocal, bool quietly = false);
    static QName createUCn(const char * uri, const char * prefixAndLocal, bool quietly = false);
    static QName createUPL(const char * uri, const char * prefix, const char * localName, bool quietly = false);
    static QName createResolveCn(const char * prefixAndLocal, xptr node, dynamic_context *cxt, bool quietly = false);
};

}

/*

// Separates prefix and local name from the QName in text
// representaion: prefix:local. Allocates memory for prefix with malloc.
// Changes qname to point to the local name start.
// void separateLocalAndPrefix(char*& prefix, const char*& qname);
*/

#endif /* __XSD_H */
