/*
 * File:  xsd.h
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef __XS_H
#define __XS_H

#include "common/sedna.h"

#include <iostream>
#include "common/xptr.h"

class dynamic_context;

/**
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
XML Schema Part 2 Datatypes to C++ Types Mapping
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~





xs:string               variable            char*
xs:boolean              1                   bool
xs:decimal              8                   xs_decimal           should be fixed to have length of 12
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

xs:integer              8                   __int64
xs:nonPositiveInteger   8                   __int64
xs:negativeInteger      8                   __int64
xs:long                 8                   __int64
xs:int                  4                   __int32
xs:short                2                   __int16
xs:byte                 1                   __int8
xs:nonNegativeInteger   8                   __int64
xs:unsignedLong         8                   __uint64
xs:unsignedInt          4                   __uint32
xs:unsignedShort        2                   __uint16
xs:unsignedByte         1                   __uint8
xs:positiveInteger      8                   __uint64

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

*/

///
/// XML Schema Part 2 NCName Functions
///
/// xs_NCName is a string, that could be created in main memory or persistent heap (PH).
/// So we have to provide facilities for doing this. Moreover, we could not just add
/// 'bool persistent' parameter to functions because while constructing XPath expressions
/// we should aware of memory being allocated. For this purpose we provide addition 
/// functions like 'PathExpr_pers_malloc' and 'PathExpr_malloc' that we could pass as 
//// paramters to xs_NCName_create.
char *xs_NCName_create(const char* value, void* (*alloc_func)(size_t));
void  xs_NCName_release(char *ncname, void (*free_func)(void*));
void  xs_NCName_print(const char *ncname, std::ostream& str);
void  xs_NCName_print_to_lr(const char *ncname, std::ostream& str);



///
/// XML Schema Part 2 anyURI Functions
///
/// xs_anyURI is a string, that could be created in main memory or persistent heap (PH).
/// So we have to provide facilities for doing this. Moreover, we could not just add
/// 'bool persistent' parameter to functions because while constructing XPath expressions
/// we should aware of memory being allocated. For this purpose we provide addition 
/// functions like 'PathExpr_pers_malloc' and 'PathExpr_malloc' that we could pass as 
//// paramters to xs_anyURI_create.
char *xs_anyURI_create(const char* value, void* (*alloc_func)(size_t));
void  xs_anyURI_release(char *uri, void (*free_func)(void*));
void  xs_anyURI_print(const char *uri, std::ostream& str);
void  xs_anyURI_print_to_lr(const char *uri, std::ostream& str);




struct xml_ns;

///
/// XML Schema Part 2 QName (qualified name from Namespaces in XML standard) Functions
///
/// xs_QName is a string, that could be created in main memory or persistent heap (PH).
/// So we have to provide facilities for doing this. Moreover, we could not just add
/// 'bool persistent' parameter to functions because while constructing XPath expressions
/// we should aware of memory being allocated. For this purpose we provide addition 
/// functions like 'PathExpr_pers_malloc' and 'PathExpr_malloc' that we could pass as 
/// paramters to xs_QName_create.
// when xmlns is known use this function
char *xs_QName_create(xml_ns* xmlns,
                      const char *local_part, 
                      void* (*alloc_func)(size_t));
// backs up xs:QName() function
char *xs_QName_create(const char *uri,
                      const char *prefix,
                      const char *local,
                      void* (*alloc_func)(size_t),
                      dynamic_context *cxt);
// backs up fn:QName() function (prefix_and_local constains prefix and local part separated by ':')
char *xs_QName_create(const char* uri,
                      const char* prefix_and_local, 
                      void* (*alloc_func)(size_t),
                      dynamic_context *cxt);
// backs up fn:resolve-QName() function (prefix_and_local constains prefix and local part separated by ':')
char *xs_QName_create(const char* prefix_and_local,
                      const xptr& elem_node,
                      void* (*alloc_func)(size_t),
                      dynamic_context *cxt);


void  xs_QName_release(char *qname, void (*free_func)(void*));
const char *xs_QName_get_prefix(const char* qname);
const char *xs_QName_get_uri(const char* qname);
const char *xs_QName_get_local_name(const char* qname);
xml_ns     *xs_QName_get_xmlns(const char* qname);
void  xs_QName_print(const char* qname, std::ostream& str);
void  xs_QName_print_to_lr(const char* qname, std::ostream& str);



inline bool _xs_QName_equal(const char* uri1, const char *local1, const char *uri2, const char *local2)
{
    if (uri1 != uri2)
    {
        if (uri1 == NULL || uri2 == NULL) return false;
        if (strcmp(uri1, uri2) != 0) return false;
    }

    return strcmp(local1, local2) == 0;
}

inline bool _xs_QName_not_equal(const char* uri1, const char *local1, const char *uri2, const char *local2)
{
    if (strcmp(local1, local2) != 0) return true;

    if (uri1 == NULL || uri2 == NULL)
    {
        if (uri1 == uri2) return false;
        else return true;
    }

    return strcmp(uri1, uri2) != 0;
}

// This function is used for Sequence Type implementation and intended to be faster
// than creating xs:QNames from prefix and local part and calling dm:node-name (bla-bla-bla)
// Parameters:
// uri could be NULL
// node must be element or attribute and CHECKP should be called on node already
bool _xs_QName_not_equal(const char *uri, const char *local, const xptr &node);

#endif
