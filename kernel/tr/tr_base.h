/*
 * File:  tr_base.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _TR_BASE_H
#define _TR_BASE_H

#include "common/sedna.h"

/** File contains common trn type definitions.
 * Usually if type is defined here it means that
 * it is used exactly within trn and shared among
 * several trn subparts.
 */

/**
 * Type of query that executed by query processor
 */
enum QueryType
{
    TL_XQuery       = 9,    // XQuery query (xq_string -> AST -> full analysis -> QEP)
    TL_XQueryMod    = 8,    // XQuery query for modules (without further lreturn; only sema; xq_string -> AST -> sema)
    TL_ASTInitial   = 7,    // internal initial (before semantic pass) AST representation (ast_string -> AST -> full analysis -> QEP)
    TL_ASTQEPReady  = 6,    // internal QEP-ready AST representation (we wont run any analysis; ast_string -> AST -> QEP)
};

/**
 * Possible atomic types
 */
typedef __int16 xmlscm_type;


/**
 * Type of schema node
 */
enum t_item {
    element,
    text,
    attribute,
    document,
    virtual_root,
    xml_namespace,
    comment,
    pr_ins,
    cdata
};


/**
 * XML Schema Part 2 Datatypes
 * NOTE!: The order of types is significant, because some functions depend on
 * this order. If you are going to change something below, think twice!
 */

// Abstract base types
#define xs_anyType				0
#define xs_anySimpleType		1
#define xs_anyAtomicType		2

// Built-in simple, non-atomic types
#define xs_IDREFS				3
#define xs_NMTOKENS				4
#define xs_ENTITIES				5

// Built-in complex types
#define xs_untyped				6

// Built-in atomic types (Primitive types)
#define xs_dateTime				10
#define xs_date					11
#define xs_time					12
#define xs_duration				13
#define xs_yearMonthDuration	14
#define xs_dayTimeDuration		15
#define xs_gYearMonth			16
#define xs_gYear				17
#define xs_gMonthDay			18
#define xs_gDay					19
#define xs_gMonth				20
#define xs_float				21
#define xs_double				22
#define xs_decimal				23
#define xs_integer				24
#define xs_boolean				25
#define xs_untypedAtomic		26
#define xs_string				27
#define xs_base64Binary			28
#define xs_hexBinary			29
#define xs_anyURI				30
#define xs_QName				31
#define xs_NOTATION				32

// Special Sedna types
#define se_separator		    33
#define se_sequence             34

// Types derived from xs:string
#define xs_normalizedString		41
#define xs_token				42
#define xs_language				43
#define xs_NMTOKEN				44
#define xs_Name					45
#define xs_NCName				46
#define xs_ID					47
#define xs_IDREF				48
#define xs_ENTITY				49

// Types derived from xs:integer
#define xs_nonPositiveInteger   50
#define xs_negativeInteger      51
#define xs_long                 52
#define xs_int 				    53
#define xs_short                54
#define xs_byte                 55
#define xs_nonNegativeInteger   56
#define xs_unsignedLong         57
#define xs_unsignedInt          58
#define xs_unsignedShort        59
#define xs_unsignedByte         60
#define xs_positiveInteger      61


#endif /* _TR_BASE_H */

