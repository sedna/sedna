/*
 * File:  uri.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "uri.h"
#include "e_string.h"
#include "strings.h"

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/// Regexps constructed from the URI EBNF defined by the RFC 2396
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////


/****************************************************************************
  EBNF presented in form of regular expressions:
 ****************************************************************************
   
  hex "[0-9a-fA-F]"           
  escaped "%" + hex + "{2}"     
  mark "[\\-_\\.!~\\*'\\(\\)]"  
  unreserved "(" + alphanum + "|" + mark + ")"  
  reserved "[;/\\?:@&=\\+$,\\[\\]]"  
  uric "(" + reserved + "|" + unreserved + "|" + escaped + ")"     
  fragment uric + "*"
  query uric + "*"
  pchar "(" + unreserved + "|" + escaped + "|[:@&=\\+$,])"
  param pchar + "*"
  segment "(" + param + "(;" + param + ")*)"
  pathSegments "(" + segment + "(/" + segment + ")*)"
  port "[0-9]*"
  __upTo3digits "[0-9]{1,3}"
  IPv4address __upTo3digits + "\\." + __upTo3digits + "\\." + __upTo3digits + "\\." + __upTo3digits
  hex4 hex + "{1,4}"
  hexseq hex4 + "(:" + hex4 + ")*"
  hexpart "((" + hexseq + "(::(" + hexseq + ")?)?)|(::(" + hexseq + ")?))"
  IPv6address "((" + hexpart + "(:" + IPv4address + ")?)|(::" + IPv4address + "))"
  IPv6reference "\\[" + IPv6address + "\\]"
  domainlabel alphanum + "([0-9A-Za-z\\-]*" + alphanum + ")?"
  toplabel alpha + "([0-9A-Za-z\\-]*" + alphanum + ")?"
  hostname "(" + domainlabel + "\\.)*" + toplabel + "(\\.)?"
  host "((" + hostname + ")|(" + IPv4address + ")|(" + IPv6reference + "))"
  hostport host + "(:" + port + ")?"
  userinfo "(" + unreserved + "|" + escaped + "|[;:&=\\+$,])*"
  server "((" + userinfo + "@)?" + hostport + ")?"
  regName "(" + unreserved + "|" + escaped + "|[$,;:@&=\\+])+"
  authority "((" + server + ")|(" + regName + "))"
  scheme alpha + "[A-Za-z0-9\\+\\-\\.]*"
  relSegment "(" + unreserved + "|" + escaped + "|[;@&=\\+$,])+"
  absPath "/" + pathSegments
  relPath relSegment + "(" + absPath + ")?"
  netPath "//" + authority + "(" + absPath + ")?"
  uricNoSlash "(" + unreserved + "|" + escaped + "|[;\\?:@&=\\+$,])"
  opaquePart uricNoSlash + "(" + uric + ")*"
  hierPart "((" + netPath + ")|(" + absPath + "))(\\?" + query + ")?"
  //   path            "(("+absPath+")|("+opaquePart+"))?"
  relativeURI "((" + netPath + ")|(" + absPath + ")|(" + relPath + "))(\\?" + query + ")?"
  absoluteURI scheme + ":((" + hierPart + ")|(" + opaquePart + "))"
  uriRef "(" + absoluteURI + "|" + relativeURI + ")?(#" + fragment + ")?"

******************************************************************************/


const char* relativeURI = 
"^((//(((((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[;:&=\\+$,])*@)?((([0-9a-zA-Z]([0-9A\
-Za-z\\-]*[0-9a-zA-Z])?\\.)*[a-zA-Z]([0-9A-Za-z\\-]*[0-9a-zA-Z])?(\\.)?)|([0-9]{1,3}\\.[0-9]{1,3}\\\
.[0-9]{1,3}\\.[0-9]{1,3})|(\\[(((([0-9a-fA-F]{1,4}(:[0-9a-fA-F]{1,4})*(::([0-9a-fA-F]{1,4}(:[0-9a-f\
A-F]{1,4})*)?)?)|(::([0-9a-fA-F]{1,4}(:[0-9a-fA-F]{1,4})*)?))(:[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\
\\.[0-9]{1,3})?)|(::[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}))\\]))(:[0-9]*)?)?)|((([0-9a-\
zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[$,;:@&=\\+])+))(/(((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)\
])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*(;(([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])\
*)*)(/((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*(;(([0-9a-zA-Z]|[\\-_\\.!~"
"\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*)*))*))?)|(/(((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a\
-fA-F]{2}|[:@&=\\+$,])*(;(([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*)*)(/(((\
[0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*(;(([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\\
)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*)*))*))|((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[;@&\
=\\+$,])+(/(((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*(;(([0-9a-zA-Z]|[\\-\
_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*)*)(/((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-f\
A-F]{2}|[:@&=\\+$,])*(;(([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*)*))*))?))\
(\\?([;/\\?:@&=\\+$,\\[\\]]|([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2})*)?$";

const char* absoluteURI =
"^[a-zA-Z][A-Za-z0-9\\+\\-\\.]*:((((//(((((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[;:&=\
\\+$,])*@)?((([0-9a-zA-Z]([0-9A-Za-z\\-]*[0-9a-zA-Z])?\\.)*[a-zA-Z]([0-9A-Za-z\\-]*[0-9a-zA-Z])?(\\\
.)?)|([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})|(\\[(((([0-9a-fA-F]{1,4}(:[0-9a-fA-F]{1,4}\
)*(::([0-9a-fA-F]{1,4}(:[0-9a-fA-F]{1,4})*)?)?)|(::([0-9a-fA-F]{1,4}(:[0-9a-fA-F]{1,4})*)?))(:[0-9]\
{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})?)|(::[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3\
}))\\]))(:[0-9]*)?)?)|((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[$,;:@&=\\+])+))(/(((([\
0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*(;(([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)\
])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*)*)(/((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+"
"$,])*(;(([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*)*))*))?)|(/(((([0-9a-zA-Z\
]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*(;(([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a\
-fA-F]{2}|[:@&=\\+$,])*)*)(/((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*(;((\
[0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*)*))*)))(\\?([;/\\?:@&=\\+$,\\[\\]]\
|([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2})*)?)|((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0\
-9a-fA-F]{2}|[;\\?:@&=\\+$,])(([;/\\?:@&=\\+$,\\[\\]]|([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA\
-F]{2}))*))$";

const char* uriRef = 
"^([a-zA-Z][A-Za-z0-9\\+\\-\\.]*:((((//(((((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[;:&\
=\\+$,])*@)?((([0-9a-zA-Z]([0-9A-Za-z\\-]*[0-9a-zA-Z])?\\.)*[a-zA-Z]([0-9A-Za-z\\-]*[0-9a-zA-Z])?(\\\
.)?)|([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})|(\\[(((([0-9a-fA-F]{1,4}(:[0-9a-fA-F]{1,4})\
*(::([0-9a-fA-F]{1,4}(:[0-9a-fA-F]{1,4})*)?)?)|(::([0-9a-fA-F]{1,4}(:[0-9a-fA-F]{1,4})*)?))(:[0-9]{1\
,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})?)|(::[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}))\
\\]))(:[0-9]*)?)?)|((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[$,;:@&=\\+])+))(/(((([0-9a\
-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*(;(([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[\
0-9a-fA-F]{2}|[:@&=\\+$,])*)*)(/((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*(\
;(([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*)*))*))?)|(/(((([0-9a-zA-Z]|[\\-_\
\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*(;(([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2\
}|[:@&=\\+$,])*)*)(/((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*(;(([0-9a-zA-\
Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*)*))*)))(\\?([;/\\?:@&=\\+$,\\[\\]]|([0-9a-zA\
-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2})*)?)|((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2\
}|[;\\?:@&=\\+$,])(([;/\\?:@&=\\+$,\\[\\]]|([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}))*))|\
((//(((((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[;:&=\\+$,])*@)?((([0-9a-zA-Z]([0-9A-Za\
-z\\-]*[0-9a-zA-Z])?\\.)*[a-zA-Z]([0-9A-Za-z\\-]*[0-9a-zA-Z])?(\\.)?)|([0-9]{1,3}\\.[0-9]{1,3}\\.[0-"
"9]{1,3}\\.[0-9]{1,3})|(\\[(((([0-9a-fA-F]{1,4}(:[0-9a-fA-F]{1,4})*(::([0-9a-fA-F]{1,4}(:[0-9a-fA-F]{\
1,4})*)?)?)|(::([0-9a-fA-F]{1,4}(:[0-9a-fA-F]{1,4})*)?))(:[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-\
9]{1,3})?)|(::[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}))\\]))(:[0-9]*)?)?)|((([0-9a-zA-Z]|[\
\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[$,;:@&=\\+])+))(/(((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9\
a-fA-F]{2}|[:@&=\\+$,])*(;(([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*)*)(/(((\
[0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*(;(([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)\
])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*)*))*))?)|(/(((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[\
:@&=\\+$,])*(;(([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*)*)(/((([0-9a-zA-Z]|\
[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*(;(([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA\
-F]{2}|[:@&=\\+$,])*)*))*))|((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[;@&=\\+$,])+(/(((\
([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*(;(([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\\
)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*)*)(/((([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+\
$,])*(;(([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2}|[:@&=\\+$,])*)*))*))?))(\\?([;/\\?:@&=\\\
+$,\\[\\]]|([0-9a-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2})*)?)?(#([;/\\?:@&=\\+$,\\[\\]]|([0-9a\
-zA-Z]|[\\-_\\.!~\\*'\\(\\)])|%[0-9a-fA-F]{2})*)?$";


/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/// Checks constraints for the xs:anyURI type
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////


/// Checks general syntax of the given URI as defined by the RFC 2396:
/// "Uniform Resource Identifier (URI): Generic Syntax"
bool chech_constraints_for_xs_anyURI(const tuple_cell *tc)
{
    return collation_handler->matches(tc, uriRef);
}

bool check_constraints_for_absolute_URI(const tuple_cell *tc)
{
    return collation_handler->matches(tc, absoluteURI);
}

bool check_constraints_for_relative_URI(const tuple_cell *tc)
{
    return collation_handler->matches(tc, relativeURI);
}
 
