/*
 * File:  uri.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "uri.h"
#include "e_string.h"

#define PCRE_STATIC
#define SUPPORT_UTF8
#define SUPPORT_UCP

#include "pcre.h"

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


bool Uri::chech_constraints_for_xs_anyURI(const tuple_cell *tc)
{
    return collation_handler->matches(tc, uriRef);
}

bool Uri::check_constraints_for_absolute_URI(const tuple_cell *tc)
{
    return collation_handler->matches(tc, absoluteURI);
}

bool Uri::check_constraints_for_relative_URI(const tuple_cell *tc)
{
    return collation_handler->matches(tc, relativeURI);
}

void Uri::resolve(const char* relative, const char* base, t_str_buf &dest)
{
    /*PcrePattern re("^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?$", PCRE_UTF8 | PCRE_NO_UTF8_CHECK);
    int len = strlen(relative); 
	char_iterator start(relative, len, 0);
	char_iterator end(relative, len, len);
    int match_flags = PCRE_NO_UTF8_CHECK;
    PcreMatcher<char_iterator> matcher(re);
    
    if (matcher.matches(start, end, start, match_flags))
    {
        
        for (int i = 0; i < matcher.groupCount(); i++)
        {
			char bb[12];
			char_iterator s = matcher.start(i), e = matcher.end(i);
			u_itoa(i, bb, 10);
			dest << "group "<<bb<<":";
			while (s<e)
				dest << *s++;
			dest << "\n";
		}
	}
	else
		throw USER_EXCEPTION2(SE1003, "Incorrect relative URI reference in resolve URI.");*/
}

void Uri::recompose(t_str_buf &dest)
{

   if (get_scheme() != NULL)
   { 
      dest << get_scheme();
      dest << ":";
   }

   if (get_authority() != NULL)
   {
      dest << "//";
      dest << get_authority();
   }

   if (get_path() != NULL) 
   {
       dest << get_path();
   }

   if (get_query() != NULL)
   {
      dest << "?";
      dest << get_query();
   }

   if (get_fragment() != NULL)
   {
      dest << "#";
      dest << get_fragment();
   }
}

Uri Uri::parse(const char* u)
{
    PcrePattern re("^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?$", PCRE_UTF8 | PCRE_NO_UTF8_CHECK);
    int len = strlen(u); 
    char_iterator start((char*)u, len, 0);
    char_iterator end((char*)u, len, len);
    int match_flags = PCRE_NO_UTF8_CHECK;
    PcreMatcher<char_iterator> matcher(re);
    char* buffer = new char[len + 1];
    Uri res;

    if (matcher.matches(start, end, start, match_flags))
    {
        for (int i = 1; i < matcher.groupCount(); i++) 
        {
            char_iterator s = matcher.start(i);
            char_iterator e = matcher.end(i);
            int shift = 0;
            if(i == 1 || i == 3 || i == 6 || i == 8) 
            {
                continue;
            }
            while(s < e)
            {
                *(buffer+shift) = *s;
                s++;
                shift++;
            }
            if(shift == 0) continue;
            *(buffer+shift) = '\0';
            char* component = new char[shift+1];
            strcpy(component, buffer);
            switch(i)
            {
                case 2 : res.scheme    = str_counted_ptr(component); break;
                case 4 : res.authority = str_counted_ptr(component); break;
                case 5 : res.path      = str_counted_ptr(component); break;
                case 7 : res.query     = str_counted_ptr(component); break;
                case 9 : res.fragment  = str_counted_ptr(component); break;
                default: throw USER_EXCEPTION2(SE1003, "Impossible case in Uri:parse.");
            }
        }
    }
    else
        throw USER_EXCEPTION2(SE1003, "Invalid URI in Uri::parse.");

    return res;
}
