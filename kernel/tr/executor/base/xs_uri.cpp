/*
 * File:  xs_uri.cpp
 * Copyright (C) 2010 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include <stack>

#include "common/sedna.h"

#include "tr/strings/strings.h"
#include "tr/executor/base/xs_uri.h"
#include "tr/executor/base/xs_helper.h"
#include "tr/executor/base/PPBase.h"
#include "tr/structures/system_tables.h"
#define PCRE_STATIC
#define SUPPORT_UTF8
#define SUPPORT_UCP

#include "pcre/pcre.h"

typedef std::stack<size_t> stack_of_positions;

enum remover_state {BEGIN, SLASH, DOT, DOUBLE_DOT, SLASH_DOT, SLASH_DOUBLE_DOT, SYMBOL};

#define MAX_SCHEME_NAME_SIZE 10

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/// Regexps constructed from the URI ABNF defined by the RFC 3986
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/*
   General syntax
   ==============
   The Java code below was used to generate regular expressions for URI and 
   relative_ref. This code was created based on ABNF presented in RFC 3986.

  String gen_delims       = "[:/\\?#\\[\\]@]";
  String sub_delims       = "[!$&'\\(\\)\\*\\+,;=]";
  String alpha            = "[a-zA-Z]";
  String alphanum         = "[0-9a-zA-Z]";
  String hex              = "[0-9a-fA-F]";
  String pct_encoded      = "%" + hex + "{2}";
  String reserved         = "[:/\\?#\\[\\]@!$&'\\(\\)\\*\\+,;=]";
  String unreserved       = "[0-9a-zA-Z\\-\\._~]";
  String pchar            = "(" + unreserved + "|" + pct_encoded + "|[!$&'\\(\\)\\*\\+,;=@:])";
  String query            = "(" + pchar + "|[/\\?])*";
  String fragment         = "(" + pchar + "|[/\\?])*";
  String segment          = pchar + "*";
  String segment_nz       = pchar + "+";
  String segment_nz_nc    = "(" + unreserved + "|" + pct_encoded + "|[!$&'\\(\\)\\*\\+,;=@])+";
  String path_abempty     = "(/" + segment + ")*";
  String path_absolute    = "(/" + "(" + segment_nz + "(/" + segment + ")*)?)";
  String path_noscheme    = "(" + segment_nz_nc + "(/" + segment + ")*)";
  String path_rootless    = "(" + segment_nz + "(/" + segment + ")*)";
  String path             = "(" + path_abempty + "|" + path_absolute + "|" + path_noscheme + "|" + path_rootless + ")?";
  String dec_octet        = "[0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])";
  String reg_name         = "(" + unreserved + "|" + pct_encoded + "|" + sub_delims + ")*";
  String IPv4address      = "("+ dec_octet + "\\.){3}" + dec_octet;
  String h16              = hex + "{1,4}";
  String ls32             = "((" + h16 + ":" + h16 + ")|(" +  IPv4address + "))";
  String IPv6address      = "(((" + h16 + ":){6}" + ls32 + ")"                                  + "|" + 
                            "(::(" + h16 + ":){5}" + ls32 + ")"                                 + "|" +
                            "((" + h16 + ")*::(" + h16 + ":){4}" + ls32 + ")"                   + "|" +
                            "(((" + h16 + ":)?"+ h16 + ")*::(" + h16 + ":){3}" + ls32 + ")"     + "|" +
                            "(((" + h16 + ":){0,2}"+ h16 + ")*::(" + h16 + ":){2}" + ls32 + ")" + "|" +
                            "(((" + h16 + ":){0,3}"+ h16 + ")*::" + h16 + ":" + ls32 + ")"      + "|" +
                            "(((" + h16 + ":){0,4}"+ h16 + ")*::" + ls32 + ")"                  + "|" +
                            "(((" + h16 + ":){0,5}"+ h16 + ")*::" + h16 + ")"                   + "|" +
                            "(((" + h16 + ":){0,6}"+ h16 + ")*::))";
  String IPvFuture        = "(v" + hex + "+\\.(" + unreserved + "|" + "[!$&'\\(\\)\\*\\+,;=:])+)";
  String IP_literal       = "(\\[(" + IPv6address + "|" + IPvFuture  + ")\\])";
  String port             = "[0-9]*";
  String host             = "(" + IP_literal + "|" + IPv4address  + "|" + reg_name + ")";
  String userinfo         = "(" + unreserved  + "|" + pct_encoded  + "|" + "[!$&'\\(\\)\\*\\+,;=:])*";
  String authority        = "((" + userinfo + "@)?" + host + "(:" + port + ")?)";
  String scheme           = alpha + "[A-Za-z0-9\\+\\-\\.]*";
  String relative_part    = "((//" + authority + path_abempty + ")" + "|" + path_absolute + "|" + path_noscheme + ")*";
  String relative_ref     = relative_part + "(\\?" + query + ")?" + "(#" + fragment + ")?";
  String hier_part        = "((//" + authority + path_abempty + ")" + "|" + path_absolute + "|" + path_rootless + ")*";
  String absolute_URI     = scheme + ":" + hier_part + "(\\?" + query + ")?" ;
  String URI              = scheme + ":" + hier_part + "(\\?" + query + ")?" + "(#" + fragment + ")?";
  String URI_reference    = "(" + URI + ")|(" + relative_ref +")";
  

   HTTP specific URI:
   (it is based on RFC 2393 and RFC 2616)
   ==================
  String old_path_absolute= "(/" + "(" + segment + "(/" + segment + ")*)?)";
  String hostname         = "(" + unreserved + "|" + pct_encoded + "|" + sub_delims + ")+";
  String old_host         = "(" + IP_literal + "|" + IPv4address  + "|" + hostname + ")";
  String http_URI         = "http://" + old_host + "(:" + port + ")?" + "(" + old_path_absolute + "(\\?" + query + ")? + "(#" + fragment + ")?)?";
  http equal to http|httP|htTp|htTP|hTtp|hTtP|hTTp|hTTP|Http|HttP|HtTp|HtTP|HTtp|HTtP|HTTp|HTTP
*/
  
const char* URI = 
"^[\\n\\r\\t ]*[a-zA-Z][A-Za-z0-9\\+\\-\\.]*:((//((([ 0-9a-zA-Z\\-\\._~]|%[0-9a-fA-F]{2}|[!$&'\\(\\)\\*\\+,;=:])*@\
)?((\\[(((([0-9a-fA-F]{1,4}:){6}(([0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4})|(([0-9]|([1-9][0-9])|(1[0-9]{2}\
)|(2[0-4][0-9])|(25[0-5])\\.){3}[0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5]))))|(::([0-9a-\
fA-F]{1,4}:){5}(([0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4})|(([0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(\
25[0-5])\\.){3}[0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5]))))|(([0-9a-fA-F]{1,4})*::([0-9\
a-fA-F]{1,4}:){4}(([0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4})|(([0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])\
|(25[0-5])\\.){3}[0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5]))))|((([0-9a-fA-F]{1,4}:)?[0-\
9a-fA-F]{1,4})*::([0-9a-fA-F]{1,4}:){3}(([0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4})|(([0-9]|([1-9][0-9])|(1[\
0-9]{2})|(2[0-4][0-9])|(25[0-5])\\.){3}[0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5]))))|(((\
[0-9a-fA-F]{1,4}:){0,2}[0-9a-fA-F]{1,4})*::([0-9a-fA-F]{1,4}:){2}(([0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4}\
)|(([0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])\\.){3}[0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0"
"-4][0-9])|(25[0-5]))))|((([0-9a-fA-F]{1,4}:){0,3}[0-9a-fA-F]{1,4})*::[0-9a-fA-F]{1,4}:(([0-9a-fA-F]{\
1,4}:[0-9a-fA-F]{1,4})|(([0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])\\.){3}[0-9]|([1-9][0\
-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5]))))|((([0-9a-fA-F]{1,4}:){0,4}[0-9a-fA-F]{1,4})*::(([0-9a-fA\
-F]{1,4}:[0-9a-fA-F]{1,4})|(([0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])\\.){3}[0-9]|([1-\
9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5]))))|((([0-9a-fA-F]{1,4}:){0,5}[0-9a-fA-F]{1,4})*::[0-9a-\
fA-F]{1,4})|((([0-9a-fA-F]{1,4}:){0,6}[0-9a-fA-F]{1,4})*::))|(v[0-9a-fA-F]+\\.([ 0-9a-zA-Z\\-\\._~]|[\
!$&'\\(\\)\\*\\+,;=:])+))\\])|([0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])\\.){3}[0-9]|([\
1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])|([ 0-9a-zA-Z\\-\\._~]|%[0-9a-fA-F]{2}|[!$&'\\(\\)\\*\
\\+,;=])*)(:[0-9]*)?)(/([ 0-9a-zA-Z\\-\\._~]|%[0-9a-fA-F]{2}|[!$&'\\(\\)\\*\\+,;=@:])*)*)|(/(([0-9a-zA-\
Z\\-\\._~]|%[0-9a-fA-F]{2}|[!$&'\\(\\)\\*\\+,;=@:])+(/([ 0-9a-zA-Z\\-\\._~]|%[0-9a-fA-F]{2}|[!$&'\\(\
\\)\\*\\+,;=@:])*)*)?)|(([ 0-9a-zA-Z\\-\\._~]|%[0-9a-fA-F]{2}|[!$&'\\(\\)\\*\\+,;=@:])+(/([0-9a-zA-Z\
\\-\\._~]|%[0-9a-fA-F]{2}|[!$&'\\(\\)\\*\\+,;=@:])*)*))*(\\?(([ 0-9a-zA-Z\\-\\._~]|%[0-9a-fA-F]{2}|[!$&\
'\\(\\)\\*\\+,;=@:])|[/\\?])*)?(#(([ 0-9a-zA-Z\\-\\._~]|%[0-9a-fA-F]{2}|[!$&'\\(\\)\\*\\+,;=@:])|[/\
\\?])*)?[\\n\\r\\t ]*$";

const char* http_URI =
"^[\\n\\r\\t ]*(h|H)(t|T){2}(p|P)://((\\[(((([0-9a-fA-F]{1,4}:){6}(([0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4})|(([0-9]|([1-9][0-9])|(1[\
0-9]{2})|(2[0-4][0-9])|(25[0-5])\\.){3}[0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5]))))|(::\
([0-9a-fA-F]{1,4}:){5}(([0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4})|(([0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][\
0-9])|(25[0-5])\\.){3}[0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5]))))|(([0-9a-fA-F]{1,4})*\
::([0-9a-fA-F]{1,4}:){4}(([0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4})|(([0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4\
][0-9])|(25[0-5])\\.){3}[0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5]))))|((([0-9a-fA-F]{1,4\
}:)?[0-9a-fA-F]{1,4})*::([0-9a-fA-F]{1,4}:){3}(([0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4})|(([0-9]|([1-9][0-\
9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])\\.){3}[0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])\
)))|((([0-9a-fA-F]{1,4}:){0,2}[0-9a-fA-F]{1,4})*::([0-9a-fA-F]{1,4}:){2}(([0-9a-fA-F]{1,4}:[0-9a-fA-\
F]{1,4})|(([0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])\\.){3}[0-9]|([1-9][0-9])|(1[0-9]{2"
"})|(2[0-4][0-9])|(25[0-5]))))|((([0-9a-fA-F]{1,4}:){0,3}[0-9a-fA-F]{1,4})*::[0-9a-fA-F]{1,4}:(([0-9a\
-fA-F]{1,4}:[0-9a-fA-F]{1,4})|(([0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])\\.){3}[0-9]|(\
[1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5]))))|((([0-9a-fA-F]{1,4}:){0,4}[0-9a-fA-F]{1,4})*::(([\
0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4})|(([0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])\\.){3}[0-\
9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5]))))|((([0-9a-fA-F]{1,4}:){0,5}[0-9a-fA-F]{1,4})*:\
:[0-9a-fA-F]{1,4})|((([0-9a-fA-F]{1,4}:){0,6}[0-9a-fA-F]{1,4})*::))|(v[0-9a-fA-F]+\\.([0-9a-zA-Z\\- \
\\._~]|[!$&'\\(\\)\\*\\+,;=:])+))\\])|([0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])\\.){3}\
[0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])|([ 0-9a-zA-Z\\-\\._~]|%[0-9a-fA-F]{2}|[!$&'\\\
(\\)\\*\\+,;=])+)(:[0-9]*)?((/(([ 0-9a-zA-Z\\-\\._~]|%[0-9a-fA-F]{2}|[!$&'\\(\\)\\*\\+,;=@:])*(/([0-9\
a-zA-Z\\-\\._~]|%[0-9a-fA-F]{2}|[!$&'\\(\\)\\*\\+,;=@:])*)*)?)(\\?(([ 0-9a-zA-Z\\-\\._~]|%[0-9a-fA-F]\
{2}|[!$&'\\(\\)\\*\\+,;=@:])|[/\\?])*)?(#(([ 0-9a-zA-Z\\-\\._~]|%[0-9a-fA-F]{2}|[!$&'\\(\\)\\*\\+,;\
=@:])|[/\\?])*)?)?[\\n\\r\\t ]*$";

const char* relative_ref = 
"^[\\n\\r\\t ]*((//((([ 0-9a-zA-Z\\-\\._~]|%[0-9a-fA-F]{2}|[!$&'\\(\\)\\*\\+,;=:])*@)?((\\[(((([0-9a-fA-F]{1,4}:){6\
}(([0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4})|(([0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])\\.){3}\
[0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5]))))|(::([0-9a-fA-F]{1,4}:){5}(([0-9a-fA-F]{1,4}\
:[0-9a-fA-F]{1,4})|(([0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])\\.){3}[0-9]|([1-9][0-9])|\
(1[0-9]{2})|(2[0-4][0-9])|(25[0-5]))))|(([0-9a-fA-F]{1,4})*::([0-9a-fA-F]{1,4}:){4}(([0-9a-fA-F]{1,4}\
:[0-9a-fA-F]{1,4})|(([0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])\\.){3}[0-9]|([1-9][0-9])|\
(1[0-9]{2})|(2[0-4][0-9])|(25[0-5]))))|((([0-9a-fA-F]{1,4}:)?[0-9a-fA-F]{1,4})*::([0-9a-fA-F]{1,4}:){\
3}(([0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4})|(([0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])\\.){3\
}[0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5]))))|((([0-9a-fA-F]{1,4}:){0,2}[0-9a-fA-F]{1,4}\
)*::([0-9a-fA-F]{1,4}:){2}(([0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4})|(([0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-\
4][0-9])|(25[0-5])\\.){3}[0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5]))))|((([0-9a-fA-F]{1,4\
}:){0,3}[0-9a-fA-F]{1,4})*::[0-9a-fA-F]{1,4}:(([0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4})|(([0-9]|([1-9][0-9]\
)|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])\\.){3}[0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5]))))\
|((([0-9a-fA-F]{1,4}:){0,4}[0-9a-fA-F]{1,4})*::(([0-9a-fA-F]{1,4}:[0-9a-fA-F]{1,4})|(([0-9]|([1-9][0-\
9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])\\.){3}[0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5]))"
"))|((([0-9a-fA-F]{1,4}:){0,5}[0-9a-fA-F]{1,4})*::[0-9a-fA-F]{1,4})|((([0-9a-fA-F]{1,4}:){0,6}[0-9a-fA\
-F]{1,4})*::))|(v[0-9a-fA-F]+\\.([ 0-9a-zA-Z\\-\\._~]|[!$&'\\(\\)\\*\\+,;=:])+))\\])|([0-9]|([1-9][0-9\
])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])\\.){3}[0-9]|([1-9][0-9])|(1[0-9]{2})|(2[0-4][0-9])|(25[0-5])|(\
[ 0-9a-zA-Z\\-\\._~]|%[0-9a-fA-F]{2}|[!$&'\\(\\)\\*\\+,;=])*)(:[0-9]*)?)(/([ 0-9a-zA-Z\\-\\._~]|%[0-9a-\
fA-F]{2}|[!$&'\\(\\)\\*\\+,;=@:])*)*)|(/(([ 0-9a-zA-Z\\-\\._~]|%[0-9a-fA-F]{2}|[!$&'\\(\\)\\*\\+,;=@:]\
)+(/([ 0-9a-zA-Z\\-\\._~]|%[0-9a-fA-F]{2}|[!$&'\\(\\)\\*\\+,;=@:])*)*)?)|(([ 0-9a-zA-Z\\-\\._~]|%[0-9a-\
fA-F]{2}|[!$&'\\(\\)\\*\\+,;=@])+(/([ 0-9a-zA-Z\\-\\._~]|%[0-9a-fA-F]{2}|[!$&'\\(\\)\\*\\+,;=@:])*)*))\
*(\\?(([ 0-9a-zA-Z\\-\\._~]|%[0-9a-fA-F]{2}|[!$&'\\(\\)\\*\\+,;=@:])|[/\\?])*)?(#(([ 0-9a-zA-Z\\-\\._~]\
|%[0-9a-fA-F]{2}|[!$&'\\(\\)\\*\\+,;=@:])|[/\\?])*)?[\\n\\r\\t ]*$";


const unsigned char scheme_allowed[16] = {0x00, 0x00, 0x00, 0x00,
                                          0x00, 0x16, 0x00, 0x00, 
                                          0x7F, 0xFF, 0xFF, 0xE0, 
                                          0x7F, 0xFF, 0xFF, 0xE0};
/*
00000000  // 0x00 00-07;
00000000  // 0x00 08-15;
00000000  // 0x00 16-23;
00000000  // 0x00 24-31;
00000000  // 0x00 32-39;
00010110  // 0x16 40-47;
00000000  // 0x00 48-55;
00000000  // 0x00 56-63;
01111111  // 0x7F 64-71;
11111111  // 0xFF 72-79;
11111111  // 0xFF 80-87;
11100000  // 0xE0 88-95;
01111111  // 0x7F 96-103;
11111111  // 0xFF 104-111;
11111111  // 0xFF 112-119;
11100000  // 0xE0 120-127;
*/

#define IS_BYTE_SCHEME_ALLOWED(byte) \
    (byte & 0x80 ? 0 : (scheme_allowed[(byte >> 3)] & (0x80 >> (byte & 7))))

#define TO_LOWER_CASE(byte) \
    ('A' <= byte && byte <= 'Z' \
    ? 'a' + (byte - 'A') \
    : byte)


template <class Iterator>
static inline void is_URI_contains_space(Iterator &start, const Iterator &end, bool* result)
{
    *result = false;
    while(start < end && *start != ' ') { start++; }
    if(start < end) *result = true;
}

template <class Iterator>
static inline void is_URI_with_scheme_and_normalized(Iterator &start, const Iterator &end, bool* res, char* scheme_buf, bool* normalized)
{
    *res = false;
    int counter = 0;
    unsigned char value;

    if(start < end && IS_WHITESPACE(*start)) *normalized = false;
    while(start < end && IS_WHITESPACE(*start)) { start++; }
    
    if(start == end) return;    
    value = *start;
    if(('A' > value || value > 'Z') && 
       ('a' > value || value > 'z')) return;
    scheme_buf[counter++] = TO_LOWER_CASE(value);
    ++start;
    
    while(start < end)
    {
        value = *start;
        /* Some versions of GCC are going crazy here. Don't 
           optimize it removing temp variable. */
        int temp = IS_BYTE_SCHEME_ALLOWED(value);
        if(temp == 0) break;
        ++start;
        if(counter < MAX_SCHEME_NAME_SIZE) scheme_buf[counter++] = TO_LOWER_CASE(value);
    }

    scheme_buf[counter] = '\0';

    if(*start == ':') *res = true;
    
    if(*normalized)
    {
        while(start < end && !IS_WHITESPACE(*start)) { start++; }
        if(start < end) *normalized = false;
    }
}

template <class Iterator>
static inline void is_URI_with_scheme_and_normalized(Iterator &start, const Iterator &end, bool* res, bool* normalized)
{
    *res = false;
    unsigned char value;

    if(start < end && IS_WHITESPACE(*start)) *normalized = false;
    while(start < end && IS_WHITESPACE(*start)) { start++; }
    
    if(start == end) return;    
    value = *start;
    if(('A' > value || value > 'Z') && 
       ('a' > value || value > 'z')) return;
    ++start;
    
    while(start < end)
    {
        value = *start;
        /* Some versions of GCC are going crazy here. Don't 
           optimize it removing temp variable. */
        int temp = IS_BYTE_SCHEME_ALLOWED(value); 
        if(temp == 0) break;
        ++start;
    }

    if(*start == ':') *res = true;
    
    if(*normalized)
    {
        while(start < end && !IS_WHITESPACE(*start)) { start++; }
        if(start < end) *normalized = false;
    }
}

//////////////////////////////////////////////////////////////////////
/// Cache is filled with predefined and already checked URIs.
/// For now we use it only for predefined system documents like
/// $db_security_data.
//////////////////////////////////////////////////////////////////////
static inline bool
check_in_cache(const char* uri, bool *valid, Uri::Information *nfo)
{
    /* We check only system documents now */
    if(uri == NULL || uri[0] != '$') return false;

    if(strcmp(SECURITY_METADATA_DOCUMENT, uri) == 0 ||
       get_document_type(uri, dbe_document) != DT_NON_SYSTEM)
    {
        if(nfo != NULL) {
           nfo -> type = Uri::UT_ABSOLUTE;
           nfo -> normalized = true;
        }
        if(valid != NULL) *valid = true;
        return true;
    }

    return false;
}


void Uri::check_constraints(const tuple_cell *in_tc, bool *valid, Uri::Information *nfo)
{
    //////////////////////////////////////////////////////////////////////
    /// Possibly, in future we will need to convert IRI (RFC 3987, which 
    /// is allowed by XPath Functions spec. as value for xs:anyURI type) 
    /// to URI before run constraints checking. It can be achieved using 
    /// fn:iri-to-uri() implementation.
    //////////////////////////////////////////////////////////////////////
    bool is_scheme  = false;
    bool normalized = true;
    char scheme_buf[MAX_SCHEME_NAME_SIZE + 1];
    const char* re = URI;
    
    /* Check if URI is already cached. In this case there is nothing to do for us. */
    if(in_tc -> is_light_atomic() && check_in_cache(in_tc -> get_str_mem(), valid, nfo)) return;

    STRING_ITERATOR_CALL_TEMPLATE_1tcptr_3p(is_URI_with_scheme_and_normalized, in_tc, &is_scheme, scheme_buf, &normalized); 

    if(is_scheme)
    {
         ///////////////////////////////////////////////////////////////
         /// RFC 3986 does not define scheme specific checking, but
         /// in some cases it is better to check absolute URI over scheme 
         /// specific regular expression.
         /// In future we can add any scheme specific checking using
         /// one more strcmp instruction and one more scheme specific
         /// regular expression.
         if(strcmp("http", scheme_buf) == 0) re = http_URI;
         ///////////////////////////////////////////////////////////////
    }
    else 
        re = relative_ref;

    (*valid) = charset_handler->matches(in_tc, re);

    if(*valid && nfo != NULL)
    {
        nfo -> type = is_scheme ? Uri::UT_ABSOLUTE : Uri::UT_RELATIVE;
        nfo -> normalized = normalized;
    }
}

void Uri::check_constraints(const char *s, bool *valid, Uri::Information *nfo)
{
    bool is_scheme  = false;
    bool normalized = true;
    char scheme_buf[MAX_SCHEME_NAME_SIZE + 1];
    const char* re = URI;
    const char* str = s;  // create copy of s to save its value within is_URI_with_scheme_and_normalized!

    /* Check if URI is already cached. In this case there is nothing to do for us. */
    if(check_in_cache(s, valid, nfo)) return;
    
    is_URI_with_scheme_and_normalized<const char*> (str, str + strlen(str), &is_scheme, scheme_buf, &normalized);
    
    if(is_scheme)
    {
        if(strcmp("http", scheme_buf) == 0) re = http_URI;
    }
    else 
        re = relative_ref;

    (*valid) = charset_handler->matches(s, re);

    if(*valid && nfo != NULL)
    {
        nfo -> type = is_scheme ? Uri::UT_ABSOLUTE : Uri::UT_RELATIVE;
        nfo -> normalized = normalized;
    }
}

bool Uri::is_relative(const char *s, Uri::Information *nfo)
{
    bool is_scheme  = false;
    bool normalized = true;
    
    is_URI_with_scheme_and_normalized<const char*> (s, s + strlen(s), &is_scheme, &normalized);
    
    if(nfo != NULL)
    {
        nfo -> type = is_scheme ? Uri::UT_ABSOLUTE : Uri::UT_RELATIVE;
        nfo -> normalized = normalized;
    }
    return !is_scheme;
}

bool Uri::is_relative(const tuple_cell *in_tc, Uri::Information *nfo)
{
    bool is_scheme  = false;
    bool normalized = true;

    STRING_ITERATOR_CALL_TEMPLATE_1tcptr_2p(is_URI_with_scheme_and_normalized, in_tc, &is_scheme, &normalized); 
    
    if(nfo != NULL)
    {
        nfo -> type = is_scheme ? Uri::UT_ABSOLUTE : Uri::UT_RELATIVE;
        nfo -> normalized = normalized;
    }
    return !is_scheme;
}


char* remove_dot_segments(const char* path)
{
    stack_of_positions positions;
    size_t source_length = strlen(path);
    char* output_buffer = new char[source_length + 1];
    size_t output_position = 0;

    remover_state state = BEGIN;

    for(size_t i = 0; i < source_length; i++)
    {
        char lexem = path[i];
        switch(state)
        {
            case BEGIN:
                if(lexem == '/')      state = SLASH;
                else if(lexem == '.') state = DOT;
                else                
                { 
                    state = SYMBOL; 
                    output_buffer[output_position++] = lexem; 
                }
                break;
            case SLASH:
                if(lexem == '.') state = SLASH_DOT;
                else if (lexem == '/')
                {
                    state = SLASH;
                    positions.push(output_position);
                    output_buffer[output_position++] = '/';
                }
                else 
                { 
                    state = SYMBOL; 
                    positions.push(output_position);
                    output_buffer[output_position++] = '/';
                    output_buffer[output_position++] = lexem; 
                }
                break;
            case DOT:
                if(lexem == '/') state = BEGIN;  
                else if(lexem == '.') state = DOUBLE_DOT;
                else 
                { 
                    state = SYMBOL; 
                    output_buffer[output_position++] = '.'; 
                    output_buffer[output_position++] = lexem; 
                }
                break;
            case DOUBLE_DOT:  
                if(lexem == '/') state = BEGIN;
                else 
                { 
                    state = SYMBOL; 
                    output_buffer[output_position++] = '.';
                    output_buffer[output_position++] = '.';  
                    output_buffer[output_position++] = lexem; 
                }
                break;
            case SLASH_DOT: 
                if(lexem == '/') state = SLASH;
                else if(lexem == '.') state = SLASH_DOUBLE_DOT;
                else 
                {
                    state = SYMBOL; 
                    positions.push(output_position);
                    output_buffer[output_position++] = '/';
                    output_buffer[output_position++] = '.';  
                    output_buffer[output_position++] = lexem; 
                }
                break;
            case SLASH_DOUBLE_DOT: 
                if(lexem == '/')
                {
                    state = SLASH;
                    if(positions.size()) 
                    {
                        output_position = positions.top(); 
                        positions.pop();                    
                    }
                    else output_position = 0;
                }
                else 
                {
                    state = SYMBOL; 
                    positions.push(output_position);
                    output_buffer[output_position++] = '/';
                    output_buffer[output_position++] = '.';
                    output_buffer[output_position++] = '.';  
                    output_buffer[output_position++] = lexem; 
                }
                break;
            case SYMBOL:
                if(lexem == '/') state = SLASH;
                else output_buffer[output_position++] = lexem;
                break;
            default: throw USER_EXCEPTION2(SE1003, "Impossible case in remove_dot_segments.");
        }    
    }
    
    if(state == SLASH_DOUBLE_DOT)
    {
        if(!positions.size()) output_position = 0;
        else { output_position = positions.top(); positions.pop(); }
    }
    
    if(state == SLASH_DOUBLE_DOT || state == SLASH || state == SLASH_DOT) output_buffer[output_position++] = '/';

    output_buffer[output_position] = '\0';

    char* result;
    if(output_position)
    {
        result = new char[output_position + 1];
        strcpy(result, output_buffer);
    }
    else result = NULL;

    delete [] output_buffer;
    output_buffer = NULL;

    return result;
}

bool Uri::resolve(const char* relative, const char* base, stmt_str_buf &dest)
{
    /// Since xs:anyURI type is not strict and allows spaces we must
    /// check there if given URIs contain spaces or not:
    bool is_spaces;
    const char* str = relative;
    is_URI_contains_space<const char*> (str, str + strlen(str), &is_spaces);
    if(is_spaces) throw XQUERY_EXCEPTION2(FORG0002, "Relative URI contains spaces.");
    str = base;
    is_URI_contains_space<const char*> (str, str + strlen(str), &is_spaces);
    if(is_spaces) throw XQUERY_EXCEPTION2(FORG0002, "Base URI contains spaces.");

    Uri R = parse(relative);      /// Parsed relative URI which we are going to resolve.
    
    if(R.scheme_defined)
    {
        if(R.fragment_defined)    
            throw XQUERY_EXCEPTION2(FORG0002, "Relative URI contains invalid absolute URI. Absolute URI can not contain fragment component (#).");
        return false;             /// If we have absolute URI given as $relative argument then we have nothing to do.
    }

    Uri B = parse(base);          /// Parsed base-uri over which we are going to resolve relative URI.
    
    if(B.scheme_defined)
    {
        if(B.fragment_defined)    
            throw XQUERY_EXCEPTION2(FORG0002, "Base URI contains invalid absolute URI. Absolute URI can not contain fragment component (#).");
    }
    else 
        throw XQUERY_EXCEPTION2(FORG0002, "Base URI is relative. Absolute URI was expected.");
    
    Uri T;                        /// Target URI which we will return after recomposition.

    if(R.authority_defined)
    {
        T.authority         = R.authority; 
        T.authority_defined = true;
        if(R.get_path() != NULL)
            T.path          = str_counted_ptr(remove_dot_segments(R.get_path()));
        T.query = R.query; 
        T.query_defined     = R.query_defined; 
    }
    else
    {
        if(R.get_path() == NULL)
        {
           T.path = B.path;
           if(R.query_defined) 
           {
              T.query = R.query;
              T.query_defined = true;
           }
           else
           {
              T.query = B.query;
              T.query_defined = B.query_defined;
           }
        }
        else
        {
           if(R.get_path()[0] == '/')
              T.path = str_counted_ptr(remove_dot_segments(R.get_path()));
           else
           {
             /////////////////////////////////////////////////////////////////////////////////////////////
             /// Merge Paths Algorithm: (from RFC 3986 certainly :))
             /// ===== ===== ==========
             /// If the base URI has a defined authority component and an empty path, then 
             /// return a string consisting of "/" concatenated with the reference's path; otherwise,
             /// return a string consisting of the reference's path component appended to 
             /// all but the last segment of the base URI's path (i.e., excluding any characters after 
             /// the right-most "/" in the base URI path, or excluding the entire base URI path if 
             /// it does not contain any "/" characters).
             /////////////////////////////////////////////////////////////////////////////////////////////

             char* temp_path;
             char* base_path = B.get_path();
             if(B.authority_defined && base_path == NULL)
             {
                 temp_path = new char[strlen(R.get_path()) + 2];
                 temp_path[0] = '/';
                 strcpy(temp_path + 1, R.get_path());
             }
             else
             {
                 int last_slash_pos = -1;
                 int counter        = 0;        

                 if(base_path != NULL)
                 {
                    while(base_path[counter] != '\0')
                    {
                        if(base_path[counter] == '/') last_slash_pos = counter;
                        counter++;
                    }
                 }

                 int base_path_fragment_length = last_slash_pos + 1;

                 temp_path = new char[base_path_fragment_length + strlen(R.get_path()) + 1];
                 if(base_path_fragment_length) 
                     memcpy(temp_path, base_path, base_path_fragment_length);
                 strcpy(temp_path + base_path_fragment_length, R.get_path());
             }
             
             /////////////////////////////////////////////////////////////////////////////////////////////
             /// Now we must have temp_path == merge(B.path, R.path);
             /////////////////////////////////////////////////////////////////////////////////////////////

             T.path = str_counted_ptr(remove_dot_segments(temp_path));
             delete [] temp_path;
           }
           T.query = R.query;
           T.query_defined = R.query_defined;
        }
        T.authority = B.authority;
        T.authority_defined = B.authority_defined;
    }

    T.scheme           = B.scheme;
    T.scheme_defined   = true;
    T.fragment         = R.fragment;
    T.fragment_defined = R.fragment_defined;

    T.recompose(dest);
    return true;
}

void Uri::recompose(stmt_str_buf &dest) const
{
   if (scheme_defined)
   { 
      if (get_scheme() != NULL) dest << get_scheme();
      dest << ":";
   }
   if (authority_defined)
   {
      dest << "//";
      if (get_authority() != NULL) dest << get_authority();
   }
   if (get_path() != NULL) 
   {
       dest << get_path();
   }
   if (query_defined)
   {
      dest << "?";
      if (get_query() != NULL) dest << get_query();
   }
   if (fragment_defined)
   {
      dest << "#";
      if (get_fragment() != NULL) dest << get_fragment();
   }
}

Uri Uri::parse(const char* uri)
{
    PcrePattern re("^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?$", PCRE_UTF8 | PCRE_NO_UTF8_CHECK);
    size_t uri_len = strlen(uri); 
    PcreMatcher<const char*,ptrdiff_t> matcher(re);
    Uri res;

    if (matcher.matches(uri, uri + uri_len, uri, PCRE_NO_UTF8_CHECK))
    {
        ////////////////////////////////////////////////////////////////
        /// If matcher matched our URI it contains the following groups
        ///    $0 = whole parsed string;      -- this group we don't use.
        ///    $1 = scheme + ':';             -- if this group is not empty then scheme component is defined;
        ///    $2 = scheme;                   -- scheme component itself; 
        ///    $3 = '//' + authority;         -- if this group is not empty then authority component is defined;
        ///    $4 = authority;                -- authority component itself; 
        ///    $5 = path;                     -- path component, it could be empty but not undefined;
        ///    $6 = '?' + query;              -- if this group is not empty then query component is defined;
        ///    $7 = query;                    -- query component itself; 
        ///    $8 = '#' + fragment;           -- if this group is not empty then fragment component is defined;
        ///    $9 = fragment;                 -- fragment component itself;    
        /// For the details see RFC 3986.
        //////////////////////////////////////////////////////////////// 
        
        for (int i = 1; i < matcher.groupCount(); i++) 
        {
            const char* s = matcher.start(i);
            const char* e = matcher.end(i);
            size_t group_len = e - s; 
            if( !group_len ) continue;

            switch(i)
            {
                case 1 : res.scheme_defined    = true; continue;
                case 3 : res.authority_defined = true; continue;
                case 6 : res.query_defined     = true; continue;
                case 8 : res.fragment_defined  = true; continue;
            }

            char* component = new char[group_len + 1];
            memcpy(component, s, group_len);
            component[group_len] = '\0';

            switch(i)
            {
                case 2 : res.scheme    = str_counted_ptr(component); break;
                case 4 : res.authority = str_counted_ptr(component); break;
                case 5 : res.path      = str_counted_ptr(component); break;
                case 7 : res.query     = str_counted_ptr(component); break;
                case 9 : res.fragment  = str_counted_ptr(component); break;
                default: throw USER_EXCEPTION2(SE1003, "Impossible case in Uri::parse.");
            }
        }
    }
    else
        throw USER_EXCEPTION2(SE1003, "Invalid URI in Uri::parse.");

    return res;
}
