/*
 * File:  parserutils.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef __PARSERUTILS_H
#define __PARSERUTILS_H

#include "common/sedna.h"
#include <string>

/// Basic method. Remove duplication of quotes/double quotes in 
/// XQuery string literals and prepare them to be passed into
/// Scheme part, i.e. escapes back slashes and double quotes.
std::string scheme_prepare_string         (const char* lex_text);

/// Addidtional helpers to remove XQuery syntax "sugar".
/// Note! "replace_entity" replaces lex_text buffer with 
/// new value of string
void replace_entity                       (char* lex_text, const char* find_ent, const char* replc_ent);
std::string replace_charref               (const char* lex_text);

/// Some usefull helpers
void escape_double_quote                  (std::string& text, bool escaped_escape);
void escape_back_slash                    (std::string& text);

char *encoding_processing                 (const char *query);

/// Parse raw content of the PI direct constructor.
/// Can raise XPST0003 (line used in exception details) if name
/// is not valid upon NAME production or reserved (XML).
/// Returs name and content of the PI.
void parse_processing_instruction         (const std::string& raw, 
                                 /* out */ std::string& name, 
                                 /* out */ std::string& content, 
/* used to form exceptions' description */ int line);


#endif  /* __PARSERUTILS_H */
