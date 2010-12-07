    /*
    * File:  ftq_lexer.l
    * Copyright (C) 2010 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
    */

%top{
#ifdef _WIN32
    #define YY_NO_UNISTD_H 1
    #include <stdint.h>
#endif
}

%{
#include "tr/strings/strings_base.h"
#include "tr/ft/query/ftq_lex_types.h"
%}

/* we don't include unistd.h, so need to get rid of isatty call */
%option never-interactive

%option reentrant stack noyywrap
%option extra-type="str_cursor_reader *"
%option nounput

%option prefix="ftq_"

%{

#define YY_INPUT(buf,result,max_size) \
        if ((result = yyextra->read(buf, max_size)) <= 0) \
			result = YY_NULL; \

%}

    /* we don't care that some non-ascii chars aren't letters here, since all special characters (incl. whitespace) are ascii for now */
WordChar [a-zA-Z_\x80-\xFF]
Word {WordChar}+
WS [ \t\n\r]+

%%

{Word}   { return ftq_token::WORD; }
{WS}+    { /* skip */ }
"\""     { return ftq_token::QUOT; }
"'"      { return ftq_token::APOS; }

.|\n     { return ftq_token::_ERROR_; }
