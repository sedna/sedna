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
WordChar [0-9a-zA-Z_\x80-\xFF\.\-]
Word {WordChar}+
WS [ \t\n\r]+
Number [0-9]+

%s after_word

%%

"CONTAINS" { BEGIN(INITIAL); return ftq_token::CONTAINS; }
"OR"       { BEGIN(INITIAL); return ftq_token::OR; }
"\""       { BEGIN(INITIAL); return ftq_token::QUOT; }
"'"        { BEGIN(INITIAL); return ftq_token::APOS; }
"("        { BEGIN(INITIAL); return ftq_token::BR_OPEN; }
")"        { BEGIN(INITIAL); return ftq_token::BR_CLOSE; }
{Number}   { BEGIN(INITIAL); return ftq_token::NUMBER; }
{Word}     { BEGIN(after_word); return ftq_token::WORD; }
<after_word>"~" { BEGIN(INITIAL); return ftq_token::TILDE_MOD; }
<after_word>":"{Number} { BEGIN(INITIAL); return ftq_token::BOOST_MOD; }
{WS}+      { BEGIN(INITIAL); /* skip */ }

.|\n       { BEGIN(INITIAL); return ftq_token::_ERROR_; }
