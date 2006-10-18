
#header
<<
#include "sedna.h"
#include "ANTLRToken.h"
#include "XQueryDLGLexer.h"
#include "AST.h"
#include "exceptions.h"
#include "flwr.h"
#include "quantifier.h"
#include "parserutils.h"

#include <iostream>
#include <string>

extern XQueryDLGLexer* my_lexer;
extern bool is_preserve_boundary_space;
>>