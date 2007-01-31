
#header
<<
#include "common/sedna.h"
#include "tr/xqp/ANTLRToken.h"
#include "tr/xqp/XQueryDLGLexer.h"
#include "tr/xqp/AST.h"
#include "common/errdbg/exceptions.h"
#include "tr/xqp/flwr.h"
#include "tr/xqp/quantifier.h"
#include "tr/xqp/parserutils.h"

#include <iostream>
#include <string>

extern XQueryDLGLexer* my_lexer;
extern bool is_preserve_boundary_space;
>>