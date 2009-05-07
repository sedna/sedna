#!/bin/bash

#
# Prepares Sedna parser for source distribution
#

set -u
set -e

PP=../../..

# Generate Lexer and Parser

flex -Cf XQLexer.l
bison XQParser.y

# Copy locally FlexLexer.h since we do not want to depend on
# system's version and location of this file.

cp $PP/libs/compat/FlexLexer.h .

# Force compiler to include local version of the FlexLexer.h

sed -e 's|^#include <FlexLexer.h>|#include "tr/xqp/FlexLexer.h"|g' XQueryLexer.cpp > XQueryLexer.cpp.release
rm -f XQueryLexer.cpp
mv XQueryLexer.cpp.release XQueryLexer.cpp

# Replace Makefile with release version

mv Makefile Makefile.development
mv Makefile.release Makefile
