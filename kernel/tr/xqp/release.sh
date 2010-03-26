#!/bin/bash

#
# Prepares Sedna parser for source distribution
#

set -e

# Generate Lexer and Parser

flex -Cf -o XQueryLexer.cpp XQLexer.l
bison -o XQueryParser.cpp XQParser.y 
