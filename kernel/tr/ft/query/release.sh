#!/bin/bash

#
# Prepares Sedna full-text query parser for source distribution
#

set -e

# Generate Lexer

flex -Cf -8 -o ftq_lexer.cpp --header-file=ftq_lexer.h ftq_lexer.l
