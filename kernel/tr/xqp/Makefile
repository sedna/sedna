#
# Makefile for XQuery parser
#

PP = ../../..

include $(PP)/Makefile.include

OBJS = XQueryDriver$(OBJ_EXT) XQueryParser$(OBJ_EXT) XQueryLexer$(OBJ_EXT) XQuerytoLR$(OBJ_EXT)

GENERATED = XQueryParser.cpp XQueryParser.hpp XQueryLexer.cpp position.hh location.hh stack.hh lex.backup XQueryParser.output

CUSTOM_CLEAN = clean_generated

SUBDIRS = ast visitor

include $(PP)/Makefile.ordlib

################################################################################
# generated                                                                    #
################################################################################
generated: gen_lexer gen_parser
	touch generated

gen_lexer: XQLexer.l
	flex -b -p -L -Cf XQLexer.l
	touch gen_lexer

gen_parser: XQParser.y
	bison -v -l XQParser.y
	touch gen_parser

################################################################################
# clean_generated                                                              #
################################################################################
clean_generated:
	-$(REMOVE) $(GENERATED)
	-$(REMOVE) gen_lexer gen_parser
