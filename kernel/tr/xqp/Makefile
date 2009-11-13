#
# Makefile for XQuery parser (development version)
#

#################################################
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
# Note! DON'T FORGET TO CHANGE Makefile.release!#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
#################################################

PP = ../../..

include $(PP)/Makefile.include

#################################################
# Note! DON'T FORGET TO CHANGE Makefile.release!#
#################################################
OBJS = XQueryDriver$(OBJ_EXT) XQueryParser$(OBJ_EXT) XQueryLexer$(OBJ_EXT) XQuerytoLR$(OBJ_EXT) XQueryModule$(OBJ_EXT) XQFunction$(OBJ_EXT)

GENERATED = XQueryParser.cpp XQueryParser.hpp XQueryLexer.cpp position.hh location.hh stack.hh lex.backup XQueryParser.output

CUSTOM_CLEAN = clean_generated

#################################################
# Note! DON'T FORGET TO CHANGE Makefile.release!#
#################################################
SUBDIRS = ast visitor sema serial lr lreturn



include $(PP)/Makefile.ordlib

################################################################################
# generated                                                                    #
################################################################################
generated: gen_lexer gen_parser
	touch generated

gen_lexer: XQLexer.l
	flex -b -p -Cf XQLexer.l
	touch gen_lexer

gen_parser: XQParser.y
	bison -v XQParser.y
	touch gen_parser

################################################################################
# clean_generated                                                              #
################################################################################
clean_generated:
	-$(REMOVE) $(GENERATED)
	-$(REMOVE) gen_lexer gen_parser


#################################################
# Note! DON'T FORGET TO CHANGE Makefile.release!#
#################################################


