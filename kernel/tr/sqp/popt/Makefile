#
# Makefile for popt
#

PP = ../../../..

include $(PP)/Makefile.include

OBJS = att$(OBJ_EXT) att_xpath$(OBJ_EXT) cpp_popt$(OBJ_EXT) \
       popt_plan$(OBJ_EXT) serialize2lr$(OBJ_EXT) popt$(OBJ_EXT)

CUSTOM_CLEAN = clean_generated

include $(PP)/Makefile.pseudolib

clean_generated:
	-$(REMOVE) popt.c
