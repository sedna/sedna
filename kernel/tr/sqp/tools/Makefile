#
# Makefile for tools
#

PP = ../../../..

include $(PP)/Makefile.include

OBJS = impl-differences$(OBJ_EXT)

CUSTOM_CLEAN = clean_generated

include $(PP)/Makefile.pseudolib

clean_generated:
	-$(REMOVE) impl-differences.c
