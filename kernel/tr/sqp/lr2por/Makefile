#
# Makefile for lr2por
#

PP = ../../../..

include $(PP)/Makefile.include

OBJS = lr2por$(OBJ_EXT) lr2por-lib$(OBJ_EXT) por-cons$(OBJ_EXT)

CUSTOM_CLEAN = clean_generated

include $(PP)/Makefile.pseudolib

clean_generated:
	-$(REMOVE) lr2por.c lr2por-lib.c por-cons.c
