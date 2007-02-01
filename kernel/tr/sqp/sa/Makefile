#
# Makefile for sa
#

PP = ../../../..

include $(PP)/Makefile.include

OBJS = sa$(OBJ_EXT)

CUSTOM_CLEAN = clean_generated

include $(PP)/Makefile.pseudolib

clean_generated:
	-$(REMOVE) sa.c
