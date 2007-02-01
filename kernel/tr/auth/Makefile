#
# Makefile for authentication
#

PP = ../../..

include $(PP)/Makefile.include

OBJS = auth$(OBJ_EXT) auc$(OBJ_EXT) prec-qs$(OBJ_EXT)

CUSTOM_CLEAN = clean_generated

include $(PP)/Makefile.ordlib

clean_generated:
	-$(REMOVE) auth.c prec-qs.c
