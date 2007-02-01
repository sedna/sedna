#
# Makefile for sqp
#

PP = ../../..

include $(PP)/Makefile.include

OBJS = common-lib$(OBJ_EXT) dschema$(OBJ_EXT) dschema-utils$(OBJ_EXT) \
       xquery-lr-lib$(OBJ_EXT)
SUBDIRS = lr2por lropt popt sa tools

CUSTOM_CLEAN = clean_generated

include $(PP)/Makefile.ordlib


clean_generated:
	-$(REMOVE) common-lib.c dschema.c dschema-utils.c xquery-lr-lib.c
