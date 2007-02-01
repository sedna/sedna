#
# Makefile for pq
#

PP = ../../..

include $(PP)/Makefile.include

OBJS = pq$(OBJ_EXT) modules$(OBJ_EXT) scm-entry$(OBJ_EXT) scm-debug$(OBJ_EXT)

CUSTOM_CLEAN = clean_generated

include $(PP)/Makefile.ordlib

scm-debug.scm: scm-debug.scm.m4
	$(M4) -D EL_DEBUG=$(EL_DEBUG) < $< > $@

################################################################################
# Clean                                                                        #
################################################################################
clean_generated: 
	-$(REMOVE) scm-debug.scm
	-$(REMOVE) scm-debug.c
	-$(REMOVE) scm-entry.c
