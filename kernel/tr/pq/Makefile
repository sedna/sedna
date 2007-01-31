#
# Makefile for pq
#

PP = ../../..

include $(PP)/Makefile.include

all: pq$(LIB_EXT)
	@echo ===================================================================
	@echo pq DONE
	@echo ===================================================================

OBJS = pq$(OBJ_EXT) modules$(OBJ_EXT) scm-entry$(OBJ_EXT) scm-debug$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# pq.lib                                                                      #
################################################################################
pq$(LIB_EXT): $(OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


scm-debug.scm: scm-debug.scm.m4
	$(M4) -D EL_DEBUG=$(EL_DEBUG) < $< > $@

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) scm-debug.scm
	-$(REMOVE) scm-debug.c
	-$(REMOVE) scm-entry.c
