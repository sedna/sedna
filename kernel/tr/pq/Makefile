#
# Makefile for pq (GNU make)
#

PP = ../../..

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifeq ($(words $(wildcard depend)), 0)
ifneq ($(findstring depend, $(MAKECMDGOALS)), depend)
$(error Type "make depend" first)
endif
endif
endif


INCLUDE_DIRS = . $(PP)/kernel/common $(PP)/kernel/common/u $(PP)/kernel/tr	\
               $(PP)/kernel/common/ph $(PP)/driver/c                            \
               $(PP)/libs/chicken $(PP)/kernel/tr/xqp				\
               $(PP)/kernel/tr/executor/por2qep                                 \
               $(PP)/kernel/tr/crmutils $(PP)/kernel/tr/ft $(PP)/kernel/tr/strings                                        \
               $(PP)/kernel/tr/executor/base $(PP)/kernel/tr/mo                                   \
               $(PP)/kernel/tr/vmm $(PP)/kernel/tr/structures					\
               $(PP)/kernel/tr/nid $(PP)/kernel/tr/pstr

VPATH = .


include $(PP)/Makefile.include

all: libpq$(LIB_EXT)
	@echo ===================================================================
	@echo PQ DONE
	@echo ===================================================================

include $(PP)/Makefile.depend

depend:
	$(DEPACT)


ifneq ($(words $(wildcard depend)), 0)
include depend
endif


PQ_LIB = libpq$(LIB_EXT)

################################################################################
# Prepare Query                                                                #
################################################################################

scm-debug.scm: scm-debug.scm.m4
	$(M4) -D EL_DEBUG=$(EL_DEBUG) < $< > $@

PQ_OBJS = pq$(OBJ_EXT) 					\
          scm-entry$(OBJ_EXT)			\
          scm-debug$(OBJ_EXT)


$(PQ_LIB): $(PQ_OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^



################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) $(PQ_LIB)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) *.SUP
	-$(REMOVE) scm-debug.scm
