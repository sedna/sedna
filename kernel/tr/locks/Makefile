#
# Makefile for locks (GNU make)
#

PP = ../../..

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifeq ($(words $(wildcard depend)), 0)
ifneq ($(findstring depend, $(MAKECMDGOALS)), depend)
$(error Type "make depend" first)
endif
endif
endif


INCLUDE_DIRS = . $(PP)/kernel/common $(PP)/kernel/common/u $(PP)/driver/c			\
               $(PP)/kernel/tr/vmm $(PP)/kernel/common/ph					\
               $(PP)/kernel/sm/plmgr $(PP)/kernel/tr/executor/base			\
               $(PP)/kernel/tr/structures $(PP)/kernel/tr/nid				\
               $(PP)/kernel/tr/crmutils $(PP)/kernel/tr/ft $(PP)/kernel/tr/strings .. $(PP)/kernel/tr					\
               $(PP)/kernel/tr/executor/por2qep $(PP)/kernel/tr/pstr $(PP)/kernel/tr/mo

VPATH = .


include $(PP)/Makefile.include

all: liblocks$(LIB_EXT)
	@echo ===================================================================
	@echo LOCKS DONE
	@echo ===================================================================

include $(PP)/Makefile.depend

depend:
	$(DEPACT)


ifneq ($(words $(wildcard depend)), 0)
include depend
endif

LOCKS_LIB = liblocks$(LIB_EXT)

################################################################################
# LOCKS                                                                          #
################################################################################
$(LOCKS_LIB):    locks$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) $(LOCKS_LIB)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) *.SUP
