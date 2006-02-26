#
# Makefile for mo (GNU make)
#

PP = ../../..

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifeq ($(words $(wildcard depend)), 0)
ifneq ($(findstring depend, $(MAKECMDGOALS)), depend)
$(error Type "make depend" first)
endif
endif
endif


INCLUDE_DIRS = . $(PP)/kernel/common $(PP)/kernel/common/u $(PP)/driver/c		\
               $(PP)/kernel/tr/vmm $(PP)/kernel/tr/locks					\
               $(PP)/kernel/tr/pstr $(PP)/kernel/tr/log						\
               $(PP)/kernel/tr/crmutils $(PP)/kernel/common/ph				\
               $(PP)/kernel/tr/executor/base $(PP)/kernel/sm/plmgr			\
               $(PP)/kernel/sm/llmgr $(PP)/kernel/tr/structures				\
               $(PP)/kernel/tr/nid $(PP)/kernel/tr/idx $(PP)/kernel/tr/updates $(PP)/kernel/tr/ft						\
               $(PP)/kernel/tr/executor/por2qep

VPATH = .


include $(PP)/Makefile.include

all: libmo$(LIB_EXT)
	@echo ===================================================================
	@echo MO DONE
	@echo ===================================================================

include $(PP)/Makefile.depend

depend:
	$(DEPACT)


ifneq ($(words $(wildcard depend)), 0)
include depend
endif

MO_LIB = libmo$(LIB_EXT)

################################################################################
# MO                                                                          #
################################################################################
$(MO_LIB):    micro$(OBJ_EXT) mo_write_utils$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) $(MO_LIB)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) *.SUP
