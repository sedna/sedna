#
# Makefile for updates (GNU make)
#

PP = ../../..

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifeq ($(words $(wildcard depend)), 0)
ifneq ($(findstring depend, $(MAKECMDGOALS)), depend)
$(error Type "make depend" first)
endif
endif
endif


INCLUDE_DIRS = . $(PP)/kernel/common $(PP)/kernel/common/u $(PP)/driver/c	\
               $(PP)/kernel/tr/strings $(PP)/kernel/tr/vmm $(PP)/kernel/tr/mo						\
               $(PP)/kernel/tr/executor/base $(PP)/kernel/tr/crmutils		\
               $(PP)/kernel/common/ph $(PP)/kernel/tr/executor/xqops		\
               $(PP)/kernel/sm/plmgr $(PP)/kernel/tr/structures				\
               $(PP)/kernel/tr/nid $(PP)/kernel/tr/auth						\
               $(PP)/kernel/tr/executor/por2qep $(PP)/kernel/tr/strings		\
               $(PP)/kernel/tr/pstr $(PP)/kernel/tr/ft

VPATH = .


include $(PP)/Makefile.include

all: libupdates$(LIB_EXT) 
	@echo ===================================================================
	@echo UPDATES DONE
	@echo ===================================================================

include $(PP)/Makefile.depend

depend:
	$(DEPACT)


ifneq ($(words $(wildcard depend)), 0)
include depend
endif

UPDATES_LIB = libupdates$(LIB_EXT)

################################################################################
# UPDATES                                                                          #
################################################################################
$(UPDATES_LIB): updates$(OBJ_EXT)		\
                insertions$(OBJ_EXT)	\
                deletions$(OBJ_EXT)		\
                replace$(OBJ_EXT)		\
                other_updates$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) $(UPDATES_LIB)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) *.SUP
