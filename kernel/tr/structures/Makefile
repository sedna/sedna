#
# Makefile for structures (GNU make)
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
               $(PP)/kernel/tr/mo $(PP)/kernel/tr/vmm $(PP)/kernel/tr/pstr	\
               $(PP)/kernel/common/ph $(PP)/kernel/tr/executor/base			\
               $(PP)/kernel/tr/nid $(PP)/kernel/tr/crmutils					\
               $(PP)/kernel/tr/log $(PP)/kernel/sm/llmgr $(PP)/kernel/tr/strings				\
               $(PP)/kernel/tr/idx/btree $(PP)/kernel/tr/locks $(PP)/kernel/tr/idx					\
               $(PP)/kernel/tr/updates $(PP)/kernel/tr/executor/por2qep $(PP)/kernel/tr/ft \
               $(PP)/kernel/sm/plmgr

VPATH = .


include $(PP)/Makefile.include

all: libstructures$(LIB_EXT)
	@echo ===================================================================
	@echo STRUCTURES DONE
	@echo ===================================================================

include $(PP)/Makefile.depend

depend:
	$(DEPACT)


ifneq ($(words $(wildcard depend)), 0)
include depend
endif

STRUCTURES_LIB = libstructures$(LIB_EXT)

################################################################################
# STRUCTURES                                                                   #
################################################################################
$(STRUCTURES_LIB): indirection$(OBJ_EXT) 	\
                   metadata$(OBJ_EXT)		\
                   nodes$(OBJ_EXT)			\
                   schema$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) $(STRUCTURES_LIB)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) *.SUP
