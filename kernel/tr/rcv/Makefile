#
# Makefile for rcv (GNU make)
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
               $(PP)/kernel/sm/llmgr  $(PP)/kernel/tr/log  $(PP)/kernel/tr/mo 	\
               $(PP)/kernel/tr/vmm $(PP)/kernel/common/ph ..                	\
               $(PP)/kernel/tr/crmutils $(PP)/kernel/tr/structures          	\
               $(PP)/kernel/tr/executor/base $(PP)/kernel/tr/nid				\
               $(PP)/kernel/tr/executor/por2qep $(PP)/kernel/tr/idx				\
               $(PP)/kernel/tr/idx/btree $(PP)/kernel/tr/pstr $(PP)/kernel/tr/strings $(PP)/kernel/tr/ft


VPATH = .


include $(PP)/Makefile.include

all: librcv$(LIB_EXT)
	@echo ===================================================================
	@echo Rcv Library Has Been Done
	@echo ===================================================================

include $(PP)/Makefile.depend

depend:
	$(DEPACT)


ifneq ($(words $(wildcard depend)), 0)
include depend
endif

RCV_LIB = librcv$(LIB_EXT)

################################################################################
# LOCKS                                                                          #
################################################################################
$(RCV_LIB):    rcv_funcs$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) $(LOCKS_LIB)
	-$(REMOVE) *$(LIB_EXT)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) *.SUP
