#
# Makefile for VMM (GNU make)
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
               $(PP)/kernel/common/ph $(PP)/kernel/tr						\
               $(PP)/kernel/tr/crmutils	$(PP)/kernel/tr/ft $(PP)/kernel/tr/strings									\
               $(PP)/kernel/tr/executor/por2qep $(PP)/kernel/tr/executor/base $(PP)/kernel/tr/structures		\
               $(PP)/kernel/tr/nid $(PP)/kernel/tr/pstr $(PP)/kernel/tr/mo


VPATH = .


include $(PP)/Makefile.include

all: libvmm$(LIB_EXT)
	@echo ===================================================================
	@echo VMM DONE
	@echo ===================================================================

include $(PP)/Makefile.depend

depend:
	$(DEPACT)

test: vmm_test$(EXE_EXT)

ifneq ($(words $(wildcard depend)), 0)
include depend
endif

VMM_LIB = libvmm$(LIB_EXT)
COMMON_FILES_LIB = $(PP)/kernel/common/libcommon_files$(LIB_EXT)

################################################################################
# VMM                                                                          #
################################################################################
$(VMM_LIB):    vmm$(OBJ_EXT) os_exceptions$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

################################################################################
# VMM_TEST                                                                     #
################################################################################

VMM_TEST_OBJS = vmm_test$(OBJ_EXT)			\
                os_exceptions$(OBJ_EXT)		\
                libvmm$(LIB_EXT)			\
               $(COMMON_FILES_LIB)

vmm_test$(EXE_EXT): $(VMM_TEST_OBJS)
	$(LD) $(LFLAGS) $(LDOUT)$@ $(VMM_TEST_OBJS) $(LSLIBS)



################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) $(VMM_LIB)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb
	-$(REMOVE) vmm_test$(EXE_EXT)
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) *.SUP
