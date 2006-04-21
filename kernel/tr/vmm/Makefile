#
# Makefile for VMM (GNU make)
#

PP = ../../..


VPATH = $(PP)/kernel/tr


include $(PP)/Makefile.include

all: libvmm$(LIB_EXT)
	@echo ===================================================================
	@echo VMM DONE
	@echo ===================================================================

test: vmm_test$(EXE_EXT)


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/tr/depend
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
	-$(REMOVE) vmm_test$(EXE_EXT)
