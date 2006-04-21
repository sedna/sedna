#
# Makefile for plmgr (GNU make)
#

PP = ../../..


VPATH = $(PP)/kernel/sm


include $(PP)/Makefile.include

all: libsm_phys_log_mgr$(LIB_EXT) libtr_phys_log_mgr$(LIB_EXT)
	@echo ===================================================================
	@echo Low Level Physical Log has been Done
	@echo ===================================================================

test: plmgr_test$(EXE_EXT) sector_test$(EXE_EXT)
	@echo ===================================================================
	@echo PLMGR TEST HAS BEEN DONE
	@echo ===================================================================


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/sm/depend
endif


COMMON_FILES_LIB = $(PP)/kernel/common/common_files$(LIB_EXT)

################################################################################
# PL_MGR_LIB                                                                          #
################################################################################
libsm_phys_log_mgr$(LIB_EXT): plmgr$(OBJ_EXT)			\
                              plmgr_helpers$(OBJ_EXT)	\
                              plmgr_core$(OBJ_EXT)		\
                              sm_plmgr$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

libtr_phys_log_mgr$(LIB_EXT): plmgr_helpers$(OBJ_EXT)	\
                              plmgr_core$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# PL_MGR_TEST                                                                          #
################################################################################
plmgr_test$(EXE_EXT): phys_log_mgr$(LIB_EXT) plmgr_test$(OBJ_EXT)
	$(LD) $(LFLAGS) $(LDOUT)$@ phys_log_mgr$(LIB_EXT) plmgr_test$(OBJ_EXT) $(COMMON_FILES_LIB)

sector_test$(EXE_EXT): $(COMMON_FILES_LIB) sector_test$(OBJ_EXT)
	$(LD) $(LFLAGS) $(LDOUT)$@ sector_test$(OBJ_EXT)  $(COMMON_FILES_LIB)
################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) *$(LIB_EXT)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) plmgr_test$(EXE_EXT)
	-$(REMOVE) sector_test$(EXE_EXT)

