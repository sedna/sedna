#
# Makefile for llmgr (GNU make)
#

PP = ../../..


VPATH = $(PP)/kernel/sm


include $(PP)/Makefile.include

all: liblogical_log_mgr$(LIB_EXT) 
	@echo ===================================================================
	@echo Logical Log has been Done
	@echo ===================================================================

test: test.exe
	@echo ===================================================================
	@echo Logical Log Test has been Done
	@echo ===================================================================


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/sm/depend
endif


COMMON_FILES_LIB = $(PP)/kernel/common/libcommon_files$(LIB_EXT)


################################################################################
# LL_MGR_LIB                                                                          #
################################################################################
liblogical_log_mgr$(LIB_EXT): llmgr_core$(OBJ_EXT) 	\
                              llmgr$(OBJ_EXT) 		\
                           llmgr_helpers$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

################################################################################
# TEST                                                                          #
################################################################################

test.exe: liblogical_log_mgr$(LIB_EXT) \
          $(COMMON_FILES_LIB) \
          test$(OBJ_EXT)
	$(LD) $(LFLAGS) $(LDOUT)$@ liblogical_log_mgr$(LIB_EXT) $(COMMON_FILES_LIB) test$(OBJ_EXT)

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) *$(LIB_EXT)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
