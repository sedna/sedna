#
# Makefile for llmgr (GNU make)
#

PP = ../../..

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifeq ($(words $(wildcard depend)), 0)
ifneq ($(findstring depend, $(MAKECMDGOALS)), depend)
$(error Type "make depend" first)
endif
endif
endif


INCLUDE_DIRS = $(PP)/kernel/common $(PP)/kernel/common/u $(PP)/driver/c ..  \
               $(PP)/kernel/common/ph $(PP)/kernel/sm/plmgr $(PP)/kernel/sm/bufmgr

VPATH = .


include $(PP)/Makefile.include

all: liblogical_log_mgr$(LIB_EXT) 
	@echo ===================================================================
	@echo Logical Log has been Done
	@echo ===================================================================

test: test.exe
	@echo ===================================================================
	@echo Logical Log Test has been Done
	@echo ===================================================================


include $(PP)/Makefile.depend

depend:
	$(DEPACT)


ifneq ($(words $(wildcard depend)), 0)
include depend
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
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) *.SUP
