#
# Makefile for governor (GNU Make)
#

PP = ../..

include $(PP)/Makefile.include

all: $(PP)/bin/se_gov$(EXE_EXT) $(PP)/bin/se_stop$(EXE_EXT) $(PP)/bin/se_rc$(EXE_EXT) 
	@echo ===================================================================
	@echo GOV DONE
	@echo ===================================================================

GOV_OBJS  = gov_globals$(OBJ_EXT) gov_table$(OBJ_EXT) listener$(OBJ_EXT) \
            gov_functions$(OBJ_EXT) gov$(OBJ_EXT)
STOP_OBJS = stop_server$(OBJ_EXT)
RC_OBJS   = rc$(OBJ_EXT)
OBJS      = $(GOV_OBJS) $(STOP_OBJS) $(RC_OBJS)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

COMMON_FILES_LIB = $(PP)/kernel/common/common_files$(LIB_EXT)
#                   $(PP)/kernel/common/errdbg/errdbg$(LIB_EXT) \
#                   $(PP)/kernel/common/u/u$(LIB_EXT) \
#                   $(PP)/kernel/common/mmgr/mmgr$(LIB_EXT)

################################################################################
# GOV                                                                          #
################################################################################
$(PP)/bin/se_gov$(EXE_EXT): $(GOV_OBJS) $(COMMON_FILES_LIB)
	$(LD) $(LFLAGS) $(LDOUT)$@ $^ $(LSLIBS)

################################################################################
# STOP SERVER                                                                  #
################################################################################
$(PP)/bin/se_stop$(EXE_EXT): $(STOP_OBJS) $(COMMON_FILES_LIB)
	$(LD) $(LFLAGS) $(LDOUT)$@ $^ $(LSLIBS)

################################################################################
# RC                                                                           #
################################################################################
$(PP)/bin/se_rc$(EXE_EXT): $(RC_OBJS) $(COMMON_FILES_LIB)
	$(LD) $(LFLAGS) $(LDOUT)$@ $^ $(LSLIBS)

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
