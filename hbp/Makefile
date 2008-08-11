#
# Makefile for hot-backup process (GNU Make)
#

PP = ..

include $(PP)/Makefile.include

all: $(PP)/bin/se_hb$(EXE_EXT)
	@echo ===================================================================
	@echo HBP DONE
	@echo ===================================================================

OBJS = hot_backup$(OBJ_EXT) hb_aux$(OBJ_EXT) hb_main$(OBJ_EXT) hb_files$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifndef NO_DEP
include $(OBJS:$(OBJ_EXT)=.d)
endif
endif

COMMON_FILES_LIB = $(PP)/kernel/common/common_files$(LIB_EXT)

################################################################################
# HBP                                                                          #
################################################################################
$(PP)/bin/se_hb$(EXE_EXT): $(OBJS) $(COMMON_FILES_LIB)
	$(LD) $(LFLAGS) $(LDOUT)$@ $^ $(LSLIBS)

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
