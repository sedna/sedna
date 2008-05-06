#
# Makefile for hot-backup process (GNU Make)
#

PP = ..

include $(PP)/Makefile.include

all: $(PP)/bin/se_hbp$(EXE_EXT)
	@echo ===================================================================
	@echo HBP DONE
	@echo ===================================================================

OBJS = hot_backup$(OBJ_EXT) hb_aux$(OBJ_EXT) hb_main$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifndef NO_DEP
include $(OBJS:$(OBJ_EXT)=.d)
endif
endif

################################################################################
# HBP                                                                          #
################################################################################
$(PP)/bin/se_hbp$(EXE_EXT): $(OBJS)
	$(LD) $(LFLAGS) $(LDOUT)$@ $^ $(LSLIBS)

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
