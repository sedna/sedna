#
# Makefile for plmgr (GNU make)
#

PP = ../../..

include $(PP)/Makefile.include

all: plmgr$(LIB_EXT) plmgr_tr$(LIB_EXT)
	@echo ===================================================================
	@echo Low Level Physical Log has been Done
	@echo ===================================================================

TR_OBJS = plmgr_helpers$(OBJ_EXT) plmgr_core$(OBJ_EXT)
OBJS = $(TR_OBJS) plmgr$(OBJ_EXT) sm_plmgr$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif


################################################################################
# PLMGR_LIB                                                                    #
################################################################################
plmgr$(LIB_EXT): $(OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

plmgr_tr$(LIB_EXT): $(TR_OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean

