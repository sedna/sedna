#
# Makefile for llmgr (GNU make)
#

PP = ../../..

include $(PP)/Makefile.include

all: llmgr$(LIB_EXT) tr_llmgr$(LIB_EXT)
	@echo ===================================================================
	@echo Low Level Logical Log has been Done
	@echo ===================================================================

TR_OBJS = llmgr_helpers$(OBJ_EXT) llmgr_core$(OBJ_EXT)
OBJS = $(TR_OBJS) llmgr$(OBJ_EXT) sm_llmgr$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifndef NO_DEP
include $(OBJS:$(OBJ_EXT)=.d)
endif
endif


################################################################################
# PLMGR_LIB                                                                    #
################################################################################
llmgr$(LIB_EXT): $(OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

tr_llmgr$(LIB_EXT): $(TR_OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean

