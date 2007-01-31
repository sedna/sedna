#
# Makefile for llmgr (GNU make)
#

PP = ../../..

include $(PP)/Makefile.include

all: llmgr$(LIB_EXT) 
	@echo ===================================================================
	@echo Logical Log has been Done
	@echo ===================================================================

OBJS = llmgr$(OBJ_EXT) llmgr_core$(OBJ_EXT) llmgr_helpers$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# LLMGR_LIB                                                                    #
################################################################################
llmgr$(LIB_EXT): $(OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
