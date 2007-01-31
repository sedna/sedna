#
# Makefile for llmgr (GNU make)
#

PP = ../../..

include $(PP)/Makefile.include

all: lm$(LIB_EXT) 
	@echo ===================================================================
	@echo Lock Manager has been Done
	@echo ===================================================================

OBJS = lm_globals$(OBJ_EXT) lock_table$(OBJ_EXT) trans_table$(OBJ_EXT) \
       TransCB$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# LL_LIB                                                                       #
################################################################################
lm$(LIB_EXT): $(OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
