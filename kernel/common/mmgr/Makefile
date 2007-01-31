#
# Makefile for mmgr (GNU make)
#

PP = ../../..

include $(PP)/Makefile.include

all: mmgr$(LIB_EXT) 
	@echo ===================================================================
	@echo mmgr has been Done
	@echo ===================================================================

OBJS = aset$(OBJ_EXT) mcxt$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# mmgr$(LIB_EXT)                                                               #
################################################################################
mmgr$(LIB_EXT): $(OBJS)
	$(PLD) $(PLDOUT)$@ $^

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
