#
# Makefile for lr2por
#

PP = ../../../..

include $(PP)/Makefile.include

all: lr2por$(LIB_EXT)
	@echo ===================================================================
	@echo lr2por DONE
	@echo ===================================================================

OBJS = lr2por$(OBJ_EXT) lr2por-lib$(OBJ_EXT) por-cons$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# lr2por.lib                                                                   #
################################################################################
lr2por$(LIB_EXT): $(OBJS)
	$(PLD) $(PLDOUT)$@ $^

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) lr2por.c
	-$(REMOVE) lr2por-lib.c
	-$(REMOVE) por-cons.c
