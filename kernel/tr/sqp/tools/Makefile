#
# Makefile for tools
#

PP = ../../../..

include $(PP)/Makefile.include

all: tools$(LIB_EXT)
	@echo ===================================================================
	@echo tools DONE
	@echo ===================================================================

OBJS = impl-differences$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# tools.lib                                                                   #
################################################################################
tools$(LIB_EXT): $(OBJS)
	$(PLD) $(PLDOUT)$@ $^

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) impl-differences.c
