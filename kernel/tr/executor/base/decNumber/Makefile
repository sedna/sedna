#
# Makefile for decNumber (GNU make)
#

PP = ../../../../..

include $(PP)/Makefile.include

all: decNumber$(LIB_EXT)
	@echo ===================================================================
	@echo decNumber DONE
	@echo ===================================================================

OBJS = decContext$(OBJ_EXT) decimal128$(OBJ_EXT) decimal64$(OBJ_EXT) decNumber$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# CRMUTILS                                                                          #
################################################################################
decNumber$(LIB_EXT): $(OBJS)
	$(PLD) $(PLDOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
