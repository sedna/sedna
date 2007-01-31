#
# Makefile for por2qep (GNU make)
#

PP = ../../../..

include $(PP)/Makefile.include

all: por2qep$(LIB_EXT)
	@echo ===================================================================
	@echo por2qep DONE
	@echo ===================================================================

OBJS = ext$(OBJ_EXT) por2qep$(OBJ_EXT) scheme_tree$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# CRMUTILS                                                                          #
################################################################################
por2qep$(LIB_EXT): $(OBJS)
	$(PLD) $(PLDOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
