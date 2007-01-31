#
# Makefile for ph (GNU make)
#

PP = ../../..

include $(PP)/Makefile.include

all: ph$(LIB_EXT) 
	@echo ===================================================================
	@echo ph has been Done
	@echo ===================================================================

OBJS = pers_free$(OBJ_EXT) pers_heap$(OBJ_EXT) pers_malloc$(OBJ_EXT) \
       pers_realloc$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# ph$(LIB_EXT)                                                                 #
################################################################################
ph$(LIB_EXT): $(OBJS)
	$(PLD) $(PLDOUT)$@ $^

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
