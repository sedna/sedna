#
# Makefile for u (GNU make)
#

PP = ../../..

include $(PP)/Makefile.include

all: u$(LIB_EXT)
	@echo ===================================================================
	@echo u has been Done
	@echo ===================================================================

OBJS = u$(OBJ_EXT) uhdd$(OBJ_EXT) ummap$(OBJ_EXT) umutex$(OBJ_EXT) \
       upipe$(OBJ_EXT) uprocess$(OBJ_EXT) usecurity$(OBJ_EXT) usem$(OBJ_EXT) \
       ushm$(OBJ_EXT) usocket$(OBJ_EXT) uthread$(OBJ_EXT) utime$(OBJ_EXT) \
       uutils$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# u$(LIB_EXT)                                                                  #
################################################################################
u$(LIB_EXT): $(OBJS)
	$(PLD) $(PLDOUT)$@ $^

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
