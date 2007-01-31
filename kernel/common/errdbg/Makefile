#
# Makefile for errdbg (GNU make)
#

PP = ../../..

include $(PP)/Makefile.include

all: errdbg$(LIB_EXT) 
	@echo ===================================================================
	@echo errdbg has been Done
	@echo ===================================================================

OBJS = assert$(OBJ_EXT) d_printf$(OBJ_EXT) event_log$(OBJ_EXT) \
       exceptions$(OBJ_EXT) error_codes$(OBJ_EXT) \
       error_codes_scm$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include generated
include $(OBJS:$(OBJ_EXT)=.d)

$(OBJS:$(OBJ_EXT)=.d): generated
endif

################################################################################
# errdbg$(LIB_EXT)                                                             #
################################################################################
errdbg$(LIB_EXT): $(OBJS)
	$(PLD) $(PLDOUT)$@ $^

################################################################################
# generated                                                                    #
################################################################################
error_codes.h error_codes.c error_codes_scm.scm ErrorCodes.java: generated

generated: gen_error_codes$(EXE_EXT) error.codes
	./gen_error_codes$(EXE_EXT)
	touch generated

gen_error_codes$(EXE_EXT): gen_error_codes$(OBJ_EXT)
	$(LD) $(LFLAGS_CHICKEN_EXE) $(LDOUT)$@ $^ $(LSLIBS_CHICKEN_EXE)

gen_error_codes$(OBJ_EXT): gen_error_codes.c
	$(CC) $(CFLAGS_CHICKEN_EXE) $<

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) error_codes.h
	-$(REMOVE) error_codes.c
	-$(REMOVE) gen_error_codes.c
	-$(REMOVE) gen_error_codes$(EXE_EXT)
	-$(REMOVE) error_codes_scm.scm
	-$(REMOVE) error_codes_scm.c
	-$(REMOVE) ErrorCodes.java
