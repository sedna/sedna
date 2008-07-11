#
# Makefile for logical log (GNU make)
#

PP = ../../..

include $(PP)/Makefile.include

all: llcommon$(LIB_EXT) llsm$(LIB_EXT)
	@echo ===================================================================
	@echo Low Level Logical Log has been Done
	@echo ===================================================================

OBJS = lfsStorage$(OBJ_EXT) llMain$(OBJ_EXT)
OBJS_SM = $(OBJS) llhb$(OBJ_EXT) physlog$(OBJ_EXT) physrcv$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifndef NO_DEP
include $(OBJS:$(OBJ_EXT)=.d)
endif
endif


################################################################################
# llcommon and llsm                                                            #
################################################################################
llcommon$(LIB_EXT): $(OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

llsm$(LIB_EXT): $(OBJS_SM)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean

