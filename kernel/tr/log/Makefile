#
# Makefile for log
#

PP = ../../..

include $(PP)/Makefile.include

all: log$(LIB_EXT)
	@echo ===================================================================
	@echo log DONE
	@echo ===================================================================

OBJS = hl_logical_log$(OBJ_EXT) hl_phys_log$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# log.lib                                                                      #
################################################################################
log$(LIB_EXT): $(OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
