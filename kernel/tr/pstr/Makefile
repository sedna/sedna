#
# Makefile for pstr
#

PP = ../../..

include $(PP)/Makefile.include

all: pstr$(LIB_EXT)
	@echo ===================================================================
	@echo pstr DONE
	@echo ===================================================================

OBJS = hh$(OBJ_EXT) pstr$(OBJ_EXT) pstr_long$(OBJ_EXT) utl$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# pstr.lib                                                                     #
################################################################################
pstr$(LIB_EXT): $(OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
