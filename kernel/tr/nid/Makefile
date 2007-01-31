#
# Makefile for nid
#

PP = ../../..

include $(PP)/Makefile.include

all: nid$(LIB_EXT)
	@echo ===================================================================
	@echo nid DONE
	@echo ===================================================================

OBJS = lex$(OBJ_EXT) nidalloc$(OBJ_EXT) numb_scheme$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# nid.lib                                                                      #
################################################################################
nid$(LIB_EXT): $(OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
