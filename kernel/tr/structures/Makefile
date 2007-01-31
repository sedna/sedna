#
# Makefile for structures
#

PP = ../../..

include $(PP)/Makefile.include

all: structures$(LIB_EXT)
	@echo ===================================================================
	@echo structures DONE
	@echo ===================================================================

OBJS = indirection$(OBJ_EXT) metadata$(OBJ_EXT) nodes$(OBJ_EXT) schema$(OBJ_EXT)


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# structures.lib                                                               #
################################################################################
structures$(LIB_EXT): $(OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
