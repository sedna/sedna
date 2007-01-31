#
# Makefile for updates
#

PP = ../../..

include $(PP)/Makefile.include

all: updates$(LIB_EXT)
	@echo ===================================================================
	@echo updates DONE
	@echo ===================================================================

OBJS = deletions$(OBJ_EXT) insertions$(OBJ_EXT) updates$(OBJ_EXT) \
       replace$(OBJ_EXT) other_updates$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# updates.lib                                                                  #
################################################################################
updates$(LIB_EXT): $(OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
