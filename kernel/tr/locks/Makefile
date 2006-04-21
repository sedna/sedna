#
# Makefile for locks (GNU make)
#

PP = ../../..


VPATH = $(PP)/kernel/tr


include $(PP)/Makefile.include

all: liblocks$(LIB_EXT)
	@echo ===================================================================
	@echo LOCKS DONE
	@echo ===================================================================


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/tr/depend
endif


LOCKS_LIB = liblocks$(LIB_EXT)

################################################################################
# LOCKS                                                                          #
################################################################################
$(LOCKS_LIB):    locks$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) $(LOCKS_LIB)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
