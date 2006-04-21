#
# Makefile for mo (GNU make)
#

PP = ../../..


VPATH = $(PP)/kernel/tr


include $(PP)/Makefile.include

all: libmo$(LIB_EXT)
	@echo ===================================================================
	@echo MO DONE
	@echo ===================================================================


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/tr/depend
endif


MO_LIB = libmo$(LIB_EXT)

################################################################################
# MO                                                                          #
################################################################################
$(MO_LIB):    micro$(OBJ_EXT) mo_write_utils$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) $(MO_LIB)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
