#
# Makefile for rcv (GNU make)
#

PP = ../../..


VPATH = $(PP)/kernel/tr


include $(PP)/Makefile.include

all: librcv$(LIB_EXT)
	@echo ===================================================================
	@echo Rcv Library Has Been Done
	@echo ===================================================================


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/tr/depend
endif


RCV_LIB = librcv$(LIB_EXT)

################################################################################
# LOCKS                                                                          #
################################################################################
$(RCV_LIB):    rcv_funcs$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) *$(LIB_EXT)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
