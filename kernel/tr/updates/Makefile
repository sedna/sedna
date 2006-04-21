#
# Makefile for updates (GNU make)
#

PP = ../../..


VPATH = $(PP)/kernel/tr


include $(PP)/Makefile.include

all: libupdates$(LIB_EXT) 
	@echo ===================================================================
	@echo UPDATES DONE
	@echo ===================================================================


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/tr/depend
endif


UPDATES_LIB = libupdates$(LIB_EXT)

################################################################################
# UPDATES                                                                          #
################################################################################
$(UPDATES_LIB): updates$(OBJ_EXT)		\
                insertions$(OBJ_EXT)	\
                deletions$(OBJ_EXT)		\
                replace$(OBJ_EXT)		\
                other_updates$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) $(UPDATES_LIB)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
