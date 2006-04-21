#
# Makefile for structures (GNU make)
#

PP = ../../..


VPATH = $(PP)/kernel/tr


include $(PP)/Makefile.include

all: libstructures$(LIB_EXT)
	@echo ===================================================================
	@echo STRUCTURES DONE
	@echo ===================================================================


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/tr/depend
endif


STRUCTURES_LIB = libstructures$(LIB_EXT)

################################################################################
# STRUCTURES                                                                   #
################################################################################
$(STRUCTURES_LIB): indirection$(OBJ_EXT) 	\
                   metadata$(OBJ_EXT)		\
                   nodes$(OBJ_EXT)			\
                   schema$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) $(STRUCTURES_LIB)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
