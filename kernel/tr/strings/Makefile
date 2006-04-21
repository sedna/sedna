#
# Makefile for strings (GNU make)
#

PP = ../../..


VPATH = $(PP)/kernel/tr


include $(PP)/Makefile.include

all: libstrings$(LIB_EXT)
	@echo ===================================================================
	@echo STRINGS DONE
	@echo ===================================================================


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/tr/depend
endif


STRINGS_LIB = libstrings$(LIB_EXT)

################################################################################
# STRUCTURES                                                                   #
################################################################################
$(STRINGS_LIB): strings$(OBJ_EXT) 	\
                e_string_iterator$(OBJ_EXT) \
                char_iterator$(OBJ_EXT) \
                utf8$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) $(STRINGS_LIB)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
