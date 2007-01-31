#
# Makefile for strings
#

PP = ../../..

include $(PP)/Makefile.include

all: strings$(LIB_EXT)
	@echo ===================================================================
	@echo strings DONE
	@echo ===================================================================

OBJS = e_string$(OBJ_EXT) e_string_iterator$(OBJ_EXT) strings_base$(OBJ_EXT) \
       strings$(OBJ_EXT) utf8$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# strings.lib                                                                  #
################################################################################
strings$(LIB_EXT): $(OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
