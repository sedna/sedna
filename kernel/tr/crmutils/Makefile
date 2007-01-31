#
# Makefile for crmutils (GNU make)
#

PP = ../../..

include $(PP)/Makefile.include

all: crmutils$(LIB_EXT)
	@echo ===================================================================
	@echo CRMUTILS DONE
	@echo ===================================================================

OBJS = file_utils$(OBJ_EXT) print_utils$(OBJ_EXT) debug_utils$(OBJ_EXT) \
       scheme_utils$(OBJ_EXT) system_tables$(OBJ_EXT) str_matcher$(OBJ_EXT) \
       node_utils$(OBJ_EXT) namespace_utils$(OBJ_EXT) exec_output$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# CRMUTILS                                                                     #
################################################################################
crmutils$(LIB_EXT): $(OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
