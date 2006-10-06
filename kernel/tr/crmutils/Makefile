#
# Makefile for crmutils (GNU make)
#

PP = ../../..


VPATH = $(PP)/kernel/tr


include $(PP)/Makefile.include

all: libcrmutils$(LIB_EXT)
	@echo ===================================================================
	@echo CRMUTILS DONE
	@echo ===================================================================


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/tr/depend
endif


CRMUTILS_LIB = libcrmutils$(LIB_EXT)

################################################################################
# CRMUTILS                                                                          #
################################################################################
$(CRMUTILS_LIB): file_utils$(OBJ_EXT)		\
                 print_utils$(OBJ_EXT) 		\
                 debug_utils$(OBJ_EXT)		\
                 scheme_utils$(OBJ_EXT)		\
                 system_tables$(OBJ_EXT)	\
                 str_matcher$(OBJ_EXT)	\
                 node_utils$(OBJ_EXT)		\
                 exec_output$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) $(CRMUTILS_LIB)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
