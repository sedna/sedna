#
# Makefile for strings (GNU make)
#

PP = ../../..

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifeq ($(words $(wildcard depend)), 0)
ifneq ($(findstring depend, $(MAKECMDGOALS)), depend)
$(error Type "make depend" first)
endif
endif
endif


INCLUDE_DIRS = . $(PP)/kernel/common $(PP)/kernel/common/u $(PP)/driver/c	\
               $(PP)/kernel/tr/mo $(PP)/kernel/tr/vmm $(PP)/kernel/tr/pstr	\
               $(PP)/kernel/common/ph $(PP)/kernel/tr/executor/base	$(PP)/kernel/tr/executor/fo		\
               $(PP)/kernel/tr/structures $(PP)/kernel/tr/nid $(PP)/kernel/tr/executor/por2qep \
               $(PP)/kernel/tr/crmutils \
               $(PP)/libs/pcre

VPATH = .


include $(PP)/Makefile.include

all: libstrings$(LIB_EXT)
	@echo ===================================================================
	@echo STRINGS DONE
	@echo ===================================================================

include $(PP)/Makefile.depend

depend:
	$(DEPACT)


ifneq ($(words $(wildcard depend)), 0)
include depend
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
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) *.SUP
