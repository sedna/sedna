#
# Makefile for crmutils (GNU make)
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
               $(PP)/kernel/tr/strings $(PP)/libs/expat $(PP)/kernel/tr/mo			\
               $(PP)/kernel/tr/vmm $(PP)/kernel/tr/pstr $(PP)/kernel/common/ph 	\
               $(PP)/kernel/tr/executor/base $(PP)/kernel/sm/plmgr	\
               $(PP)/kernel/tr/structures $(PP)/kernel/tr/nid \
               $(PP)/kernel/tr/locks $(PP)/kernel/tr/ft $(PP)/kernel/tr/idx $(PP)/kernel/tr/executor/fo\
               $(PP)/kernel/tr/executor/por2qep $(PP)/kernel/tr/ft $(PP)/kernel/tr/updates

VPATH = .


include $(PP)/Makefile.include

all: libcrmutils$(LIB_EXT)
	@echo ===================================================================
	@echo CRMUTILS DONE
	@echo ===================================================================

include $(PP)/Makefile.depend

depend:
	$(DEPACT)


ifneq ($(words $(wildcard depend)), 0)
include depend
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
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) *.SUP
