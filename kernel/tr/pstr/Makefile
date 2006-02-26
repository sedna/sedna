#
# Makefile for pstr (GNU make)
#

PP = ../../..

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifeq ($(words $(wildcard depend)), 0)
ifneq ($(findstring depend, $(MAKECMDGOALS)), depend)
$(error Type "make depend" first)
endif
endif
endif


INCLUDE_DIRS = . $(PP)/kernel/common $(PP)/kernel/common/u $(PP)/driver/c		\
               $(PP)/kernel/tr/vmm $(PP)/kernel/tr/crmutils					\
               $(PP)/kernel/tr/mo $(PP)/kernel/tr/log $(PP)/kernel/common/ph $(PP)/kernel/tr/mo\
               $(PP)/kernel/tr/locks $(PP)/kernel/sm/plmgr 					\
               $(PP)/kernel/sm/llmgr $(PP)/kernel/tr/executor/base			\
               $(PP)/kernel/tr/structures $(PP)/kernel/tr/strings			\
               $(PP)/kernel/tr/nid $(PP)/kernel/tr/executor/por2qep
                
VPATH = .


include $(PP)/Makefile.include

all: libpstr$(LIB_EXT)
	@echo ===================================================================
	@echo PSTR DONE
	@echo ===================================================================

include $(PP)/Makefile.depend

depend:
	$(DEPACT)


ifneq ($(words $(wildcard depend)), 0)
include depend
endif

PSTR_LIB = libpstr$(LIB_EXT)

################################################################################
# pstr                                                                         #
################################################################################
#pstr$(LIB_EXT): hh$(OBJ_EXT) pstr$(OBJ_EXT) utl$(OBJ_EXT)
#	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

$(PSTR_LIB): hh$(OBJ_EXT) pstr$(OBJ_EXT) pstr_long$(OBJ_EXT) utl$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

#pstr_test$(EXE_EXT): pstr$(LIB_EXT) test$(OBJ_EXT)
#	link /debug /out:pstr_test$(EXE_EXT) pstr$(LIB_EXT) test$(OBJ_EXT) locks.lib mo.lib updates.lib xqp.lib executor.lib prepare_query.lib ../vmm/vmm.lib ../../common/common_files.lib ../nid/nid.lib C:/home/charisma/libs/pers_heap/pers_heap.lib


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) $(PSTR_LIB)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb
	-$(REMOVE) pstr_test$(EXE_EXT)
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) *.SUP

