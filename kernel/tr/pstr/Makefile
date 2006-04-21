#
# Makefile for pstr (GNU make)
#

PP = ../../..

                
VPATH = $(PP)/kernel/tr


include $(PP)/Makefile.include

all: libpstr$(LIB_EXT)
	@echo ===================================================================
	@echo PSTR DONE
	@echo ===================================================================


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/tr/depend
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
	-$(REMOVE) pstr_test$(EXE_EXT)

