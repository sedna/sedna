#
# Makefile for nid (GNU make)
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
               $(PP)/kernel/tr/vmm $(PP)/kernel/tr/crmutils $(PP)/kernel/tr/ft					\
               $(PP)/kernel/tr/mo $(PP)/kernel/tr/log $(PP)/kernel/common/ph					\
               $(PP)/kernel/tr/pstr $(PP)/kernel/tr/executor/base			\
               $(PP)/kernel/sm/plmgr $(PP)/kernel/sm/llmgr $(PP)/kernel/tr/strings					\
               $(PP)/kernel/tr/structures $(PP)/kernel/tr/executor/por2qep

VPATH = .


include $(PP)/Makefile.include

all: libnid$(LIB_EXT) #test_nbm$(EXE_EXT)
	@echo ===================================================================
	@echo NID DONE
	@echo ===================================================================

include $(PP)/Makefile.depend

depend:
	$(DEPACT)


ifneq ($(words $(wildcard depend)), 0)
include depend
endif

NID_LIB = libnid$(LIB_EXT)

################################################################################
# NID                                                                          #
################################################################################
$(NID_LIB): lex$(OBJ_EXT) numb_scheme$(OBJ_EXT) nidalloc$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

test_lex$(EXE_EXT): nid$(LIB_EXT) test_lex$(OBJ_EXT)
	link /debug /out:test_lex$(EXE_EXT) lex$(OBJ_EXT) numb_scheme$(OBJ_EXT) nidalloc$(OBJ_EXT) test_lex$(OBJ_EXT) \
	../vmm/vmm.lib ../../common/common_files.lib ../pstr/pstr.lib ../../../libs/pers_heap/pers_heap.lib \
	../locks/locks.lib
test_nbm$(EXE_EXT): nid$(LIB_EXT) test_nbm$(OBJ_EXT)
	link /debug /out:test_nbm$(EXE_EXT) lex$(OBJ_EXT) numb_scheme$(OBJ_EXT) nidalloc$(OBJ_EXT) test_nbm$(OBJ_EXT) \
	../vmm/vmm.lib ../../common/common_files.lib ../pstr/pstr.lib ../../../libs/pers_heap/pers_heap.lib \
	../locks/locks.lib ../mo/mo.lib
################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) $(NID_LIB)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb
	-$(REMOVE) test_nbm$(OBJ_EXT)
	-$(REMOVE) test_lex$(OBJ_EXT)
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) *.SUP
