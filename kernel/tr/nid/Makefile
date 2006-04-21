#
# Makefile for nid (GNU make)
#

PP = ../../..


VPATH = $(PP)/kernel/tr


include $(PP)/Makefile.include

all: libnid$(LIB_EXT) #test_nbm$(EXE_EXT)
	@echo ===================================================================
	@echo NID DONE
	@echo ===================================================================


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/tr/depend
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
	-$(REMOVE) test_nbm$(OBJ_EXT)
	-$(REMOVE) test_lex$(OBJ_EXT)
