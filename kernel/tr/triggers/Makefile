#
# Makefile for TRIGGERS
#

PP = ../../..


VPATH = $(PP)/kernel/tr


include $(PP)/Makefile.include



all: libtriggers$(LIB_EXT)
	@echo ===================================================================
	@echo TRIGGERS DONE
	@echo ===================================================================


include $(PP)/Makefile.depend

depend:
	$(DEPACT)

ifneq ($(words $(wildcard depend)), 0)
include depend
endif

.PHONY: clean libraries


TRIGGERS_LIB = libtriggers$(LIB_EXT)


################################################################################
# triggers                                                                     #
################################################################################
$(TRIGGERS_LIB): triggers_data$(OBJ_EXT)	\
		 triggers_utils$(OBJ_EXT)	\
                 triggers$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

################################################################################
# Clean                                                                        #
################################################################################
clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) *$(LIB_EXT)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) *.SUP
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb
	-$(REMOVE) log

