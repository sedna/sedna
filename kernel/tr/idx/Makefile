#
# Makefile for IDX
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
               $(PP)/kernel/tr/vmm $(PP)/kernel/tr/executor/xqops			\
               $(PP)/kernel/common/ph btree $(PP)/kernel/tr/executor/base	\
               $(PP)/kernel/tr/executor/fo $(PP)/kernel/sm/plmgr			\
               $(PP)/kernel/tr/structures $(PP)/kernel/tr/nid $(PP)/kernel/tr/ft \
               $(PP)/kernel/tr/mo $(PP)/kernel/tr/crmutils $(PP)/kernel/tr/log 				\
               $(PP)/kernel/sm/llmgr $(PP)/kernel/tr/executor/por2qep		\
               $(PP)/kernel/tr/pstr

VPATH = .


include $(PP)/Makefile.include



all: libidx$(LIB_EXT)
	@echo ===================================================================
	@echo IDX DONE
	@echo ===================================================================


include $(PP)/Makefile.depend

depend:
	$(DEPACT)
	$(MAKE) -C btree depend

ifneq ($(words $(wildcard depend)), 0)
include depend
endif

.PHONY: clean libraries


BTREE_LIB = btree/libbtree$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
BTREE_S = $(BTREE_LIB)
else
BTREE_S = -Lbtree -lbtree
endif

IDX_LIB = libidx$(LIB_EXT)

$(BTREE_LIB): libraries
	

################################################################################
# idx.lib                                                                      #
################################################################################
$(IDX_LIB): indexes$(OBJ_EXT) index_data$(OBJ_EXT) $(BTREE_LIB)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ indexes$(OBJ_EXT) index_data$(OBJ_EXT)

libraries:
	$(MAKE) -C btree


################################################################################
# Clean                                                                        #
################################################################################
clean: generic_clean
	$(MAKE) -C btree clean
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

