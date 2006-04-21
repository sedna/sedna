#
# Makefile for IDX
#

PP = ../../..


VPATH = $(PP)/kernel/tr


include $(PP)/Makefile.include



all: libidx$(LIB_EXT)
	@echo ===================================================================
	@echo IDX DONE
	@echo ===================================================================


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/tr/depend
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
	-$(REMOVE) log

