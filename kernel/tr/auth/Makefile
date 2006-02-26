#
# Makefile for authentication
#

PP = ../../..

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifeq ($(words $(wildcard depend)), 0)
ifneq ($(findstring depend, $(MAKECMDGOALS)), depend)
$(error Type "make depend" first)
endif
endif
endif


INCLUDE_DIRS = $(PP)/kernel/common		\
	       $(PP)/kernel/common/u $(PP)/driver/c		\
	       $(PP)/kernel/tr/strings $(PP)/libs/chicken		\
	       $(PP)/kernel/common/ph $(PP)/kernel/tr/executor/root		\
		$(PP)/kernel/tr/executor/xqops $(PP)/kernel/tr/executor/por2qep \
		$(PP)/kernel/tr/executor/base $(PP)/kernel/tr/idx	\
		$(PP)/kernel/tr/idx/btree $(PP)/kernel/tr/executor/fo	\
		$(PP)/kernel/tr/mo $(PP)/kernel/tr $(PP)/kernel/tr/ft 	\
		$(PP)/kernel/tr/structures $(PP)/kernel/tr/nid \
		$(PP)/kernel/tr/strings $(PP)/kernel/tr/locks \
		$(PP)/kernel/tr/nid $(PP)/kernel/tr/crmutils $(PP)/kernel/tr/vmm  \
        $(PP)/kernel/tr/pstr \
		$(PP)/libs/dtsearch/include

include $(PP)/Makefile.include

VPATH = .


all: libauth$(LIB_EXT)
	@echo ===================================================================
	@echo Authorization Library has been done
	@echo ===================================================================

include $(PP)/Makefile.depend

depend:
	$(DEPACT)


ifneq ($(words $(wildcard depend)), 0)
include depend
endif

AUTH_LIB = libauth$(LIB_EXT)

################################################################################
# auth                                                    #
################################################################################
AUTH_LIB_OBJS = auth$(OBJ_EXT)                      \
		auc$(OBJ_EXT)                      \
            prec-qs$(OBJ_EXT)		


$(AUTH_LIB): $(AUTH_LIB_OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^



################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) *$(LIB_EXT)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) *.SUP
