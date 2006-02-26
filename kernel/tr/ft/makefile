#
# Makefile for FT
#

PP = ../../..

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifeq ($(words $(wildcard depend)), 0)
ifneq ($(findstring depend, $(MAKECMDGOALS)), depend)
$(error Type "make depend" first)
endif
endif
endif



INCLUDE_DIRS = . $(PP)/kernel/common $(PP)/kernel/common/u					\
               $(PP)/kernel/tr/vmm $(PP)/kernel/tr/executor/xqops			\
               $(PP)/kernel/common/ph btree $(PP)/kernel/tr/executor/base	\
               $(PP)/kernel/tr/executor/fo $(PP)/kernel/sm/plmgr			\
               $(PP)/kernel/tr/structures $(PP)/kernel/tr/nid				\
               $(PP)/kernel/tr/mo $(PP)/kernel/tr/crmutils $(PP)/kernel/tr/log 				\
               $(PP)/kernel/sm/llmgr $(PP)/kernel/tr/executor/por2qep	$(PP)/driver/c	\
               $(PP)/kernel/tr/strings $(PP)/kernel/tr/pstr $(PP)/libs/dtsearch/include \
			   $(PP)/kernel/tr

VPATH = .


include $(PP)/Makefile.include



all: libft$(LIB_EXT)
	@echo ===================================================================
	@echo FT DONE
	@echo ===================================================================


include $(PP)/Makefile.depend

depend:
	$(DEPACT)

ifneq ($(words $(wildcard depend)), 0)
include depend
endif

.PHONY: clean libraries


FT_LIB = libft$(LIB_EXT)

DTOBJS = dtsviewr$(OBJ_EXT) \
		       dstring$(OBJ_EXT) \
		       dfile$(OBJ_EXT) \
		       dinputstream$(OBJ_EXT) \
		       dtsfc$(OBJ_EXT) \
		       dtsearch$(OBJ_EXT)

################################################################################
# ft.lib                                                                       #
################################################################################
$(FT_LIB): FTsearch$(OBJ_EXT) FTindex$(OBJ_EXT) ft_index_data$(OBJ_EXT) $(DTOBJS)
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

