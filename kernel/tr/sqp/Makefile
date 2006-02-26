#
# Makefile for sqp (GNU make)
#

PP = ../../..

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifeq ($(words $(wildcard depend)), 0)
ifneq ($(findstring depend, $(MAKECMDGOALS)), depend)
$(error Type "make depend" first)
endif
endif
endif


INCLUDE_DIRS = . $(PP)/libs/chicken $(PP)/kernel/tr/executor/base	\
               $(PP)/kernel/common $(PP)/kernel/common/u $(PP)/driver/c	\
               $(PP)/kernel/tr/structures $(PP)/kernel/tr/nid		\
               $(PP)/kernel/tr/crmutils $(PP)/kernel/tr/strings $(PP)/kernel/tr/vmm			\
               $(PP)/kernel/common/ph 								\
               $(PP)/kernel/tr/executor/por2qep						\
               $(PP)/kernel/tr/executor/root						\
               $(PP)/kernel/tr/executor/fo							\
               $(PP)/kernel/tr/executor/xqops $(PP)/kernel/tr/idx	\
               $(PP)/kernel/tr/idx/btree $(PP)/kernel/tr/mo			\
               $(PP)/kernel/tr/pstr $(PP)/kernel/tr/ft $(PP)/kernel/tr/strings \
			   $(PP)/libs/dtsearch/include


include $(PP)/Makefile.include

VPATH = .  lr2por tools sa lropt popt


all: libsqp$(LIB_EXT)
	@echo ===================================================================
	@echo sqp has been done
	@echo ===================================================================

include $(PP)/Makefile.depend

depend:
	$(DEPACT)


ifneq ($(words $(wildcard depend)), 0)
include depend
endif

SQP_LIB = libsqp$(LIB_EXT)

################################################################################
# SQP                                                                          #
################################################################################
$(SQP_LIB): common-lib$(OBJ_EXT)		\
            lr2por$(OBJ_EXT)			\
            por-cons$(OBJ_EXT)			\
            lreturn$(OBJ_EXT)			\
            lr2por-lib$(OBJ_EXT)		\
            xquery-lr-lib$(OBJ_EXT)		\
            sa$(OBJ_EXT)				\
            popt$(OBJ_EXT)				\
            cpp_popt$(OBJ_EXT)			\
            att$(OBJ_EXT)				\
            att_xpath$(OBJ_EXT)			\
            serialize2lr$(OBJ_EXT)		\
            popt_plan$(OBJ_EXT)			\
            dschema$(OBJ_EXT)			\
            impl-differences$(OBJ_EXT)	\
            $(PP)/kernel/common/error_codes_scm$(OBJ_EXT)		
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

