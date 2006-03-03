#
# Makefile for log (GNU make)
#

PP = ../../..

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifeq ($(words $(wildcard depend)), 0)
ifneq ($(findstring depend, $(MAKECMDGOALS)), depend)
$(error Type "make depend" first)
endif
endif
endif


INCLUDE_DIRS = $(PP)/kernel/common $(PP)/kernel/common/u $(PP)/driver/c		\
               $(PP)/kernel/common/ph $(PP)/kernel/tr/ft                                                          \
               $(PP)/kernel/sm/plmgr $(PP)/kernel/tr $(PP)/kernel/sm/llmgr			\
               $(PP)/kernel/tr/crmutils $(PP)/kernel/tr/ft $(PP)/kernel/tr/strings                                                         \
               $(PP)/kernel/tr/executor/base  $(PP)/libs/dtsearch/include                                                   \
               $(PP)/kernel/tr/mo $(PP)/kernel/tr/vmm $(PP)/kernel/sm $(PP)/kernel/tr/structures	\
               $(PP)/kernel/tr/nid $(PP)/kernel/tr/executor/por2qep $(PP)/kernel/tr/pstr $(PP)/kernel/tr/rcv

VPATH = .


include $(PP)/Makefile.include

all: libhl_phys_log$(LIB_EXT) libhl_logical_log$(LIB_EXT)
	@echo ===================================================================
	@echo High Level Physical Log has been Done
	@echo ===================================================================

include $(PP)/Makefile.depend

depend:
	$(DEPACT)


ifneq ($(words $(wildcard depend)), 0)
include depend
endif

HL_PHYS_LOG_LIB = libhl_phys_log$(LIB_EXT)
HL_LOGICAL_LOG_LIB = libhl_logical_log$(LIB_EXT)
PHYS_LOG_LIB = $(PP)/kernel/sm/plmgr/libtr_phys_log_mgr$(LIB_EXT)
LOGICAL_LOG_LIB = $(PP)/kernel/sm/llmgr/liblogical_log_mgr$(LIB_EXT)

################################################################################
# Logs                                                                         #
################################################################################
$(HL_PHYS_LOG_LIB): hl_phys_log$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

$(HL_LOGICAL_LOG_LIB): hl_logical_log$(OBJ_EXT) $(LOGICAL_LOG_LIB)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) $(HL_PHYS_LOG_LIB)
	-$(REMOVE) $(HL_LOGICAL_LOG_LIB)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) *.SUP
