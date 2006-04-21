#
# Makefile for log (GNU make)
#

PP = ../../..


VPATH = $(PP)/kernel/tr


include $(PP)/Makefile.include

all: libhl_phys_log$(LIB_EXT) libhl_logical_log$(LIB_EXT)
	@echo ===================================================================
	@echo High Level Physical Log has been Done
	@echo ===================================================================


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/tr/depend
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
