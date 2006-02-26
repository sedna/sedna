#
# Makefile for governor (GNU Make)
#

PP = ../..

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifeq ($(words $(wildcard depend)), 0)
ifneq ($(findstring depend, $(MAKECMDGOALS)), depend)
$(error Type "make depend" first)
endif
endif
endif


INCLUDE_DIRS = . $(PP)/kernel/common $(PP)/kernel/common/u $(PP)/driver/c

VPATH = .


include $(PP)/Makefile.include

all: $(PP)/bin/se_gov$(EXE_EXT) $(PP)/bin/se_stop$(EXE_EXT) $(PP)/bin/se_rc$(EXE_EXT) 
	@echo ===================================================================
	@echo GOV DONE
	@echo ===================================================================

test: test-server$(EXE_EXT) test-client$(EXE_EXT)
	@echo Test Done

################################################################################
# Test                                                                          
################################################################################
test-server$(EXE_EXT): test-server$(OBJ_EXT)
	$(LD) $(LFLAGS) $(LDOUT)$@ test-server$(OBJ_EXT)  $(COMMON_FILES_S) $(LSLIBS)

test-client$(EXE_EXT): test-client$(OBJ_EXT)
	$(LD) $(LFLAGS) $(LDOUT)$@ test-client$(OBJ_EXT)  $(COMMON_FILES_S) $(LSLIBS)


include $(PP)/Makefile.depend

depend:
	$(DEPACT)


ifneq ($(words $(wildcard depend)), 0)
include depend
endif

COMMON_FILES_LIB = $(PP)/kernel/common/libcommon_files$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
COMMON_FILES_S = $(COMMON_FILES_LIB)
else
COMMON_FILES_S = -L$(PP)/kernel/common -lcommon_files
endif

################################################################################
# GOV                                                                          #
################################################################################
GOV_OBJS = gov_globals$(OBJ_EXT)		\
           gov_table$(OBJ_EXT)			\
           listener$(OBJ_EXT)			\
           gov_functions$(OBJ_EXT)		\
           gov$(OBJ_EXT)

$(PP)/bin/se_gov$(EXE_EXT): $(GOV_OBJS) $(COMMON_FILES_LIB)
	$(LD) $(LFLAGS) $(LDOUT)$@ $(GOV_OBJS) $(COMMON_FILES_S) $(LSLIBS)

################################################################################
# STOP SERVER                                                                  #
################################################################################
STOP_SERV_OBJS = stop_server$(OBJ_EXT)

$(PP)/bin/se_stop$(EXE_EXT): $(STOP_SERV_OBJS) $(COMMON_FILES_LIB)
	$(LD) $(LFLAGS) $(LDOUT)$@ $(STOP_SERV_OBJS) $(COMMON_FILES_S) $(LSLIBS)

################################################################################
# RC                                                                           #
################################################################################
RC_OBJS = rc$(OBJ_EXT)

$(PP)/bin/se_rc$(EXE_EXT): $(RC_OBJS) $(COMMON_FILES_LIB)
	$(LD) $(LFLAGS) $(LDOUT)$@ $(RC_OBJS) $(COMMON_FILES_S) $(LSLIBS)


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) *.SUP

