#
# Makefile for SM
#

PP = ../..

include $(PP)/Makefile.include

all: $(PP)/bin/se_sm$(EXE_EXT) $(PP)/bin/se_cdb$(EXE_EXT) $(PP)/bin/se_ddb$(EXE_EXT) $(PP)/bin/se_smsd$(EXE_EXT)
	@echo ===================================================================
	@echo SM DONE
	@echo ===================================================================

SM_COMM   = 
SM_OBJS   = sm$(OBJ_EXT) sm_functions$(OBJ_EXT) sm_globals$(OBJ_EXT) \
            trmgr$(OBJ_EXT)
CDB_OBJS  = cdb$(OBJ_EXT) cdb_globals$(OBJ_EXT)	sm_functions$(OBJ_EXT) \
            sm_globals$(OBJ_EXT) db_utils$(OBJ_EXT) trmgr$(OBJ_EXT) 
DDB_OBJS  = db_utils$(OBJ_EXT) ddb$(OBJ_EXT)
SMSD_OBJS = smsd$(OBJ_EXT)
OBJS      = sm$(OBJ_EXT) sm_functions$(OBJ_EXT) sm_globals$(OBJ_EXT) \
            trmgr$(OBJ_EXT) cdb$(OBJ_EXT) cdb_globals$(OBJ_EXT)	\
            db_utils$(OBJ_EXT) trmgr$(OBJ_EXT) ddb$(OBJ_EXT) smsd$(OBJ_EXT)                                               

SUBDIRS   = bufmgr plmgr llmgr lm
SUBDIROBJS= $(foreach dir,$(SUBDIRS),$(dir)/$(dir)$(LIB_EXT))

COMMON_FILES_LIB = $(PP)/kernel/common/common_files$(LIB_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# sm                                                                           #
################################################################################
$(PP)/bin/se_sm$(EXE_EXT): $(SM_OBJS) $(SUBDIROBJS) $(COMMON_FILES_LIB)
	$(LD) $(LFLAGS) $(LDOUT)$@ $^ $(SUBDIROBJS) $(LSLIBS)

################################################################################
# cdb                                                                          #
################################################################################
$(PP)/bin/se_cdb$(EXE_EXT): $(CDB_OBJS) $(SUBDIROBJS) $(COMMON_FILES_LIB)
	$(LD) $(LFLAGS) $(LDOUT)$@ $^ $(SUBDIROBJS) $(LSLIBS)

################################################################################
# ddb                                                                          #
################################################################################
$(PP)/bin/se_ddb$(EXE_EXT): $(DDB_OBJS) $(SUBDIROBJS) $(COMMON_FILES_LIB)
	$(LD) $(LFLAGS) $(LDOUT)$@ $^ $(LSLIBS)

################################################################################
# smsd                                                                         #
################################################################################
$(PP)/bin/se_smsd$(EXE_EXT): $(SMSD_OBJS) $(COMMON_FILES_LIB)
	$(LD) $(LFLAGS) $(LDOUT)$@ $^ $(LSLIBS)



$(SUBDIROBJS): $(SUBDIRS:%=%-recursive) ;

.PHONY: $(SUBDIRS:%=%-recursive)
$(SUBDIRS:%=%-recursive):
	$(MAKE) -C $(subst -recursive,,$@)


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	for dir in $(SUBDIRS); do $(MAKE) -C $$dir $@ || exit; done


