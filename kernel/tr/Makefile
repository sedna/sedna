#
# Makefile for TR
#

PP = ../..

include $(PP)/Makefile.include

all: $(PP)/bin/se_trn$(EXE_EXT) $(PP)/bin/se_rcv$(EXE_EXT)
	@echo ===================================================================
	@echo TR DONE
	@echo ===================================================================


TRN_OBJS = tr$(OBJ_EXT) socket_client$(OBJ_EXT) cl_client$(OBJ_EXT) \
           tr_functions$(OBJ_EXT) tr_common_funcs$(OBJ_EXT) \
           tr_globals$(OBJ_EXT)
RCV_OBJS = rcv_db$(OBJ_EXT) tr_common_funcs$(OBJ_EXT) tr_globals$(OBJ_EXT)
OBJS     = tr$(OBJ_EXT) socket_client$(OBJ_EXT) cl_client$(OBJ_EXT) \
           tr_functions$(OBJ_EXT) tr_common_funcs$(OBJ_EXT) \
           tr_globals$(OBJ_EXT) rcv_db$(OBJ_EXT)

SUBDIRS  = auth crmutils executor idx locks log mo nid pq pstr rcv sqp \
           strings structures updates vmm xqp

ifeq ($(ENABLE_DTSEARCH), 1)
SUBDIRS += ft
endif

ifeq ($(ENABLE_TRIGGERS), 1)
SUBDIRS += triggers
endif

SUBDIROBJS = $(foreach dir,$(SUBDIRS),$(dir)/$(dir)$(LIB_EXT))


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif


COMMON_FILES_LIB = $(PP)/kernel/common/common_files$(LIB_EXT)
PHYS_LOG_LIB = $(PP)/kernel/sm/plmgr/plmgr_tr$(LIB_EXT)
LOGICAL_LOG_LIB = $(PP)/kernel/sm/llmgr/llmgr$(LIB_EXT)


ifeq ($(ENABLE_DTSEARCH), 1)
ifeq ($(PLATFORM), WIN32)
FT_ADD_LIB = dten600.lib Nafxcw.lib LIBCMT.LIB
else
FT_ADD_LIB = 
endif
else
FT_ADD_LIB = 
endif

ADD_LIBS = $(COMMON_FILES_LIB) $(PHYS_LOG_LIB) $(LOGICAL_LOG_LIB) $(FT_ADD_LIB)


################################################################################
# tr (Transaction)                                                             #
################################################################################
ifeq ($(ENABLE_DTSEARCH), 1)
E_LFLAGS = $(LFLAGS_DTS)
else
E_LFLAGS = $(LFLAGS)
endif


ifeq ("$(PLATFORM)", "WIN32")
$(PP)/bin/se_trn$(EXE_EXT): $(TRN_OBJS) $(SUBDIROBJS)
	$(LD) $(E_LFLAGS) $(LDOUT)$@ $^ $(ADD_LIBS) $(LSLIBS)
else
$(PP)/bin/se_trn$(EXE_EXT): $(TRN_OBJS) $(SUBDIROBJS) 
	$(LD) $(LFLAGS) $(LDOUT)$@ $^ $(SUBDIROBJS) $(ADD_LIBS) $(LSLIBS)
endif



ifeq ("$(PLATFORM)", "WIN32")
$(PP)/bin/se_rcv$(EXE_EXT): $(RCV_OBJS) $(SUBDIROBJS)
	$(LD) $(E_LFLAGS) $(LDOUT)$@ $^ $(ADD_LIBS)
else					
$(PP)/bin/se_rcv$(EXE_EXT): $(RCV_OBJS) $(SUBDIROBJS) $(ADD_LIBS)
	$(LD) $(LFLAGS) $(LDOUT)$@ $^ $(SUBDIROBJS) $(ADD_LIBS) $(LSLIBS)
endif



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
