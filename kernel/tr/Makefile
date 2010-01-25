#
# Makefile for TR
#

PP = ../..

include $(PP)/Makefile.include

all: $(PP)/bin/se_trn$(EXE_EXT)
	@echo ===================================================================
	@echo TR DONE
	@echo ===================================================================


TRN_OBJS = tr$(OBJ_EXT) socket_client$(OBJ_EXT) cl_client$(OBJ_EXT) \
           tr_functions$(OBJ_EXT) tr_common_funcs$(OBJ_EXT) \
           tr_globals$(OBJ_EXT)

SUBDIRS  = auth cat crmutils executor idx locks log mo nid pstr rcv \
           strings structures updates vmm bstrie
           
ifeq ($(ENABLE_FTSEARCH), 1)
SUBDIRS += ft
endif

ifeq ($(ENABLE_TRIGGERS), 1)
SUBDIRS += triggers
endif

SUBDIRS_PARSER = xqp $(SUBDIRS)

SUBDIROBJS = $(foreach dir,$(SUBDIRS_PARSER),$(dir)/$(dir)$(LIB_EXT))


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifndef NO_DEP
include $(TRN_OBJS:$(OBJ_EXT)=.d)
endif
endif


COMMON_FILES_LIB = $(PP)/kernel/common/common_files$(LIB_EXT)
LOGICAL_LOG_LIB = $(PP)/kernel/sm/llsm/llcommon$(LIB_EXT)

ifeq ($(PLATFORM), WIN32)
LFLAGS += /STACK:104857600
LFLAGS_DTS += /STACK:104857600
endif

ifeq ($(ENABLE_DTSEARCH), 1)
ifeq ($(PLATFORM), WIN32)
ifeq ("$(ACTIVE_CONFIGURATION)","Debug")
FT_ADD_LIB = dten600.lib Nafxcwd.lib LIBCMTD.LIB
else
FT_ADD_LIB = dten600.lib Nafxcw.lib LIBCMT.LIB
endif
else
FT_ADD_LIB =
endif
else
FT_ADD_LIB =
endif


DEPENDS_ON_LIBS = $(COMMON_FILES_LIB) $(LOGICAL_LOG_LIB)


################################################################################
# tr (Transaction)                                                             #
################################################################################
ifeq ($(ENABLE_DTSEARCH), 1)
E_LFLAGS = $(LFLAGS_DTS)
else
E_LFLAGS = $(LFLAGS)
endif


ifeq ("$(PLATFORM)", "WIN32")
$(PP)/bin/se_trn$(EXE_EXT): $(TRN_OBJS) $(SUBDIROBJS) $(DEPENDS_ON_LIBS)
	$(LD) $(E_LFLAGS) $(LDOUT)$@ $^ $(LSLIBS) $(FT_ADD_LIB)
else
$(PP)/bin/se_trn$(EXE_EXT): $(TRN_OBJS) $(SUBDIROBJS) $(DEPENDS_ON_LIBS)
	$(LD) $(LFLAGS) $(LDOUT)$@ $^  $(SUBDIROBJS) $(DEPENDS_ON_LIBS) $(LSLIBS) $(FT_ADD_LIB)
endif

$(TRN_OBJS:$(OBJ_EXT)=.cpp): xqp-recursive

$(SUBDIROBJS): $(SUBDIRS_PARSER:%=%-recursive) ;

.PHONY: $(SUBDIRS_PARSER:%=%-recursive)
$(SUBDIRS:%=%-recursive): xqp-recursive
	$(MAKE) -C $(subst -recursive,,$@)
xqp-recursive:
	$(MAKE) -C $(subst -recursive,,$@)

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	for dir in $(SUBDIRS); do $(MAKE) -C $$dir $@ || exit; done
