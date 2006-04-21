PP = ..

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifeq ($(words $(wildcard depend)), 0)
ifneq ($(findstring depend, $(MAKECMDGOALS)), depend)
$(error Type "make depend" first)
endif
endif
endif


VPATH = .

include $(PP)/Makefile.include

all: $(PP)/bin/se_exp$(EXE_EXT)
	@echo ===================================================================
	@echo Sedna Export has been done
	@echo ===================================================================



include $(PP)/Makefile.depend

depend:
	$(DEPACT)

ifneq ($(words $(wildcard depend)), 0)
include depend
endif


COMMON_FILES_LIB = $(PP)/kernel/common/libcommon_files$(LIB_EXT)

ifeq ("$(PLATFORM)", "WIN32")
DRIVER_LIB = $(PP)/driver/c/libsednamt$(LIB_EXT)
else
DRIVER_LIB = $(PP)/driver/c/libsedna$(LIB_EXT)
endif

SE_EXP_OBJS = se_exp$(OBJ_EXT)\
              se_exp_common$(OBJ_EXT)\
              se_exp_export$(OBJ_EXT)\
              se_exp_import$(OBJ_EXT)\
              sprompt$(OBJ_EXT)



#############################################################################
# export
#############################################################################

$(PP)/bin/se_exp$(EXE_EXT): $(SE_EXP_OBJS)
	$(LD) $(LFLAGS) $(LDOUT)$@ $(SE_EXP_OBJS) $(DRIVER_LIB) $(COMMON_FILES_LIB) $(LSLIBS)


#############################################################################
# clean
#############################################################################
clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors

