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

all: $(PP)/bin/se_term$(EXE_EXT)
	@echo ===================================================================
	@echo Sedna Terminal has been done
	@echo ===================================================================



include $(PP)/Makefile.depend

depend:
	$(DEPACT)

ifneq ($(words $(wildcard depend)), 0)
include depend
endif


ifeq ("$(PLATFORM)", "WIN32")
DRIVER_LIB = $(PP)/driver/c/libsednamt$(LIB_EXT)
else
DRIVER_LIB = $(PP)/driver/c/libsedna$(LIB_EXT)
endif

COMMON_FILES_LIB = $(PP)/kernel/common/libcommon_files$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
COMMON_FILES_S = $(COMMON_FILES_LIB)
else
COMMON_FILES_S = -L$(PP)/kernel/common -lcommon_files
endif

#############################################################################
# term
#############################################################################

STERM_OBJS = term_funcs$(OBJ_EXT)\
             term_globals$(OBJ_EXT)\
             sprompt$(OBJ_EXT)\
             startup$(OBJ_EXT)\
             mainloop$(OBJ_EXT)
        

$(PP)/bin/se_term$(EXE_EXT): $(STERM_OBJS) $(DRIVER_LIB) $(COMMON_FILES_LIB)
	$(LD) $(LFLAGS) $(LDOUT)$@ $(STERM_OBJS) $(LSLIBS) $(DRIVER_LIB) $(COMMON_FILES_S)


#############################################################################
# Clean
#############################################################################
clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) *.SUP
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb

