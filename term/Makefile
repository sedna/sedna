PP = ..

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


COMMON_FILES_LIB = $(PP)/kernel/common/libcommon_files$(LIB_EXT)

ifeq ("$(PLATFORM)", "WIN32")
DRIVER_LIB = $(PP)/driver/c/libsednamt$(LIB_EXT)
else
DRIVER_LIB = $(PP)/driver/c/libsedna$(LIB_EXT)
endif

STERM_OBJS = term_funcs$(OBJ_EXT)\
             term_globals$(OBJ_EXT)\
             sprompt$(OBJ_EXT)\
             startup$(OBJ_EXT)\
             mainloop$(OBJ_EXT)
        



#############################################################################
# term
#############################################################################

$(PP)/bin/se_term$(EXE_EXT): $(STERM_OBJS) $(DRIVER_LIB)
	$(LD) $(LFLAGS) $(LDOUT)$@ $(STERM_OBJS) $(LSLIBS) $(DRIVER_LIB) $(COMMON_FILES_LIB)


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

