#
# Makefile for se_exp
#

PP = ..

include $(PP)/Makefile.include

all: $(PP)/bin/se_exp$(EXE_EXT)
	@echo ===================================================================
	@echo Sedna export utility has been done
	@echo ===================================================================


OBJS = se_exp$(OBJ_EXT) se_exp_common$(OBJ_EXT) se_exp_export$(OBJ_EXT) \
       se_exp_import$(OBJ_EXT) sprompt$(OBJ_EXT)

COMMON_FILES_LIB = $(PP)/kernel/common/common_files$(LIB_EXT)

ifeq ($(PLATFORM), WIN32)
DRIVER_LIB = $(PP)/driver/c/libsednamt$(LIB_EXT)
DRIVER_LIB_S = $(DRIVER_LIB)
else
DRIVER_LIB = $(PP)/driver/c/libsedna$(LIB_EXT)
DRIVER_LIB_S = -L$(PP)/driver/c -lsedna
endif


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif


#############################################################################
# se_exp
#############################################################################
$(PP)/bin/se_exp$(EXE_EXT): $(OBJS) $(COMMON_FILES_LIB) $(DRIVER_LIB)
	$(LD) $(LFLAGS) $(LDOUT)$@ $(OBJS) $(COMMON_FILES_LIB) $(DRIVER_LIB_S) $(LSLIBS)

#############################################################################
# Clean
#############################################################################
.PHONY: clean

clean: generic_clean

