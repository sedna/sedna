#
# Makefile for se_term
#

PP = ..

include $(PP)/Makefile.include

all: $(PP)/bin/se_term$(EXE_EXT)
	@echo ===================================================================
	@echo Sedna Terminal has been done
	@echo ===================================================================

ifeq ($(ENABLE_LIBEDIT),1)
CPPFLAGS += -I../libs/libedit/include -DENABLE_LIBEDIT
endif

OBJS = term_funcs$(OBJ_EXT) term_globals$(OBJ_EXT) sprompt$(OBJ_EXT) \
       startup$(OBJ_EXT) mainloop$(OBJ_EXT) term_ile$(OBJ_EXT) complet$(OBJ_EXT)

COMMON_FILES_LIB = $(PP)/kernel/common/common_files$(LIB_EXT)

ifeq ($(PLATFORM), WIN32)
DRIVER_LIB = $(PP)/driver/c/libsednamt$(LIB_EXT)
DRIVER_LIB_S = $(DRIVER_LIB)
else
DRIVER_LIB = $(PP)/driver/c/libsedna$(LIB_EXT)
DRIVER_LIB_S = -L$(PP)/driver/c -lsedna 
ifeq ($(ENABLE_LIBEDIT),1)
#libedit.so is picked if I try -ledit
DRIVER_LIB_S += $(PP)/libs/libedit/lib/libedit.a -lcurses
endif
endif


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifndef NO_DEP
include $(OBJS:$(OBJ_EXT)=.d)
endif
endif


#############################################################################
# se_term
#############################################################################
$(PP)/bin/se_term$(EXE_EXT): $(OBJS) $(COMMON_FILES_LIB) $(DRIVER_LIB)
	$(LD) $(LFLAGS) $(LDOUT)$@ $(OBJS) $(COMMON_FILES_LIB) $(DRIVER_LIB_S) $(LSLIBS)

#############################################################################
# Clean
#############################################################################
.PHONY: clean

clean: generic_clean

