#
# Makefile for libsedna
#

PP = ../..

VPATH = . $(PP)/kernel/common/u $(PP)/kernel/common/errdbg $(PP)/kernel/common

include $(PP)/Makefile.include


ifeq ("$(PLATFORM)", "WIN32")
all: libsednamt$(LIB_EXT) libsedna$(LIB_EXT)
	@echo ===================================================================
	@echo C API done
	@echo ===================================================================
else
all: libsedna$(LIB_EXT) 
	@echo ===================================================================
	@echo C API done
	@echo ===================================================================
endif

OBJS = libsedna$(OBJ_EXT) usocket$(OBJ_EXT) uhdd$(OBJ_EXT) sp$(OBJ_EXT) \
       uutils$(OBJ_EXT) usecurity$(OBJ_EXT) d_printf$(OBJ_EXT) \
       error_codes$(OBJ_EXT) u$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif


ifeq ("$(PLATFORM)", "WIN32")
# WIN32
ST_OBJS =     $(OBJS:$(OBJ_EXT)=.st$(OBJ_EXT))
DLL_OBJS =    $(OBJS:$(OBJ_EXT)=.dl$(OBJ_EXT))
DLL_ST_OBJS = $(OBJS:$(OBJ_EXT)=.dl.st$(OBJ_EXT))


OBJ_NAME = /Fo
CFLAGS += /DSE_NO_EVENT_LOG
CFLAGS_ST += /DSE_NO_EVENT_LOG


# multi-threaded static library
libsednamt$(LIB_EXT): $(OBJS)
	  $(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

# single-threaded static library
libsedna$(LIB_EXT): $(ST_OBJS)
	  $(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

%.st$(OBJ_EXT): %.d

%.st$(OBJ_EXT): %.c
	$(CC) $(CFLAGS_ST) $(OBJ_NAME)$@ $<

# multi-threaded dynamic library
#libsednamt$(DL_EXT): $(DLL_OBJS)
#	link /DLL $(LIBFLAGS) $(LIBOUT)$@ $^

#%.dl$(OBJ_EXT): %.c
#	$(CC) $(CFLAGS) /MD $(OBJ_NAME)$@ $<

else
# UNIX 

CFLAGS += -DSE_NO_EVENT_LOG

libsedna$(LIB_EXT): $(OBJS)
	  $(LB) $(LIBFLAGS) $(LIBOUT)$@ $^
endif


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean 

clean: generic_clean

