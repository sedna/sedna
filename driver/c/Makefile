#
# Makefile for libsedna
#

PP = ../..

VPATH = . $(PP)/kernel/common/u $(PP)/kernel/common/errdbg $(PP)/kernel/common

include $(PP)/Makefile.include

ifeq ("$(PLATFORM)", "WIN32")
all: libsednamt$(LIB_EXT) libsedna$(LIB_EXT)
	@echo ===================================================================
	@echo C Driver Done
	@echo ===================================================================
else
all: libsedna$(LIB_EXT) 
	@echo ===================================================================
	@echo C Driver Done
	@echo ===================================================================
endif

OBJS = libsedna$(OBJ_EXT) usocket$(OBJ_EXT) uhdd$(OBJ_EXT) sp$(OBJ_EXT) \
       uutils$(OBJ_EXT) usecurity$(OBJ_EXT) d_printf$(OBJ_EXT) \
       error_codes$(OBJ_EXT) u$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifndef NO_DEP
include $(OBJS:$(OBJ_EXT)=.d)
endif
endif

MT_OBJS =     $(OBJS:$(OBJ_EXT)=.mt$(OBJ_EXT))
ST_OBJS =     $(OBJS:$(OBJ_EXT)=.st$(OBJ_EXT))
DLL_MT_OBJS = $(OBJS:$(OBJ_EXT)=.dl.mt$(OBJ_EXT))
DLL_ST_OBJS = $(OBJS:$(OBJ_EXT)=.dl.st$(OBJ_EXT))


error_codes.d: gen_error_codes.c
	NO_DEP=1; export NO_DEP; $(MAKE) -C $(PP)/kernel/common/errdbg generated


ifeq ("$(PLATFORM)", "WIN32")
# WIN32
OBJ_NAME = /Fo
CFLAGS += /DSE_NO_EVENT_LOG
CFLAGS_ST += /DSE_NO_EVENT_LOG

# multi-threaded static library
libsednamt$(LIB_EXT): $(MT_OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

%.mt$(OBJ_EXT): %.d

%.mt$(OBJ_EXT): %.c
	$(CC) $(CFLAGS) $(OBJ_NAME)$@ $<

# single-threaded static library
libsedna$(LIB_EXT): $(ST_OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

%.st$(OBJ_EXT): %.d

%.st$(OBJ_EXT): %.c
	$(CC) $(CFLAGS_ST) $(OBJ_NAME)$@ $<

# multi-threaded dynamic library
# libsednamt$(DL_EXT): $(DLL_MT_OBJS)
#	link /DLL  $(LIBFLAGS) $(LIBOUT)$@ $^ wsock32.lib Advapi32.lib
#
#%.dl.mt$(OBJ_EXT): %.d
#
#%.dl.mt$(OBJ_EXT): %.c
#	$(CC) $(CFLAGS) /MD $(OBJ_NAME)$@ $<

else
# UNIX 
OBJ_NAME = -o
CFLAGS += -DSE_NO_EVENT_LOG

libsedna$(LIB_EXT): $(MT_OBJS)
	  $(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

%.mt$(OBJ_EXT): %.d

%.mt$(OBJ_EXT): %.c
	$(CC) $(CFLAGS) $(OBJ_NAME) $@ $<
endif


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean 

clean: generic_clean

