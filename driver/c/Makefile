#
# Makefile for libsedna
#

PP = ../..

VPATH = . $(PP)/kernel/common/u $(PP)/kernel/common/errdbg $(PP)/kernel/common

include $(PP)/Makefile.include

ifeq ("$(PLATFORM)", "WIN32")
all: libsednamt$(LIB_EXT) libsednamd$(LIB_EXT) sednamt$(DL_EXT)
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

MT_OBJS =    $(OBJS:$(OBJ_EXT)=.mt$(OBJ_EXT))
DL_MT_OBJS = $(OBJS:$(OBJ_EXT)=.dl.mt$(OBJ_EXT))

error_codes.d: error.codes
	NO_DEP=1; export NO_DEP; $(MAKE) -C $(PP)/kernel/common/errdbg generated
	touch error_codes.d


ifeq ("$(PLATFORM)", "WIN32")

# WIN32
OBJ_NAME = /Fo
CFLAGS += /DSE_NO_EVENT_LOG
CFLAGS_DL += /DSE_NO_EVENT_LOG

# static multi-threaded library linked with /MT[d]
libsednamt$(LIB_EXT): $(MT_OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

# static multi-threaded library linked with /MD[d]
libsednamd$(LIB_EXT): $(DL_MT_OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

# dynamic multi-threaded library linked with /MT[d]
sednamt$(DL_EXT): $(MT_OBJS)
	$(LD) $(LFLAGS_NOLIB) /DLL /DEF:sednamt$(DEF_EXT) $(LIBOUT)$@  $^
	if (test -e $@.manifest); then \
	    $(MT) -nologo -manifest $@.manifest -outputresource:$@\;2; \
	    $(REMOVE) $@.manifest; \
	else \
	    echo "There is no manifest file to embed."; \
	fi

%.mt$(OBJ_EXT): %.d

%.mt$(OBJ_EXT): %.c
	$(CC) $(CFLAGS) $(OBJ_NAME)$@ $<

%.dl.mt$(OBJ_EXT): %.d

%.dl.mt$(OBJ_EXT): %.c
	$(CC) $(CFLAGS_DL) $(OBJ_NAME)$@ $<

else # UNIX
LD=gcc # It must be enough to build C-driver!
OBJ_NAME = -o
CFLAGS += $(CFLAGS_SL)

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
	-$(REMOVE) *.exp *$(DL_EXT)

