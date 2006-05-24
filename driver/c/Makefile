#
# $Id: 
#
# Makefile for libsedna
#

PP = ../..

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifeq ($(words $(wildcard depend)), 0)
ifneq ($(findstring depend, $(MAKECMDGOALS)), depend)
$(error Type "make depend" first)
endif
endif
endif



VPATH = .
include $(PP)/Makefile.include


ifeq ("$(PLATFORM)", "WIN32")
all: libsedna$(LIB_EXT) libsednamt$(LIB_EXT)
	@echo ===================================================================
	@echo libsedna, libsednamt done
	@echo ===================================================================
else
all: libsedna$(LIB_EXT) 
	@echo ===================================================================
	@echo libsedna done
	@echo ===================================================================
endif

include $(PP)/Makefile.depend

depend:
	$(DEPACT)


ifneq ($(words $(wildcard depend)), 0)
include depend
endif



ifeq ("$(PLATFORM)", "WIN32")
# WIN32


OBJ_NAME = /Fo
CFLAGS += /DSE_NO_EVENT_LOG
CFLAGS_ST += /DSE_NO_EVENT_LOG

%$(OBJ_EXT): $(PP)/kernel/common/u/%.c
	$(CC) $(CFLAGS) $<

%$(OBJ_EXT): $(PP)/kernel/common/errdbg/%.c
	$(CC) $(CFLAGS) $<

%$(OBJ_EXT): $(PP)/kernel/common/%.c
	$(CC) $(CFLAGS) $<

# multi threaded library - libsednamt
libsednamt$(LIB_EXT): libsedna$(OBJ_EXT) usocket$(OBJ_EXT) uhdd$(OBJ_EXT) sp$(OBJ_EXT) uutils$(OBJ_EXT) \
                      usecurity$(OBJ_EXT) d_printf$(OBJ_EXT) error_codes$(OBJ_EXT) u$(OBJ_EXT)
	  $(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


# single threaded library - libsedna
libsednast$(OBJ_EXT):libsedna.c
		$(CC) $(CFLAGS_ST) $(OBJ_NAME)libsednast$(OBJ_EXT) $<
usocketst$(OBJ_EXT):$(PP)/kernel/common/u/usocket.c
		$(CC) $(CFLAGS_ST) $(OBJ_NAME)usocketst$(OBJ_EXT)  $<
uhddst$(OBJ_EXT):$(PP)/kernel/common/u/uhdd.c
		$(CC) $(CFLAGS_ST) $(OBJ_NAME)uhddst$(OBJ_EXT)  $<
spst$(OBJ_EXT):$(PP)/kernel/common/sp.c
		$(CC) $(CFLAGS_ST) $(OBJ_NAME)spst$(OBJ_EXT)  $<
uutilsst$(OBJ_EXT):$(PP)/kernel/common/u/uutils.c
		$(CC) $(CFLAGS_ST) $(OBJ_NAME)uutilsst$(OBJ_EXT)  $<
usecurityst$(OBJ_EXT):$(PP)/kernel/common/u/usecurity.c
		$(CC) $(CFLAGS_ST) $(OBJ_NAME)usecurityst$(OBJ_EXT)  $<
d_printfst$(OBJ_EXT):$(PP)/kernel/common/errdbg/d_printf.c
		$(CC) $(CFLAGS_ST) $(OBJ_NAME)d_printfst$(OBJ_EXT)  $<
error_codesst$(OBJ_EXT):$(PP)/kernel/common/error_codes.c
		$(CC) $(CFLAGS_ST) $(OBJ_NAME)error_codesst$(OBJ_EXT)  $<
ust$(OBJ_EXT):$(PP)/kernel/common/u/u.c
		$(CC) $(CFLAGS_ST) $(OBJ_NAME)ust$(OBJ_EXT)  $<


libsedna$(LIB_EXT): libsednast$(OBJ_EXT) usocketst$(OBJ_EXT) uhddst$(OBJ_EXT) spst$(OBJ_EXT) uutilsst$(OBJ_EXT) \
                      usecurityst$(OBJ_EXT) d_printfst$(OBJ_EXT) error_codesst$(OBJ_EXT) ust$(OBJ_EXT)
	  $(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

# multithreaded dynamic library - libsednamt.dll
libsednamtdll$(OBJ_EXT):libsedna.c
		$(CC) $(CFLAGS_ST) /MD $(OBJ_NAME)libsednamtdll$(OBJ_EXT) $<
usocketmtdll$(OBJ_EXT):$(PP)/kernel/common/u/usocket.c
		$(CC) $(CFLAGS_ST) /MD $(OBJ_NAME)usocketmtdll$(OBJ_EXT)  $<
uhddsmtdll$(OBJ_EXT):$(PP)/kernel/common/u/uhdd.c
		$(CC) $(CFLAGS_ST) /MD $(OBJ_NAME)uhddsmtdll$(OBJ_EXT)  $<
spmtdll$(OBJ_EXT):$(PP)/kernel/common/sp.c
		$(CC) $(CFLAGS_ST) /MD $(OBJ_NAME)spmtdll$(OBJ_EXT)  $<
uutilsmtdll$(OBJ_EXT):$(PP)/kernel/common/u/uutils.c
		$(CC) $(CFLAGS_ST) /MD $(OBJ_NAME)uutilsmtdll$(OBJ_EXT)  $<
usecuritymtdll$(OBJ_EXT):$(PP)/kernel/common/u/usecurity.c
		$(CC) $(CFLAGS_ST) /MD $(OBJ_NAME)usecuritymtdll$(OBJ_EXT)  $<
d_printfmtdll$(OBJ_EXT):$(PP)/kernel/common/errdbg/d_printf.c
		$(CC) $(CFLAGS_ST) /MD $(OBJ_NAME)d_printfmtdll$(OBJ_EXT)  $<
error_codesmtdll$(OBJ_EXT):$(PP)/kernel/common/error_codes.c
		$(CC) $(CFLAGS_ST) /MD $(OBJ_NAME)error_codesmtdll$(OBJ_EXT)  $<
umtdll$(OBJ_EXT):$(PP)/kernel/common/u/u.c
		$(CC) $(CFLAGS_ST) /MD $(OBJ_NAME)umtdll$(OBJ_EXT)  $<


libsednamt$(DLL_EXT): libsednamtdll$(OBJ_EXT) usocketmtdll$(OBJ_EXT) uhddsmtdll$(OBJ_EXT)	\
                spmtdll$(OBJ_EXT) uutilsmtdll$(OBJ_EXT) \
                usecuritymtdll$(OBJ_EXT) d_printfmtdll$(OBJ_EXT) error_codesmtdll$(OBJ_EXT) umtdll$(OBJ_EXT)
	  $(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

else
# UNIX 

CFLAGS += -DSE_NO_EVENT_LOG

%$(OBJ_EXT): $(PP)/kernel/common/u/%.c
	$(CC) $(CFLAGS) $<

%$(OBJ_EXT): $(PP)/kernel/common/errdbg/%.c
	$(CC) $(CFLAGS) $<

%$(OBJ_EXT): $(PP)/kernel/common/%.c
	$(CC) $(CFLAGS) $<

libsedna$(LIB_EXT): usocket$(OBJ_EXT) libsedna$(OBJ_EXT) uhdd$(OBJ_EXT) sp$(OBJ_EXT) uutils$(OBJ_EXT) \
                      usecurity$(OBJ_EXT) d_printf$(OBJ_EXT) error_codes$(OBJ_EXT) u$(OBJ_EXT)
	  $(LB) $(LIBFLAGS) $(LIBOUT)$@ $^
endif


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean 

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) *$(LIB_EXT)

