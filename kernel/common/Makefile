#
# Makefile for COMMON_FILES
#

PP = ../..

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifeq ($(words $(wildcard depend)), 0)
ifneq ($(findstring depend, $(MAKECMDGOALS)), depend)
$(error Type "make depend" first)
endif
endif
endif


VPATH = . u ph mmgr errdbg


include $(PP)/Makefile.include

all: libcommon_files$(LIB_EXT)
	@echo ===================================================================
	@echo COMMON_FILES DONE
	@echo ===================================================================

include $(PP)/Makefile.depend

depend:
	$(DEPACT)


ifneq ($(words $(wildcard depend)), 0)
include depend
endif


################################################################################
# common_files                                                                 #
################################################################################
libcommon_files$(LIB_EXT): sedna$(OBJ_EXT)              \
                           base$(OBJ_EXT)				\
                           xptr$(OBJ_EXT)				\
                           argtable$(OBJ_EXT)			\
                           SSMMsg$(OBJ_EXT)				\
                           umutex$(OBJ_EXT)				\
                           u$(OBJ_EXT)					\
                           usocket$(OBJ_EXT)			\
                           uthread$(OBJ_EXT)			\
                           uprocess$(OBJ_EXT)			\
                           upipe$(OBJ_EXT)				\
                           usem$(OBJ_EXT)				\
                           ushm$(OBJ_EXT)				\
                           uhdd$(OBJ_EXT)				\
                           ummap$(OBJ_EXT)				\
                           usecurity$(OBJ_EXT)			\
                           uutils$(OBJ_EXT)				\
                           utils$(OBJ_EXT)				\
                           exceptions$(OBJ_EXT)			\
                           ipc_ops$(OBJ_EXT)			\
                           sp$(OBJ_EXT)                 \
                           pers_free$(OBJ_EXT)			\
                           pers_heap$(OBJ_EXT)			\
                           pers_malloc$(OBJ_EXT)		\
                           pers_realloc$(OBJ_EXT)		\
                           pping$(OBJ_EXT)				\
                           version$(OBJ_EXT)			\
                           ugc$(OBJ_EXT)				\
                           error_codes$(OBJ_EXT)		\
                           error_codes_scm$(OBJ_EXT)	\
                           tr_debug$(OBJ_EXT)			\
                           gmm$(OBJ_EXT)				\
                           d_printf$(OBJ_EXT)           \
                           assert$(OBJ_EXT)             \
                           aset$(OBJ_EXT)               \
                           mcxt$(OBJ_EXT)               \
                           event_log$(OBJ_EXT)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


gen_error_codes$(OBJ_EXT): gen_error_codes.c
	$(CC) $(CFLAGS_CHICKEN_EXE) $<

gen_error_codes$(EXE_EXT): gen_error_codes$(OBJ_EXT)
	$(LD) $(LFLAGS_CHICKEN_EXE) $(LDOUT)$@ $^ $(LSLIBS_CHICKEN_EXE)

error_codes.h error_codes.c error_codes_scm.scm: gen_error_codes$(EXE_EXT) error.codes
	./gen_error_codes$(EXE_EXT)



################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) libcommon_files$(LIB_EXT)
	-$(REMOVE) error_codes.h
	-$(REMOVE) error_codes.c
	-$(REMOVE) gen_error_codes.c
	-$(REMOVE) gen_error_codes$(EXE_EXT)
	-$(REMOVE) error_codes_scm.scm
	-$(REMOVE) ErrorCodes.java

