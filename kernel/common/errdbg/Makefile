#
# Makefile for errdbg (GNU make)
#

PP = ../../..

include $(PP)/Makefile.include

OBJS = assert$(OBJ_EXT) d_printf$(OBJ_EXT) event_log$(OBJ_EXT) \
       exceptions$(OBJ_EXT) error_codes$(OBJ_EXT) \
       error_codes_scm$(OBJ_EXT)

GENERATED = error_codes.h error_codes.c error_codes_scm.scm ErrorCodes.java
CUSTOM_CLEAN = clean_generated

include $(PP)/Makefile.pseudolib

################################################################################
# generated                                                                    #
################################################################################
generated: gen_error_codes$(EXE_EXT) error.codes
	./gen_error_codes$(EXE_EXT)
	touch generated

gen_error_codes$(EXE_EXT): gen_error_codes$(OBJ_EXT)
	$(LD) $(LFLAGS_NOLIB) $(LDOUT)$@ $^

gen_error_codes$(OBJ_EXT): gen_error_codes.c
	$(CC) $(CFLAGS) $<

################################################################################
# Clean                                                                        #
################################################################################
clean_generated:
	-$(REMOVE) error_codes.h error_codes.c gen_error_codes$(EXE_EXT) error_codes_scm.scm error_codes_scm.c ErrorCodes.java
