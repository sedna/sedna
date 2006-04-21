#
# Makefile for authentication
#

PP = ../../..


include $(PP)/Makefile.include

VPATH = $(PP)/kernel/tr


all: libauth$(LIB_EXT)
	@echo ===================================================================
	@echo Authorization Library has been done
	@echo ===================================================================


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/tr/depend
endif


AUTH_LIB = libauth$(LIB_EXT)

################################################################################
# auth                                                    #
################################################################################
AUTH_LIB_OBJS = auth$(OBJ_EXT)                      \
                auc$(OBJ_EXT)                      \
                prec-qs$(OBJ_EXT)		


$(AUTH_LIB): $(AUTH_LIB_OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^



################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) *$(LIB_EXT)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
