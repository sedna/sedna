#
# Makefile for authentication
#

PP = ../../..


include $(PP)/Makefile.include

all: auth$(LIB_EXT)
	@echo ===================================================================
	@echo Authorization Library has been done
	@echo ===================================================================

OBJS = auth$(OBJ_EXT) auc$(OBJ_EXT) prec-qs$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# auth                                                                         #
################################################################################
auth$(LIB_EXT): $(OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) auth.c
	-$(REMOVE) prec-qs.c