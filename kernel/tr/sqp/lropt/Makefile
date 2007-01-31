#
# Makefile for lropt
#

PP = ../../../..

include $(PP)/Makefile.include

all: lropt$(LIB_EXT)
	@echo ===================================================================
	@echo lropt DONE
	@echo ===================================================================

OBJS = lreturn$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# lropt.lib                                                                    #
################################################################################
lropt$(LIB_EXT): $(OBJS)
	$(PLD) $(PLDOUT)$@ $^

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) lreturn.c
