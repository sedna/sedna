#
# Makefile for popt
#

PP = ../../../..

include $(PP)/Makefile.include

all: popt$(LIB_EXT)
	@echo ===================================================================
	@echo popt DONE
	@echo ===================================================================

OBJS = att$(OBJ_EXT) att_xpath$(OBJ_EXT) cpp_popt$(OBJ_EXT) \
       popt_plan$(OBJ_EXT) serialize2lr$(OBJ_EXT) popt$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# popt.lib                                                                     #
################################################################################
popt$(LIB_EXT): $(OBJS)
	$(PLD) $(PLDOUT)$@ $^

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) popt.c
