#
# Makefile for btree
#

PP = ../../../..

include $(PP)/Makefile.include

all: btree$(LIB_EXT)
	@echo ===================================================================
	@echo btree DONE
	@echo ===================================================================

OBJS = auxi$(OBJ_EXT) bisection$(OBJ_EXT) btree$(OBJ_EXT) btstruct$(OBJ_EXT) \
       buff$(OBJ_EXT) cmp$(OBJ_EXT) delete$(OBJ_EXT) find$(OBJ_EXT) \
       insert$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# btree.lib                                                                    #
################################################################################
btree$(LIB_EXT): $(OBJS)
	$(PLD) $(PLDOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
