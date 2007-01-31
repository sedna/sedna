#
# Makefile for bufmgr (GNU make)
#

PP = ../../..

include $(PP)/Makefile.include

all: bufmgr$(LIB_EXT) 
	@echo ===================================================================
	@echo Logical Log has been Done
	@echo ===================================================================

OBJS = blk_mngmt$(OBJ_EXT) bm_core$(OBJ_EXT) bm_functions$(OBJ_EXT) \
       bm_rcv$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# bufmgr_LIB                                                                   #
################################################################################
bufmgr$(LIB_EXT): $(OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
