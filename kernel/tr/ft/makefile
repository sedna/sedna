#
# Makefile for FT
#

PP = ../../..

VPATH =  $(PP)/libs/dtsearch/common

include $(PP)/Makefile.include

all: ft$(LIB_EXT)
	@echo ===================================================================
	@echo FT DONE
	@echo ===================================================================

OBJS = FTsearch$(OBJ_EXT) FTindex$(OBJ_EXT) ft_index_data$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif


DTOBJS = dtsviewr$(OBJ_EXT) \
         dstring$(OBJ_EXT) \
         dfile$(OBJ_EXT) \
         dinputstream$(OBJ_EXT) \
         dtsfc$(OBJ_EXT) \
         dtsearch$(OBJ_EXT)


################################################################################
# ft.lib                                                                       #
################################################################################
ft$(LIB_EXT): $(OBJS) $(DTOBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
