#
# Makefile for FT
#

PP = ../../..


VPATH = $(PP)/kernel/tr $(PP)/libs/dtsearch/common


include $(PP)/Makefile.include



all: libft$(LIB_EXT)
	@echo ===================================================================
	@echo FT DONE
	@echo ===================================================================


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/tr/depend
endif


.PHONY: clean libraries


FT_LIB = libft$(LIB_EXT)

DTOBJS = dtsviewr$(OBJ_EXT) \
		       dstring$(OBJ_EXT) \
		       dfile$(OBJ_EXT) \
		       dinputstream$(OBJ_EXT) \
		       dtsfc$(OBJ_EXT) \
		       dtsearch$(OBJ_EXT)

################################################################################
# ft.lib                                                                       #
################################################################################
$(FT_LIB): FTsearch$(OBJ_EXT) FTindex$(OBJ_EXT) ft_index_data$(OBJ_EXT) $(DTOBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^


################################################################################
# Clean                                                                        #
################################################################################
clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) *$(LIB_EXT)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) log

