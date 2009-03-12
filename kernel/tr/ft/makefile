#
# Makefile for FT
#

PP = ../../..

VPATH =  $(PP)/libs/dtsearch/common

include $(PP)/Makefile.include

OBJS = ft_index_data$(OBJ_EXT) ft_index$(OBJ_EXT) ft_cache$(OBJ_EXT)

ifeq ($(ENABLE_DTSEARCH), 1)

OBJS += FTsearch$(OBJ_EXT) FTindex$(OBJ_EXT)

OBJS_NODEP += dtsviewr$(OBJ_EXT) \
        dstring$(OBJ_EXT) \
        dfile$(OBJ_EXT) \
        dinputstream$(OBJ_EXT) \
        dtsfc$(OBJ_EXT) \
        dtsearch$(OBJ_EXT)
endif

include $(PP)/Makefile.ordlib