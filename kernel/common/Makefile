#
# Makefile for common_files
#

PP = ../..

include $(PP)/Makefile.include

OBJS = argtable$(OBJ_EXT) base$(OBJ_EXT) bit_set$(OBJ_EXT) gmm$(OBJ_EXT) ipc_ops$(OBJ_EXT) \
       pping$(OBJ_EXT) rcv_test$(OBJ_EXT) sp$(OBJ_EXT) SSMMsg$(OBJ_EXT) \
       tr_debug$(OBJ_EXT) ugc$(OBJ_EXT) utils$(OBJ_EXT) version$(OBJ_EXT) \
       xptr$(OBJ_EXT) sedna$(OBJ_EXT) rcv_test$(OBJ_EXT)
SUBDIRS = errdbg mmgr ph u

LIB_NAME = common_files



include $(PP)/Makefile.ordlib


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
$(OBJS:$(OBJ_EXT)=.d): errdbg/errdbg$(LIST_EXT)
$(OBJS): errdbg/errdbg$(LIST_EXT)
mmgr-recursive ph-recursive u-recursive: errdbg-recursive
endif

