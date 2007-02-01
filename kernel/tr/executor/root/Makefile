#
# Makefile for root (GNU make)
#

PP = ../../../..

include $(PP)/Makefile.include

OBJS = PPBulkLoad$(OBJ_EXT) PPCreateIndex$(OBJ_EXT) \
       PPCreateMetadata$(OBJ_EXT) PPDeleteDeep$(OBJ_EXT) \
       PPDeleteUndeep$(OBJ_EXT) PPDropIndex$(OBJ_EXT) \
       PPDropMetadata$(OBJ_EXT) PPDropModule$(OBJ_EXT) \
       PPInsertBefore$(OBJ_EXT) PPInsertFollowing$(OBJ_EXT) \
       PPInsertTo$(OBJ_EXT) PPLoadModule$(OBJ_EXT) PPQueryRoot$(OBJ_EXT) \
       PPRename$(OBJ_EXT) PPReplace$(OBJ_EXT) PPRetrieveDS$(OBJ_EXT) \
       PPRetrieveMetadata$(OBJ_EXT)

ifeq ($(ENABLE_DTSEARCH),1)
OBJS  +=   PPDropFtIndex$(OBJ_EXT) PPCreateFtIndex$(OBJ_EXT)
endif

include $(PP)/Makefile.pseudolib
