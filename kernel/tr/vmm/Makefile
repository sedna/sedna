#
# Makefile for vmm
#

PP = ../../..

include $(PP)/Makefile.include


ifeq ("$(PLATFORM)","WIN32")
OBJS = os_exceptions$(OBJ_EXT) vmm$(OBJ_EXT)  sym_engine$(OBJ_EXT) exndisphook$(OBJ_EXT)
else
OBJS = os_exceptions$(OBJ_EXT) vmm$(OBJ_EXT)
endif                         

include $(PP)/Makefile.ordlib
