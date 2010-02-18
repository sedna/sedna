#
# Makefile for vmm
#

PP = ../../..

include $(PP)/Makefile.include


ifeq ("$(PLATFORM)","WIN32")
OBJS = os_exceptions$(OBJ_EXT) vmm$(OBJ_EXT) vmmwin32$(OBJ_EXT) vmmtrace$(OBJ_EXT)
else
OBJS = os_exceptions$(OBJ_EXT) vmm$(OBJ_EXT) vmmunix$(OBJ_EXT) vmmtrace$(OBJ_EXT)
endif                         

include $(PP)/Makefile.ordlib
