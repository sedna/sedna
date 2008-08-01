#
# Makefile for u (GNU make)
#

PP = ../../..


include $(PP)/Makefile.include



ifeq ("$(OS)", "CYGWIN_NT")
VPATH =  $(PP)/kernel/common/st/os_nt
else
VPATH =  $(PP)/kernel/common/st/os_other
endif

OBJS = stacktrace$(OBJ_EXT) stacktrfmt$(OBJ_EXT)

include $(PP)/Makefile.pseudolib
