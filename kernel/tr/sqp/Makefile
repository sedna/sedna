#
# Makefile for sqp (GNU make)
#

PP = ../../..


include $(PP)/Makefile.include

VPATH = lr2por lropt popt sa tools $(PP)/kernel/tr


all: libsqp$(LIB_EXT)
	@echo ===================================================================
	@echo sqp has been done
	@echo ===================================================================


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/tr/depend
endif


SQP_LIB = libsqp$(LIB_EXT)

################################################################################
# SQP                                                                          #
################################################################################
$(SQP_LIB): common-lib$(OBJ_EXT)		\
            lr2por$(OBJ_EXT)			\
            por-cons$(OBJ_EXT)			\
            lreturn$(OBJ_EXT)			\
            lr2por-lib$(OBJ_EXT)		\
            xquery-lr-lib$(OBJ_EXT)		\
            sa$(OBJ_EXT)				\
            popt$(OBJ_EXT)				\
            cpp_popt$(OBJ_EXT)			\
            att$(OBJ_EXT)				\
            att_xpath$(OBJ_EXT)			\
            serialize2lr$(OBJ_EXT)		\
            popt_plan$(OBJ_EXT)			\
            dschema$(OBJ_EXT)			\
            impl-differences$(OBJ_EXT)	\
            $(PP)/kernel/common/error_codes_scm$(OBJ_EXT)		
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^



################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) *$(LIB_EXT)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors

