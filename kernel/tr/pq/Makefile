#
# Makefile for pq (GNU make)
#

PP = ../../..


VPATH = $(PP)/kernel/tr


include $(PP)/Makefile.include

all: libpq$(LIB_EXT)
	@echo ===================================================================
	@echo PQ DONE
	@echo ===================================================================


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(PP)/kernel/tr/depend
endif


PQ_LIB = libpq$(LIB_EXT)

################################################################################
# Prepare Query                                                                #
################################################################################

scm-debug.scm: scm-debug.scm.m4
	$(M4) -D EL_DEBUG=$(EL_DEBUG) < $< > $@

PQ_OBJS = pq$(OBJ_EXT) 					\
          scm-entry$(OBJ_EXT)			\
          scm-debug$(OBJ_EXT)


$(PQ_LIB): $(PQ_OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^



################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) $(PQ_LIB)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) scm-debug.scm
