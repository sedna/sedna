#
# Makefile for btree (GNU make)
#

PP = ../../../..

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifeq ($(words $(wildcard depend)), 0)
ifneq ($(findstring depend, $(MAKECMDGOALS)), depend)
$(error Type "make depend" first)
endif
endif
endif


INCLUDE_DIRS = . $(PP)/kernel/common $(PP)/kernel/common/u $(PP)/driver/c	\
               $(PP)/kernel/tr/vmm $(PP)/kernel/tr/crmutils		\
               $(PP)/kernel/common/ph $(PP)/kernel/tr/mo		\
               $(PP)/kernel/tr/locks $(PP)/kernel/sm/plmgr		\
               $(PP)/kernel/tr/executor/base					\
               $(PP)/kernel/tr/structures						\
               $(PP)/kernel/tr/nid


VPATH = .


include $(PP)/Makefile.include

all: libbtree$(LIB_EXT)
	@echo ===================================================================
	@echo BTREE DONE
	@echo ===================================================================

include $(PP)/Makefile.depend

depend:
	$(DEPACT)


ifneq ($(words $(wildcard depend)), 0)
include depend
endif


#COMMON_FILES_LIB = $(PP)/kernel/common/common_files$(LIB_EXT)
#CRMUTILS_LIB = $(PP)/kernel/tr/crmutils/crmutils$(LIB_EXT)
#LOCKS_LIB = $(PP)/kernel/tr/locks/locks$(LIB_EXT)
#MO_LIB = $(PP)/kernel/tr/mo/mo$(LIB_EXT)
#PSTR_LIB = $(PP)/kernel/tr/pstr/pstr$(LIB_EXT)
#UPDATES_LIB = $(PP)/kernel/tr/updates/updates$(LIB_EXT)
#NID_LIB = $(PP)/kernel/tr/nid/nid$(LIB_EXT)
#VMM_LIB = $(PP)/kernel/tr/vmm/vmm$(LIB_EXT)
#XQP_LIB = $(PP)/kernel/tr/xqp/xqp$(LIB_EXT)
#EXECUTOR_LIB = $(PP)/kernel/tr/executor/executor$(LIB_EXT)
#PREPARE_QUERY_LIB = $(PP)/kernel/tr/pq/prepare_query$(LIB_EXT)
#PERS_HEAP_LIB = $(PP)/libs/pers_heap/pers_heap$(LIB_EXT)

#TEST_OBJS = test_overall$(OBJ_EXT)	\
#			test_find$(OBJ_EXT)		\
#			test_helpers$(OBJ_EXT)	\
#			btree$(LIB_EXT)			\
#			test_insert$(OBJ_EXT)	\
#			test_cmp_bisection$(OBJ_EXT)

#LIB_OBJS =  $(CRMUTILS_LIB)				\
#           $(LOCKS_LIB)					\
#           $(MO_LIB)					\
#           $(PSTR_LIB)					\
#           $(UPDATES_LIB)				\
#           $(NID_LIB)					\
#           $(VMM_LIB)					\
#           $(XQP_LIB)					\
#           $(EXECUTOR_LIB)				\
#           $(PREPARE_QUERY_LIB)			\
#           $(COMMON_FILES_LIB)			\
#           $(PERS_HEAP_LIB)


################################################################################
# btree                                                                        #
################################################################################
BTREE_OBJS = auxi$(OBJ_EXT)			\
             bisection$(OBJ_EXT)	\
             btree$(OBJ_EXT)		\
             btstruct$(OBJ_EXT)		\
             buff$(OBJ_EXT)			\
             cmp$(OBJ_EXT)			\
             delete$(OBJ_EXT)		\
             find$(OBJ_EXT)			\
             insert$(OBJ_EXT)

libbtree$(LIB_EXT): $(BTREE_OBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

#test$(EXE_EXT): $(TEST_OBJS)
#	$(LD) $(LFLAGS) $(LDOUT)$@ $(TEST_OBJS) $(LIB_OBJS)


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) *$(LIB_EXT)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) vc60.idb
	-$(REMOVE) vc60.pdb
	-$(REMOVE) vc70.idb
	-$(REMOVE) vc70.pdb
	-$(REMOVE) test$(EXE_EXT)
	-$(REMOVE) *.map
	-$(REMOVE) *.pdb
	-$(REMOVE) *.SUP
