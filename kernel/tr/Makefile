#
# Makefile for TR
#

PP = ../..

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
ifeq ($(words $(wildcard depend)), 0)
ifneq ($(findstring depend, $(MAKECMDGOALS)), depend)
$(error Type "make depend" first)
endif
endif
endif


VPATH = . structures

include $(PP)/Makefile.include

all: $(PP)/bin/se_trn$(EXE_EXT) $(PP)/bin/se_rcv$(EXE_EXT)
	@echo ===================================================================
	@echo TR DONE
	@echo ===================================================================

#test: tr_test$(EXE_EXT)
#	@echo ===================================================================
#	@echo tr_test DONE
#	@echo ===================================================================


include $(PP)/Makefile.depend

depend: $(PP)/kernel/tr/depend

$(PP)/kernel/tr/depend:
	$(DEPACT)




ifneq ($(words $(wildcard $(PP)/kernel/tr/depend)), 0)
include $(PP)/kernel/tr/depend
endif

.PHONY: clean libraries depend

COMMON_FILES_LIB = $(PP)/kernel/common/libcommon_files$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
COMMON_FILES_S = $(COMMON_FILES_LIB)
else
#COMMON_FILES_S = -L$(PP)/kernel/common -lcommon_files
COMMON_FILES_S =  -lcommon_files
COMMON_FILES_P = -L$(PP)/kernel/common
endif

CRMUTILS_LIB = crmutils/libcrmutils$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
CRMUTILS_S = $(CRMUTILS_LIB)
else
#CRMUTILS_S = -Lcrmutils -lcrmutils
CRMUTILS_S = -lcrmutils
CRMUTILS_P = -Lcrmutils
endif

EXECUTOR_LIB = executor/libexecutor$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
EXECUTOR_S = $(EXECUTOR_LIB)
else
#EXECUTOR_S = -Lexecutor -lexecutor
EXECUTOR_S = -lexecutor
EXECUTOR_P = -Lexecutor
endif

IDX_LIB = idx/libidx$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
IDX_S = $(IDX_LIB)
else
#IDX_S = -Lidx -lidx
IDX_S = -lidx
IDX_P = -Lidx
endif

ifeq ($(ENABLE_TRIGGERS), 1)
TRIGGERS_LIB = triggers/libtriggers$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
TRIGGERS_S = $(TRIGGERS_LIB)
else
#TRIGGERS_S = -Ltriggers -ltriggers
TRIGGERS_S = -ltriggers
TRIGGERS_P = -Ltriggers
endif
else
TRIGGERS_LIB =
TRIGGERS_S =
TRIGGERS_P =
endif

ifeq ($(ENABLE_DTSEARCH), 1)

FT_LIB = dten600.lib Nafxcw.lib LIBCMT.LIB ft/libft$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
FT_S = $(FT_LIB)
else
#FT_S = -Lft -lft
FT_S = -lft
FT_P = -Lft
endif

else

FT_LIB =
FT_S =
FT_P =

endif

BTREE_LIB = idx/btree/libbtree$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
BTREE_S = $(BTREE_LIB)
else
#IDX_S = -Lidx -lidx
BTREE_S = -lbtree
BTREE_P = -Lidx/btree
endif

LOCKS_LIB = locks/liblocks$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
LOCKS_S = $(LOCKS_LIB)
else
#LOCKS_S = -Llocks -llocks
LOCKS_S = -llocks
LOCKS_P = -Llocks
endif

HL_PHYS_LOG_LIB = log/libhl_phys_log$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
HL_PHYS_LOG_S = $(HL_PHYS_LOG_LIB)
else
#HL_PHYS_LOG_S = -Llog -lhl_phys_log
HL_PHYS_LOG_S = -lhl_phys_log
HL_PHYS_LOG_P = -Llog
endif

HL_LOGICAL_LOG_LIB = log/libhl_logical_log$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
HL_LOGICAL_LOG_S = $(HL_LOGICAL_LOG_LIB)
else
#HL_LOGICAL_LOG_S = -Llog -lhl_logical_log
HL_LOGICAL_LOG_S = -lhl_logical_log
HL_LOGICAL_LOG_P = -Llog
endif

MO_LIB = mo/libmo$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
MO_S = $(MO_LIB)
else
#MO_S = -Lmo -lmo
MO_S = -lmo
MO_P = -Lmo
endif

NID_LIB = nid/libnid$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
NID_S = $(NID_LIB)
else
#NID_S = -Lnid -lnid
NID_S = -lnid
NID_P = -Lnid
endif

PQ_LIB = pq/libpq$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
PQ_S = $(PQ_LIB)
else
#PQ_S = -Lpq -lpq
PQ_S = -lpq
PQ_P = -Lpq
endif

PSTR_LIB = pstr/libpstr$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
PSTR_S = $(PSTR_LIB)
else
#PSTR_S = -Lpstr -lpstr
PSTR_S = -lpstr
PSTR_P = -Lpstr
endif

SQP_LIB = sqp/libsqp$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
SQP_S = $(SQP_LIB)
else
#SQP_S = -Lsqp -lsqp
SQP_S = -lsqp
SQP_P = -Lsqp
endif

STRINGS_LIB = strings/libstrings$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
STRINGS_S = $(STRINGS_LIB)
else
#STRINGS_S = -Lstrings -lstrings
STRINGS_S = -lstrings
STRINGS_P = -Lstrings
endif

STRUCTURES_LIB = structures/libstructures$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
STRUCTURES_S = $(STRUCTURES_LIB)
else
#STRUCTURES_S = -Lstructures -lstructures
STRUCTURES_S = -lstructures
STRUCTURES_P = -Lstructures
endif

UPDATES_LIB = updates/libupdates$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
UPDATES_S = $(UPDATES_LIB)
else
#UPDATES_S = -Lupdates -lupdates
UPDATES_S = -lupdates
UPDATES_P = -Lupdates
endif

VMM_LIB = vmm/libvmm$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
VMM_S = $(VMM_LIB)
else
#VMM_S = -Lvmm -lvmm
VMM_S = -lvmm
VMM_P = -Lvmm
endif

XQP_LIB = xqp/libxqp$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
XQP_S = $(XQP_LIB)
else
#XQP_S = -Lxqp -lxqp
XQP_S = -lxqp
XQP_P = -Lxqp
endif

PHYS_LOG_LIB = $(PP)/kernel/sm/plmgr/libtr_phys_log_mgr$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
PHYS_LOG_S = $(PHYS_LOG_LIB)
else
#PHYS_LOG_S = -L$(PP)/kernel/sm/plmgr -ltr_phys_log_mgr
PHYS_LOG_S = -ltr_phys_log_mgr
PHYS_LOG_P = -L$(PP)/kernel/sm/plmgr
endif

LOGICAL_LOG_LIB = $(PP)/kernel/sm/llmgr/liblogical_log_mgr$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
LOGICAL_LOG_S = $(LOGICAL_LOG_LIB)
else
#LOGICAL_LOG_S = -L$(PP)/kernel/sm/llmgr -llogical_log_mgr
LOGICAL_LOG_S = -llogical_log_mgr
LOGICAL_LOG_P = -L$(PP)/kernel/sm/llmgr
endif

AUTH_LIB = auth/libauth$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
AUTH_S = $(AUTH_LIB)
else
#AUTH_S = -Lauth -lauth
AUTH_S = -lauth
AUTH_P = -Lauth
endif

RCV_LIB = rcv/librcv$(LIB_EXT)
ifeq ($(PLATFORM), WIN32)
RCV_S = $(RCV_LIB)
else
RCV_S = -lrcv
RCV_P = -Lrcv
endif


$(COMMON_FILES_LIB): libraries

$(CRMUTILS_LIB): libraries

$(EXECUTOR_LIB): libraries

$(IDX_LIB): libraries

ifeq ($(ENABLE_DTSEARCH), 1)
$(FT_LIB): libraries
endif

$(BTREE_LIB): libraries

$(LOCKS_LIB): libraries

$(HL_PHYS_LOG): libraries

$(HL_LOGICAL_LOG): libraries

$(MO_LIB): libraries

$(NID_LIB): libraries

$(PQ_LIB): libraries

$(PSTR_LIB): libraries

$(SQP_LIB): libraries

$(STRINGS_LIB): libraries

$(STRUCTURES_LIB): libraries

$(UPDATES_LIB): libraries

$(VMM_LIB): libraries

$(XQP_LIB): libraries

$(AUTH_LIB): libraries

$(RCV_LIB): libraries

ifeq ($(ENABLE_TRIGGERS), 1)
$(TRIGGERS_LIB): libraries
endif

################################################################################
# tr (Transaction)                                                             #
################################################################################
TRN_OBJS = tr$(OBJ_EXT)					\
           socket_client$(OBJ_EXT)      \
           cl_client$(OBJ_EXT)          \
           tr_functions$(OBJ_EXT)       \
           tr_common_funcs$(OBJ_EXT)    \
           tr_globals$(OBJ_EXT)		

TRN_RCV_OBJS = rcv_db$(OBJ_EXT)         \
               tr_globals$(OBJ_EXT)     \
               tr_common_funcs$(OBJ_EXT)    

#BL_OBJS =  bl$(OBJ_EXT)					\
#           tr_globals$(OBJ_EXT)         \
#           $(CRMUTILS_LIB)				\
#           $(LOCKS_LIB)					\
#           $(MO_LIB)					\
#           $(PSTR_LIB)					\
#           $(UPDATES_LIB)				\
#           $(NID_LIB)					\
#           $(VMM_LIB)					\
#           $(XQP_LIB)					\
#           $(EXECUTOR_LIB)				\
#           $(PREPARE_QUERY_LIB)			\
#           $(COMMON_FILES_LIB)

#TR_TEST_OBJS = tr_test$(OBJ_EXT)        \
#               register_tr$(OBJ_EXT)    \
#               tr_functions$(OBJ_EXT)   \
#               tr_globals$(OBJ_EXT)     \
#               $(CRMUTILS_LIB)			\
#               $(LOCKS_LIB)					\
#               $(MO_LIB)					\
#               $(PSTR_LIB)					\
#               $(UPDATES_LIB)				\
#               $(NID_LIB)					\
#               $(VMM_LIB)					\
#               $(XQP_LIB)					\
#               $(EXECUTOR_LIB)				\
#               $(PREPARE_QUERY_LIB)			\
#               $(COMMON_FILES_LIB)

ifeq ($(ENABLE_DTSEARCH), 1)
E_LFLAGS = $(LFLAGS_DTS)
else
E_LFLAGS = $(LFLAGS)
endif


ifeq ("$(PLATFORM)", "WIN32")
$(PP)/bin/se_trn$(EXE_EXT): $(TRN_OBJS) libraries
	$(LD) $(E_LFLAGS) $(LDOUT)$@ $(TRN_OBJS)					\
                               $(EXECUTOR_S)				\
                               $(COMMON_FILES_S)			\
                               $(CRMUTILS_S)				\
                               $(IDX_S)						\
                               $(FT_S)						\
                               $(TRIGGERS_S)						\
                               $(BTREE_S)					\
                               $(LOCKS_S)					\
                               $(HL_PHYS_LOG_S)				\
                               $(HL_LOGICAL_LOG_S)			\
                               $(MO_S)						\
                               $(NID_S)						\
                               $(PQ_S)						\
                               $(PSTR_S)					\
                               $(SQP_S)						\
                               $(STRINGS_S)					\
                               $(STRUCTURES_S)				\
                               $(UPDATES_S)					\
                               $(VMM_S)						\
                               $(XQP_S)						\
                               $(PHYS_LOG_S)				\
                               $(LOGICAL_LOG_S)				\
                               $(AUTH_S)					\
                               $(RCV_S)
else
$(PP)/bin/se_trn$(EXE_EXT): $(TRN_OBJS) libraries
	$(LD) $(LFLAGS) 										\
                               $(EXECUTOR_P)				\
                               $(COMMON_FILES_P)			\
                               $(CRMUTILS_P)				\
                               $(IDX_P)						\
                               $(FT_P)						\
                               $(TRIGGERS_P)						\
                               $(BTREE_P)					\
                               $(LOCKS_P)					\
                               $(HL_PHYS_LOG_P)				\
                               $(HL_LOGICAL_LOG_P)			\
                               $(MO_P)						\
                               $(NID_P)						\
                               $(PQ_P)						\
                               $(PSTR_P)					\
                               $(SQP_P)						\
                               $(STRINGS_P)					\
                               $(STRUCTURES_P)				\
                               $(UPDATES_P)					\
                               $(VMM_P)						\
                               $(XQP_P)						\
                               $(PHYS_LOG_P)				\
                               $(LOGICAL_LOG_P)				\
                               $(AUTH_P)					\
                               $(RCV_P)						\
                    $(LDOUT)$@ $(TRN_OBJS)					\
                --start-group  $(EXECUTOR_S)				\
                               $(COMMON_FILES_S)			\
                               $(CRMUTILS_S)				\
                               $(IDX_S)						\
                               $(FT_S)						\
                               $(TRIGGERS_S)						\
                               $(BTREE_S)					\
                               $(LOCKS_S)					\
                               $(HL_PHYS_LOG_S)				\
                               $(HL_LOGICAL_LOG_S)			\
                               $(MO_S)						\
                               $(NID_S)						\
                               $(PQ_S)						\
                               $(PSTR_S)					\
                               $(SQP_S)						\
                               $(STRINGS_S)					\
                               $(STRUCTURES_S)				\
                               $(UPDATES_S)					\
                               $(VMM_S)						\
                               $(XQP_S)						\
                               $(PHYS_LOG_S)				\
                               $(LOGICAL_LOG_S)				\
                               $(AUTH_S)					\
                               $(RCV_S)						\
                               $(EXECUTOR_S)				\
                               $(COMMON_FILES_S) --end-group\
                               $(LSLIBS)
endif



ifeq ("$(PLATFORM)", "WIN32")
$(PP)/bin/se_rcv$(EXE_EXT): $(TRN_RCV_OBJS) libraries
	$(LD) $(E_LFLAGS) $(LDOUT)$@ $(TRN_RCV_OBJS)				\
                               $(EXECUTOR_S)				\
                               $(LOCKS_S)					\
                               $(COMMON_FILES_S)			\
                               $(CRMUTILS_S)				\
                               $(IDX_S)						\
                               $(FT_S)						\
                               $(PQ_S)						\
                               $(SQP_S)						\
                               $(XQP_S)						\
                               $(TRIGGERS_S)						\
                               $(BTREE_S)					\
                               $(HL_PHYS_LOG_S)				\
                               $(HL_LOGICAL_LOG_S)			\
                               $(MO_S)						\
                               $(NID_S)						\
                               $(PSTR_S)					\
                               $(STRUCTURES_S)				\
                               $(STRINGS_S)					\
                               $(UPDATES_S)					\
                               $(VMM_S)						\
                               $(PHYS_LOG_S)				\
                               $(LOGICAL_LOG_S)				\
                               $(AUTH_S)					\
                               $(RCV_S)	
else					
$(PP)/bin/se_rcv$(EXE_EXT): $(TRN_RCV_OBJS) libraries
	$(LD) $(LFLAGS)        					\
                               $(EXECUTOR_P)				\
                               $(LOCKS_P)					\
                               $(COMMON_FILES_P)			\
                               $(CRMUTILS_P)				\
                               $(IDX_P)						\
                               $(FT_P)						\
                               $(TRIGGERS_P)						\
                               $(BTREE_P)					\
                               $(HL_PHYS_LOG_P)				\
                               $(HL_LOGICAL_LOG_P)			\
                               $(MO_P)						\
                               $(NID_P)						\
                               $(PSTR_P)					\
                               $(STRUCTURES_P)				\
                               $(STRINGS_P)					\
                               $(UPDATES_P)					\
                               $(VMM_P)						\
                               $(PHYS_LOG_P)				\
                               $(LOGICAL_LOG_P)				\
                               $(AUTH_P)					\
                               $(RCV_P)						\
                               $(EXECUTOR_P)					\
                               $(MO_P)						\
                               $(CRMUTILS_P)					\
                    $(LDOUT)$@ $(TRN_RCV_OBJS)				\
                --start-group  $(EXECUTOR_S)				\
                               $(LOCKS_S)					\
                               $(COMMON_FILES_S)			\
                               $(CRMUTILS_S)				\
                               $(IDX_S)						\
                               $(FT_S)						\
                               $(TRIGGERS_S)						\
                               $(BTREE_S)					\
                               $(HL_PHYS_LOG_S)				\
                               $(HL_LOGICAL_LOG_S)			\
                               $(MO_S)						\
                               $(NID_S)						\
                               $(PSTR_S)					\
                               $(STRUCTURES_S)				\
                               $(STRINGS_S)					\
                               $(UPDATES_S)					\
                               $(VMM_S)						\
                               $(PHYS_LOG_S)				\
                               $(LOGICAL_LOG_S)				\
                               $(AUTH_S)					\
                               $(RCV_S)						\
                               $(EXECUTOR_S)				\
                               $(CRMUTILS_S)					\
                               $(STRUCTURES_S)					\
                               $(MO_S)						\
                               $(UPDATES_S)					\
                               $(COMMON_FILES_S) --end-group\
                               $(LSLIBS)
endif

#tr_test$(EXE_EXT): $(TR_TEST_OBJS) libraries 
#	$(LD) $(LFLAGS) $(LDOUT)$@ $(TR_TEST_OBJS)

#bl$(EXE_EXT): $(BL_OBJS) libraries
#	$(LD) $(LFLAGS) $(LDOUT)$@ $(BL_OBJS)

libraries:
	$(MAKE) -C crmutils
	$(MAKE) -C executor
	$(MAKE) -C idx
ifeq ($(ENABLE_DTSEARCH), 1)
	$(MAKE) -C ft
endif
	$(MAKE) -C locks
	$(MAKE) -C log
	$(MAKE) -C mo
	$(MAKE) -C nid
	$(MAKE) -C xqp
	$(MAKE) -C pq
	$(MAKE) -C pstr
	$(MAKE) -C sqp
	$(MAKE) -C strings
	$(MAKE) -C structures
	$(MAKE) -C updates
	$(MAKE) -C vmm
	$(MAKE) -C auth
	$(MAKE) -C rcv
ifeq ($(ENABLE_TRIGGERS), 1)
	$(MAKE) -C triggers
endif

################################################################################
# Clean                                                                        #
################################################################################
clean: generic_clean
	$(MAKE) -C crmutils clean
	$(MAKE) -C executor clean
	$(MAKE) -C idx clean
ifeq ($(ENABLE_DTSEARCH), 1)
	$(MAKE) -C ft clean
endif
ifeq ($(ENABLE_TRIGGERS), 1)
	$(MAKE) -C triggers clean
endif
	$(MAKE) -C locks clean
	$(MAKE) -C log clean
	$(MAKE) -C mo clean
	$(MAKE) -C nid clean
	$(MAKE) -C pq clean
	$(MAKE) -C pstr clean
	$(MAKE) -C sqp clean
	$(MAKE) -C strings clean
	$(MAKE) -C structures clean
	$(MAKE) -C updates clean
	$(MAKE) -C vmm clean
	$(MAKE) -C xqp clean
	$(MAKE) -C auth clean
	$(MAKE) -C rcv clean
	-$(REMOVE) *$(OBJ_EXT)
	-$(REMOVE) depend
	-$(REMOVE) depend.errors
	-$(REMOVE) STACKTRACE

