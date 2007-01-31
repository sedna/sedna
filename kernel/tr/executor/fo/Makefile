#
# Makefile for fo (GNU make)
#

PP = ../../../..

include $(PP)/Makefile.include

all: fo$(LIB_EXT) 
	@echo ===================================================================
	@echo fo has been Done
	@echo ===================================================================

OBJS = boolean_operations$(OBJ_EXT) casting_operations$(OBJ_EXT) \
       comparison_operations$(OBJ_EXT) dateTime_operations$(OBJ_EXT) \
       op_map$(OBJ_EXT) other_fo$(OBJ_EXT) string_operations$(OBJ_EXT) \
       numeric_operations$(OBJ_EXT)

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include generated
include $(OBJS:$(OBJ_EXT)=.d)

$(OBJS:$(OBJ_EXT)=.d): generated
endif

################################################################################
# fo$(LIB_EXT)                                                                 #
################################################################################
fo$(LIB_EXT): $(OBJS)
	$(PLD) $(PLDOUT)$@ $^

################################################################################
# generated                                                                    #
################################################################################
numeric_operations.cpp numeric_operations.h: generated

generated: gen_num_ops$(EXE_EXT)
	./gen_num_ops$(EXE_EXT)
	touch generated

gen_num_ops$(OBJ_EXT): gen_num_ops.c
	$(CC) $(CFLAGS_CHICKEN_EXE) $<

gen_num_ops$(EXE_EXT): gen_num_ops$(OBJ_EXT)
	$(LD) $(LFLAGS_CHICKEN_EXE) $(LDOUT)$@ $^ $(LSLIBS_CHICKEN_EXE)


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	-$(REMOVE) numeric_operations.cpp 
	-$(REMOVE) numeric_operations.h
	-$(REMOVE) gen_num_ops.c
	-$(REMOVE) gen_num_ops$(EXE_EXT)
