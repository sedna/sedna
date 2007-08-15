#
# Makefile for fo (GNU make)
#

PP = ../../../..

include $(PP)/Makefile.include

OBJS = boolean_operations$(OBJ_EXT) casting_operations$(OBJ_EXT) \
       comparison_operations$(OBJ_EXT) dateTime_operations$(OBJ_EXT) \
       op_map$(OBJ_EXT) other_fo$(OBJ_EXT) string_operations$(OBJ_EXT) \
       numeric_operations$(OBJ_EXT)

GENERATED = numeric_operations.cpp numeric_operations.h
CUSTOM_CLEAN = clean_generated

include $(PP)/Makefile.pseudolib 


################################################################################
# generated                                                                    #
################################################################################
generated: gen_num_ops$(EXE_EXT)
	./gen_num_ops$(EXE_EXT)
	touch generated

gen_num_ops$(EXE_EXT): gen_num_ops$(OBJ_EXT)
	$(LD) $(LFLAGS_CHICKEN_EXE) $(LDOUT)$@ $^ $(LSLIBS_CHICKEN_EXE)

gen_num_ops$(OBJ_EXT): gen_num_ops.c
	$(CC) $(CFLAGS_CHICKEN_EXE) $<

gen_num_ops.c: gen_num_ops.scm
	$(CHICKEN) $< -output-file $@ $(CHICKEN_OPTIONS)	

################################################################################
# clean_generated                                                              #
################################################################################
clean_generated:
	-$(REMOVE) numeric_operations.cpp numeric_operations.h gen_num_ops.c gen_num_ops$(EXE_EXT)






