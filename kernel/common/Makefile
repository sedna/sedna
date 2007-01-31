#
# Makefile for common_files
#

PP = ../..

include $(PP)/Makefile.include

all: common_files$(LIB_EXT)
	@echo ===================================================================
	@echo COMMON_FILES DONE
	@echo ===================================================================

OBJS = argtable$(OBJ_EXT) base$(OBJ_EXT) gmm$(OBJ_EXT) ipc_ops$(OBJ_EXT) \
       pping$(OBJ_EXT) sedna$(OBJ_EXT) sp$(OBJ_EXT) SSMMsg$(OBJ_EXT) \
       tr_debug$(OBJ_EXT) ugc$(OBJ_EXT) utils$(OBJ_EXT) version$(OBJ_EXT) \
       xptr$(OBJ_EXT)
SUBDIRS = errdbg mmgr ph u
SUBDIROBJS = $(foreach dir,$(SUBDIRS),$(dir)/$(dir)$(LIB_EXT))


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)

$(OBJS:$(OBJ_EXT)=.d): errdbg/errdbg$(LIB_EXT)
endif


################################################################################
# common_files                                                                 #
################################################################################
common_files$(LIB_EXT): $(OBJS) $(SUBDIROBJS)
	$(LB) $(LIBFLAGS) $(LIBOUT)$@ $^

$(SUBDIROBJS): $(SUBDIRS:%=%-recursive) ;

.PHONY: $(SUBDIRS:%=%-recursive)
$(SUBDIRS:%=%-recursive):
	$(MAKE) -C $(subst -recursive,,$@)


################################################################################
# Clean                                                                        #
################################################################################
.PHONY: clean

clean: generic_clean
	for dir in $(SUBDIRS); do $(MAKE) -C $$dir $@ || exit; done
