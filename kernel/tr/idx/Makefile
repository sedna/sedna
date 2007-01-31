#
# Makefile for idx
#

PP = ../../..

include $(PP)/Makefile.include

all: idx$(LIB_EXT)
	@echo ===================================================================
	@echo idx DONE
	@echo ===================================================================

OBJS      = index_data$(OBJ_EXT) indexes$(OBJ_EXT)
SUBDIRS   = btree
SUBDIROBJS= $(foreach dir,$(SUBDIRS),$(dir)/$(dir)$(LIB_EXT))

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# idx                                                                 #
################################################################################
idx$(LIB_EXT): $(OBJS) $(SUBDIROBJS)
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

