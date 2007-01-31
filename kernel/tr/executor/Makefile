#
# Makefile for XQuery Executor
#

PP = ../../..

include $(PP)/Makefile.include

all: executor$(LIB_EXT)
	@echo ===================================================================
	@echo EXECUTOR DONE
	@echo ===================================================================

SUBDIRS   = base fo por2qep root xqops
SUBDIROBJS= $(foreach dir,$(SUBDIRS),$(dir)/$(dir)$(LIB_EXT))


################################################################################
# EXECUTOR                                                                     #
################################################################################
executor$(LIB_EXT): $(SUBDIROBJS)
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
