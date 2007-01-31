#
# Makefile for sqp
#

PP = ../../..

include $(PP)/Makefile.include

all: sqp$(LIB_EXT)
	@echo ===================================================================
	@echo sqp DONE
	@echo ===================================================================

OBJS = common-lib$(OBJ_EXT) dschema$(OBJ_EXT) dschema-utils$(OBJ_EXT) \
       xquery-lr-lib$(OBJ_EXT)
SUBDIRS = lr2por lropt popt sa tools
SUBDIROBJS = $(foreach dir,$(SUBDIRS),$(dir)/$(dir)$(LIB_EXT))


ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif


################################################################################
# sqp                                                                          #
################################################################################
sqp$(LIB_EXT): $(OBJS) $(SUBDIROBJS)
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
	-$(REMOVE) common-lib.c
	-$(REMOVE) dschema.c
	-$(REMOVE) dschema-utils.c
	-$(REMOVE) xquery-lr-lib.c
