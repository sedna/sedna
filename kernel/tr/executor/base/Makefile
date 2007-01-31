#
# Makefile for base
#

PP = ../../../..

include $(PP)/Makefile.include

all: base$(LIB_EXT)
	@echo ===================================================================
	@echo base DONE
	@echo ===================================================================

OBJS      = bit_set$(OBJ_EXT) compare$(OBJ_EXT) dm_accessors$(OBJ_EXT) \
            dynamic_context$(OBJ_EXT) ext_sort$(OBJ_EXT) merge$(OBJ_EXT) \
            PPBase$(OBJ_EXT) PPUtils$(OBJ_EXT) sequence$(OBJ_EXT) \
            SequenceType$(OBJ_EXT) sorted_sequence$(OBJ_EXT) tuple$(OBJ_EXT) \
            XMLDateTime$(OBJ_EXT) XPath$(OBJ_EXT) XPathOnSchema$(OBJ_EXT) \
            xptr_sequence$(OBJ_EXT) xptrChanneledMerge$(OBJ_EXT) \
            xs_binary$(OBJ_EXT) xs_decimal_t$(OBJ_EXT) \
            xs_fp_converter$(OBJ_EXT) xs_helper$(OBJ_EXT) xs_names$(OBJ_EXT) \
            xs_uri$(OBJ_EXT) xsd$(OBJ_EXT)
SUBDIRS   = decNumber lip
SUBDIROBJS= $(foreach dir,$(SUBDIRS),$(dir)/$(dir)$(LIB_EXT))

ifneq ($(findstring clean, $(MAKECMDGOALS)), clean)
include $(OBJS:$(OBJ_EXT)=.d)
endif

################################################################################
# base                                                                 #
################################################################################
base$(LIB_EXT): $(OBJS) $(SUBDIROBJS)
	$(PLD) $(PLDOUT)$@ $^


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

