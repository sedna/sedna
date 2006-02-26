
#SEDNA_VERSION :=$(shell cat ../ver)
#REMOVE = rm -f

all: guides
	@echo ===================================================================
	@echo Building Documentation
	@echo ===================================================================
	$(MAKE) -C AdminGuide
	$(MAKE) -C ProgGuide
	$(MAKE) -C QuickStart
	@echo ===================================================================
	@echo Documentation has been done
	@echo ===================================================================

#readme.txt: readme.txt.m4
#	m4 -DSEDNA_VERSION="$(SEDNA_VERSION)" $^ >$@

guides:
	$(MAKE) -C AdminGuide
	$(MAKE) -C ProgGuide
	$(MAKE) -C QuickStart


clean:
	$(MAKE) -C AdminGuide clean
	$(MAKE) -C ProgGuide clean
	$(MAKE) -C QuickStart clean

#	-$(REMOVE) readme.txt
