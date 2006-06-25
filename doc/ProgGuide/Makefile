PP = ../..


include $(PP)/Makefile.include

all: clean_dvi ProgGuide.pdf ClientServerProtocol.pdf
	@echo ===================================================================
	@echo ProgGuide has been done
	@echo ===================================================================

#clean_dvi is required to make pdf and html publishing working correct together
clean_dvi: 
	-$(REMOVE) *.dvi


ClientServerProtocol.pdf:
	$(MAKE) -C ClientServerProtocol


clean:
	-$(REMOVE) *.aux
	-$(REMOVE) *.dvi
	-$(REMOVE) *.log
	-$(REMOVE) *.toc
	-$(REMOVE) *.ps
	-$(REMOVE) *.pdf
	$(MAKE) -C ClientServerProtocol clean
