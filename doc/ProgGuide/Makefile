PP = ../..


include $(PP)/Makefile.include

all: ProgGuide.pdf ClientServerProtocol.pdf
	@echo ===================================================================
	@echo ProgGuide has been done
	@echo ===================================================================


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
