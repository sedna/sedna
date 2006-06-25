PP = ../..


include $(PP)/Makefile.include


all: clean_dvi AdminGuide.pdf
	@echo ===================================================================
	@echo AdminGuide has been done
	@echo ===================================================================

#clean_dvi is required to make pdf and html publishing working correct together
clean_dvi: 
	-$(REMOVE) *.dvi

clean:
	-$(REMOVE) *.aux
	-$(REMOVE) *.dvi
	-$(REMOVE) *.log
	-$(REMOVE) *.toc
	-$(REMOVE) *.ps
	-$(REMOVE) *.pdf
