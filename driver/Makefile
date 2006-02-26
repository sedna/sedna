#
# Makefile for Sedna drivers (GNU make)
#

PP = ..

include $(PP)/Makefile.include

	
all: 
ifeq ($(JAVA_DRIVER),1)
	$(MAKE) -C java
endif
	$(MAKE) -C c depend
	$(MAKE) -C c

clean:
ifeq ($(JAVA_DRIVER),1)
	$(MAKE) -C java clean
endif
	$(MAKE) -C c clean