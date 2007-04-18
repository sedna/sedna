
PP=..
include $(PP)/Makefile.platform


ifeq ("$(OS)", "CYGWIN_NT")
# Win32
CC=cl /c
LB = lib
LIBFLAGS = /NOLOGO
LIBOUT = /OUT:
LIB_EXT = .lib
else
# UNIX
CC=gcc
LB = ar
LIBFLAGS = rcs
LIBOUT = 
OBJ_EXT = .o
LIB_EXT = .a
endif


ifdef SUB_PLATFORM

BIN_LIBS=bin/$(PLATFORM)_$(SUB_PLATFORM)

else

BIN_LIBS=bin/$(PLATFORM)

endif

build_chicken=yes
build_pccts=yes
build_expat=yes
build_pcre=yes

.PHONY: default build clean


default: build


ifeq ("$(build_chicken)", "yes")

.PHONY: build_chicken clean_chicken
BUILD_DEPS += build_chicken
CLEAN_DEPS += clean_chicken

BIN_CHICKEN= $(shell if test -d $(BIN_LIBS)/chicken; then echo 1; else echo 0; fi)

ifeq ("$(BIN_CHICKEN)", "1")

chicken/chicken.h: $(BIN_LIBS)/chicken
	cp -pR $(BIN_LIBS)/chicken .
	touch chicken/chicken.h
	-chmod a+x chicken/chicken


build_chicken: chicken/chicken.h

else

CHICKEN_VERSION=2.2

chicken/chicken.h: chicken-$(CHICKEN_VERSION).tar.gz
	gzip -dc chicken-$(CHICKEN_VERSION).tar.gz | tar xvf -
	touch chicken-$(CHICKEN_VERSION)/chicken.h
	rm -rf chicken
	mv chicken-$(CHICKEN_VERSION) chicken
	patch --directory=chicken <chicken_panic_hook.diff

ifeq ("$(PLATFORM)", "WIN32")

build_chicken: chicken/chicken.h
	cd chicken; \
	cat tcp.c | sed -e 's/\bsocklen_t\b/int/g' > tcp.c.new; \
        mv tcp.c.new tcp.c; \
	cat utcp.c | sed -e 's/\bsocklen_t\b/int/g' > utcp.c.new; \
        mv utcp.c.new utcp.c; \
	MAKEFLAGS=; \
	nmake /f ../chicken.mak

clean_chicken:
	cd chicken; \
	MAKEFLAGS=; \
	nmake /f ../chicken.mak clean

else

chicken/configure: chicken/chicken.h

chicken/Makefile: chicken/configure
	export CFLAGS='$(CFLAGS) -fexceptions'; \
	export LDFLAGS='$(LDFLAGS) -fexceptions'; \
	export CPPFLAGS='$(CPPFLAGS) -fexceptions'; \
	export CXXFLAGS='$(CXXFLAGS) -fexceptions'; \
	cd chicken; \
	./configure --disable-shared

build_chicken: chicken/Makefile
	$(MAKE) -C chicken
	cp chicken/.libs/*.a chicken

clean_chicken:
	-rm -f chicken/*.a
	-$(MAKE) -C chicken clean

endif

endif

endif


ifeq ("$(build_pccts)", "yes")

.PHONY: build_pccts clean_pccts
BUILD_DEPS += build_pccts
CLEAN_DEPS += clean_pccts

BIN_PCCTS= $(shell if test -d $(BIN_LIBS)/pg; then echo 1; else echo 0; fi)

ifeq ("$(BIN_PCCTS)", "1")

pg/COPYRIGHT: $(BIN_LIBS)/pg
	cp -pR $(BIN_LIBS)/pg .
	touch pg/COPYRIGHT
	-chmod a+x pg/antlr pg/dlg pg/sor

build_pccts: pg/COPYRIGHT

else

pccts: pccts133mr.tar.gz
	gzip -dc pccts133mr.tar.gz | tar xvf -

ifeq ("$(PLATFORM)", "WIN32")

build_pccts: pccts
	-mkdir pg
	-mkdir pg/h
	$(MAKE) -C pccts/antlr -f ../../Makefile.antlr COPT="-DPCCTS_USE_STDARG"
	$(MAKE) -C pccts/dlg -f ../../Makefile.dlg COPT="-DPCCTS_USE_STDARG"
	$(MAKE) -C pccts/sorcerer -f ../../Makefile.sor COPT="-DPCCTS_USE_STDARG"
	cp pccts/bin/Antlr.exe pccts/bin/Dlg.exe pccts/bin/Sorcerer.exe pg
	cp pccts/h/*.h pg/h
	cp pccts/sorcerer/h/*.h pg/h
	cd pccts/h; $(CC) /MT -DPCCTS_USE_STDARG -c *.cpp
	$(MAKE) -C pccts/sorcerer/lib -f ../../../Makefile.sor-lib COPT="-DPCCTS_USE_STDARG"
	$(LB) $(LIBFLAGS) $(LIBOUT)pg/pccts_release$(LIB_EXT) pccts/h/*$(OBJ_EXT) pccts/sorcerer/lib/STreeParser$(OBJ_EXT)

clean_pccts:
	$(MAKE) -C pccts/antlr -f ../../Makefile.antlr clean
	$(MAKE) -C pccts/dlg -f ../../Makefile.dlg clean
	$(MAKE) -C pccts/sorcerer -f ../../Makefile.sor clean
	$(MAKE) -C pccts/sorcerer/lib -f ../../../Makefile.sor-lib clean

else

build_pccts: pccts
	$(MAKE) -C pccts COPT="-DPCCTS_USE_STDARG -O2" CC=$(CC)
	-mkdir pg
	cp pccts/bin/antlr pccts/bin/dlg pccts/bin/sor pg
	-mkdir pg/h
	cp pccts/h/*.h pg/h
	cp pccts/sorcerer/h/*.h pg/h
	cd pccts/h; $(CC) -DPCCTS_USE_STDARG -O2 -c *.cpp
	cd pccts/sorcerer/lib; $(CC) -DPCCTS_USE_STDARG -O2 -I../h -I../../h -c STreeParser.cpp
	$(LB) $(LIBFLAGS) $(LIBOUT)pg/libpccts$(LIB_EXT) pccts/h/*.o pccts/sorcerer/lib/STreeParser.o

clean_pccts:
	-rm -f pccts/h/*.o
	-$(MAKE) -C pccts clean
	-rm -rf pg

	
endif

endif

endif

ifeq ("$(build_expat)", "yes")

.PHONY: build_expat clean_expat
BUILD_DEPS += build_expat
CLEAN_DEPS += clean_expat

BIN_EXPAT= $(shell if test -d $(BIN_LIBS)/expat; then echo 1; else echo 0; fi)

ifeq ("$(BIN_EXPAT)", "1")

expat/expat.h: $(BIN_LIBS)/expat
	cp -pR $(BIN_LIBS)/expat .
	touch expat/expat.h

build_expat: expat/expat.h

else

EXPAT_VERSION=1.95.8

expat/expat.h: expat-$(EXPAT_VERSION).tar.gz
	-rm -rf expat
	gzip -dc expat-$(EXPAT_VERSION).tar.gz | tar xvf -
	mv expat-$(EXPAT_VERSION) expat


ifeq ("$(PLATFORM)", "WIN32")

build_expat: expat/expat.h
	cd expat/lib; \
	MAKEFLAGS=; \
	nmake /f ../../expat_static.mak
	cp expat/lib/libexpatMT.lib expat/libexpatMT.lib
	cp expat/lib/expat.h expat/lib/expat_external.h expat

clean_expat:
	cd expat/lib; \
	MAKEFLAGS=; \
	nmake /f ../../expat_static.mak clean

else
	
expat/configure: expat/expat.h

expat/Makefile: expat/configure
	cd expat && \
	./configure --enable-shared=no

build_expat: expat/Makefile
	$(MAKE) -C expat
	cp expat/.libs/*.a expat
	cp expat/lib/expat.h expat/lib/expat_external.h expat

clean_expat:
	-rm -f expat/*.a
	-$(MAKE) -C expat clean

	
endif

endif

endif


ifeq ("$(build_pcre)", "yes")

.PHONY: build_pcre clean_pcre
BUILD_DEPS += build_pcre
CLEAN_DEPS += clean_pcre

BIN_PCRE= $(shell if test -d $(BIN_LIBS)/pcre; then echo 1; else echo 0; fi)

ifeq ("$(BIN_PCRE)", "1")

pcre/libpcre$(LIB_EXT): $(BIN_LIBS)/pcre
	cp -pR $(BIN_LIBS)/pcre .
	touch pcre/libpcre$(LIB_EXT)

build_pcre: pcre/libpcre$(LIB_EXT)

else

pcre/Makefile: src/pcre
	cp -pR src/pcre .
	touch pcre/Makefile

ifeq ("$(PLATFORM)", "WIN32")

build_pcre: pcre/Makefile
	-$(MAKE) -C pcre

clean_pcre: pcre/Makefile
	-$(MAKE) -C pcre clean

else

build_pcre: pcre/Makefile
	-$(MAKE) -C pcre

clean_pcre: pcre/Makefile
	-$(MAKE) -C pcre clean

endif

endif

endif


.PHONY: build_libedit clean_libedit
BUILD_DEPS+=build_libedit
CLEAN_DEPS+=clean_libedit

build_libedit: libedit libedit/Makefile
	-$(MAKE) -C libedit
	-$(MAKE) -C libedit install

libedit:
	tar -zxf libedit-*.tar.gz
	mv `find -name 'libedit-*' -type d | head -n 1` libedit

libedit/Makefile:
	cd libedit ; ./configure --enable-static --prefix=`pwd`


build: $(BUILD_DEPS)

clean: $(CLEAN_DEPS)

