
PP=..
include $(PP)/Makefile.platform
include $(PP)/Makefile.include

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
BIN_LIBS=bin/$(SUB_PLATFORM)
else
BIN_LIBS=bin/$(PLATFORM)
endif

build_chicken=yes
build_expat=yes
build_pcre=yes
build_libedit=yes

.PHONY: default build clean

default: build

#####################################################################
###                          Chicken
#####################################################################

ifeq ("$(build_chicken)", "yes")

.PHONY: build_chicken clean_chicken
BUILD_DEPS += build_chicken
CLEAN_DEPS += clean_chicken

BIN_CHICKEN= $(shell if test -d $(BIN_LIBS)/chicken; then echo 1; else echo 0; fi)

ifeq ("$(BIN_CHICKEN)", "1")

chicken/chicken.h: $(BIN_LIBS)/chicken
	cp -pR $(BIN_LIBS)/chicken .
	touch chicken/chicken.h
	-chmod -f a+x chicken/chicken

build_chicken: chicken/chicken.h
	@echo ===================================================================
	@echo "Chicken (binaries) has been done"
	@echo ===================================================================

clean_chicken:
	-rm -rf chicken

else # Binaries are not available

CHICKEN_VERSION=2.2

chicken/chicken.h: chicken-$(CHICKEN_VERSION).tar.gz
	gzip -dc chicken-$(CHICKEN_VERSION).tar.gz | tar xvf -
	touch chicken-$(CHICKEN_VERSION)/chicken.h
	rm -rf chicken
	mv chicken-$(CHICKEN_VERSION) chicken
	patch -d chicken < chicken_panic_hook.diff

ifeq ("$(PLATFORM)", "WIN32")

build_chicken: chicken/chicken.h
	cd chicken; \
	cat tcp.c | sed -e 's/\bsocklen_t\b/int/g' > tcp.c.new; \
        mv tcp.c.new tcp.c; \
	cat utcp.c | sed -e 's/\bsocklen_t\b/int/g' > utcp.c.new; \
        mv utcp.c.new utcp.c; \
	MAKEFLAGS=; \
	nmake /f ../chicken.mak
	@echo ===================================================================
	@echo Chicken has been done
	@echo ===================================================================

clean_chicken:
	-cd chicken; MAKEFLAGS=; nmake /f ../chicken.mak clean
	-rm -rf chicken

else # Unix

chicken/configure: chicken/chicken.h
	touch chicken/configure

chicken/Makefile: chicken/configure
	CFLAGS='-fexceptions'; \
	LDFLAGS='-fexceptions'; \
	CPPFLAGS='-fexceptions'; \
	CXXFLAGS='-fexceptions'; \
	export CFLAGS LDFLAGS CPPFLAGS CXXFLAGS; \
	cd chicken; \
	./configure --disable-shared

build_chicken: chicken/Makefile
	$(MAKE) -C chicken
	cp chicken/.libs/*.a chicken
	@echo ===================================================================
	@echo Chicken has been done
	@echo ===================================================================

clean_chicken:
	-$(MAKE) -C chicken clean
	-rm -rf chicken
endif
endif
endif

#####################################################################
###                            Expat
#####################################################################

ifeq ("$(build_expat)", "yes")

.PHONY: build_expat clean_expat
BUILD_DEPS += build_expat
CLEAN_DEPS += clean_expat

BIN_EXPAT= $(shell if test -d $(BIN_LIBS)/expat; then echo 1; else echo 0; fi)

ifeq ("$(BIN_EXPAT)", "1")

expat/expat.h: $(BIN_LIBS)/expat
	cp -pR $(BIN_LIBS)/expat .
	touch expat/expat.h
	@echo ===================================================================
	@echo "Expat (binaries) has been done"
	@echo ===================================================================

build_expat: expat/expat.h

clean_expat:
	-rm -rf expat

else # Binaries are not available

EXPAT_VERSION=1.95.8

expat/lib/expat.h: expat-$(EXPAT_VERSION).tar.gz
	-rm -rf expat
	gzip -dc expat-$(EXPAT_VERSION).tar.gz | tar xvf -
	mv expat-$(EXPAT_VERSION) expat
	touch expat/lib/expat.h

ifeq ("$(PLATFORM)", "WIN32")

build_expat: expat/lib/expat.h
	cd expat/lib; \
	MAKEFLAGS=; \
	nmake /f ../../expat_static.mak
	cp -p expat/lib/libexpatMT.lib expat/libexpatMT.lib
	cp -p expat/lib/expat.h expat/lib/expat_external.h expat
	@echo ===================================================================
	@echo Expat has been done
	@echo ===================================================================

clean_expat:
	cd expat/lib; \
	MAKEFLAGS=; \
	nmake /f ../../expat_static.mak clean

else # Binaries are not available

expat/configure: expat/lib/expat.h
	touch expat/configure

expat/Makefile: expat/configure
	cd expat; \
	./configure --enable-shared=no

build_expat: expat/Makefile
	$(MAKE) -C expat
	cp -p expat/.libs/*.a expat
	cp -p expat/lib/expat.h expat/lib/expat_external.h expat
	@echo ===================================================================
	@echo Expat has been done
	@echo ===================================================================

clean_expat:
	-$(MAKE) -C expat clean
	-rm -rf expat

endif
endif
endif

#####################################################################
###                            PCRE
#####################################################################

ifeq ("$(build_pcre)", "yes")

.PHONY: build_pcre clean_pcre
BUILD_DEPS += build_pcre
CLEAN_DEPS += clean_pcre

pcre/Makefile: src/pcre
	cp -pR src/pcre .
	touch pcre/Makefile

build_pcre: pcre/Makefile
	$(MAKE) -C pcre
	@echo ===================================================================
	@echo PCRE has been done
	@echo ===================================================================

clean_pcre: pcre/Makefile
	-$(MAKE) -C pcre clean

endif

#####################################################################
###                            Libedit
#####################################################################

ifeq ("$(build_libedit)","yes")
ifeq ("$(ENABLE_LIBEDIT)","1")

.PHONY: build_libedit clean_libedit
BUILD_DEPS+=build_libedit
CLEAN_DEPS+=clean_libedit

build_libedit: libedit/lib/libedit.a
	@echo ===================================================================
	@echo Libedit has been done
	@echo ===================================================================

libedit/lib/libedit.a: libedit/Makefile
	-$(MAKE) -C libedit
	-$(MAKE) -C libedit install

libedit/Makefile: libedit/configure
	cd libedit ; ./configure --enable-static --prefix=`pwd`

libedit/configure:
	gzip -dc libedit-*.tar.gz | tar xvf -
	mv `find libedit-* -type d | head -n 1` libedit

clean_libedit:
	-$(MAKE) -C libedit uninstall
	-$(MAKE) -C libedit clean
	-rm -rf libedit

endif
endif

#####################################################################
###                         General targets
#####################################################################

build: $(BUILD_DEPS)

ifeq ("$(CLEANUP_LIBRARIES)","1")
clean: $(CLEAN_DEPS)
else
clean:
endif


