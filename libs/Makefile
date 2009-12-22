
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

build_expat=yes
build_pcre=yes
build_libedit=yes

.PHONY: default build clean

default: build

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
	$(PATCHER) -Np1 -d libedit < 01-libedit-dups.patch
	$(PATCHER) -Np1 -d libedit < 02-libedit-space.patch

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


