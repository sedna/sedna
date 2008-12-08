
# Makefile for Sedna (GNU make)
#

PP = .
include $(PP)/Makefile.include

# This will not work under Solaris. Solution is needed if you want to use EFF_UID! 
#EFF_UID := $(shell id -u)


ifeq ($(PLATFORM), UNIX)
BUILD_FILE := build-linux-$(SEDNA_VERSION)
BUILD_SUFFIX := linux
UFTP_ARGS := ncftp <
DISTR_EXT := sh
SRC_EXT := tar.gz
# for executables that require root privileges
#PERM1 := -oroot -groot -m4771
# for ordinary executable files
#PERM2 := -oroot -groot -m0775
# for ordinary files
#PERM3 := -oroot -groot -m0664
# for directory
PERM4 := -m0755
PERM1 := -m0755
PERM2 := -m0755
PERM3 := -m0664
else
BUILD_FILE := build-$(SEDNA_VERSION)
BUILD_SUFFIX := win
UFTP_ARGS := ftp -s:
DISTR_EXT := tar.gz
SRC_EXT := tar.gz
PERM1 := 
PERM2 := 
PERM3 := 
PERM4 := 
endif

# 'install' has not preserve option under Solaris
ifeq ("$(SUB_PLATFORM)","SunOS") 
PRESERVE := 
else
PRESERVE := -p
endif

build:
	@echo ===================================================================
	@echo Preparing libs
	@echo ===================================================================
	$(MAKE) -C libs
	@echo ===================================================================
	@echo Building Kernel
	@echo ===================================================================
	$(MAKE) -C kernel
	@echo ===================================================================
	@echo Building hot-backup utility
	@echo ===================================================================
	$(MAKE) -C hbp
	@echo ===================================================================
	@echo Building driver
	@echo ===================================================================
	$(MAKE) -C driver
	@echo ===================================================================
	@echo Building terminal
	@echo ===================================================================
	$(MAKE) -C term
	@echo ===================================================================
	@echo Building export utility
	@echo ===================================================================
	$(MAKE) -C export
ifeq ($(MAKE_DOC), 1)
	@echo ===================================================================
	@echo Building Docs
	@echo ===================================================================
	$(MAKE) -C doc
endif
	@echo ===================================================================
	@echo Make done
	@echo ===================================================================


bi: build install


clean:
	$(MAKE) -C kernel clean
	$(MAKE) -C driver clean
ifeq ($(MAKE_DOC), 1)
	$(MAKE) -C doc clean
endif
	$(MAKE) -C bin clean
	$(MAKE) -C term clean
	$(MAKE) -C export clean
	$(MAKE) -C hbp clean
	$(MAKE) -C libs clean


.PHONY: build bi clean grouped_install install uninstall


ifeq ($(findstring install, $(MAKECMDGOALS)), install)
ifeq ($(JAVA_DRIVER), 1)
DOC_DIRLIST := $(shell (find ./driver/java/doc -type d))
DOC_FILELIST := $(shell (find ./driver/java/doc -type f))
endif
endif

# Installs everything into $(SEDNA_INSTALL) directory. Use this goal if you want to
# install "all in one catalog".
# This is the default installation goal for Windows and installation goal for binary 
# distribution for Linux/FreeBSD/Mac OS X
grouped_install:
	$(INSTALL) -d $(PERM1) $(SEDNA_INSTALL)/sedna/bin

	$(INSTALL) $(PRESERVE) $(PERM1) bin/se_gov$(EXE_EXT)  $(SEDNA_INSTALL)/sedna/bin/se_gov$(EXE_EXT)
	$(INSTALL) $(PRESERVE) $(PERM1) bin/se_stop$(EXE_EXT) $(SEDNA_INSTALL)/sedna/bin/se_stop$(EXE_EXT)
	$(INSTALL) $(PRESERVE) $(PERM1) bin/se_rc$(EXE_EXT)   $(SEDNA_INSTALL)/sedna/bin/se_rc$(EXE_EXT)
	$(INSTALL) $(PRESERVE) $(PERM1) bin/se_cdb$(EXE_EXT)  $(SEDNA_INSTALL)/sedna/bin/se_cdb$(EXE_EXT)
	$(INSTALL) $(PRESERVE) $(PERM1) bin/se_sm$(EXE_EXT)   $(SEDNA_INSTALL)/sedna/bin/se_sm$(EXE_EXT)
	$(INSTALL) $(PRESERVE) $(PERM1) bin/se_smsd$(EXE_EXT) $(SEDNA_INSTALL)/sedna/bin/se_smsd$(EXE_EXT)
	$(INSTALL) $(PRESERVE) $(PERM1) bin/se_trn$(EXE_EXT)  $(SEDNA_INSTALL)/sedna/bin/se_trn$(EXE_EXT)
	$(INSTALL) $(PRESERVE) $(PERM1) bin/se_rcv$(EXE_EXT)  $(SEDNA_INSTALL)/sedna/bin/se_rcv$(EXE_EXT)
	$(INSTALL) $(PRESERVE) $(PERM1) bin/se_ddb$(EXE_EXT)  $(SEDNA_INSTALL)/sedna/bin/se_ddb$(EXE_EXT)
	$(INSTALL) $(PRESERVE) $(PERM2) bin/se_term$(EXE_EXT) $(SEDNA_INSTALL)/sedna/bin/se_term$(EXE_EXT)
	$(INSTALL) $(PRESERVE) $(PERM2) bin/se_exp$(EXE_EXT)  $(SEDNA_INSTALL)/sedna/bin/se_exp$(EXE_EXT)
	$(INSTALL) $(PRESERVE) $(PERM2) bin/se_hb$(EXE_EXT)  $(SEDNA_INSTALL)/sedna/bin/se_hb$(EXE_EXT)

ifeq ($(INSTALL_DOC), 1)
	$(INSTALL) -d $(PERM2) $(SEDNA_INSTALL)/sedna/doc

	$(INSTALL) $(PRESERVE) $(PERM3) doc/AdminGuide/AdminGuide.pdf $(SEDNA_INSTALL)/sedna/doc/AdminGuide.pdf
	$(INSTALL) $(PRESERVE) $(PERM3) doc/ProgGuide/ProgGuide.pdf $(SEDNA_INSTALL)/sedna/doc/ProgGuide.pdf
	$(INSTALL) $(PRESERVE) $(PERM3) doc/ProgGuide/ClientServerProtocol/ClientServerProtocol.pdf $(SEDNA_INSTALL)/sedna/doc/ClientServerProtocol.pdf
	$(INSTALL) $(PRESERVE) $(PERM3) doc/QuickStart/QuickStart.pdf $(SEDNA_INSTALL)/sedna/doc/QuickStart.pdf
endif

ifeq ($(JAVA_DRIVER), 1)
	$(INSTALL) -d $(PERM2) $(SEDNA_INSTALL)/sedna/driver/java

	for arg in $(DOC_DIRLIST) ;do $(INSTALL) $(PERM2) -d "$(SEDNA_INSTALL)/sedna/$$arg"; done
	$(INSTALL) $(PRESERVE) $(PERM3) driver/java/sednadriver.jar $(SEDNA_INSTALL)/sedna/driver/java/sednadriver.jar
	for arg in $(DOC_FILELIST) ;do $(INSTALL) $(PRESERVE) $(PERM3) $$arg "$(SEDNA_INSTALL)/sedna/$$arg"; done
endif

	$(INSTALL) -d $(PERM2) $(SEDNA_INSTALL)/sedna/driver/c
	$(INSTALL) -d $(PERM2) $(SEDNA_INSTALL)/sedna/examples/api/c
	$(INSTALL) -d $(PERM2) $(SEDNA_INSTALL)/sedna/examples/api/java
	$(INSTALL) -d $(PERM2) $(SEDNA_INSTALL)/sedna/examples/api/scheme
	$(INSTALL) -d $(PERM2) $(SEDNA_INSTALL)/sedna/examples/api/data
	$(INSTALL) -d $(PERM2) $(SEDNA_INSTALL)/sedna/examples/commandline
	$(INSTALL) -d $(PERM2) $(SEDNA_INSTALL)/sedna/examples/api/external-functions/c
	$(INSTALL) -d $(PERM2) $(SEDNA_INSTALL)/sedna/driver/scheme/libs/plt
	$(INSTALL) -d $(PERM2) $(SEDNA_INSTALL)/sedna/driver/scheme/libs/chicken
	$(INSTALL) -d $(PERM2) $(SEDNA_INSTALL)/sedna/driver/scheme/sedna-api
	$(INSTALL) -d $(PERM2) $(SEDNA_INSTALL)/sedna/etc
	$(INSTALL) -d $(PERM2) $(SEDNA_INSTALL)/sedna/include
	$(INSTALL) -d $(PERM2) $(SEDNA_INSTALL)/sedna/share

ifeq ($(PLATFORM), UNIX)
	$(INSTALL) $(PRESERVE) $(PERM3) driver/c/libsedna.a $(SEDNA_INSTALL)/sedna/driver/c/libsedna.a
	$(INSTALL) $(PRESERVE) $(PERM2) examples/api/c/build.sh $(SEDNA_INSTALL)/sedna/examples/api/c/build.sh
	$(INSTALL) $(PRESERVE) $(PERM2) examples/api/java/Clientbuild.sh $(SEDNA_INSTALL)/sedna/examples/api/java/Clientbuild.sh
	$(INSTALL) $(PRESERVE) $(PERM2) examples/api/java/Client.sh $(SEDNA_INSTALL)/sedna/examples/api/java/Client.sh
else
	$(INSTALL) $(PRESERVE) $(PERM3) driver/c/sednamt.dll $(SEDNA_INSTALL)/sedna/driver/c/sednamt.dll
	$(INSTALL) $(PRESERVE) $(PERM3) driver/c/sednamt.lib $(SEDNA_INSTALL)/sedna/driver/c/sednamt.lib
	$(INSTALL) $(PRESERVE) $(PERM3) driver/c/libsednamt.lib $(SEDNA_INSTALL)/sedna/driver/c/libsednamt.lib
	$(INSTALL) $(PRESERVE) $(PERM3) driver/c/libsednamd.lib $(SEDNA_INSTALL)/sedna/driver/c/libsednamd.lib
	$(INSTALL) $(PRESERVE) $(PERM2) examples/api/c/build.bat $(SEDNA_INSTALL)/sedna/examples/api/c/build.bat
	$(INSTALL) $(PRESERVE) $(PERM2) examples/api/java/Clientbuild.bat $(SEDNA_INSTALL)/sedna/examples/api/java/Clientbuild.bat
	$(INSTALL) $(PRESERVE) $(PERM2) examples/api/java/Client.bat $(SEDNA_INSTALL)/sedna/examples/api/java/Client.bat
endif

	$(INSTALL) $(PRESERVE) $(PERM3) driver/c/libsedna.h $(SEDNA_INSTALL)/sedna/driver/c/libsedna.h
	$(INSTALL) $(PRESERVE) $(PERM3) driver/c/sp_defs.h $(SEDNA_INSTALL)/sedna/driver/c/sp_defs.h
	$(INSTALL) $(PRESERVE) $(PERM3) driver/scheme/collect-sedna-plt.scm $(SEDNA_INSTALL)/sedna/driver/scheme/collect-sedna-plt.scm
	$(INSTALL) $(PRESERVE) $(PERM3) driver/scheme/collect-sedna-chicken.scm $(SEDNA_INSTALL)/sedna/driver/scheme/collect-sedna-chicken.scm
	$(INSTALL) $(PRESERVE) $(PERM3) driver/scheme/libs/srfi-12.scm $(SEDNA_INSTALL)/sedna/driver/scheme/libs/srfi-12.scm
	$(INSTALL) $(PRESERVE) $(PERM3) driver/scheme/libs/plt/common.scm $(SEDNA_INSTALL)/sedna/driver/scheme/libs/plt/common.scm
	$(INSTALL) $(PRESERVE) $(PERM3) driver/scheme/libs/plt/myenv.scm $(SEDNA_INSTALL)/sedna/driver/scheme/libs/plt/myenv.scm
	$(INSTALL) $(PRESERVE) $(PERM3) driver/scheme/libs/chicken/common.scm $(SEDNA_INSTALL)/sedna/driver/scheme/libs/chicken/common.scm
	$(INSTALL) $(PRESERVE) $(PERM3) driver/scheme/libs/chicken/myenv.scm $(SEDNA_INSTALL)/sedna/driver/scheme/libs/chicken/myenv.scm
	$(INSTALL) $(PRESERVE) $(PERM3) driver/scheme/sedna-api/sedna-api.scm $(SEDNA_INSTALL)/sedna/driver/scheme/sedna-api/sedna-api.scm
	$(INSTALL) $(PRESERVE) $(PERM3) driver/scheme/sedna-api/sedna-low.scm $(SEDNA_INSTALL)/sedna/driver/scheme/sedna-api/sedna-low.scm
	$(INSTALL) $(PRESERVE) $(PERM3) etc/sednaconf.xml.sample $(SEDNA_INSTALL)/sedna/etc/sednaconf.xml.sample
	$(INSTALL) $(PRESERVE) $(PERM3) examples/api/scheme/client.scm $(SEDNA_INSTALL)/sedna/examples/api/scheme/client.scm
	$(INSTALL) $(PRESERVE) $(PERM3) examples/api/c/*.c $(SEDNA_INSTALL)/sedna/examples/api/c/
	$(INSTALL) $(PRESERVE) $(PERM3) examples/api/c/math.xqlib $(SEDNA_INSTALL)/sedna/examples/api/c/math.xqlib
	$(INSTALL) $(PRESERVE) $(PERM3) examples/api/java/Client.java $(SEDNA_INSTALL)/sedna/examples/api/java/Client.java
	$(INSTALL) $(PRESERVE) $(PERM3) examples/commandline/auction.xml $(SEDNA_INSTALL)/sedna/examples/commandline/auction.xml
	$(INSTALL) $(PRESERVE) $(PERM3) examples/commandline/*.xquery $(SEDNA_INSTALL)/sedna/examples/commandline/
	$(INSTALL) $(PRESERVE) $(PERM3) examples/api/external-functions/c/sample.xquery $(SEDNA_INSTALL)/sedna/examples/api/external-functions/c/sample.xquery
	$(INSTALL) $(PRESERVE) $(PERM3) examples/api/external-functions/c/sample.cpp $(SEDNA_INSTALL)/sedna/examples/api/external-functions/c/sample.cpp
	$(INSTALL) $(PRESERVE) $(PERM3) examples/api/external-functions/c/Makefile $(SEDNA_INSTALL)/sedna/examples/api/external-functions/c/Makefile
	$(INSTALL) $(PRESERVE) $(PERM3) include/sedna_ef.h $(SEDNA_INSTALL)/sedna/include/sedna_ef.h
	$(INSTALL) $(PRESERVE) $(PERM3) examples/api/data/categories.xml $(SEDNA_INSTALL)/sedna/examples/api/data/categories.xml
	$(INSTALL) $(PRESERVE) $(PERM3) examples/api/data/*.xml $(SEDNA_INSTALL)/sedna/examples/api/data/
	$(INSTALL) $(PRESERVE) $(PERM3) share/sedna_auth_md.xml $(SEDNA_INSTALL)/sedna/share/sedna_auth_md.xml
	$(INSTALL) $(PRESERVE) $(PERM3) AUTHORS   $(SEDNA_INSTALL)/sedna/AUTHORS
	$(INSTALL) $(PRESERVE) $(PERM3) COPYRIGHT $(SEDNA_INSTALL)/sedna/COPYRIGHT
	$(INSTALL) $(PRESERVE) $(PERM3) HISTORY   $(SEDNA_INSTALL)/sedna/HISTORY
	$(INSTALL) $(PRESERVE) $(PERM3) LICENSE   $(SEDNA_INSTALL)/sedna/LICENSE
	$(INSTALL) $(PRESERVE) $(PERM3) README    $(SEDNA_INSTALL)/sedna/README

	$(INSTALL) $(PERM4) -d $(SEDNA_INSTALL)/sedna/cfg
	$(INSTALL) $(PERM4) -d $(SEDNA_INSTALL)/sedna/data

ifndef PREFIX
SHARE_PREFIX := /usr/share
INCLUDE_PREFIX := /usr/include
ETC_PREFIX := /etc
else
ifeq ($(PREFIX), /)
SHARE_PREFIX := /usr/share
INCLUDE_PREFIX := /usr/include
ETC_PREFIX := /etc
else
SHARE_PREFIX := $(PREFIX)/share
INCLUDE_PREFIX := $(PREFIX)/include
ETC_PREFIX := $(PREFIX)/etc
endif
endif


SEDNA_DIR := sedna-$(SEDNA_VERSION).$(SEDNA_BUILD)


install: grouped_install


uninstall:
	echo "Not implemented"



#ifeq ($(PLATFORM), UNIX)
#	if test ! "$(EFF_UID)" "=" "0"; then			\
#	  echo "It is worthless to perform install without root permissions";	\
#	  exit 1;	\
#	fi
#endif

#	$(MKDIR) $(PRESERVE) $(PERM4) $(SEDNA_INSTALL)/sedna/data



ifeq ($(PLATFORM), UNIX)


.PHONY: gcov_reset gcov

gcov_reset:
	find . -name \*.da | xargs -r rm -v

endif
