#
# Makefile for Sedna (GNU make)
#

PP = .
include $(PP)/Makefile.include

EFF_UID := $(shell id -u)


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
PERM4 := -m0775
PERM1 := -m0775
PERM2 := -m0775
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






build:
	$(MAKE) -C libs
	@echo ===================================================================
	@echo Building Kernel
	@echo ===================================================================
	$(MAKE) -C kernel
	@echo ===================================================================
	@echo Building driver
	@echo ===================================================================
	$(MAKE) -C driver
	@echo ===================================================================
	@echo Building terminal
	@echo ===================================================================
	$(MAKE) -C term depend
	$(MAKE) -C term
	$(MAKE) -C export depend
	$(MAKE) -C export
ifeq ($(DOCUMENTATION), 1)
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
	$(MAKE) -C doc clean
	$(MAKE) -C bin clean
	$(MAKE) -C term clean
	$(MAKE) -C export clean
	$(MAKE) -C libs clean


.PHONY: build bi clean grouped_install dispersal_install install uninstall



# grouped_install goal
# Installs everything into $(SEDNA_INSTALL) directory. Use this goal if you want to
# install "all in one catalog".
# This is the default installation goal for Windows and installation goal for binary 
# distribution for Linux
grouped_install:
	$(INSTALL) -Dp $(PERM1) bin/se_gov$(EXE_EXT)  $(SEDNA_INSTALL)/sedna/bin/se_gov$(EXE_EXT)
	$(INSTALL) -Dp $(PERM1) bin/se_stop$(EXE_EXT) $(SEDNA_INSTALL)/sedna/bin/se_stop$(EXE_EXT)
	$(INSTALL) -Dp $(PERM1) bin/se_rc$(EXE_EXT)   $(SEDNA_INSTALL)/sedna/bin/se_rc$(EXE_EXT)
	$(INSTALL) -Dp $(PERM1) bin/se_cdb$(EXE_EXT)  $(SEDNA_INSTALL)/sedna/bin/se_cdb$(EXE_EXT)
	$(INSTALL) -Dp $(PERM1) bin/se_sm$(EXE_EXT)   $(SEDNA_INSTALL)/sedna/bin/se_sm$(EXE_EXT)
	$(INSTALL) -Dp $(PERM1) bin/se_smsd$(EXE_EXT) $(SEDNA_INSTALL)/sedna/bin/se_smsd$(EXE_EXT)
	$(INSTALL) -Dp $(PERM1) bin/se_trn$(EXE_EXT)  $(SEDNA_INSTALL)/sedna/bin/se_trn$(EXE_EXT)
	$(INSTALL) -Dp $(PERM1) bin/se_rcv$(EXE_EXT)  $(SEDNA_INSTALL)/sedna/bin/se_rcv$(EXE_EXT)
	$(INSTALL) -Dp $(PERM1) bin/se_ddb$(EXE_EXT)  $(SEDNA_INSTALL)/sedna/bin/se_ddb$(EXE_EXT)
	$(INSTALL) -Dp $(PERM2) bin/se_term$(EXE_EXT) $(SEDNA_INSTALL)/sedna/bin/se_term$(EXE_EXT)
	$(INSTALL) -Dp $(PERM2) bin/se_exp$(EXE_EXT)  $(SEDNA_INSTALL)/sedna/bin/se_exp$(EXE_EXT)
ifeq ($(DOCUMENTATION), 1)
	$(INSTALL) -Dp $(PERM3) doc/AdminGuide/AdminGuide.pdf $(SEDNA_INSTALL)/sedna/doc/AdminGuide.pdf
	$(INSTALL) -Dp $(PERM3) doc/ProgGuide/ProgGuide.pdf $(SEDNA_INSTALL)/sedna/doc/ProgGuide.pdf
	$(INSTALL) -Dp $(PERM3) doc/ProgGuide/ClientServerProtocol/ClientServerProtocol.pdf $(SEDNA_INSTALL)/sedna/doc/ClientServerProtocol.pdf
	$(INSTALL) -Dp $(PERM3) doc/QuickStart/QuickStart.pdf $(SEDNA_INSTALL)/sedna/doc/QuickStart.pdf
endif
ifeq ($(JAVA_DRIVER), 1)
	$(INSTALL) -Dp $(PERM3) driver/java/sednadriver.jar $(SEDNA_INSTALL)/sedna/driver/java/sednadriver.jar
endif
ifeq ($(PLATFORM), UNIX)
	$(INSTALL) -Dp $(PERM3) driver/c/libsedna.a $(SEDNA_INSTALL)/sedna/driver/c/libsedna.a
	$(INSTALL) -Dp $(PERM2) examples/applications/c/Clientbuild.sh $(SEDNA_INSTALL)/sedna/examples/applications/c/Clientbuild.sh
	$(INSTALL) -Dp $(PERM2) examples/applications/java/Clientbuild.sh $(SEDNA_INSTALL)/sedna/examples/applications/java/Clientbuild.sh
	$(INSTALL) -Dp $(PERM2) examples/applications/java/Client.sh $(SEDNA_INSTALL)/sedna/examples/applications/java/Client.sh
else
	$(INSTALL) -Dp $(PERM3) driver/c/libsedna.lib $(SEDNA_INSTALL)/sedna/driver/c/libsedna.lib
	$(INSTALL) -Dp $(PERM3) driver/c/libsednamt.lib $(SEDNA_INSTALL)/sedna/driver/c/libsednamt.lib
	$(INSTALL) -Dp $(PERM2) examples/applications/c/Clientbuild.bat $(SEDNA_INSTALL)/sedna/examples/applications/c/Clientbuild.bat
	$(INSTALL) -Dp $(PERM2) examples/applications/java/Clientbuild.bat $(SEDNA_INSTALL)/sedna/examples/applications/java/Clientbuild.bat
	$(INSTALL) -Dp $(PERM2) examples/applications/java/Client.bat $(SEDNA_INSTALL)/sedna/examples/applications/java/Client.bat
endif
	$(INSTALL) -Dp $(PERM3) driver/c/libsedna.h $(SEDNA_INSTALL)/sedna/driver/c/libsedna.h
	$(INSTALL) -Dp $(PERM3) driver/c/sp_defs.h $(SEDNA_INSTALL)/sedna/driver/c/sp_defs.h
	$(INSTALL) -Dp $(PERM3) driver/scheme/collect-sedna-plt.scm $(SEDNA_INSTALL)/sedna/driver/scheme/collect-sedna-plt.scm
	$(INSTALL) -Dp $(PERM3) driver/scheme/collect-sedna-chicken.scm $(SEDNA_INSTALL)/sedna/driver/scheme/collect-sedna-chicken.scm
	$(INSTALL) -Dp $(PERM3) driver/scheme/libs/srfi-12.scm $(SEDNA_INSTALL)/sedna/driver/scheme/libs/srfi-12.scm
	$(INSTALL) -Dp $(PERM3) driver/scheme/libs/plt/common.scm $(SEDNA_INSTALL)/sedna/driver/scheme/libs/plt/common.scm
	$(INSTALL) -Dp $(PERM3) driver/scheme/libs/plt/myenv.scm $(SEDNA_INSTALL)/sedna/driver/scheme/libs/plt/myenv.scm
	$(INSTALL) -Dp $(PERM3) driver/scheme/libs/chicken/common.scm $(SEDNA_INSTALL)/sedna/driver/scheme/libs/chicken/common.scm
	$(INSTALL) -Dp $(PERM3) driver/scheme/libs/chicken/myenv.scm $(SEDNA_INSTALL)/sedna/driver/scheme/libs/chicken/myenv.scm
	$(INSTALL) -Dp $(PERM3) driver/scheme/sedna-api/sedna-api.scm $(SEDNA_INSTALL)/sedna/driver/scheme/sedna-api/sedna-api.scm
	$(INSTALL) -Dp $(PERM3) driver/scheme/sedna-api/sedna-low.scm $(SEDNA_INSTALL)/sedna/driver/scheme/sedna-api/sedna-low.scm
	$(INSTALL) -Dp $(PERM3) etc/sednaconf.xml.sample $(SEDNA_INSTALL)/sedna/etc/sednaconf.xml.sample
	$(INSTALL) -Dp $(PERM3) examples/applications/scheme/client.scm $(SEDNA_INSTALL)/sedna/examples/applications/scheme/client.scm
	$(INSTALL) -Dp $(PERM3) examples/applications/scheme/region.xml $(SEDNA_INSTALL)/sedna/examples/applications/scheme/region.xml
	$(INSTALL) -Dp $(PERM3) examples/applications/c/region.xml $(SEDNA_INSTALL)/sedna/examples/applications/c/region.xml
	$(INSTALL) -Dp $(PERM3) examples/applications/c/Client.c $(SEDNA_INSTALL)/sedna/examples/applications/c/Client.c
	$(INSTALL) -Dp $(PERM3) examples/applications/java/Client.java $(SEDNA_INSTALL)/sedna/examples/applications/java/Client.java
	$(INSTALL) -Dp $(PERM3) examples/applications/java/region.xml $(SEDNA_INSTALL)/sedna/examples/applications/java/region.xml
	$(INSTALL) -Dp $(PERM3) examples/databases/auction/auction.xml $(SEDNA_INSTALL)/sedna/examples/databases/auction/auction.xml
	$(INSTALL) -Dp $(PERM3) examples/databases/auction/*.xquery $(SEDNA_INSTALL)/sedna/examples/databases/auction/
	$(INSTALL) -Dp $(PERM3) examples/applications/external-functions/c/sample.xquery $(SEDNA_INSTALL)/sedna/examples/applications/external-functions/c/sample.xquery
	$(INSTALL) -Dp $(PERM3) examples/applications/external-functions/c/sample.cpp $(SEDNA_INSTALL)/sedna/examples/applications/external-functions/c/sample.cpp
	$(INSTALL) -Dp $(PERM3) examples/applications/external-functions/c/Makefile $(SEDNA_INSTALL)/sedna/examples/applications/external-functions/c/Makefile
	$(INSTALL) -Dp $(PERM3) include/sedna_ef.h $(SEDNA_INSTALL)/sedna/include/sedna_ef.h
ifeq ($(AUTH_SWITCH), 1)
	$(INSTALL) -Dp $(PERM3) share/sedna_auth_md.xml $(SEDNA_INSTALL)/sedna/share/sedna_auth_md.xml
endif
	$(INSTALL) -Dp $(PERM3) AUTHORS   $(SEDNA_INSTALL)/sedna/AUTHORS
	$(INSTALL) -Dp $(PERM3) COPYRIGHT $(SEDNA_INSTALL)/sedna/COPYRIGHT
	$(INSTALL) -Dp $(PERM3) FAQ       $(SEDNA_INSTALL)/sedna/FAQ
	$(INSTALL) -Dp $(PERM3) HISTORY   $(SEDNA_INSTALL)/sedna/HISTORY
	$(INSTALL) -Dp $(PERM3) LICENSE   $(SEDNA_INSTALL)/sedna/LICENSE
	$(INSTALL) -Dp $(PERM3) README    $(SEDNA_INSTALL)/sedna/README


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



# dispersal_install goal
# This is the default installation goal for Unix'es. 
# Installs executables into $(PREFIX)/bin, doc files into share
dispersal_install:
	$(INSTALL) -Dp $(PERM1) bin/se_gov$(EXE_EXT)  $(PREFIX)/bin/se_gov$(EXE_EXT)
	$(INSTALL) -Dp $(PERM1) bin/se_stop$(EXE_EXT) $(PREFIX)/bin/se_stop$(EXE_EXT)
	$(INSTALL) -Dp $(PERM1) bin/se_rc$(EXE_EXT)   $(PREFIX)/bin/se_rc$(EXE_EXT)
	$(INSTALL) -Dp $(PERM1) bin/se_cdb$(EXE_EXT)  $(PREFIX)/bin/se_cdb$(EXE_EXT)
	$(INSTALL) -Dp $(PERM1) bin/se_sm$(EXE_EXT)   $(PREFIX)/bin/se_sm$(EXE_EXT)
	$(INSTALL) -Dp $(PERM1) bin/se_smsd$(EXE_EXT) $(PREFIX)/bin/se_smsd$(EXE_EXT)
	$(INSTALL) -Dp $(PERM1) bin/se_trn$(EXE_EXT)  $(PREFIX)/bin/se_trn$(EXE_EXT)
	$(INSTALL) -Dp $(PERM1) bin/se_rcv$(EXE_EXT)  $(PREFIX)/bin/se_rcv$(EXE_EXT)
	$(INSTALL) -Dp $(PERM1) bin/se_ddb$(EXE_EXT)  $(PREFIX)/bin/se_ddb$(EXE_EXT)
	$(INSTALL) -Dp $(PERM2) bin/se_term$(EXE_EXT) $(PREFIX)/bin/se_term$(EXE_EXT)
	$(INSTALL) -Dp $(PERM2) bin/se_exp$(EXE_EXT)  $(PREFIX)/bin/se_exp$(EXE_EXT)
ifeq ($(DOCUMENTATION), 1)
	$(INSTALL) -Dp $(PERM3) doc/AdminGuide/AdminGuide.pdf $(SHARE_PREFIX)/doc/$(SEDNA_DIR)/AdminGuide.pdf
	$(INSTALL) -Dp $(PERM3) doc/ProgGuide/ProgGuide.pdf $(SHARE_PREFIX)/doc/$(SEDNA_DIR)/ProgGuide.pdf
	$(INSTALL) -Dp $(PERM3) doc/ProgGuide/ClientServerProtocol/ClientServerProtocol.pdf $(SHARE_PREFIX)/doc/$(SEDNA_DIR)/ClientServerProtocol.pdf
	$(INSTALL) -Dp $(PERM3) doc/QuickStart/QuickStart.pdf $(SHARE_PREFIX)/doc/$(SEDNA_DIR)/QuickStart.pdf
endif
ifeq ($(JAVA_DRIVER), 1)
	$(INSTALL) -Dp $(PERM3) driver/java/sednadriver.jar $(SHARE_PREFIX)/java/sednadriver.jar
endif
	$(INSTALL) -Dp $(PERM3) driver/c/libsedna.a $(PREFIX)/lib/libsedna.a
	$(INSTALL) -Dp $(PERM2) examples/applications/c/Clientbuild.sh $(SHARE_PREFIX)/$(SEDNA_DIR)/examples/applications/c/Clientbuild.sh
	$(INSTALL) -Dp $(PERM2) examples/applications/java/Clientbuild.sh $(SHARE_PREFIX)/$(SEDNA_DIR)/examples/applications/java/Clientbuild.sh
	$(INSTALL) -Dp $(PERM2) examples/applications/java/Client.sh $(SHARE_PREFIX)/$(SEDNA_DIR)/examples/applications/java/Client.sh
	$(INSTALL) -Dp $(PERM3) driver/c/libsedna.h $(INCLUDE_PREFIX)/libsedna.h
	$(INSTALL) -Dp $(PERM3) driver/c/sp_defs.h $(INCLUDE_PREFIX)/sp_defs.h
	$(INSTALL) -Dp $(PERM3) driver/scheme/collect-sedna-plt.scm $(SHARE_PREFIX)/$(SEDNA_DIR)/driver/scheme/collect-sedna-plt.scm
	$(INSTALL) -Dp $(PERM3) driver/scheme/collect-sedna-chicken.scm $(SHARE_PREFIX)/$(SEDNA_DIR)/driver/scheme/collect-sedna-chicken.scm
	$(INSTALL) -Dp $(PERM3) driver/scheme/libs/srfi-12.scm $(SHARE_PREFIX)/$(SEDNA_DIR)/driver/scheme/libs/srfi-12.scm
	$(INSTALL) -Dp $(PERM3) driver/scheme/libs/plt/common.scm $(SHARE_PREFIX)/$(SEDNA_DIR)/driver/scheme/libs/plt/common.scm
	$(INSTALL) -Dp $(PERM3) driver/scheme/libs/plt/myenv.scm $(SHARE_PREFIX)/$(SEDNA_DIR)/driver/scheme/libs/plt/myenv.scm
	$(INSTALL) -Dp $(PERM3) driver/scheme/libs/chicken/common.scm $(SHARE_PREFIX)/$(SEDNA_DIR)/driver/scheme/libs/chicken/common.scm
	$(INSTALL) -Dp $(PERM3) driver/scheme/libs/chicken/myenv.scm $(SHARE_PREFIX)/$(SEDNA_DIR)/driver/scheme/libs/chicken/myenv.scm
	$(INSTALL) -Dp $(PERM3) driver/scheme/sedna-api/sedna-api.scm $(SHARE_PREFIX)/$(SEDNA_DIR)/driver/scheme/sedna-api/sedna-api.scm
	$(INSTALL) -Dp $(PERM3) driver/scheme/sedna-api/sedna-low.scm $(SHARE_PREFIX)/$(SEDNA_DIR)/driver/scheme/sedna-api/sedna-low.scm
	$(INSTALL) -Dp $(PERM3) etc/sednaconf.xml.sample $(ETC_PREFIX)/sednaconf.xml.sample
	$(INSTALL) -Dp $(PERM3) examples/applications/scheme/client.scm $(SHARE_PREFIX)/$(SEDNA_DIR)/examples/applications/scheme/client.scm
	$(INSTALL) -Dp $(PERM3) examples/applications/scheme/region.xml $(SHARE_PREFIX)/$(SEDNA_DIR)/examples/applications/scheme/region.xml
	$(INSTALL) -Dp $(PERM3) examples/applications/c/region.xml $(SHARE_PREFIX)/$(SEDNA_DIR)/examples/applications/c/region.xml
	$(INSTALL) -Dp $(PERM3) examples/applications/c/Client.c $(SHARE_PREFIX)/$(SEDNA_DIR)/examples/applications/c/Client.c
	$(INSTALL) -Dp $(PERM3) examples/applications/java/Client.java $(SHARE_PREFIX)/$(SEDNA_DIR)/examples/applications/java/Client.java
	$(INSTALL) -Dp $(PERM3) examples/applications/java/region.xml $(SHARE_PREFIX)/$(SEDNA_DIR)/examples/applications/java/region.xml
	$(INSTALL) -Dp $(PERM3) examples/databases/auction/auction.xml $(SHARE_PREFIX)/$(SEDNA_DIR)/examples/databases/auction/auction.xml
	$(INSTALL) -Dp $(PERM3) examples/databases/auction/*.xquery $(SHARE_PREFIX)/$(SEDNA_DIR)/examples/databases/auction/
	$(INSTALL) -Dp $(PERM3) examples/applications/external-functions/c/sample.xquery $(SHARE_PREFIX)/$(SEDNA_DIR)/examples/applications/external-functions/c/sample.xquery
	$(INSTALL) -Dp $(PERM3) examples/applications/external-functions/c/sample.cpp $(SHARE_PREFIX)/$(SEDNA_DIR)/examples/applications/external-functions/c/sample.cpp
	$(INSTALL) -Dp $(PERM3) examples/applications/external-functions/c/Makefile $(SHARE_PREFIX)/$(SEDNA_DIR)/examples/applications/external-functions/c/Makefile
	$(INSTALL) -Dp $(PERM3) include/sedna_ef.h $(INCLUDE_PREFIX)/sedna_ef.h
ifeq ($(AUTH_SWITCH), 1)
	$(INSTALL) -Dp $(PERM3) share/sedna_auth_md.xml $(SEDNA_INSTALL)/sedna/share/sedna_auth_md.xml
endif
	$(INSTALL) -Dp $(PERM3) AUTHORS   $(SHARE_PREFIX)/doc/$(SEDNA_DIR)/AUTHORS
	$(INSTALL) -Dp $(PERM3) COPYRIGHT $(SHARE_PREFIX)/doc/$(SEDNA_DIR)/COPYRIGHT
	$(INSTALL) -Dp $(PERM3) HISTORY   $(SHARE_PREFIX)/doc/$(SEDNA_DIR)/HISTORY
	$(INSTALL) -Dp $(PERM3) LICENSE   $(SHARE_PREFIX)/doc/$(SEDNA_DIR)/LICENSE
	$(INSTALL) -Dp $(PERM3) README    $(SHARE_PREFIX)/doc/$(SEDNA_DIR)/README



install: grouped_install



uninstall:
	echo "Not implemented"




#ifeq ($(PLATFORM), UNIX)
#	if test ! "$(EFF_UID)" "=" "0"; then			\
#	  echo "It is worthless to perform install without root permissions";	\
#	  exit 1;	\
#	fi
#endif

#	$(MKDIR) -p $(PERM4) $(SEDNA_INSTALL)/sedna/data



ifeq ($(PLATFORM), UNIX)


.PHONY: gcov_reset gcov

gcov_reset:
	find . -name \*.da | xargs -r rm -v

endif
