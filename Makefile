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
	$(MAKE) -C libs clean


.PHONY: build bi clean grouped_install install uninstall


ifeq ($(JAVA_DRIVER), 1)
DOC_FILELIST := $(shell (find ./driver/java/doc -type f))
endif


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
ifeq ($(INSTALL_DOC), 1)
	$(INSTALL) -Dp $(PERM3) doc/AdminGuide/AdminGuide.pdf $(SEDNA_INSTALL)/sedna/doc/AdminGuide.pdf
	$(INSTALL) -Dp $(PERM3) doc/ProgGuide/ProgGuide.pdf $(SEDNA_INSTALL)/sedna/doc/ProgGuide.pdf
	$(INSTALL) -Dp $(PERM3) doc/ProgGuide/ClientServerProtocol/ClientServerProtocol.pdf $(SEDNA_INSTALL)/sedna/doc/ClientServerProtocol.pdf
	$(INSTALL) -Dp $(PERM3) doc/QuickStart/QuickStart.pdf $(SEDNA_INSTALL)/sedna/doc/QuickStart.pdf
endif
ifeq ($(JAVA_DRIVER), 1)
	$(INSTALL) -Dp $(PERM3) driver/java/sednadriver.jar $(SEDNA_INSTALL)/sedna/driver/java/sednadriver.jar

	for arg in $(DOC_FILELIST) ;do $(INSTALL) -Dp $(PERM3) $$arg "$(SEDNA_INSTALL)/sedna/$$arg"; done
	
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/package-list $(SEDNA_INSTALL)/sedna/driver/java/doc/package-list
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/allclasses-frame.html $(SEDNA_INSTALL)/sedna/driver/java/doc/allclasses-frame.html
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/allclasses-noframe.html $(SEDNA_INSTALL)/sedna/driver/java/doc/allclasses-noframe.html
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/constant-values.html $(SEDNA_INSTALL)/sedna/driver/java/doc/constant-values.html
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/deprecated-list.html $(SEDNA_INSTALL)/sedna/driver/java/doc/deprecated-list.html
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/help-doc.html $(SEDNA_INSTALL)/sedna/driver/java/doc/help-doc.html
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/index.html $(SEDNA_INSTALL)/sedna/driver/java/doc/index.html
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/index-all.html $(SEDNA_INSTALL)/sedna/driver/java/doc/index-all.html
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/overview-tree.html $(SEDNA_INSTALL)/sedna/driver/java/doc/overview-tree.html
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/serialized-form.html $(SEDNA_INSTALL)/sedna/driver/java/doc/serialized-form.html
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/stylesheet.css $(SEDNA_INSTALL)/sedna/driver/java/doc/stylesheet.css
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/ru/ispras/sedna/driver/DatabaseManager.html $(SEDNA_INSTALL)/sedna/driver/java/doc/ru/ispras/sedna/driver/DatabaseManager.html
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/ru/ispras/sedna/driver/DriverException.html $(SEDNA_INSTALL)/sedna/driver/java/doc/ru/ispras/sedna/driver/DriverException.html
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/ru/ispras/sedna/driver/package-frame.html $(SEDNA_INSTALL)/sedna/driver/java/doc/ru/ispras/sedna/driver/package-frame.html
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/ru/ispras/sedna/driver/package-summary.html $(SEDNA_INSTALL)/sedna/driver/java/doc/ru/ispras/sedna/driver/package-summary.html
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/ru/ispras/sedna/driver/package-tree.html $(SEDNA_INSTALL)/sedna/driver/java/doc/ru/ispras/sedna/driver/package-tree.html
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/ru/ispras/sedna/driver/ResultType.html $(SEDNA_INSTALL)/sedna/driver/java/doc/ru/ispras/sedna/driver/ResultType.html
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/ru/ispras/sedna/driver/SednaConnection.html $(SEDNA_INSTALL)/sedna/driver/java/doc/ru/ispras/sedna/driver/SednaConnection.html
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/ru/ispras/sedna/driver/SednaSerializedResult.html $(SEDNA_INSTALL)/sedna/driver/java/doc/ru/ispras/sedna/driver/SednaSerializedResult.html
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/ru/ispras/sedna/driver/SednaStatement.html $(SEDNA_INSTALL)/sedna/driver/java/doc/ru/ispras/sedna/driver/SednaStatement.html
	#$(INSTALL) -Dp $(PERM3) driver/java/doc/resources/inherit.gif $(SEDNA_INSTALL)/sedna/driver/java/doc/resources/inherit.gif

endif
ifeq ($(PLATFORM), UNIX)
	$(INSTALL) -Dp $(PERM3) driver/c/libsedna.a $(SEDNA_INSTALL)/sedna/driver/c/libsedna.a
	$(INSTALL) -Dp $(PERM2) examples/api/c/build.sh $(SEDNA_INSTALL)/sedna/examples/api/c/build.sh
	$(INSTALL) -Dp $(PERM2) examples/api/java/Clientbuild.sh $(SEDNA_INSTALL)/sedna/examples/api/java/Clientbuild.sh
	$(INSTALL) -Dp $(PERM2) examples/api/java/Client.sh $(SEDNA_INSTALL)/sedna/examples/api/java/Client.sh
else
	$(INSTALL) -Dp $(PERM3) driver/c/libsedna.lib $(SEDNA_INSTALL)/sedna/driver/c/libsedna.lib
	$(INSTALL) -Dp $(PERM3) driver/c/libsednamt.lib $(SEDNA_INSTALL)/sedna/driver/c/libsednamt.lib
	$(INSTALL) -Dp $(PERM2) examples/api/c/build.bat $(SEDNA_INSTALL)/sedna/examples/api/c/build.bat
	$(INSTALL) -Dp $(PERM2) examples/api/java/Clientbuild.bat $(SEDNA_INSTALL)/sedna/examples/api/java/Clientbuild.bat
	$(INSTALL) -Dp $(PERM2) examples/api/java/Client.bat $(SEDNA_INSTALL)/sedna/examples/api/java/Client.bat
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
	$(INSTALL) -Dp $(PERM3) examples/api/scheme/client.scm $(SEDNA_INSTALL)/sedna/examples/api/scheme/client.scm
	$(INSTALL) -Dp $(PERM3) examples/api/c/*.c $(SEDNA_INSTALL)/sedna/examples/api/c/
	$(INSTALL) -Dp $(PERM3) examples/api/c/math.xqlib $(SEDNA_INSTALL)/sedna/examples/api/c/math.xqlib
	$(INSTALL) -Dp $(PERM3) examples/api/java/Client.java $(SEDNA_INSTALL)/sedna/examples/api/java/Client.java
	$(INSTALL) -Dp $(PERM3) examples/commandline/auction.xml $(SEDNA_INSTALL)/sedna/examples/commandline/auction.xml
	$(INSTALL) -Dp $(PERM3) examples/commandline/*.xquery $(SEDNA_INSTALL)/sedna/examples/commandline/
	$(INSTALL) -Dp $(PERM3) examples/api/external-functions/c/sample.xquery $(SEDNA_INSTALL)/sedna/examples/api/external-functions/c/sample.xquery
	$(INSTALL) -Dp $(PERM3) examples/api/external-functions/c/sample.cpp $(SEDNA_INSTALL)/sedna/examples/api/external-functions/c/sample.cpp
	$(INSTALL) -Dp $(PERM3) examples/api/external-functions/c/Makefile $(SEDNA_INSTALL)/sedna/examples/api/external-functions/c/Makefile
	$(INSTALL) -Dp $(PERM3) include/sedna_ef.h $(SEDNA_INSTALL)/sedna/include/sedna_ef.h
	$(INSTALL) -Dp $(PERM3) examples/api/data/*.xml $(SEDNA_INSTALL)/sedna/examples/api/data/
ifeq ($(AUTH_SWITCH), 1)
	$(INSTALL) -Dp $(PERM3) share/sedna_auth_md.xml $(SEDNA_INSTALL)/sedna/share/sedna_auth_md.xml
endif
	$(INSTALL) -Dp $(PERM3) AUTHORS   $(SEDNA_INSTALL)/sedna/AUTHORS
	$(INSTALL) -Dp $(PERM3) COPYRIGHT $(SEDNA_INSTALL)/sedna/COPYRIGHT
	$(INSTALL) -Dp $(PERM3) FAQ.html  $(SEDNA_INSTALL)/sedna/FAQ.html
	$(INSTALL) -Dp $(PERM3) HISTORY   $(SEDNA_INSTALL)/sedna/HISTORY
	$(INSTALL) -Dp $(PERM3) LICENSE   $(SEDNA_INSTALL)/sedna/LICENSE
	$(INSTALL) -Dp $(PERM3) README    $(SEDNA_INSTALL)/sedna/README
	$(INSTALL) -p $(PERM4) -d $(SEDNA_INSTALL)/sedna/cfg
	$(INSTALL) -p $(PERM4) -d $(SEDNA_INSTALL)/sedna/data

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

#	$(MKDIR) -p $(PERM4) $(SEDNA_INSTALL)/sedna/data



ifeq ($(PLATFORM), UNIX)


.PHONY: gcov_reset gcov

gcov_reset:
	find . -name \*.da | xargs -r rm -v

endif
