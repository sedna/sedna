#!/bin/bash

# File:  release.sh
# Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

# This is script for making release of the Sedna system.
# Unfortunately, it depends on the environment of MODIS group and
# will not work in other places without modification

BUILD_MACHINE=volga.ispras.ru


failwith() {
  echo "Error: $*" 1>&2
  exit 1
}

##### SCRIPT USAGE ############################################################
usage() {
    echo "Usage"
    echo "    $0" " local          - build local release"
    echo "    $0" " release        - build release"
    echo "    $0" " local-dts      - build local dtSearch release"
    echo
    failwith "Wrong arguments"
}

BUILD_DTSEARCH=0
if test $# -ne 1; then
    usage;
else
    case $1
    in
        local)
            BUILD_TYPE=local
            ;;
        release)
            BUILD_TYPE=release
            ;;
        local-dts)
            BUILD_TYPE=local
            BUILD_DTSEARCH=1
            ;;
        *)
            usage;;
    esac
fi
##### SCRIPT USAGE ############################################################


lookfor() {
  save_IFS="${IFS}"
  IFS="${IFS}:"
  for dir in $PATH; do
    if test -x "$dir/$1"; then
      eval "$1=$dir/$1"
      IFS="$save_IFS"
      return
    fi
  done
  IFS="$save_IFS"
  failwith "Could not find \"$1\"."
}


if test "x$SEDNA_INSTALL" = "x"; then
  failwith "SEDNA_INSTALL is not set up"
fi

lookfor uname
lookfor cat
lookfor cp
lookfor rm
lookfor mv
lookfor tar
lookfor flex
lookfor bison

export OS=`uname` || failwith "Cannot mine operating system name"
export SEDNA_VERSION=`cat ver` || failwith "Cannot read ver file"
export BUILD_FILE=build-$SEDNA_VERSION
export MD5_EXT=md5

if test "$OS" "=" "Linux"; then

  export BUILD_SUFFIX=linux
  export BUILD_PLATFORM=`uname -m` || failwith "Cannot mine platform type"
  export DISTR_EXT=sh
  export SRC_EXT=tar.gz
  export SQL_CONNECTION=0
  MAKE_COMMAND=make
  MD5=md5sum
  OS_TYPE=nix

elif test "$OS" "=" "Darwin"; then

  export BUILD_SUFFIX=darwin
  export BUILD_PLATFORM=`uname -p` || failwith "Cannot mine platform type"
  export DISTR_EXT=sh
  export SRC_EXT=tar.gz
  export SQL_CONNECTION=0
  MAKE_COMMAND=make
  MD5=md5
  OS_TYPE=nix

elif test "$OS" "=" "FreeBSD"; then

  export BUILD_SUFFIX=freebsd
  export BUILD_PLATFORM=`uname -p` || failwith "Cannot mine platform type"
  export DISTR_EXT=sh
  export SRC_EXT=tar.gz
  export SQL_CONNECTION=0
  MAKE_COMMAND=gmake
  MD5=md5
  OS_TYPE=nix

elif test "$OS" "=" "SunOS"; then

  export BUILD_SUFFIX=sunos
  export BUILD_PLATFORM=`uname -p` || failwith "Cannot mine platform type"
  export DISTR_EXT=sh
  export SRC_EXT=tar.gz
  export SQL_CONNECTION=0
  MAKE_COMMAND=gmake
  MD5="digest -a md5"
  OS_TYPE=nix

else  #Windows (Cygwin)

  export BUILD_SUFFIX=win
  export BUILD_PLATFORM=""
  export DISTR_EXT=tar.gz
  export SRC_EXT=tar.gz
  export SQL_CONNECTION=1
  MAKE_COMMAND=make
  MD5=md5sum
  OS_TYPE=win

fi

if test "$STATIC_LIBS"x = "true"x; then
  BUILD_SUFFIX=$BUILD_SUFFIX-static
fi

prepare_win_source() {
    echo prepare_windows_source &&
    rm -rf $FILE_BASE/libs/*.tar.gz &&
    rm -rf $FILE_BASE/libs/pcre/*.a &&
    $MAKE_COMMAND -C $FILE_BASE/libs &&
    rm -rf $FILE_BASE/libs/bin &&
    rm -rf $FILE_BASE/libs/src &&
    rm -rf $FILE_BASE/libs/chicken_panic_hook.diff &&
    echo "build:" > $FILE_BASE/libs/Makefile &&
    echo "clean:" >> $FILE_BASE/libs/Makefile
}

prepare_nix_source() {
    rm -rf $FILE_BASE/libs/expat &&
    rm -rf $FILE_BASE/libs/chicken &&
    rm -rf $FILE_BASE/libs/pcre &&
    rm -rf $FILE_BASE/libs/bin
}

create_sed_script()
{
    cat > $SEDSCRIPT <<EEE
s/^ACTIVE_CONFIGURATION[ ]*=[A-Za-z0-9 ]\{1,\}\(\r\{0,1\}\)$/ACTIVE_CONFIGURATION = Release\1/
s/^MAKE_DOC[ ]*=[A-Za-z0-9 ]\{1,\}\(\r\{0,1\}\)$/MAKE_DOC = 0\1/
s/^INSTALL_DOC[ ]*=[A-Za-z0-9 ]\{1,\}\(\r\{0,1\}\)$/INSTALL_DOC = 1\1/
s/^EL_DEBUG[ ]*=[A-Za-z0-9 ]\{1,\}\(\r\{0,1\}\)$/EL_DEBUG = 0\1/
s/^JAVA_DRIVER[ ]*=[A-Za-z0-9 ]\{1,\}\(\r\{0,1\}\)$/JAVA_DRIVER = 0\1/
s/^SQL_CONNECTION[ ]*=[A-Za-z0-9 ]\{1,\}\(\r\{0,1\}\)$/SQL_CONNECTION = 0\1/
s/^STATIC_SYS_LIBS[ ]*=[A-Za-z0-9 ]\{1,\}\(\r\{0,1\}\)$/STATIC_SYS_LIBS = 0\1/
s/^SE_ENABLE_GCOV[ ]*=[A-Za-z0-9 ]\{1,\}\(\r\{0,1\}\)$/SE_ENABLE_GCOV = 0\1/
s/^ENABLE_DTSEARCH[ ]*=[A-Za-z0-9 ]\{1,\}\(\r\{0,1\}\)$/ENABLE_DTSEARCH = $BUILD_DTSEARCH\1/
s/^ENABLE_TRIGGERS[ ]*=[A-Za-z0-9 ]\{1,\}\(\r\{0,1\}\)$/ENABLE_TRIGGERS = 1\1/
s/^CLEANUP_LIBRARIES[ ]*=[A-Za-z0-9 ]\{1,\}\(\r\{0,1\}\)$/CLEANUP_LIBRARIES = 1\1/
EEE
}

exclude_files() {
  for exclude_file in `cat $FILE_BASE/exclude_files`; do 
    rm -rf $FILE_BASE/$exclude_file || return 1
  done
}

prepare_source() {
    if test "$OS_TYPE" "=" "nix"; then
	  prepare_nix_source
    else
	  prepare_win_source
    fi
    mv $FILE_BASE/Makefile.include $FILE_BASE/Makefile.include.orig &&
    SEDSCRIPT=Makefile.include.sed &&
    create_sed_script &&
    sed -f $SEDSCRIPT $FILE_BASE/Makefile.include.orig > $FILE_BASE/Makefile.include &&
    rm -f $FILE_BASE/Makefile.include.orig &&
    exclude_files &&
    OLDDIR="`pwd`" &&

    cd $FILE_BASE/doc &&
    $MAKE_COMMAND &&
    rm -f AdminGuide/*.{aux,log,tex,toc} AdminGuide/Makefile &&
    rm -f QuickStart/*.{aux,log,tex,toc} QuickStart/Makefile &&
    rm -f ProgGuide/ClientServerProtocol/*.{aux,log,tex,toc} ProgGuide/ClientServerProtocol/Makefile &&
    rm -f ProgGuide/*.{aux,log,tex,toc} ProgGuide/Makefile &&
    rm Makefile &&
    cd "$OLDDIR" &&

    cd $FILE_BASE/kernel/tr/xqp &&
    chmod +x release.sh &&
    ./release.sh &&
    cd "$OLDDIR"
}


#script for downloading build-number-file
get_build_file() {
    # get build_file and build_state_file
    echo "open $BUILD_MACHINE" > ftpscript.txt &&
    echo "anonymous" >> ftpscript.txt &&
    echo "password" >> ftpscript.txt &&
    echo "cd build" >> ftpscript.txt &&
    echo "get $BUILD_FILE" >> ftpscript.txt &&
    echo "close" >> ftpscript.txt &&
    echo "quit" >> ftpscript.txt || failwith "Cannot write to ftpscript.txt"

    if test "$OS_TYPE" "=" "nix"; then
        ncftp <ftpscript.txt
    else
        ftp -s:ftpscript.txt
    fi || failwith "Cannot get build_file"

    BUILD=`cat $BUILD_FILE` || failwith "Cannot read build_file"

    rm -f ftpscript.txt || failwith "Cannot remove ftpscript.txt"
    rm -f $BUILD_FILE || failwith "Cannot remove build_file"
}


#script for uploading results of build to the BUILD_MACHINE
#parameters: binary_file_name source_file_name
#requirements: current directory must be $SEDNA_INSTALL
put_results_to_build_machine() {
	echo "open $BUILD_MACHINE" > ftpscript.txt &&
	echo "anonymous" >> ftpscript.txt &&
	echo "password" >> ftpscript.txt &&
	echo "binary" >> ftpscript.txt &&
	echo "cd build" >> ftpscript.txt &&
	echo "put $1" >> ftpscript.txt &&
	echo "put $2" >> ftpscript.txt &&
	if test $# -ge 3; then echo "put $3" >> ftpscript.txt; fi &&
	if test $# -eq 4; then echo "put $4" >> ftpscript.txt; fi &&
	echo "close" >> ftpscript.txt &&
	echo "quit" >> ftpscript.txt || failwith "Cannot write to ftpscript.txt"

    if test "$OS_TYPE" "=" "nix"; then
        ncftp <ftpscript.txt
    else
        ftp -s:ftpscript.txt
    fi || failwith "Cannot upload build results to the build machine"

    rm -f ftpscript.txt || failwith "Cannot remove build_file"
}


##### CREATE BUILD FILE AND SET UP VARIABLES ##################################
get_build_file

echo -n $BUILD > build || failwith "Cannot write to build file"

FILE_BASE=sedna-$SEDNA_VERSION.$BUILD
if [ $BUILD_PLATFORM ]; then
  BIN_FILE_NAME=$FILE_BASE-bin-$BUILD_SUFFIX-$BUILD_PLATFORM
  SRC_FILE_NAME=$FILE_BASE-src-$BUILD_SUFFIX-$BUILD_PLATFORM
else
  BIN_FILE_NAME=$FILE_BASE-bin-$BUILD_SUFFIX
  SRC_FILE_NAME=$FILE_BASE-src-$BUILD_SUFFIX
fi
##### CREATE BUILD FILE AND SET UP VARIABLES ##################################



##### MAKE CLEAN ##############################################################
$MAKE_COMMAND clean || failwith "make clean failed"
##### MAKE CLEAN ##############################################################



##### SOURCE RELEASE ##########################################################
(cd .. &&
 cp -r sedna $FILE_BASE &&
 prepare_source &&
 (tar cvf - $FILE_BASE | gzip 1>$SRC_FILE_NAME.$SRC_EXT) &&
 rm -rf $FILE_BASE &&
 mv $SRC_FILE_NAME.$SRC_EXT $SEDNA_INSTALL) || 
                               failwith "Failed to create source distribution"
##### SOURCE RELEASE ##########################################################



##### MAKE ####################################################################
export ACTIVE_CONFIGURATION=Release
export DOCUMENTATION=1
export EL_DEBUG=0
export JAVA_DRIVER=1
if test "$STATIC_LIBS"x = "true"x; then
  export STATIC_SYS_LIBS=1
fi
export ENABLE_DTSEARCH=$BUILD_DTSEARCH
$MAKE_COMMAND || failwith "make failed"
##### MAKE ####################################################################



##### MAKE INSTALL ############################################################
rm -fr $SEDNA_INSTALL/sedna
$MAKE_COMMAND grouped_install || failwith "make install failed"
##### MAKE INSTALL ############################################################



##### RELEASE #################################################################
if test "$OS_TYPE" "=" "nix"; then 
    cp scripts/linux-install.sh $SEDNA_INSTALL; 
fi || failwith "Cannot copy scripts/linux-install.sh"

(cd $SEDNA_INSTALL &&
 (tar cvf - sedna | gzip 1>$BIN_FILE_NAME.tar.gz) || failwith "Cannot create archive of binaries"

 if test "$OS_TYPE" "=" "nix"; then 
    (SUM=`cksum $BIN_FILE_NAME.tar.gz` &&
     SUM=`set $SUM; echo $1` &&
     sed "s/PLACE_FOR_BINARY_SUM/$SUM/" linux-install.sh >$BIN_FILE_NAME.sh &&
     cat $BIN_FILE_NAME.tar.gz >>$BIN_FILE_NAME.sh &&
     chmod a+x $BIN_FILE_NAME.sh &&
     rm -f linux-install.sh &&
     rm -f $BIN_FILE_NAME.tar.gz);
 fi || failwith "Cannot create selfextracted binary package"
 
 $MD5 $BIN_FILE_NAME.$DISTR_EXT > $BIN_FILE_NAME.$DISTR_EXT.$MD5_EXT
 $MD5 $SRC_FILE_NAME.$SRC_EXT > $SRC_FILE_NAME.$SRC_EXT.$MD5_EXT

 if test "$BUILD_TYPE" "!=" "local"; then 
     put_results_to_build_machine $BIN_FILE_NAME.$DISTR_EXT $SRC_FILE_NAME.$SRC_EXT $BIN_FILE_NAME.$DISTR_EXT.$MD5_EXT $SRC_FILE_NAME.$SRC_EXT.$MD5_EXT
 fi)
##### RELEASE #################################################################



