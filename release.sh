#!/bin/bash

# File:  release.sh
# Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)

# This is script for making release of the Sedna system.
# Unfortunately, it depends on the environment of MODIS group and
# may not work in other places without modification

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

BUILD_DTSEARCH=OFF
VERSION_SUFFIX=
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
            BUILD_DTSEARCH=ON
            VERSION_SUFFIX=dt
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
  export SQL_CONNECTION=OFF
  MAKE_COMMAND=make
  MD5=md5sum
  OS_TYPE=nix

elif test "$OS" "=" "Darwin"; then

  export BUILD_SUFFIX=darwin
  export BUILD_PLATFORM=`uname -p` || failwith "Cannot mine platform type"
  export DISTR_EXT=sh
  export SRC_EXT=tar.gz
  export SQL_CONNECTION=OFF
  MAKE_COMMAND=make
  MD5=md5
  OS_TYPE=nix

elif test "$OS" "=" "FreeBSD"; then

  export BUILD_SUFFIX=freebsd
  export BUILD_PLATFORM=`uname -p` || failwith "Cannot mine platform type"
  export DISTR_EXT=sh
  export SRC_EXT=tar.gz
  export SQL_CONNECTION=OFF
  MAKE_COMMAND=gmake
  MD5=md5
  OS_TYPE=nix

elif test "$OS" "=" "SunOS"; then

  export BUILD_SUFFIX=sunos
  export BUILD_PLATFORM=`uname -p` || failwith "Cannot mine platform type"
  export DISTR_EXT=sh
  export SRC_EXT=tar.gz
  export SQL_CONNECTION=OFF
  MAKE_COMMAND=gmake
  MD5="digest -a md5"
  OS_TYPE=nix

else  #Windows (Cygwin)

  export BUILD_SUFFIX=win
  export BUILD_PLATFORM=""
  export DISTR_EXT=tar.gz
  export SRC_EXT=tar.gz
  export SQL_CONNECTION=ON
  MAKE_COMMAND=nmake
  MD5=md5sum
  OS_TYPE=win

fi

if test "$STATIC_LIBS"x = "true"x; then
  BUILD_SUFFIX=$BUILD_SUFFIX-static
fi

prepare_win_source() {
    echo prepare_windows_source &&
    rm -rf $FILE_BASE/libs/bin
}

prepare_nix_source() {
    echo prepare_linux_source &&
    rm -rf $FILE_BASE/libs/bin
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
    OLDDIR="`pwd`" &&

    cd $FILE_BASE/kernel/tr/xqp &&
    chmod +x release.sh &&
    ./release.sh &&
    cd "$OLDDIR"
}

prepare_post_source() {
    cp $SEDNA_INSTALL/sedna/doc/AdminGuide.pdf $FILE_BASE/doc/AdminGuide &&
    cp $SEDNA_INSTALL/sedna/doc/ProgGuide.pdf $FILE_BASE/doc/ProgGuide &&
    cp $SEDNA_INSTALL/sedna/doc/ClientServerProtocol.pdf $FILE_BASE/doc/ClientServerProtocol &&
    cp $SEDNA_INSTALL/sedna/doc/QuickStart.pdf $FILE_BASE/doc/QuickStart &&
    exclude_files
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
FILE_BASE=sedna-$SEDNA_VERSION.$BUILD$VERSION_SUFFIX
if [ $BUILD_PLATFORM ]; then
  BIN_FILE_NAME=$FILE_BASE-bin-$BUILD_SUFFIX-$BUILD_PLATFORM
  SRC_FILE_NAME=$FILE_BASE-src-$BUILD_SUFFIX-$BUILD_PLATFORM
else
  BIN_FILE_NAME=$FILE_BASE-bin-$BUILD_SUFFIX
  SRC_FILE_NAME=$FILE_BASE-src-$BUILD_SUFFIX
fi
##### CREATE BUILD FILE AND SET UP VARIABLES ##################################


##### PREPARE SOURCE RELEASE ##########################################################
(cd .. && cp -r sedna $FILE_BASE && prepare_source) || failwith "Failed to prepare source distribution"
##### PREPARE SOURCE RELEASE ##########################################################

##### MAKE ####################################################################
STATIC_SYS_LIBS=OFF
if test "$STATIC_LIBS"x = "true"x; then
  STATIC_SYS_LIBS=ON
fi
ENABLE_DTSEARCH=$BUILD_DTSEARCH

# configure
mkdir -p ../$FILE_BASE.build
pushd ../$FILE_BASE.build > /dev/null 2>&1

# determine generator name
if test "$OS_TYPE" "=" "nix"; then
    CMAKE_GENERATOR="Unix Makefiles"
else
    CMAKE_GENERATOR="NMake Makefiles"
fi

cmake -G "$CMAKE_GENERATOR" -D CMAKE_BUILD_TYPE=Release -D CMAKE_INSTALL_PREFIX=$SEDNA_INSTALL/sedna -D SQL_CONNECTION=$SQL_CONNECTION -D STATIC_SYS_LIBS=$STATIC_SYS_LIBS -D ENABLE_DTSEARCH=$ENABLE_DTSEARCH -D MAKE_DOC=ON -D JAVA_DRIVER=ON ../$FILE_BASE || failwith "cmake failed"

rm -fr $SEDNA_INSTALL/sedna
$MAKE_COMMAND install || failwith "make (install) failed"
popd
rm -rf ../$FILE_BASE.build

(cd .. && prepare_post_source) || failwith "Failed to prepare source distribution (post-phase)"

##### MAKE ####################################################################

##### SOURCE RELEASE #################################################################
(cd .. &&
    (tar cvf - $FILE_BASE | gzip 1>$SRC_FILE_NAME.$SRC_EXT) &&
    rm -rf $FILE_BASE &&
    mv $SRC_FILE_NAME.$SRC_EXT $SEDNA_INSTALL) || failwith "Failed to create source distribution"
##### SOURCE RELEASE #################################################################

##### BINARY RELEASE #################################################################
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
##### BINARY RELEASE #################################################################
