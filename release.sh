#!/bin/bash

# File:  release.sh
# Copyright (C) 2010 ISP RAS
# The Institute for System Programming of the Russian Academy of Sciences

# This is script for making release of the Sedna system.
# Unfortunately, it depends on the environment of MODIS group and
# may not work in other places without modification

BUILD_MACHINE=modis.ispras.ru


failwith() {
  echo "Error: $*" 1>&2
  exit 1
}

##############################################################################
# Command line parsing
##############################################################################
usage() {
    echo "Usage"
    echo "    $0" " local          - build local release"
    echo "    $0" " release        - build release"
    echo "    $0" " release-dts    - build dtSearch release"
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
        release-dts)
            BUILD_TYPE=release
            BUILD_DTSEARCH=ON
            VERSION_SUFFIX=dt
            ;;
        *)
            usage;;
    esac
fi


##############################################################################
# Check environment
##############################################################################
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


##############################################################################
# Adjust configuration.
# See sedna-tests/src/night/config.sh for example of configuration file.
##############################################################################
SEDNA_VERSION=`cat ver` || failwith "Cannot read ver file"
BUILD_FILE=build-$SEDNA_VERSION
MD5_EXT=md5

if [ ! -r ../config.sh ]; then
    failwith "Could not find configuration file (config.sh)"
else
    . ../config.sh
fi

if test "$STATIC_LIBS"x = "true"x; then
  BUILD_SUFFIX=$BUILD_SUFFIX-static
fi

if test "$STATIC_LIBS"x = "true"x; then
  STATIC_SYS_LIBS=ON
else
  STATIC_SYS_LIBS=OFF
fi

if test "$SQL_CONNECTION"x = "true"x; then
  SQL_CONNECTION=ON
else
  SQL_CONNECTION=OFF
fi

if test "$OS_TYPE"x = "nix"x; then
    CMAKE_GENERATOR="Unix Makefiles"
else
    CMAKE_GENERATOR="NMake Makefiles"
fi

ENABLE_DTSEARCH=$BUILD_DTSEARCH


##############################################################################
# Helpers to create binaries and sources builds
##############################################################################
exclude_files() {
  for exclude_file in `cat $FILE_BASE/exclude_files`; do 
    rm -rf $FILE_BASE/$exclude_file || return 1
  done
}

prepare_source() {
    echo "Preparing sources ..."
    rm -rf $FILE_BASE/libs/bin
    OLDDIR="`pwd`" &&

    cd $FILE_BASE/kernel/tr/xqp &&
    chmod +x release.sh &&
    ./release.sh &&
    cd "$OLDDIR" &&
    cd $FILE_BASE/kernel/tr/ft/query &&
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


get_build_file() {
    scp sedna@$BUILD_MACHINE:build/$BUILD_FILE . || failwith "Cannot get build file to the build machine"
    BUILD=`cat $BUILD_FILE` || failwith "Cannot read build_file"
    rm -f $BUILD_FILE || failwith "Cannot remove build_file"
}


#script for uploading results of build to the BUILD_MACHINE
#parameters: binary_file_name source_file_name
#requirements: current directory must be $SEDNA_INSTALL
put_results_to_build_machine() {
    scp $1 sedna@$BUILD_MACHINE:build || failwith "Cannot put $1 to the build machine"
    scp $2 sedna@$BUILD_MACHINE:build || failwith "Cannot put $2 to the build machine"
    if test $# -ge 3; then scp $3 sedna@$BUILD_MACHINE:build || failwith "Cannot put $3 to the build machine"; fi
    if test $# -eq 4; then scp $4 sedna@$BUILD_MACHINE:build || failwith "Cannot put $4 to the build machine"; fi
}


##############################################################################
# Create build file and calculate file names
##############################################################################
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


##############################################################################
# Make and install Sedna
##############################################################################

(cd .. && cp -r sedna $FILE_BASE && prepare_source) || failwith "Failed to prepare source distribution"

mkdir -p ../$FILE_BASE.build
pushd ../$FILE_BASE.build > /dev/null 2>&1

cmake -G "$CMAKE_GENERATOR" -D CMAKE_BUILD_TYPE=Release -D CMAKE_INSTALL_PREFIX=$SEDNA_INSTALL/sedna -D SQL_CONNECTION=$SQL_CONNECTION -D STATIC_SYS_LIBS=$STATIC_SYS_LIBS -D ENABLE_DTSEARCH=$ENABLE_DTSEARCH -D MAKE_DOC=ON -D JAVA_DRIVER=ON ../$FILE_BASE || failwith "cmake failed"

rm -fr $SEDNA_INSTALL/sedna
$MAKE_COMMAND install || failwith "make (install) failed"
popd
rm -rf ../$FILE_BASE.build

(cd .. && prepare_post_source) || failwith "Failed to prepare source distribution (post-phase)"


##############################################################################
# Create source build
##############################################################################

(cd .. &&
    (tar cvf - $FILE_BASE | gzip 1>$SRC_FILE_NAME.$SRC_EXT) &&
    rm -rf $FILE_BASE &&
    mv $SRC_FILE_NAME.$SRC_EXT $SEDNA_INSTALL) || failwith "Failed to create source distribution"


##############################################################################
# Create binary build, calculate md5 and put builds to the $BUILD_MACHINE
##############################################################################

if test "$OS_TYPE" "=" "nix"; then 
    cp scripts/linux-install.sh $SEDNA_INSTALL || failwith "Cannot copy scripts/linux-install.sh"
fi

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
