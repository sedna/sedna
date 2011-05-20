#!/bin/bash

# common options
ENHANCE_TERM=Readline
EL_DEBUG=OFF
MAKE_DOC=ON
JAVA_DRIVER=ON
CMAKE_BUILD_TYPE=Debug
DTSEARCH=ON
SQL_CONNECTION=ON

error () {
    echo "$@" > /dev/stderr
}

usage () {
    if [ "$1" == "error" ]; then
        cmd=error
    else
        cmd=echo
    fi

    $cmd "usage: `basename $0` [--help] [--nmake] [--eclipse]"
    $cmd "       [--cmake=<path>] <path-to-build-sedna>"
    $cmd ""
    $cmd "       Options:"
    $cmd "           --help -- this help message"
    $cmd "           --nmake   -- builds NMake Makefiles"
    $cmd "           --eclipse  -- builds Eclipse CDT4 Project"
    $cmd "           --cmake=<path>  -- use specified cmake binary"

    if [ "$1" == "error" ]; then
        exit 1
    else
        exit 0
    fi
}

SEDNA_SOURCE_DIR=$(cd $(dirname $0) && pwd) # sedna source directory
SEDNA_BUILD_DIR="" # sedna build directory
CMAKE_GENERATOR="" # cmake generator to use
CMAKE_BIN="" # cmake executable to use

# need this if we run from cygwin since we use win-cmake there
if [ `uname | grep "CYGWIN"` ]; then
   SEDNA_SOURCE_DIR=`cygpath -aw $SEDNA_SOURCE_DIR`
fi

# parse options
while [ $# -gt 0 ]; do
    case $1 in
        --cmake*)
            # support both --cmake=<dir> and --cmake <dir> syntax
            if [ "$1" != "${1##--cmake=}" ]; then
                CMAKE_BIN=$(eval echo ${1##--cmake=}) # to expand ~
            else
                CMAKE_BIN=$2
                shift
            fi
            ;;
        # support '-nm' as legacy
        --nmake|-nm)
            CMAKE_GENERATOR="NMake Makefiles"
                ;;
        --eclipse)
            CMAKE_GENERATOR="Eclipse CDT4 - Unix Makefiles"
                ;;
        --help)
            usage
                ;;
        *)
            if [ -z "$SEDNA_BUILD_DIR" ]; then
                SEDNA_BUILD_DIR=$1
            else
                usage error
            fi
            ;;
    esac

    shift
done

if [ -z "$SEDNA_BUILD_DIR" ]; then
    usage error
fi

if [ -z "$CMAKE_BIN" ]; then
    CMAKE_BIN=`which cmake 2>/dev/null`
    if [ $? -ne 0 ]; then
        error "Cannot find cmake in PATH"
        exit 1
    fi
fi

if [ ! -e "$CMAKE_BIN" ]; then
    error "Cannot find cmake: $CMAKE_BIN"
    exit 1
fi

# make build directory
mkdir -p "$SEDNA_BUILD_DIR" > /dev/null 2>&1
if [ $? -ne 0 ]; then
    error "cannot create build directory: $SEDNA_BUILD_DIR"
    exit 1
fi

echo "Sedna sources in: $SEDNA_SOURCE_DIR"
echo "Configuring Sedna in: $SEDNA_BUILD_DIR"
echo "Using CMake: $CMAKE_BIN"

if [ -n "$CMAKE_GENERATOR" ]; then
    CMAKE_G="-G$CMAKE_GENERATOR"
fi

pushd "$SEDNA_BUILD_DIR" > /dev/null 2>&1
"$CMAKE_BIN" "$CMAKE_G" -DSQL_CONNECTION=$SQL_CONNECTION \
                        -DENABLE_DTSEARCH=$DTSEARCH \
                        -DENHANCE_TERM=$ENHANCE_TERM \
                        -DEL_DEBUG=$EL_DEBUG \
                        -DCMAKE_BUILD_TYPE=$CMAKE_BUILD_TYPE \
                        -DMAKE_DOC=$MAKE_DOC \
                        -DJAVA_DRIVER=$JAVA_DRIVER \
                        "$SEDNA_SOURCE_DIR"
CMAKE_RET=$?
popd  > /dev/null 2>&1

if [ $CMAKE_RET -ne 0 ]; then
    error "Sedna configuration failed!!!"
    exit 1
fi
