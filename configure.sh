#! /bin/sh

# common options
ENHANCE_TERM=Readline
EL_DEBUG=ON
MAKE_DOC=ON
JAVA_DRIVER=ON
CMAKE_BUILD_TYPE=Debug
DTSEARCH=ON
SQL_CONNECTION=ON

print_usage()
{
    echo "Usage: $(basename $0) [-nm] <path-to-build-sedna> ('nm' builds NMake files on Windows)"
    exit 1
}

if [ $# -lt 1 ]; then
    print_usage;
fi

SEDNA_SOURCE_DIR=$(cd $(dirname $0) && pwd)

if [ `uname | grep "CYGWIN"` ] ; then
   SEDNA_SOURCE_DIR=`cygpath -aw $SEDNA_SOURCE_DIR`
fi

if [ $# -gt 1 ]; then
    case "$1" in
    -nm)
            CMAKE_GENERATOR="NMake Makefiles"
            shift
            ;;
    *)
            print_usage
            ;;
    esac
fi

SEDNA_BUILD_DIR=$1

mkdir -p $SEDNA_BUILD_DIR

echo "Sedna sources in: $SEDNA_SOURCE_DIR"
echo "Configuring Sedna in: $SEDNA_BUILD_DIR"

if [ -z "$CMAKE_GENERATOR" ] ; then
    CMAKE_G=""
else
    CMAKE_G="-G$CMAKE_GENERATOR"
fi

pushd $SEDNA_BUILD_DIR > /dev/null 2>&1
cmake "$CMAKE_G" -DSQL_CONNECTION=$SQL_CONNECTION -DENABLE_DTSEARCH=$DTSEARCH -DENHANCE_TERM=$ENHANCE_TERM -DEL_DEBUG=$EL_DEBUG -DCMAKE_BUILD_TYPE=$CMAKE_BUILD_TYPE -DMAKE_DOC=$MAKE_DOC -DJAVA_DRIVER=$JAVA_DRIVER $SEDNA_SOURCE_DIR
CMAKE_RET=$?
popd  > /dev/null 2>&1

if [ $CMAKE_RET -ne 0 ] ; then
    echo "Sedna configuartion failed!!!"
    exit 1
fi
