#!/bin/sh

# File:  release.sh
# Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)


# This is script for making release of the Sedna system.
# Unfortunately, it depends on the environment of MODIS group and
# will not work in other places without modification


failwith() {
  echo "Error: $*" 1>&2
  exit 1
}

##### SCRIPT USAGE ############################################################
usage() {
    echo "Usage"
    echo "    $0" " release        - build release"
    echo "    $0" " release_ftp    - build release and put it to modis.ispras.ru"
    echo
    failwith "Wrong arguments"
}


if test $# -ne 1; then
    usage;
else
   if test $1 "!=" "release" -a $1 "!=" "release_ftp"; then
       usage;
   fi
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



export OS=`uname` || failwith "Cannot mine operating system name"
export SEDNA_VERSION=`cat ver` || failwith "Cannot read ver file"

export BUILD_FILE=build-$SEDNA_VERSION
export BUILD_STATE_FILE=build-state-$SEDNA_VERSION

if test "$OS" "=" "Linux"
then
export OS_DEP_BUILD_STATE=L
export BUILD_SUFFIX=linux
export DISTR_EXT=sh
export SRC_EXT=tar.gz
else
export OS_DEP_BUILD_STATE=W
export BUILD_SUFFIX=win
export DISTR_EXT=tar.gz
export SRC_EXT=tar.gz
fi

EXCLUDE_FROM_SOURCE_DISTR="--exclude exclude_files --exclude copyright.sh --exclude linecount.sh --exclude release.sh --exclude _darcs --exclude coverage.pl"
for exclude_file in libs/AntlrMSVC60.mak libs/Makefile.antlr libs/Makefile.dlg libs/Makefile.sor libs/Makefile.sor-lib libs/chicken.mak libs/expat_static.mak; do 
	EXCLUDE_FROM_SOURCE_DISTR="$EXCLUDE_FROM_SOURCE_DISTR --exclude $exclude_file"
done

prepare_windows_source() {
    rm -rf $FILE_BASE/libs/*.tar.gz &&
    rm -rf $FILE_BASE/libs/pcre/*.a &&
    make -C $FILE_BASE/libs &&
    rm -rf $FILE_BASE/libs/bin &&
    rm -rf $FILE_BASE/libs/src &&
    mv $FILE_BASE/libs/Makefile $FILE_BASE/libs/Makefile.orig &&
    sed -e 's/\(build_\w\+\)=yes/\1=no/' $FILE_BASE/libs/Makefile.orig > $FILE_BASE/libs/Makefile &&
    rm -f $FILE_BASE/libs/Makefile.orig
}

prepare_linux_source() {
    rm -rf $FILE_BASE/libs/pccts	&&
    rm -rf $FILE_BASE/libs/pg		&&
    rm -rf $FILE_BASE/libs/expat	&&
    rm -rf $FILE_BASE/libs/chicken	&&
    rm -rf $FILE_BASE/libs/pcre/*.a		&&
    rm -rf $FILE_BASE/libs/pcre/*.o		&&
    rm -rf $FILE_BASE/libs/pcre/*.obj	&&
    rm -rf $FILE_BASE/libs/pcre/*.lib	&&
    mv $FILE_BASE/libs/Makefile $FILE_BASE/libs/Makefile.orig &&
    sed -e 's/\(build_\w\+\)=no/\1=yes/' $FILE_BASE/libs/Makefile.orig > $FILE_BASE/libs/Makefile &&
    rm -f $FILE_BASE/libs/Makefile.orig &&
    rm -rf $FILE_BASE/libs/bin
}

create_sed_script()
{
    cat > $SEDSCRIPT <<EEE
s/^ACTIVE_CONFIGURATION[ ]\+\?=[A-Za-z0-9 ]\+$/ACTIVE_CONFIGURATION = Release/
s/^AUTH_SWITCH[ ]\+\?=[A-Za-z0-9 ]\+$/AUTH_SWITCH = 1/
s/^DOCUMENTATION[ ]\+\?=[A-Za-z0-9 ]\+$/DOCUMENTATION = 1/
s/^EL_DEBUG[ ]\+\?=[A-Za-z0-9 ]\+$/EL_DEBUG = 0/
s/^JAVA_DRIVER[ ]\+\?=[A-Za-z0-9 ]\+$/JAVA_DRIVER = 1/
s/^SQL_CONNECTION[ ]\+\?=[A-Za-z0-9 ]\+$/SQL_CONNECTION = 0/
s/^STATIC_SYS_LIBS[ ]\+\?=[A-Za-z0-9 ]\+$/STATIC_SYS_LIBS = 0/
s/^SE_ENABLE_GCOV[ ]\+\?=[A-Za-z0-9 ]\+$/SE_ENABLE_GCOV = 0/
s/^ENABLE_DTSEARCH[ ]\+\?=[A-Za-z0-9 ]\+$/ENABLE_DTSEARCH = 0/
EEE
}

prepare_source() {
    if test "$OS" "=" "Linux"; then
	prepare_linux_source
    else
	prepare_windows_source
    fi
    mv $FILE_BASE/Makefile.include $FILE_BASE/Makefile.include.orig &&
    SEDSCRIPT=Makefile.include.sed &&
    create_sed_script &&
    sed -f $SEDSCRIPT $FILE_BASE/Makefile.include.orig > $FILE_BASE/Makefile.include &&
    rm -f $FILE_BASE/Makefile.include.orig
}


#script for downloading build-number-file and build-state-file
get_build_file() {
    # get build_file and build_state_file
    echo "open seine.ispras.ru" > ftpscript.txt &&
    echo "anonymous" >> ftpscript.txt &&
    echo "password" >> ftpscript.txt &&
    echo "cd build" >> ftpscript.txt &&
    echo "get $BUILD_FILE" >> ftpscript.txt &&
    echo "get $BUILD_STATE_FILE" >> ftpscript.txt &&
    echo "close" >> ftpscript.txt &&
    echo "quit" >> ftpscript.txt || failwith "Cannot write to ftpscript.txt"

    if test "$OS" "=" "Linux"; then
        ncftp <ftpscript.txt
    else
        ftp -s:ftpscript.txt
    fi || failwith "Cannot get build_file or build_state_file"

    BUILD=`cat $BUILD_FILE` || failwith "Cannot read build_file"
    STATE=`cat $BUILD_STATE_FILE` || failwith "Cannot read build_state_file"

    rm -f ftpscript.txt || failwith "Cannot remove ftpscript.txt"
    rm -f $BUILD_FILE || failwith "Cannot remove build_file"
    rm -f $BUILD_STATE_FILE || failwith "Cannot remove build_state_file"
}

#script for uploadind build-number-file and build-state-file
put_build_file() {
    echo $BUILD > $BUILD_FILE &&
    echo $STATE > $BUILD_STATE_FILE &&
    echo "open seine.ispras.ru" > ftpscript.txt &&
    echo "anonymous" >> ftpscript.txt &&
    echo "password" >> ftpscript.txt &&
    echo "cd build" >> ftpscript.txt &&
    echo "put $BUILD_FILE" >> ftpscript.txt &&
    echo "put $BUILD_STATE_FILE" >> ftpscript.txt &&
    echo "close" >> ftpscript.txt &&
    echo "quit" >> ftpscript.txt || failwith "Cannot write to ftpscript.txt"

    if test "$OS" "=" "Linux"; then
        ncftp <ftpscript.txt
    else
        ftp -s:ftpscript.txt
    fi || failwith "Cannot upload build_file or build_state_file"

    rm -f ftpscript.txt || failwith "Cannot remove ftpscript.txt"
    rm -f $BUILD_FILE || failwith "Cannot remove build_file"
    rm -f $BUILD_STATE_FILE || failwith "Cannot remove build_state_file"
}

#script for uploading results of build to seine
#parameters: binary_file_name source_file_name
#requirements: current directory must be $SEDNA_INSTALL
put_results_to_seine() {
	echo "open seine.ispras.ru" > ftpscript.txt &&
	echo "anonymous" >> ftpscript.txt &&
	echo "password" >> ftpscript.txt &&
	echo "binary" >> ftpscript.txt &&
	echo "cd build" >> ftpscript.txt &&
	echo "put $1" >> ftpscript.txt &&
	echo "put $2" >> ftpscript.txt &&
	echo "close" >> ftpscript.txt &&
	echo "quit" >> ftpscript.txt || failwith "Cannot write to ftpscript.txt"

    if test "$OS" "=" "Linux"; then
        ncftp <ftpscript.txt
    else
        ftp -s:ftpscript.txt
    fi || failwith "Cannot upload build results to seine"

    rm -f ftpscript.txt || failwith "Cannot remove build_file"
}

#script for downloading user name to modis.ispras.ru
get_modis_ftp_uname() {
    echo "open seine.ispras.ru" > ftpscript.txt &&
    echo "anonymous" >> ftpscript.txt &&
    echo "password" >> ftpscript.txt &&
    echo "cd build" >> ftpscript.txt &&
    echo "get modisftpusername.txt" >> ftpscript.txt &&
    echo "close" >> ftpscript.txt &&
    echo "quit" >> ftpscript.txt || failwith "Cannot write to ftpscript.txt"

    if test "$OS" "=" "Linux"; then
        ncftp <ftpscript.txt
    else
        ftp -s:ftpscript.txt
    fi || failwith "Cannot get modisftpusername.txt"

    FTP_UNAME=`cat modisftpusername.txt` || failwith "Cannot read modisftpusername.txt"

    rm -f ftpscript.txt || failwith "Cannot remove ftpscript.txt"
    rm -f modisftpusername.txt || failwith "Cannot remove modisftpusername.txt"
}

#script for downloading password to modis.ispras.ru
get_modis_ftp_passw() {
    echo "open seine.ispras.ru" > ftpscript.txt &&
    echo "anonymous" >> ftpscript.txt &&
    echo "password" >> ftpscript.txt &&
    echo "cd build" >> ftpscript.txt &&
    echo "get modisftppassword.txt" >> ftpscript.txt &&
    echo "close" >> ftpscript.txt &&
    echo "quit" >> ftpscript.txt || failwith "Cannot write to ftpscript.txt"

    if test "$OS" "=" "Linux"; then
        ncftp <ftpscript.txt
    else
        ftp -s:ftpscript.txt
    fi || failwith "Cannot get modisftppassword.txt"

    FTP_PASSW=`cat modisftppassword.txt` || failwith "Cannot read modisftppassword.txt"

    rm -f ftpscript.txt || failwith "Cannot remove ftpscript.txt"
    rm -f modisftppassword.txt || failwith "Cannot remove modisftppassword.txt"
}

#script for uploading results of build to modis.ispras.ru
#parameters: binary_file_name source_file_name
#requirements: current directory must be $SEDNA_INSTALL
put_results_to_modis() {
    if test "$OS" "=" "Linux"; then 
        echo "" > ftpscript.txt; 
    else 
        echo "open modis.ispras.ru" > ftpscript.txt && 
        echo "$FTP_UNAME" >> ftpscript.txt && 
        echo "$FTP_PASSW" >> ftpscript.txt;
    fi &&
    echo "binary" >> ftpscript.txt &&
    echo "put $1" >> ftpscript.txt &&
    echo "put $2" >> ftpscript.txt &&
    echo "close" >> ftpscript.txt &&
    echo "quit" >> ftpscript.txt || failwith "Cannot write to ftpscript.txt"

    if test "$OS" "=" "Linux"; then 
	    ncftp -u $FTP_UNAME -p $FTP_PASSW modis.ispras.ru <ftpscript.txt
    else 
	    ftp -s:ftpscript.txt
    fi || failwith "Cannot upload build results to modis.ispras.ru"

    rm -f ftpscript.txt || failwith "Cannot remove ftpscript.txt"
}



##### CREATE BUILD FILE AND SET UP VARIABLES ##################################
get_build_file

if test $STATE "=" LW -o $STATE "=" $OS_DEP_BUILD_STATE; then 
   BUILD=`expr $BUILD + 1` || failwith "Cannot increment BUILD variable"
   STATE=$OS_DEP_BUILD_STATE
else 
   STATE=LW
fi

echo $BUILD > build || failwith "Cannot write to build file"

FILE_BASE=sedna-$SEDNA_VERSION.$BUILD
BIN_FILE_NAME=$FILE_BASE-bin-$BUILD_SUFFIX
SRC_FILE_NAME=$FILE_BASE-src-$BUILD_SUFFIX
##### CREATE BUILD FILE AND SET UP VARIABLES ##################################



##### MAKE CLEAN ##############################################################
make clean || failwith "make clean failed"
##### MAKE CLEAN ##############################################################




##### SOURCE RELEASE ##########################################################
(cd .. &&
 cp -r sedna $FILE_BASE &&
 prepare_source &&
 tar cvfz $SRC_FILE_NAME.$SRC_EXT -h $EXCLUDE_FROM_SOURCE_DISTR $FILE_BASE &&
 rm -rf $FILE_BASE &&
 mv $SRC_FILE_NAME.$SRC_EXT $SEDNA_INSTALL) || 
                               failwith "Failed to create source distribution"
##### SOURCE RELEASE ##########################################################



##### MAKE ####################################################################
export ACTIVE_CONFIGURATION=Release
export AUTH_SWITCH=1
export DOCUMENTATION=1
export EL_DEBUG=0
export JAVA_DRIVER=1
if test "$OS" "=" "Linux"
then
export SQL_CONNECTION=0
else
export SQL_CONNECTION=1
fi
export STATIC_SYS_LIBS=1
make || failwith "make failed"
##### MAKE ####################################################################



##### MAKE INSTALL ############################################################
rm -fr $SEDNA_INSTALL/sedna
make grouped_install || failwith "make install failed"
##### MAKE INSTALL ############################################################



##### RELEASE #################################################################
if test "$OS" "=" "Linux"; then 
    cp scripts/linux-install.sh $SEDNA_INSTALL; 
fi || failwith "Cannot copy scripts/linux-install.sh"

(cd $SEDNA_INSTALL &&
 tar cvfz $BIN_FILE_NAME.tar.gz sedna || failwith "Cannot create archive of binaries"

 if test "$OS" "=" "Linux"; then 
    (SUM=`cksum $BIN_FILE_NAME.tar.gz` &&
     SUM=`set $SUM; echo $1` &&
     sed "s/PLACE_FOR_BINARY_SUM/$SUM/" linux-install.sh >$BIN_FILE_NAME.sh &&
     cat $BIN_FILE_NAME.tar.gz >>$BIN_FILE_NAME.sh &&
     chmod a+x $BIN_FILE_NAME.sh &&
     rm -f linux-install.sh);
 fi || failwith "Cannot create selfextracted binary package"
 put_results_to_seine $BIN_FILE_NAME.$DISTR_EXT $SRC_FILE_NAME.$SRC_EXT)
##### RELEASE #################################################################


put_build_file 


##### FTP #####################################################################
if test $1 "!=" "release_ftp"; then
    exit 0;
fi
get_modis_ftp_uname
get_modis_ftp_passw
(cd $SEDNA_INSTALL &&
 put_results_to_modis $BIN_FILE_NAME.$DISTR_EXT $SRC_FILE_NAME.$SRC_EXT)
##### FTP #####################################################################



