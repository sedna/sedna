#!/bin/sh

# File:  linux-install.sh
# Copyright (C) 2007 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)


# This is self-extracting shell script for Sedna distribution.
# To use it, just run it, or run "sh" with this file.

DISTNAME="Sedna XML DBMS"
TARGET=sedna
DEFAULT_TREE_NAME=sedna
BINSUM="PLACE_FOR_BINARY_SUM"

failwith() {
  echo "Error: $*" 1>&2
  exit 1
}
exithandler() {
  failwith "Abort."
}

trap exithandler 2 3 9 15

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

link() { # args are source, target, where we are
  "$rm" -f "$2"      || failwith "Could not remove \"$2\" in \"$3\"."
  "$ln" -s "$1" "$2" || failwith "Could not link \"$2\" in \"$3\"."
}

lookfor rm
lookfor ls
lookfor ln
lookfor tail
lookfor cksum
lookfor tar
lookfor gunzip
lookfor mkdir
lookfor basename
lookfor dirname
lookfor awk
lookfor chown
lookfor chmod
lookfor mv

BINSTARTLINE=`awk '/^__ARCHIVE_FOLLOWS__/ { print NR + 1; exit 0; }' $0`

# Need this to make new `tail' respect old-style command-line arguments.  Can't
# use `tail -n #' because some old tails won't know what to do with that.
_POSIX2_VERSION=199209
export _POSIX2_VERSION

origpwd="`pwd`"

#id -u doesn't work under Solaris!
#if test ! `id -u` "=" "0"; then
#  printf "NOTICE: $DISTNAME is system software that requires deep integration with your operating system. "
#  printf "To obtain best performance Sedna components use sophisticated memory management techniques and "
#  printf "low-level disk access operations, which require additional privelegies."
#  echo ""
#  echo ""
#  failwith "You have to be root to perform installation operation."
#fi

echo "This program will extract and install $DISTNAME."
echo ""
echo "Where do you want to install the \"$TARGET\" directory tree?"
echo "  1 - /usr/local/$TARGET"
echo "  2 - /opt/$TARGET"
echo "  3 - \$HOME/$TARGET ($HOME/$TARGET) [default]"
echo "  4 - ./$TARGET (here)"
echo "  Or enter a different directory to install in."
printf "> "
read where
case "$where" in
  "1" ) where="/usr/local" ;;
  "2" ) where="/opt" ;;
  "" | "3" ) where="$HOME" ;;
  "4" | "." ) where="`pwd`" ;;
  "/"* )
    TARGET="`\"$basename\" \"$where\"`"
    where="`\"$dirname\" \"$where\"`"
    ;;
  * )
    TARGET="`\"$basename\" \"$where\"`"
    where="`\"$dirname\" \"$where\"`"
    if test -d "$where"; then cd "$where"; where=`pwd`; cd "$origpwd"
    else failwith "The directory \"$where\" does not exist."; fi
    ;;
esac

if test ! -d "$where"; then
  failwith "The directory \"$where\" does not exist."
fi
if test ! -w "$where"; then
  failwith "Cannot write to \"$where\"."
fi

printf "Checking the integrity of the binary archive... "
SUM="`\"$tail\" +\"$BINSTARTLINE\" \"$0\" | \"$cksum\"`" \
  || failwith "Problems running cksum."
SUM="`set $SUM; echo $1`"
test "$BINSUM" = "$SUM" || failwith "Bad CRC checksum."
echo "ok"

if test -d "$where/$TARGET" || test -f "$where/$TARGET"; then
  printf "\"$where/$TARGET\" exists, delete? "
  read yesno
  case "$yesno" in
    [yY]*)
      printf "Deleting old \"$where/$TARGET\"... "
      "$rm" -rf "$where/$TARGET" \
      || failwith "Could not delete \"$where/$TARGET\"."
      echo "done"
      ;;
    *) failwith "Aborting because \"$where/$TARGET\" exists." ;;
  esac
fi

printf "Unpacking into \"$where/$TARGET\"... "
"$mkdir" "$where/$TARGET" || failwith "Could not create \"$where/$TARGET\"."
"$tail" +"$BINSTARTLINE" "$0" | "$gunzip" -c \
| { cd "$where/$TARGET"
    "$tar" xf - \
    || failwith "Problems during unpacking of binary archive."
  }
cd "$where/$TARGET"
"$mv" "$DEFAULT_TREE_NAME"/* . || failwith "Could not move directory tree."
"$rm" -rf "$DEFAULT_TREE_NAME"
test -d "bin" \
|| failwith "Unpack failed (could not find \"$where/$TARGET/bin\")."
echo "done"

printf "Running the Sedna installer... "
#"$chown" -R root:root "$where/$TARGET" || failwith "chown"
"$chmod" u=rwx,g=rwx,o=rx "$where/$TARGET/bin/se_cdb"       || failwith "chmod"
"$chmod" u=rwx,g=rwx,o=rx "$where/$TARGET/bin/se_ddb"       || failwith "chmod"
"$chmod" u=rwx,g=rwx,o=rx "$where/$TARGET/bin/se_gov"       || failwith "chmod"
"$chmod" u=rwx,g=rwx,o=rx "$where/$TARGET/bin/se_rc"        || failwith "chmod"
"$chmod" u=rwx,g=rwx,o=rx "$where/$TARGET/bin/se_sm"        || failwith "chmod"
"$chmod" u=rwx,g=rwx,o=rx "$where/$TARGET/bin/se_smsd"      || failwith "chmod"
"$chmod" u=rwx,g=rwx,o=rx "$where/$TARGET/bin/se_stop"      || failwith "chmod"
"$chmod" u=rwx,g=rwx,o=rx "$where/$TARGET/bin/se_term"      || failwith "chmod"
"$chmod" u=rwx,g=rwx,o=rx "$where/$TARGET/bin/se_trn"       || failwith "chmod"
"$chmod" u=rwx,g=rwx,o=rx "$where/$TARGET/bin/se_rcv"       || failwith "chmod"
echo "done"

cd "$where"

printf "Creating sedna configuration file... "

cat > $TARGET/etc/sednaconf.xml <<EOF
<?xml version="1.0" standalone="yes"?>

<sednaconf>
  <!-- Path to database files -->
  <sedna_data>`pwd`/$TARGET</sedna_data>
  <!-- Left bounf of range for identifiers of system resources -->
  <os_primitives_id_min_bound>1500</os_primitives_id_min_bound>
  <!-- Sedna server listening port number -->
  <listener_port>5050</listener_port>
  <!-- Sedna server ping port number -->
  <ping_port>5151</ping_port>
  <!-- Event logging level 
       0 -           event logging is off
       1 -           log only fatal errors when system goes down
       2 -           log all errors/warnings (e.g. errors in queries)
       3 - (default) server operational messages. This is 
                     quite complete log of what is going on in system.
       4 -           log everything including internal debug messages
  -->
  <event_log_level>3</event_log_level>
</sednaconf>
EOF
rm $TARGET/etc/sednaconf.xml.sample

echo "done"

if test -d "bin"; then
  echo "Do you want to install new system links within the bin subdirectory of "
  echo "\"$where\", "
  echo "possibly overriding existing links for the programs se_cdb, se_ddb, se_gov, "
  printf "se_rc, se_sm, se_smsd, se_stop, se_term, se_trn, se_rcv? "
  read yesno
  case "$yesno" in
    [yY]* ) sysdir="$where" ;;
    * ) sysdir="" ;;
  esac
else
  cd "$origpwd"
  echo "If you want to install new system links within the bin "
  echo "subdirectory of a common directory prefix (for "
  echo "example, \"/usr/local\") for the programs se_cdb, se_ddb, se_gov, se_rc, "
  echo "se_sm, se_smsd, se_serv, se_term, se_trn, se_rcv then enter the prefix you "
  echo "want to use."
  printf "(default: skip links) > "
  read sysdir
  if test ! "x$sysdir" = "x"; then
    if test ! -d "$sysdir"; then
      echo "Directory \"$sysdir\" does not exist, skipping links."
      sysdir=""
    elif test ! -w "$sysdir"; then
      echo "Directory \"$sysdir\" is not writable, skipping links."
      sysdir=""
    else
      cd "$sysdir"
      sysdir="`pwd`"
    fi
  fi
fi


if test ! "x$sysdir" = "x"; then
  # binaries
  cd "$sysdir"
  if test -d "bin" && test -w "bin"; then
    echo "Installing links in \"$sysdir/bin\"..."
    printsep="  "
    cd "bin"
    for x in "se_cdb" "se_ddb" "se_gov" "se_rc" "se_sm" "se_smsd" \
             "se_stop" "se_term" "se_trn" "se_rcv"; do
      if test -x "$where/$TARGET/bin/$x"; then
        printf "${printsep}$x"
        printsep=", "
        link "$where/$TARGET/bin/$x" "$x" "$sysdir/bin"
      fi
    done
    echo ""
    echo "Done"
  else
    echo "Skipping \"$sysdir/bin\" (does not exist or not writable)."
  fi
fi

echo ""
echo "ALL DONE"
echo ""
echo "==========================================="
echo "Note: The operating system user that is going to run Sedna must have "
echo "r-w-x permissions for the following Sedna directories: "
echo ""
echo "$where/$TARGET/data"
echo "$where/$TARGET/cfg"
echo ""
echo "To grant the necessary permisions to the user on Linux "
echo "you can use the following command (suppose, <sedna-user> is the user "
echo "that is going to run Sedna): "
echo ""
echo "chown <sedna-user> cfg data"
echo "==========================================="
echo ""
echo "See file $where/$TARGET/README to get started"

exit

__ARCHIVE_FOLLOWS__
