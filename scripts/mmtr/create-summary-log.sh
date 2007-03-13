#!/bin/bash


##########################################################################
# Define environment

DB_NAME=mt
DOC_NAME=mt
LOAD_QUERY=load.xquery
SUMMARY_QUERY=distinct-with-summary.xquery

BIN_GOV=se_gov
BIN_SM=se_sm
BIN_SMSD=se_smsd
BIN_STOP=se_stop
BIN_CDB=se_cdb
BIN_RC=se_rc
BIN_TRN=se_trn
BIN_TERM=se_term

##########################################################################


##########################################################################
# Useful functions
# Test if there is an error occurred
error_test () 
{
    if [[ $? -ne 0 ]]; then 
        popd >/dev/null
        exit 1
    fi
}

# Print usage hint and exit
usage_print ()
{
    echo
    echo "Usage: ./create-summary-log.sh  dump-file-to-proceed  path-to-Sedna"
    echo

    popd >/dev/null
    exit 1
}

# Shutdowns and deletes given database
sd_and_del_db () 
{
    $SEDNA_HOME/bin/$BIN_SMSD $1
    rm -r -f $SEDNA_DB/data/${1}_files
    rm -f $SEDNA_DB/cfg/${1}_cfg.xml
}
##########################################################################


# This allows starting this script correctly from any directory
pushd `dirname $0` >/dev/null


##########################################################################
# Parse command line and create config file

if [[ -z $1 ]]; then
    echo "Incorrect number of arguments passed to the script."
    usage_print
fi

index=1

if [[ $# -lt 2 ]]; then
    usage_print
else
   if [[ ! -d "$2" ]]; then 
       echo "Invalid path to Sedna '$2' passed to the script."
       usage_print
   else
       export SEDNA_HOME=$2
       export SEDNA_DIR=$2          
   fi
fi


###########################################################################
# Find path to Sedna database (used sednaconf.xml)

OS=`uname | sed 's/^CYGWIN_NT.*$/CYGWIN_NT/'`

echo "Platform detected: $OS."

if [[ $OS == "CYGWIN_NT" ]]; then
    SEDNA_DB=$SEDNA_HOME
else
    SEDNA_DB=/var/lib/sedna
fi

##########################################################################

echo 
echo "==================================================================="
echo "Running Sedna initialization:"
echo "==================================================================="
echo 

# Stop Sedna server
if [[ `$SEDNA_HOME/bin/$BIN_RC | grep GOV | grep running` ]]; then
	echo "Sedna is running. I will stop it because it is required for summary creating!"
	$SEDNA_HOME/bin/$BIN_STOP
	error_test
fi

# Start gov if it is not started yet
echo "TRYING TO START SEDNA: "
echo "---------------------  "
$SEDNA_HOME/bin/$BIN_GOV
error_test

echo

# Creates and runs the test database 
echo "DELETING THE TEST DATABASE (CALLED \"$DB_NAME\"):"
echo "-------------------------- "
sd_and_del_db $DB_NAME
echo
echo "CREATING AND STARTING NEW DATABASE:"
echo "---------------------------------- "
$SEDNA_HOME/bin/$BIN_CDB $DB_NAME 
error_test
echo
echo
$SEDNA_HOME/bin/$BIN_SM $DB_NAME
error_test

# Load memory tracking dump
echo
echo "==================================================================="
echo "Loading dump:"
echo "==================================================================="
echo

rm -f $LOAD_QUERY
echo "LOAD '$1' '$DOC_NAME'"   > $LOAD_QUERY

$SEDNA_HOME/bin/$BIN_TERM -file $LOAD_QUERY $DB_NAME
error_test

# Run query on loaded dump
echo
echo "==================================================================="
echo "Run summary query:"
echo "==================================================================="
echo

echo "Please wait ... "
$SEDNA_HOME/bin/$BIN_TERM -file $SUMMARY_QUERY $DB_NAME >log 2>error_log
echo

echo "DELETING TEST DATABASE AND STOPPING SEDNA:"
echo "----------------------------------------- "
sd_and_del_db $DB_NAME
$SEDNA_HOME/bin/$BIN_STOP

echo ""
echo "==================================================================="
echo "Complete"
echo "==================================================================="
echo ""

#Returns to the directory where this script was started from
popd >/dev/null
