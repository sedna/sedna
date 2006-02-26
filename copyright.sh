#!/bin/sh

export TMP_FILE=_tmp_.src

export EX_PARA="! (  -name "argtable.h" -or -name "argtable.c" -or -name "counted_ptr.h" -or -path "./kernel/tr/sqp/tools/env" -prune -or -path "./driver/scheme/libs" -prune -or -path "./libs" -prune )"



#####################
# Copyright message #
#####################
export LSTRING='Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)'


#######################
# processing C++ files#
#######################
echo "--------------------"
echo "processing C++ files"
echo "--------------------"
for i in `find . $EX_PARA -and \( -name "*.cpp" -or -name "*.h" -or -name "*.c" \)`
do  
 if (!(grep -i "$LSTRING" $i > out )) 
 then (
   echo "processing" $i
   echo "/*" > $TMP_FILE
   echo " * File: " `echo $i | sed s/".\/\([a-z0-9A-Z_.\-]\+\/\)*"//` >> $TMP_FILE
   echo "$LSTRING" | sed 's/^/\ *\ /' >> $TMP_FILE
   echo " */" >> $TMP_FILE
   echo >> $TMP_FILE 
   cat $i >> $TMP_FILE
   mv -f $TMP_FILE $i
   echo -e "processing complete\n"
 )
 else (
   echo $i "already contains copyright information"
   echo  
 )
 fi
done



########################
# processing Java files#
########################
echo "---------------------"
echo "processing Java files"
echo "---------------------"
for i in `find . $EX_PARA -and \( -name "*.java" \)`
do  
 if (!(grep -i "$LSTRING" $i > out )) 
 then (
   echo "processing" $i
   echo "/*" > $TMP_FILE
   echo " * File: " `echo $i | sed s/".\/\([a-z0-9A-Z_.\-]\+\/\)*"//` >> $TMP_FILE
   echo "$LSTRING" | sed 's/^/\ *\ /' >> $TMP_FILE
   echo " */" >> $TMP_FILE
   echo >> $TMP_FILE 
   cat $i >> $TMP_FILE
   mv -f $TMP_FILE $i
   echo -e "processing complete\n"
 )
 else (
   echo $i "already contains copyright information" 
   echo 
 )
 fi
done



##########################
# processing Scheme files#
##########################
echo "-----------------------"
echo "processing Scheme files"
echo "-----------------------"
for i in `find . $EX_PARA -and \( -name "*.scm" -or -name "*.ss" \)`
do  
 if (!(grep -i "$LSTRING" $i > out )) 
 then (
   echo "processing" $i
   echo > $TMP_FILE
   echo "; File: " `echo $i | sed s/".\/\([a-z0-9A-Z_.\-]\+\/\)*"//` >> $TMP_FILE
   echo "$LSTRING" | sed 's/^/;\ /' >> $TMP_FILE
   echo >> $TMP_FILE 
   cat $i >> $TMP_FILE
   mv -f $TMP_FILE $i
   echo -e "processing complete\n"
 )
 else (
   echo $i "already contains copyright information" 
   echo 
 )
 fi
done


##########################
# processing Tex files#
##########################
echo "-----------------------"
echo "processing Tex files"
echo "-----------------------"
for i in `find . $EX_PARA -and \( -name "*.tex" \)`
do  
 if (!(grep -i "$LSTRING" $i > out )) 
 then (
   echo "processing" $i
   echo > $TMP_FILE
   echo "% File: " `echo $i | sed s/".\/\([a-z0-9A-Z_.\-]\+\/\)*"//` >> $TMP_FILE
   echo "$LSTRING" | sed 's/^/%\ /' >> $TMP_FILE
   echo >> $TMP_FILE 
   cat $i >> $TMP_FILE
   mv -f $TMP_FILE $i
   echo -e "processing complete\n"
 )
 else (
   echo $i "already contains copyright information" 
   echo 
 )
 fi
done



########################
# processing Bash files#
########################
echo "---------------------"
echo "processing Bash files"
echo "---------------------"
for i in `find . $EX_PARA -and \( -name "*.sh" \)`
do  
 if (!(grep -i "$LSTRING" $i > out )) 
 then (
   echo "processing" $i
   #echo p1
   export FLINE=`head -n 1 $i`
   #echo p2
   if (!(echo "$FLINE" | grep "\#\!" >out)) 
   then
     #echo p3_then
     #if file don't contain information about shell
     echo "WARNING: file doesn't contain shell processor information"
     echo "inserting shell information..."
     echo "#!/bin/sh" > $TMP_FILE
     export TAIL_LINE="+1"
   else
     #echo p3_else
     #if file contain information about shell
     echo "$FLINE" > $TMP_FILE
     export TAIL_LINE="+2"
   fi
   #old version - echo `head -n 1 $i` > $TMP_FILE
   echo >> $TMP_FILE
   echo "# File: " `echo $i | sed s/".\/\([a-z0-9A-Z_.\-]\+\/\)*"//` >> $TMP_FILE
   echo "$LSTRING" | sed 's/^/#\ /' >> $TMP_FILE
   echo >> $TMP_FILE 
   tail -n $TAIL_LINE $i >> $TMP_FILE
   mv -f $TMP_FILE $i
   echo -e "processing complete\n"
 )
 else (
   echo $i "already contains copyright information" 
   echo 
 )
 fi
done

rm out




