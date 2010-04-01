#!/bin/sh

# File:  linecount.sh
# Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)


# This is script for counting number of lines in the sources of Sedna project.
# To use it, just run it, or run "sh" with this file.

total=0
for x in `find -iname "*.c*" -o -iname "*.h" -o -iname "*.java" -o -iname "*.scm" -o -iname "*.g" -o -iname "*.sor"`; do
   echo "`wc -l $x`"
   z="`wc -l $x | sed s/"[a-zA-Z./][a-zA-Z0-9_./-]\+"// | sed s/\ \*//`"
   total=`expr $total + $z`
done

for x in `find libs -iname "*.c*" -o -iname "*.h" -o -iname "*.java" -o -iname "*.scm" -o -iname "*.g" -o -iname "*.sor"`; do
   echo "`wc -l $x`"
   z="`wc -l $x | sed s/"[a-zA-Z./][a-zA-Z0-9_./-]\+"// | sed s/\ \*//`"
   total=`expr $total - $z`
done


echo " "
echo "total number of lines in the project: $total"
echo " " 

