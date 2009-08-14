#!/bin/sh

SEDNADRIVER=../../../driver/java/lib/sednadriver.jar

if [ -f $SEDNADRIVER ]; then
    java -classpath .:$SEDNADRIVER Client
else
    echo "Could not locate sednadriver.jar"
fi
