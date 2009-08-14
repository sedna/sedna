#!/bin/sh

SEDNADRIVER=../../../driver/java/lib/sednadriver.jar

if [ -f $SEDNADRIVER ]; then
    javac -classpath .:$SEDNADRIVER Client
else
    echo "Could not locate sednadriver.jar"
fi
