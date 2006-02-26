#!/bin/sh
if [ -f ../../../driver/java/sednadriver.jar ];
then
java -classpath .:../../../driver/java/sednadriver.jar Client
elif [ -f ../../../share/java/sednadriver.jar ];
then
java -classpath .:../../../share/java/sednadriver.jar Client
elif [ -f /usr/share/java/sednadriver.jar ];
then
java -classpath .:usr/share/java/sednadriver.jar Client
fi

