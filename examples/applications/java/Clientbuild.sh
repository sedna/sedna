#!/bin/sh
if [ -f ../../../driver/java/sednadriver.jar ];
then
javac -classpath .:../../../driver/java/sednadriver.jar Client.java
elif [ -f ../../../share/java/sednadriver.jar ];
then
javac -classpath .:../../../share/java/sednadriver.jar Client.java
elif [ -f /usr/share/java/sednadriver.jar ];
then
javac -classpath .:usr/share/java/sednadriver.jar Client.java
fi
