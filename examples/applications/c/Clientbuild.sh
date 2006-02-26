#!/bin/sh
if [ -f ../../../driver/c/libsedna.a ];
then
gcc -I../../../driver/c -osocket-test socket-test.cpp ../../../driver/c/libsedna.a
else
gcc -I../../../include -osocket-test socket-test.cpp ../../../lib/libsedna.a
fi
