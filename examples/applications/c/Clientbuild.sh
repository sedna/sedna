#!/bin/sh
if [ -f ../../../driver/c/libsedna.a ];
then
gcc -I../../../driver/c -oClient Client.c ../../../driver/c/libsedna.a
else
gcc -I../../../include -oClient Client.c ../../../lib/libsedna.a
fi
