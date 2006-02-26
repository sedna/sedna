#!/bin/bash


echo indenting "$1"....
cp "$1" "$1~"
indent -bli0 -i4 -bl -nut-npsl -l300 -npcs -npsl "$1"
