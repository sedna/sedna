#!/bin/sh
gcc -I../../../driver/c -o01_connect 01_connect.c ../../../driver/c/libsedna.a
gcc -I../../../driver/c -o02_load 02_load.c ../../../driver/c/libsedna.a
gcc -I../../../driver/c -o03_load_coll 03_load_coll.c ../../../driver/c/libsedna.a
gcc -I../../../driver/c -o04_query 04_query.c ../../../driver/c/libsedna.a
gcc -I../../../driver/c -o05_trans 05_trans.c ../../../driver/c/libsedna.a
gcc -I../../../driver/c -o06_value_index 06_value_index.c ../../../driver/c/libsedna.a
gcc -I../../../driver/c -o07_fts_index 07_fts_index.c ../../../driver/c/libsedna.a
gcc -I../../../driver/c -o08_update 08_update.c ../../../driver/c/libsedna.a
gcc -I../../../driver/c -o09_trigger 09_trigger.c ../../../driver/c/libsedna.a
gcc -I../../../driver/c -o10_module 10_module.c ../../../driver/c/libsedna.a
gcc -I../../../driver/c -oClient Client.c ../../../driver/c/libsedna.a

