/*
* File: test_sba.c
* Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SBA_SIZE 16

#include "smallbitarray.h"

const uint32_t tmp[SBA_SIZE] =
  {
    0x00f2ff50, 0x05ff23f0,
    0x00000000, 0xff4fffff,
    0xffffffff, 0xf4ffffaf,
    0xffffffff, 0xffffbfff,
    0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff,
    0xffffffff, 0xffffffff
};

int main () {
  struct bit_array A;
  int a, n = 2;

  memcpy(A.a, tmp, 4*16);

  sba_print(&A);
  a = sba_find(&A, &n);
  printf("find : %d, %d\n", a, n);
  sba_occupy(&A, 0, 2);
  sba_occupy(&A, 2, 2);
  sba_occupy(&A, 5, 1);
  sba_occupy(&A, 7, 1);
  sba_occupy(&A, 16, 1);
  sba_occupy(&A, 18, 2);
  sba_occupy(&A, 24, 80);
  sba_occupy(&A, 116, 2);
  sba_occupy(&A, 119, 120);

  sba_print(&A);
  a = sba_find(&A, &n);
  printf("find : %d, %d\n", a, n);

  return 0;
}