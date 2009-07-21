/*
 * File: catvars.cpp
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "tr/cat/catvars.h"

#include "common/sedna.h"

const bool ccache_available = false;
int ccache_size;

int last_nid_size = 0;
unsigned char * last_nid = NULL;

xptr catalog_masterblock = XNULL;
