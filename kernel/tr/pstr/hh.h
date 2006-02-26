/*
 * File:  hh.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _HH_H
#define _HH_H

shft hh_maxhole_size(xptr blk);
void hh_insert(xptr blk, hh_slot s);
void hh_remove_max(xptr blk);
void hh_remove(xptr blk, int i);
void hh_heapify(xptr blk, int i);

#endif