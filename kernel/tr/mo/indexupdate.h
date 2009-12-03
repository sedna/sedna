/*
 * File:  indexupdate.h
 * Copyright (C) 2009 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _INDEXUPDATE_H
#define _INDEXUPDATE_H

#include <stddef.h>
#include <stdint.h>

#include "common/sedna.h"
#include "common/xptr.h"
#include "tr/cat/catptr.h"

void update_idx_add(xptr node);
void update_idx_add(xptr node, const char* value, strsize_t size);
void update_idx_add_text(xptr node);

void update_idx_delete(xptr node);
void update_idx_delete_text(xptr node);

#endif /* _INDEXUPDATE_H */
