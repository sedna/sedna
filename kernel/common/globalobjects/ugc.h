/*
 * File:  ugc.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _UGC_H
#define _UGC_H

void gov_ugc(bool background_off_from_background_on, int os_primitives_bound);

void sm_ugc(bool background_off_from_background_on, int db_id, int os_primitives_bound);

void cdb_ugc(int db_id, int os_primitives_bound);

#endif
