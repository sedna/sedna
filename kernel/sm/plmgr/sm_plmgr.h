/*
 * File:  sm_plmgr.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _SM_PLMGR_H
#define _SM_PLMGR_H


class sm_plmgr : public plmgr_core
{
public:

  LONG_LSN recoverDataBase();

};

#endif

