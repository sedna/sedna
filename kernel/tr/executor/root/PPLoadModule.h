/*
 * File:  PPBulkLoad.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPLOADMODULE_H
#define _PPLOADMODULE_H

#include "sedna.h"
#include "PPBase.h"

class PPLoadModule : public PPUpdate
{
    se_ostream& s;
    PPOpIn filename, modulename;
    bool is_load_replace;

public:
    void open();
    void close();
    void execute();

    PPLoadModule(PPOpIn _filename_,
                 PPOpIn _modulename_,
                 bool _is_load_replace,
                 se_ostream& _s_);
    ~PPLoadModule();
};


#endif

