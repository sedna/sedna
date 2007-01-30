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
    arr_of_PPOpIn   filenames;
    bool            is_load_replace;

public:
    void open();
    void close();
    void execute();

    PPLoadModule(arr_of_PPOpIn  _filenames_,
                 bool           _is_load_replace);
    ~PPLoadModule();
};


#endif

