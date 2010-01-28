/*
 * File:  PPBulkLoad.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPLOADMODULE_H
#define _PPLOADMODULE_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPLoadModule : public PPUpdate
{
    arr_of_PPOpIn   filenames;
    bool            is_load_replace;
    dynamic_context *cxt;

public:
    void open();
    void close();
    void execute();
    void accept(PPVisitor& v);

    PPLoadModule(arr_of_PPOpIn  _filenames_,
                 bool           _is_load_replace,
                 dynamic_context *_cxt_);
    ~PPLoadModule();
    
    inline bool is_replace() const { return is_load_replace; }
};


#endif

