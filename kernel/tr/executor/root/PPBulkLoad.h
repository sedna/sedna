/*
 * File:  PPBulkLoad.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPBULKLOAD_H
#define _PPBULKLOAD_H

#include "sedna.h"
#include "PPBase.h"

class PPBulkLoad : public PPUpdate
{
    dynamic_context *cxt1, *cxt2, *cxt3;
    se_ostream& s;
    PPOpIn filename, document, collection;

public:
    void open();
    void close();
    void execute();

    PPBulkLoad(PPOpIn _filename_,
               dynamic_context *_cxt1_,
               PPOpIn _document_,
               dynamic_context *_cxt2_,
               PPOpIn _collection_,
               dynamic_context *_cxt3_,
               se_ostream& _s_);
    ~PPBulkLoad();
};


#endif

