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
    se_ostream& s;
    PPOpIn filename, document, collection;

public:
    void open();
    void close();
    void execute();

    PPBulkLoad(PPOpIn _filename_,
               PPOpIn _document_,
               PPOpIn _collection_,
               se_ostream& _s_);
    ~PPBulkLoad();
};


#endif

