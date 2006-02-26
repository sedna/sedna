/*
 * File:  PPBulkLoad.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPBULKLOAD_H
#define _PPBULKLOAD_H

#include "PPBase.h"

class PPBulkLoad : public PPUpdate
{
    crmostream& s;
    PPOpIn filename, document, collection;
    bool print_progress;

public:
    void open();
    void close();
    void execute();

    PPBulkLoad(PPOpIn _filename_,
               PPOpIn _document_,
               PPOpIn _collection_,
               crmostream& _s_,
               bool _print_progress_);
    ~PPBulkLoad();
};


#endif

