/*
 * File:  PPDropMetadata.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPDROPMETADATA_H
#define _PPDROPMETADATA_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPDropDocument : public PPUpdate
{
    // given parameters
    PPOpIn name;
    dynamic_context *cxt;

public:
    void open();
    void close();
    void execute();
    void accept(PPVisitor& v);

    PPDropDocument(PPOpIn _name_, dynamic_context *_cxt_);
    ~PPDropDocument();
};

class PPDropCollection : public PPUpdate
{
    // given parameters
    PPOpIn name;
    dynamic_context *cxt;

public:
    void open();
    void close();
    void execute();
    void accept(PPVisitor& v);

    PPDropCollection(PPOpIn _name_, dynamic_context *_cxt_);
    ~PPDropCollection();
};

class PPDropDocumentInCollection : public PPUpdate
{
    // given parameters
    PPOpIn document, collection;
    dynamic_context *cxt1, *cxt2;

public:
    void open();
    void close();
    void execute();
    void accept(PPVisitor& v);

    PPDropDocumentInCollection(PPOpIn _document_,
                               dynamic_context *_cxt1_,
                               PPOpIn _collection_,
                               dynamic_context *_cxt2_);
    ~PPDropDocumentInCollection();
};

#endif

