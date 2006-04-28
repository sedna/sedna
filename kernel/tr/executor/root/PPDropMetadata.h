/*
 * File:  PPDropMetadata.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPDROPMETADATA_H
#define _PPDROPMETADATA_H

#include "sedna.h"
#include "PPBase.h"

class PPDropDocument : public PPUpdate
{
    // given parameters
    PPOpIn name;

public:
    void open();
    void close();
    void execute();

    PPDropDocument(PPOpIn _name_);
    ~PPDropDocument();
};

class PPDropCollection : public PPUpdate
{
    // given parameters
    PPOpIn name;

public:
    void open();
    void close();
    void execute();

    PPDropCollection(PPOpIn _name_);
    ~PPDropCollection();
};

class PPDropDocumentInCollection : public PPUpdate
{
    // given parameters
    PPOpIn document, collection;

public:
    void open();
    void close();
    void execute();

    PPDropDocumentInCollection(PPOpIn _document_,
                               PPOpIn _collection_);
    ~PPDropDocumentInCollection();
};

#endif

