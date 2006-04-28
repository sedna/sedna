/*
 * File:  PPCreateMetadata.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPCREATEMETADATA_H
#define _PPCREATEMETADATA_H

#include "sedna.h"
#include "PPBase.h"

class PPCreateDocument : public PPUpdate
{
    // given parameters
    PPOpIn name;

public:
    void open();
    void close();
    void execute();

    PPCreateDocument(PPOpIn _name_);
    ~PPCreateDocument();
};

class PPCreateCollection : public PPUpdate
{
    // given parameters
    PPOpIn name;

public:
    void open();
    void close();
    void execute();

    PPCreateCollection(PPOpIn _name_);
    ~PPCreateCollection();
};

class PPCreateDocumentInCollection : public PPUpdate
{
    // given parameters
    PPOpIn document, collection;

public:
    void open();
    void close();
    void execute();

    PPCreateDocumentInCollection(PPOpIn _document_,
                                 PPOpIn _collection_);
    ~PPCreateDocumentInCollection();
};


#endif

