/*
 * File:  PPCreateMetadata.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _PPCREATEMETADATA_H
#define _PPCREATEMETADATA_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"

class PPCreateDocument : public PPUpdate
{
    // given parameters
    PPOpIn name;
    dynamic_context *cxt;

public:
    void open();
    void close();
    void execute();
    void accept(PPVisitor& v);
    
    PPCreateDocument(PPOpIn _name_, dynamic_context *_cxt_);
    ~PPCreateDocument();
};

class PPCreateCollection : public PPUpdate
{
    // given parameters
    PPOpIn name;
    dynamic_context *cxt;

public:
    void open();
    void close();
    void execute();
    void accept(PPVisitor& v);
    
    PPCreateCollection(PPOpIn _name_, dynamic_context *_cxt_);
    ~PPCreateCollection();
};

class PPCreateDocumentInCollection : public PPUpdate
{
    // given parameters
    dynamic_context *cxt1, *cxt2;
    PPOpIn document, collection;

public:
    void open();
    void close();
    void execute();
    void accept(PPVisitor& v);
    
    PPCreateDocumentInCollection(PPOpIn _document_,
                                 dynamic_context *_cxt1_,
                                 PPOpIn _collection_,
                                 dynamic_context *_cxt2_);
    ~PPCreateDocumentInCollection();
};


#endif

