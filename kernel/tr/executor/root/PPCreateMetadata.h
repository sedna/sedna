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
private:
    PPOpIn name;
    dynamic_context *cxt;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:

    PPCreateDocument(PPOpIn _name_, dynamic_context *_cxt_);
    ~PPCreateDocument();
};

class PPCreateCollection : public PPUpdate
{
private:
    PPOpIn name;
    dynamic_context *cxt;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:
    
    PPCreateCollection(PPOpIn _name_, dynamic_context *_cxt_);
    ~PPCreateCollection();
};

class PPCreateDocumentInCollection : public PPUpdate
{
private:
    dynamic_context *cxt1, *cxt2;
    PPOpIn document, collection;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:
    
    PPCreateDocumentInCollection(PPOpIn _document_,
                                 dynamic_context *_cxt1_,
                                 PPOpIn _collection_,
                                 dynamic_context *_cxt2_);
    ~PPCreateDocumentInCollection();
};

#endif /* _PPCREATEMETADATA_H */
