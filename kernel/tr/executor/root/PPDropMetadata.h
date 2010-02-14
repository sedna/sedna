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
private:
    PPOpIn name;
    dynamic_context *cxt;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:
    PPDropDocument(PPOpIn _name_, dynamic_context *_cxt_);
    ~PPDropDocument();
};

class PPDropCollection : public PPUpdate
{
private:
    PPOpIn name;
    dynamic_context *cxt;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:

    PPDropCollection(PPOpIn _name_, dynamic_context *_cxt_);
    ~PPDropCollection();
};

class PPDropDocumentInCollection : public PPUpdate
{
private:
    PPOpIn document, collection;
    dynamic_context *cxt1, *cxt2;

    virtual void do_open();
    virtual void do_close();
    virtual void do_execute();
    virtual void do_accept(PPVisitor& v);

public:

    PPDropDocumentInCollection(PPOpIn _document_,
                               dynamic_context *_cxt1_,
                               PPOpIn _collection_,
                               dynamic_context *_cxt2_);
    ~PPDropDocumentInCollection();
};

#endif /* _PPDROPMETADATA_H */

