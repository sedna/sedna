/*
 * File:  PPFnAccessors.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPFnAccessors.h"
#include "dm_accessors.h"
#include "casting_operations.h"


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnString
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnString::PPFnString(variable_context *_cxt_,
                       PPOpIn _child_) : PPIterator(_cxt_),
                                         child(_child_)
{
}

PPFnString::~PPFnString()
{
    delete child.op;
    child.op = NULL;
}

void PPFnString::open  ()
{
    child.op->open();
    first_time = true;
}

void PPFnString::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnString::close ()
{
    child.op->close();
}

void PPFnString::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos())
        {
            t.copy(EMPTY_STRING_TC);
            return;
        }

        tuple_cell tc;
        if ((child.get(t).is_node())) 
            tc = dm_string_value(child.get(t).get_node());
        else
            tc = cast_to_xs_string(child.get(t));

        child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Length of sequence passed to fn:string is more than 1");

        t.copy(tc);
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnString::copy(variable_context *_cxt_)
{
    PPFnString *res = new PPFnString(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnString::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnString::result");
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnData
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnData::PPFnData(variable_context *_cxt_,
                   PPOpIn _child_) : PPIterator(_cxt_),
                                     child(_child_)
{
}

PPFnData::~PPFnData()
{
    delete child.op;
    child.op = NULL;
}

void PPFnData::open  ()
{
    child.op->open();
}

void PPFnData::reopen()
{
    child.op->reopen();
}

void PPFnData::close ()
{
    child.op->close();
}

void PPFnData::next  (tuple &t)
{
    child.op->next(t);

    if (!t.is_eos() && child.get(t).is_node())
    {
        tuple_cell tc = dm_typed_value(child.get(t).get_node());
        t.copy(tc);
    }
}

PPIterator* PPFnData::copy(variable_context *_cxt_)
{
    PPFnData *res = new PPFnData(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnData::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnData::result");
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnDocumentURI
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnDocumentURI::PPFnDocumentURI(variable_context *_cxt_,
                   PPOpIn _child_) : PPIterator(_cxt_),
                                     child(_child_)
{
}

PPFnDocumentURI::~PPFnDocumentURI()
{
    delete child.op;
    child.op = NULL;
}

void PPFnDocumentURI::open  ()
{
    child.op->open();
    first_time = true;
}

void PPFnDocumentURI::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnDocumentURI::close ()
{
    child.op->close();
}

void PPFnDocumentURI::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos())
        {
            first_time = true;
            return;
        }

        if (!(child.get(t).is_node())) throw USER_EXCEPTION2(XPTY0004, "Argument of fn:document-uri is not a node");

        tuple_cell tc = dm_document_uri(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Argument of fn:document-uri is not a node");

        if (tc.is_eos())
        {
            first_time = true;
            // t is eos already
        }
        else
        {
            t.copy(tc);
        }
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnDocumentURI::copy(variable_context *_cxt_)
{
    PPFnDocumentURI *res = new PPFnDocumentURI(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnDocumentURI::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnDocumentURI::result");
}
