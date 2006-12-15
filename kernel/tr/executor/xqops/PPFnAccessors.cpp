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
/// PPDmNodeKind
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPDmNodeKind::PPDmNodeKind(dynamic_context *_cxt_,
                           PPOpIn _child_) : PPIterator(_cxt_),
                                             child(_child_)
{
}

PPDmNodeKind::~PPDmNodeKind()
{
    delete child.op;
    child.op = NULL;
}

void PPDmNodeKind::open  ()
{
    child.op->open();
    first_time = true;
}

void PPDmNodeKind::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPDmNodeKind::close ()
{
    child.op->close();
}

void PPDmNodeKind::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos()) throw USER_EXCEPTION2(XPTY0004, "Argument of fn:node-kind is not a node");
        if (!(child.get(t).is_node())) throw USER_EXCEPTION2(XPTY0004, "Argument of fn:node-kind is not a node");

        dm_node_kind_type res = dm_node_kind(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Argument of fn:node-kind is not a node");

        switch (res)
        {
        case nk_document				: 
             t.copy(tuple_cell::atomic_deep(xs_string, "document"));
             break;
        case nk_element					: 
             t.copy(tuple_cell::atomic_deep(xs_string, "element"));
             break;
        case nk_attribute				: 
             t.copy(tuple_cell::atomic_deep(xs_string, "attribute"));
             break;
        case nk_text					:
             t.copy(tuple_cell::atomic_deep(xs_string, "text"));
             break;
        case nk_namespace				:
             t.copy(tuple_cell::atomic_deep(xs_string, "namespace"));
             break;
        case nk_processing_instruction	:
             t.copy(tuple_cell::atomic_deep(xs_string, "processing-instruction"));
             break;
        case nk_comment					:
             t.copy(tuple_cell::atomic_deep(xs_string, "comment"));
             break;
        default							: 
             throw USER_EXCEPTION2(SE1003, "Unexpected value in fn:node-kind");
        }
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPDmNodeKind::copy(dynamic_context *_cxt_)
{
    PPDmNodeKind *res = new PPDmNodeKind(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPDmNodeKind::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPDmNodeKind*)cur)->children(child);

    void *nk_r;
    bool nk_s = (child.op->res_fun())(child.op, cxt, nk_r);

    if (!nk_s) // if expression is not strict
    { // create PPDmNodeKind and transmit state
        child.op = (PPIterator*)nk_r;
        r = new PPDmNodeKind(cxt, child);
        return false;
    }

    sequence *d_seq = (sequence*)nk_r;
    if (d_seq->size() != 1) throw USER_EXCEPTION2(XPTY0004, "Argument of fn:node-kind is not a node");
    const tuple_cell &tc = d_seq->get_00();
    if (!(tc.is_node())) throw USER_EXCEPTION2(XPTY0004, "Argument of fn:node-kind is not a node");

    dm_node_kind_type res = dm_node_kind(tc.get_node());

    switch (res)
    {
        case nk_document				: 
             r = new sequence(tuple_cell::atomic_deep(xs_string, "document"));
             return true;
        case nk_element					: 
             r = new sequence(tuple_cell::atomic_deep(xs_string, "element"));
             return true;
        case nk_attribute				: 
             r = new sequence(tuple_cell::atomic_deep(xs_string, "attribute"));
             return true;
        case nk_text					:
             r = new sequence(tuple_cell::atomic_deep(xs_string, "text"));
             return true;
        case nk_namespace				:
             r = new sequence(tuple_cell::atomic_deep(xs_string, "namespace"));
             return true;
        case nk_processing_instruction	:
             r = new sequence(tuple_cell::atomic_deep(xs_string, "processing-instruction"));
             return true;
        case nk_comment					:
             r = new sequence(tuple_cell::atomic_deep(xs_string, "comment"));
             return true;
        default							: 
             throw USER_EXCEPTION2(SE1003, "Unexpected value in fn:node-kind");
    }
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnNodeName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnNodeName::PPFnNodeName(dynamic_context *_cxt_,
                           PPOpIn _child_) : PPIterator(_cxt_),
                                             child(_child_)
{
}

PPFnNodeName::~PPFnNodeName()
{
    delete child.op;
    child.op = NULL;
}

void PPFnNodeName::open  ()
{
    child.op->open();
    first_time = true;
}

void PPFnNodeName::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnNodeName::close ()
{
    child.op->close();
}

void PPFnNodeName::next  (tuple &t)
{
    if (first_time)
    {
		child.op->next(t);

        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION2(XPTY0004, "Argument of fn:node-name is not a node");

        tuple_cell tc = dm_node_name(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Argument of fn:node-name is not a node");

        if (tc.is_eos()) t.set_eos();
		else 
		{
			t.copy(tc);
			first_time = false;
		}
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnNodeName::copy(dynamic_context *_cxt_)
{
    PPFnNodeName *res = new PPFnNodeName(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnNodeName::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    PPOpIn child;
    ((PPFnNodeName*)cur)->children(child);

    void *nn_r;
    bool nn_s = (child.op->res_fun())(child.op, cxt, nn_r);

    if (!nn_s) // if expression is not strict
    { // create PPFnNodeName and transmit state
        child.op = (PPIterator*)nn_r;
        r = new PPFnNodeName(cxt, child);
        return false;
    }

    sequence *d_seq = (sequence*)nn_r;
    if (d_seq->size() != 1) throw USER_EXCEPTION2(XPTY0004, "Argument of fn:node-name is not a node");
    const tuple_cell &tc = d_seq->get_00();
    if (!(tc.is_node())) throw USER_EXCEPTION2(XPTY0004, "Argument of fn:node-name is not a node");

    r = new sequence(dm_node_name(tc.get_node()));
    return true;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnNilled
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnNilled::PPFnNilled(dynamic_context *_cxt_,
                       PPOpIn _child_) : PPIterator(_cxt_),
                                         child(_child_)
{
}

PPFnNilled::~PPFnNilled()
{
    delete child.op;
    child.op = NULL;
}

void PPFnNilled::open  ()
{
    child.op->open();
	first_time = true;
}

void PPFnNilled::reopen()
{
    child.op->reopen();
	first_time = true;
}

void PPFnNilled::close ()
{
    child.op->close();
}

void PPFnNilled::next  (tuple &t)
{
    if (first_time)
    {
		child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION2(XPTY0004, "Argument of fn:nilled is not a node");

        tuple_cell tc = dm_nilled(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Argument of fn:nilled is not a node");

        if (tc.is_eos()) t.set_eos();
		else 
		{
			t.copy(tc);
			first_time = false;
		}
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnNilled::copy(dynamic_context *_cxt_)
{
    PPFnNilled *res = new PPFnNilled(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnNilled::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnNilled::result");
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnString
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnString::PPFnString(dynamic_context *_cxt_,
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
            tc = cast(child.get(t), xs_string);

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

PPIterator* PPFnString::copy(dynamic_context *_cxt_)
{
    PPFnString *res = new PPFnString(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnString::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnString::result");
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnData
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnData::PPFnData(dynamic_context *_cxt_,
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

PPIterator* PPFnData::copy(dynamic_context *_cxt_)
{
    PPFnData *res = new PPFnData(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnData::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnData::result");
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnBaseURI
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnBaseURI::PPFnBaseURI(dynamic_context *_cxt_,
                         PPOpIn _child_) : PPIterator(_cxt_),
                                           child(_child_)
{
}

PPFnBaseURI::~PPFnBaseURI()
{
    delete child.op;
    child.op = NULL;
}

void PPFnBaseURI::open  ()
{
    child.op->open();
    first_time = true;
}

void PPFnBaseURI::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnBaseURI::close ()
{
    child.op->close();
}

void PPFnBaseURI::next  (tuple &t)
{
    if (first_time)
    {
		child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw USER_EXCEPTION2(XPTY0004, "Argument of fn:base-uri is not a node");

        tuple_cell tc = dm_base_uri(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Argument of fn:base-uri is not a node");

        if (tc.is_eos()) t.set_eos();
		else 
		{
			t.copy(tc);
			first_time = false;
		}
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnBaseURI::copy(dynamic_context *_cxt_)
{
    PPFnBaseURI *res = new PPFnBaseURI(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnBaseURI::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnBaseURI::result");
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnDocumentURI
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnDocumentURI::PPFnDocumentURI(dynamic_context *_cxt_,
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

PPIterator* PPFnDocumentURI::copy(dynamic_context *_cxt_)
{
    PPFnDocumentURI *res = new PPFnDocumentURI(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnDocumentURI::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnDocumentURI::result");
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnStaticBaseUri
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnStaticBaseUri::PPFnStaticBaseUri(dynamic_context *_cxt_) : PPIterator(_cxt_)
{
}

PPFnStaticBaseUri::~PPFnStaticBaseUri() { }

void PPFnStaticBaseUri::open  ()        { first_time = true; }

void PPFnStaticBaseUri::reopen()        { first_time = true; }

void PPFnStaticBaseUri::close ()        { }

void PPFnStaticBaseUri::next  (tuple &t)
{
    if(first_time)
    {
        first_time = false;    

        if ( tr_globals::st_ct.base_uri == NULL ) 
        {
            t.copy( EMPTY_STRING_TC );
            (&t.cells[0]) -> set_xtype(xs_anyURI);
        }
        else 
            t.copy( tuple_cell::atomic_deep(xs_anyURI, tr_globals::st_ct.base_uri) );
    }
    else 
    {
        t.set_eos();
        first_time = true;
    }
}


PPIterator* PPFnStaticBaseUri::copy(dynamic_context *_cxt_)
{
    PPFnStaticBaseUri *res = new PPFnStaticBaseUri(_cxt_);
    return res;
}

bool PPFnStaticBaseUri::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnStaticBaseUri::result");
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnDefaultCollation
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnDefaultCollation::PPFnDefaultCollation(dynamic_context *_cxt_) : PPIterator(_cxt_)
{
}

PPFnDefaultCollation::~PPFnDefaultCollation() { }

void PPFnDefaultCollation::open  ()        { first_time = true; }

void PPFnDefaultCollation::reopen()        { first_time = true; }

void PPFnDefaultCollation::close ()        { }

void PPFnDefaultCollation::next  (tuple &t)
{
    if(first_time)
    {
        first_time = false;    

        if ( tr_globals::st_ct.default_collation_uri == NULL ) 
            throw USER_EXCEPTION2(SE1003, "Default collation property could not be undefined in PPFnDefaultCollation.");
        else 
            t.copy( tuple_cell::atomic_deep(xs_anyURI, tr_globals::st_ct.default_collation_uri) );
    }
    else 
    {
        t.set_eos();
        first_time = true;
    }
}


PPIterator* PPFnDefaultCollation::copy(dynamic_context *_cxt_)
{
    PPFnDefaultCollation *res = new PPFnDefaultCollation(_cxt_);
    return res;
}

bool PPFnDefaultCollation::result(PPIterator* cur, dynamic_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnDefaultCollation::result");
}

