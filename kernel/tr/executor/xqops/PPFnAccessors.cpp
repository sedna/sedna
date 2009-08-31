/*
 * File:  PPFnAccessors.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"

#include "tr/executor/xqops/PPFnAccessors.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/executor/fo/casting_operations.h"


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPDmNodeKind
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPDmNodeKind::PPDmNodeKind(dynamic_context *_cxt_,
                           operation_info _info_,
                           PPOpIn _child_) : PPIterator(_cxt_, _info_),
                                             child(_child_)
{
}

PPDmNodeKind::~PPDmNodeKind()
{
    delete child.op;
    child.op = NULL;
}

void PPDmNodeKind::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPDmNodeKind::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPDmNodeKind::do_close()
{
    child.op->close();
}

void PPDmNodeKind::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);

        if (t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:node-kind is not a node");
        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:node-kind is not a node");

        dm_node_kind_type res = dm_node_kind(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:node-kind is not a node");

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

PPIterator* PPDmNodeKind::do_copy(dynamic_context *_cxt_)
{
    PPDmNodeKind *res = se_new PPDmNodeKind(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnNodeName
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnNodeName::PPFnNodeName(dynamic_context *_cxt_,
                           operation_info _info_,
                           PPOpIn _child_) : PPIterator(_cxt_, _info_),
                                             child(_child_)
{
}

PPFnNodeName::~PPFnNodeName()
{
    delete child.op;
    child.op = NULL;
}

void PPFnNodeName::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnNodeName::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnNodeName::do_close()
{
    child.op->close();
}

void PPFnNodeName::do_next (tuple &t)
{
    if (first_time)
    {
		child.op->next(t);

        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:node-name is not a node");

        tuple_cell tc = dm_node_name(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:node-name is not a node");

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

PPIterator* PPFnNodeName::do_copy(dynamic_context *_cxt_)
{
    PPFnNodeName *res = se_new PPFnNodeName(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnNilled
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnNilled::PPFnNilled(dynamic_context *_cxt_,
                       operation_info _info_,
                       PPOpIn _child_) : PPIterator(_cxt_, _info_),
                                         child(_child_)
{
}

PPFnNilled::~PPFnNilled()
{
    delete child.op;
    child.op = NULL;
}

void PPFnNilled::do_open ()
{
    child.op->open();
	first_time = true;
}

void PPFnNilled::do_reopen()
{
    child.op->reopen();
	first_time = true;
}

void PPFnNilled::do_close()
{
    child.op->close();
}

void PPFnNilled::do_next (tuple &t)
{
    if (first_time)
    {
		child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:nilled is not a node");

        tuple_cell tc = dm_nilled(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:nilled is not a node");

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

PPIterator* PPFnNilled::do_copy(dynamic_context *_cxt_)
{
    PPFnNilled *res = se_new PPFnNilled(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnString
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnString::PPFnString(dynamic_context *_cxt_,
                       operation_info _info_,
                       PPOpIn _child_) : PPIterator(_cxt_, _info_),
                                         child(_child_)
{
}

PPFnString::~PPFnString()
{
    delete child.op;
    child.op = NULL;
}

void PPFnString::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnString::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnString::do_close()
{
    child.op->close();
}

void PPFnString::do_next (tuple &t)
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
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Length of sequence passed to fn:string is more than 1");

        t.copy(tc);
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnString::do_copy(dynamic_context *_cxt_)
{
    PPFnString *res = se_new PPFnString(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnData
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnData::PPFnData(dynamic_context *_cxt_,
                   operation_info _info_,
                   PPOpIn _child_) : PPIterator(_cxt_, _info_),
                                     child(_child_)
{
}

PPFnData::~PPFnData()
{
    delete child.op;
    child.op = NULL;
}

void PPFnData::do_open ()
{
    child.op->open();
}

void PPFnData::do_reopen()
{
    child.op->reopen();
}

void PPFnData::do_close()
{
    child.op->close();
}

void PPFnData::do_next (tuple &t)
{
    child.op->next(t);

    if (!t.is_eos() && child.get(t).is_node())
    {
        tuple_cell tc = dm_typed_value(child.get(t).get_node());
        t.copy(tc);
    }
}

PPIterator* PPFnData::do_copy(dynamic_context *_cxt_)
{
    PPFnData *res = se_new PPFnData(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnBaseURI
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnBaseURI::PPFnBaseURI(dynamic_context *_cxt_,
                         operation_info _info_,
                         PPOpIn _child_) : PPIterator(_cxt_, _info_),
                                           child(_child_)
{
}

PPFnBaseURI::~PPFnBaseURI()
{
    delete child.op;
    child.op = NULL;
}

void PPFnBaseURI::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnBaseURI::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnBaseURI::do_close()
{
    child.op->close();
}

void PPFnBaseURI::do_next (tuple &t)
{
    if (first_time)
    {
		child.op->next(t);
        if (t.is_eos()) return;

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:base-uri is not a node");

        tuple_cell tc = dm_base_uri(child.get(t).get_node(), cxt);

        child.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:base-uri is not a node");

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

PPIterator* PPFnBaseURI::do_copy(dynamic_context *_cxt_)
{
    PPFnBaseURI *res = se_new PPFnBaseURI(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnDocumentURI
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnDocumentURI::PPFnDocumentURI(dynamic_context *_cxt_,
                                 operation_info _info_,
                                 PPOpIn _child_) : PPIterator(_cxt_, _info_),
                                                   child(_child_)
{
}

PPFnDocumentURI::~PPFnDocumentURI()
{
    delete child.op;
    child.op = NULL;
}

void PPFnDocumentURI::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnDocumentURI::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnDocumentURI::do_close()
{
    child.op->close();
}

void PPFnDocumentURI::do_next (tuple &t)
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

        if (!(child.get(t).is_node())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:document-uri is not a node");

        tuple_cell tc = dm_document_uri(child.get(t).get_node());

        child.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Argument of fn:document-uri is not a node");

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

PPIterator* PPFnDocumentURI::do_copy(dynamic_context *_cxt_)
{
    PPFnDocumentURI *res = se_new PPFnDocumentURI(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnStaticBaseUri
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnStaticBaseUri::PPFnStaticBaseUri(dynamic_context *_cxt_, 
                                     operation_info _info_) : PPIterator(_cxt_, _info_)
{
}

PPFnStaticBaseUri::~PPFnStaticBaseUri() { }

void PPFnStaticBaseUri::do_open ()        { first_time = true; }

void PPFnStaticBaseUri::do_reopen()        { first_time = true; }

void PPFnStaticBaseUri::do_close()        { }

void PPFnStaticBaseUri::do_next (tuple &t)
{
        
    if(first_time)
    {
        first_time = false;    

        if ( cxt->st_cxt->base_uri == NULL ) 
        {
            t.copy( EMPTY_STRING_TC );
            (&t.cells[0]) -> set_xtype(xs_anyURI);
        }
        else 
            t.copy( tuple_cell::atomic_deep(xs_anyURI, cxt->st_cxt->base_uri) );
    }
    else 
    {
        t.set_eos();
        first_time = true;
    }
}


PPIterator* PPFnStaticBaseUri::do_copy(dynamic_context *_cxt_)
{
    PPFnStaticBaseUri *res = se_new PPFnStaticBaseUri(_cxt_, info);
    return res;
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnDefaultCollation
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnDefaultCollation::PPFnDefaultCollation(dynamic_context *_cxt_,
                                           operation_info _info_) : PPIterator(_cxt_, _info_)
{
}

PPFnDefaultCollation::~PPFnDefaultCollation() { }

void PPFnDefaultCollation::do_open ()        { first_time = true; }

void PPFnDefaultCollation::do_reopen()        { first_time = true; }

void PPFnDefaultCollation::do_close()        { }

void PPFnDefaultCollation::do_next (tuple &t)
{
    if(first_time)
    {
        first_time = false;    

        if ( cxt->st_cxt->default_collation_uri == NULL ) 
            throw USER_EXCEPTION2(SE1003, "Default collation property could not be undefined in PPFnDefaultCollation.");
        else 
            t.copy( tuple_cell::atomic_deep(xs_anyURI, cxt->st_cxt->default_collation_uri) );
    }
    else 
    {
        t.set_eos();
        first_time = true;
    }
}


PPIterator* PPFnDefaultCollation::do_copy(dynamic_context *_cxt_)
{
    PPFnDefaultCollation *res = se_new PPFnDefaultCollation(_cxt_, info);
    return res;
}
