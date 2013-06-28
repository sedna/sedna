/*
 * File:  PPUriFuncs.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include "tr/executor/xqops/PPUriFuncs.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/strings/strings.h"
#include "tr/executor/base/xs_uri.h"
#include "tr/executor/base/xs_helper.h"
#include "tr/executor/base/visitor/PPVisitor.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// URI encoding helpers and implementations
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

const unsigned char hex_value_to_char[16] = {'0','1','2','3','4','5','6','7','8','9',
                                             'A','B','C','D','E','F'};
/*
"unreserved" by [RFC 3986], that is the upper- and lower-case letters A-Z, 
the digits 0-9, HYPHEN-MINUS ("-"), LOW LINE ("_"), FULL STOP ".", and TILDE "~".
*/

const unsigned char uri_unreserved_ascii[16] = {0x00, 0x00, 0x00, 0x00,
                                                0x00, 0x06, 0xFF, 0xC0, 
                                                0x7F, 0xFF, 0xFF, 0xE1, 
                                                0x7F, 0xFF, 0xFF, 0xE2};

#define IS_BYTE_URI_UNRESERVED(byte) \
    (byte & 0x80 ? 0 : (uri_unreserved_ascii[(byte >> 3)] & (0x80 >> (byte & 7))))
  
const unsigned char iri_1byte_allowed[16]    = {0x00, 0x00, 0x00, 0x00,
                                                0x5F, 0xFF, 0xFF, 0xF5,
                                                0xFF, 0xFF, 0xFF, 0xF5,
                                                0x7F, 0xFF, 0xFF, 0xE2};

/*
allowed one byte ASCII characters in IRI are determined from RFC 3987.
*/
#define IS_IRI_ALLOWED(byte) \
    (byte & 0x80 ? 0 : (iri_1byte_allowed[(byte >> 3)] & (0x80 >> (byte & 7))))


inline void hex_encode_utf8_byte(unsigned char byte, stmt_str_buf &out_buf)
{
    out_buf << '%';
    out_buf << hex_value_to_char[((byte & 0xF0) >> 4)];
    out_buf << hex_value_to_char[(byte & 15)];
}


template <class Iterator>
static inline void encode_for_uri(Iterator &start, const Iterator &end, stmt_str_buf &res)
{
    unsigned char value;

    while(start < end)
    {
        value = *start;
        if(IS_BYTE_URI_UNRESERVED(value)) res << value;
        else hex_encode_utf8_byte(value, res);
        ++start;
    }
}

template <class Iterator>
static inline void iri_to_uri(Iterator &start, const Iterator &end, stmt_str_buf &res)
{
    unsigned char value;
    
    while(start < end)
    {
        value = *start;
        if(IS_IRI_ALLOWED(value)) res << value;
        else hex_encode_utf8_byte(value, res);
        ++start;

    }
}

template <class Iterator>
static inline void escape_html_uri(Iterator &start, const Iterator &end, stmt_str_buf &res)
{
    unsigned char value;

    while(start < end)
    {
        value = *start;
        if(32 <= value && value <= 126) res << value;
        else hex_encode_utf8_byte(value, res);
        ++start;
    }
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnUriEncoding
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnUriEncoding::PPFnUriEncoding(dynamic_context *_cxt_,
                                 operation_info _info_,
                                 PPOpIn _child_,
                                 uri_function_type _type_) : PPIterator(_cxt_,_info_, "PPFnUriEncoding"),
                                                             child(_child_),
                                                             type(_type_)
{
}

PPFnUriEncoding::~PPFnUriEncoding()
{
    delete child.op;
    child.op = NULL;
}

void PPFnUriEncoding::do_open ()
{
    first_time = true;
    child.op->open();
}

void PPFnUriEncoding::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnUriEncoding::do_close()
{
    child.op->close();
}

void PPFnUriEncoding::do_next (xqp_tuple &t)
{
    if(first_time)
    {
        child.op->next(t);
        
        first_time = false;    

        if(!t.is_eos()) 
        {
            tuple_cell tc = atomize(child.get(t));
        
            if(!is_string_type(tc.get_atomic_type())) throw XQUERY_EXCEPTION2(XPTY0004, error());

            child.op->next(t);
            if(!t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, error());
        
            stmt_str_buf res;
        
            switch(type)
            {
                case(ENCODE_FOR_URI):
                    STRING_ITERATOR_CALL_TEMPLATE_1tcptr_1p(encode_for_uri, &tc, res);  break;
                case(IRI_TO_URI):
                    STRING_ITERATOR_CALL_TEMPLATE_1tcptr_1p(iri_to_uri, &tc, res);      break;
                case(ESCAPE_HTML_URI):
                    STRING_ITERATOR_CALL_TEMPLATE_1tcptr_1p(escape_html_uri, &tc, res); break;
                default: 
                    throw USER_EXCEPTION2(SE1003, "Impossible function type in PPFnUriEncoding::next");
            }
        
            t.copy(res.get_tuple_cell());
        }
        else
            t.copy(EMPTY_STRING_TC);
    }
    else 
    {
        t.set_eos();
        first_time = true;
    }
}

const char* PPFnUriEncoding::error()
{
    switch(type)
    {
        case(ENCODE_FOR_URI):
            return "Expected one or zero value of xs:string/derived/promotable type in fn:encode_for_uri.";
        case(IRI_TO_URI):
            return "Expected one or zero value of xs:string/derived/promotable type in fn:iri_to_uri.";
        case(ESCAPE_HTML_URI):
            return "Expected one or zero value of xs:string/derived/promotable type in fn:escape_html_uri.";
        default: 
            throw USER_EXCEPTION2(SE1003, "Impossible function type in PPFnUriEncoding::error");
    }
}

PPIterator* PPFnUriEncoding::do_copy(dynamic_context *_cxt_)
{
    PPFnUriEncoding *res = se_new PPFnUriEncoding(_cxt_, info, child, type);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnUriEncoding::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}



///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnResolveUri
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnResolveUri::PPFnResolveUri(dynamic_context *_cxt_,
                               operation_info _info_,
                               PPOpIn _relative_) : PPIterator(_cxt_, _info_, "PPFnResolveUri"),
                                                    relative(_relative_),
                                                    is_base_static(true)
{
}

PPFnResolveUri::PPFnResolveUri(dynamic_context *_cxt_,
                               operation_info _info_,
                               PPOpIn _relative_,
                               PPOpIn _base_) : PPIterator(_cxt_, _info_, "PPFnResolveUri"),
                                                relative(_relative_),
                                                base(_base_),
                                                is_base_static(false)
{
}


PPFnResolveUri::~PPFnResolveUri()
{
    delete relative.op;
    relative.op = NULL;

    if(!is_base_static)
    {
        delete base.op;
        base.op = NULL;
    }
}

void PPFnResolveUri::do_open ()
{
    first_time = true;
    need_reopen = false;
    relative.op->open();

    if(!is_base_static) base.op->open();
}

void PPFnResolveUri::do_reopen()
{
    relative.op->reopen();
    if(!is_base_static) base.op->reopen();    
    first_time = true;
    need_reopen = false;
}

void PPFnResolveUri::do_close()
{
    relative.op->close();
    if(!is_base_static) base.op->reopen();    
}

void PPFnResolveUri::do_next (xqp_tuple &t)
{
    if(first_time)
    {
        relative.op->next(t);

        if(t.is_eos()) 
        {
            if(is_base_static) need_reopen = true;
            return;
        }
        
        tuple_cell base_tc;
        const char* base_uri;
        first_time = false;
        bool valid = false;

        tuple_cell relative_tc = atomize(relative.get(t));
        if(!is_string_type(relative_tc.get_atomic_type())) 
            throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the first argument in fn:resolve-uri (xs_string/derived/promotable is expected).");

        Uri::Information nfo;
        Uri::check_constraints(&relative_tc, &valid, &nfo);
        if(!valid) throw XQUERY_EXCEPTION2(FORG0002, "First argument of the fn:resolve-uri is not a valid URI.");
        relative_tc = tuple_cell::make_sure_light_atomic(relative_tc);
        
        if(is_base_static)
        {
            base_uri = cxt->get_static_context()->get_base_uri();
            if (base_uri == NULL) throw XQUERY_EXCEPTION(FONS0005); //base uri property is not defined in static context.
        }
        else
        {
            if(need_reopen) base.op->reopen();   
      
            base.op->next(t);
            if(t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the second argument in fn:resolve-uri. Second argument could not be emty sequence.");

            base_tc = atomize(base.get(t));

            if(!is_string_type(base_tc.get_atomic_type())) 
                throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the second argument in fn:resolve-uri (xs_string/derived/promotable is expected).");

            Uri::check_constraints(&base_tc, &valid, &nfo);
            if(!valid) throw XQUERY_EXCEPTION2(FORG0002, "Second argument of the fn:resolve-uri is not a valid URI.");

            base.op->next(t);
            if(!t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the second argument in fn:resolve-uri. Second argument contains more than one item.");
            need_reopen = false;
            base_tc  = tuple_cell::make_sure_light_atomic(base_tc);
            base_uri = base_tc.get_str_mem();
        }

        relative.op->next(t);
        if(!t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the first argument in fn:resolve-uri. First argument contains more than one item.");

        stmt_str_buf result;
        if(Uri::resolve(relative_tc.get_str_mem(), base_uri, result)) 
            t.copy(result.get_tuple_cell());
        else 
            t.copy(relative_tc);
        
        (&t.cells[0]) -> set_xtype(xs_anyURI);
    }
    else 
    {
        t.set_eos();
        first_time = true;
    }
}

PPIterator* PPFnResolveUri::do_copy(dynamic_context *_cxt_)
{
    PPFnResolveUri *res = is_base_static ? se_new PPFnResolveUri(_cxt_, info, relative) 
                                         : se_new PPFnResolveUri(_cxt_, info, relative, base);
    res->relative.op = relative.op->copy(_cxt_);
    if(!is_base_static) res->base.op = base.op->copy(_cxt_);
    return res;
}

void PPFnResolveUri::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    relative.op->accept(v);
    if(!is_base_static) base.op->accept(v);
    v.pop();
}
