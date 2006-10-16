/*
 * File:  PPUriFuncs.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "PPUriFuncs.h"
#include "e_string.h"
#include "strings.h"


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


inline void hex_encode_utf8_byte(unsigned char byte, e_string_o_iterator<unsigned char> &o_itr)
{
    *o_itr = '%';
    ++o_itr;
    *o_itr = hex_value_to_char[((byte & 0xF0) >> 4)];
    ++o_itr;
    *o_itr = hex_value_to_char[(byte & 15)];
    ++o_itr;
}


template <class Iterator>
static inline void encode_for_uri(Iterator &start, const Iterator &end, e_string_o_iterator<unsigned char> &res_it)
{
    unsigned char value;

    while(start < end)
    {
        value = *start;
        if(IS_BYTE_URI_UNRESERVED(value))
        {
            *res_it = value;
            ++res_it;
        }
        else
            hex_encode_utf8_byte(value, res_it);
        ++start;
    }
}

template <class Iterator>
static inline void iri_to_uri(Iterator &start, const Iterator &end, e_string_o_iterator<unsigned char> &res_it)
{
    unsigned char value;
    
    while(start < end)
    {
        value = *start;
        if(IS_IRI_ALLOWED(value))
        {
            *res_it = value;
            ++res_it;
        }
        else
            hex_encode_utf8_byte(value, res_it);
        ++start;

    }
}

template <class Iterator>
static inline void escape_html_uri(Iterator &start, const Iterator &end, e_string_o_iterator<unsigned char> &res_it)
{
    unsigned char value;

    while(start < end)
    {
        value = *start;
        if(32 <= value && value <= 126)
        {
            *res_it = value;
            ++res_it;
        }
        else
            hex_encode_utf8_byte(value, res_it);
        ++start;
    }
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnUriEncoding
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnUriEncoding::PPFnUriEncoding(variable_context *_cxt_,
                                 PPOpIn _child_,
                                 uri_function_type _type_) : PPIterator(_cxt_),
                                                             child(_child_),
                                                             type(_type_),
                                                             first_time(true)
{
}

PPFnUriEncoding::~PPFnUriEncoding()
{
    delete child.op;
    child.op = NULL;
}

void PPFnUriEncoding::open  ()
{
    child.op->open();
}

void PPFnUriEncoding::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnUriEncoding::close ()
{
    child.op->close();
}

void PPFnUriEncoding::next  (tuple &t)
{
    if(first_time)
    {
        child.op->next(t);
        
        first_time = false;    

        if(t.is_eos()) 
        {
            t.copy(EMPTY_STRING_TC);
            return;
        }
            
        tuple_cell tc = child.get(t);
        xmlscm_type xtype = tc.get_atomic_type();
    
        if(xtype != xs_string        && 
           xtype != xs_untypedAtomic && 
           xtype != xs_anyURI        &&
           !is_derived_from_xs_string(xtype)) throw USER_EXCEPTION2(XPTY0004, error());
    
        if (e_string_last_blk==XNULL) 
        {
            vmm_alloc_tmp_block(&e_string_last_blk);
            e_str_blk_hdr::init(XADDR(e_string_last_blk));
            e_string_first_blk = e_string_last_blk;
        }
        
        e_string_o_iterator<unsigned char> res_it;
        xptr start_pos = res_it.pos;
    
        switch(type)
        {
            case(ENCODE_FOR_URI):
                STRING_ITERATOR_CALL_TEMPLATE_1tcptr_1p(encode_for_uri, &tc, res_it); break;
            case(IRI_TO_URI):
                STRING_ITERATOR_CALL_TEMPLATE_1tcptr_1p(iri_to_uri, &tc, res_it); break;
            case(ESCAPE_HTML_URI):
                STRING_ITERATOR_CALL_TEMPLATE_1tcptr_1p(escape_html_uri, &tc, res_it); break;
            default: 
                throw USER_EXCEPTION2(SE1003, "Impossible function type in PPFnUriEncoding::next");
        }
    
        child.op->next(t);
        if(!t.is_eos()) throw USER_EXCEPTION2(XPTY0004, error());
    
        int reslen = get_length_of_last_str(start_pos);  //FIXME!!! String length must be __int64;
        if(reslen > 0) reslen--;
    
        t.copy(tuple_cell::atomic_estr(xs_string, reslen, start_pos));
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

PPIterator* PPFnUriEncoding::copy(variable_context *_cxt_)
{
    PPFnUriEncoding *res = new PPFnUriEncoding(_cxt_, child, type);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

bool PPFnUriEncoding::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnUriEncoding::result");
}

