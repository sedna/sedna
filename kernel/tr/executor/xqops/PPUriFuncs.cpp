/*
 * File:  PPUriFuncs.cpp
 * Copyright (C) 2006 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "PPUriFuncs.h"
#include "strings.h"
#include "uri.h"

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
                                                             type(_type_)
{
}

PPFnUriEncoding::~PPFnUriEncoding()
{
    delete child.op;
    child.op = NULL;
}

void PPFnUriEncoding::open  ()
{
    first_time = true;
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


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnResolveUri
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnResolveUri
///////////////////////////////////////////////////////////////////////////////
PPFnResolveUri::PPFnResolveUri(variable_context *_cxt_,
                               PPOpIn _relative_) : PPIterator(_cxt_),
                                                    relative(_relative_),
                                                    is_base_static(true)
{
}

PPFnResolveUri::PPFnResolveUri(variable_context *_cxt_,
                               PPOpIn _relative_,
                               PPOpIn _base_) : PPIterator(_cxt_),
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

void PPFnResolveUri::open  ()
{
    first_time = true;
    need_reopen = false;
    relative.op->open();

    if(!is_base_static) base.op->open();
}

void PPFnResolveUri::reopen()
{
    relative.op->reopen();
    if(!is_base_static) base.op->reopen();    
    first_time = true;
    need_reopen = false;
}

void PPFnResolveUri::close ()
{
    relative.op->close();
    if(!is_base_static) base.op->reopen();    
}

void PPFnResolveUri::next  (tuple &t)
{
    if(first_time)
    {
        relative.op->next(t);

        if(t.is_eos()) 
        {
            if(is_base_static) need_reopen = true;
            return;
        }
        
        t_str_buf result;
        tuple r(relative.ts);
        tuple_cell base_tc;
        first_time = false;

        tuple_cell relative_tc = relative.get(t);
        xmlscm_type xtype = relative_tc.get_atomic_type();
        if(xtype != xs_string        && 
           xtype != xs_untypedAtomic && 
           xtype != xs_anyURI        &&
           !is_derived_from_xs_string(xtype)) throw USER_EXCEPTION2(XPTY0004, "Invalid type of the first argument in fn:resolve-uri (xs_string/derived/promotable is expected).");

        relative_tc = tuple_cell::make_sure_light_atomic(relative_tc);
            if(!Uri::chech_constraints_for_xs_anyURI(&relative_tc))
                throw USER_EXCEPTION2(FORG0002, "First argument of the fn:resolve-uri is not valid URI.");
        
        if(is_base_static)
        {
            if (tr_globals::st_ct.base_uri == NULL) throw USER_EXCEPTION(FONS0005); //base uri property is not defined in static context.

            base_tc = tuple_cell::atomic(xs_string, tr_globals::st_ct.base_uri);
            if(!Uri::chech_constraints_for_xs_anyURI(&base_tc))
                throw USER_EXCEPTION2(FORG0002, "Base URI property defined in the prolog contains invalid URI (fn:resolve-uri).");
        }
        else
        {
            if(need_reopen) base.op->reopen();   
            tuple b(base.ts);
            base.op->next(b);
            
            if(b.is_eos())
                throw USER_EXCEPTION2(XPTY0004, "Invalid arity of the second argument in fn:resolve-uri. Second argument could not be emty sequence.");

            base_tc = base.get(b);
            xtype = base_tc.get_atomic_type();
            if(xtype != xs_string        && 
               xtype != xs_untypedAtomic && 
               xtype != xs_anyURI        &&
               !is_derived_from_xs_string(xtype)) throw USER_EXCEPTION2(XPTY0004, "Invalid type of the second argument in fn:resolve-uri (xs_string/derived/promotable is expected).");

            base_tc = tuple_cell::make_sure_light_atomic(base_tc);
            if(!Uri::chech_constraints_for_xs_anyURI(&base_tc))
                throw USER_EXCEPTION2(FORG0002, "Second argument of the fn:resolve-uri is not valid URI.");

            base.op->next(b);
            if(!b.is_eos()) 
                throw USER_EXCEPTION2(XPTY0004, "Invalid arity of the second argument in fn:resolve-uri. Second argument contains more than one item.");
            need_reopen = false;
        }

        relative.op->next(r);
        if(!r.is_eos()) 
            throw USER_EXCEPTION2(XPTY0004, "Invalid arity of the first argument in fn:resolve-uri. First argument contains more than one item.");

        if(Uri::resolve(relative_tc.get_str_mem(), base_tc.get_str_mem(), result))
        {
            if (result.get_type() == text_mem)
                t.copy(tuple_cell::atomic_deep(xs_anyURI, (char*)result.get_ptr_to_text()));
            else
                t.copy(tuple_cell::atomic_estr(xs_anyURI, result.get_size(), *(xptr*)result.get_ptr_to_text()));
        }
        else
        {
            tuple_cell* tc = &(t.cells[0]);
            tc->set_xtype(xs_anyURI);
        }
    }
    else 
    {
        t.set_eos();
        first_time = true;
    }
}

PPIterator* PPFnResolveUri::copy(variable_context *_cxt_)
{
    PPFnResolveUri *res = is_base_static ? new PPFnResolveUri(_cxt_, relative) 
                                         : new PPFnResolveUri(_cxt_, relative, base);
    res->relative.op = relative.op->copy(_cxt_);
    if(!is_base_static) res->base.op = base.op->copy(_cxt_);
    return res;
}

bool PPFnResolveUri::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnResolveUri::result");
}
