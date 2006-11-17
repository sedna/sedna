/*
 * File:  PPStringFuncs.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"

#include "PPStringFuncs.h"
#include "casting_operations.h"
#include "PPUtils.h"
#include "e_string.h"
#include "strings.h"
#include "xs_helper.h"
#include "dm_accessors.h"
#include <math.h>


///////////////////////////////////////////////////////////////////////////////
/// PPFnConcat
///////////////////////////////////////////////////////////////////////////////
PPFnConcat::PPFnConcat(variable_context *_cxt_,
                       arr_of_PPOpIn _ch_arr_) : PPIterator(_cxt_), 
                                                 ch_arr(_ch_arr_)
{
    for (i = 0; i < ch_arr.size(); i++)
    {
        if (ch_arr[i].ts != 1) throw USER_EXCEPTION2(SE1003, "Children of PPFnConcat operation have different tuple sizes");
        data.push_back(new tuple(1));
    }

    tcv.resize(ch_arr.size());
}

PPFnConcat::~PPFnConcat()
{
    for (i = 0; i < ch_arr.size(); i++)
    {
        delete (data[i]);
        data[i] = NULL;
    }

    for (i = 0; i < ch_arr.size(); i++) 
    {
        delete (ch_arr[i].op);
        ch_arr[i].op = NULL;
    }
}

void PPFnConcat::open ()
{
    for (i = 0; i < ch_arr.size(); i++) 
        ch_arr[i].op->open();

    first_time = true;
}

void PPFnConcat::reopen ()
{
    for (i = 0; i < ch_arr.size(); i++) 
        ch_arr[i].op->reopen();

    first_time = true;
}

void PPFnConcat::close ()
{
    for (i = 0; i < ch_arr.size(); i++)
        ch_arr[i].op->close();
}

void PPFnConcat::next(tuple &t)
{
    if (!first_time)
    {
        first_time = true;
        t.set_eos();
        return;
    }

    first_time = false;
    int res_str_len = 0;

    for (i = 0; i < ch_arr.size(); i++)
    {
        ch_arr[i].op->next(t);
        if (t.is_eos())
        {
            tcv[i] = EMPTY_STRING_TC;
            continue;
        }

        tuple_cell tc = ch_arr[i].get(t);
        tcv[i] = cast(atomize(tc), xs_string);
        res_str_len += tcv[i].get_strlen();

        ch_arr[i].op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Length of sequence passed to fn:concat is more than 1");
    }

    tuple_cell res;

    if (res_str_len > MAX_MEM_STR_SIZE)
    {
        e_str_buf buf;
        for (i = 0; i < ch_arr.size(); i++)
        {
            buf.append(tcv[i]);
        }

        res = buf.content();
    }
    else
    {
        char *str = new char[res_str_len + 1];
        int offs = 0;
        for (i = 0; i < ch_arr.size(); i++)
        {
            tcv[i].copy_string(str + offs);
            offs += tcv[i].get_strlen();
        }
        str[res_str_len] = '\0';

        res = tuple_cell::atomic(xs_string, str);
    }

    t.copy(res);
}

PPIterator* PPFnConcat::copy(variable_context *_cxt_)
{
    PPFnConcat *res = new PPFnConcat(_cxt_, ch_arr);

    int i;
    for (i = 0; i < ch_arr.size(); i++)
        res->ch_arr[i].op = ch_arr[i].op->copy(_cxt_);

    return res;
}

bool PPFnConcat::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnConcat::result");
}




///////////////////////////////////////////////////////////////////////////////
/// PPFnString2CodePoints
///////////////////////////////////////////////////////////////////////////////
PPFnString2CodePoints::PPFnString2CodePoints(variable_context *_cxt_,
                                   PPOpIn _child_) : PPIterator(_cxt_),
                                                     child(_child_)
{
}

PPFnString2CodePoints::~PPFnString2CodePoints()
{
    delete child.op;
    child.op = NULL;

}

void PPFnString2CodePoints::open  ()
{
    child.op->open();
    first_time = true;	
	
}

void PPFnString2CodePoints::reopen()
{
    child.op->reopen();
    first_time = true;	
	
}

void PPFnString2CodePoints::close ()
{
    child.op->close();
}

template <class Iterator>
static inline void utf8_getcharcode(const Iterator &start, const Iterator &end, bool *str_end, int *ofs, int *ch)
{
	Iterator a = start;
	a += *ofs;
	if (a < end)
	{
		utf8_iterator<Iterator> it(a);
		*ch = *it;
		++it;
		*ofs = it.base_iterator() - start;
		*str_end = false;
	}
	else
	{
		*str_end = true;
	}
}


void PPFnString2CodePoints::next  (tuple &t)
{
    if (first_time)
    {
        
        

        child.op->next(t);
        if (!t.is_eos())
        {
			first_time = false;
            in_str = child.get(t);
            in_str = cast(atomize(in_str), xs_string);
			position=0;
            child.op->next(t);
            if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Length of sequence passed to fn:string-length is more than 1");
        }
		else return;
       // t.copy(tuple_cell::atomic((__int64)len));
    }
	bool end;
	int code;
	STRING_ITERATOR_CALL_TEMPLATE_1tcptr_3p(utf8_getcharcode, &in_str, &end, &position, &code);
	if (!end)
	{
		t.copy(tuple_cell::atomic((__int64)code));
	}
	else
	{
		first_time = true;
		in_str.set_eos();
		position=0;
		t.set_eos();
	}    
}

PPIterator* PPFnString2CodePoints::copy(variable_context *_cxt_)
{
    PPFnString2CodePoints *res = new PPFnString2CodePoints(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnString2CodePoints::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnString2CodePoints::result");
}

///////////////////////////////////////////////////////////////////////////////
/// PPFnTranslate
///////////////////////////////////////////////////////////////////////////////
PPFnTranslate::PPFnTranslate(variable_context *_cxt_, 
							 PPOpIn _str_, PPOpIn _map_str_, PPOpIn _trans_str_) : PPIterator(_cxt_),
								   str(_str_), map_str(_map_str_), trans_str(_trans_str_)
{
}

PPFnTranslate::~PPFnTranslate()
{
	delete str.op;
	str.op = NULL;

	delete map_str.op;
	map_str.op = NULL;

	delete trans_str.op;
	trans_str.op = NULL;
}

void PPFnTranslate::open  ()
{
	str.op->open();
	map_str.op->open();
	trans_str.op->open();
	first_time = true;
}

void PPFnTranslate::reopen()
{
	str.op->reopen();
	map_str.op->reopen();
	trans_str.op->reopen();
	first_time = true;
}

void PPFnTranslate::close ()
{
	str.op->close();
	map_str.op->close();
	trans_str.op->close();
}

void PPFnTranslate::next  (tuple &t)
{
	if (first_time)
	{
		first_time = false;

		map_str.op->next(t);
		if (t.is_eos())
			throw USER_EXCEPTION2(XPTY0004, "2nd argument of fn:translate is empty sequence");
		tuple_cell map_tc = map_str.get(t);
		map_tc = atomize(map_tc);
		if (map_tc.get_atomic_type()==xs_untypedAtomic)
			map_tc.set_xtype(xs_string);
		else
			if (!is_string_type(map_tc.get_atomic_type()))
				throw USER_EXCEPTION2(XPTY0004, "2nd argument of fn:translate is not a string");
		map_tc = tuple_cell::make_sure_light_atomic(map_tc);
		map_str.op->next(t);
		if (!t.is_eos())
			throw USER_EXCEPTION2(XPTY0004, "2nd argument of fn:translate is not atomic value");

		trans_str.op->next(t);
		if (t.is_eos())
			throw USER_EXCEPTION2(XPTY0004, "3rd argument of fn:translate is empty sequence");
		tuple_cell trans_tc = trans_str.get(t);
		trans_tc = atomize(trans_tc);
		if (trans_tc.get_atomic_type()==xs_untypedAtomic)
			trans_tc.set_xtype(xs_string);
		else
			if (!is_string_type(trans_tc.get_atomic_type()))
				throw USER_EXCEPTION2(XPTY0004, "3rd argument of fn:translate is not a string");
		trans_tc = tuple_cell::make_sure_light_atomic(trans_tc);
		trans_str.op->next(t);
		if (!t.is_eos())
			throw USER_EXCEPTION2(XPTY0004, "3rd argument of fn:translate is not atomic value");

		str.op->next(t);
		if (!t.is_eos())
		{
			tuple_cell tc = str.get(t);
			tc = atomize(tc);
			if (!is_string_type(tc.get_atomic_type()))
				throw USER_EXCEPTION2(XPTY0004, "1st argument of fn:translate is not a string");
			str.op->next(t);
			if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Length of sequence passed to fn:translate as 1st argument is more than 1");

			charset_handler->transtale(t, &tc, &map_tc, &trans_tc);
		}
		else
			t.copy(EMPTY_STRING_TC);
	}
	else 
	{
		first_time = true;
		t.set_eos();
	}
}

PPIterator* PPFnTranslate::copy(variable_context *_cxt_)
{
	PPFnTranslate *res = new PPFnTranslate(_cxt_, str, map_str, trans_str);
	res->str.op = str.op->copy(_cxt_);
	res->map_str.op = map_str.op->copy(_cxt_);
	res->trans_str.op = trans_str.op->copy(_cxt_);

	return res;
}

bool PPFnTranslate::result(PPIterator* cur, variable_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFnTranslate::result");
}


///////////////////////////////////////////////////////////////////////////////
/// PPFnChangeCase
///////////////////////////////////////////////////////////////////////////////
PPFnChangeCase::PPFnChangeCase(variable_context *_cxt_,  PPOpIn _str_, bool _to_upper_) : 
                                      PPIterator(_cxt_), str(_str_), to_upper(_to_upper_)
{
}

PPFnChangeCase::~PPFnChangeCase()
{
	delete str.op;
	str.op = NULL;
}

void PPFnChangeCase::open  ()
{
	str.op->open();
	first_time = true;
}

void PPFnChangeCase::reopen()
{
	str.op->reopen();
	first_time = true;
}

void PPFnChangeCase::close ()
{
	str.op->close();
}

void PPFnChangeCase::next  (tuple &t)
{
	if (first_time)
	{
		first_time = false;

		str.op->next(t);
		if (!t.is_eos())
		{
			const tuple_cell tc = atomize(str.get(t));
			if (!is_string_type(tc.get_atomic_type()))
				throw USER_EXCEPTION2(XPTY0004, "1st argument of fn:upper-case or fn:lower-case is not a string"); //FIXME: make 2 sep. err. strings
			str.op->next(t);
			if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Length of sequence passed to fn:upper-case or fn:lower-case as 1st argument is more than 1"); //FIXME: make 2 sep. err. strings

			if (this->to_upper)
				t.copy(charset_handler->toupper(&tc));
			else
				t.copy(charset_handler->tolower(&tc));
		}
		else
			t.copy(EMPTY_STRING_TC);
	}
	else 
	{
		first_time = true;
		t.set_eos();
	}
}

PPIterator* PPFnChangeCase::copy(variable_context *_cxt_)
{
	PPFnChangeCase *res = new PPFnChangeCase(_cxt_, str, to_upper);
	res->str.op = str.op->copy(_cxt_);

	return res;
}

bool PPFnChangeCase::result(PPIterator* cur, variable_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFnChangeCase::result");
}


///////////////////////////////////////////////////////////////////////////////
/// PPFnStringLength
///////////////////////////////////////////////////////////////////////////////
PPFnStringLength::PPFnStringLength(variable_context *_cxt_,
                                   PPOpIn _child_) : PPIterator(_cxt_),
                                                     child(_child_)
{
}

PPFnStringLength::~PPFnStringLength()
{
    delete child.op;
    child.op = NULL;
}

void PPFnStringLength::open  ()
{
    child.op->open();
    first_time = true;
}

void PPFnStringLength::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnStringLength::close ()
{
    child.op->close();
}

void PPFnStringLength::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;
        int len = 0;

        child.op->next(t);
        if (!t.is_eos())
        {
            tuple_cell tc = child.get(t);
            tc = cast(atomize(tc), xs_string);
			len = charset_handler->length(&tc);

            child.op->next(t);
            if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Length of sequence passed to fn:string-length is more than 1");
        }

        t.copy(tuple_cell::atomic((__int64)len));
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnStringLength::copy(variable_context *_cxt_)
{
    PPFnStringLength *res = new PPFnStringLength(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);

    return res;
}

bool PPFnStringLength::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnStringLength::result");
}

///////////////////////////////////////////////////////////////////////////////
/// PPFnNormalizeSpace
///////////////////////////////////////////////////////////////////////////////
PPFnNormalizeSpace::PPFnNormalizeSpace(variable_context *_cxt_,
                                       PPOpIn _child_) : PPIterator(_cxt_),
                                                         child(_child_)
{
}

PPFnNormalizeSpace::~PPFnNormalizeSpace()
{
    delete child.op;
    child.op = NULL;
}

void PPFnNormalizeSpace::open  ()
{
    child.op->open();
    first_time = true;
}

void PPFnNormalizeSpace::reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnNormalizeSpace::close ()
{
    child.op->close();
}

void PPFnNormalizeSpace::next  (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);
        if (!t.is_eos())
        {
            tuple_cell tc = child.get(t);
            
            if (tc.is_node())
                tc = dm_string_value(tc.get_node());
            else
            {
                xmlscm_type xtype = tc.get_atomic_type();
                if(xtype != xs_string        && 
                   xtype != xs_untypedAtomic && 
                   xtype != xs_anyURI        &&
                   !is_derived_from_xs_string(xtype)) throw USER_EXCEPTION2(XPTY0004, "Invalid type of the argument in fn:normalize-space (xs_string/derived/promotable is expected).");
            }

            child.op->next(t);
            if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Invalid arity of the argument in fn:normalize-space. Argument contains more than one item.");
            stmt_str_buf result;
            collapse_string_normalization(&tc, result);
            t.copy(result.get_tuple_cell());
        }
        else
            t.copy(EMPTY_STRING_TC);
    }
    else 
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnNormalizeSpace::copy(variable_context *_cxt_)
{
    PPFnNormalizeSpace *res = new PPFnNormalizeSpace(_cxt_, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

bool PPFnNormalizeSpace::result(PPIterator* cur, variable_context *cxt, void*& r)
{
    throw USER_EXCEPTION2(SE1002, "PPFnNormalizeSpace::result");
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnSubstring
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnSubstring::PPFnSubstring(variable_context *_cxt_,
                             PPOpIn _str_child_,
                             PPOpIn _start_child_) : PPIterator(_cxt_),
                                                     str_child(_str_child_),
                                                     start_child(_start_child_),
                                                     is_length(false)
{
}

PPFnSubstring::PPFnSubstring(variable_context *_cxt_,
                             PPOpIn _str_child_,
                             PPOpIn _start_child_,
                             PPOpIn _length_child_) : PPIterator(_cxt_),
                                                      str_child(_str_child_),
                                                      start_child(_start_child_),
                                                      length_child(_length_child_),
                                                      is_length(true)
{
}

PPFnSubstring::~PPFnSubstring()
{
    delete str_child.op;
    str_child.op = NULL;
    delete start_child.op;
    start_child.op = NULL;

    if(is_length)
    {
    	delete length_child.op;
    	length_child.op = NULL;
    }
}

void PPFnSubstring::open  ()
{
    str_child.op->open();
    start_child.op->open();
    if(is_length) length_child.op->open();
    first_time = true;
}

void PPFnSubstring::reopen()
{
    str_child.op->reopen();
    start_child.op->reopen();
    if(is_length) length_child.op->reopen();
    first_time = true;
}

void PPFnSubstring::close ()
{
    str_child.op->close();
    start_child.op->close();
    if(is_length) length_child.op->close();

}

void PPFnSubstring::next(tuple &t)
{
    __int64 start_pos = 0;
    __int64 length = 0;
    if (first_time)
    {
        tuple_cell tc;

        first_time = false;

        start_child.op->next(t);
        if (t.is_eos()) throw USER_EXCEPTION2(XPTY0004, "Empty second argument is not allowed in fn:substring.");
        
        tc = start_child.get(t);
        if(!tc.is_atomic()) tc = atomize(tc);
        xmlscm_type xtype = tc.get_atomic_type();
        
        if(!is_numeric_type(xtype) && !(xtype == xs_untypedAtomic)) 
            throw USER_EXCEPTION2(XPTY0004, "Invalid type of the second argument in fn:substring (xs:double or promotable expected).");
        
        start_pos = (__int64)floor(cast(tc, xs_double).get_xs_double() + 0.5);  //floor(x+0.5) is equal there to fn:round
        
        start_child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Invalid cardinality of the second argument in fn:substring.");

        if(is_length)
        {
            length_child.op->next(t);
            if (t.is_eos()) throw USER_EXCEPTION2(XPTY0004, "Empty third argument is not allowed in fn:substring.");
            
            tc = start_child.get(t);
            tc = atomize(tc);
            xtype = tc.get_atomic_type();

            if(!is_numeric_type(xtype) && !(xtype == xs_untypedAtomic))  
                throw USER_EXCEPTION2(XPTY0004, "Invalid type of the third argument in fn:substring (xs:double or promotable expected).");

            length = (__int64)floor(cast(tc, xs_double).get_xs_double() + 0.5); //floor(x+0.5) is equal there to fn:round
        
            length_child.op->next(t);
            if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Invalid cardinality of the third argument in fn:substring.");
        }
        
        str_child.op->next(t);
        if (t.is_eos())
        {
             t.copy(EMPTY_STRING_TC);
             return;
        }
        tc = atomize(str_child.get(t));
        if (!is_string_type(tc.get_atomic_type()))
        	throw USER_EXCEPTION2(XPTY0004, "1st argument of fn:substring is not a string");
        str_child.op->next(t);
        if (!(t.is_eos())) throw USER_EXCEPTION2(XPTY0004, "Length of sequence passed to fn:substring as 1st argument is more than 1");

        if(is_length)
        {
            if (start_pos < 1)
            {
                length += start_pos - 1;
                start_pos = 1;
            }
            if (length > 0)
                t.copy(charset_handler->substring(&tc, start_pos-1, length));
            else
                t.copy(EMPTY_STRING_TC);
        }
        else
            t.copy(charset_handler->substring(&tc, start_pos-1, _I64_MAX));
    }
    else
    {
        t.set_eos();
        first_time = true;
    }
}

PPIterator* PPFnSubstring::copy(variable_context *_cxt_)
{
    PPFnSubstring *res = is_length ? new PPFnSubstring(_cxt_, str_child, start_child, length_child) :
                                     new PPFnSubstring(_cxt_, str_child, start_child);
                                
    res->str_child.op = str_child.op->copy(_cxt_);
    res->start_child.op = start_child.op->copy(_cxt_);
    if(is_length) res->length_child.op = length_child.op->copy(_cxt_);

    return res;
}

bool PPFnSubstring::result(PPIterator* cur, variable_context *cxt, void*& r)
{
	throw USER_EXCEPTION2(SE1002, "PPFnSubstring::result");
}
