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
        tcv[i] = cast_to_xs_string(atomize(tc));
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
            tc = cast_to_xs_string(atomize(tc));
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
/// PPFnTranslate
///////////////////////////////////////////////////////////////////////////////
PPFnTranslate::PPFnTranslate(variable_context *_cxt_, 
							 PPOpIn _str_, PPOpIn _map_str_, PPOpIn _trans_str_) : PPIterator(_cxt_),
								   str(_str_), map_str(_map_str_), trans_str(_trans_str_)
{
	if (e_string_last_blk == XNULL)
		init_e_string_blks();
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
			if (!map_tc.is_string_type())
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
			if (!trans_tc.is_string_type())
				throw USER_EXCEPTION2(XPTY0004, "3rd argument of fn:translate is not a string");
		trans_tc = tuple_cell::make_sure_light_atomic(trans_tc);
		trans_str.op->next(t);
		if (!t.is_eos())
			throw USER_EXCEPTION2(XPTY0004, "3rd argument of fn:translate is not atomic value");

		str.op->next(t);
		if (!t.is_eos())
		{
			tuple_cell tc = str.get(t);
			tc = cast_to_xs_string(atomize(tc));
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
