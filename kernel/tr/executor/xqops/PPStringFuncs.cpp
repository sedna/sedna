/*
 * File:  PPStringFuncs.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <math.h>

#include "common/sedna.h"

#include "tr/executor/xqops/PPStringFuncs.h"
#include "tr/executor/fo/casting_operations.h"
#include "tr/executor/base/PPUtils.h"
#include "tr/strings/e_string.h"
#include "tr/strings/strings.h"
#include "tr/strings/utf8.h"
#include "tr/executor/base/xs_helper.h"
#include "common/u/uutils.h"
#include "tr/executor/base/dm_accessors.h"
#include "tr/executor/base/visitor/PPVisitor.h"

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnConcat
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnConcat::PPFnConcat(dynamic_context *_cxt_,
                       operation_info _info_,
                       arr_of_PPOpIn _ch_arr_) : PPIterator(_cxt_, _info_, "PPFnConcat"),
                                                 ch_arr(_ch_arr_)
{
    tcv.resize(ch_arr.size());
}

PPFnConcat::~PPFnConcat()
{
    for (i = 0; i < ch_arr.size(); i++)
    {
        delete (ch_arr[i].op);
        ch_arr[i].op = NULL;
    }
}

void PPFnConcat::do_open ()
{
    for (i = 0; i < ch_arr.size(); i++)
        ch_arr[i].op->open();

    first_time = true;
}

void PPFnConcat::do_reopen()
{
    for (i = 0; i < ch_arr.size(); i++)
        ch_arr[i].op->reopen();

    first_time = true;
}

void PPFnConcat::do_close()
{
    for (i = 0; i < ch_arr.size(); i++)
        ch_arr[i].op->close();
}

void PPFnConcat::do_next(tuple &t)
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
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Length of sequence passed to fn:concat is more than 1");
    }

    tuple_cell res;

    if (res_str_len > MAX_MEM_STR_SIZE)
    {
        estr_buf buf;
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

PPIterator* PPFnConcat::do_copy(dynamic_context *_cxt_)
{
    PPFnConcat *res = new PPFnConcat(_cxt_, info, ch_arr);

    for (i = 0; i < ch_arr.size(); i++)
        res->ch_arr[i].op = ch_arr[i].op->copy(_cxt_);
    return res;
}

void PPFnConcat::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    for (i = 0; i < ch_arr.size(); i++)
        ch_arr[i].op->accept(v);
    v.pop();
}



///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnStringJoin
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnStringJoin::PPFnStringJoin(dynamic_context *_cxt_,
                               operation_info _info_,
                               PPOpIn _members_,
                               PPOpIn _separator_) : PPIterator(_cxt_, _info_, "PPFnStringJoin"),
                                                     members(_members_),
                                                     separator(_separator_)
{
}

PPFnStringJoin::~PPFnStringJoin()
{
    delete members.op;
    members.op = NULL;
    delete separator.op;
    separator.op = NULL;
}

void PPFnStringJoin::do_open ()
{
    members.op -> open();
    separator.op -> open();
    first_time = true;
    need_clear = false;
}

void PPFnStringJoin::do_reopen()
{
    members.op -> reopen();
    separator.op -> reopen();
    first_time = true;
    need_clear = true;
}

void PPFnStringJoin::do_close()
{
    members.op -> close();
    separator.op -> close();
}

void PPFnStringJoin::do_next(tuple &t)
{
    if (!first_time)
    {
        first_time = true;
        need_clear = true;
        t.set_eos();
        return;
    }

    first_time = false;

    if(need_clear) { tcv.clear(); need_clear = false; }

    bool is_sep = false;

    tuple_cell sep;
    tuple_cell tc;

    separator.op->next(t);
    if (t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the second argument of fn:string-join. Argument contains zero items.");
    sep = atomize(separator.get(t));
    if(!is_string_type(sep.get_atomic_type())) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the separator of fn:string-join (xs_string/derived/promotable is expected).");
    separator.op->next(t);
    if (!t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the second argument of fn:string-join. Argument contains more than one item.");

    is_sep = (sep.get_strlen() > 0);

    while(true)
    {
        members.op->next(t);
        if (t.is_eos()) break;
        tc = atomize(members.get(t));
        if(!is_string_type(tc.get_atomic_type())) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the item in first argument of fn:string-join (xs_string/derived/promotable is expected).");
        tcv.push_back(tc);
    }

    stmt_str_buf result;
    for(unsigned int i = 0; i < tcv.size(); i++)
    {
        if(is_sep && i) result.append(sep);
        result.append(tcv[i]);
    }
    t.copy(result.get_tuple_cell());
}

PPIterator* PPFnStringJoin::do_copy(dynamic_context *_cxt_)
{
    PPFnStringJoin *res = new PPFnStringJoin(_cxt_, info, members, separator);
    res->members.op   = members.op->copy(_cxt_);
    res->separator.op = separator.op->copy(_cxt_);
    return res;
}

void PPFnStringJoin::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    members.op->accept(v);
    separator.op->accept(v);
    v.pop();
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnStartsEndsWith
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnStartsEndsWith::PPFnStartsEndsWith(dynamic_context *_cxt_,
                                       operation_info _info_,
                                       PPOpIn _source_,
                                       PPOpIn _prefix_,
                                       PPFnStartsEndsWith::FunctionType _type_) : PPIterator(_cxt_, _info_, "PPFnStartsEndsWith"),
                                                                                  source(_source_),
                                                                                  prefix(_prefix_),
                                                                                  type(_type_),
                                                                                  is_collation(false)
{
}

PPFnStartsEndsWith::PPFnStartsEndsWith(dynamic_context *_cxt_,
                                       operation_info _info_,
                                       PPOpIn _source_,
                                       PPOpIn _prefix_,
                                       PPOpIn _collation_,
                                       PPFnStartsEndsWith::FunctionType _type_) : PPIterator(_cxt_, _info_, "PPFnStartsEndsWith"),
                                                                                  source(_source_),
                                                                                  prefix(_prefix_),
                                                                                  collation(_collation_),
                                                                                  type(_type_),
                                                                                  is_collation(true)
{
}


PPFnStartsEndsWith::~PPFnStartsEndsWith()
{
    delete source.op;
    source.op = NULL;
    delete prefix.op;
    prefix.op = NULL;
    if(is_collation)
    {
        delete collation.op;
        collation.op = NULL;
    }
}

void PPFnStartsEndsWith::do_open ()
{
    source.op -> open();
    prefix.op -> open();
    if(is_collation)
        collation.op -> open();
    first_time = true;
}

void PPFnStartsEndsWith::do_reopen()
{
    source.op -> reopen();
    prefix.op -> reopen();
    if(is_collation)
        collation.op -> reopen();
    first_time = true;
}

void PPFnStartsEndsWith::do_close()
{
    if(is_collation)
        collation.op -> close();
    source.op -> close();
    prefix.op -> close();
}

void PPFnStartsEndsWith::do_next(tuple &t)
{
    if (!first_time)
    {
        first_time = true;
        t.set_eos();
        return;
    }
    first_time = false;

    CollationHandler* handler;
    tuple_cell src;
    int64_t src_len = 0;
    tuple_cell prf;
    int64_t prf_len = 0;
    tuple_cell col;

    source.op->next(t);
    if (!t.is_eos())
    {
        src = atomize(source.get(t));

        if(!is_string_type(src.get_atomic_type())) error("Invalid type of the first argument (xs_string/derived/promotable is expected) ");

        source.op->next(t);
        if (!t.is_eos()) error("Invalid arity of the first argument. Argument contains more than one item ");
        src_len = src.get_strlen();
    }

    prefix.op->next(t);
    if (!t.is_eos())
    {
        prf = atomize(prefix.get(t));

        if(!is_string_type(prf.get_atomic_type())) error("Invalid type of the second argument (xs_string/derived/promotable is expected) ");

        prefix.op->next(t);
        if (!t.is_eos()) error("Invalid arity of the second argument. Argument contains more than one item ");
        prf_len = prf.get_strlen();
    }

    if(prf_len == 0) { t.copy(tuple_cell::atomic(true)); return; }
    else if(src_len == 0 || src_len < prf_len) { t.copy(tuple_cell::atomic(false)); return; }

    if(is_collation)
    {
        collation.op->next(t);
        if(t.is_eos()) error("Invalid arity of the third argument. Argument contains zero items ");

        col = atomize(collation.get(t));

        if(!is_string_type(col.get_atomic_type())) error("Invalid type of the third argument (xs_string/derived/promotable is expected) ");

        collation.op->next(t);
        if(!t.is_eos()) error("Invalid arity of the third argument. Argument contains more than one item ");

        col = tuple_cell::make_sure_light_atomic(col);

        int res = cxt->get_static_context()->get_collation(col.get_str_mem(), &handler);
        if(res != 0) throw XQUERY_EXCEPTION2(FOCH0002, (static_context::get_error_description(res) + ".").c_str());

    }
    else
        handler = cxt->get_static_context()->get_default_collation();

    if(type == PPFnStartsEndsWith::FN_STARTS_WITH)
        t.copy(tuple_cell::atomic(handler->starts_with(&src, &prf)));
    else if(type == PPFnStartsEndsWith::FN_ENDS_WITH)
        t.copy(tuple_cell::atomic(handler->ends_with(&src, &prf)));
    else
        throw USER_EXCEPTION2(SE1003, "Imposible type of function in PPFnStartsEndsWith::do_next().");
}

void PPFnStartsEndsWith::error(const char* msg)
{
    if(type == PPFnStartsEndsWith::FN_STARTS_WITH)
        throw XQUERY_EXCEPTION2(XPTY0004, (std::string(msg) + "in fn:starts-with().").c_str());
    else if(type == PPFnStartsEndsWith::FN_ENDS_WITH)
        throw XQUERY_EXCEPTION2(XPTY0004, (std::string(msg) + "in fn:ends-with().").c_str());
    else
        throw XQUERY_EXCEPTION2(SE1003, "Imposible type of function in PPFnStartsEndsWith::error().");
}

PPIterator* PPFnStartsEndsWith::do_copy(dynamic_context *_cxt_)
{
    PPFnStartsEndsWith *res = is_collation ?
                              new PPFnStartsEndsWith(_cxt_, info, source, prefix, collation, type) :
                              new PPFnStartsEndsWith(_cxt_, info, source, prefix, type);
    res->source.op = source.op->copy(_cxt_);
    res->prefix.op = prefix.op->copy(_cxt_);
    if(is_collation) res->collation.op = collation.op->copy(_cxt_);
    return res;
}

void PPFnStartsEndsWith::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    source.op->accept(v);
    prefix.op->accept(v);
    if(is_collation) collation.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnString2CodePoints
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnString2CodePoints::PPFnString2CodePoints(dynamic_context *_cxt_,
                                             operation_info _info_,
                                             PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnString2CodePoints"),
                                                               child(_child_),
                                                               ucp_it(NULL)
{
}

PPFnString2CodePoints::~PPFnString2CodePoints()
{
    delete child.op;
    child.op = NULL;

	if (ucp_it != NULL)
	{
		delete ucp_it;
		ucp_it = NULL;
	}
}

void PPFnString2CodePoints::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnString2CodePoints::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnString2CodePoints::do_close()
{
    child.op->close();
}

void PPFnString2CodePoints::do_next (tuple &t)
{
    if (first_time)
    {
        child.op->next(t);
        if (!t.is_eos())
        {
            first_time = false;
            in_str = atomize(child.get(t));

            if(!is_string_type(in_str.get_atomic_type())) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the argument in fn:string-to-codepoints (xs_string/derived/promotable is expected).");

            child.op->next(t);
            if (!t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the argument. Argument contains more than one item in fn:string-to-codepoints.");

		    ucp_it = charset_handler->get_unicode_cp_iterator(&in_str);
        }
		else return;
    }
	int code = ucp_it->get_next_char();
	if (code != -1)
	{
		t.copy(tuple_cell::atomic((int64_t)code));
	}
	else
	{
		first_time = true;
		t.set_eos();
		delete ucp_it;
		ucp_it = NULL;
		in_str.set_eos();
	}
}

PPIterator* PPFnString2CodePoints::do_copy(dynamic_context *_cxt_)
{
    PPFnString2CodePoints *res = new PPFnString2CodePoints(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnString2CodePoints::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnCodePoints2String
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnCodePoints2String::PPFnCodePoints2String(dynamic_context *_cxt_,
                                             operation_info _info_,
                                             PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnCodePoints2String"),
                                                               child(_child_)
{
}

PPFnCodePoints2String::~PPFnCodePoints2String()
{
    delete child.op;
    child.op = NULL;
}

void PPFnCodePoints2String::do_open ()
{
    child.op->open();
    first_time = true;
    need_clear = false;
}

void PPFnCodePoints2String::do_reopen()
{
    child.op->reopen();
    first_time = true;
    need_clear = true;
}

void PPFnCodePoints2String::do_close()
{
    child.op->close();
}

void PPFnCodePoints2String::do_next (tuple &t)
{
    if (first_time)
    {
        first_time  = false;
        if(need_clear)  { codepoints.clear(); need_clear = false; }

        while(true)
        {
            child.op->next(t);
            if(t.is_eos()) break;

            tuple_cell tc = atomize(child.get(t));
            xmlscm_type xtype = tc.get_atomic_type();

            if(!(xtype == xs_untypedAtomic ||
                 xtype == xs_integer       ||
                 is_derived_from_xs_integer(xtype)))
                     throw XQUERY_EXCEPTION2(XPTY0004, "Invalid item type in the argument of fn:codepoints-to-string (xs:untypedAtomic, xs:integer or derived expected).");

            int64_t value = (xtype == xs_untypedAtomic ?
                             cast(tc, xs_integer).get_xs_integer() :
                             tc.get_xs_integer());

            if(value > 0x10FFFF || !isXML10Valid(value))
                throw XQUERY_EXCEPTION2(FOCH0001, "Invalid codepoint in the argument of fn:codepoints-to-string.");

            codepoints.push_back(value);
        }

        stmt_str_buf result;

        for(unsigned int i = 0; i < codepoints.size(); i++)
            result.append(utf8_encode_char(codepoints[i]));

        t.copy(result.get_tuple_cell());
    }
	else
    {
        first_time = true;
        need_clear = true;
        t.set_eos();
    }
}

PPIterator* PPFnCodePoints2String::do_copy(dynamic_context *_cxt_)
{
    PPFnCodePoints2String *res = new PPFnCodePoints2String(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnCodePoints2String::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnTranslate
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnTranslate::PPFnTranslate(dynamic_context *_cxt_,
                             operation_info _info_,
							 PPOpIn _str_,
                             PPOpIn _map_str_,
                             PPOpIn _trans_str_) : PPIterator(_cxt_, _info_, "PPFnTranslate"),
								                   str(_str_),
                                                   map_str(_map_str_),
                                                   trans_str(_trans_str_)
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

void PPFnTranslate::do_open ()
{
	str.op->open();
	map_str.op->open();
	trans_str.op->open();
	first_time = true;
}

void PPFnTranslate::do_reopen()
{
	str.op->reopen();
	map_str.op->reopen();
	trans_str.op->reopen();
	first_time = true;
}

void PPFnTranslate::do_close()
{
	str.op->close();
	map_str.op->close();
	trans_str.op->close();
}

void PPFnTranslate::do_next (tuple &t)
{
	if (first_time)
	{
		first_time = false;

		map_str.op->next(t);
		if (t.is_eos())
			throw XQUERY_EXCEPTION2(XPTY0004, "2nd argument of fn:translate is empty sequence");
		tuple_cell map_tc = map_str.get(t);
		map_tc = atomize(map_tc);
		if (map_tc.get_atomic_type()==xs_untypedAtomic)
			map_tc.set_xtype(xs_string);
		else
			if (!is_string_type(map_tc.get_atomic_type()))
				throw XQUERY_EXCEPTION2(XPTY0004, "2nd argument of fn:translate is not a string");
		map_tc = tuple_cell::make_sure_light_atomic(map_tc);
		map_str.op->next(t);
		if (!t.is_eos())
			throw XQUERY_EXCEPTION2(XPTY0004, "2nd argument of fn:translate is not atomic value");

		trans_str.op->next(t);
		if (t.is_eos())
			throw XQUERY_EXCEPTION2(XPTY0004, "3rd argument of fn:translate is empty sequence");
		tuple_cell trans_tc = trans_str.get(t);
		trans_tc = atomize(trans_tc);
		if (trans_tc.get_atomic_type()==xs_untypedAtomic)
			trans_tc.set_xtype(xs_string);
		else
			if (!is_string_type(trans_tc.get_atomic_type()))
				throw XQUERY_EXCEPTION2(XPTY0004, "3rd argument of fn:translate is not a string");
		trans_tc = tuple_cell::make_sure_light_atomic(trans_tc);
		trans_str.op->next(t);
		if (!t.is_eos())
			throw XQUERY_EXCEPTION2(XPTY0004, "3rd argument of fn:translate is not atomic value");

		str.op->next(t);
		if (!t.is_eos())
		{
			tuple_cell tc = str.get(t);
			tc = atomize(tc);
			if (!is_string_type(tc.get_atomic_type()))
				throw XQUERY_EXCEPTION2(XPTY0004, "1st argument of fn:translate is not a string");
			str.op->next(t);
			if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Length of sequence passed to fn:translate as 1st argument is more than 1");

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

PPIterator* PPFnTranslate::do_copy(dynamic_context *_cxt_)
{
	PPFnTranslate *res = new PPFnTranslate(_cxt_, info, str, map_str, trans_str);
	res->str.op = str.op->copy(_cxt_);
	res->map_str.op = map_str.op->copy(_cxt_);
	res->trans_str.op = trans_str.op->copy(_cxt_);
	return res;
}

void PPFnTranslate::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    str.op->accept(v);
	map_str.op->accept(v);
	trans_str.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnSubsBeforeAfter
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnSubsBeforeAfter::PPFnSubsBeforeAfter(dynamic_context *_cxt_,
                                         operation_info _info_,
                                         PPOpIn _src_child_,
                                         PPOpIn _srch_child_,
                                         PPFnSubsBeforeAfter::FunctionType _type_) : PPIterator(_cxt_, _info_, "PPFnSubsBeforeAfter"),
                                                                                     src_child(_src_child_),
                                                                                     srch_child(_srch_child_),
                                                                                     type(_type_)
{
}

PPFnSubsBeforeAfter::PPFnSubsBeforeAfter(dynamic_context *_cxt_,
                                         operation_info _info_,
                                         PPOpIn _src_child_,
                                         PPOpIn _srch_child_,
                                         PPOpIn _collation_child_,
                                         PPFnSubsBeforeAfter::FunctionType _type_) : PPIterator(_cxt_, _info_, "PPFnSubsBeforeAfter"),
                                                                                     src_child(_src_child_),
                                                                                     srch_child(_srch_child_),
                                                                                     collation_child(_collation_child_),
                                                                                     type(_type_)
{
}

PPFnSubsBeforeAfter::~PPFnSubsBeforeAfter()
{
    delete src_child.op;
    src_child.op = NULL;
    delete srch_child.op;
    srch_child.op = NULL;
    if (collation_child.op)
    {
        delete collation_child.op;
        collation_child.op = NULL;
    }
}

void PPFnSubsBeforeAfter::do_open ()
{
    src_child.op->open();
    srch_child.op->open();
    if (collation_child.op)
        collation_child.op->open();
    handler = NULL;
}

void PPFnSubsBeforeAfter::do_reopen()
{
    src_child.op->reopen();
    srch_child.op->reopen();
    if (collation_child.op)
        collation_child.op->reopen();
    handler = NULL;
}

void PPFnSubsBeforeAfter::do_close()
{
    src_child.op->close();
    srch_child.op->close();
    if (collation_child.op)
        collation_child.op->close();
    handler = NULL;
}

///////////////////////////////////////////////////////////////////////////////////////////
/// We need byte based version of substring because PPSubsMatch::contains is byte based.
template <class Iterator>
static inline void byte_based_substring_call_stub(Iterator &start, const Iterator &end, stmt_str_buf &res, int64_t start_pos, int64_t length)
{
    while (start < end && start_pos > 0) { start_pos--; start++; }

    while (start < end && length > 0)
    {
        res << (*start);
        start++;
        length--;
    }
}

static inline tuple_cell byte_based_substring(const tuple_cell *tc, int64_t start_pos, int64_t length)
{
	stmt_str_buf res;
	STRING_ITERATOR_CALL_TEMPLATE_1tcptr_3p(byte_based_substring_call_stub, tc, res, start_pos, length);
	return res.get_tuple_cell();
}
///////////////////////////////////////////////////////////////////////////////////////////


void PPFnSubsBeforeAfter::do_next(tuple &t)
{
    if (handler) // the same as '!first_time'
    {
        handler = NULL;
        t.set_eos();
        return;
    }

    handler = charset_handler->get_unicode_codepoint_collation();

    if (collation_child.op)
    {
        collation_child.op->next(t);
        if(t.is_eos()) error("Invalid arity of the third argument. Argument contains zero items");

        tuple_cell col = atomize(collation_child.get(t));
        if (!is_string_type(col.get_atomic_type()))
            error("Invalid type of the third argument (xs_string/derived/promotable is expected)");

        collation_child.op->next(t);
        if (!t.is_eos()) error("Invalid arity of the third argument. Argument contains more than one item");

        col = tuple_cell::make_sure_light_atomic(col);

        int res = cxt->get_static_context()->get_collation(col.get_str_mem(), &handler);
        if(res != 0) throw XQUERY_EXCEPTION2(FOCH0002, (static_context::get_error_description(res) + ".").c_str());
    }

    src_child.op->next(t);
    tuple_cell src;
    int64_t src_len = 0;

    if (!t.is_eos())
    {
        src = atomize(src_child.get(t));

        if(!is_string_type(src.get_atomic_type())) error("Invalid type of the first argument (xs_string/derived/promotable is expected) ");

        src_child.op->next(t);
        if (!t.is_eos()) error("Invalid arity of the first argument. Argument contains more than one item ");
        src_len = src.get_strlen();
    }

    srch_child.op->next(t);
    tuple_cell srch_str;
    int64_t srch_str_len = 0;

    if (!t.is_eos())
    {
        srch_str = atomize(srch_child.get(t));

        if(!is_string_type(srch_str.get_atomic_type())) error("Invalid type of the second argument (xs_string/derived/promotable is expected) ");

        srch_child.op->next(t);
        if (!t.is_eos()) error("Invalid arity of the second argument. Argument contains more than one item ");
        srch_str_len = srch_str.get_strlen();
    }

    if(src_len == 0 || srch_str_len >= src_len)
    {
        t.copy(EMPTY_STRING_TC);
    }
    else if(srch_str_len == 0)
    {
        type == PPFnSubsBeforeAfter::FN_BEFORE ? t.copy(EMPTY_STRING_TC) : t.copy(src);
    }
    else
    {
        int64_t pos = handler->contains(&src, &srch_str);
        if(pos >= 0)
        {
            type == PPFnSubsBeforeAfter::FN_BEFORE ?
                    t.copy(byte_based_substring(&src, 0, pos)) :
                    t.copy(byte_based_substring(&src, pos + srch_str_len, INT64_MAX));
        }
        else
            t.copy(EMPTY_STRING_TC);
    }
}

void PPFnSubsBeforeAfter::error(const char* msg)
{
    if(type == PPFnSubsBeforeAfter::FN_BEFORE)
        throw XQUERY_EXCEPTION2(XPTY0004, (std::string(msg) + " in fn:substring-before().").c_str());
    else if(type == PPFnSubsBeforeAfter::FN_AFTER)
        throw XQUERY_EXCEPTION2(XPTY0004, (std::string(msg) + " in fn:substring-after().").c_str());
    else
        throw XQUERY_EXCEPTION2(SE1003, "Imposible type of function in PPFnSubsBeforeAfter::error().");
}


PPIterator* PPFnSubsBeforeAfter::do_copy(dynamic_context *_cxt_)
{
    PPFnSubsBeforeAfter *res = new PPFnSubsBeforeAfter(_cxt_, info, src_child, srch_child, collation_child, type);
    res->src_child.op = src_child.op->copy(_cxt_);
    res->srch_child.op = srch_child.op->copy(_cxt_);
    if (collation_child.op)
        res->collation_child.op = collation_child.op->copy(_cxt_);
    return res;
}

void PPFnSubsBeforeAfter::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    src_child.op->accept(v);
    srch_child.op->accept(v);
    if (collation_child.op)
        collation_child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnChangeCase
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

PPFnChangeCase::PPFnChangeCase(dynamic_context *_cxt_,
                               operation_info _info_,
                               PPOpIn _str_,
                               bool _to_upper_) : PPIterator(_cxt_, _info_, "PPFnChangeCase"),
                                                  str(_str_),
                                                  to_upper(_to_upper_)
{
}

PPFnChangeCase::~PPFnChangeCase()
{
	delete str.op;
	str.op = NULL;
}

void PPFnChangeCase::do_open ()
{
	str.op->open();
	first_time = true;
}

void PPFnChangeCase::do_reopen()
{
	str.op->reopen();
	first_time = true;
}

void PPFnChangeCase::do_close()
{
	str.op->close();
}

void PPFnChangeCase::do_next (tuple &t)
{
	if (first_time)
	{
		first_time = false;

		str.op->next(t);
		if (!t.is_eos())
		{
			const tuple_cell tc = atomize(str.get(t));
			if (!is_string_type(tc.get_atomic_type()))
				throw XQUERY_EXCEPTION2(XPTY0004, "1st argument of fn:upper-case or fn:lower-case is not a string"); //FIXME: make 2 sep. err. strings
			str.op->next(t);
			if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Length of sequence passed to fn:upper-case or fn:lower-case as 1st argument is more than 1"); //FIXME: make 2 sep. err. strings

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

PPIterator* PPFnChangeCase::do_copy(dynamic_context *_cxt_)
{
	PPFnChangeCase *res = new PPFnChangeCase(_cxt_, info, str, to_upper);
	res->str.op = str.op->copy(_cxt_);
	return res;
}

void PPFnChangeCase::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    str.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnStringLength
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnStringLength::PPFnStringLength(dynamic_context *_cxt_,
                                   operation_info _info_,
                                   PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnStringLength"),
                                                     child(_child_)
{
}

PPFnStringLength::~PPFnStringLength()
{
    delete child.op;
    child.op = NULL;
}

void PPFnStringLength::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnStringLength::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnStringLength::do_close()
{
    child.op->close();
}

void PPFnStringLength::do_next (tuple &t)
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
            if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Length of sequence passed to fn:string-length is more than 1");
        }

        t.copy(tuple_cell::atomic((int64_t)len));
    }
    else
    {
        first_time = true;
        t.set_eos();
    }
}

PPIterator* PPFnStringLength::do_copy(dynamic_context *_cxt_)
{
    PPFnStringLength *res = new PPFnStringLength(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnStringLength::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnNormalizeSpace
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnNormalizeSpace::PPFnNormalizeSpace(dynamic_context *_cxt_,
                                       operation_info _info_,
                                       PPOpIn _child_) : PPIterator(_cxt_, _info_, "PPFnNormalizeSpace"),
                                                         child(_child_)
{
}

PPFnNormalizeSpace::~PPFnNormalizeSpace()
{
    delete child.op;
    child.op = NULL;
}

void PPFnNormalizeSpace::do_open ()
{
    child.op->open();
    first_time = true;
}

void PPFnNormalizeSpace::do_reopen()
{
    child.op->reopen();
    first_time = true;
}

void PPFnNormalizeSpace::do_close()
{
    child.op->close();
}

void PPFnNormalizeSpace::do_next (tuple &t)
{
    if (first_time)
    {
        first_time = false;

        child.op->next(t);
        if (!t.is_eos())
        {
            tuple_cell tc = atomize(child.get(t));

            if(!is_string_type(tc.get_atomic_type())) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the argument in fn:normalize-space (xs_string/derived/promotable is expected).");

            child.op->next(t);
            if (!t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the argument in fn:normalize-space. Argument contains more than one item.");

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

PPIterator* PPFnNormalizeSpace::do_copy(dynamic_context *_cxt_)
{
    PPFnNormalizeSpace *res = new PPFnNormalizeSpace(_cxt_, info, child);
    res->child.op = child.op->copy(_cxt_);
    return res;
}

void PPFnNormalizeSpace::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    child.op->accept(v);
    v.pop();
}

///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnSubstring
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnSubstring::PPFnSubstring(dynamic_context *_cxt_,
                             operation_info _info_,
                             PPOpIn _str_child_,
                             PPOpIn _start_child_) : PPIterator(_cxt_, _info_, "PPFnSubstring"),
                                                     str_child(_str_child_),
                                                     start_child(_start_child_),
                                                     is_length(false)
{
}

PPFnSubstring::PPFnSubstring(dynamic_context *_cxt_,
                             operation_info _info_,
                             PPOpIn _str_child_,
                             PPOpIn _start_child_,
                             PPOpIn _length_child_) : PPIterator(_cxt_, _info_, "PPFnSubstring"),
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

void PPFnSubstring::do_open ()
{
    str_child.op->open();
    start_child.op->open();
    if(is_length) length_child.op->open();
    first_time = true;
}

void PPFnSubstring::do_reopen()
{
    str_child.op->reopen();
    start_child.op->reopen();
    if(is_length) length_child.op->reopen();
    first_time = true;
}

void PPFnSubstring::do_close()
{
    str_child.op->close();
    start_child.op->close();
    if(is_length) length_child.op->close();

}

void PPFnSubstring::do_next(tuple &t)
{
    int64_t start_pos = 0;
    int64_t length = 0;
    if (first_time)
    {
        tuple_cell tc;

        first_time = false;

        start_child.op->next(t);
        if (t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Empty second argument is not allowed in fn:substring.");

        tc = atomize(start_child.get(t));
        xmlscm_type xtype = tc.get_atomic_type();

        if(!is_numeric_type(xtype) && !(xtype == xs_untypedAtomic))
            throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the second argument in fn:substring (xs:double or promotable expected).");

        double temp = floor(cast(tc, xs_double).get_xs_double() + 0.5);  //floor(x+0.5) is equal there to fn:round
        start_pos = u_is_nan(temp) ? INT64_MAX : u_double2int64(temp);

        start_child.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid cardinality of the second argument in fn:substring.");

        if(is_length)
        {
            length_child.op->next(t);
            if (t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Empty third argument is not allowed in fn:substring.");

            tc = atomize(start_child.get(t));
            xtype = tc.get_atomic_type();

            if(!is_numeric_type(xtype) && !(xtype == xs_untypedAtomic))
                throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the third argument in fn:substring (xs:double or promotable expected).");

            /// u_double2int64(NaN) is 0, it is ok for us
            length = u_double2int64(floor(cast(tc, xs_double).get_xs_double() + 0.5)); //floor(x+0.5) is equal there to fn:round

            length_child.op->next(t);
            if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Invalid cardinality of the third argument in fn:substring.");
        }

        str_child.op->next(t);
        if (t.is_eos())
        {
             t.copy(EMPTY_STRING_TC);
             return;
        }
        tc = atomize(str_child.get(t));
        if (!is_string_type(tc.get_atomic_type()))
        	throw XQUERY_EXCEPTION2(XPTY0004, "1st argument of fn:substring is not a string");
        str_child.op->next(t);
        if (!(t.is_eos())) throw XQUERY_EXCEPTION2(XPTY0004, "Length of sequence passed to fn:substring as 1st argument is more than 1");

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
            t.copy(charset_handler->substring(&tc, start_pos-1, INT64_MAX));
    }
    else
    {
        t.set_eos();
        first_time = true;
    }
}

PPIterator* PPFnSubstring::do_copy(dynamic_context *_cxt_)
{
    PPFnSubstring *res = is_length ? new PPFnSubstring(_cxt_, info, str_child, start_child, length_child) :
                                     new PPFnSubstring(_cxt_, info, str_child, start_child);

    res->str_child.op = str_child.op->copy(_cxt_);
    res->start_child.op = start_child.op->copy(_cxt_);
    if(is_length) res->length_child.op = length_child.op->copy(_cxt_);
    return res;
}

void PPFnSubstring::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    str_child.op->accept(v);
    start_child.op->accept(v);
    if(is_length) length_child.op->accept(v);
    v.pop();
}


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
/// PPFnNormalizeUnicode
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
PPFnNormalizeUnicode::PPFnNormalizeUnicode(dynamic_context *_cxt_,
                             operation_info _info_,
                             PPOpIn _str_child_,
                             PPOpIn _type_child_) : PPIterator(_cxt_, _info_, "PPFnNormalizeUnicode"),
                                                    str_child(_str_child_),
                                                    type_child(_type_child_)
{
}

PPFnNormalizeUnicode::PPFnNormalizeUnicode(dynamic_context *_cxt_,
                             operation_info _info_,
                             PPOpIn _str_child_) : PPIterator(_cxt_, _info_, "PPFnNormalizeUnicode"),
                                                   str_child(_str_child_)
{
}

PPFnNormalizeUnicode::~PPFnNormalizeUnicode()
{
    delete str_child.op;
    str_child.op = NULL;

    if (type_child.op)
    {
        delete type_child.op;
        type_child.op = NULL;
    }
}

void PPFnNormalizeUnicode::do_open ()
{
    str_child.op->open();

    if (type_child.op)
        type_child.op->open();

    first_time = true;
}

void PPFnNormalizeUnicode::do_reopen()
{
    str_child.op->reopen();

    if (type_child.op)
        type_child.op->reopen();

    first_time = true;
}

void PPFnNormalizeUnicode::do_close()
{
    str_child.op->close();

    if (type_child.op)
        type_child.op->close();

    first_time = true;
}

void PPFnNormalizeUnicode::do_next(tuple &t)
{
    if (first_time)
    {
        tuple_cell str_tc;
        char *type = (char *)"NFC";

        first_time = false;

        str_child.op->next(t);
        if (t.is_eos())
        {
            t.copy(EMPTY_STRING_TC);
            return;
        }

        str_tc = atomize(str_child.get(t));

        if (!is_string_type(str_tc.get_atomic_type()))
            throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the first argument to fn:normalize-unicode() (xs:string is expected)");

        str_child.op->next(t);
        if (!t.is_eos())
            throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the first argument of fn:normalize-unicode(). Argument contains more than one item.");

        if(type_child.op)
        {
            tuple_cell type_tc;

            type_child.op->next(t);
            if (t.is_eos()) throw XQUERY_EXCEPTION2(XPTY0004, "Empty sequence as second argument is not allowed in fn:normalize-unicode().");

            type_tc = atomize(type_child.get(t));

            if(!is_string_type(type_tc.get_atomic_type()))
                throw XQUERY_EXCEPTION2(XPTY0004, "Invalid type of the second argument in fn:normalize-unicode() (xs:string is expected).");

            type_child.op->next(t);
            if (!t.is_eos())
                throw XQUERY_EXCEPTION2(XPTY0004, "Invalid arity of the second argument of fn:normalize-unicode(). Argument contains more than one item.");

            type_tc = tuple_cell::make_sure_light_atomic(type_tc);
            type = type_tc.get_str_mem();

            if (strcmp(type, "NFC"))
                throw XQUERY_EXCEPTION2(FOCH0003, (std::string("Normalization form ") + type + " is not supported").c_str());
        }

        // for now, NFC normalization form just copies the string;
        // TODO: make it proper
        t.copy(str_tc);
    }
    else
    {
        t.set_eos();
        first_time = true;
    }
}

PPIterator* PPFnNormalizeUnicode::do_copy(dynamic_context *_cxt_)
{
    PPFnNormalizeUnicode *res = type_child.op ? new PPFnNormalizeUnicode(_cxt_, info, str_child, type_child) :
                                                new PPFnNormalizeUnicode(_cxt_, info, str_child);

    res->str_child.op = str_child.op->copy(_cxt_);

    if (type_child.op)
        res->type_child.op = type_child.op->copy(_cxt_);

    return res;
}

void PPFnNormalizeUnicode::do_accept(PPVisitor &v)
{
    v.visit (this);
    v.push  (this);
    str_child.op->accept(v);
    if(type_child.op) 
        type_child.op->accept(v);
    v.pop();
}
