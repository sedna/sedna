/*
 * File:  PPStringFuncs.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPSTRINGFUNCS_H
#define _PPSTRINGFUNCS_H

#include "sedna.h"
#include "PPBase.h"


///////////////////////////////////////////////////////////////////////////////
/// PPFnConcat
///////////////////////////////////////////////////////////////////////////////
class PPFnConcat : public PPIterator
{
private:
    int i;
    arr_of_PPOpIn ch_arr;
    arr_of_tuple_pointer data;
    std::vector<tuple_cell> tcv;
    bool first_time;

    void children(arr_of_PPOpIn &_ch_arr_)
    {
        _ch_arr_ = ch_arr;
    }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator *copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnConcat(variable_context *_cxt_,
               arr_of_PPOpIn _ch_arr_);
    virtual ~PPFnConcat();
};



///////////////////////////////////////////////////////////////////////////////
/// PPFnStringLength
///////////////////////////////////////////////////////////////////////////////
class PPFnStringLength : public PPIterator
{
protected:
    // obtained parameters and local data
    PPOpIn child;
    bool first_time;

    void children(PPOpIn &_child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnStringLength(variable_context *_cxt_, 
                     PPOpIn _child_);
    virtual ~PPFnStringLength();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnString2CodePoints
///////////////////////////////////////////////////////////////////////////////
class PPFnString2CodePoints : public PPIterator
{
protected:
    // obtained parameters and local data
    PPOpIn child;
    bool first_time;
	int position;
	//int size;
	//int length;
	tuple_cell in_str;

    void children(PPOpIn &_child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnString2CodePoints(variable_context *_cxt_, 
                     PPOpIn _child_);
    virtual ~PPFnString2CodePoints();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnTranslate
///////////////////////////////////////////////////////////////////////////////
class PPFnTranslate : public PPIterator
{
protected:
	// obtained parameters and local data
	PPOpIn str, map_str, trans_str;
	bool first_time;

public:
	virtual void open   ();
	virtual void reopen ();
	virtual void close  ();
	virtual strict_fun res_fun () { return result; };
	virtual void next   (tuple &t);

	virtual PPIterator* copy(variable_context *_cxt_);
	static bool result(PPIterator* cur, variable_context *cxt, void*& r);

	PPFnTranslate(variable_context *_cxt_, 
		PPOpIn _str_, PPOpIn _map_str_, PPOpIn _trans_str_);
	virtual ~PPFnTranslate();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnTranslate
///////////////////////////////////////////////////////////////////////////////
class PPFnChangeCase : public PPIterator
{
protected:
	// obtained parameters and local data
	PPOpIn str;
	bool to_upper;
	bool first_time;

public:
	virtual void open   ();
	virtual void reopen ();
	virtual void close  ();
	virtual strict_fun res_fun () { return result; };
	virtual void next   (tuple &t);

	virtual PPIterator* copy(variable_context *_cxt_);
	static bool result(PPIterator* cur, variable_context *cxt, void*& r);

	PPFnChangeCase(variable_context *_cxt_, 
		PPOpIn _str_, bool _to_upper_);
	virtual ~PPFnChangeCase();
};


#endif