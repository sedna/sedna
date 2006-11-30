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
/// PPFnStringJoin
///////////////////////////////////////////////////////////////////////////////
class PPFnStringJoin : public PPIterator
{
private:
    PPOpIn members;
    PPOpIn separator;
    bool first_time;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator *copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnStringJoin(variable_context *_cxt_,
                   PPOpIn _members_,
                   PPOpIn _separator_);
    virtual ~PPFnStringJoin();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnStartsEndsWith
///////////////////////////////////////////////////////////////////////////////
class PPFnStartsEndsWith : public PPIterator
{
public:
    enum FunctionType
    {
        FN_STARTS_WITH,
        FN_ENDS_WITH
    };

private:
    PPOpIn source;
    PPOpIn prefix;
    PPOpIn collation;
    
    FunctionType type;

    bool is_collation;
    bool first_time;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator *copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    void error(const char* msg);
    
    PPFnStartsEndsWith(variable_context *_cxt_,
                       PPOpIn _source_,
                       PPOpIn _prefix_,
                       FunctionType _type_);
    PPFnStartsEndsWith(variable_context *_cxt_,
                       PPOpIn _source_,
                       PPOpIn _prefix_,
                       PPOpIn _collation_,
                       FunctionType _type_);
 
    virtual ~PPFnStartsEndsWith();
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
/// PPFnNormalizeSpace
///////////////////////////////////////////////////////////////////////////////
class PPFnNormalizeSpace : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnNormalizeSpace(variable_context *_cxt_, 
                       PPOpIn _child_);
    virtual ~PPFnNormalizeSpace();
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
	unicode_cp_iterator *ucp_it;
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
/// PPFnCodePoints2String
///////////////////////////////////////////////////////////////////////////////
class PPFnCodePoints2String : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;

    std::vector<int> codepoints;
	
	void children(PPOpIn &_child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnCodePoints2String(variable_context *_cxt_, 
                          PPOpIn _child_);
    virtual ~PPFnCodePoints2String();
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
/// PPFnUpperCase and PPFnLowerCase
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

///////////////////////////////////////////////////////////////////////////////
/// PPFnSubstring
///////////////////////////////////////////////////////////////////////////////
class PPFnSubstring : public PPIterator
{
protected:
    PPOpIn str_child;
    PPOpIn start_child;
    PPOpIn length_child;
    
    bool is_length; 		//equal to length_child.op != NULL;
    bool first_time;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPFnSubstring(variable_context *_cxt_, 
                  PPOpIn _str_child_,
                  PPOpIn _start_child_);

    PPFnSubstring(variable_context *_cxt_, 
                  PPOpIn _str_child_,
                  PPOpIn _start_child_,
                  PPOpIn _length_child_);

    virtual ~PPFnSubstring();
};


#endif
