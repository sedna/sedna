/*
 * File:  PPStringFuncs.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPSTRINGFUNCS_H
#define _PPSTRINGFUNCS_H

#include "common/sedna.h"
#include "tr/executor/base/PPBase.h"


///////////////////////////////////////////////////////////////////////////////
/// PPFnConcat
///////////////////////////////////////////////////////////////////////////////
class PPFnConcat : public PPIterator
{
private:
    unsigned int i;
    arr_of_PPOpIn ch_arr;
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

    virtual PPIterator *copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnConcat(dynamic_context *_cxt_,
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

    bool need_clear;
    std::vector<tuple_cell> tcv;

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator *copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnStringJoin(dynamic_context *_cxt_,
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

    virtual PPIterator *copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    void error(const char* msg);
    
    PPFnStartsEndsWith(dynamic_context *_cxt_,
                       PPOpIn _source_,
                       PPOpIn _prefix_,
                       FunctionType _type_);
    PPFnStartsEndsWith(dynamic_context *_cxt_,
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

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnStringLength(dynamic_context *_cxt_, 
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

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnNormalizeSpace(dynamic_context *_cxt_, 
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

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnString2CodePoints(dynamic_context *_cxt_, 
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

    bool need_clear;
    std::vector<int> codepoints;
	
	void children(PPOpIn &_child_) { _child_ = child; }

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnCodePoints2String(dynamic_context *_cxt_, 
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

	virtual PPIterator* copy(dynamic_context *_cxt_);
	static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

	PPFnTranslate(dynamic_context *_cxt_, 
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

	virtual PPIterator* copy(dynamic_context *_cxt_);
	static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

	PPFnChangeCase(dynamic_context *_cxt_, 
		PPOpIn _str_, bool _to_upper_);
	virtual ~PPFnChangeCase();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnSubstringBefore and PPFnSubstringAfter
///////////////////////////////////////////////////////////////////////////////
class PPFnSubsBeforeAfter : public PPIterator
{
public:
    enum FunctionType
    {
        FN_BEFORE,
        FN_AFTER
    };

protected:
    PPOpIn src_child;
    PPOpIn srch_child;
    PPOpIn collation_child;

    FunctionType type;

    CollationHandler* handler;

    void children(PPOpIn &_src_child_, PPOpIn &_srch_child_, PPOpIn &_collation_child_) 
        { _src_child_ = src_child; _srch_child_ = srch_child; _collation_child_ = collation_child; }

    void error(const char* msg);

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnSubsBeforeAfter(dynamic_context *_cxt_,
                PPOpIn _src_child_,
                PPOpIn _srch_child_,
                FunctionType _type_);
    PPFnSubsBeforeAfter(dynamic_context *_cxt_,
                PPOpIn _src_child_,
                PPOpIn _srch_child_,
                PPOpIn _collation_child_,
                FunctionType _type_);
    virtual ~PPFnSubsBeforeAfter();
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

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPFnSubstring(dynamic_context *_cxt_, 
                  PPOpIn _str_child_,
                  PPOpIn _start_child_);

    PPFnSubstring(dynamic_context *_cxt_, 
                  PPOpIn _str_child_,
                  PPOpIn _start_child_,
                  PPOpIn _length_child_);

    virtual ~PPFnSubstring();
};


#endif
