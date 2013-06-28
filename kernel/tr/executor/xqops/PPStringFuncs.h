/*
 * File: PPStringFuncs.h
 * Copyright (C) 2004 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
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

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnConcat(dynamic_context *_cxt_,
               operation_info _info_,
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

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnStringJoin(dynamic_context *_cxt_,
                   operation_info _info_,
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
    
    static inline const char* FunctionType2c_string(FunctionType type)
    {
        switch(type)
        {
        case FN_STARTS_WITH: return "fn:starts-with()";
        case FN_ENDS_WITH: return "fn:ends-with()";
        default: throw USER_EXCEPTION2(SE1003, "Impossible case in function type to string conversion (starts/ends with)");
        }
    }

protected:
    PPOpIn source;
    PPOpIn prefix;
    PPOpIn collation;

    FunctionType type;

    bool is_collation;
    bool first_time;

    void error(const char* msg);

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnStartsEndsWith(dynamic_context *_cxt_,
                       operation_info _info_,
                       PPOpIn _source_,
                       PPOpIn _prefix_,
                       FunctionType _type_);

    PPFnStartsEndsWith(dynamic_context *_cxt_,
                       operation_info _info_,
                       PPOpIn _source_,
                       PPOpIn _prefix_,
                       PPOpIn _collation_,
                       FunctionType _type_);

    virtual ~PPFnStartsEndsWith();
    
    inline FunctionType get_function_type() const { return type; }
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnStringLength
///////////////////////////////////////////////////////////////////////////////
class PPFnStringLength : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnStringLength(dynamic_context *_cxt_,
                     operation_info _info_,
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

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnNormalizeSpace(dynamic_context *_cxt_,
                       operation_info _info_,
                       PPOpIn _child_);
    virtual ~PPFnNormalizeSpace();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnString2CodePoints
///////////////////////////////////////////////////////////////////////////////
class PPFnString2CodePoints : public PPIterator
{
protected:
    PPOpIn child;
    bool first_time;
    unicode_cp_iterator *ucp_it;
    tuple_cell in_str;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnString2CodePoints(dynamic_context *_cxt_,
                          operation_info _info_,
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

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnCodePoints2String(dynamic_context *_cxt_,
                          operation_info _info_,
                          PPOpIn _child_);
    virtual ~PPFnCodePoints2String();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnTranslate
///////////////////////////////////////////////////////////////////////////////
class PPFnTranslate : public PPIterator
{
protected:
    PPOpIn str, map_str, trans_str;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnTranslate(dynamic_context *_cxt_,
            operation_info _info_,
            PPOpIn _str_,
            PPOpIn _map_str_,
            PPOpIn _trans_str_);
    virtual ~PPFnTranslate();
};


///////////////////////////////////////////////////////////////////////////////
/// PPFnUpperCase and PPFnLowerCase
///////////////////////////////////////////////////////////////////////////////
class PPFnChangeCase : public PPIterator
{
protected:
    PPOpIn str;
    bool to_upper;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnChangeCase(dynamic_context *_cxt_,
                   operation_info _info_,
                   PPOpIn _str_,
                   bool _to_upper_);
    virtual ~PPFnChangeCase();
    inline bool is_to_upper() const { return to_upper; }
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

    static inline const char* FunctionType2c_string(FunctionType type)
    {
        switch(type)
        {
        case FN_BEFORE: return "fn:starts-with()";
        case FN_AFTER: return "fn:ends-with()";
        default: throw USER_EXCEPTION2(SE1003, "Impossible case in function type to string conversion (substring before/after)");
        }
    }

protected:
    PPOpIn src_child;
    PPOpIn srch_child;
    PPOpIn collation_child;

    FunctionType type;

    CollationHandler* handler;
    void error(const char* msg);

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnSubsBeforeAfter(dynamic_context *_cxt_,
                        operation_info _info_,
                        PPOpIn _src_child_,
                        PPOpIn _srch_child_,
                        FunctionType _type_);

    PPFnSubsBeforeAfter(dynamic_context *_cxt_,
                        operation_info _info_,
                        PPOpIn _src_child_,
                        PPOpIn _srch_child_,
                        PPOpIn _collation_child_,
                        FunctionType _type_);
    virtual ~PPFnSubsBeforeAfter();
 
    inline FunctionType get_function_type() const { return type; }
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

    bool is_length;             //equal to length_child.op != NULL;
    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnSubstring(dynamic_context *_cxt_,
                  operation_info _info_,
                  PPOpIn _str_child_,
                  PPOpIn _start_child_);

    PPFnSubstring(dynamic_context *_cxt_,
                  operation_info _info_,
                  PPOpIn _str_child_,
                  PPOpIn _start_child_,
                  PPOpIn _length_child_);

    virtual ~PPFnSubstring();
};

///////////////////////////////////////////////////////////////////////////////
/// PPFnNormalizeUnicode
///////////////////////////////////////////////////////////////////////////////
class PPFnNormalizeUnicode : public PPIterator
{
protected:
    PPOpIn str_child;
    PPOpIn type_child;

    bool first_time;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t);
    virtual void do_accept (PPVisitor &v);
    
    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPFnNormalizeUnicode(dynamic_context *_cxt_,
                  operation_info _info_,
                  PPOpIn _str_child_,
                  PPOpIn _type_child_);

    PPFnNormalizeUnicode(dynamic_context *_cxt_,
                  operation_info _info_,
                  PPOpIn _str_child_);

    virtual ~PPFnNormalizeUnicode();
};

#endif /* _PPSTRINGFUNCS_H */
