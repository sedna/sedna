/*
 * File:  PPConstructors.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPCONSTRUCTORS_H
#define _PPCONSTRUCTORS_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/structures/schema.h"
#include "tr/strings/strings.h"
#include "tr/structures/indirection.h"
#include "tr/mo/micro.h"
///////////////////////////////////////////////////////////////////////////////
/// PPConstructor
///////////////////////////////////////////////////////////////////////////////
class PPConstructor : public PPIterator
{
protected:
    // obtained parameters and local data
    bool first_time;
    bool eos_reached;
	static bool firstCons;
	static schema_node_cptr root_schema;
	static xptr virt_root;
	static xptr last_elem;
	static xptr cont_parind;
	static xptr cont_leftind;
	static int conscnt;
	bool deep_copy;
public:
	static bool checkInitial();
	virtual void open   ();
	static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);
    PPConstructor(dynamic_context *_cxt_,bool _deep_copy) : PPIterator(_cxt_),deep_copy(_deep_copy) {};
friend xptr copy_to_temp(xptr node);
friend void clear_virtual_root();
};

void clear_virtual_root();

///////////////////////////////////////////////////////////////////////////////
/// PPElementConstructor
///////////////////////////////////////////////////////////////////////////////
class PPElementConstructor : public PPConstructor
{
protected:
    // obtained parameters and local data
    PPOpIn qname;
    PPOpIn content;
    char* el_name;
    bool ns_inside;

    void children(PPOpIn &_qname_,PPOpIn &_content_) { _qname_ = qname;_content_=content ;}
public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPElementConstructor(dynamic_context *_cxt_, 
            PPOpIn _qname_, PPOpIn _content_,bool _deep_copy, bool _ns_inside);
    PPElementConstructor(dynamic_context *_cxt_,
           const char* name, PPOpIn _content_,bool _deep_copy, bool _ns_inside);
    virtual ~PPElementConstructor();
};

///////////////////////////////////////////////////////////////////////////////
/// PPAttributeConstructor
///////////////////////////////////////////////////////////////////////////////
class PPAttributeConstructor : public PPConstructor
{
protected:
    // obtained parameters and local data
    PPOpIn qname;
	PPOpIn content;
	char* at_name;
	char* at_value;
	
    

    void children(PPOpIn &_qname_,PPOpIn &_content_) { _qname_ = qname;_content_=content ;}

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPAttributeConstructor(dynamic_context *_cxt_, 
            PPOpIn _qname_, PPOpIn _content_,bool _deep_copy);
	PPAttributeConstructor(dynamic_context *_cxt_, 
           const char* name, PPOpIn _content_,bool _deep_copy);
	PPAttributeConstructor(dynamic_context *_cxt_, 
            PPOpIn _qname_, const char* value,bool _deep_copy);
	PPAttributeConstructor(dynamic_context *_cxt_, 
           const char* name, const char* value,bool _deep_copy);
    virtual ~PPAttributeConstructor();
};
///////////////////////////////////////////////////////////////////////////////
/// PPNamespaceConstructor
///////////////////////////////////////////////////////////////////////////////
class PPNamespaceConstructor : public PPConstructor
{
protected:
    // obtained parameters and local data
    PPOpIn content;
	char* at_name;
	char* at_value;
	
    

    void children(PPOpIn &_content_) {_content_=content ;}

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPNamespaceConstructor(dynamic_context *_cxt_, 
           const char* name, PPOpIn _content_);
	PPNamespaceConstructor(dynamic_context *_cxt_, 
           const char* name, const char* value);
    virtual ~PPNamespaceConstructor();
};

///////////////////////////////////////////////////////////////////////////////
/// PPCommentConstructor
///////////////////////////////////////////////////////////////////////////////
class PPCommentConstructor : public PPConstructor
{
protected:
    // obtained parameters and local data
    PPOpIn content;
	char* at_value;
	StrMatcher strm;
    

    void children(PPOpIn &_content_) {if (at_value==NULL)_content_=content ;}

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPCommentConstructor(dynamic_context *_cxt_, 
            PPOpIn _content_,bool _deep_copy);
	PPCommentConstructor(dynamic_context *_cxt_, 
            const char* value,bool _deep_copy);
    virtual ~PPCommentConstructor();
};

///////////////////////////////////////////////////////////////////////////////
/// PPTextConstructor
///////////////////////////////////////////////////////////////////////////////
class PPTextConstructor : public PPConstructor
{
protected:
    // obtained parameters and local data
    PPOpIn content;
	char* at_value;
	
    

    void children(PPOpIn &_content_) {if (at_value==NULL)_content_=content ;}

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPTextConstructor(dynamic_context *_cxt_, 
            PPOpIn _content_,bool _deep_copy);
	PPTextConstructor(dynamic_context *_cxt_, 
            const char* value,bool _deep_copy);
    virtual ~PPTextConstructor();
};

///////////////////////////////////////////////////////////////////////////////
/// PPDocumentConstructor
///////////////////////////////////////////////////////////////////////////////
class PPDocumentConstructor : public PPConstructor
{
protected:
    // obtained parameters and local data
    PPOpIn content;
	void children(PPOpIn &_content_) {_content_=content ;}

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPDocumentConstructor(dynamic_context *_cxt_, 
            PPOpIn _content_);
    virtual ~PPDocumentConstructor();
};


///////////////////////////////////////////////////////////////////////////////
/// PPPIConstructor
///////////////////////////////////////////////////////////////////////////////
class PPPIConstructor : public PPConstructor
{
protected:
    // obtained parameters and local data
    PPOpIn qname;
	PPOpIn content;
	char* at_name;
	char* at_value;
	StrMatcher strm;
    

    void children(PPOpIn &_qname_,PPOpIn &_content_) { _qname_ = qname;_content_=content ;}

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(dynamic_context *_cxt_);
    static bool result(PPIterator* cur, dynamic_context *cxt, void*& r);

    PPPIConstructor(dynamic_context *_cxt_, 
            PPOpIn _qname_, PPOpIn _content_,bool _deep_copy);
	PPPIConstructor(dynamic_context *_cxt_, 
           const char* name, PPOpIn _content_,bool _deep_copy);
	PPPIConstructor(dynamic_context *_cxt_, 
            PPOpIn _qname_, const char* value,bool _deep_copy);
	PPPIConstructor(dynamic_context *_cxt_, 
           const char* name, const char* value,bool _deep_copy);
    virtual ~PPPIConstructor();
};


#endif
