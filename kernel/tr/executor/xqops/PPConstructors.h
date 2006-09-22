/*
 * File:  PPConstructors.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPCONSTRUCTORS_H
#define _PPCONSTRUCTORS_H

#include "sedna.h"

#include "PPBase.h"
#include "schema.h"
#include "strings.h"
#include "indirection.h"
#include "micro.h"
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
	static schema_node* root_schema;
	static xptr virt_root;
	static xptr last_elem;
	static xptr cont_parind;
	static xptr cont_leftind;
	static int conscnt;
	bool schema_carrier;
	bool deep_copy;
public:
	static bool checkInitial();
	virtual void open   ();
	static bool result(PPIterator* cur, variable_context *cxt, void*& r);
    PPConstructor(variable_context *_cxt_,bool _deep_copy) : PPIterator(_cxt_),deep_copy(_deep_copy) 
	{
	 schema_carrier=false;
	}
friend xptr copy_to_temp(xptr node);
friend void clear_temp();
};

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

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPElementConstructor(variable_context *_cxt_, 
            PPOpIn _qname_, PPOpIn _content_,bool _deep_copy, bool _ns_inside);
	PPElementConstructor(variable_context *_cxt_, 
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

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPAttributeConstructor(variable_context *_cxt_, 
            PPOpIn _qname_, PPOpIn _content_,bool _deep_copy);
	PPAttributeConstructor(variable_context *_cxt_, 
           const char* name, PPOpIn _content_,bool _deep_copy);
	PPAttributeConstructor(variable_context *_cxt_, 
            PPOpIn _qname_, const char* value,bool _deep_copy);
	PPAttributeConstructor(variable_context *_cxt_, 
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

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPNamespaceConstructor(variable_context *_cxt_, 
           const char* name, PPOpIn _content_);
	PPNamespaceConstructor(variable_context *_cxt_, 
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
	
    

    void children(PPOpIn &_content_) {if (at_value==NULL)_content_=content ;}

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPCommentConstructor(variable_context *_cxt_, 
            PPOpIn _content_,bool _deep_copy);
	PPCommentConstructor(variable_context *_cxt_, 
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

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPTextConstructor(variable_context *_cxt_, 
            PPOpIn _content_,bool _deep_copy);
	PPTextConstructor(variable_context *_cxt_, 
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

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPDocumentConstructor(variable_context *_cxt_, 
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
    

    void children(PPOpIn &_qname_,PPOpIn &_content_) { _qname_ = qname;_content_=content ;}

public:
    virtual void open   ();
    virtual void reopen ();
    virtual void close  ();
    virtual strict_fun res_fun () { return result; };
    virtual void next   (tuple &t);

    virtual PPIterator* copy(variable_context *_cxt_);
    static bool result(PPIterator* cur, variable_context *cxt, void*& r);

    PPPIConstructor(variable_context *_cxt_, 
            PPOpIn _qname_, PPOpIn _content_,bool _deep_copy);
	PPPIConstructor(variable_context *_cxt_, 
           const char* name, PPOpIn _content_,bool _deep_copy);
	PPPIConstructor(variable_context *_cxt_, 
            PPOpIn _qname_, const char* value,bool _deep_copy);
	PPPIConstructor(variable_context *_cxt_, 
           const char* name, const char* value,bool _deep_copy);
    virtual ~PPPIConstructor();
};


#endif
