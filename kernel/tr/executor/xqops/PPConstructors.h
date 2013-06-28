/*
 * File:  PPConstructors.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _PPCONSTRUCTORS_H
#define _PPCONSTRUCTORS_H

#include "common/sedna.h"

#include "tr/executor/base/PPBase.h"
#include "tr/structures/schema.h"

///////////////////////////////////////////////////////////////////////////////
/// PPConstructor
///////////////////////////////////////////////////////////////////////////////
class PPConstructor : public PPIterator
{
protected:
    bool first_time;
    bool deep_copy;

    PPConstructor(dynamic_context *_cxt_,
                  operation_info _info_,
                  bool _deep_copy) : PPIterator(_cxt_, _info_, "PPConstructor"),
                                     deep_copy(_deep_copy) {};

public:
    static void checkInitial();
    static void clear_virtual_root();
    static Node getVirtualRoot();

    inline bool is_deep_copy() { return deep_copy; }
};

///////////////////////////////////////////////////////////////////////////////
/// PPElementConstructor
///////////////////////////////////////////////////////////////////////////////
class PPElementConstructor : public PPConstructor
{
protected:
    PPOpIn qname;
    PPOpIn content;
    char* el_name;

    NamespaceSnapshot * sknSnapshot;
    int sknMarker;

    bool virtualElement;
private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t) ;
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPElementConstructor(dynamic_context *_cxt_,
                         operation_info _info_,
                         PPOpIn _qname_,
                         PPOpIn _content_,
                         bool _deep_copy,
                         int sknMarker,
                         bool virtualElement);

    PPElementConstructor(dynamic_context *_cxt_,
                         operation_info _info_,
                         const char* name,
                         PPOpIn _content_,
                         bool _deep_copy,
                         int sknMarker,
                         bool virtualElement);

    virtual ~PPElementConstructor();

    /* May return NULL if name is not predefined */
    inline const char* get_name() { return el_name; }
    inline bool is_virtual() { return virtualElement; }
};

///////////////////////////////////////////////////////////////////////////////
/// PPAttributeConstructor
///////////////////////////////////////////////////////////////////////////////
class PPAttributeConstructor : public PPConstructor
{
protected:
    PPOpIn qname;
    PPOpIn content;
    char* at_name;
    char* at_value;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t) ;
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPAttributeConstructor(dynamic_context *_cxt_,
                           operation_info _info_,
                           PPOpIn _qname_,
                           PPOpIn _content_,
                           bool _deep_copy);

	PPAttributeConstructor(dynamic_context *_cxt_,
                           operation_info _info_,
                           const char* name,
                           PPOpIn _content_,
                           bool _deep_copy);

	PPAttributeConstructor(dynamic_context *_cxt_,
                           operation_info _info_,
                           PPOpIn _qname_,
                           const char* value,
                           bool _deep_copy);

	PPAttributeConstructor(dynamic_context *_cxt_,
                           operation_info _info_,
                           const char* name,
                           const char* value,
                           bool _deep_copy);

    virtual ~PPAttributeConstructor();
    /* May return NULL if name is not predefined */
    inline const char* get_name() { return at_name; }
    /* May return NULL if value is not predefined */
    inline const char* get_value() { return at_value; }
};

///////////////////////////////////////////////////////////////////////////////
/// PPNamespaceConstructor
///////////////////////////////////////////////////////////////////////////////
class PPNamespaceConstructor : public PPConstructor
{
protected:
    PPOpIn content;
    char* at_name;
    char* at_value;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t) ;
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPNamespaceConstructor(dynamic_context *_cxt_,
                           operation_info _info_,
                           const char* name,
                           PPOpIn _content_);

	PPNamespaceConstructor(dynamic_context *_cxt_,
                           operation_info _info_,
                           const char* name,
                           const char* value);

    virtual ~PPNamespaceConstructor();
    inline const char* get_name() { return at_name; }
    /* May return NULL if value is not predefined */
    inline const char* get_value() { return at_value; }
};

///////////////////////////////////////////////////////////////////////////////
/// PPCommentConstructor
///////////////////////////////////////////////////////////////////////////////
class PPCommentConstructor : public PPConstructor
{
protected:
    PPOpIn content;
	char* at_value;
	StrMatcher strm;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t) ;
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPCommentConstructor(dynamic_context *_cxt_,
                         operation_info _info_,
                         PPOpIn _content_,
                         bool _deep_copy);

	PPCommentConstructor(dynamic_context *_cxt_,
                         operation_info _info_,
                         const char* value,
                         bool _deep_copy);

    virtual ~PPCommentConstructor();
    /* May return NULL if value is not predefined */
    inline const char* get_value() { return at_value; }
};

///////////////////////////////////////////////////////////////////////////////
/// PPTextConstructor
///////////////////////////////////////////////////////////////////////////////
class PPTextConstructor : public PPConstructor
{
protected:
    PPOpIn content;
    char* at_value;
    bool cdataflag;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t) ;
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPTextConstructor(dynamic_context *_cxt_,
                      operation_info _info_,
                      PPOpIn _content_,
                      bool _deep_copy,
                      bool cdataflag);

	PPTextConstructor(dynamic_context *_cxt_,
                      operation_info _info_,
                      const char* value,
                      bool _deep_copy,
                      bool cdataflag);

    virtual ~PPTextConstructor();
    /* May return NULL if value is not predefined */
    inline const char* get_value() { return at_value; }
};

///////////////////////////////////////////////////////////////////////////////
/// PPDocumentConstructor
///////////////////////////////////////////////////////////////////////////////
class PPDocumentConstructor : public PPConstructor
{
protected:
    PPOpIn content;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t) ;
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPDocumentConstructor(dynamic_context *_cxt_,
                          operation_info _info_,
                          PPOpIn _content_);

    virtual ~PPDocumentConstructor();
};


///////////////////////////////////////////////////////////////////////////////
/// PPPIConstructor
///////////////////////////////////////////////////////////////////////////////
class PPPIConstructor : public PPConstructor
{
protected:
    PPOpIn qname;
	PPOpIn content;
	char* at_name;
	char* at_value;
	StrMatcher strm;

private:
    virtual void do_open   ();
    virtual void do_reopen ();
    virtual void do_close  ();
    virtual void do_next   (xqp_tuple &t) ;
    virtual void do_accept (PPVisitor &v);

    virtual PPIterator* do_copy(dynamic_context *_cxt_);

public:
    PPPIConstructor(dynamic_context *_cxt_,
                    operation_info _info_,
                    PPOpIn _qname_,
                    PPOpIn _content_,
                    bool _deep_copy);

	PPPIConstructor(dynamic_context *_cxt_,
                    operation_info _info_,
                    const char* name,
                    PPOpIn _content_,
                    bool _deep_copy);

	PPPIConstructor(dynamic_context *_cxt_,
                    operation_info _info_,
                    PPOpIn _qname_,
                    const char* value,
                    bool _deep_copy);

	PPPIConstructor(dynamic_context *_cxt_,
                    operation_info _info_,
                    const char* name,
                    const char* value,
                    bool _deep_copy);

    virtual ~PPPIConstructor();
    /* May return NULL if name is not predefined */
    inline const char* get_name() { return at_name; }
    /* May return NULL if value is not predefined */
    inline const char* get_value() { return at_value; }
};


#endif /* _PPCONSTRUCTORS_H */
