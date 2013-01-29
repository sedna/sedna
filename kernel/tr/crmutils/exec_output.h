/*
 * File:  exec_output.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#ifndef _EXEC_OUTPUT_H
#define _EXEC_OUTPUT_H

#include <iostream>

#include "common/sedna.h"

#include "common/u/usocket.h"
#include "common/sp.h"
#include "tr/tr_base.h"
#include "tr/crmutils/crmbase.h"

void write_func(void *param, const char *str, int len);

se_ostream& endl(se_ostream& s);

class se_stdlib_ostream: public se_ostream
{
protected:
    std::ostream &o_str;

public:
    se_stdlib_ostream(std::ostream &_o_str_) : o_str(_o_str_) {}
    virtual ~se_stdlib_ostream() {}

    virtual se_ostream& operator<<(se_stdlib_ostream& (*pf)(se_stdlib_ostream&)) { (*pf)(*this); return *this; }
    virtual se_ostream& operator<<(se_ostream& (*pf)(se_ostream&))	             { (*pf)(*this); return *this; }
    virtual se_ostream& operator<<(const char *s)                                { o_str << s; return *this; }
    virtual se_ostream& operator<<(char c)                                       { o_str << c; return *this; }
    virtual se_ostream& operator<<(bool n)                                       { o_str << n; return *this; }
    virtual se_ostream& operator<<(short n)                                      { o_str << n; return *this; }
    virtual se_ostream& operator<<(unsigned short n)                             { o_str << n; return *this; }
    virtual se_ostream& operator<<(int n)                                        { o_str << n; return *this; }
    virtual se_ostream& operator<<(unsigned int n)                               { o_str << n; return *this; }
    virtual se_ostream& operator<<(long n)                                       { o_str << n; return *this; }
    virtual se_ostream& operator<<(unsigned long n)                              { o_str << n; return *this; }
    virtual se_ostream& operator<<(float n)                                      { o_str << n; return *this; }
    virtual se_ostream& operator<<(double n)                                     { o_str << n; return *this; }
    virtual se_ostream& operator<<(long double n)                                { o_str << n; return *this; }
    virtual se_ostream& operator<<(void * n)                                     { o_str << n; return *this; }
    virtual se_ostream& operator<<(xptr n)                                       { o_str << n.layer << "@" << n.getOffs(); return *this; }

    virtual se_ostream& put(char c)                                              { o_str.put(c); return *this; }
    virtual se_ostream& write(const char *s, int n)                              { o_str.write(s, n); return *this; }
    virtual se_ostream& flush(bool force = false)                                { o_str.flush(); return *this; }
    virtual void endline()                                                       { o_str << std::endl; }
    virtual void error(const char* str)                                          { o_str << str << std::endl; }
    virtual se_ostream* get_debug_ostream()                                      { return se_new se_stdlib_ostream(std::cerr); }
    virtual void set_debug_info_type(se_debug_info_type /*type*/)                {};
};

class se_nullostream: public se_ostream
{
public:
    se_nullostream() {}
    virtual ~se_nullostream() {}

    virtual se_ostream& operator<<(se_nullostream& (* /*pf*/)(se_nullostream&)) { return *this; }
    virtual se_ostream& operator<<(se_ostream& (* /*pf*/)(se_ostream&))         { return *this; }
    virtual se_ostream& operator<<(const char * /*s*/)                          { return *this; }
    virtual se_ostream& operator<<(char /*c*/)                                  { return *this; }
    virtual se_ostream& operator<<(bool /*n*/)                                  { return *this; }
    virtual se_ostream& operator<<(short /*n*/)                                 { return *this; }
    virtual se_ostream& operator<<(unsigned short /*n*/)                        { return *this; }
    virtual se_ostream& operator<<(int /*n*/)                                   { return *this; }
    virtual se_ostream& operator<<(unsigned int /*n*/)                          { return *this; }
    virtual se_ostream& operator<<(long /*n*/)                                  { return *this; }
    virtual se_ostream& operator<<(unsigned long /*n*/)                         { return *this; }
    virtual se_ostream& operator<<(float /*n*/)                                 { return *this; }
    virtual se_ostream& operator<<(double /*n*/)                                { return *this; }
    virtual se_ostream& operator<<(long double /*n*/)                           { return *this; }
    virtual se_ostream& operator<<(void * /*n*/)                                { return *this; }
    virtual se_ostream& operator<<(xptr)                                        { return *this; }
                                                                                
    virtual se_ostream& put(char /*c*/)                                         { return *this; }
    virtual se_ostream& write(const char * /*s*/, int /*n*/)                    { return *this; }
    virtual se_ostream& write_debug(int /*debug_type*/,
                                    const char * /*s*/, 
                                    int /*n*/)                                  { return *this; }
    virtual se_ostream& flush(bool force = false)                               { return *this; }
    virtual void endline()                                                      { ; }
    virtual void error(const char* /*str*/)                                     { ; }
    virtual se_ostream* get_debug_ostream()                                     { return se_new se_nullostream(); }
    virtual void set_debug_info_type(se_debug_info_type /*type*/)               {};      
};


class se_socketostream_base: public se_ostream
{
protected:
    USOCKET _out_socket;
    protocol_version _p_ver;
    msg_struct* _res_msg;
    int _instruction;
    int _type_offset;
    int max_result_size;
    int result_portion_sent;
    
public:

    virtual se_ostream& operator<<(se_socketostream_base& (*pf)(se_socketostream_base&))	{ (*pf)(*this); return *this; }
    virtual se_ostream& operator<<(se_ostream& (*pf)(se_ostream&))                          { (*pf)(*this); return *this; }

    /* string MUST be valid UTF-8 sequence */
    virtual se_ostream& operator<<(const char *s);
    virtual se_ostream& operator<<(char c);

    virtual se_ostream& operator<<(bool n);
    virtual se_ostream& operator<<(short n);
    virtual se_ostream& operator<<(unsigned short n);
    virtual se_ostream& operator<<(int n);
    virtual se_ostream& operator<<(unsigned int n);
    virtual se_ostream& operator<<(long n);    
    virtual se_ostream& operator<<(unsigned long n);
    virtual se_ostream& operator<<(float n);
    virtual se_ostream& operator<<(double n);
    virtual se_ostream& operator<<(long double n);
    virtual se_ostream& operator<<(void * n);
    virtual se_ostream& operator<<(xptr n);
    virtual se_ostream& put(char c);
    
    /* string MUST be valid UTF-8 sequence */
    virtual se_ostream& write(const char *s, int n);

    virtual se_ostream& flush(bool force = false);
    virtual void endline();
    virtual void error(const char* str);

    virtual se_ostream* get_debug_ostream() = 0;
    virtual void set_debug_info_type(se_debug_info_type type) = 0;
};

class se_socketostream: public se_socketostream_base
{
    friend class se_debug_socketostream;

protected:
    msg_struct res_msg;
    
public:
    se_socketostream(USOCKET out_socket, protocol_version p_ver);

    virtual ~se_socketostream() {}

    void set_max_result_size_to_pass(int _max_result_size_) {
        max_result_size = _max_result_size_;
    }
    virtual void begin_item (bool is_atomic, xmlscm_type st, t_item nt, const char* url);
    virtual void end_item   (qepNextAnswer res);

    virtual se_ostream* get_debug_ostream();
    virtual void set_debug_info_type(se_debug_info_type /*type*/) {};
    virtual se_ostream& flush(bool force = false);
};

class se_debug_socketostream: public se_socketostream_base
{
    friend class se_socketostream;

protected:
    msg_struct debug_msg;

    se_debug_socketostream(se_socketostream& sostream);
    virtual ~se_debug_socketostream() {}
    virtual se_ostream* get_debug_ostream() { return NULL; }
    virtual void set_debug_info_type(se_debug_info_type type);
};


#endif /* _EXEC_OUTPUT_H */
