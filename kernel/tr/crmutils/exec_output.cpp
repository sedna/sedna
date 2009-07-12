/*
* File:  exec_output.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "common/sedna.h"
#include "common/u/uutils.h"
#include "tr/crmutils/exec_output.h"
#include "tr/executor/base/PPBase.h"

void write_func(void *param, const char *str, int len)
{
    ((se_ostream*)param)->write(str,len);
}

se_ostream& endl(se_ostream& s)
{
    s.endline();
    return s;
}

///////////////////////////////////////////////////////////////////////////////
/// se_ostream
///////////////////////////////////////////////////////////////////////////////

se_ostream& se_ostream::operator<<(__int64 n)
{
    char z[20];
    short pos = 19;
    __int64 k = n;
    int i = 0;
    while (k > 9)
    {
        z[pos] = (char)(k % 10);
        k= k / 10;
        --pos;
    }
    z[pos] = (char)k;
    for (i = pos; i < 20; ++i)
        *this << (int)z[i];
    return *this;
}

se_ostream& se_ostream::writextext(char *s, int n)
{
    dynamic_context::stm.parse(s,n,write_func,this,(int)pat_element);
    return *this; 
}

se_ostream& se_ostream::writeattribute(char *s, int n)
{ 
    dynamic_context::stm.parse(s,n,write_func,this,(int)pat_attribute);
    return *this; 
}



///////////////////////////////////////////////////////////////////////////////
/// se_socketostream_base
///////////////////////////////////////////////////////////////////////////////

se_ostream& 
se_socketostream_base::operator<<(const char *s)	
{ 
    int len = strlen(s);

    if((_res_msg->length + len) > (SE_SOCKET_MSG_BUF_SIZE-5-_type_offset))
    {
        flush();	
        int celoe = len/(SE_SOCKET_MSG_BUF_SIZE-5-_type_offset);
        int ost;
        if(celoe==0) ost = len; else ost = len%(SE_SOCKET_MSG_BUF_SIZE-5-_type_offset);

        for (int i=0;i<celoe;i++)
        {
            _res_msg->length = SE_SOCKET_MSG_BUF_SIZE;
            // the body contains string format - 1 byte, string length - 4 bytes and a string
            // construct the buf for body.	   
            int2net_int(SE_SOCKET_MSG_BUF_SIZE-5-_type_offset, _res_msg->body+1+_type_offset);
            memcpy(_res_msg->body+5+_type_offset, s+(SE_SOCKET_MSG_BUF_SIZE-5-_type_offset)*i, SE_SOCKET_MSG_BUF_SIZE-5-_type_offset);

            flush();
        } //end for

        _res_msg->length = ost+5+_type_offset;
        int2net_int(ost, _res_msg->body+1+_type_offset);
        memcpy(_res_msg->body+5+_type_offset, s+(SE_SOCKET_MSG_BUF_SIZE-5-_type_offset)*celoe, ost);
    }
    else
    {
        memcpy(_res_msg->body+_res_msg->length, s, strlen(s));
        _res_msg->length += strlen(s);
        int2net_int(_res_msg->length-5-_type_offset, _res_msg->body+1+_type_offset);
    }
    return *this; 
}

se_ostream&
se_socketostream_base::write(const char *s, int n)		
{
    if((_res_msg->length + n) > (SE_SOCKET_MSG_BUF_SIZE-5-_type_offset))
    {
        flush();	
        int celoe = n/(SE_SOCKET_MSG_BUF_SIZE-5-_type_offset);
        int ost;
        if(celoe==0) ost = n; else ost = n%(SE_SOCKET_MSG_BUF_SIZE-5-_type_offset);
        for (int i=0;i<celoe;i++)
        {
            _res_msg->length = SE_SOCKET_MSG_BUF_SIZE;
            // the body contains string format - 1 byte, string length - 4 bytes and a string
            // construct the buf for body.
            int2net_int(SE_SOCKET_MSG_BUF_SIZE-5-_type_offset, _res_msg->body+1+_type_offset);
            memcpy(_res_msg->body+5+_type_offset, s+(SE_SOCKET_MSG_BUF_SIZE-5-_type_offset)*i, SE_SOCKET_MSG_BUF_SIZE-5-_type_offset);

            flush();
        } //end for

        _res_msg->length = ost+5+_type_offset;
        int2net_int(ost, _res_msg->body+1);
        memcpy(_res_msg->body+5+_type_offset, s+(SE_SOCKET_MSG_BUF_SIZE-5-_type_offset)*celoe, ost);

    }
    else
    {
        memcpy(_res_msg->body+_res_msg->length, s, n);
        _res_msg->length += n;
        int2net_int(_res_msg->length-5-_type_offset, _res_msg->body+1+_type_offset);
    }
    return *this; 
}

se_ostream&
se_socketostream_base::flush()				
{
    // if result portion sent is already of maximum size - do not send anymore
    if(max_result_size)
        if(result_portion_sent >= max_result_size-_res_msg->length+5+_type_offset)
        {
            _res_msg->length = max_result_size-result_portion_sent+5+_type_offset;
            if(_res_msg->length <= 0)
            {
                result_portion_sent = 0;
                throw USER_EXCEPTION(SE2041);
            }
        }

        if(_res_msg->length > 5+_type_offset)
        {
            _res_msg->instruction = _instruction; 
            int2net_int(_res_msg->length-5-_type_offset, _res_msg->body+1+_type_offset);

            if(sp_send_msg(_out_socket, _res_msg)!=0) throw USER_EXCEPTION(SE3006);
            result_portion_sent += _res_msg->length;

            _res_msg->length = 5+_type_offset;
        }
        return *this; 
}

void
se_socketostream_base::endline()
{
    _res_msg->body[_res_msg->length]='\n'; 
    _res_msg->length +=1;
}

void
se_socketostream_base::error(const char* str)
{
    flush();
    _res_msg->instruction = se_ErrorResponse; //ErrorResponse
    memcpy(_res_msg->body+5+_type_offset, str, strlen(str));
    int2net_int(strlen(str), _res_msg->body+1+_type_offset);      
    _res_msg->length = strlen(str)+5+_type_offset;
    if(sp_send_msg(_out_socket, _res_msg)!=0) throw USER_EXCEPTION(SE3006);
    result_portion_sent += _res_msg->length;

    _res_msg->length = 5;
    result_portion_sent = 0;
}


se_ostream& 
se_socketostream_base::operator<<(char c)
{	
    flush();
    _res_msg->body[_res_msg->length] = c;
    _res_msg->length += 1;
    return *this; 
}

se_ostream& 
se_socketostream_base::operator<<(bool n)
{ 
    flush();
    (n) ? _res_msg->body[_res_msg->length] = '1' 
        : _res_msg->body[_res_msg->length] = '0';
    _res_msg->length += 1;
    return *this;
}

se_ostream& 
se_socketostream_base::operator<<(short n)
{
    flush();
    u_ltoa((long)n, _res_msg->body+_res_msg->length, 10); 
    _res_msg->length += strlen(_res_msg->body + 5 + _type_offset);
    return *this; 
}

se_ostream& 
se_socketostream_base::operator<<(unsigned short n)
{
    flush();
    u_ultoa((unsigned long)n, _res_msg->body + _res_msg->length, 10);
    _res_msg->length += strlen(_res_msg->body + 5 +_type_offset);
    return *this; 
}

se_ostream& 
se_socketostream_base::operator<<(int n)
{
    flush();	
    u_ltoa((long)n, _res_msg->body +_res_msg->length, 10);
    _res_msg->length += strlen(_res_msg->body + 5 + _type_offset);
    return *this; 
}

se_ostream& 
se_socketostream_base::operator<<(unsigned int n)
{
    flush();
    u_ultoa((long)n, _res_msg->body + _res_msg->length, 10); 
    _res_msg->length += strlen(_res_msg->body + 5 +_type_offset);
    return *this; 
}

se_ostream& 
se_socketostream_base::operator<<(long n)
{
    flush();
    u_ltoa((long)n, _res_msg->body + _res_msg->length, 10); 
    _res_msg->length += strlen(_res_msg->body + 5 + _type_offset);
    return *this;
}

se_ostream& 
se_socketostream_base::operator<<(unsigned long n)
{
    flush();
    u_ultoa((long)n, _res_msg->body + _res_msg->length, 10);
    _res_msg->length += strlen(_res_msg->body+5+_type_offset);
    return *this;
}

se_ostream& 
se_socketostream_base::operator<<(float n)
{
    flush();
    u_gcvt((double)n, 10, _res_msg->body + _res_msg->length);
    _res_msg->length += strlen(_res_msg->body + 5 + _type_offset);
    return *this;
}

se_ostream& 
se_socketostream_base::operator<<(double n)
{
    flush();
    u_gcvt(n, 10, _res_msg->body + _res_msg->length);
    _res_msg->length += strlen(_res_msg->body + 5 + _type_offset);
    return *this;
}

se_ostream& 
se_socketostream_base::operator<<(long double n)
{
    flush();
    u_gcvt((long double)n, 10, _res_msg->body + _res_msg->length);
    _res_msg->length += strlen(_res_msg->body + 5 + _type_offset);
    return *this;
}

se_ostream& 
se_socketostream_base::operator<<(void * n)
{
    flush();
    sprintf(_res_msg->body+_res_msg->length, "%08X", *((int *)n)); 
    _res_msg->length += 4;
    return *this;
}

se_ostream& 
se_socketostream_base::operator<<(xptr n)
{
    flush();
    sprintf(_res_msg->body + _res_msg->length, 
        "%08X@%08X", 
        *((int *)n.layer), 
        *((int *)n.addr)); 
    _res_msg->length += 4;
    return *this;
}

se_ostream& 
se_socketostream_base::put(char c)
{
    flush();
    _res_msg->body[_res_msg->length] = c;
    _res_msg->length += 1;
    return *this;
}


///////////////////////////////////////////////////////////////////////////////
/// se_socketostream
///////////////////////////////////////////////////////////////////////////////

se_socketostream::se_socketostream(USOCKET out_socket, protocol_version p_ver)
{  
    _out_socket = out_socket;
    _p_ver = p_ver;
    _res_msg = &res_msg;
    _instruction = se_ItemPart;
    _type_offset = 0;
    _res_msg->body[0] = 0;         // in this version string format is always 0
    _res_msg->length = 5;          // the body contains string format - 1 byte, string length - 4 bytes and a string

    max_result_size = 0;
    result_portion_sent = 0;
}

void
se_socketostream::end_of_data(qepNextAnswer res)	
{
    flush(); 
    if (res == se_next_item_exists)
    {
        _res_msg->instruction = se_ItemEnd;      //ItemEnd
        _res_msg->length = 0;
    }
    else if ((res == se_no_next_item) || (res == se_result_is_cut_off))
    {
        _res_msg->instruction = se_ResultEnd;    //ResultEnd
        _res_msg->length = 0;
    }
    else
        throw SYSTEM_EXCEPTION("Got incorrect qepNextAnswer in end_of_data");

    /* This feature is not implemented yet */
    //else // res == se_result_is_cut_off
    //{
    //    _res_msg->instruction = se_ResultEnd;    //ResultEnd
    //    _res_msg->length = 4;
    //    int2net_int(1, _res_msg->body);          //the flag means result was cut
    //}

    if(sp_send_msg(_out_socket, _res_msg)!=0)
        throw USER_EXCEPTION(SE3006);
    _res_msg->length = 5+_type_offset;
    result_portion_sent = 0;
}

se_ostream* 
se_socketostream::get_debug_ostream() { 
    if (_p_ver.major_version < 2)
        return se_new se_nullostream();
    else
        return se_new se_debug_socketostream(*this);
}


///////////////////////////////////////////////////////////////////////////////
/// se_debug_socketostream
///////////////////////////////////////////////////////////////////////////////

se_debug_socketostream::se_debug_socketostream(se_socketostream& sostream) 
{  
    _out_socket  = sostream._out_socket;
    _p_ver       = sostream._p_ver;
    _res_msg     = &debug_msg;
    _instruction = se_DebugInfo;
    _type_offset = 4;

    /* If debug_info_type is not set, it is se_QueryTrace by default */
    se_debug_info_type type = se_QueryTrace;

    max_result_size     = 0;
    result_portion_sent = 0;

    int2net_int(type, _res_msg->body);

    /* In this version string format is always 0 */
    _res_msg->body[_type_offset] = 0;    

    /* Body contains type          - 4 bytes, 
    *               string format - 1 byte, 
    *               string length - 4 bytes 
    * and a string itself 
    */
    _res_msg->length = 5 + _type_offset;
}

void 
se_debug_socketostream::set_debug_info_type(se_debug_info_type type) {
    int2net_int(type, _res_msg->body);
}
