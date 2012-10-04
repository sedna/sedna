/*
* File:  exec_output.cpp
* Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
*/

#include "common/sedna.h"
#include "u/uutils.h"
#include "auxiliary/commutil.h"
#include "tr/crmutils/exec_output.h"
#include "tr/executor/base/PPBase.h"


///////////////////////////////////////////////////////////////////////////////
/// Some usefull helpers
///////////////////////////////////////////////////////////////////////////////

#define AVAILABLE_SPACE(msg)      (SE_SOCKET_MSG_BUF_SIZE - (msg)->length)

static inline se_item_type
xmlscm_type2se_item_type(xmlscm_type t) {
    return (se_item_type)t;
}

static inline se_item_class
node_type2se_item_class(t_item t, bool is_atomic) {
    if(is_atomic)
        return se_atomic;
    else
    {
        switch(t)
        {
            case element:                      return se_element;
            case text:                         return se_text;
            case attribute:                    return se_attribute;
            case xml_namespace:                return se_namespace;
            case document: case virtual_root:  return se_document;
            case comment:                      return se_comment;
            case pr_ins:                       return se_pi;
            default: throw USER_EXCEPTION2(SE1003, "unexpected item type in node_type2se_item_class");
        }
    }
}

static inline const char*
found_last_utf8_valid_byte(const char* str, int shift) {

    const unsigned char* cur = (const unsigned char*) str;

    while (shift > 0) {
        if((*(cur-1) & 0x80) == 0)             /* 0xxxxxxx */
            break;

        if((*(cur-1) & 0xE0) == 0xC0 ||        /* 110xxxxx */
           (*(cur-1) & 0xF0) == 0xE0 ||        /* 1110xxxx */
           (*(cur-1) & 0xF8) == 0xF0)          /* 11110xxx */
        {
            cur--; break;
        }

        cur--;
        shift--;
    }
    return (const char*)cur;
}

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

#if defined(_MSC_VER) || !defined(SEDNA_X64)
se_ostream& se_ostream::operator<<(int64_t n)
{
    char z[20];
    short pos = 19;
    int64_t k = n;
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
#endif

///////////////////////////////////////////////////////////////////////////////
/// se_socketostream_base
///////////////////////////////////////////////////////////////////////////////


/* string MUST be a valid UTF-8 sequence */
se_ostream&
se_socketostream_base::operator<<(const char *s)
{
    return write(s, strlen(s));
}

/* string MUST be a valid UTF-8 sequence */
se_ostream&
se_socketostream_base::write(const char *s, int len)
{
    int shift = 0;

    while (shift < len)
    {
        int written_bytes = MIN(len - shift, AVAILABLE_SPACE(_res_msg));

        U_ASSERT(written_bytes >= 0);

        if(0 != written_bytes)
        {
            memcpy(_res_msg->body + _res_msg->length,
                   s + shift,
                   written_bytes);

            _res_msg->length += written_bytes;
        }

        if ( 0 == written_bytes )
        {
            /* Check if message ends with a valid UTF-8 sequence */
            if (shift < len && shift != 0) {

               U_ASSERT(_res_msg->length > 5 + _type_offset);

               const char* end = _res_msg->body +_res_msg->length;
               const char* cur = found_last_utf8_valid_byte(end, shift);
               int delta = end - cur;

               U_ASSERT(shift >= delta &&
                        _res_msg->length >= 5 + _type_offset + delta);

               shift -= delta;
               _res_msg->length -= delta;
            }
            flush();
        }
        else
            shift += written_bytes;
    }

    return *this;
}

se_ostream&
se_socketostream_base::flush(bool force)
{
    if(max_result_size)
    {
         /* If result portion sent is already of maximum size -
          * do not send anymore. */
        if(result_portion_sent + _res_msg->length - 5 - _type_offset > max_result_size)
        {
            _res_msg->length = max_result_size - result_portion_sent + 5 + _type_offset;
            if(_res_msg->length <= 5 + _type_offset)
            {
                result_portion_sent = 0;
                throw USER_EXCEPTION(SE2041);
            }
        }
    }

    if((force && _res_msg->length == 5 + _type_offset) ||
        _res_msg->length > 5 + _type_offset)
    {
        _res_msg->instruction = _instruction;

        int2net_int(_res_msg->length - 5 - _type_offset,
                    _res_msg->body   + 1 + _type_offset);

        if(sp_send_msg(_out_socket, _res_msg)!=0) throw USER_EXCEPTION(SE3006);

        if(max_result_size)
            result_portion_sent += _res_msg->length - 5 - _type_offset;

        _res_msg->length = 5 + _type_offset;
    }

    return *this;
}

void
se_socketostream_base::error(const char* str)
{
    flush();

    _res_msg->instruction = se_ErrorResponse;
    memcpy(_res_msg->body + 5, str, strlen(str));
    int2net_int(strlen(str), _res_msg->body + 1);
    _res_msg->length = strlen(str) + 5;

    if(sp_send_msg(_out_socket, _res_msg) !=0 )
        throw USER_EXCEPTION(SE3006);
}

void
se_socketostream_base::endline()
{
    if (AVAILABLE_SPACE(_res_msg) < 1) flush();
    _res_msg->body[_res_msg->length] = '\n';
    _res_msg->length += 1;
}

se_ostream&
se_socketostream_base::operator<<(char c)
{
    if (AVAILABLE_SPACE(_res_msg) < 1) flush();
    _res_msg->body[_res_msg->length] = c;
    _res_msg->length += 1;
    return *this;
}

se_ostream&
se_socketostream_base::operator<<(bool n)
{
    if (AVAILABLE_SPACE(_res_msg) < 1) flush();
    (n) ? _res_msg->body[_res_msg->length] = '1'
        : _res_msg->body[_res_msg->length] = '0';
    _res_msg->length += 1;
    return *this;
}

se_ostream&
se_socketostream_base::operator<<(short n)
{
    if (AVAILABLE_SPACE(_res_msg) < 20) flush();
    u_ltoa((long)n, _res_msg->body+_res_msg->length, 10);
    _res_msg->length += strlen(_res_msg->body + 5 + _type_offset);
    return *this;
}

se_ostream&
se_socketostream_base::operator<<(unsigned short n)
{
    if (AVAILABLE_SPACE(_res_msg) < 20) flush();
    u_ultoa((unsigned long)n, _res_msg->body + _res_msg->length, 10);
    _res_msg->length += strlen(_res_msg->body + _res_msg->length);
    return *this;
}

se_ostream&
se_socketostream_base::operator<<(int n)
{
    if (AVAILABLE_SPACE(_res_msg) < 20) flush();
    u_ltoa((long)n, _res_msg->body +_res_msg->length, 10);
    _res_msg->length += strlen(_res_msg->body + _res_msg->length);
    return *this;
}

se_ostream&
se_socketostream_base::operator<<(unsigned int n)
{
    if (AVAILABLE_SPACE(_res_msg) < 20) flush();
    u_ultoa((long)n, _res_msg->body + _res_msg->length, 10);
    _res_msg->length += strlen(_res_msg->body + _res_msg->length);
    return *this;
}

se_ostream&
se_socketostream_base::operator<<(long n)
{
    if (AVAILABLE_SPACE(_res_msg) < 20) flush();
    u_ltoa((long)n, _res_msg->body + _res_msg->length, 10);
    _res_msg->length += strlen(_res_msg->body + _res_msg->length);
    return *this;
}

se_ostream&
se_socketostream_base::operator<<(unsigned long n)
{
    if (AVAILABLE_SPACE(_res_msg) < 20) flush();
    u_ultoa((long)n, _res_msg->body + _res_msg->length, 10);
    _res_msg->length += strlen(_res_msg->body + _res_msg->length);
    return *this;
}

se_ostream&
se_socketostream_base::operator<<(float n)
{
    if (AVAILABLE_SPACE(_res_msg) < 20) flush();
    u_gcvt((double)n, 10, _res_msg->body + _res_msg->length);
    _res_msg->length += strlen(_res_msg->body + _res_msg->length);
    return *this;
}

se_ostream&
se_socketostream_base::operator<<(double n)
{
    if (AVAILABLE_SPACE(_res_msg) < 20) flush();
    u_gcvt(n, 10, _res_msg->body + _res_msg->length);
    _res_msg->length += strlen(_res_msg->body + _res_msg->length);
    return *this;
}

se_ostream&
se_socketostream_base::operator<<(long double n)
{
    if (AVAILABLE_SPACE(_res_msg) < 20) flush();
    u_gcvt((long double)n, 10, _res_msg->body + _res_msg->length);
    _res_msg->length += strlen(_res_msg->body + _res_msg->length);
    return *this;
}

se_ostream&
se_socketostream_base::operator<<(void* n)
{
    if (AVAILABLE_SPACE(_res_msg) < 8) flush();
    sprintf(_res_msg->body + _res_msg->length, "%08"PRIXPTR"", (uintptr_t) n);
    _res_msg->length += 8;
    return *this;
}

se_ostream&
se_socketostream_base::operator<<(xptr n)
{
    if (AVAILABLE_SPACE(_res_msg) < 17) flush();
    sprintf(_res_msg->body + _res_msg->length,
            "%08"PRId32"@%08"PRIu32,
            n.layer, n.getOffs());
    _res_msg->length += 17;
    return *this;
}

se_ostream&
se_socketostream_base::put(char c)
{
    if (AVAILABLE_SPACE(_res_msg) < 1) flush();
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
    _res_msg->body[_type_offset] = 0;       // in this version string format is always 0
    _res_msg->length = 5 + _type_offset;    // the body contains string format - 1 byte,
                                            //                   string length - 4 bytes
                                            //                   and a string
    max_result_size = 0;
    result_portion_sent = 0;
}

void
se_socketostream::end_item(qepNextAnswer res)
{
    flush(true);

    if (res == se_next_item_exists)
    {
        _res_msg->instruction = se_ItemEnd;
        _res_msg->length = 0;
    }
    else if ((res == se_no_next_item) || (res == se_result_is_cut_off))
    {
        _res_msg->instruction = se_ResultEnd;
        _res_msg->length = 0;
    }
    else
        throw SYSTEM_EXCEPTION("Got incorrect qepNextAnswer in end_item");

    /* This feature is not implemented yet
       The flag means result was cut. */
    //else // res == se_result_is_cut_off
    //{
    //    _res_msg->instruction = se_ResultEnd;
    //    _res_msg->length = 4;
    //    int2net_int(1, _res_msg->body);
    //}

    if(sp_send_msg(_out_socket, _res_msg)!=0)
        throw USER_EXCEPTION(SE3006);
}

void
se_socketostream::begin_item (bool is_atomic, xmlscm_type st, t_item nt, const char* url)
{
    flush(true);
    _res_msg->length = 0;

    if (_p_ver.major_version >= 4) {
        /* Since protocol version 4 we send type of the item in the
         * first message. Then in se_socketostream::flush() we change
         * _instruction back to the se_ItemPart.
         */
        _instruction      = se_ItemStart;
        _type_offset      = 3;
        _res_msg->body[0] = (unsigned char) node_type2se_item_class(nt, is_atomic);
        _res_msg->body[1] = (unsigned char) xmlscm_type2se_item_type(st);

        if(url != NULL)
        {
            /* set url flag */
            _res_msg->body[2] = (unsigned char) 1;
            /* set string type, always 0 at this version */
            _res_msg->body[3] = (unsigned char) 0;

            int url_len = strlen(url);

            if ( url_len > SE_SOCKET_MSG_BUF_SIZE - 30)
                throw USER_EXCEPTION2(SE3012, "too long node URI");

            /* set string length */
            int2net_int(url_len, _res_msg->body + 4);

            /* set URL itself */
            strcpy(_res_msg->body + _type_offset + 5, url);
            _type_offset += 5 + url_len;
        }
        else
        {
            /* unset url flag */
            _res_msg->body[2] = (unsigned char) 0;
        }

    }
    else
    {
        _instruction = se_ItemPart;
        _type_offset = 0;
    }

    _res_msg->body[_type_offset] = 0;
    _res_msg->length  = 5 + _type_offset;
}

se_ostream&
se_socketostream::flush(bool force)
{
    if((force && _res_msg->length == 5 + _type_offset) ||
       _res_msg->length > 5 + _type_offset)
    {
         se_socketostream_base::flush();
        _instruction = se_ItemPart;
        _type_offset = 0;
        _res_msg->body[_type_offset] = 0;
        _res_msg->length = 5 + _type_offset;
    }
    return *this;
}

se_ostream*
se_socketostream::get_debug_ostream() {
    if (_p_ver.major_version < 2)
        return new se_nullostream();
    else
        return new se_debug_socketostream(*this);
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
    *                string format - 1 byte,
    *                string length - 4 bytes
    *  and a string itself
    */
    _res_msg->length = 5 + _type_offset;
}

void
se_debug_socketostream::set_debug_info_type(se_debug_info_type type) {
    int2net_int(type, _res_msg->body);
}
