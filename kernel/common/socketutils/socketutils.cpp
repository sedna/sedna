#include "socketutils.h"

#ifndef _WIN32
#include <ancillary.h>

#include <netdb.h>
#include <sys/types.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#else
#include <Winsock2.h>
#include <ws2tcpip.h>
#include <WSPiApi.h>
#endif

#include "common/sedna.h"
#include "common/protocol/sp.h"

typedef struct {
    struct cmsghdr h;
    int fd[1];
} ancil_lib_buffer;

struct uint64_split {
    uint32_t lo, hi;
};


int sendShortMessage(USOCKET socket, sp_int32 instruction)
{
    static msg_struct sp_msg;

    sp_msg.instruction = instruction;
    sp_msg.length = 0;

    return sp_send_msg(socket, &sp_msg);
};

int sendLongMessage(USOCKET socket, sp_int32 instruction, sp_int32 msg_len, msg_struct * msg)
{
    msg->instruction = instruction;
    msg->length = msg_len;

    return sp_send_msg(socket, msg);
};

void socketSetNoDelay(USOCKET sock)
{

    static const int true_value = 1;

    if (usetsockopt(sock, IPPROTO_TCP, TCP_NODELAY, (char*) &true_value, sizeof(true_value), __sys_call_error)==U_SOCKET_ERROR) {
//  TODO: Error failed to set socket option
    }
};


void BaseMessageExchanger::writeString(const char* source)
{
    size_t     len = strlen(source);

    writeChar(0);
    writeInt32((uint32_t) len);
    memcpy(sender_buffer + offset, source, len);
    offset += len;

    return;
}


void BaseMessageExchanger::writeString(const std::string & source)
{
    size_t len = source.length();

    writeChar(0);
    writeInt32((uint32_t) len);

    memcpy(sender_buffer + offset, source.c_str(), len);
    offset += len;

    return;
}


void BaseMessageExchanger::writeInt32(uint32_t value)
{
    value = htonl(value);
    memcpy (sender_buffer + offset, &value, sizeof(uint32_t));
    offset += sizeof(uint32_t);

    return;
}


void BaseMessageExchanger::writeInt64(uint64_t value)
{
    uint64_split  real_value;
    memcpy(&real_value, &value, sizeof(uint64_t));

    real_value.hi = htonl(real_value.hi);
    real_value.lo = htonl(real_value.lo);

    memcpy(sender_buffer + offset, &real_value, sizeof(uint64_split));
    offset += sizeof(uint64_split);

    length = (uint64_t) (offset - sizeof(uint32_t) - sizeof(uint32_t));
    return;
}


void BaseMessageExchanger::writeInt32(uint32_t value, size_t manual_offset)
{
    value = htonl(value);
    memcpy (sender_buffer + manual_offset, &value, sizeof(uint32_t));

    return;
}


void BaseMessageExchanger::writeInt64(uint64_t value, size_t manual_offset)
{
    uint64_split  real_value;
    memcpy(&real_value, &value, sizeof(uint64_t));

    real_value.hi = htonl(real_value.hi);
    real_value.lo = htonl(real_value.lo);

    memcpy(sender_buffer + manual_offset, &real_value, sizeof(uint64_split));

    return;
}

void BaseMessageExchanger::writeChar(uint8_t value, size_t manual_offset)
{
    memcpy (sender_buffer + manual_offset, &value, sizeof(uint8_t));
    return;
}

void BaseMessageExchanger::writeChar(uint8_t value)
{
    memcpy (sender_buffer + offset, &value, sizeof(uint8_t));
    offset += sizeof(uint8_t);

    return;
}


void BaseMessageExchanger::readString(char* dest, size_t maxlen)
{
    if (0 == readChar()) {
//      size_t len = strlen(reader_buffer + offset) + sizeof(char);
        size_t len = readInt32();
        if (len >= maxlen) {
            len = maxlen;
        }
        memcpy(dest, reader.getBuf() + offset, len);
        offset += len;
        dest[len] = 0;
    } else {
        /*TODO error*/
    }

    return;
}


void BaseMessageExchanger::readString(std::string & dest, size_t maxlen)
{
    if (0 == readChar()) {
// FIXME : combine this with previous method
//      size_t len = strlen(reader_buffer + offset);
        size_t len = readInt32();
        if (len >= maxlen) {
            len = maxlen;
        }
        char * localbuf = (char *) malloc(len+1);
        memcpy(localbuf, reader.getBuf() + offset, len);
        localbuf[len] = 0;
        offset += len;
        dest.append(localbuf);

        free(localbuf);
    } else {
        /*TODO error*/
    }

    return;
}

uint8_t BaseMessageExchanger::readChar(void )
{
    uint8_t buf;
    memcpy(&buf, reader_buffer + offset, sizeof(uint8_t));
    offset += sizeof(uint8_t);

    return buf;
}


uint32_t BaseMessageExchanger::readInt32(void )
{
    uint32_t buf;
    memcpy(&buf, reader_buffer + offset, sizeof(uint32_t));
    offset += sizeof(uint32_t);

    buf = ntohl(buf);

    return buf;
}


uint64_t BaseMessageExchanger::readInt64(void )
{
    uint64_split split_value;
    uint64_t     real_value;
    memcpy(&split_value, reader_buffer + offset, sizeof(uint64_split));
    offset += sizeof(uint64_split);

    split_value.hi = ntohl(split_value.hi);
    split_value.lo = ntohl(split_value.lo);
    memcpy(&real_value, &split_value, sizeof(uint64_t));

    return real_value;
}


void BaseMessageExchanger::beginSend(sp_int32 instr)
{
    offset = sizeof(uint32_t) + sizeof(uint32_t)/* + sizeof(uint8_t)*/;
    length = 0;
    instruction = instr;
    return;
}


bool BaseMessageExchanger::receive(void)
{
    if (reader.isClosed()) {
        return true;
    }

    if (state == exch_got_full_message) {
        offset = 0;
        length = 0;
        instruction = 0;
//      flags = 0;
        reader.reset();
        state = exch_ready_to_receive;
    }

    // Read message instruction and it's length at first.
    if (state == exch_ready_to_receive) {
        if (!reader.started()) {
            reader.startRead(sizeof(uint32_t) + sizeof(uint32_t) /* + sizeof(uint8_t) */ );
        }

        if (!reader.read()) {
            return false;
        } else {
            if (reader.isClosed()) {
                state = exch_connection_closed_ok;
                return true;
            }

            // Message header received
            offset = 0;
            reader_buffer = reader.getBuf();
            instruction = readInt32();
            length = (size_t) readInt32();
//            flags = readChar();
            state = exch_getting_message;
        }
    }

    // Try to read message body
    if (!reader.started()) {
        if (length > SE_SOCKET_MSG_BUF_SIZE) {
            /* Message length exceeds available size */
            throw USER_EXCEPTION(SE3006);
        }
        offset = 0;
        reader.startRead(length);
    }

    if (!reader.read()) {
        return false;
    } else {
        if (reader.isClosed()) {
            state = exch_connection_closed_error;
        } else {
            // Message received
            state = exch_got_full_message;
        }

        return true;
    }
}

int BaseMessageExchanger::endSend(void )
{
    length = (size_t) (offset - sizeof(uint32_t) - sizeof(uint32_t));
    writeInt32(instruction, 0);
    writeInt32((uint32_t) length, sizeof(sp_int32));
//    writeChar(flags, sizeof(sp_int32) + sizeof(uint32_t));

    char* ptr = sender_buffer;
    uint32_t sent = 0;
    int rc = 0;

    while (sent < sizeof(sp_int32) + sizeof(uint32_t) /*+ sizeof(uint8_t)*/ ) {
        rc = usend(sock, ptr + sent, sizeof(sp_int32) + sizeof(uint32_t) - sent, __sys_call_error);
        if (rc == U_SOCKET_ERROR) {
            return U_SOCKET_ERROR;
        }
        sent += rc;
    }

    ptr += sizeof(sp_int32) + sizeof(uint32_t) /* + sizeof(uint8_t) */;
    sent = rc = 0;

    while (sent < length) {
        rc = usend(sock, (const char *) (ptr + sent), length - sent, __sys_call_error);
        if (rc == U_SOCKET_ERROR) {
            return U_SOCKET_ERROR;
        }
        sent += rc;
    }

    return 0;
}


//this function is used on windows only
void MessageExchanger::writeSock ( void* source )
{
#ifdef _WIN32
    LPWSAPROTOCOL_INFO client_socket_info;
    memcpy(sender_buffer + offset, source, sizeof( *client_socket_info));
    offset += sizeof( *client_socket_info);

    return;
#endif
}

//this function is used on windows only
void MessageExchanger::readSock ( void* dest )
{
#ifdef _WIN32
    LPWSAPROTOCOL_INFO client_socket_info;
    memcpy(dest, reader_buffer + offset, sizeof( *client_socket_info));
    offset +=  sizeof( *client_socket_info);

    return;
#endif
}

#ifndef _WIN32
sighandler_t sigpipe_handler;
void sighandler(int sig_num)
{
    sigpipe_handler = signal(sig_num, SIG_IGN);
}
#endif

int MessageExchanger::sendSocket ( UPID dest_pid, USOCKET dest_sock, USOCKET source_sock )
{
#ifdef _WIN32
    offset = sizeof(uint32_t) + sizeof(uint32_t)/* + sizeof(uint8_t)*/;
    length = 0;
    instruction = se_ReceiveSocket;

    LPWSAPROTOCOL_INFO client_socket_info = (LPWSAPROTOCOL_INFO) malloc( sizeof( *client_socket_info));
    if (WSADuplicateSocket(source_sock, dest_pid, client_socket_info) == SOCKET_ERROR) {
        throw USER_EXCEPTION2(SE3035, usocket_error_translator());
    }
    writeSock ( (void *) client_socket_info );
    free(client_socket_info);

    return endSend();
#else
    if(ancil_send_fd(dest_sock, source_sock) == -1) {
        throw SocketTransmissionException(usocket_error_translator());
    }
    return 0;
#endif
}

int MessageExchanger::receiveSocket ( USOCKET * dest_sock )
{
#ifdef _WIN32
    while (!receive()) {
        ;
    }
    LPWSAPROTOCOL_INFO client_socket_info = (LPWSAPROTOCOL_INFO) malloc( sizeof( *client_socket_info));
    readSock( (void *) client_socket_info);
    *dest_sock = WSASocket(AF_INET, SOCK_STREAM, IPPROTO_TCP, client_socket_info, 0, 0);
    free(client_socket_info);

    return 0;
#else
    while (*dest_sock == U_INVALID_SOCKET) {
        ancil_recv_fd(sock, dest_sock);
    }
    return 0;
#endif
}




