#ifndef SOCKETUTILS_H
#define SOCKETUTILS_H

#include "u/usocket.h"
#include "u/uprocess.h"
#include "aux/counted_ptr.h"
#include "common/protocol/sp.h"

#include <string>

enum message_exch_state_t {
      exch_ready_to_receive,
      exch_getting_message,          //by this moment instruction number and full length of the message is already received.
      exch_got_full_message,
      exch_connection_closed_ok,
      exch_connection_closed_error,
};

void    socketSetNoDelay (USOCKET sock);

/* class SocketReader inlined for better performance */
class SocketReader {
// TODO: implement buffer reallocation on exceeding capacity
  private:
    USOCKET                 socket;
    char                    buffer[SE_SOCKET_MSG_BUF_SIZE];
    char *                  pos;
//     size_t                  bufferCapacity;
    size_t                  readLeft;
    bool                    m_closed;
  public:
    inline void             startRead       (size_t n)    { readLeft = n; pos = buffer; };
    inline bool             started         ()            { return readLeft > 0; };
    inline bool             isClosed        ()            { return m_closed; };

    inline USOCKET          getSocketReader ()            { return socket; }

    bool                    read            () 
        {
            int reallyRead;

            if (readLeft > 0) {
                reallyRead = urecv(socket, pos, readLeft, __sys_call_error);
                
                if (reallyRead <= 0) {
                    m_closed = true;
                    readLeft = 0;
                    return true;
                }

                readLeft -= reallyRead;
                pos += reallyRead;
            }

            return readLeft == 0;
        };

    inline const char *     getBuf          () const      { return buffer; };
    inline size_t           getBufLen       () const      { return pos - buffer; };

    inline                  SocketReader    (USOCKET s) : socket(s), readLeft(0), m_closed(false) 
        {
            pos = buffer;
        };

//     inline                 ~SocketReader    ()            { free(buffer); };
    
    inline void             reset           (void)        { readLeft = 0; pos = 0; }
};

/* This class provides more convinient way for message exchange between processes.
 * You need to make several writeStrings/writeInts and then to call .send to send it.
 * The recepient must call readStrings/readInts in the same order that they were written.
 * 
 * !For size_t you MUST use writeInt64/readInt64, it's obligatory!
 * 
 * You don't need to worry about memory allocations (on send) or max message size. !TODO this
 * But you should keep in mind that you need to allocate memory for receipt variables.
 * And also you are to watch for types convertions on readInt32/64 because these functions return
 * unsigned types always.
 * 
 * Message structure (one "=" symbol equals 1 char, "x" means 0 or more of any chars, 
 * "()" means logical block, "*" means 0 or more repeats of previous blocks):
 * [ (====)(====)( (=)(x)* "/0"  | (====) | (========) )* ]
 *   (====) -- message instruction
 *         (====) -- current message length
 *                 (=)(x)* "/0" -- format code (default 0) and null-terminated string. More format codes maybe would be supported later
 *                                 (====) -- uint32
 *                                          (========) -- uint64
 * not implemented yet  (=) -- current message flags (final or not final message)
 */
class BaseMessageExchanger {
  protected:
    sp_int32                instruction;  //instruction number and length are duplicated from buffer just for convinience
    size_t                  length;       //instruction number and length are duplicated from buffer just for convinience
//    uint8_t                 flags;
    
    message_exch_state_t    state;
    
    char                    sender_buffer   [SE_SOCKET_MSG_BUF_SIZE];
    const char *            reader_buffer;
    
    USOCKET                 sock;
    SocketReader            reader;
    
    size_t                  offset;
    
    uint8_t                 format_code;
/* to write header or something special with manually set offset */
    void                    writeInt32      (uint32_t value, size_t manual_offset);
    void                    writeInt64      (uint64_t value, size_t manual_offset);
    void                    writeChar       (uint8_t value, size_t manual_offset);
    
  public:
    message_exch_state_t    getState        () const            { return state; };
    
    sp_int32                getInstruction  (void) const        { return instruction; };
    uint8_t                 getFormatCode   (void) const        { return format_code; };
    void                    setFormatCode   (void)              { format_code = 0; };

    void                    writeString     (const char * source);
    void                    writeString     (const std::string & source);
    void                    writeChar       (uint8_t value);
    void                    writeInt32      (uint32_t value);
    void                    writeInt64      (uint64_t value);

    void                    readString      (char * dest, size_t maxlen);
    void                    readString      (std::string& dest, size_t maxlen); //in this case read string attaches to dest string
    uint8_t                 readChar        (void);
    uint32_t                readInt32       (void);
    uint64_t                readInt64       (void);
    
    void                    beginSend       (sp_int32 instr); //every message may (and usually SHOULD) contain instruction.
    bool                    receive         (void);

    int                     endSend         (void);
    
    USOCKET                 getCommunicationSock (void) const   { return sock; };
    explicit                BaseMessageExchanger (USOCKET &s): length(0), /*flags(1),*/ state(exch_ready_to_receive), sock(s), reader(s), offset(0), format_code(0) {};
};

class MessageExchanger : public BaseMessageExchanger {
    /*
    * Socket transmission part. It's obligatory to send only sockets, 
    * buffers should be empty if you want to transmit socket. 
    * I mean that you shouldn't send anything except one socket info in this message.
    * Note that this function works absolutely in different way under win and *nix.
    */
  private:
    void                    writeSock       (void* source);
    void                    readSock        (void * dest);
  public:
    /*  Note! in this part we need to pass unix socket as parameter in *nix and the ususal network socket in windows.
     *  Also keep in mind that receiving side (trn in our case) needs to connect to unix socket.
     */
    int                     sendSocket      (UPID dest_pid, USOCKET dest_sock, USOCKET source_sock); 
    int                     receiveSocket   (USOCKET* dest_sock);
    
    explicit                MessageExchanger (USOCKET &s): BaseMessageExchanger(s) {};
    
};

//////////////////////////////////////////////////////////////////////////////
/// SocketClient
//////////////////////////////////////////////////////////////////////////////

class SocketClient {
  protected:
    USOCKET                clientSocket;
    counted_ptr<MessageExchanger> communicator;
    bool                   aIsObsolete;

  public:
    struct ListenerConstructor {
        USOCKET _clientSocket;
        explicit ListenerConstructor(USOCKET socket) : _clientSocket(socket) {};
        ListenerConstructor(const ListenerConstructor & x) : _clientSocket(x._clientSocket) {};
    };

    explicit SocketClient(const ListenerConstructor &c) 
      : clientSocket(c._clientSocket), communicator(NULL), aIsObsolete(false) {}
    
    SocketClient(USOCKET socket) : clientSocket(socket), aIsObsolete(false)
    { 
        communicator = new MessageExchanger(socket); 
    };

    SocketClient(counted_ptr<MessageExchanger> _comm) : aIsObsolete(false)
    {
        communicator = _comm;
        clientSocket = _comm->getCommunicationSock();
    };

    inline USOCKET         getSocket() const { return clientSocket; };
    inline bool            isObsolete() const { return aIsObsolete; };
    
    inline void            setObsolete(bool shutdown = true) 
    { 
        if (!aIsObsolete) { 
          if (shutdown) {
              ushutdown_socket(clientSocket, __sys_call_error); 
          };
          uclose_socket(clientSocket, __sys_call_error); 
          aIsObsolete = true; 
        } 
    };

    inline void            respondError(void) { 
                              communicator->beginSend(se_ErrorResponse);
                              communicator->endSend();
                              setObsolete();
                           };
    inline void            respondError(sp_int32 error_code) {
                              communicator->beginSend(error_code);
                              communicator->endSend();
                              setObsolete();
                           }
    
    virtual               ~SocketClient() {};
    virtual SocketClient * processData() = 0;
};

class MessageExchangerException : public std::exception {
private:
    const char * error;
public:
    MessageExchangerException(const char* _error)
      : error(_error) { };

    virtual const char* what() const throw() { return error; };
};

class SocketTransmissionException : public MessageExchangerException {
private:
    const char * error;
public:
    SocketTransmissionException(const char* _error)
      : MessageExchangerException(_error) {}

    virtual const char* what() const throw() { return error; };
};

#endif /* SOCKETUTILS_H */
