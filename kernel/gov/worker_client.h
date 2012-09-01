#ifndef WORKER_CLIENT_H
#define WORKER_CLIENT_H

#include "common/socketutils/socketutils.h"
#include <exception>

struct ProcessInfo;
class Worker;

enum se_socket_client_type
{
  se_Client_Priority_NULL        = 0,
  se_Client_Priority_Listener    = 1,
  se_Client_Priority_Negot       = 2,
  se_Client_Priority_Cdb         = 3,
  se_Client_Priority_SM          = 4,
  se_Client_Priority_TRN         = 5,
  se_Client_Priority_Client      = 6,
  se_Client_Priority_Stop        = 7,
  se_Client_Priority_SMsd        = 8
};

class WorkerSocketClient;

class SednaClientSocketException : public std::exception
{
public:
    WorkerSocketClient * ref;
    SednaClientSocketException(WorkerSocketClient * _ref) : ref(_ref) { };
    const char *what() const throw() { return "Connection with one of Sedna processes broken\n"; }
};

class WorkerSocketClient : public SocketClient {
protected:
    Worker * worker;
    int priority;

public:
    WorkerSocketClient (Worker * _parent, USOCKET _socket, int _priority)
      : SocketClient(_socket), worker(_parent), priority(_priority) {};

    WorkerSocketClient (Worker * _parent, const SocketClient::ListenerConstructor& dummy, int _priority)
      : SocketClient(SocketClient::ListenerConstructor(dummy)), worker(_parent), priority(_priority) {};

    WorkerSocketClient (WorkerSocketClient * producer, int _priority)
      : SocketClient(producer->communicator), worker(producer->worker), priority(_priority) {};

    bool readmessage()
    {
        if (communicator->receive()) {
            return true;
        } else {
            if (communicator->getState() >= exch_connection_closed_ok) {
                throw SednaClientSocketException(this);
            }

            return false;
        };
    };

    inline int getPriority() const { return priority; };
    inline Worker * getParent() const { return worker; };
};

#define WORKER_READ_MESSAGE_SAFE if (!readmessage()) { return this; };

struct WorkerSocketClientLess {
    bool operator()( const WorkerSocketClient * lx, const WorkerSocketClient * rx ) const {
        if (lx->getPriority() == rx->getPriority()) {
            return (lx - rx < 0);
        } else {
            return (lx->getPriority() < rx->getPriority());
        }
    }
};

/* InternalSocketClient --- for connections with parts of Sedna
 */
class InternalSocketClient : public WorkerSocketClient {
protected:
    std::string ticket;
    ProcessInfo * process;
public:
    InternalSocketClient(WorkerSocketClient * producer, int _priority, const std::string& _ticket, ProcessInfo * _process)
      : WorkerSocketClient(producer, _priority), ticket(_ticket), process(_process) {};

    InternalSocketClient(InternalSocketClient * producer, int _priority)
      : WorkerSocketClient(producer, _priority), ticket(producer->ticket), process(producer->process) {};

    virtual void           cleanupOnError() = 0;
};

class ListenerSocket : public WorkerSocketClient {
  public:
    SocketClient *          acceptTransactionClient ();

    ListenerSocket (Worker * _parent, USOCKET sock) 
      : WorkerSocketClient(_parent, SocketClient::ListenerConstructor(sock), se_Client_Priority_Listener) {};

    virtual ~ListenerSocket() { setObsolete(true); }

    virtual SocketClient *  processData      ();
};

class SednaGovSocketException : public std::exception
{   
public:
    InternalSocketClient * ref;
    SednaGovSocketException(InternalSocketClient * _ref) : ref(_ref) { };
    const char *what() const throw() { return "Connection with one of Sedna processes broken\n"; }
};

#endif /* WORKER_CLIENT_H */
