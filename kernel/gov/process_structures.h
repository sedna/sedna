#ifndef _PROCESS_STRUCTURES_H
#define _PROCESS_STRUCTURES_H

#include "common/base.h"
#include "common/structures/config_data.h"

#include "u/u.h"
#include "u/usocket.h"
#include "u/uprocess.h"

#include <set>

class IProcessCallback;
class SocketClient;

enum trninfo_state_t {
      trninfo_not_started,
      trninfo_registered,
      trninfo_availaible,
      trninfo_busy,
      trninfo_dead,
      trninfo_error
};

struct ProcessInfo
{
    virtual ~ProcessInfo() {};
  
//    utime_t startTime; TODO: start time 
    UPID pid; // Every process have PID
    UPHANDLE pHandle;
    SocketClient * socketClient; // Every process communicates with the master process, namely gov
    
    bool locked;
    std::set<IProcessCallback *> clientCallbackSet;
};

typedef std::set<ProcessInfo *>  ProcessList;

struct DatabaseProcessInfo;

struct SessionProcessInfo : public ProcessInfo
{
    trninfo_state_t state; // Transaction state
    session_id sessionId; /* Unique session identifier */

#ifdef UNIX_SOCKET_CHANNEL
    USOCKET socketChannel; // Client socket trading channel
#endif /* UNIX_SOCKET_CHANNEL */

    bool specialTransaction; // True for non-client transactions

    DatabaseProcessInfo * database; // Database process for transaction
    
    void sendSocket (USOCKET clientSock);
};

typedef std::set<SessionProcessInfo *> SessionList;

struct DatabaseProcessInfo : public ProcessInfo
{
    std::string databaseName; // Every process is associated with a database
    bool databaseCreationMode;

    uint32_t databaseId;

    SessionList sessions;
    SessionList availableSessions;

    DatabaseOptions options;
};

#endif /* _PROCESS_STRUCTURES_H */
