#ifndef _PROCESS_STRUCTURES_H
#define _PROCESS_STRUCTURES_H

#include "common/base.h"
#include "common/u/usocket.h"
#include "common/u/uprocess.h"
#include "common/structures/config_data.h"

#include <set>

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
  
    UPID pid; // Every process have PID
    UPHANDLE pHandle;
    SocketClient * socketClient; // Every process communicates
                  // with the master process, namely gov
};

typedef std::set<ProcessInfo *>  ProcessList;

struct SessionProcessInfo : public ProcessInfo
{
    trninfo_state_t state; // Transaction state
    session_id sessionId; /* Unique session identifier */

#ifdef UNIX_SOCKET_CHANNEL
    USOCKET socketChannel; // Client socket trading channel
#endif /* UNIX_SOCKET_CHANNEL */

    bool specialTransaction; // True for non-client transactions

    DatabaseProcessInfo * database; // Database process for transaction
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
