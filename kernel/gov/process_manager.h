#ifndef _PROCESS_MANAGER_H_
#define _PROCESS_MANAGER_H_

#include "common/base.h"
#include "common/socketutils/socketutils.h"

#include "process_structures.h"

#include <map>
#include <set>

class ClientConnectionProcessor;

struct SocketClientGreater {
    bool operator()( const SocketClient * & lx, const SocketClient * & rx ) const {
        if (lx->getType() == rx->getType()) {
            return lx - rx < 0;
        } else {
            return lx->getType() < rx->getType();
        }
    }
};

typedef std::set<SocketClient *, SocketClientGreater> SocketClientList;
typedef std::set<SocketClient *> SocketClientList;
typedef std::set<ClientConnectionProcessor *> ClientConnectionList;
typedef std::map<std::string, DatabaseProcessInfo *> DatabaseMap;
typedef std::map<std::string, ClientConnectionList > SessionClientMap;
typedef std::multimap<std::string, SocketClient * > DatabaseServiceClientMap;

typedef std::map<session_id, SessionProcessInfo *> SessionMap;

// TODO: Move it into another file
class ProcessManager {
    ProcessList processList;
    SocketClientList clientConnectionList;

    DatabaseMap databaseMap;
    SessionClientMap clientsWaitingForSession;
    DatabaseServiceClientMap clientsWaitingForDatabase;

    SessionMap sessionIndexById;

    session_id lastSessionId;
public:
    session_id getNewSessionId() { return lastSessionId++; };

    void addSessionProcess(SessionProcessInfo * session);
    void changeSessionId(session_id oldSid, session_id newSid);
    void removeSessionProcess(session_id sid);
    void removeSessionProcess(SessionProcessInfo * session);

    void addClientConnection(SocketClient * cc);
    void removeClientConnection(SocketClient * cc);

    void addDatabase(DatabaseProcessInfo * sm);
    void removeDatabase(DatabaseProcessInfo * sm);

    DatabaseProcessInfo * getDatabaseProcess(const std::string& dbName);

    void addClientWaitingForSession(ClientConnectionProcessor * cc);
    ClientConnectionList * getClientsForSession(const std::string& dbName);

    void addServiceClientWaitingForDatabase(SocketClient * client);
    SocketClient * getServiceClient(const std::string& dbName);

    ProcessManager() : lastSessionId(0) {};
    ~ProcessManager();
};

/*
class ProcessManagerLock {
public:
    ProcessManagerLock(ProcessManager *);
    ~ProcessManagerLock();

    void lock();
    void unlock();
};
*/

#endif /* _PROCESS_MANAGER_H_ */

