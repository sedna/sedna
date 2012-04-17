#ifndef _PROCESS_MANAGER_H_
#define _PROCESS_MANAGER_H_

#include "common/base.h"
#include "common/socketutils/socketutils.h"

#include "worker_client.h"
#include "process_structures.h"

#include <map>
#include <set>

class ClientConnectionProcessor;

struct SocketClientGreater {
    bool operator()( const WorkerSocketClient * & lx, const WorkerSocketClient * & rx ) const {
        if (lx->getPriority() == rx->getPriority()) {
            return lx - rx < 0;
        } else {
            return lx->getPriority() < rx->getPriority();
        }
    }
};

typedef std::set<WorkerSocketClient *, SocketClientGreater> SocketClientList;

typedef std::map<USOCKET, WorkerSocketClient *> SystemSocketMap;
typedef std::set<ClientConnectionProcessor *> ClientConnectionList;
typedef std::map<std::string, DatabaseProcessInfo *> DatabaseMap;
typedef std::map<std::string, ClientConnectionList > SessionClientMap;
typedef std::multimap<std::string, SocketClient * > DatabaseServiceClientMap;

typedef std::map<session_id, SessionProcessInfo *> SessionMap;
typedef std::string ClientTicket;

typedef std::map<ClientTicket, std::pair<DatabaseOptions *, bool> > DatabaseProcessMap;

class IProcessCallback {
public:
    virtual ~IProcessCallback() {};

    virtual void onError(const char * cause) = 0;
    virtual void onDatabaseProcessStart(DatabaseProcessInfo *, WorkerSocketClient *) = 0;
    virtual void onSessionProcessStart(SessionProcessInfo *, WorkerSocketClient *) = 0;
    virtual void onDatabaseShutdown() = 0;
};

// TODO: Move it into another file
class ProcessManager {
    GlobalParameters parameters;

    ProcessList processList;
    SystemSocketMap systemSocketMap;

    DatabaseProcessMap databaseProcessMap;
    DatabaseMap databaseMap;
    SessionMap sessionIndexById;

    session_id lastSessionId;

    bool requestsPending;
    void doProcessRequests();
public:
    ProcessManager() : lastSessionId(0), requestsPending(false) {};
    ~ProcessManager();

    session_id getNewSessionId() { return lastSessionId++; };

    void addSessionProcess(SessionProcessInfo * session);
    void changeSessionId(session_id oldSid, session_id newSid);
    void removeSessionProcess(session_id sid);
    void removeSessionProcess(SessionProcessInfo * session);

    void startDatabase(const std::string& dbName, IProcessCallback * callback);
    void shutdownDatabase(DatabaseProcessInfo * sm, IProcessCallback * callback);

    void createDatabase(const DatabaseOptions& options, IProcessCallback * callback);
    void onDatabaseCreationFinished(const DatabaseOptions& options);
    
    void requestSession(DatabaseProcessInfo * sm, IProcessCallback * callback);

    void processRequests() {
        if (requestsPending) { doProcessRequests(); }
    };
    
    DatabaseProcessInfo * getDatabaseProcess(const std::string& dbName);

    GlobalParameters * getGlobalParameters() { return &parameters; };

    SessionProcessInfo * getAvailableSession(DatabaseProcessInfo * sm) const;
    
    WorkerSocketClient * getClientBySocket(USOCKET socket) const {
        SystemSocketMap::const_iterator it = systemSocketMap.find(socket);

        if (it == systemSocketMap.end()) {
            return it->second;
        } else {
            return NULL;
        };
    };

    SessionProcessInfo * getSessionById(session_id sid) const {
        SessionMap::const_iterator it = sessionIndexById.find(sid);

        if (it == sessionIndexById.end()) {
            return it->second;
        } else {
            return NULL;
        };
    };
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

