#ifndef _PROCESS_MANAGER_H_
#define _PROCESS_MANAGER_H_

#include "common/base.h"
#include "common/socketutils/socketutils.h"

#include "worker_client.h"
#include "process_structures.h"

#include <map>
#include <set>
#include <queue>
#include <stack>


/* NOTE:
 * Callback idea:
 * 1) client connects to gov.
 * 2) connectionProcessor is created; 
 * 3) connectionProcessor starts db creation, callback is created with pm->CreateDatabase(for example).
 * 4) connectionProcessor returns new processor (service processor for example)
 * 5) when databaseConnectionProcessor execution is finished, it calls callback in associated ProcessInfo
 * 6) callback responds to client the result
 * 
 */


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

typedef std::vector<WorkerSocketClient *> UnsortedSocketClientList;
typedef std::set<WorkerSocketClient *, SocketClientGreater> SocketClientList;

typedef std::map<USOCKET, WorkerSocketClient *> SystemSocketMap;
typedef std::set<ClientConnectionProcessor *> ClientConnectionList;
typedef std::map<std::string, DatabaseProcessInfo *> DatabaseMap;
typedef std::map<std::string, ClientConnectionList > SessionClientMap;
typedef std::multimap<std::string, SocketClient * > DatabaseServiceClientMap;

typedef std::map<session_id, SessionProcessInfo *> SessionMap;
typedef std::string ClientTicket;

typedef std::map<ClientTicket, ProcessInfo *> ProcessMap;

struct CallbackMessage {
    IProcessCallback * callback;
    
    enum cbmessage_t {
        Error,
        Success,
    };
    
    cbmessage_t result;

    ProcessInfo * pinfo;
    WorkerSocketClient * socketClient;
    const char * messageInfo;
};

class IProcessCallback {
public:
    virtual ~IProcessCallback() {};

    virtual void onError(const char * cause) = 0;
    virtual void onSuccess(CallbackMessage * cbm) = 0;
};

class ProcessManager {
    GlobalParameters & parameters;

    /* List of all launched process of Sedna instance */
    ProcessList processList;

//    SystemSocketMap systemSocketMap;

    /* Lookup for launched processes by ticket id */
    ProcessMap processMap;

    /* Database lookup by dbmap */
    DatabaseMap databaseMap;

    /* Session process lookup by session */
    SessionMap sessionIndexById;

    session_id lastSessionId;

    /* on database drop we shoul push it's db_id here  */     
    std::stack<int> availaibleDatabaseIds;

    std::queue<CallbackMessage> requestProcessQueue;

    void callbackError(IProcessCallback * cb, const char * messageInfo);
    void callbackSuccess(IProcessCallback * cb, ProcessInfo * pinfo, WorkerSocketClient * socketClient);

    bool requestsPending;
    void doProcessRequests();
    void generateTicket(ClientTicket & ticket);
    void execStorageManagerProcess(const std::string& ticket, DatabaseProcessInfo * databaseProcessInfo);
    void execTransactionProcess(const std::string& ticket, SessionProcessInfo * sessionProcessInfo);
    
public:
    ProcessManager(GlobalParameters & _parameters);
    ~ProcessManager();

    session_id getNewSessionId() { return lastSessionId++; };

    void addSessionProcess(SessionProcessInfo * session);
    void changeSessionId(session_id oldSid, session_id newSid);
    void removeSessionProcess(session_id sid);
    void removeSessionProcess(SessionProcessInfo * session);

    void startDatabase(const std::string& dbName, const std::string& options, IProcessCallback * callback);
    void shutdownDatabase(DatabaseProcessInfo * sm, IProcessCallback * callback);

    DatabaseOptions * getDatabaseOptions(const std::string& dbName);
    void setDatabaseOptions(const std::string& dbName, const std::string& xmlOptions);

    void removeDatabaseProcess(const std::string& dbName);
    void createDatabase(const std::string& dbName, const std::string& options, IProcessCallback * callback);
    void onDatabaseCreationFinished(const DatabaseOptions& options);
    
    void requestSession(DatabaseProcessInfo * sm, IProcessCallback * callback);

    void processRequests() {
        if (requestsPending) { doProcessRequests(); }
    };

    ProcessInfo * getUnregisteredProcess(const std::string& ticket);
    void processRegistered(const std::string& ticket, WorkerSocketClient * processor);
    void processRegistrationFailed(const std::string& ticket, const std::string& reason);

    DatabaseProcessInfo * getDatabaseProcess(const std::string& dbName)
    {
        DatabaseMap::iterator it = databaseMap.find(dbName);

        if (it == databaseMap.end()) {
            return NULL;
        } else {
            return it->second;
        };
    };

    SessionProcessInfo * getAvailableSession(DatabaseProcessInfo * sm) const
    {
        return NULL;
    };
    

    GlobalParameters * getGlobalParameters() { return &parameters; };

/*    
    WorkerSocketClient * getClientBySocket(USOCKET socket) const {
        SystemSocketMap::const_iterator it = systemSocketMap.find(socket);

        if (it == systemSocketMap.end()) {
            return it->second;
        } else {
            return NULL;
        };
    };
*/
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

