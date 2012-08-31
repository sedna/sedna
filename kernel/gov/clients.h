#ifndef CLIENTS_H
#define CLIENTS_H

#include "common/base.h"

#include "common/socketutils/socketutils.h"
#include "common/structures/config_data.h"

#include "worker_client.h"
#include "process_manager.h"

#include <string>

class CreateDatabaseCallback;
struct DatabaseProcessInfo;
struct SessionProcessInfo;

class Worker;

enum client_state_t {
      client_initial_state,
      client_awaiting_parameters,
      client_awaiting_auth,
      client_awaiting_sm_and_trn,
      client_close_connection,
};

enum service_client_state_t {
      service_client_initial_state,  //we get here at initial construction at negotiationProcessor
      service_client_awaiting_auth,  //here we wait for login and password
      service_client_awaiting_instructions  //we get here after each service operation -- we already have auth.
};

enum cdb_state_t {
      cdb_awaiting_db_options,
      cdb_awaiting_sm_start,
      cdb_fallthrough
};

enum sm_internal_state_t {
      sm_initial_state,
      sm_awaiting_db_stop
};

enum trn_state {
    trn_initial_state,
    trn_registered
};

struct ProtocolVersion {
    uint8_t min;
    uint8_t maj;
};

struct ServiceProtocolClient {
    const ProtocolVersion & protocolVersion;
    const std::string & databaseName;

    explicit ServiceProtocolClient(
      const ProtocolVersion & _protocolVersion,
      const std::string & _databaseName)
        : protocolVersion(_protocolVersion),
          databaseName(_databaseName) {};
};

class InternalProcessNegotiation : public InternalSocketClient {
//     enum {
//         iproc_initial,
//         iproc_awaiting_key,
//         iproc_ticket_recieved
//     } state;
public:
    InternalProcessNegotiation(WorkerSocketClient* producer)
        : InternalSocketClient(producer, se_Client_Priority_SM, std::string()) {}
    
    virtual SocketClient* processData();
    virtual void cleanupOnError();
};



class ClientNegotiationManager : public WorkerSocketClient {
public:
                                ClientNegotiationManager(Worker * _parent, USOCKET sock) 
                                  : WorkerSocketClient(_parent, sock, se_Client_Priority_Negot) { };

    virtual SocketClient *      processData();
};

class SessionConnectionProcessor : public InternalSocketClient {
private:
    session_id                  sid;
    SessionProcessInfo *        trnInfo;
    trn_state                   state;
    
public:
    SessionConnectionProcessor  (WorkerSocketClient * producer, const std::string& _ticket)
      : InternalSocketClient(producer, se_Client_Priority_TRN, ticket), sid(0), trnInfo(NULL), state(trn_initial_state) { }
                                
    virtual SocketClient * processData ();
    virtual void cleanupOnError();
};

class SednaShutdownProcessor : public WorkerSocketClient {
public:
    SednaShutdownProcessor(WorkerSocketClient * producer)
      : WorkerSocketClient(producer, se_Client_Priority_Stop) {};

    virtual SocketClient * processData();
};


class DatabaseShutdownProcessor : public WorkerSocketClient {
public:
    DatabaseShutdownProcessor(WorkerSocketClient * producer)
      : WorkerSocketClient(producer, se_Client_Priority_SMsd) {};

    virtual SocketClient * processData();
};


class ServiceConnectionProcessor : public WorkerSocketClient {
private:
    ProtocolVersion protocolVersion;
    TopLevelAuthentication authData;
    
    service_client_state_t state;
    
public:
    ServiceConnectionProcessor(WorkerSocketClient * producer, ProtocolVersion _protocolVersion)
      : WorkerSocketClient(producer, se_Client_Priority_Client), protocolVersion(_protocolVersion), state(service_client_initial_state) { };
    
    ServiceConnectionProcessor(WorkerSocketClient * producer, bool toContinue)
      : WorkerSocketClient(producer, se_Client_Priority_Client), state(service_client_awaiting_instructions) { };

    virtual ~ServiceConnectionProcessor();
    virtual  SocketClient * processData();
};

class ClientConnectionCallback;

class ClientConnectionProcessor : public WorkerSocketClient {
private:
    friend class ClientConnectionCallback;
    ClientConnectionCallback * activeCallback;

    ProtocolVersion protocolVersion;
    client_state_t state;

    CommonClientAuthentication authData;
public:
    DatabaseProcessInfo * sminfo;
    SessionProcessInfo * trninfo;
    SessionConnectionProcessor * associatedSessionClient;

    struct CommonProtocolClient { };

    explicit ClientConnectionProcessor(WorkerSocketClient * producer, const CommonProtocolClient & )
        : WorkerSocketClient(producer, se_Client_Priority_Client), 
          activeCallback(NULL), 
          state(client_initial_state), 
          sminfo(NULL), 
          trninfo(NULL),
          associatedSessionClient(NULL) {};

    explicit ClientConnectionProcessor(WorkerSocketClient * producer, const ServiceProtocolClient & _authData)
        : WorkerSocketClient(producer, se_Client_Priority_Client),
          activeCallback(NULL), 
          state(client_awaiting_sm_and_trn), 
          sminfo(NULL), 
          trninfo(NULL),
          associatedSessionClient(NULL) {
        authData.databaseName = _authData.databaseName;
    };
        
    virtual ~ClientConnectionProcessor();
    virtual SocketClient * processData();

    bool processStartDatabase();
    bool processRequestSession();
    bool processSendSocket();
};


class DatabaseConnectionProcessor : public InternalSocketClient {
  private:
    sm_internal_state_t         state;
    std::string                 dbName;
    DatabaseProcessInfo *       dbInfo;
  public:
                                DatabaseConnectionProcessor   (WorkerSocketClient * producer, const std::string & ticket);
    void                        registerCdb              ();
    virtual SocketClient *      processData              ();
    virtual void                cleanupOnError();
};


class CreateDatabaseRequestProcessor : public WorkerSocketClient {
  private:
    friend class CreateDatabaseCallback;
    CreateDatabaseCallback      * activeCallback;

    std::string                 dbName;
    cdb_state_t                 state;
    
    void                        writeDatabaseConfig ();
    
  public:
                                CreateDatabaseRequestProcessor (WorkerSocketClient * producer);
    virtual SocketClient *      processData             (void);
};

class HotBackupConnectionProcessor : public WorkerSocketClient {
  public:
                                HotBackupConnectionProcessor(WorkerSocketClient * producer);
    virtual SocketClient *      processData();
};


#endif /* CLIENTS_H */
