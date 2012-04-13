#ifndef CLIENTS_H
#define CLIENTS_H

#include "common/base.h"

#include "common/socketutils/socketutils.h"
#include "common/structures/config_data.h"

#include "worker_client.h"

#include <string>

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
      cdb_awaiting_parameters,
      cdb_awaiting_auth,
      cdb_awaiting_db_options,
      cdb_awaiting_sm_start
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

class ClientNegotiationManager : public WorkerSocketClient {
public:
                                ClientNegotiationManager(Worker * _parent, USOCKET sock) 
                                  : WorkerSocketClient(_parent, sock, se_Client_Priority_Negot) { };

    virtual SocketClient *      processData();
};

class DatabaseConnectionProcessor : public InternalSocketClient {
private:
    DatabaseProcessInfo * info;
public:
    DatabaseConnectionProcessor   (WorkerSocketClient * producer)
      : WorkerSocketClient(producer, se_Client_Priority_SMsd), info(NULL) { }

    void registerSM();
    virtual SocketClient * processData();
    virtual void cleanupOnError();
};

class SessionConnectionProcessor : public InternalSocketClient {
private:
    session_id                  sid;
    SessionProcessInfo *        trninfo;
public:
    SessionConnectionProcessor  (WorkerSocketClient * producer)
      : InternalSocketClient(producer, se_Client_Priority_TRN), sid(0), trninfo(NULL) { }
                                
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
    CommonClientAuthentication authData;
    
    service_client_state_t state;
    
public:
    ServiceConnectionProcessor(WorkerSocketClient * producer, ProtocolVersion _protocolVersion)
      : WorkerSocketClient(producer, se_Client_Priority_Client), protocolVersion(_protocolVersion) { };
      
    virtual ~ServiceConnectionProcessor();
    virtual  SocketClient * processData();
};


class ClientConnectionProcessor : public WorkerSocketClient {
private:
    friend class ClientConnectionCallback;

    ClientConnectionCallback * activeCallback;

    ProtocolVersion protocolVersion;
    client_state_t state;

    CommonClientAuthentication authData;

    DatabaseProcessInfo * sminfo;
    SessionProcessInfo * trninfo;
    SessionConnectionProcessor * associatedSessionClient;
public:
    struct CommonProtocolClient { };

    explicit ClientConnectionProcessor(WorkerSocketClient * producer, const CommonProtocolClient & )
        : activeCallback(NULL), WorkerSocketClient(producer, se_Client_Priority_Client),
          state(client_initial_state), sminfo(NULL), trninfo(NULL),
          associatedSessionClient(NULL) {};

    explicit ClientConnectionProcessor(WorkerSocketClient * producer, const ServiceProtocolClient & _authData)
        : activeCallback(NULL), WorkerSocketClient(producer, se_Client_Priority_Client),
          state(client_awaiting_sm_and_trn), sminfo(NULL), trninfo(NULL),
          associatedSessionClient(NULL) {
        authData.databaseName = _authData.databaseName;
    };
        
    virtual ~ClientConnectionProcessor();
    virtual SocketClient * processData();

    bool processStartDatabase();
    bool processRequestSession();
    bool processSendSocket();
};


class CdbConnectionProcessor : public InternalSocketClient {
  private:
    std::string                      db_name;
  public:
                                CdbConnectionProcessor   (WorkerSocketClient * producer);
    void                        registerCdb              ();
    virtual SocketClient *      processData              ();
    virtual void                cleanupOnError();
};


class CdbRequestProcessor : public WorkerSocketClient {
  private:
    struct DatabaseOptions *    cdbParams;
    cdb_state_t                 state;
    
  public:
                                CdbRequestProcessor  (WorkerSocketClient * producer);
//     void                        authAndPrepare          (void);
    virtual SocketClient *      processData             (void);
};

class HotBackupConnectionProcessor : public WorkerSocketClient {
  public:
                                HotBackupConnectionProcessor(WorkerSocketClient * producer);
    virtual SocketClient *      processData();
};


#endif /* CLIENTS_H */