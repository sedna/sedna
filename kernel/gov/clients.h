#ifndef CLIENTS_H
#define CLIENTS_H

#include "common/socketutils/socketutils.h"
#include "gov/processes.h"
#include "common/structures/listener_states.h"
#include "common/structures/cdb_structures.h"
#include "common/structures/sm_structures.h"
#include "common/structures/tr_structures.h"
#include "common/xptr.h"

#include "worker_client.h"

class Worker;
struct SessionParameters;
class ClientConnectionProcessor;

inline static
void socket_check(int result) {
    if (U_SOCKET_ERROR == result) {
        throw SYSTEM_EXCEPTION(string(usocket_error_translator()).c_str());
    }
};

class ClientNegotiationManager : public WorkerSocketClient {
  public:
                                ClientNegotiationManager(Worker * _parent, USOCKET sock) 
                                  : WorkerSocketClient(_parent, sock, se_Client_Priority_Negot) { };

    virtual SocketClient *      processData();
};

class SednaStopProcessor : public WorkerSocketClient {
  public:
                                SednaStopProcessor(WorkerSocketClient * producer);
    void                        getParams();
    
    virtual SocketClient *      processData();
};


class SMStopProcessor : public WorkerSocketClient {
  public:
                                SMStopProcessor(WorkerSocketClient * producer);
    void                        getParams();
                                
    virtual SocketClient *      processData();
};



class SMConnectionProcessor : public InternalSocketClient {
  private:
    struct SMInfo *             info;
    
  public:
                                SMConnectionProcessor   (WorkerSocketClient * producer);
    void                        registerSM              ();
    virtual SocketClient *      processData             ();
    virtual void                cleanupOnError();
};

class CdbConnectionProcessor : public InternalSocketClient {
  private:
    string                      db_name;
    
  public:
                                CdbConnectionProcessor   (WorkerSocketClient * producer);
    void                        registerCdb              ();
    virtual SocketClient *      processData              ();
    virtual void                cleanupOnError();
};


class TRNConnectionProcessor : public InternalSocketClient {
  private:
    session_id                  s_id;
    TRNInfo *                   trninfo;

  public:
                                TRNConnectionProcessor  (WorkerSocketClient * producer);
    void                        registerTRN             ();
    void                        setAvailable            ();
    virtual SocketClient *      processData             ();
    virtual void                cleanupOnError();
    
    friend class ClientConnectionProcessor;
};

class ClientConnectionProcessor : public WorkerSocketClient {
  private:
    client_state_t              state;
    SMInfo *                    sminfo;
    TRNInfo *                   trninfo;
    TRNConnectionProcessor *    trnProcessor;

    struct SessionParameters    sessionParams;
    bool                        trn_launch;
    bool                        is_socket_transmitted;
    
  public:
                                ClientConnectionProcessor(WorkerSocketClient * producer);

    virtual                    ~ClientConnectionProcessor();
    virtual SocketClient *      processData();
    void                        setTrn (TRNConnectionProcessor * trn);
};

class CdbRequestProcessor : public WorkerSocketClient {
  private:
    struct SessionParameters    sessionParams;
    struct CdbParameters *      cdbParams;
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