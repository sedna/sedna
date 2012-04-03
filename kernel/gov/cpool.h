#ifndef CPOOL_H
#define CPOOL_H

#include <vector>
#include <map>
#include "common/sedna.h"
#include "common/base.h"
#include "common/u/uprocess.h"
#include "gov/config_utils.h"

#include "gov/processes.h"
#include "gov/clients.h"
#include "common/socketutils/socketutils.h"


using namespace std;

#define COMMON_BUFFER_LEN 1024*64

typedef vector<WorkerSocketClient *>          ClientList;
typedef map<string, vector<struct TRNInfo> >   TrnMap;  //first value for database name. I need this for search and maybe it's not optimal
typedef map<string, vector<WorkerSocketClient *> >   ClientsPendingOnSmMap;    //first value for db name. The vector contains objects that are pending on dbstart
typedef map<string, vector<ClientConnectionProcessor *> >   ClientsPendingOnTrnMap;    //first value for db name. The vector contains objects that are pending on trnstart //not to forget: as trn will register on gov after it's availaible, this would work correctly.
typedef map<string, struct SMInfo>             SMMap;   //first value for database name.
typedef map<string, struct CdbParameters>      CdbMap;
typedef map<string, vector<WorkerSocketClient *> >   SmsdPendingOnSmMap;  //first value for db name, vector contains clients waiting for SM shutdown
class BaseException { };


enum { 
  trnSessionRegistered = 0,
  trnFirstTransaction = 1,
  trnNotReady = -1,
  trnNoMoreSessionSlots = -2
};

class Worker {
  private:
        U_SSET                  allSet, 
                                readySet;

        ClientList              clientList;
        ClientList              newClients;
        
        ClientList              clientsWaitingForShutdown;
        ClientsPendingOnSmMap   clientsPendingOnSM;
        ClientsPendingOnSmMap   clientsPendingOnCdb;
        ClientsPendingOnTrnMap  clientsPendingOnTrn;
        SmsdPendingOnSmMap      smsdClients;
        
        TrnMap                  availaibleTrns;
        SMMap                   runningSms;
        CdbMap                  creatingDbs;
        
        session_id              lastRegisteredSessId;
        
        USOCKET                 maxfd;
        WorkerSocketClient *          ownListenerSocket;

        bool                    shutting_down;
        
        void                    processClientData(WorkerSocketClient * client);
  public:
        /* usable buffer */
        char                    commonBuffer[COMMON_BUFFER_LEN];

        //!FIXME it's temporary: need to have gov_config_struct in the other way
        
        gov_config_struct *     cfg;
        
        
                                Worker(gov_config_struct * _cfg) 
    : lastRegisteredSessId(0), maxfd(0), shutting_down(false), cfg(_cfg)  { U_SSET_ZERO(&allSet); }

        WorkerSocketClient *          addClient           (WorkerSocketClient * stream);
        void                    deleteClient        (WorkerSocketClient * stream);
        
        int                     addShutdownClient   (WorkerSocketClient * client);
        int                     addCdb              (CdbParameters * cdbinfo);
        int                     removeCdb           (string dbname);
        int                     addSM               (SMInfo * sminfo);
        int                     removeSM            (string dbname);
        int                     addTRN              (TRNInfo * trninfo, 
                                                     session_id * sess_id);
        int                     removeTRN           (string dbname, session_id sess_id);
        
        int                     stopAllDatabases    (void);
        int                     stopSM              (string dbname);
        
        int                     startCdb            (CdbParameters* cdbinfo);
        int                     startStorageManager (char* dbname);
        int                     startStorageManager (SMInfo* sminfo);
        int                     startTransactionProcess(SessionParameters* sess_params);
        bool                    findAvailaibleSM    (SMInfo* &sminfo, char* dbname);
        bool                    findAvailaibleTRN   (TRNInfo* &trninfo, char* dbname);
        TRNInfo *               findTRNbyId         (session_id s_id);
        CdbParameters *         findCdbByDbName     (string dbname);
        
        void                    addPendingOnCdbClient(char* dbname, WorkerSocketClient * client);
        void                    addPendingOnSmClient (char* dbname, WorkerSocketClient * client);
        void                    addPendingOnTrnClient(char* dbname, ClientConnectionProcessor * client);
        void                    addPendingOnSMSmsd   (string dbname, WorkerSocketClient * client);
                                                     
        void                    resumePendingOnCdbClients(string dbname);
        void                    resumePendingOnSMClients(string dbname);
        void                    resumePendingOnTrnClient(string dbname, TRNConnectionProcessor * trnProcessor); //resumes only one client!
        void                    resumePendingOnSMSmsd(string dbname);
        
        void                    createUnixListener  (TRNInfo * trninfo);

        WorkerSocketClient *          createListener      ();
        
        void                    setShutdown () { shutting_down = true;}
        bool                    getShutdown () { return shutting_down;}
        
        void                    run();
        
};


#endif /* CPOOL_H */
