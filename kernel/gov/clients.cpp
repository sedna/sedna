#include "gov/clients.h"

#include "gov/cpool.h"
#include "common/structures/listener_states.h"

#include "common/protocol/int_sp.h"
#include "auxiliary/internstr.h"
#include "auxiliary/options/xml_options.h"
#include "u/uhdd.h"

#include <set>

#ifdef EL_DEBUG
#include "common/errdbg/d_printf.h"
#endif /* EL_DEBUG */

#define MAX_TICKET_SIZE 1024
#define MAX_ERROR_LENGTH 1024
#define MAX_DB_NAME 1024
#define MAX_XML_PARAMS 10240

using namespace std;

SocketClient* InternalProcessNegotiation::processData()
{
//     switch(state) {
//       case iproc_initial :
//         if (!communicator->receive()) { return this; }
//         communicator->beginSend(se_Handshake);
//         
//         // Here should be initial key someday
//         
//         communicator->endSend();
//         state = iproc_awaiting_key;
//       case iproc_awaiting_key :
//         if (!communicator->receive()) { return this; }
        
    ticket = communicator->readString(MAX_TICKET_SIZE);
    ProcessManager * pm = worker->getProcessManager();
    ProcessInfo * info = pm->getUnregisteredProcess(ticket);
    if (DatabaseProcessInfo * dbInfo = dynamic_cast <DatabaseProcessInfo *> (info)) {
        return new DatabaseConnectionProcessor(this, ticket);
    }
    
    if (SessionProcessInfo * trnInfo = dynamic_cast <SessionProcessInfo *> (info)) {
        return new SessionConnectionProcessor(this, ticket);
    }

    respondError();
    setObsolete();
    return NULL;
}

void InternalProcessNegotiation::cleanupOnError()
{

}


ServiceConnectionProcessor::~ServiceConnectionProcessor()
{

}


/////////////////////////////class ClientNegotiationManager//////////////////////////////////

SocketClient * ClientNegotiationManager::processData() {
    if (!communicator->receive()) {
        return this;
    }

    size_t length;
    ProtocolVersion protocolVersion;
    
    switch (communicator->getInstruction()) {

#ifdef EL_DEBUG
    d_printf1("Got new connection");
#endif /* EL_DEBUG */
      case se_StartUp :
        length = communicator->getMessageLength();

        // Distincts new protocol from the old one
        // New protocol client sends protocol version immediately
        if (length == 2) {
            // New protocol

            protocolVersion.min = communicator->readChar();
            protocolVersion.maj = communicator->readChar();
            
#ifdef EL_DEBUG
            d_printf1("New protocol connection established");
#endif /* EL_DEBUG */
            
            return new ServiceConnectionProcessor(this, protocolVersion);
        } else {
            return new ClientConnectionProcessor(this, ClientConnectionProcessor::CommonProtocolClient());
        };

        break;
      case se_ConnectProcess :
        return new InternalProcessNegotiation(this);

      default:
        respondError();
        setObsolete();
        return NULL;
    };
    // Message received, parsing it
    
    // !TODO: replace with switch-case
    
/*    if (se_ReceiveSocket == communicator->getInstruction()) { //this can happen only under *nix
        int s_id = communicator->readInt32();
        TRNInfo * info;
        info = worker->findTRNbyId(s_id);
        info->setAvailaible();
        info->unix_s = this->getSocket();
        worker->resumePendingOnTrnClient(string(info->db_name), (TRNConnectionProcessor *) info->processor);
        info = NULL;
        return this;
    }
*/
    return this;
};

////////////////////////////class ClientConnectionProcessor//////////////////////////////////////////

class ClientConnectionCallback : public IProcessCallback {
public:
    ClientConnectionProcessor * client;

    ClientConnectionCallback(ClientConnectionProcessor * _client) : client(_client) {};

    virtual ~ClientConnectionCallback() {
        if (client != NULL && client->activeCallback == this) {
            client->activeCallback = NULL;
        }
    }

    virtual void onError(const char* cause) {
        if (client != NULL) {
            client->respondError(cause);
            client->setObsolete(false);
            client->state = client_close_connection;
        }
    };
};


class ClientDatabaseStartCallback : public ClientConnectionCallback {
public: 
    ClientDatabaseStartCallback(ClientConnectionProcessor* _client) : ClientConnectionCallback(_client) {};
    
    virtual void onSuccess(CallbackMessage * cbm) {
        if (client != NULL) {
            client->sminfo = dynamic_cast<DatabaseProcessInfo *>(cbm->pinfo);
            client->processRequestSession();
        }
    };
};

class ClientSessionStartCallback : public ClientConnectionCallback {
public: 
    ClientSessionStartCallback(ClientConnectionProcessor* _client) : ClientConnectionCallback(_client) {};
    
    virtual void onSuccess(CallbackMessage * cbm) {
        if (client != NULL) {
            client->trninfo = dynamic_cast<SessionProcessInfo *>(cbm->pinfo);
//             client->processSendSocket(client->trninfo.);
        }
    };
};

ClientConnectionProcessor::~ClientConnectionProcessor() {
    if (activeCallback != NULL) {
        activeCallback->client = NULL;
    };

    setObsolete();
};

bool ClientConnectionProcessor::processStartDatabase()
{
    U_ASSERT(state == client_awaiting_sm_and_trn);

    ProcessManager * pm = worker->getProcessManager();
    
    sminfo = pm->getDatabaseProcess(authData.databaseName);

    if (sminfo == NULL) {
        // Try to start storage manager and wait for SM to start
        // NOTE: this function may instantly call onError function in callback!
        pm->startDatabase(authData.databaseName, new ClientDatabaseStartCallback(this));
        return false;
    };

    return processRequestSession();
}

bool ClientConnectionProcessor::processRequestSession()
{
    U_ASSERT(state == client_awaiting_sm_and_trn);
    U_ASSERT(sminfo != NULL);

    ProcessManager * pm = worker->getProcessManager();
    trninfo = pm->getAvailableSession(sminfo);

    if (trninfo == NULL) {
        // NOTE: this function may instantly call onError function in callback!
        pm->requestSession(sminfo, new ClientSessionStartCallback(this));
        return false;
    };

    processSendSocket();

    return true;
}

bool ClientConnectionProcessor::processSendSocket()
{
    U_ASSERT(state == client_awaiting_sm_and_trn);
    U_ASSERT(trninfo != NULL);

    communicator->beginSend(se_SendAuthParameters);
    communicator->endSend();

    state = client_awaiting_auth;

    // TODO
    
    return true;
}

SocketClient * ClientConnectionProcessor::processData() {
    ProcessManager * pm = worker->getProcessManager();

    switch (state) {
    case client_initial_state :
        communicator->beginSend(se_SendSessionParameters);
        communicator->endSend();
        state = client_awaiting_parameters;

    case client_awaiting_parameters:
        if (!communicator->receive()) return this;

        if (communicator->getInstruction() != se_SessionParameters) {
            respondError("Waiting for session parameters");
            return NULL;
        }

        protocolVersion.min = communicator->readChar();
        protocolVersion.maj = communicator->readChar();

//         if (protocolVersion.maj < 3) {
//             respondError("Protocol version is too old");
//             return NULL;
//         } else if (protocolVersion.maj < 5) {
//             respondError("Protocol version is too old");
//             return NULL;
//             // TODO : implement
// //            return new OldProtocolClientProcessor(this);
// 
//         };

        authData.recvInitialAuth(communicator.get());
        
        state = client_awaiting_sm_and_trn;

    case client_awaiting_sm_and_trn:
        if (!processStartDatabase()) {
            return this;
        };
    case client_awaiting_auth:
        if (!communicator->receive()) return this;

        if (communicator->getInstruction() != se_AuthenticationParameters) {
            respondError("");
            return NULL;
        }

        authData.recvPassword(communicator.get());

        U_ASSERT(false);
//        trninfo->sendSocket(clientSocket);

        // TODO : send auth info to trn
        
        state = client_close_connection;
    case client_close_connection:
        setObsolete(false);
        return NULL;

    default:
        respondError("Unexpected instruction at ClientConnectionProcessor");
        setObsolete();
        return NULL;
    }
    
    return this;
}


/////////////////////////////class ServiceConnectionProcessor//////////////////////////////////

SocketClient* ServiceConnectionProcessor::processData()
{
   ProcessManager * pm = worker->getProcessManager();
   
   switch (state) {
     case service_client_initial_state:
        communicator->beginSend(se_SendServiceAuth);
        communicator->endSend();
        state = service_client_awaiting_auth;
        
     case service_client_awaiting_auth:
        if (!communicator->receive()) { return this; }
        if (communicator->getInstruction() != se_SendServiceAuth) {
            respondError("");
            return NULL;
        }
        
        authData.recvServiceAuth(communicator.get());
        
        /* TODO: auth check and respondError if auth failed */
        communicator->beginSend(se_AuthenticationOK);    //here we ask what client wants us to do
        communicator->endSend();
        state = service_client_awaiting_instructions;
       
        
     case service_client_awaiting_instructions:
       if (!communicator->receive()) { return this; }
       switch (communicator->getInstruction()) {
//          case se_StartSM: need to think if we need this at all

         case se_StartUp:   //start new session;
         case se_CreateDbRequest:
           return new CreateDatabaseRequestProcessor(this);
         case se_Stop:
           return new SednaShutdownProcessor(this);
         case se_StopSM:
           return new DatabaseShutdownProcessor(this);
         case se_RuntimeConfig:
//            return new RuntimeConfigProcessor(this);
         case se_DropDbRequest:
//            return new DropDatabaseProcessor(this);
         case se_StartHotBackup:
//           return new HotBackupConnectionProcessor(this);
           
         default:
           respondError("Unexpected instruction at ServiceConnectionProcessor");
           setObsolete();
           return NULL;
       }
       
     default:
       respondError("Unexpected state at ServiceConnectionProcessor");
       setObsolete();
       return NULL;
   }
   return this;
}

SocketClient* DatabaseShutdownProcessor::processData()
{
    U_ASSERT(false);
    return NULL;
}

/////////////////////////////class DatabaseConnectionProcessor////////////////////////////////////////
///* This class is responsible for communications between se_gov and se_sm (create db and start db) */

DatabaseConnectionProcessor::DatabaseConnectionProcessor(WorkerSocketClient* producer, const string& ticket)
  : InternalSocketClient(producer, se_Client_Priority_Cdb, ticket), state(sm_initial_state) { }


SocketClient* DatabaseConnectionProcessor::processData()
{
    // at this point handshake is over -- we must get here from  internal process negotiation.
    ProcessManager * pm = worker->getProcessManager();
    
    switch (state) {
         case sm_initial_state:
         {
        //       NOTE : do not copy/paste
            dbInfo = dynamic_cast<DatabaseProcessInfo *>(pm->getUnregisteredProcess(ticket));

            if (dbInfo == NULL) {
                pm->processRegistrationFailed(ticket, "Invalid ticket");
                respondError();
                return NULL;
            };

            if (dbInfo->databaseCreationMode) {
                communicator->beginSend(se_CdbRegisteringOK); //send params to cdb
                state = sm_awaiting_db_stop;
            } else {
                communicator->beginSend(se_SMRegisteringOK);
            }

            XMLBuilder serializedOptions;
            
            dbInfo->options.saveToXml(&serializedOptions);
            communicator->writeString(serializedOptions.str());
            communicator->endSend();
        }
        case sm_awaiting_db_stop:
        if (!communicator->receive()) { return this; }

        if (se_RegistrationFailed == communicator->getInstruction()) {
            pm->processRegistrationFailed(ticket, communicator->readString(MAX_ERROR_LENGTH));
            setObsolete();
            return NULL;
        }
        
        if (!(se_UnRegisterDB == communicator->getInstruction())) {
            pm->processRegistrationFailed(ticket, "Invalid CDB response");
            respondError("Invalid response");
            return NULL;
        }

        pm->processRegistered(ticket, this);

        //TODO when sm would be more clear: at this point cdb should terminate but sm should wait for shutdown
        // that means that there should be one more message for both.
        
        setObsolete();
        return NULL;
    }
    return NULL;
}

void DatabaseConnectionProcessor::cleanupOnError()
{
    U_ASSERT(false);
    // TODO: FIXME
}

////////////////////////////class CreateDatabaseRequestProcessor//////////////////////////////////////////////

class CreateDatabaseCallback : public IProcessCallback {
public:
    CreateDatabaseRequestProcessor * client;
    
    CreateDatabaseCallback(CreateDatabaseRequestProcessor * _client) : client(_client) {};

    virtual ~CreateDatabaseCallback() {
        if (client != NULL && client->activeCallback == this) {
            client->activeCallback = NULL;
        }
    }

    virtual void onError(const char* cause) 
    {
        if (client != NULL) {
            client->respondError(cause);
            client->setObsolete(false);
            client->state = cdb_fallthrough;

            elog(EL_LOG, ("Request for database creation failed"));
        }
    };
    
    virtual void onSuccess(CallbackMessage* cbm)
    {
        if (client != NULL) {
            client->communicator->beginSend(se_CreateDbOK);
            client->communicator->endSend();
            client->state = cdb_fallthrough;
            
            client->writeDatabaseConfig();

            elog(EL_LOG, ("Request for database creation satisfied"));
        }
    };
};

CreateDatabaseRequestProcessor::CreateDatabaseRequestProcessor(WorkerSocketClient* producer)
  : WorkerSocketClient(producer, se_Client_Priority_Cdb), activeCallback(NULL), state(cdb_awaiting_db_options) {  }

void CreateDatabaseRequestProcessor::writeDatabaseConfig () {
   unsigned int nbytes_written = 0;
   UFile cfgFileHandle;
   USECURITY_ATTRIBUTES *def_sa;
   XMLBuilder serializedOptions;

   ProcessManager * pm = worker->getProcessManager();
   
   DatabaseOptions * dbInfo = pm->getDatabaseOptions(dbName);
   dbInfo->saveToXml(&serializedOptions);
   
   std::string cfgFilePath = pm->getGlobalParameters()->global.dataDirectory + dbName + "_cfg.xml";
      
   if(uCreateSA(&def_sa, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error)!=0) {
       throw USER_EXCEPTION(SE3060);
   }
   
   cfgFileHandle = uCreateFile(cfgFilePath.c_str(),
                                    0,
                                    U_READ_WRITE,
                                    U_WRITE_THROUGH,
                                    def_sa, __sys_call_error);
   if (cfgFileHandle == U_INVALID_FD) {
      throw USER_EXCEPTION2(SE4040, "Can not create database configuration file");
   }

   uReleaseSA(def_sa, __sys_call_error);

   int res = uWriteFile(cfgFileHandle,
                        serializedOptions.str().c_str(),
                        serializedOptions.str().size(),
                        &nbytes_written,
                        __sys_call_error);

   if ( res == 0 || nbytes_written != serializedOptions.str().size()) {
      throw USER_EXCEPTION2( SE4045, cfgFilePath.c_str() );
   }
   if (uCloseFile(cfgFileHandle, __sys_call_error) == 0) {
      throw USER_EXCEPTION2(SE4043, "Can not close configuration file");
   }
}
  
SocketClient* CreateDatabaseRequestProcessor::processData()
{
    ProcessManager * pm = worker->getProcessManager();
    
    switch (state) {
       case cdb_awaiting_db_options:
          if (communicator->getInstruction() != se_CreateDbParams) {
              respondError();
              elog(EL_LOG, ("Database creation aborted: unexpected message, database parameters were expected"));
              return NULL;
          }

          elog(EL_LOG, ("Request for database creation"));

          dbName = communicator->readString(MAX_DB_NAME);

          if (pm->getDatabaseOptions(dbName) != NULL) {
              respondError();
              return NULL;
          };

          pm->setDatabaseOptions(dbName, communicator->readString(MAX_XML_PARAMS));
          pm->createDatabase(dbName, new CreateDatabaseCallback(this));
          
          state = cdb_awaiting_sm_start;
          break;
       case cdb_awaiting_sm_start:
          U_ASSERT(false);
       case cdb_fallthrough:
          return new ServiceConnectionProcessor(this, true);
    }
    return this;
}

SocketClient* SednaShutdownProcessor::processData()
{
    U_ASSERT(false);
    return NULL;
}



SocketClient* SessionConnectionProcessor::processData()
{
    ProcessManager * pm = worker->getProcessManager();
    trnInfo = dynamic_cast<SessionProcessInfo *> (pm->getUnregisteredProcess(ticket));

    switch (state) {
        case trn_initial_state: {
            if (NULL == trnInfo) {
                pm->processRegistrationFailed(ticket, "Invalid ticket");
                respondError();
                return NULL;
            };
        
            XMLBuilder serializedOptions;
                    
            pm->getDatabaseOptions(trnInfo->database->databaseName)->saveToXml(&serializedOptions);
            
            /* send database options to trn */
            communicator->beginSend(se_TrnRegisterOK);
            communicator->writeInt32(trnInfo->sessionId);
            communicator->writeString(serializedOptions.str());
            communicator->endSend();
            state = trn_registered;
        }
        case trn_registered: {
            if (!communicator->receive()) return this;
            if (communicator->getInstruction() != se_ReceiveSocket) {
                pm->processRegistrationFailed(ticket, "Unexpected message received");
                respondError();
                return NULL;
            }
            pm->processRegistered(ticket, this);
            break;
        }
        default: {
            return this;
        }
    }        
    
    return this;
}

void SessionConnectionProcessor::cleanupOnError()
{

}



