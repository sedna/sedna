#include "gov/clients.h"
#include "gov/cpool.h"

#include "common/structures/listener_states.h"
#include "common/protocol/int_sp.h"
#include "common/protocol/messages/MStartDatabase.h"

#include "auxiliary/internstr.h"
#include "auxiliary/options/xml_options.h"

#include "u/uhdd.h"

#include <sp_defs.h>

#include <set>

#ifdef EL_DEBUG
#include "common/errdbg/d_printf.h"
#endif /* EL_DEBUG */

using namespace std;

SocketClient* InternalProcessNegotiation::processData()
{
    ticket = communicator->readString(MAX_TICKET_SIZE);

    ProcessManager * pm = worker->getProcessManager();
    ProcessInfo * info = pm->getUnregisteredProcess(ticket);

    if (DatabaseProcessInfo * dbInfo = dynamic_cast <DatabaseProcessInfo *> (info)) {
        return new DatabaseConnectionProcessor(this, ticket, info);
    }

    if (SessionProcessInfo * trnInfo = dynamic_cast <SessionProcessInfo *> (info)) {
        return new SessionConnectionProcessor(this, ticket, info);
    }

    respondError();
    setObsolete();
    return NULL;
}

void InternalProcessNegotiation::cleanupOnError()
{

}

void InternalProcessNegotiation::shutdown()
{
    communicator->beginSend(se_Stop);
    communicator->endSend();
    //no states here; we don't even know what process has been connected so we don't care
    setObsolete();
}


ServiceConnectionProcessor::~ServiceConnectionProcessor()
{

}


/////////////////////////////class ClientNegotiationManager//////////////////////////////////

SocketClient * ClientNegotiationManager::processData() {
    WORKER_READ_MESSAGE_SAFE

    size_t length;
    ProtocolVersion protocolVersion;

    switch (communicator->getInstruction()) {
      case se_StartUp :
        length = communicator->getMessageLength();

        // Distincts new protocol from the old one
        // New protocol client sends protocol version immediately
        if (length == 2) {
            // New protocol

            protocolVersion.maj = communicator->readChar();
            protocolVersion.min = communicator->readChar();

            d_printf1("New protocol connection established\n");

            return new ServiceConnectionProcessor(this, protocolVersion);
        } else {
            return new ClientConnectionProcessor(this, ClientConnectionProcessor::CommonProtocolClient());
        };

        break;
      case se_int_ConnectProcess :
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
        pm->startDatabase(authData.databaseName, "<databaseOptions/>", new ClientDatabaseStartCallback(this));
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
        WORKER_READ_MESSAGE_SAFE

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
        WORKER_READ_MESSAGE_SAFE

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
        WORKER_READ_MESSAGE_SAFE
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
       WORKER_READ_MESSAGE_SAFE
       switch (communicator->getInstruction()) {
//          case se_StartSM: need to think if we need this at all

         case se_StartUp:   //start new session;
         case se_CreateDatabaseRequest:
           return new CreateDatabaseRequestProcessor(this);
         case se_Stop:
           return new SednaShutdownProcessor(this);
         case se_StopSM:
           return new DatabaseShutdownProcessor(this);
         case se_RuntimeConfig:
//            return new RuntimeConfigProcessor(this);
         case se_DropDatabaseRequest:
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
    return       NULL;
}

/////////////////////////////class DatabaseConnectionProcessor////////////////////////////////////////
///* This class is responsible for communications between se_gov and se_sm (create db and start db) */

DatabaseConnectionProcessor::DatabaseConnectionProcessor(WorkerSocketClient* producer, const string& ticket, ProcessInfo * _process)
  : InternalSocketClient(producer, se_Client_Priority_Cdb, ticket, _process), state(sm_initial_state) { }


SocketClient* DatabaseConnectionProcessor::processData()
{
    // at this point handshake is over -- we must get here from  internal process negotiation.
    ProcessManager * pm = worker->getProcessManager();
    DatabaseProcessInfo * dbInfo = static_cast<DatabaseProcessInfo *>(process);

    switch (state) {
        case sm_initial_state:
        {
        //       NOTE : do not copy/paste
            if (process == NULL) {
                pm->processRegistrationFailed(ticket, "Invalid ticket");
                respondError();
                return NULL;
            };

            std::ostringstream options;
            pm->getGlobalParameters()->saveDatabaseToStream(dbInfo->databaseName, &options);

            if (dbInfo->databaseCreationMode) {
                proto::StartDatabase(options.str(), se_int_CreateDatabaseInternal) >> *communicator;
            } else {
                proto::StartDatabase(options.str(), se_int_StartDatabaseInternal) >> *communicator;
            }

            state = sm_confirmation;
        }

        case sm_confirmation: {
            WORKER_READ_MESSAGE_SAFE

            std::string errorString = "Unknown message from client or socket closed";

            switch(communicator->getInstruction()) {
            case se_Ok:
                pm->processRegistered(ticket, this);

                if (dbInfo->databaseCreationMode) {
                    writeDatabaseConfig();
                    dbInfo->databaseCreationMode = false;
                };

                break;
            case se_int_SoftError:
                communicator->readInt32();
            case se_int_RegistrationFailed:
                errorString = communicator->readString(MAX_ERROR_LENGTH);
            default:
                pm->processRegistrationFailed(ticket, errorString);
                setObsolete();
                return NULL;
            };
            break;
        }
        case sm_shutdown: {
            WORKER_READ_MESSAGE_SAFE
            
            if (se_int_SuccessefulStop != communicator->getInstruction()) {
              elog(EL_ERROR, ("Database %s has failed during shutdown", dbInfo->databaseName.c_str()));
            } else {
              elog(EL_INFO, ("Database %s has been shut down successfully", dbInfo->databaseName.c_str()));
            }
            pm->removeDatabaseProcess(dbInfo->databaseName);
            setObsolete();
            return NULL;
        }
        default: {

        }

        setObsolete();
        return NULL;
    }
    return NULL;
}

void DatabaseConnectionProcessor::shutdown()
{
    communicator->beginSend(se_Stop);
    communicator->endSend();
    state = sm_shutdown;
}

void DatabaseConnectionProcessor::writeDatabaseConfig () {
    unsigned int nbytes_written = 0;
    UFile cfgFileHandle;
    USECURITY_ATTRIBUTES *def_sa;

    std::ostringstream options;

    GlobalParameters * globals = worker->getProcessManager()->getGlobalParameters();
    DatabaseProcessInfo * dbInfo = static_cast<DatabaseProcessInfo *>(process);
    globals->saveDatabaseToStream(dbInfo->databaseName, &options);

    std::string cfgFilePath = globals->global.dataDirectory + dbInfo->databaseName + ".conf.xml";

    if(uCreateSA(&def_sa, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error)!=0) {
        throw USER_EXCEPTION(SE3060);
    }

    cfgFileHandle = uCreateFile(cfgFilePath.c_str(), 0, U_READ_WRITE, 0, def_sa, __sys_call_error);

    if (cfgFileHandle == U_INVALID_FD) {
        throw USER_EXCEPTION2(SE4040, "Can not create database configuration file");
    }

    uReleaseSA(def_sa, __sys_call_error);

    int res = uWriteFile(cfgFileHandle, options.str().c_str(), options.str().size(), &nbytes_written, __sys_call_error);

    if (res == 0 || nbytes_written != options.str().size()) {
        throw USER_EXCEPTION2( SE4045, cfgFilePath.c_str() );
    }

    if (uCloseFile(cfgFileHandle, __sys_call_error) == 0) {
        throw USER_EXCEPTION2(SE4043, "Can not close configuration file");
    }
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
            client->respondServiceError(cause);
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

            elog(EL_LOG, ("Request for database creation satisfied"));
        }
    };
};

CreateDatabaseRequestProcessor::CreateDatabaseRequestProcessor(WorkerSocketClient* producer)
  : WorkerSocketClient(producer, se_Client_Priority_Cdb), activeCallback(NULL), state(cdb_awaiting_db_options) {  }

  
SocketClient* CreateDatabaseRequestProcessor::processData()
{
    ProcessManager * pm = worker->getProcessManager();
    
    switch (state) {
       case cdb_awaiting_db_options:
          if (communicator->getInstruction() != se_CreateDatabaseRequest) {
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

          pm->createDatabase(dbName, communicator->readString(MAX_XML_PARAMS), new CreateDatabaseCallback(this));

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

/*
    U_ASSERT(false);
    switch (state) {
        case trn_initial_state: {
            if (NULL == trnInfo) {
                pm->processRegistrationFailed(ticket, "Invalid ticket");
                respondError();
                return NULL;
            };
        
            XMLBuilder serializedOptions;
                    
            pm->getDatabaseOptions(trnInfo->database->databaseName)->saveToXml(&serializedOptions);
            
            communicator->beginSend(se_TrnRegisterOK);
            communicator->writeInt32(trnInfo->sessionId);
            communicator->writeString(serializedOptions.str());
            communicator->endSend();

            state = trn_registered;
        }
        case trn_registered: {
            WORKER_READ_MESSAGE_SAFE
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
*/

/*      case trn_shutdown: {
 *          WORKER_READ_MESSAGE_SAFE
 *          if (se_int_SuccessefulStop != communicator->getInstruction())
 *             elog(EL_ERROR, ("TRN from database %s has failed during shutdown", trnInfo->database->databaseName.c_str()));
            } else {
              elog(EL_INFO, ("Database %s has been shut down successfully", dbInfo->database->databaseName.c_str()));
            }
            pm->removeSessionProcess(trnInfo->sid);
            setObsolete();
            return NULL;
 */


    return this;
}

void SessionConnectionProcessor::cleanupOnError()
{

}

void SessionConnectionProcessor::shutdown()
{
    communicator->beginSend(se_Stop);
    communicator->endSend();
    state = trn_shutdown;
}


