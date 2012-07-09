#include "clients.h"

#include "gov/cpool.h"
#include "common/structures/listener_states.h"

#include "common/protocol/int_sp.h"
#include "aux/internstr.h"
#include "aux/options/xml_options.h"

#include <set>

#define MAX_TICKET_SIZE 1024
#define MAX_ERROR_LENGTH 1024
#define MAX_DB_NAME 1024
#define MAX_XML_PARAMS 10240

using namespace std;

SocketClient* InternalProcessNegotiation::processData()
{
    switch(state) {
      case iproc_initial :
        if (!communicator->receive()) { return this; }
        communicator->beginSend(se_Handshake);
        // Here should be initial key someday
        communicator->endSend();
        state = iproc_awaiting_key;
      case iproc_awaiting_key :
        if (!communicator->receive()) { return this; }
        ticket = communicator->readString(MAX_TICKET_SIZE);
        // Here we recieve key, so we know type of client
        // Next processor
      default : break;
    };

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
      case se_StartUp :
        length = communicator->getMessageLength();

        // Distincts new protocol from the old one
        // New protocol sends protocol version immediately
        if (length == 2) {
            // New protocol

            protocolVersion.min = communicator->readChar();
            protocolVersion.maj = communicator->readChar();

            return new ServiceConnectionProcessor(this, protocolVersion);
        } else {
            return new ClientConnectionProcessor(this, ClientConnectionProcessor::CommonProtocolClient());
        };

        break;
      case se_ConnectProcess :
        return new InternalProcessNegotiation(this);
      case se_ReceiveSocket :
        U_ASSERT(false);
        //worker->getProcessManager()->getSessionById();
        //
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
            client->processSendSocket();
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
            if (!communicator->receive()) { return this; }

            if (!(se_RegisterCDB == communicator->getInstruction())) {
                communicator->beginSend(se_CdbRegisteringFailed);
                communicator->endSend();
                setObsolete();
                return NULL;
            }

            dbName = communicator->readString(MAX_DB_NAME); // TODO: FIXME!

            DatabaseOptions * options = pm->getDatabaseOptions(dbName);

            if (options == NULL) {
                respondError();
                return NULL;
            };

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
            options->saveToXml(&serializedOptions);
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
            client->state = cdb_awaiting_sm_start; // >TODO FIXME
        }
    };
    
    virtual void onSuccess(CallbackMessage* cbm)
    {
        U_ASSERT(false);
        // TODO
    };
};

CreateDatabaseRequestProcessor::CreateDatabaseRequestProcessor(WorkerSocketClient* producer)
  : WorkerSocketClient(producer, se_Client_Priority_Cdb), activeCallback(NULL), state(cdb_awaiting_db_options) {  }

SocketClient* CreateDatabaseRequestProcessor::processData()
{
    ProcessManager * pm = worker->getProcessManager();
    string dbName;
    
    switch (state) {
       case cdb_awaiting_db_options:
          if (!communicator->receive()) return this;
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

          break;
       case cdb_awaiting_sm_start:
          communicator->beginSend(se_CreateDbOK);
          communicator->endSend();
          elog(EL_LOG, ("Request for database creation satisfied"));
          
          return new ServiceConnectionProcessor(this, true);
    }
    return this;
}

SocketClient* SednaShutdownProcessor::processData()
{
    U_ASSERT(false);
    return NULL;
}





