#include "gov/cpool.h"
#include "gov/clients.h"
#include "gov/processes.h"
#include "common/structures/listener_states.h"
#include "common/errdbg/d_printf.h"
#include "common/u/uutils.h"
#include "common/db_utils.h"
#include "common/ipc_ops.h"
#include "common/ugc.h"
#include "common/structures/cdb_structures.h"
#include "common/SSMMsg.h"
#include "common/sm_vmm_data.h"

#include <iostream> // Temporary!!!


// #include <arpa/inet.h>

#include <set>
using namespace std;

/////////////////////////////class ClientNegotiationManager//////////////////////////////////

SocketClient * ClientNegotiationManager::processData() {
    if (!communicator->receive()) {
        return this;
    }
    
    // Message received, parsing it
    
    // !TODO: replace with switch-case
    
    if (se_StartUp == communicator->getInstruction()) {
        ClientConnectionProcessor * client = new ClientConnectionProcessor(this);
        communicator = NULL;
        return client;

    } else if (se_RegisterDB == communicator->getInstruction()) {
        SMConnectionProcessor * sm = new SMConnectionProcessor(this);
        sm->registerSM();
        communicator = NULL;
        return sm;

    } else if (se_RegisterNewSession == communicator->getInstruction()) {
        TRNConnectionProcessor * trn = new TRNConnectionProcessor(this);
        trn->registerTRN();
        communicator = NULL;
        return trn;
        
    } else if (se_ReceiveSocket == communicator->getInstruction()) { //this can happen only under *nix
        int s_id = communicator->readInt32();
        TRNInfo * info;
        info = worker->findTRNbyId(s_id);
        info->setAvailaible();
        info->unix_s = this->getSocket();
        worker->resumePendingOnTrnClient(string(info->db_name), (TRNConnectionProcessor *) info->processor);
        info = NULL;
        return this;
        
    } else if (se_CreateDbRequest == communicator->getInstruction()) {
        CdbRequestProcessor * cdbrequest = new CdbRequestProcessor(this);
        communicator->beginSend(se_SendSessionParameters);
        communicator->endSend();
        communicator = NULL;
        return cdbrequest;
      
    } else if (se_RegisterCDB == communicator->getInstruction()) {
        CdbConnectionProcessor * cdb = new CdbConnectionProcessor(this);
        cdb->registerCdb();
        communicator = NULL;
        return cdb;
    } else if (se_Stop == communicator->getInstruction()) {
        
//       !TODO: Here should be auth for shutdown
        SednaStopProcessor * se_stop = new SednaStopProcessor(this);
        se_stop->getParams();
        communicator = NULL;
        return se_stop;
         
    } else if (se_StopSM == communicator->getInstruction()) {
//       !TODO: Here should be auth for shutdown
        SMStopProcessor * se_smsd = new SMStopProcessor(this);
        se_smsd->getParams();
        communicator = NULL;
        return se_smsd;
      
    } else{
//         char buf[4];
//         u_itoa(communicator->getInstruction(),buf,10);
        respondError();
//         throw USER_EXCEPTION2(SE3009, buf);
        setObsolete();
        return NULL;
    }

    return this;
};

/////////////////////////////class SednaStopProcessor//////////////////////////////////

SednaStopProcessor::SednaStopProcessor(WorkerSocketClient* producer)
  : WorkerSocketClient(producer, se_Client_Priority_Stop) {}

void SednaStopProcessor::getParams()
{
// !TODO: need to make real auth check
  string login;
  string password;
  
  communicator->readString(login, SE_MAX_LOGIN_LENGTH);
  communicator->readString(password, SE_MAX_PASSWORD_LENGTH);
  elog(EL_LOG, ("Request for Sedna shutdown issued"));
  
  worker->addShutdownClient(this);
  
  if (!worker->getShutdown()) {
    worker->setShutdown();
    worker->stopAllDatabases();
  }
}


SocketClient* SednaStopProcessor::processData() {
  communicator->beginSend(se_Stop);
  communicator->endSend();
  setObsolete();
  return NULL;
}


/////////////////////////////class SMStopProcessor/////////////////////////////////////

SMStopProcessor::SMStopProcessor(WorkerSocketClient* producer)
  : WorkerSocketClient(producer, se_Client_Priority_SMsd) { }

void SMStopProcessor::getParams()
{
// !TODO: need to make real auth check
  string login;
  string password;
  string db_name;
  
  communicator->readString(login, SE_MAX_LOGIN_LENGTH);
  communicator->readString(password, SE_MAX_PASSWORD_LENGTH);
  communicator->readString(db_name, SE_MAX_DB_NAME_LENGTH);
  elog(EL_LOG, ("Request for SM shutdown issued"));
  
  if (0 != worker->stopSM(db_name)) 
  {
    respondError(); //already stopped or not running
  }
  worker->addPendingOnSMSmsd(db_name, this);
  
}


SocketClient* SMStopProcessor::processData() {
  communicator->beginSend(se_UnRegisterDB);
  communicator->endSend();
  setObsolete();
  return NULL;
}


/////////////////////////////class CdbConnectionProcessor//////////////////////////////////

CdbConnectionProcessor::CdbConnectionProcessor(WorkerSocketClient* producer)
  : InternalSocketClient(producer, se_Client_Priority_Cdb) { }

void CdbConnectionProcessor::registerCdb()
{
  CdbParameters * cdb_params;
  communicator->readString(db_name, SE_MAX_DB_NAME_LENGTH);

  //find cdb request parameters
  cdb_params = worker->findCdbByDbName(db_name);

  //send cdb params
  communicator->beginSend(se_CdbRegisteringOK);
  cdb_params->sendCdbParams(communicator.get());
  communicator->endSend();
  
  return;
}

SocketClient* CdbConnectionProcessor::processData()
{
  if (!communicator->receive()) return this;
  if (se_RegisterDB == communicator->getInstruction()) {
    SMConnectionProcessor * sm = new SMConnectionProcessor(this); 
    sm->registerSM();
    communicator = NULL;
    return sm;
  } else {
    worker->removeCdb(db_name);
    setObsolete();
    return NULL; //it's an error/
  }
}

void CdbConnectionProcessor::cleanupOnError()
{
    /*!TODO write this*/
}



/////////////////////////////class SMConnectionProcessor//////////////////////////////////

SMConnectionProcessor::SMConnectionProcessor(WorkerSocketClient* producer)
  : InternalSocketClient(producer, se_Client_Priority_SM)
{

}


/////////////////////////////Adding SM to running SM list. ///////////////////////////////
void SMConnectionProcessor::registerSM()
{
    UPID sm_pid;
    uint32_t db_id;
    string db_name;
    bool cdb_mode;
    int res, res2;

    communicator->readString(db_name, SE_MAX_DB_NAME_LENGTH);
    sm_pid = communicator->readInt32();
    db_id = communicator->readInt32();
    
    cdb_mode = (1 == communicator->readChar());
//    d_printf3("Listener: register SM pid: %d, name: %s", sm_pid, db_name);
    /* Registering as SMInfo */
    info = new SMInfo (db_name, sm_pid, db_id, cdb_mode);
    res = worker->addSM(info);

    switch (res) {
      case 0:
        communicator->beginSend(se_SMRegisteringOK);
        if (U_SOCKET_ERROR == communicator->endSend()) throw SYSTEM_EXCEPTION(string(usocket_error_translator()).c_str());
        
        if (cdb_mode) {
          worker->resumePendingOnCdbClients(db_name);
          char buf[1024];
          SSMMsg * sm_server = new SSMMsg(SSMMsg::Client,
          sizeof (sm_msg_struct),
          SEDNA_SSMMSG_SM_ID(db_id, buf, 1024), 
          SM_NUMBER_OF_SERVER_THREADS);

          if (sm_server->init() != 0)
              throw SYSTEM_EXCEPTION("Failed to initialize SSMMsg service (message service)");

          sm_msg_struct msg;
          msg.cmd = 10;

          if (sm_server->send_msg(&msg) != 0)
              throw SYSTEM_EXCEPTION("Can't send message via SSMMsg");

          if (sm_server->shutdown() != 0)
              throw SYSTEM_EXCEPTION("Failed to shutdown SSMMsg service (message service)");

          delete sm_server;
          sm_server = NULL;

        } else {
          worker->resumePendingOnSMClients(db_name);
        }

        break;
      
      case -2:                                  //-2, -3, -4 -- errors
      case -3:
      case -4:
      default:
        communicator->beginSend(se_SMRegisteringFailed);
        if (cdb_mode) {
          worker->removeCdb(db_name);
        } else {
          worker->removeSM(db_name);
        }
        if (U_SOCKET_ERROR == communicator->endSend()) throw SYSTEM_EXCEPTION(string(usocket_error_translator()).c_str());
        setObsolete();
        break;
    }

    return;
}

SocketClient* SMConnectionProcessor::processData()
{
  if (!communicator->receive()) return this;
  if (se_UnRegisterDB == communicator->getInstruction()) {
    worker->resumePendingOnSMSmsd(info->dbname);
    worker->removeSM(info->dbname);
    communicator->beginSend(se_UnRegisterDB);
    communicator->endSend();
  }
  setObsolete();
  return NULL;
}

void SMConnectionProcessor::cleanupOnError()
{
/*!TODO write this*/
}

/////////////////////////////class TRNConnectionProcessor//////////////////////////////////

TRNConnectionProcessor::TRNConnectionProcessor(WorkerSocketClient* producer)
  : InternalSocketClient(producer, se_Client_Priority_TRN), s_id(0), trninfo(NULL) { }

//////////////////////////////// Adding TRN to available TRNs list. //////////////////////
void TRNConnectionProcessor::registerTRN()
{
    msg_struct reg_msg;
    UPID sess_pid;
    char db_name[SE_MAX_DB_NAME_LENGTH+1];
    int res; 
    bool first_transaction;
    
    
    communicator->readString(db_name, SE_MAX_DB_NAME_LENGTH+1);
    sess_pid = communicator->readInt32();
    first_transaction = (1 == communicator->readChar());
    
//    d_printf3("Listener: register TRN pid: %d, name: %s", sess_pid, db_name);
    
    string db_name_str = string(db_name);
    res = worker->addTRN(new TRNInfo (db_name_str, communicator->getCommunicationSock(), sess_pid, first_transaction, (void *) this), &s_id);
    
    switch (res) {
      case trnSessionRegistered:
        /* Session registered successfully, we send session id back to the TRN */
        communicator->beginSend(se_TrnRegisterOK);
        communicator->writeInt32(s_id);  //FIXME: after changing all of the session_id usages, it would be needed to be changed to writeint64. Maybe.
        socket_check(communicator->endSend());

#ifdef EL_DEBUG
        elog(EL_LOG, ("Trn process registered on gov successfully"));              
#endif 
        
        setAvailable();
        
#ifdef _WIN32
        worker->resumePendingOnTrnClient(db_name_str, this);
#endif
        break;
      
      case trnFirstTransaction:
        communicator->beginSend(se_TrnRegisterOKFirstTransaction);
        communicator->writeInt32(s_id);  //FIXME: after changing all of the session_id usages, it would be needed to be changed to writeint64. Maybe.
        socket_check(communicator->endSend());
        break;
        
      case trnNotReady:
        /* Database is not running or operates in special mode */
        communicator->beginSend(se_TrnRegisterFailedNotRunningOrSpecialMode);
        socket_check(communicator->endSend());
//         worker->resumePendingOnTrnClient(db_name_str, NULL);
        break;
        
      case trnNoMoreSessionSlots:
        /* Currently there are maximum number of session in the system */
        communicator->beginSend(se_TrnRegisterFailedMaxSessLimit);
        socket_check(communicator->endSend());
//         worker->resumePendingOnTrnClient(db_name_str, NULL);
        break;

      default:
        throw SYSTEM_EXCEPTION("Governor failed while registering a new session");
    }

    return;
}

void TRNConnectionProcessor::setAvailable()
{
  trninfo = worker->findTRNbyId(s_id);
#ifndef _WIN32  
//   worker->createUnixListener(info, communicator);
#endif
  trninfo->state = trninfo_availaible;
  
}


SocketClient* TRNConnectionProcessor::processData()
{
    if (!communicator->receive()) {
        return this;
    }
    if (communicator->getState() == exch_got_full_message) {
      
      if (se_AuthenticationOK == communicator->getInstruction()) {
        return this;
      }
      
      if (se_UnRegisterSession == communicator->getInstruction()) {
        string tmp_db_name;
        session_id tmp_sess_id;
        
        communicator->readString(tmp_db_name, SE_MAX_DB_NAME_LENGTH);
        tmp_sess_id = communicator->readInt32();
        worker->removeTRN(tmp_db_name, tmp_sess_id);
        
        communicator->beginSend(se_UnRegisterSession);
        communicator->endSend();
      }
    return NULL;
    } else cleanupOnError();
}

void TRNConnectionProcessor::cleanupOnError()
{
/*!TODO write this*/
}


////////////////////////////class CdbRequestProcessor//////////////////////////////////////////////
/////////////////////////// It's only the first part, all cdb-connected stuff is in cdb_utils.cpp ///

CdbRequestProcessor::CdbRequestProcessor(WorkerSocketClient* producer)
  : WorkerSocketClient(producer, se_Client_Priority_Cdb), cdbParams(NULL), state(cdb_awaiting_parameters)
{
    cdbParams = new CdbParameters();
    elog(EL_LOG, ("Request for database creation"));
}


// void CdbConnectionProcessor::authAndPrepare()
// {
  /*
    if (communicator->getInstruction() != se_SessionParameters) {
      communicator->beginSend(se_ErrorResponse);
      communicator->endSend();
      aIsObsolete = true;
      return NULL;
    }
    if (0 != sessionParams.readSessParams(communicator)) {
      communicator->beginSend(se_ErrorResponse);
      communicator->endSend();
      aIsObsolete = true;
      return NULL;
    }
    sessionParams.readAuthParams();
    */

// }

SocketClient* CdbRequestProcessor::processData()
{
    switch (state) {
      case cdb_awaiting_parameters:
          if (!communicator->receive()) {
              return this;
          }
          
          if (communicator->getInstruction() != se_SessionParameters) {
              respondError();
              elog(EL_LOG, ("Database creation aborted: unexpected message; username and dbname were expected"));
              return NULL;
          }

//           if (0 != sessionParams.readSessParams(communicator)) {
//               respondError();
//               elog(EL_LOG, ("Database creation aborted: user is not allowed to create databases"));
//               return NULL;
//           }

          sessionParams.readSessParams(communicator.get());

          communicator->beginSend(se_SendAuthParameters);
          communicator->endSend();

          state = cdb_awaiting_auth;
          break;
          
       case cdb_awaiting_auth:
          if (!communicator->receive()) return this;
          if (communicator->getInstruction() != se_AuthenticationParameters) {
              respondError();
              elog(EL_LOG, ("Database creation aborted: unexpected message, password was expected"));
              return NULL;
          }
//           if (0 != sessionParams.readAuthParams(communicator)) {
//             // TODO : auth failed check -- now it returns 0 always
//               respondError();
//               elog(EL_LOG, ("Database creation aborted: user has provided wrong password"));              
//               return NULL;
//           }
          
          sessionParams.readAuthParams(communicator.get());
          
          communicator->beginSend(se_AuthenticationOK);
          communicator->endSend();
          state = cdb_awaiting_db_options;
          break;
          
       case cdb_awaiting_db_options:
          if (!communicator->receive()) return this;
          if (communicator->getInstruction() != se_CreateDbParams) {
              respondError();
              elog(EL_LOG, ("Database creation aborted: unexpected message, database parameters were expected"));
              return NULL;
          }
//           if (0 != cdbParams->readCdbParams(communicator)) {
//               respondError();
//               return NULL;
//           }
          strcpy(cdbParams->db_name, sessionParams.dbName);
          
          check_db_name_validness(cdbParams->db_name);
          if (exist_db(cdbParams->db_name)) {
              respondError();
              elog(EL_LOG, ("Database creation failed: database with the same name already exists"));
              return NULL;
          }

#ifdef REQUIRE_ROOT
          if (!uIsAdmin(__sys_call_error)) {
              respondError();
              elog(EL_LOG, ("Database creation failed: this action requires root privilegies"));
              return NULL;
          }
#endif

          cdbParams->db_id = get_next_free_db_id(worker->cfg);
          if (cdbParams->db_id == -1) {
              respondError();
              elog(EL_LOG, ("Database creation failed: the maximum number of databases hosted by one server is exceeded"));
              return NULL;
          }

//           createDb(); //it's implementation is in cdb_utils.cpp

          worker->addPendingOnCdbClient(sessionParams.dbName, this);
          worker->addCdb(cdbParams);

          worker->startCdb(cdbParams);

          state = cdb_awaiting_sm_start;
          break;
    
       case cdb_awaiting_sm_start:
          setObsolete();
          elog(EL_LOG, ("Request for database creation satisfied"));
          return NULL;
    }
    return this;
}


////////////////////////////class ClientConnectionProcessor//////////////////////////////////////////

ClientConnectionProcessor::~ClientConnectionProcessor() 
{
    setObsolete();
};

ClientConnectionProcessor::ClientConnectionProcessor(WorkerSocketClient* producer)
  : WorkerSocketClient(producer, se_Client_Priority_Client),
                            state(client_awaiting_parameters), 
                            sminfo(NULL), 
                            trninfo(NULL), 
                            trn_launch(false), 
                            is_socket_transmitted(false) {
// TODO : remove this from constructor
    //here we handle the first message.
    communicator->beginSend(se_SendSessionParameters);
    communicator->endSend();
};

void ClientConnectionProcessor::setTrn(TRNConnectionProcessor * trn) 
{
    trnProcessor = trn;
} 

SocketClient * ClientConnectionProcessor::processData() {
        /*
         * states:
         * 1) wait for sm register/params
         * 2) wait for trn register/params
         * 3) pass client connection socket to trn and close it
        */
     switch (state) {
      case client_awaiting_parameters:
          
          if (!communicator->receive()) return this;
          
          if (communicator->getInstruction() != se_SessionParameters) {
            respondError();
            return NULL;
          }

//           if (0 != sessionParams.readSessParams(communicator)) {
//             respondError();
//             return NULL;
//           }

          sessionParams.readSessParams(communicator.get());
          
          if (!worker->findAvailaibleSM(sminfo, sessionParams.dbName)) {
            if (0 == worker->startStorageManager(sessionParams.dbName)) {
              worker->addPendingOnSmClient(sessionParams.dbName, this);
            } else {
              respondError();
              return NULL;
            }
            
          } else if (!worker->findAvailaibleTRN(trninfo, sessionParams.dbName)) {
              worker->addPendingOnTrnClient(sessionParams.dbName, this);
              
              if (!trn_launch) { //!TODO there should be a check for total number of started trn processes.
                worker->startTransactionProcess(&sessionParams);
                trn_launch = true;
              }
          }
           
          state = client_awaiting_sm_and_trn;
          break;

      case client_awaiting_sm_and_trn:
          if (!worker->findAvailaibleSM(sminfo, sessionParams.dbName)) {
            return this; //!TODO this is an error!
          } else if (!worker->findAvailaibleTRN(trninfo, sessionParams.dbName)) {
            worker->addPendingOnTrnClient(sessionParams.dbName, this);
            
            if (!trn_launch) { //!TODO there should be a check for total number of started trn processes.
              worker->startTransactionProcess(&sessionParams);
              trn_launch = true;
              state = client_awaiting_trn_launch;
            }
            
            break;
            
          } else if (trninfo->isError()) {
            worker->removeTRN(trninfo->db_name, trninfo->sess_id);
            respondError();
            setObsolete();
            return NULL;
          }
          
          //do not insert "break here", i didn't forget about it.
          
      case client_awaiting_trn_launch:
          if (!is_socket_transmitted) {
            trnProcessor->communicator->sendSocket(trnProcessor->trninfo->pid, trnProcessor->trninfo->unix_s, clientSocket);
            is_socket_transmitted = true;

            trnProcessor->trninfo->setBusy();
            
            communicator->beginSend(se_SendAuthParameters);
            communicator->endSend();
            
            state = client_awaiting_auth;
          }

          break;
          
      case client_awaiting_auth:
          
          if (!communicator->receive()) return this;
          
          if (communicator->getInstruction() != se_AuthenticationParameters) {
            respondError();
            return NULL;
          }

//           if (0 != sessionParams.readAuthParams(communicator)) {
//             // TODO : auth failed check -- now it returns 0 always
//             respondError();
//             return NULL;
//           }
          
          sessionParams.readAuthParams(communicator.get());
        
          sessionParams.sendSessParams(trnProcessor->communicator.get());
          sessionParams.sendAuthParams(trnProcessor->communicator.get());
          
          setObsolete(false);
          return NULL;
          
          break;
    }

      
    return this;
}

