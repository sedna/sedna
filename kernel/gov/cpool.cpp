#include "gov/cpool.h"
#include "gov/clients.h"
#include "common/errdbg/d_printf.h"


using namespace std;

/////////////////////////////// WORKER implementation

WorkerSocketClient * Worker::addClient(WorkerSocketClient * stream) {
        USOCKET s = stream->getSocket();

        /* We add client to separate client list not to spoil the client list iterator */
        newClients.push_back(stream);
        
        U_SSET_SET(s, &allSet);

        if (maxfd < s) {
                maxfd = s;
        }

        return stream;
};

void Worker::deleteClient(WorkerSocketClient * stream) {
        USOCKET s = stream->getSocket();
        U_SSET_CLR(s, &allSet);
        stream->setObsolete();
}

int Worker::addCdb(CdbParameters* cdbinfo)
{
  creatingDbs.insert(CdbMap::value_type(string(cdbinfo->db_name), *cdbinfo));
  return 0;
}

int Worker::removeCdb(string dbname)
{
  CdbMap::iterator i = creatingDbs.find(dbname);
  if (creatingDbs.end() == i) {
    return -1;
  } else {
    creatingDbs.erase(i);
  }
  return 0;
}

CdbParameters* Worker::findCdbByDbName(string dbname)
{
  return &((creatingDbs.find(dbname))->second);
}

int Worker::addShutdownClient(WorkerSocketClient* client)
{
  clientsWaitingForShutdown.push_back(client);
  return 0;
}


int Worker::stopAllDatabases(void)
{
  for (SMMap::iterator i = runningSms.begin(); i != runningSms.end(); i++) {
      char buf[1024];
      SSMMsg * sm_server = new SSMMsg(SSMMsg::Client,
      sizeof (sm_msg_struct),
      SEDNA_SSMMSG_SM_ID(i->second.db_id, buf, 1024), 
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
      i->second.mode = OM_SM_SHUTDOWN;
  }
  return 0;
}

int Worker::stopSM(string dbname)
{
    SMMap::iterator i = runningSms.find(dbname);
    if (i == runningSms.end()) return -1;
    char buf[1024];
    SSMMsg * sm_server = new SSMMsg(SSMMsg::Client,
    sizeof (sm_msg_struct),
    SEDNA_SSMMSG_SM_ID(i->second.db_id, buf, 1024), 
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
    i->second.mode = OM_SM_SHUTDOWN;
    
    return 0;
}



int Worker::addSM(SMInfo * sminfo)
{
    UPHANDLE proc_handle;
    if (uOpenProcess(sminfo->pid, &proc_handle, __sys_call_error) != 0)
        return -4;

    if (runningSms.find(sminfo->dbname) != runningSms.end())
        return -3;
  
    if (MAX_DBS_NUMBER <= runningSms.size())
    {
        uCloseProcessHandle(proc_handle, __sys_call_error);
        return -2;
    }
    
    runningSms.insert(SMMap::value_type(sminfo->dbname, *sminfo));
//     int db_id = get_db_id_by_name(cfg, sminfo->dbname);
// !FIXME    cfg->db_vars[db_id].mode = SM_OM_WORKING; -- do not know if it is really needed.
    
    return 0;
}

int Worker::removeSM(string dbname)
{
  /*
  SMMap::iterator i = runningSms.find(dbname);
  if (runningSms.end() == i) {
    return -1;
  } else {
    runningSms.erase(i);
  }
  */
  runningSms.erase(dbname);
  //!TODO: mark shutdown in cfg too.
  return 0;
}


int Worker::addTRN (TRNInfo * trninfo, 
                           session_id * sess_id) {

    SMMap::iterator i;
    TrnMap::iterator j;
    CdbMap::iterator k;
    int res = trnSessionRegistered;
    
    
    if (trninfo->special_mode) {
      k = creatingDbs.find(trninfo->db_name);
      if (creatingDbs.end() == k) {
        res = trnNotReady;
      } else {
        res = trnFirstTransaction;
      }
    } else {
      i = runningSms.find(trninfo->db_name);
      if (runningSms.end() == i) {
//       Check if there is such database in list of runnisg SMs
//       uCloseProcessHandle(proc_handle, __sys_call_error);
        trninfo->setError();
        res = trnNotReady;
      }
      
      //do not allow to connect to sm if it's in cdb-mode
      if (i->second.special_mode) {
        trninfo->setError();
        res = trnNotReady;
      }
    }
    
    // Check that we don't exceed the limit of concurrently running sessions
    if (MAX_SESSIONS_NUMBER <= availaibleTrns.size()) {
        trninfo->setError();
        res = trnNoMoreSessionSlots;
    }
    
    // !FIXME !TODO change uint32_max to uint64_max after fixing session_id type usage in other parts of sedna
    if (lastRegisteredSessId < UINT32_MAX) trninfo->sess_id = *sess_id = ++lastRegisteredSessId;
    else trninfo->sess_id = *sess_id = lastRegisteredSessId = 0;
    
    if (res == 0) {
      trninfo->state = trninfo_registered;
      createUnixListener(trninfo);
    }
    
    j = availaibleTrns.find(trninfo->db_name);
    if (j == availaibleTrns.end()) {
      vector<TRNInfo> tmp;
      tmp.push_back(*trninfo);
      availaibleTrns.insert(TrnMap::value_type(trninfo->db_name, tmp));
    } else {
      j->second.push_back(*trninfo);
    }
    
    delete trninfo;
    
//     uCloseProcessHandle(proc_handle, __sys_call_error);
    
    return res;
}

int Worker::removeTRN(string dbname, session_id sess_id) {
  TrnMap::iterator i = availaibleTrns.find(dbname);
  if (availaibleTrns.end() == i) {
    return -1;
  } else {
    for (vector<TRNInfo>::iterator j = i->second.begin(); j != i->second.end(); j++) {
      
      if (j->sess_id == sess_id) i->second.erase(j);
      
      if (i->second.empty()) {
        availaibleTrns.erase(i);
        return 0;
      }
    }
  }
  return 0;
}

bool Worker::findAvailaibleSM(SMInfo* &sminfo, char * dbname)
{
    std::string dbname_str(dbname);
    SMMap::iterator i;
    i = runningSms.find(dbname_str);
    if (runningSms.end() == i) { 
      sminfo = NULL;
      return false;
    } else {
      sminfo = &i->second;
      return true;
    }
}

bool Worker::findAvailaibleTRN(TRNInfo* &trninfo, char * dbname)
{
    std::string dbname_str(dbname);
    TrnMap::iterator i;
    std::vector<TRNInfo>::iterator j;
    i = availaibleTrns.find(dbname_str);
    if (availaibleTrns.end() == i) { 
      trninfo = NULL;
      return false;
    } else {
      for (j = i->second.begin(); j != i->second.end(); j++) {
        if (j->state == trninfo_availaible) 
        {
          trninfo = &(*j);
          return true;
        }
      }
      trninfo = NULL;
      return false;
    }
}

TRNInfo* Worker::findTRNbyId(session_id s_id)
{
    TrnMap::iterator i;
    std::vector<TRNInfo>::iterator j;
    for (i = availaibleTrns.begin(); i != availaibleTrns.end(); i++) {
      for (j = i->second.begin(); j != i->second.end(); j++) {
        if (j->sess_id == s_id) 
        {
          return &(*j);
        }
      }
    }
    return NULL;
}

void Worker::addPendingOnCdbClient(char* dbname, WorkerSocketClient* client)
{
    ClientsPendingOnSmMap::iterator i = clientsPendingOnCdb.find(string(dbname));
    if (i == clientsPendingOnCdb.end()) {
//    it's the first pending client on that db
      vector<WorkerSocketClient *> tmp;
      tmp.push_back(client);
      clientsPendingOnCdb.insert(ClientsPendingOnSmMap::value_type(string(dbname), tmp));
    } else {
//    there are pending clients on that db
      i->second.push_back(client);
    }
}

void Worker::addPendingOnSmClient(char* dbname, WorkerSocketClient* client)
{
    ClientsPendingOnSmMap::iterator i = clientsPendingOnSM.find(string(dbname));
    if (i == clientsPendingOnSM.end()) {
//    it's the first pending client on that db
      vector<WorkerSocketClient *> tmp;
      tmp.push_back(client);
      clientsPendingOnSM.insert(ClientsPendingOnSmMap::value_type(string(dbname), tmp));
    } else {
//    there are pending clients on that db
      i->second.push_back(client);
    }
}

void Worker::addPendingOnTrnClient(char* dbname, ClientConnectionProcessor* client)
{

    ClientsPendingOnTrnMap::iterator i = clientsPendingOnTrn.find(string(dbname));
    if (i == clientsPendingOnTrn.end()) {
//    it's the first pending client on trn that belongs to this db
      vector<ClientConnectionProcessor *> tmp;
      tmp.push_back(client);
      clientsPendingOnTrn.insert(ClientsPendingOnTrnMap::value_type(string(dbname), tmp));
    } else {
//    there are pending clients on trn that belongs to this db
      i->second.push_back(client);
    }
}

void Worker::addPendingOnSMSmsd(string dbname, WorkerSocketClient* client)
{
    SmsdPendingOnSmMap::iterator i = smsdClients.find(dbname);
    if (i == smsdClients.end()) {
//    it's the first pending client on that db
      vector<WorkerSocketClient *> tmp;
      tmp.push_back(client);
      smsdClients.insert(SmsdPendingOnSmMap::value_type(dbname, tmp));
    } else {
//    there are pending clients on that db
      i->second.push_back(client);
    }
}

void Worker::resumePendingOnSMSmsd(string dbname)
{
    SmsdPendingOnSmMap::iterator i = smsdClients.find(dbname);
    if (i == smsdClients.end()) {
      return; //no clients wait for SM start
    } else {
      for (vector<WorkerSocketClient *>::iterator j = i->second.begin(); j != i->second.end(); j++) {
        (*j)->processData();
      }
      i->second.clear();
      smsdClients.erase(i);
      return;
    }
}



void Worker::resumePendingOnSMClients(string dbname)
{
    ClientsPendingOnSmMap::iterator i = clientsPendingOnSM.find(dbname);
    if (i == clientsPendingOnSM.end()) {
      return; //no clients wait for SM start
    } else {
      for (vector<WorkerSocketClient *>::iterator j = i->second.begin(); j != i->second.end(); j++) {
        (*j)->processData();
      }
      i->second.clear();
      clientsPendingOnSM.erase(i);
      return;
    }
}

void Worker::resumePendingOnCdbClients(string dbname)
{
    ClientsPendingOnSmMap::iterator i = clientsPendingOnCdb.find(dbname);
    if (i == clientsPendingOnCdb.end()) {
      return; //no clients wait for SM start
    } else {
      for (vector<WorkerSocketClient *>::iterator j = i->second.begin(); j != i->second.end(); j++) {
        (*j)->processData();
      }
      i->second.clear();
      clientsPendingOnCdb.erase(i);
      return;
    }
}

void Worker::resumePendingOnTrnClient(string dbname, TRNConnectionProcessor * trnProcessor)
{
    ClientsPendingOnTrnMap::iterator i = clientsPendingOnTrn.find(dbname);
    if (i == clientsPendingOnTrn.end()) {
      return; //no clients wait for SM start
    } else {
      ClientConnectionProcessor * j = i->second.back();
      i->second.pop_back();
      j->setTrn(trnProcessor);
      j->processData();
      if (i->second.empty()) {
        clientsPendingOnTrn.erase(i);
      }
      return;
    }
}



/////////////////////Launching: SM /////////////////////////////////
// !TODO: 
// 1) read settings from cfg for db
// 2) read db_vars (or to destroy it finally)
// 3) do not use SessionParameters -- we need something better
// 4) construct cmd -> run
// 5) make autostart option for db and check it right there -- if it's turned off we should respond client that he can not start db on demand

/* This is on demand version. It checks existance of db and cfg default parameters and calls the real launch function if launch on demand is allowed. */
int Worker::startStorageManager(char * dbname) {
    SMInfo *    sminfo;
    string      com_line;
    UPID        pid;
    UPHANDLE    phandle;
    char buf[U_MAX_PATH + SE_MAX_DB_NAME_LENGTH]; //!FIXME: need to change buf size, but i don't know what is the real max size of full command line
    
    if (!findAvailaibleSM(sminfo, dbname)) {
      if (!exist_db(dbname)) {
        return -1; //there is no such database
      }

/*!TODO: check the on_demand option here (when it would be written) */
      com_line = constructClForSM(cfg, NULL, dbname);

      memcpy(buf, com_line.c_str(), com_line.length() + 1);
      
      if (uCreateProcess(buf, false, NULL, 0, &phandle, NULL, &pid, NULL, NULL, __sys_call_error) != 0) {
            throw USER_EXCEPTION(SE4401);
      }
      return 0;
      
    } else {
      return -2; //database is already running
    }
}

/* This function lauches SM */
int Worker::startStorageManager(SMInfo * sminfo)
{
    UPID pid;
    UPHANDLE phandle;
    int res;
    char buf[U_MAX_PATH + SE_MAX_DB_NAME_LENGTH]; //!FIXME: need to change buf size, but i don't know what is the real max size of full command line
    SMInfo * tmpsminfo;
    if (!findAvailaibleSM(tmpsminfo, sminfo->dbname)) {
      if (!exist_db(sminfo->dbname)) {
        return -1; //there is no such database
      }

      string com_line = constructClForSM(cfg, sminfo, sminfo->dbname);
      
      memcpy(buf, com_line.c_str(), com_line.length() + 1);
      
      if (uSetEnvironmentVariable("SEDNA_SM_BACKGROUND_MODE", "1", NULL, __sys_call_error) != 0) {
            throw USER_EXCEPTION2(SE4072, "SEDNA_SM_BACKGROUND_MODE");
      }

      if (uCreateProcess(buf, false, NULL, 0, &phandle, NULL, &pid, NULL, NULL, __sys_call_error) != 0) {
            throw USER_EXCEPTION(SE4401);
      }
      return 0;
    } else {
      return -2; //database is already running
    }
}


/////////////////////Launching: TRN /////////////////////////////////
int Worker::startTransactionProcess(SessionParameters * sess_params) {
    UFlag window_mode;
    UPID pid;
    UPHANDLE proc_h;
    USECURITY_ATTRIBUTES *sa;   
    char buf[U_MAX_PATH + 10];
    
    /* Create security attributes for the new process */
    if(0 != uCreateSA(&sa, 
        U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 
        0,  /* new process will not inherit handle returned by CreateProcess */
        __sys_call_error)) 
        throw USER_EXCEPTION(SE3060);

    string con_path_str = constructClForTrn(&(cfg->gov_vars), sess_params->dbName, get_db_id_by_name(cfg, sess_params->dbName));
    strcpy(buf, con_path_str.c_str());
    
#if (defined(EL_DEBUG) && (EL_DEBUG == 1))
    d_printf1(buf);
#endif
    
/*    if (background_off_from_background_on)
        window_mode = U_DETACHED_PROCESS;  //process has no window for output
    else */
        window_mode = 0;                   //process is created without flags

    if (0 != uCreateProcess(buf,
        false, // do not inherit handles
        NULL,
        window_mode,
        &proc_h,
        NULL,
        &pid,
        NULL,
        sa, 
        __sys_call_error)) {
  #ifdef _WIN32                  
            d_printf2("create process failed %d\n", GetLastError());
  #else           
            d_printf1("create process failed\n");
  #endif
            throw USER_EXCEPTION2(SE4413, "Try to reconnect later");
    }

    /* Release security attributes */
    if(uReleaseSA(sa, __sys_call_error) !=0 ) 
        throw USER_EXCEPTION(SE3063);
    return 0;
}


SocketClient * ListenerSocket::processData() {
        USOCKET negotiation_socket_stream;
        negotiation_socket_stream = uaccept(clientSocket, __sys_call_error);

        WorkerSocketClient * newSocketStream = new ClientNegotiationManager(worker, negotiation_socket_stream);

        if(negotiation_socket_stream == U_INVALID_SOCKET) {
                throw SYSTEM_EXCEPTION("Can't accept client's connection");
        }

        socketSetNoDelay(negotiation_socket_stream);
        worker->addClient(newSocketStream);

        return this;
}



/* this function is needed for socket trespassing in *nix systems */
void Worker::createUnixListener(TRNInfo * trninfo) {
#ifndef _WIN32
        USOCKET listening_socket;
        char socket_unix_address[14];
        memset (socket_unix_address, 0, 14);
        memcpy (socket_unix_address, "sdn", strlen("sdn"));
        
        u_itoa(trninfo->sess_id, socket_unix_address + strlen("sdn"), 10);
        listening_socket = usocket(AF_UNIX, SOCK_STREAM, 0, __sys_call_error);
        if(listening_socket==U_INVALID_SOCKET)
                throw SYSTEM_EXCEPTION("Can't init unix socket");
       
        if(ubind_unix(listening_socket, socket_unix_address, __sys_call_error) == U_SOCKET_ERROR)
                throw SYSTEM_EXCEPTION("Can't bind socket");

        if(ulisten(listening_socket, 100, __sys_call_error) == U_SOCKET_ERROR)
                throw SYSTEM_EXCEPTION("Can't set socket to a listening mode ");

        WorkerSocketClient * UnixListenerSocket = new ListenerSocket(this, listening_socket);
        
        addClient(UnixListenerSocket);
#endif
        return;
}


WorkerSocketClient * Worker::createListener() {
        USOCKET listening_socket;

        listening_socket = usocket(AF_INET, SOCK_STREAM, 0, __sys_call_error);
        if(listening_socket==U_INVALID_SOCKET)
                throw SYSTEM_EXCEPTION("Can't init socket");

        if(ubind_tcp(listening_socket, cfg->gov_vars.lstnr_port_number, cfg->gov_vars.lstnr_addr, __sys_call_error) == U_SOCKET_ERROR)
                throw SYSTEM_EXCEPTION("Can't bind socket");

        if(ulisten(listening_socket, 100, __sys_call_error) == U_SOCKET_ERROR)
                throw SYSTEM_EXCEPTION("Can't set socket to a listening mode ");

        ownListenerSocket = new ListenerSocket(this, listening_socket);
        
        ///////// NOTIFY THAT SERVER IS READY //////////////////////////////////
        USemaphore started_sem;
        if (0 == USemaphoreOpen(&started_sem, SEDNA_GOVERNOR_IS_READY, __sys_call_error))
        {
            USemaphoreUp(started_sem, __sys_call_error);
            USemaphoreClose(started_sem, __sys_call_error);
        }
        ///////// NOTIFY THAT SERVER IS READY //////////////////////////////////
        
        set_session_common_environment();
        
        return addClient(ownListenerSocket);
}

void Worker::run() {
    for (;;) {
        memcpy(&readySet, &allSet, sizeof(readySet));

        int readyCount = uselect_read_arr(&readySet, maxfd, NULL, __sys_call_error);

        if (readyCount == U_SOCKET_ERROR) {
            U_ASSERT(false);
            throw USER_EXCEPTION2(SE3007, usocket_error_translator());
        }

        for (SocketClientList::iterator i = clientList.begin(); i != clientList.end(); ) {
            WorkerSocketClient * client = *i;

            if (client->isObsolete()) {
                USOCKET socket = client->getSocket();
                U_SSET_CLR(socket, &allSet);
                i = clientList.erase(i);
                delete client;
                continue;
            }

            USOCKET socket = client->getSocket();

            if (U_SSET_ISSET(socket, &readySet)) {
                try {
                    while (client != NULL) {
                        WorkerSocketClient * nextProcessor =
                            static_cast<WorkerSocketClient *>(client->processData());
                        readyCount--;

                        if (client != nextProcessor) {
                            delete client;
                            client = nextProcessor;
                            *i = client;
                        } else {
                            break;
                        };
                    }

                    if (client == NULL) {
                        U_SSET_CLR(socket, &allSet);
                        i = clientList.erase(i);
                        continue;
                    }
                } catch (SednaGovSocketException e) {
                    e.ref->cleanupOnError();
                }
            }
            ++i;

            /* If we already read all desciptors, break inner loop */
            if (readyCount == 0) {
                break;
            };
        }

        /* Add new clients to client list */
        if (!newClients.empty()) {
            clientList.insert(clientList.end(), newClients.begin(), newClients.end());
            newClients.clear();
//                sort(clientList.begin(), clientList.end(), WorkerSocketClientGreater());
        }

        if (runningSms.empty() && getShutdown()) {
            for (ClientList::iterator j = clientsWaitingForShutdown.begin(); j!= clientsWaitingForShutdown.end(); j++) {
                (*j)->processData();
            }
            clientsWaitingForShutdown.clear();
            break;
        }
    }

    delete ownListenerSocket;
}