#ifndef CPOOL_H
#define CPOOL_H

#include "common/sedna.h"
#include "common/base.h"

#include "u/uprocess.h"

#include "gov/process_structures.h"
#include "gov/process_manager.h"
#include "gov/clients.h"

#include "common/socketutils/socketutils.h"

#include <vector>
#include <map>

enum { 
  trnSessionRegistered = 0,
  trnFirstTransaction = 1,
  trnNotReady = -1,
  trnNoMoreSessionSlots = -2
};

class Worker {
private:
    U_SSET allSet, readySet;
    USOCKET maxfd;

    SocketClientList clientList;
    ProcessManager * processManager;

    WorkerSocketClient * ownListenerSocket;
    bool shutting_down;

    void processClientData(WorkerSocketClient * client);
public:
    ProcessManager * getProcessManager() { return processManager; };

    Worker(ProcessManager * _processManager)
      : maxfd(0), processManager(_processManager), shutting_down(false)
      { U_SSET_ZERO(&allSet); }

    WorkerSocketClient * createListener();
    void run();

    void createUnixListener(SessionProcessInfo * trninfo);
};


#endif /* CPOOL_H */
