#include "process_manager.h"
#include "common/procutils/cmdlines.h"

#include <sstream>

DatabaseOptions* ProcessManager::getDatabaseOptions(const std::string& dbName)
{
    DatabaseOptionMap::iterator i = parameters.databaseOptions.find(dbName);

    if (i == parameters.databaseOptions.end()) {
        return NULL;
    } else {
        return &(i->second());
    }
};

void ProcessManager::setDatabaseOptions(const std::string& dbName, const std::string& xmlOptions)
{
    std::stringstream stream(xmlOptions, std::ios_base::out);
    parameters.loadDatabaseFromStream(dbName, stream);
};

#define CMD_LINE_BUFFER_LEN

void execStorageManagerProcess(const std::string& ticket, DatabaseProcessInfo * databaseProcessInfo) 
{
    char command_line_buffer[CMD_LINE_BUFFER_LEN];

    databaseProcessInfo->locked = false;
    
    snprintf(command_line_buffer, CMD_LINE_BUFFER_LEN, "%s %s ", SMImageName, dbName.c_str());

    if (uCreateProcess(command_line_buffer, false, NULL, 0, databaseProcessInfo->pHandle, NULL, 
                      databaseProcessInfo->pid, NULL, NULL, __sys_call_error) != 0) {
        throw EProccessExecutionFailed();
    }

    return databaseProcessInfo;
};

/*TODO this function should be executed in separate thread */
void ProcessManager::createDatabase(const std::string& dbName, IProcessCallback* callback)
{
    std::string ticket;
  
    DatabaseOptionMap::iterator i = parameters.databaseOptions.find(dbName);

    if (i == parameters.databaseOptions.end()) {
        U_ASSERT(false);
    }

    DatabaseProcessInfo * databaseProcessInfo = new DatabaseProcessInfo();
    databaseProcessInfo->databaseCreationMode = true;
    databaseProcessInfo->databaseName = dbName;

    databaseProcessInfo->clientCallbackSet.insert(callback);

    // CRITICAL SECTION START TODO
    
    try {
        execStorageManagerProcess(ticket, dbName, true);
    } catch(std::exception & x) {
        delete databaseProcessInfo;
        callbackError(callback, x.what());
    };
    
    processMap.insert(ticket, databaseProcessInfo);
    
    // CRITICAL SECTION END
};

void ProcessManager::callbackSuccess(IProcessCallback* cb, ProcessInfo* pinfo, WorkerSocketClient* socketClient)
{
    CallbackMessage msg = {cb, CallbackMessage::Success, pinfo, socketClient};    
    requestProcessQueue.push(msg);
    requestsPending = true;
}

void ProcessManager::callbackError(IProcessCallback* cb, const char* messageInfo)
{
    CallbackMessage msg = {cb, CallbackMessage::Error, messageInfo};
    requestProcessQueue.push(msg);
    requestsPending = true;
}


void ProcessManager::doProcessRequests()
{
  CallbackMessage msg;
  if (requestsPending)  {
      while (!requestProcessQueue.empty()) {
          msg = requestProcessQueue.front();
          if (msg.result == CallbackMessage::Error) {
            msg.callback->onError(msg.messageInfo);
          } else {
            msg.callback->onSuccess(&msg);
          }
      }
      
      requestsPending = false;
  }
}


ProcessInfo * ProcessManager::getUnregisteredProcess(const std::string& ticket)
{
    ProcessMap::iterator i = processMap.find(ticket);
    if (i == processMap.end()) {
        return NULL;
    } else if (i->second->locked) {
        return NULL;
    } else {
        i->second->locked = true;
        return &(i->second());
    }
};


void ProcessManager::processRegistrationFailed(const std::string& ticket, const std::string& reason)
{
    ProcessMap::iterator i = processMap.find(ticket);
    if (i == processMap.end()) {
        U_ASSERT(false); //if this happens something is totally wrong
    }
    std::set<IProcessCallback *>::iterator j = i->second->clientCallbackSet.begin();
    
    for (j; j != i->second->clientCallbackSet.end(); j++) {
        callbackError(*j, reason.c_str());
    }
    
    i->second->clientCallbackSet.clear();
    delete i->second;
    
    processMap.erase(ticket);  
};

void ProcessManager::processRegistered(const std::string& ticket, WorkerSocketClient * processor)
{
    ProcessMap::iterator i = processMap.find(ticket);
    if (i == processMap.end()) {
        U_ASSERT(false); //if this happens something is totally wrong
    }
    
    i->second->locked = false;
    
    std::set<IProcessCallback *>::iterator j = i->second->clientCallbackSet.begin();
    
    for (j; j != i->second->clientCallbackSet.end(); j++) {
        callbackSuccess(*j, i->second, processor);
    }
    
    i->second->clientCallbackSet.clear();
}
