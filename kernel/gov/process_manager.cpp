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


/*TODO this function should be executed in separate thread */
void ProcessManager::createDatabase(const std::string& dbName, IProcessCallback* callback)
{
  DatabaseOptionMap::iterator i = parameters.databaseOptions.find(dbName);
  if (i == parameters.databaseOptions.end()) {
    U_ASSERT(false);
  }

  std::string ticket; //TODO: = createTicket();
  std::string cmd = constructClForCdb(ticket);
  
  DatabaseProcessInfo * databaseProcessInfo = new DatabaseProcessInfo();
  databaseProcessInfo->databaseCreationMode = true;
  databaseProcessInfo->databaseName = dbName;
  databaseProcessInfo->locked = false;
  databaseProcessInfo->clientCallbackSet.insert(callback);
  
  if (uCreateProcess(cmd.c_str(), false, NULL, 0, databaseProcessInfo->pHandle, NULL, 
                     databaseProcessInfo->pid, NULL, NULL, __sys_call_error) != 0) {
    removeDatabaseProcess(dbName);
    callback->onError();
  }
  
  processMap.insert(ticket, databaseProcessInfo);
};


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
    j->onError();
  }
  
  processMap.erase(ticket);  
};

void ProcessManager::processRegistered(const std::string& ticket)
{
  ProcessMap::iterator i = processMap.find(ticket);
  if (i == processMap.end()) {
    U_ASSERT(false); //if this happens something is totally wrong
  }
  
  i->second->locked = false;
  
  std::set<IProcessCallback *>::iterator j = i->second->clientCallbackSet.begin();
  
  for (j; j != i->second->clientCallbackSet.end(); j++) {
    j->onError();
  }
}
