#include "gov/process_manager.h"
#include "common/procutils/cmdlines.h"
#include "common/structures/config_data.h"
#include "common/errdbg/exceptions.h"
#include "u/uprocess.h"
#include "u/uhdd.h"

#include <stdio.h>
#include <sstream>
#include <fstream>

#ifndef _WIN32
#define SMImageName "se_sm "
#define TRNImageName "se_trn "
#endif /* _WIN32 */

ProcessManager::ProcessManager(GlobalParameters& _parameters)
               : parameters(_parameters), lastSessionId(0), requestsPending(false)
{
    UDir dataDirectory;
    UFindDataStruct currentCfgFile;
    int res = -1;
    std::string::size_type suffixPosition;
    std::fstream fs;
    std::string cfgText;
    cfgText.reserve(10240);
    
    for (int i = 0; i < MAX_DBS_NUMBER; i++) {
        availaibleDatabaseIds.push(i);
    }
    dataDirectory = uFindFirstFile(_parameters.global.dataDirectory.c_str(), 
                                        &currentCfgFile,
                                        __sys_call_error
                                       );
    if (U_INVALID_DIR == dataDirectory) {
        throw SYSTEM_EXCEPTION("Something's wrong; data directory disappered suddenly");
    }
    
    for (int i = 0 ; 1 ;)
    {
        if (i >= MAX_DBS_NUMBER) {
            throw USER_EXCEPTION2(SE4412, _parameters.global.dataDirectory.c_str());
        }
        
        if ( (suffixPosition = std::string(currentCfgFile.fname).find("_cfg.xml") ) != std::string::npos)
        {
            std::string dbName = std::string(currentCfgFile.fname).substr(0,suffixPosition);
            fs.open ((_parameters.global.dataDirectory + 
                        std::string("/") + 
                        dbName + 
                        std::string("_cfg.xml")).c_str(), std::ios::out);
            if (fs.failbit) {
                throw USER_EXCEPTION2(SE4042, 
                                      (_parameters.global.dataDirectory + 
                                       std::string("/") + 
                                       dbName + 
                                       std::string("_cfg.xml")).c_str()
                                      );
            }
            
            cfgText.clear();
            
            fs >> cfgText;
            fs.close();
            
            setDatabaseOptions(dbName, cfgText);
            ++i;
        }
        res = uFindNextFile(dataDirectory, 
                            &currentCfgFile,
                            __sys_call_error
                           );
        if (0 == res) {
            break; //all cfg files are already read
        }
        if (-1 == res) {
            throw USER_EXCEPTION2(SE4083, _parameters.global.dataDirectory.c_str());
        }
    }
};


DatabaseOptions* ProcessManager::getDatabaseOptions(const std::string& dbName)
{
    GlobalParameters::DatabaseOptionMap::iterator i = parameters.databaseOptions.find(dbName);

    if (i == parameters.databaseOptions.end()) {
        return NULL;
    } else {
        return &(i->second);
    }
};

void ProcessManager::setDatabaseOptions(const std::string& dbName, const std::string& xmlOptions)
{
    std::stringstream stream(xmlOptions, std::ios_base::out);
    parameters.loadDatabaseFromStream(dbName, &stream);
    parameters.databaseOptions.at(dbName).databaseId = availaibleDatabaseIds.top();
    availaibleDatabaseIds.pop();
};

#define CMD_LINE_BUFFER_LEN 1024

void ProcessManager::execStorageManagerProcess(const std::string& ticket, 
                                               DatabaseProcessInfo* databaseProcessInfo) {
    char command_line_buffer[CMD_LINE_BUFFER_LEN];

    databaseProcessInfo->locked = false;
    snprintf(command_line_buffer, CMD_LINE_BUFFER_LEN, "%s localhost %d %s", 
             SMImageName,
             this->parameters.global.listenPort,
             ticket.c_str());

    if (uCreateProcess(command_line_buffer, false, NULL, 0, &(databaseProcessInfo->pHandle), NULL, 
                      &(databaseProcessInfo->pid), NULL, NULL, __sys_call_error) != 0) {
        
        /* TODO:
        throw EProccessExecutionFailed();
        */
        elog(EL_LOG,("SM process start failed"));
        throw SYSTEM_EXCEPTION("Can't start SM process");
    }
};

void ProcessManager::execTransactionProcess(const std::string& ticket, 
                                            SessionProcessInfo* sessionProcessInfo) {
    char command_line_buffer[CMD_LINE_BUFFER_LEN];

    sessionProcessInfo->locked = false;
    snprintf(command_line_buffer, CMD_LINE_BUFFER_LEN, "%s localhost %d %d %s", 
             TRNImageName,
             this->parameters.global.listenPort,
             sessionProcessInfo->database->databaseId,
             ticket.c_str());

    if (uCreateProcess(command_line_buffer, false, NULL, 0, &(sessionProcessInfo->pHandle), NULL, 
                      &(sessionProcessInfo->pid), NULL, NULL, __sys_call_error) != 0) {
        
        /* TODO:
        throw EProccessExecutionFailed();
        */
        elog(EL_LOG,("TRN process start failed"));
        throw SYSTEM_EXCEPTION("Can't start SM process");
    }
};


ProcessManager::~ProcessManager()
{
//
}

void ProcessManager::generateTicket(ClientTicket& ticket)
{
    size_t ticket_len = 128;
    char * s = (char *) malloc(ticket_len+1);
    static const char alphanum[] =
        "0123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz";

    for (int i = 0; i < ticket_len; ++i) {
        s[i] = alphanum[rand() % (sizeof(alphanum) - 1)];
    }

    s[ticket_len] = 0;
    ticket.append(s, ticket_len);
    delete s;
}

void ProcessManager::startDatabase(const std::string& dbName, IProcessCallback* callback)
{
    std::string ticket;
    this->generateTicket(ticket);
    
    GlobalParameters::DatabaseOptionMap::iterator it = parameters.databaseOptions.find(dbName);
    // TODO : check for existence and throw exceptions if not exists
    DatabaseOptions * databaseOptions = &it->second;

    // TODO : make universal process start framework
    DatabaseProcessInfo * databaseProcessInfo = new DatabaseProcessInfo();
    databaseProcessInfo->databaseCreationMode = false;
    databaseProcessInfo->databaseName = dbName;
    databaseProcessInfo->clientCallbackSet.insert(callback);

    try {
        execStorageManagerProcess(ticket, databaseProcessInfo);
    } catch(std::exception & x) {
        delete databaseProcessInfo;
        callbackError(callback, x.what());
    };
    
    processMap.insert(ProcessMap::value_type(ticket, databaseProcessInfo));
}

void ProcessManager::requestSession(DatabaseProcessInfo* sm, IProcessCallback* callback)
{
    std::string ticket;
    generateTicket(ticket);
    // TODO : make universal process start framework
    SessionProcessInfo * sessionProcessInfo = new SessionProcessInfo();
    sessionProcessInfo->state = trninfo_not_started;
    sessionProcessInfo->database = sm;
    sessionProcessInfo->clientCallbackSet.insert(callback);
    
    try {
        execTransactionProcess(ticket, sessionProcessInfo);
    } catch(std::exception & x) {
        delete sessionProcessInfo;
        callbackError(callback, x.what());
    };
    
    processMap.insert(ProcessMap::value_type(ticket, sessionProcessInfo));
}

/*TODO this function should be executed in separate thread */
void ProcessManager::createDatabase(const std::string& dbName, IProcessCallback* callback)
{
    std::string ticket;
  
    GlobalParameters::DatabaseOptionMap::iterator i = parameters.databaseOptions.find(dbName);

    if (i == parameters.databaseOptions.end()) {
        U_ASSERT(false);
    }

    DatabaseProcessInfo * databaseProcessInfo = new DatabaseProcessInfo();
    databaseProcessInfo->databaseCreationMode = true;
    databaseProcessInfo->databaseName = dbName;

    databaseProcessInfo->clientCallbackSet.insert(callback);

    // CRITICAL SECTION START TODO
    
    try {
        execStorageManagerProcess(ticket, databaseProcessInfo);
    } catch(std::exception & x) {
        delete databaseProcessInfo;
        callbackError(callback, x.what());
    };
    
    processMap.insert(ProcessMap::value_type(ticket, databaseProcessInfo));
    
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
    CallbackMessage msg = {cb, CallbackMessage::Error, NULL, NULL, messageInfo};
    requestProcessQueue.push(msg);
    requestsPending = true;
}


void ProcessManager::doProcessRequests()
{
    CallbackMessage msg;
    
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


ProcessInfo * ProcessManager::getUnregisteredProcess(const std::string& ticket)
{
    ProcessMap::iterator i = processMap.find(ticket);
    if (i == processMap.end()) {
        return NULL;
    } else if (i->second->locked) {
        return NULL;
    } else {
        i->second->locked = true;
        return i->second;
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

