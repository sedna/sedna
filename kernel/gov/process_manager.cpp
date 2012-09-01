#include "gov/process_manager.h"

#include "common/procutils/cmdlines.h"
#include "common/structures/config_data.h"
#include "common/errdbg/exceptions.h"

#include "u/uprocess.h"
#include "u/uhdd.h"

#include "auxiliary/processwarden.h"

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

    std::string::size_type suffixPosition;

    for (int i = 0; i < SEDNA_MAX_DBS_NUMBER; i++) {
        availaibleDatabaseIds.push(i);
    }

    dataDirectory = uFindFirstFile(SEDNA_DATA, &currentCfgFile, __sys_call_error);

    if (U_INVALID_DIR == dataDirectory) {
        throw SYSTEM_EXCEPTION("Data directory not found");
    }

    int res = -1;

    do {
        if ((suffixPosition = std::string(currentCfgFile.fname).find(".conf.xml") ) != std::string::npos)
        {
            std::string dbName = std::string(currentCfgFile.fname).substr(0, suffixPosition);
            std::ifstream fs(currentCfgFile.fname);

            if (fs.failbit) {
                throw USER_EXCEPTION2(SE4042, currentCfgFile.fname);
            }

            parameters.loadDatabaseFromStream(dbName, &fs);
            fs.close();
        }

        res = uFindNextFile(dataDirectory, &currentCfgFile, __sys_call_error);

        if (-1 == res) {
            throw USER_EXCEPTION2(SE4083, SEDNA_DATA);
        }
    } while (0 != res);
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
    std::istringstream stream(xmlOptions);
    parameters.loadDatabaseFromStream(dbName, &stream);
};

#define CMD_LINE_BUFFER_LEN 1024

void ProcessManager::execStorageManagerProcess(const std::string& ticket, DatabaseProcessInfo* databaseProcessInfo) {
    std::ostringstream parameters;

    parameters << "localhost " << this->parameters.global.listenPort << " " << ticket.c_str();
    ExecuteProcess smExecutor(NULL, SM_EXE, parameters.str().c_str());

    smExecutor.execute(0, true);

    databaseProcessInfo->pHandle = smExecutor.processHandle;
    databaseProcessInfo->pid = smExecutor.pid;
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
    size_t ticket_len = 64;
    char * s = (char *) malloc(ticket_len+1);
    static const char alphanum[] =
        "0123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz";

    for (size_t i = 0; i < ticket_len; ++i) {
        s[i] = alphanum[rand() % (sizeof(alphanum) - 1)];
    }

    s[ticket_len] = 0;
    ticket.append(s, ticket_len);
    delete s;
}

void ProcessManager::startDatabase(const std::string& dbName, const std::string& options, IProcessCallback* callback)
{
    if (availaibleDatabaseIds.empty()) {
        throw std::runtime_error("Maximum number of databases reached.");
    };

    std::string ticket;
    this->generateTicket(ticket);

    GlobalParameters::DatabaseOptionMap::iterator it = parameters.databaseOptions.find(dbName);

    if (it == parameters.databaseOptions.end()) {
        throw std::runtime_error("Database not exists");
    }

    setDatabaseOptions(dbName, options);

    it->second.databaseId = availaibleDatabaseIds.top();
    availaibleDatabaseIds.pop();

    DatabaseProcessInfo * databaseProcessInfo = new DatabaseProcessInfo(it->second);
    databaseProcessInfo->databaseCreationMode = false;
    databaseProcessInfo->clientCallbackSet.insert(callback);

    try {
        execStorageManagerProcess(ticket, databaseProcessInfo);
    } catch(std::exception & x) {
        delete databaseProcessInfo;
        callbackError(callback, x.what());
    };
    
    processMap.insert(ProcessMap::value_type(ticket, databaseProcessInfo));
}

/*TODO this function should be executed in separate thread */
void ProcessManager::createDatabase(const std::string& dbName, const std::string& options, IProcessCallback* callback)
{
    DatabaseProcessInfo * databaseProcessInfo = NULL;
    try {
        if (availaibleDatabaseIds.empty()) {
            throw std::runtime_error("Maximum number of databases reached.");
        };

        std::string ticket;
        generateTicket(ticket);

        GlobalParameters::DatabaseOptionMap::iterator it = parameters.databaseOptions.find(dbName);

        if (it != parameters.databaseOptions.end()) {
            throw std::runtime_error("Database already exists");
        }

        it = parameters.databaseOptions.insert(
            GlobalParameters::DatabaseOptionMap::value_type(dbName, parameters.defaultDatabaseParameters)).first;

        setDatabaseOptions(dbName, options);

        it->second.databaseId = availaibleDatabaseIds.top();
        availaibleDatabaseIds.pop();

        it->second.databaseName = dbName;
        it->second.dataFilePath = parameters.global.dataDirectory + dbName + U_PATH_DELIMITER;

        databaseProcessInfo = new DatabaseProcessInfo(it->second);
        databaseProcessInfo->databaseCreationMode = true;
        databaseProcessInfo->clientCallbackSet.insert(callback);

        execStorageManagerProcess(ticket, databaseProcessInfo);

        processMap.insert(ProcessMap::value_type(ticket, databaseProcessInfo));
    } catch(std::exception & x) {
        delete databaseProcessInfo;
        callbackError(callback, x.what());
        throw;
    };
};


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
        requestProcessQueue.pop();

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

    for (; j != i->second->clientCallbackSet.end(); j++) {
        callbackError(*j, reason.c_str());
    }

    i->second->clientCallbackSet.clear();

    if (DatabaseProcessInfo * dbInfo = dynamic_cast<DatabaseProcessInfo *> (i->second)) {
        parameters.databaseOptions.erase(dbInfo->databaseName);
        availaibleDatabaseIds.push(dbInfo->databaseId);
    };

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
    
    for (; j != i->second->clientCallbackSet.end(); j++) {
        callbackSuccess(*j, i->second, processor);
    }
    
    i->second->clientCallbackSet.clear();
}

