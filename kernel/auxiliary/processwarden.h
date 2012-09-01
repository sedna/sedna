#ifndef  _PROCESSWARDEN_H_
#define  _PROCESSWARDEN_H_

#include "common/sedna.h"
#include "common/base.h"
#include "u/uprocess.h"

class ExecuteProcess
{
#define MAX_CMD_LINE (U_MAX_PATH * 3)
    char commandLine[MAX_CMD_LINE  + 1];
    bool processStarted;
public:
    UPID pid;
    UPHANDLE processHandle;

    ExecuteProcess(const char * basePath, const char * executable, const char * args)
      : processStarted(false)
    {
        if (basePath == NULL) {
            basePath = base_path;
        };

        if (args == NULL) {
            snprintf(commandLine, MAX_CMD_LINE, "%s"U_PATH_DELIMITER"%s", basePath, executable);
        } else {
            snprintf(commandLine, MAX_CMD_LINE, "%s"U_PATH_DELIMITER"%s %s", basePath, executable, args);
        };
    };

    ~ExecuteProcess()
    {
        release();
    };

    void execute(UFlag flags, bool inheritHandles)
    {
        if (0 != uCreateProcess(commandLine, inheritHandles, NULL,
            flags, &processHandle, NULL,
                &pid, NULL, NULL, __sys_call_error))
        {
            throw USER_ENV_EXCEPTION("Cannot create process", false);
        }

        processStarted = true;
    };

    int wait4()
    {
        if (!processStarted) {
            throw USER_ENV_EXCEPTION("Process not started", false);
        };

        int statusCode = 0;

        if (0 != uWaitForChildProcess(pid, processHandle, &statusCode, __sys_call_error)) {
            throw USER_ENV_EXCEPTION("Cannot obtain process result", false);
        }

        return statusCode;
    };

    const char * command() { return commandLine; };
    
    void release()
    {
        if (processStarted) {
            uCloseProcessHandle(processHandle, __sys_call_error);
            processStarted = false;
        }
    };
};

#endif /* _PROCESSWARDEN_H_ */

