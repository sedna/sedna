#ifndef _SMMSG_SERVER_H_
#define _SMMSG_SERVER_H_

#include "common/sedna.h"
#include "common/xptr/sm_vmm_data.h"
#include "common/ssmmsg/SSMMsg.h"

int sm_server_handler(void *arg);

struct SSMMsgServerWrapper
{
    SSMMsg * smMessageServer;
    bool started;

    SSMMsgServerWrapper() : started(false) {
        smMessageServer = new SSMMsg(SSMMsg::Server, sizeof (sm_msg_struct),
            smMessageServerName, SM_NUMBER_OF_SERVER_THREADS, U_INFINITE);
    }

    void start()
    {
        elog(EL_INFO, ("Starting SSMMsg..."));

        if (smMessageServer->init() != 0) {
            throw USER_EXCEPTION(SE3030);
        }

        if (smMessageServer->serve_clients(sm_server_handler) != 0)
            throw USER_EXCEPTION(SE3031);

        started = true;
    };


    void stop()
    {
        if (started) {
            smMessageServer->stop_serve_clients();
            smMessageServer->shutdown();

            started = false;
        }
    };

    ~SSMMsgServerWrapper()
    {
        stop();
    };
};

#endif /* _SMMSG_SERVER_H_ */
