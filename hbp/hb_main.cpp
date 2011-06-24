/*
 * File:  hb_main.cpp - Main hot-backup procedure
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "hb_main.h"
#include "hb_files.h"
#include "hb_aux.h"

#include "common/base.h"
#include "common/sp.h"
#include "common/u/u.h"
#include "common/u/uprocess.h"
#include "common/u/usocket.h"
#include "common/u/uutils.h"
#include "common/sm_vmm_data.h"

#include <string>

/*
 *  The main plan (all communications with sm are through governor via socket protocol):
 *  1. Connect to gov and request hot-backup (since we have single-threaded sm, we must do repeated request if sm is busy in case of checkpoint.)
 *     When we have been acknowledged, checkpoints are disabled and sm is switched to hot-backup mode.
 *  2. Copy data file to backup dir
 *  3. Request logical log archiving from sm (this also switches sm to "normal" mode)
 *  4. Copy log files chain using provided file names.
 *  5. Copy all other stuff (vmm.dat, ph-file, cfg-file). This filenames are provided by governor.
 *  6. Notify sm of ending hot-backup procedure. Checkpoints become enabled at this point.
 */

#define HB_REQ_WAIT_TIME 2 // seconds, wait time before another request

// this function sends message to sm and receives response
static void hbSendMsgAndRcvResponse(USOCKET hbSock, msg_struct *msg)
{
    std::string err_msg;
    uint32_t len;

    if (sp_send_msg(hbSock, msg) != 0)
    {
                uclose_socket(hbSock, __sys_call_error);
        throw USER_EXCEPTION(SE3006);
    }

        if (sp_recv_msg(hbSock, msg) != 0 || msg->instruction == HB_ERR)
    {
                uclose_socket(hbSock, __sys_call_error);

        if (msg->instruction == HB_ERR && msg->length > 5 && msg->body[0] == 0)
        {
                net_int2int(&len, &(msg->body[1]));
                err_msg = std::string(&(msg->body[5]), len);

                throw USER_EXCEPTION2(SE4903, err_msg.c_str());
            }
            else
                throw USER_EXCEPTION(SE3007);
    }
}

// this function retrieves file name from socket message (stored as a string)
static int hbRetrieveFileName(msg_struct *msg, char *file_name)
{
        if (msg->length <= 5 || msg->body[0] != 0)  return -1;

        uint32_t len;

        net_int2int(&len, &(msg->body[1]));

        if (len == 0) return -1;

        strncpy(file_name, &(msg->body[5]), len);

        file_name[len] = '\0';

        return 0;
}

// this function performs main hot-backup actions (see plan above)
//
// Parameters:
//     hb_dir_name - name of the distance directory
//     hb_db_name  - name if the database to archive
//     port        - port number to connect to gov
void hbMainProcedure(char *hb_dir_name, char *hb_db_name, char address[], int port, int is_checkp)
{
    USOCKET hbSocket;
    int nodelay = 1;
    char host[U_MAX_HOSTNAME];
    struct msg_struct msg;
    hb_state hb_incr_req;

    char file_name[U_MAX_PATH + 1];

    hbSocket = usocket(AF_INET, SOCK_STREAM, 0, NULL);

    if (hbSocket == U_INVALID_SOCKET)
        throw USER_EXCEPTION(SE3001);

    if (usetsockopt(hbSocket, IPPROTO_TCP, TCP_NODELAY, (char *)&nodelay, sizeof(int), NULL) == U_SOCKET_ERROR)
        throw USER_EXCEPTION(SE3027);

    strcpy(host, address);

    if (uconnect_tcp(hbSocket, port, host, NULL) != 0)
        throw USER_EXCEPTION(SE3003);

    // sending hot-backup start (only to gov, to start hot-backup socket there)
    printf("Connecting to database...");

    msg.instruction = HOTBACKUP_START;
    msg.length = 0;
    if (sp_send_msg(hbSocket, &msg) != 0)
    {
                uclose_socket(hbSocket, __sys_call_error);
        throw USER_EXCEPTION(SE3006);
    }
        printf("Done.\n");

    // determine increment request mode
        if (!strncmp(hb_incr_mode, "start", 512))
        hb_incr_req = HB_START_INCR;
        else if (!strncmp(hb_incr_mode, "add", 512))
        hb_incr_req = HB_ADD_INCR;
        else if (!strncmp(hb_incr_mode, "stop", 512))
        hb_incr_req = HB_STOP_INCR;
    else
        hb_incr_req = HB_NONE_INCR;

    // sending hot-backup request
    msg.instruction = HB_START;
    msg.length = 1 + 4 + 5 + (sp_int32)strlen(hb_db_name); // is_chekpoint + increment_mode + db_name

    // options: checkpoint and increment
    msg.body[0] = (is_checkp) ? 1 : 0;
    int2net_int(hb_incr_req, &(msg.body[1]));

    // db_name
    msg.body[5] = 0;
        int2net_int((int32_t)strlen(hb_db_name), &(msg.body[6]));
        strncpy(&(msg.body[10]), hb_db_name, strlen(hb_db_name));

        hbSendMsgAndRcvResponse(hbSocket, &msg);

        U_ASSERT(msg.instruction == HB_CONT || msg.instruction == HB_WAIT);

        if (msg.instruction == HB_WAIT) printf("Waiting for checkpoint to finish...");

        while (msg.instruction != HB_CONT)
    {
        uSleep(HB_REQ_WAIT_TIME, __sys_call_error);

        msg.instruction = HB_START;
            msg.length = 5;

            // continue to send increment option (ignore checkpoint)
        int2net_int(hb_incr_req, &(msg.body[1]));

                hbSendMsgAndRcvResponse(hbSocket, &msg);

                if (msg.instruction == HB_CONT) printf("Done.\n");
    }

        U_ASSERT(msg.instruction == HB_CONT);

    // nothing to do: just send confirmation
    if (hb_incr_req == HB_STOP_INCR)
        goto end_mode;

    printf("Creating necessary directories...");
    fflush(stdout);
    // prepare distance directory (make hot-backup directory with current timestamp)
    if (hbPrepareDistance(hb_dir_name, hb_db_name) == -1)
    {
                ushutdown_close_socket(hbSocket, __sys_call_error);
        throw USER_EXCEPTION2(SE4510, "Failed to create some of the backup directories");
    }
        printf("Done.\n");

    // another portion of log files must be archived
    if (hb_incr_req == HB_ADD_INCR)
        goto arch_mode;

    // retrieve file data name from message
    if (hbRetrieveFileName(&msg, file_name) == -1)
    {
                ushutdown_close_socket(hbSocket, __sys_call_error);
        throw USER_EXCEPTION2(SE4510, "Error in processing incoming mesage");
    }

    // we can copy data file now
    printf("Copying data file...%s...", file_name);
    fflush(stdout);

    if (hbCopyFile(file_name) == -1)
    {
                ushutdown_close_socket(hbSocket, __sys_call_error);
        throw USER_EXCEPTION2(SE4510, "Error in copying data file");
    }
        printf("Done.\n");

arch_mode:
        // next step: request archive log; receive all files need to backup
    msg.instruction = HB_ARCHIVELOG;
    msg.length = 5;
        int2net_int(hb_incr_req, &(msg.body[1])); // send increment mode to signalize that we need only log ifles

        hbSendMsgAndRcvResponse(hbSocket, &msg);

        U_ASSERT(msg.instruction == HB_CONT || msg.instruction == HB_END);

        while (msg.instruction == HB_CONT && msg.length != 0)
        {
            // retrieve file name from message
            if (hbRetrieveFileName(&msg, file_name) == -1)
            {
                        ushutdown_close_socket(hbSocket, __sys_call_error);
                throw USER_EXCEPTION2(SE4510, "Error in processing incoming mesage");
            }

        // copy file
        printf("Copying additional files...%s...", file_name);
        fflush(stdout);

        if (hbCopyFile(file_name) == -1)
            {
                        ushutdown_close_socket(hbSocket, __sys_call_error);
                throw USER_EXCEPTION2(SE4510, "Error in copying one of the additional database files");
            }
                printf("Done.\n");

            msg.instruction = HB_NEXTFILE;
        msg.length = 0;

                hbSendMsgAndRcvResponse(hbSocket, &msg);
    }

end_mode:
        // next step: notify gov of end
    msg.instruction = HB_END;
    msg.length = 0;

        hbSendMsgAndRcvResponse(hbSocket, &msg);

        U_ASSERT(msg.instruction == HB_END);

        ushutdown_close_socket(hbSocket, __sys_call_error);
}
