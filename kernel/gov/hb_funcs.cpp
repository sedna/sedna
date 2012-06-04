/*
 * File:  hb_funcs.cpp
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>

#include "common/sedna.h"

#include "gov/hb_funcs.h"
#include "gov/gov_globals.h"
#include "gov/hb_files.h"

#include "common/base.h"
#include "common/ssmmsg/SSMMsg.h"
#include "common/protocol/sp.h"

#include "u/uutils.h"

#include "common/llcommon/lfsGlobals.h"


#define MAX_SE_SOCKET_STR (SE_SOCKET_MSG_BUF_SIZE - 5)

using namespace std;

static string *hbDbName;    // name of the db to archive
static hb_state *status; // HB_START->HB_WAIT (sm wait case)->HB_ARCHIVELOG->HB_NEXTFILE->HB_END
static vector <string> *hbFiles;

struct hbInfo
{
	hb_state status;
	string hbDbName;
	vector <string> hbFiles;
};

static map <USOCKET, hbInfo> hbClients;

// makes socket protocol msg with error to send to hbp
static void hbMakeErrorMsg(msg_struct *msg, const char *err_msg)
{
	msg->instruction = HB_ERR;
	msg->length = 5 + (sp_int32)strlen(err_msg);

	msg->body[0] = 0;
	int2net_int((int32_t)strlen(err_msg), &(msg->body[1]));

	strncpy(&(msg->body[5]), err_msg, strlen(err_msg));
}

static void hbSendMsgToSm(sm_msg_struct *msg)
{
	SSMMsg *sm_server = NULL;
	char buf[1024];

	sm_server = new SSMMsg(SSMMsg::Client,
                           sizeof(sm_msg_struct),
                           CHARISMA_SSMMSG_SM_ID(get_db_id_by_name(gov_table->get_config_struct(), hbDbName->c_str()), buf, 1024),
                           SM_NUMBER_OF_SERVER_THREADS);

	if (sm_server->init() != 0)
	    throw SYSTEM_EXCEPTION("Failed to initialize SSMMsg service (message service)");

	if (sm_server->send_msg(msg) != 0)
	    throw SYSTEM_EXCEPTION("Can't send message via SSMMsg");

	if (sm_server->shutdown() != 0)
	    throw SYSTEM_EXCEPTION("Failed to shutdown SSMMsg service (message service)");

	delete sm_server;
}

// retrieves db name, check its existence and if sm is running
static int hbRetrieveAndCheckDbName(USOCKET sock, char *dbname, int len)
{
	string db_str = string(dbname, len);

	map <USOCKET, hbInfo>::iterator it;

	for (it = hbClients.begin(); it != hbClients.end(); it++)
		if (it->second.hbDbName == db_str)
			return -1;

	if (!(gov_table->is_database_run(db_str.c_str()))) return -1;

	it = hbClients.find(sock);

   	it->second.hbDbName = db_str;

	return 0;
}

// retrieves log file names, config file name etc. and stores them in hbFiles array
static int RetrieveAllFileNames(bool only_log)
{
    char buf[MAX_SE_SOCKET_STR];
    int len;

    // retrieve db config file
	if ((len = hbMakeConfFileName(buf, MAX_SE_SOCKET_STR, hbDbName->c_str())) == -1)
		return -1;

    hbFiles->push_back(string(buf, len));

	// retrieve all log file names
	sm_msg_struct msg;

	msg.cmd = 39;
	msg.data.hb_struct.state = HB_ARCHIVELOG;

	hbSendMsgToSm(&msg);

	if (msg.data.hb_struct.state == HB_ERR)	return -1;

	while (msg.data.hb_struct.lnumber != LFS_INVALID_FILE)
	{
		if ((len = hbMakeLogFileName(buf, MAX_SE_SOCKET_STR, hbDbName->c_str(), msg.data.hb_struct.lnumber)) == -1)
			return -1;

	    hbFiles->push_back(string(buf, len));

		msg.cmd = 39;
		msg.data.hb_struct.state = HB_GETPREVLOG;

		hbSendMsgToSm(&msg);

		if (msg.data.hb_struct.state == HB_ERR) return -1;
	}

    return 0;
}

// process hbp specific error (connection lost or error in hbp)
// do: send HB_ERROR to sm to end hot-backup process, shutdown and close corresponding socket
static void hbProcessErrorHbp(USOCKET sock)
{
	if (*status != HB_START) // no need to notify sm since hot-back isn't in progress
	{
		sm_msg_struct msg;

		msg.cmd = 39;
		msg.data.hb_struct.state = HB_ERR;

		hbSendMsgToSm(&msg);
	}

	hbClients.erase(sock);

	uclose_socket(sock, __sys_call_error);
}

// processes start message
// can issue wait request or send data file name to hbp
int hbProcessStartRequest(USOCKET sock, msg_struct *msg)
{
	hb_state req = (hb_state)msg->instruction, incr_req;

	if (*status != HB_WAIT && *status != HB_START)
	{
	    hbMakeErrorMsg(msg,
	    	"Hot-backup protocol violation (expecting start request)");

		return -1;
	}

	if (*status == HB_START)
	{
		if (msg->length < 11 || msg->body[5] != 0) return -1;

		uint32_t len;
		net_int2int(&len, &(msg->body[6]));

		if (hbRetrieveAndCheckDbName(sock, &(msg->body[10]), len) != 0)
		{
		    hbMakeErrorMsg(msg,
		    	"Hot-backup start error (duplicate request or db isn't running)");
			return -1;
		}
	}

	sm_msg_struct smmsg;

	smmsg.cmd = 39;
	smmsg.data.hb_struct.state = req;
	smmsg.data.hb_struct.is_checkp = (*status == HB_WAIT) ? false : (msg->body[0] != 0);

	net_int2int((uint32_t *)(&incr_req), &(msg->body[1]));
	smmsg.data.hb_struct.incr_state = incr_req;

	hbSendMsgToSm(&smmsg);

	if (smmsg.data.hb_struct.state == HB_CONT) // need to send data file name
	{
		if (incr_req == HB_STOP_INCR)
			*status = HB_END;
		else
        	*status = HB_ARCHIVELOG;

		msg->instruction = HB_CONT;

		msg->body[0] = 0;

		int len = hbMakeDataFileName(&(msg->body[5]), MAX_SE_SOCKET_STR, hbDbName->c_str());

		if (len == -1) // path doesn't fit into message buffer and that's strange :)
		{
			msg->instruction = HB_ERR;
			msg->length = 0;

			return -1;
		}

		int2net_int(len, &(msg->body[1]));
		msg->length = len + 5;
	}
	else
	{
		msg->instruction = *status = smmsg.data.hb_struct.state;
		msg->length = 0;

		if (*status == HB_ERR)
		{
		    hbMakeErrorMsg(msg,
		    	"Start request error (base in recovery mode or increment requested without primary copy)");
			return -1;
		}
	}

	return 0;
}

// processes archive log request
int hbProcessLogArchRequest(msg_struct *msg)
{
	hb_state incr_req; // increment type

	if (*status != HB_ARCHIVELOG)
	{
	    hbMakeErrorMsg(msg,
	    	"Hot-backup protocol violation (expecting log archive request)");

		return -1;
	}

	net_int2int((uint32_t *)(&incr_req), &(msg->body[1]));

	int res = RetrieveAllFileNames(incr_req == HB_ADD_INCR);

	if (res == 0 && hbFiles->size() > 0) // need to send file name
	{
		*status = HB_NEXTFILE;
		msg->instruction = HB_CONT;
        msg->body[0] = 0;
		strncpy(&(msg->body[5]), (*hbFiles)[0].c_str(), (*hbFiles)[0].length());
		int2net_int((int32_t)((*hbFiles)[0].length()), &(msg->body[1]));

		msg->length = (sp_int32)((*hbFiles)[0].length()) + 5;

		hbFiles->erase(hbFiles->begin());
	}
	else if (res != 0)
	{
	    hbMakeErrorMsg(msg,
	    	"Cannot determine files to copy (possible sm error)");
		return -1;
	}
    else
    {
		*status = HB_END;
		msg->instruction = HB_END;
		msg->length = 0;
	}

	return 0;
}

// processes next copy file request
static int hbProcessNextFile(msg_struct *msg)
{
	if (*status != HB_NEXTFILE)
	{
	    hbMakeErrorMsg(msg,
	    	"Hot-backup protocol violation (expecting next file request)");

		return -1;
	}

	if (hbFiles->size() > 0)
	{
		msg->instruction = HB_CONT;
        msg->body[0] = 0;
		strncpy(&(msg->body[5]), (*hbFiles)[0].c_str(), (*hbFiles)[0].length());
		int2net_int((int32_t)((*hbFiles)[0].length()), &(msg->body[1]));

		msg->length = (sp_int32)((*hbFiles)[0].length()) + 5;

		hbFiles->erase(hbFiles->begin());
	}
	else
	{
		*status = HB_END;
		msg->instruction = HB_CONT;
        msg->length = 0;
	}

	return 0;
}

// processes end request from hbp
int hbProcessEndRequest(msg_struct *msg)
{
	if (*status != HB_END)
	{
	    hbMakeErrorMsg(msg,
	    	"Hot-backup protocol violation (expecting end request)");

		return -1;
	}

	*status = HB_START;

	sm_msg_struct smsg;

	smsg.cmd = 39;
	smsg.data.hb_struct.state = HB_END;

	hbSendMsgToSm(&smsg);

	if (smsg.data.hb_struct.state == HB_END)
	{
		msg->instruction = HB_END;
		msg->length = 0;
	}
	else
	{
	    hbMakeErrorMsg(msg,
	    	"Unknown error during end request (sm error)");

		return -1;
	}

	return 0;
}

// processes message from hbp
// return: 0 - all ok, cont.; -1 - error (socket already closed); 1 - connection closed
int hbProcessMessage(USOCKET sock)
{
    msg_struct msg;
    map <USOCKET, hbInfo>::iterator it;

    if (sp_recv_msg(sock, &msg) != 0 || msg.instruction == HB_ERR)
    {
   		hbProcessErrorHbp(sock);
    	return -1;
    }

	hb_state cmd = (hb_state)msg.instruction;
	int res;

   	it = hbClients.find(sock);

   	U_ASSERT(it != hbClients.end());

   	status = &((*it).second.status);
   	hbDbName = &((*it).second.hbDbName);
   	hbFiles = &((*it).second.hbFiles);

    if (cmd == HB_START)
		res = hbProcessStartRequest(sock, &msg);
	else if (cmd == HB_ARCHIVELOG)
     	res = hbProcessLogArchRequest(&msg);
    else if (cmd == HB_NEXTFILE)
     	res = hbProcessNextFile(&msg);
	else if (cmd == HB_END)
		res = hbProcessEndRequest(&msg);
	else
	{
	    hbMakeErrorMsg(&msg,
	    	"Hot-backup protocol violation (unknown request)");

		res = -1;
	}

	// unknown error
	if (res == -1 && msg.instruction != HB_ERR)
	{
		msg.instruction = HB_ERR;
		msg.length = 0;
	}

	if (sp_send_msg(sock, &msg) != 0)
       	res = -1;

	if (res != 0)
		hbProcessErrorHbp(sock);

	if (cmd == HB_END && res == 0)
	{
		hbClients.erase(sock);
		return 1;
	}

	return res;
}

int hbNewClient(USOCKET sock)
{
	hbInfo hi;
	hi.status = HB_START;

	hbClients.insert(pair<USOCKET, hbInfo>(sock, hi));

	return 0;
}
