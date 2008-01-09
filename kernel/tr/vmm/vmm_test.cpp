/*
 * File:  vmm_test.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include <iostream>
#include <exception>
#include <string>
#include <vector>

#include "common/sedna.h"

#include "common/base.h"
#include "common/SSMMsg.h"
#include "tr/vmm/vmm.h"
#include "tr/tr_globals.h"


using namespace std;


transaction_id trid = 0;
char db_name[100];

int main(int argc, char ** argv) 
{
    strcpy(db_name, "xmark");
    d_printf1("VMM_TEST STARTED\n");

    //getc(stdin);

    char buf[1024];
    int ret_code = 0;
    sm_msg_struct msg;

    try {

        vmm_preliminary_call();
        OS_EXCEPTIONS_INSTALL_HANDLER


        //init global names
        set_global_names();
        set_global_names(db_name);


        SSMMsg sm_server(SSMMsg::Client, 
                         sizeof (sm_msg_struct), 
                         CHARISMA_SSMMSG_SM_ID(db_name, buf, 1024), 
                         SM_NUMBER_OF_SERVER_THREADS, 
                         U_INFINITE);


        d_printf1("Connecting to SM...");
        if (sm_server.init() != 0)
            throw USER_EXCEPTION(1);
        d_printf1("OK\n");


        d_printf1("Initializing VMM...");
        vmm_init(&sm_server);
        d_printf1("OK\n");


///////////////////////////////////////////////////////////////////////////////
/*
        d_printf1("basicTest...");
        msg.cmd = 23; // sm_allocate_data_block
        msg.trid = trid;

        if (sm_server.send_msg(&msg) != 0)
            throw USER_EXCEPTION(1);

        if (msg.cmd != 0) 
            throw USER_EXCEPTION(1);
        xptr p = *(xptr*)(&(msg.ptr));

        msg.cmd = 25; // sm_delete_block
        msg.trid = trid;

        if (sm_server.send_msg(&msg) != 0)
            throw USER_EXCEPTION(1);

        if (msg.cmd != 0) 
            throw USER_EXCEPTION(1);
*/

        vector<xptr> blks;
        int i = 0;

        int num = 10000;
        int proc = 0;
        for (i = 0; i < num; i++)
        {
            xptr p;
            vmm_alloc_data_block(&p);
            VMM_SIGNAL_MODIFICATION(p);
            blks.push_back(p);
            if (i * 100 / num > proc)
            {
                d_printf2("%d%\n", proc);
                proc++;
            }
        }

/*
        for (vector<xptr>::iterator it = blks.begin(); it != blks.end(); it++)
        {
            vmm_delete_block(*it);
        }
*/
        d_printf1("OK\n");

///////////////////////////////////////////////////////////////////////////////

        d_printf1("Releasing VMM...");
        vmm_release();
        d_printf1("OK\n");

        d_printf1("Releasing SM...");
        if (sm_server.shutdown() != 0)
            throw USER_EXCEPTION(1);
        d_printf1("OK\n");


    } catch (SednaException &e) {
        cout << "Sedna exception" << endl;
        cout << e.getMsg() << endl;
    } 
  
    return ret_code;
}

