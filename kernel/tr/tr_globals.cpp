/*
 * File:  tr_globals.cpp
 * Copyright (C) 2004 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include <string>

#include "common/sedna.h"

#include "common/ssmmsg/SSMMsg.h"

#include "tr/tr_globals.h"
#include "tr/auth/auc.h"
#include "tr/crmutils/serialization.h"

#define TR_ARG_MAX_LENGTH       511

using namespace tr_globals;

namespace tr_globals
{
    char e_string_buf[PAGE_SIZE];

    int run_rewriter      = 1;
    int show_time         = 0;
    int print_intermed    = 0;
    int debug_mode        = 0;
    int authentication    = 1;
    int authorization     = 1;
    
    DatabaseOptions databaseOptions;
    SednaOptions sednaOptions;
    
    char filename    [TR_ARG_MAX_LENGTH+1];
    char password    [SE_MAX_PASSWORD_LENGTH+1];
    char login       [SE_MAX_LOGIN_LENGTH+1];
    char output_file [TR_ARG_MAX_LENGTH+1];

    QueryType query_type = TL_XQuery;

    /* Special transactions */
    bool run_recovery      = false;
    bool first_transaction = false;

    transaction_id trid  = -1;
    session_id     sid   = -1;

    bool is_need_checkpoint_on_transaction_commit = false;
    bool is_ro_mode       = false;
    bool is_log_less_mode = false;

    int internal_auth_switch = DEPLOY_AUTH_CHECK;

    USemaphore wait_sem;

    /* 
     * Used to interrupt execution due to timeout. 
     * Pping manages state of this variable.
     */
    TLS_VAR_DECL
    volatile bool is_timer_fired = false;

    Serializer * serializer = NULL;

    void create_serializer(enum se_output_method method) {
        if (serializer != NULL && !serializer->supports(method)) {
            delete serializer;
            serializer = NULL;
        }

        if (serializer == NULL) {
            serializer = Serializer::createSerializer(method);
        }
    }
    
//     client_core*  client  = NULL;
}