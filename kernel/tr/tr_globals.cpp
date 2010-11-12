/*
 * File:  tr_globals.cpp
 * Copyright (C) 2004 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#include <string>

#include "common/sedna.h"

#include "common/SSMMsg.h"
#include "common/argtable.h"
#include "common/version.h"

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
    int socket_port       = 0;
    int authentication    = 1;
    int authorization     = 1;
    int query_timeout     = 0;
    int max_stack_depth   = 0;
    
    /* Special transactions */
    bool run_recovery      = false;
    bool first_transaction = false;

    char db_name     [SE_MAX_DB_NAME_LENGTH+1];
    char filename    [TR_ARG_MAX_LENGTH+1];
    char password    [SE_MAX_PASSWORD_LENGTH+1];
    char login       [SE_MAX_LOGIN_LENGTH+1];
    char output_file [TR_ARG_MAX_LENGTH+1];

    QueryType query_type = TL_XQuery;

    transaction_id trid  = -1;
    session_id     sid   = -1;

    bool is_need_checkpoint_on_transaction_commit = false;
    bool is_ro_mode       = false;
    bool is_ft_disabled   = false;
    bool is_log_less_mode = false;

    client_core*  client  = NULL;
    pping_client* ppc     = NULL;

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
}

static const size_t narg = 13;

static int tr_s_help  = 0;
static int tr_l_help  = 0;
static int tr_version = 0;

static char q_type[TR_ARG_MAX_LENGTH+1];


static arg_rec tr_argtable[] =
{
    {"-help",            NULL,       arg_lit,   &tr_s_help,                 "0",       "\t\t\t  display this help and exit"},
    {"--help",           NULL,       arg_lit,   &tr_l_help,                 "0",       "\t\t  display this help and exit"},
    {"-version",         NULL,       arg_lit,   &tr_version,                "0",       "\t\t  display product version and exit"},
    {"-output",         " file",     arg_str,   output_file,                "STDOUT",  "\t\t  outputfile (default STDOUT)"},
    {"-show-time",      " on/off",   arg_bool,  &show_time,                 "off",     "\t  show time of query execution (default off)"},
    {"-print-intermed", " on/off",   arg_bool,  &print_intermed,            "off",     "  print intermediate results for debug purposes\n\t\t\t  (default off)"},
    {"-query-type",     " type",     arg_str,   q_type,                     "XQuery",  "\t  type of the query to execute: XQuery, ASTI, ASTQ\n\t\t\t  (default XQuery)"},
    {"-debug",          " on/off",   arg_bool,  &debug_mode,                "off",     "\t\t  execute statements in debug mode (default off)\t"},
    {"-timeout",        " value",    arg_int,   &query_timeout,             "0",       "\t\t  set timeout for execution of a query in seconds (no timeout by default)\t"},
    {"-name",           " name",     arg_str,   tr_globals::login,          "SYSTEM",  "\t\t  user name (default SYSTEM)"},
    {"-pswd",           " password", arg_str,   password,                   "MANAGER", "\t  user password (default MANAGER)"},
    {NULL,              " db-name",  arg_str,   db_name,                    "???",     "\t\t  database name"},
    {NULL,              " filename", arg_str,   filename,                   "???",     "\t\t  file with an XQuery query\n\t\t\t  "}
};

static bool
is_command_line_args_length_overflow(int argc, char ** argv)
{
    for (int i = 1; i < argc; i++)
    {
        if (strlen(argv[i]) > s_min(SE_MAX_DB_NAME_LENGTH,
            s_min(SE_MAX_PASSWORD_LENGTH,
            s_min(TR_ARG_MAX_LENGTH, SE_MAX_LOGIN_LENGTH)))) return true;
    }
    return false;
}

static void
print_tr_usage()
{
    throw USER_SOFT_EXCEPTION((std::string("Usage: se_trn [options] dbname filename\n\n") +
        std::string("options:\n") + std::string(arg_glossary(tr_argtable, narg, "  ")) + std::string("\n")).c_str());
}


void
parse_trn_command_line(int argc, char** argv)
{
    char errmsg[1000];

    if (is_command_line_args_length_overflow(argc, argv))
        throw USER_EXCEPTION(SE4600);

    if (argc == 1)
        print_tr_usage();

    int res = arg_scanargv(argc, argv, tr_argtable, narg, NULL, errmsg, NULL);

    if (tr_s_help == 1 || tr_l_help == 1)
        print_tr_usage();

    if (tr_version == 1) {
        print_version_and_copyright("Sedna Transaction");
        throw USER_SOFT_EXCEPTION("");
    }

    if (0 == res)
        throw USER_EXCEPTION2(SE4601, errmsg);

    /* Convert query type */
    if (strcmp(q_type, "XQuery") == 0)   query_type = TL_XQuery;
    else if (strcmp(q_type, "ASTI") == 0) query_type = TL_ASTInitial;
    else if (strcmp(q_type, "ASTQ") == 0)  query_type = TL_ASTQEPReady;
    else throw USER_EXCEPTION(SE4002);
}

