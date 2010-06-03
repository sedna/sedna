/*
 * File:  startup.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */
    
#include <iostream>
#include <ostream>

#include "common/sedna.h"

#include "libsedna.h"
#include "common/base.h"
#include "common/argtable.h"
#include "common/errdbg/d_printf.h"
#include "common/version.h"
#include "term_globals.h"
#include "term_funcs.h"
#include "mainloop.h"
#include "sprompt.h"
#include "term_ile.h"

namespace term_globals 
{
    bool show_time        = false;
    bool interactive_mode = true;
    bool debug_mode       = false;
    bool echo             = true;
    bool debug_output     = true;
    bool on_error_stop    = false;
    
    int socket_port = 0;

    char host[TERM_ARGSTRLEN+1];
    char db_name[TERM_ARGSTRLEN+1];
    char filename[TERM_ARGSTRLEN+1];
    char login[TERM_ARGSTRLEN+1];
    char password[TERM_ARGSTRLEN+1];
    char session_dir[U_MAX_PATH+1];
    char query[TERM_ARGSTRLEN+1];
    char prompt[16]="> ";
    char micro_prompt[16]="> ";
    
    SednaConnection conn = SEDNA_CONNECTION_INITIALIZER;
}

using namespace std;
using namespace term_globals;

static const int narg = 14;

static int term_s_help     = 0;
static int term_l_help     = 0;
static int term_version    = 0;
static int term_show_time  = 0;
static int term_debug_mode = 0;

static char output_file[TERM_ARGSTRLEN+1];
static char echo_str[TERM_ARGSTRLEN+1];

static arg_rec term_argtable[] =
{
    {"-help",            NULL,       arg_lit,   &term_s_help,                 "0",       "\t\t\tdisplay this help and exit"},
    {"--help",           NULL,       arg_lit,   &term_l_help,                 "0",       "\t\tdisplay this help and exit"},
    {"-version",         NULL,       arg_lit,   &term_version,                "0",       "\t\tdisplay product version and exit"},
    {"-file",           " filename", arg_str,   filename,                     "???",     "\tfile with an XQuery query"},
    {"-output",         " filename", arg_str,   output_file,                  "STDOUT",  "\toutput file (default stdout)"},
    {"-query",          " \"query\"",arg_str,   query,                        "???",     "\tXQuery query to execute\n"},
    {"-echo",           " on/off",   arg_str,   echo_str,                     "???",     "\t\tdisplay se_term output\n\t\t\t(default: on for interactive mode,\n\t\t\t\t  off for batch mode)\n"},
    {"-show-time",      " on/off",   arg_bool,  &term_show_time,              "off",     "\tshow time of the latest query execution\n\t\t\t(default off)\n"},
    {"-debug",          " on/off",   arg_bool,  &term_debug_mode,             "off",     "\texecute statements in debug mode\n\t\t\t(default off)\n"},
    {"-host",           " host",     arg_str,   host,                         "???",     "\t\thostname of the machine with Sedna running\n\t\t\t(default localhost)\n"},
    {"-port-number",    " port",     arg_int,   &socket_port,                 "5050",    "\tsocket listening port (default 5050)"},
    {"-name",           " name",     arg_str,   login,                        "???",     "\t\tuser name "},
    {"-pswd",           " password", arg_str,   password,                     "???",     "\tuser password "},
    {NULL,              " db-name",  arg_str,   db_name,                      "???",     "\t\tdatabase name"}
};

static void print_term_usage(int ret_code)
{
    printf("Usage: se_term [options] dbname\n\n");
    printf("options:\n%s\n", arg_glossary(term_argtable, narg, "  "));
    exit(ret_code);
}

int main(int argc, char *argv[])
{
    int arg_scan_ret_val = 0;
    char errmsg[1000];

    /* 
     * Under Solaris there is no SO_NOSIGPIPE/MSG_NOSIGNAL/SO_NOSIGNAL,
     * so we must block SIGPIPE with sigignore.
     */
#if defined(SunOS)
    sigignore(SIGPIPE);
#endif

    for(int i=0; i<argc; i++)
    {
        if(strlen(argv[i])>TERM_ARGSTRLEN)
        {
            fprintf(stderr, "One of the command line argumets is too long\n");
            return EXIT_TERM_FAILED;
        }
    }

    arg_scan_ret_val = arg_scanargv(argc, argv, term_argtable, narg, NULL, errmsg, NULL);

    int ret_code;

    try{
        if (arg_scan_ret_val == 0)
            throw USER_ENV_EXCEPTION(errmsg, false);

        if (argc == 1) 
            print_term_usage(1);

        if ((term_s_help == 1) || (term_l_help == 1)) 
            print_term_usage(0);

        if (term_version == 1) {
            print_version_and_copyright("Sedna Terminal"); 
            return 0; 
        }

        if (socket_port == 0) socket_port = 5050;
        if (strcmp(host, "???") == 0 ) strcpy(host, "localhost"); 

        term_globals::show_time  = term_show_time  == 0 ? false : true;
        term_globals::debug_mode = term_debug_mode == 0 ? false : true;

        if (strcmp(db_name, "???") == 0 )
            throw USER_EXCEPTION(SE4601);

        check_db_name_validness(db_name);

        if (strcmp(echo_str, "???") != 0)
        {
            if ((strcmp(echo_str, "on") != 0) && (strcmp(echo_str, "off") != 0))
                throw USER_EXCEPTION(SE4601);
            echo = (strcmp(echo_str, "on") == 0) ? true : false;
        }
        if (strcmp(login,"???") == 0)
            strcpy(login,"SYSTEM");

        if (strcmp(password,"???") == 0)
            strcpy(password,"MANAGER");

        if (strcmp(output_file, "STDOUT") == 0)
            res_os = stdout;
        else if((res_os = fopen(output_file, "wt")) == NULL)
        {
            fprintf(stderr, "Can't open file %s\n", output_file);
            return EXIT_TERM_FAILED;
        } 

        if (strcmp(filename,"???") != 0)
        {
            FILE* script_file;
            interactive_mode=false;

            if (strcmp(echo_str, "???") == 0)
                /* echo is off when in batch mode */
                echo = false;
            if((script_file = fopen(filename, "rt")) == NULL) /* !!! \r\n !!! */ 
            {
                fprintf(stderr, "Can't open file %s\n", filename);
                return EXIT_TERM_FAILED;
            }			
            ret_code = MainLoop(script_file);
            if(fclose(script_file) != 0)
            {
                fprintf(stderr, "Can't close file %s\n", filename);
                return EXIT_TERM_FAILED;
            }
        }
        else if (strcmp(query, "???") != 0)
        {
            if (strcmp(echo_str, "???") == 0)
                /* echo is off when execute command line query */
                echo = false;

            interactive_mode = false;
            ret_code = process_commandline_query();
        }
        else
        {
            if (strcmp(echo_str, "???") == 0)
                /* echo is on when in interactive mode */
                echo = true;

            if (interactive_mode) do
            {
                interactive_mode = false;
                if (!isatty(fileno(stdin))) break;
                if (!ile_init()) break;
                interactive_mode = true;
            }
            while (0);

            ret_code = MainLoop(stdin);

            if (interactive_mode) ile_deinit();
        }

        if (strcmp(output_file, "STDOUT") != 0)
            fclose(res_os);

        return ret_code;

    } catch (SednaUserException &e) {
        if (strcmp(output_file, "STDOUT") != 0) fclose(res_os);
        fprintf(stderr, "%s\n", e.what());
    } catch (SednaException &e) {
        fprintf(stderr, "System error\n");
        fprintf(stderr, "%s\n", e.what());
    } catch (ANY_SE_EXCEPTION) {
        fprintf(stderr, "System error\n");
    }

    return 1;
}
