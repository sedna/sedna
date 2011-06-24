/*
 * File:  hb_aux.cpp - Auxillary hot-backup procedures (parsing command line)
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/version.h"
#include "common/argtable.h"
#include <ctype.h>

#include "common/sedna.h"
#include "common/u/uprocess.h"
#include "common/ipc_ops.h"

static int hb_help = 0;          // help message and exit
static int hb_vers = 0;                        // show version and exit
char hb_cmd_address[U_MAX_HOSTNAME];           // command line address of running gov
static int hb_cmd_port;                        // command line port number
int hb_checkpoint = 0;                         // checkpoint needed before hot-backup
int hb_timestamp = 0;                          // we must add timestamp to directory name
int hb_mkdir = 0;                              // create directory if it doesn't exist
char hb_db_name[SE_MAX_DB_NAME_LENGTH + 1];    // name of the db to archive
char hb_dir_name[U_MAX_PATH];                  // name of the distance directory
char hb_incr_mode[512];                        // increment mode (none, start, add, disable)
char hb_address[U_MAX_HOSTNAME] = "localhost"; // address of gov is (priority: def.value->cfg file->command line)
int hb_port = 5050;                            // port number (priority: def.value->cfg file->command line)
const size_t narg = 11;  // number of arguments for argtable (must be consistent with hb_argtable below)

// command line parameters
arg_rec hb_argtable[] =
{
{"-version",         NULL,          arg_lit,         &hb_vers,                 "0",   "\t\t\t   display product version and exit"},
{"--help",           NULL,          arg_lit,         &hb_help,                "0",   "\t\t\t   display this help and exit"},
{"-help",            NULL,          arg_lit,         &hb_help,                 "0",   "\t\t\t\t   display this help and exit"},
{"-checkpoint",      NULL,          arg_lit,         &hb_checkpoint,           "0",   "\t\t\t   make checkpoint before backup"},
{"-time-dir",        NULL,          arg_lit,         &hb_timestamp,            "0",   "\t\t\t   create timestamp-subdir"}, 
{"-make-dir",        NULL,          arg_lit,         &hb_mkdir,                "0",   "\t\t\t   create directory if it doesn't exist"}, 
{"-incr-mode",       " <increment_mode>", arg_str,   hb_incr_mode,             "none", "\t   type of the increment mode (start, add, stop)"},
{"-host",            " address",          arg_str,   &hb_address,              "localhost", "\t\t\t   address where Governor is started"},
{"-port",            " port-number",      arg_int,   &hb_cmd_port,             "-1","\t\t   port number to connect to Governor\n"},
{NULL,               " dbname",           arg_str,   hb_db_name,               "???", "\t\t\t   the name of the database "},
{NULL,               " path",             arg_str,   hb_dir_name,              "???", "\t\t\t\t   the name of the backup directory"}, 
};

// print help message
static void hbPrintUsage()
{
        print_version_and_copyright("\nSedna Hot-Backup Process");
    
    fprintf(stdout, "Usage: se_hb [options] dbname path\n\noptions:\n");
    fprintf(stdout, "%s\n", arg_glossary(hb_argtable, narg, "  "));
}

// this function parses command line and fetches parameters
void hbParseCommandLine(int argc, char **argv)
{
    char buf[1024];

        // no parameters given - print usage info and exit
        if (argc == 1)
        {
                hbPrintUsage();
        exit(1);
        }

    // parse and fetch command line
    int arg_scan_ret_val = 0; // 1 - parsed successful, 0 - there was errors
    arg_scan_ret_val = arg_scanargv(argc, argv, hb_argtable, narg, NULL, buf, NULL);

    // error in parsing command line - exit
    if (arg_scan_ret_val == 0)
        throw USER_ENV_EXCEPTION(buf, false);

    if (hb_vers == 1)
    {
        print_version_and_copyright("Sedna Hot-Backup Process");
        exit(0);
    }

    // help requested - print usage info and exit
    if (hb_help == 1)
        {
                hbPrintUsage();
        exit(0);
        }

    // name of the databse is not specified - exit
    if (strcmp(hb_db_name, "???") == 0)
        throw USER_ENV_EXCEPTION("unexpected command line parameters: no dbname parameter", false);

    check_db_name_validness(hb_db_name);

        // in case of absent directory name, use image directory instead
        if (!strcmp(hb_dir_name, "???"))
        {
        throw USER_ENV_EXCEPTION("unexpected command line parameters: no path parameter", false);
/*
                uGetImageProcPath(buf, __sys_call_error);
                if (buf[0] == '\0')
                        throw USER_EXCEPTION(SE4081);

                strncpy(hb_dir_name, buf, U_MAX_PATH);
*/
        }

        // convert increment mode to lower case
        for (int i = 0; i < 512 && hb_incr_mode[i] != '\0'; i++)
                hb_incr_mode[i] = tolower(hb_incr_mode[i]);

        // check for increment mode
        if (strncmp(hb_incr_mode, "none", 512) && strncmp(hb_incr_mode, "start", 512) &&
                strncmp(hb_incr_mode, "add", 512)  && strncmp(hb_incr_mode, "stop", 512))
                throw USER_ENV_EXCEPTION("unexpected command line parameters: wrong increment mode type", false);

        // if port is specified in command line then use it instead default or cfg
        if (hb_cmd_port != -1) {
          hb_port = hb_cmd_port;
        }
        if (!strcmp(hb_cmd_address, "localhost")) {
          strcpy(hb_address, hb_cmd_address);
        }
}

// this function tries to parse sednaconf to find port number
void hbGetDefaultValues()
{
        // obtain global configuration parameters through cfg file, if any
        gov_header_struct cfg;

        get_sednaconf_values(&cfg);

        if (cfg.lstnr_port_number != hb_port) hb_port = cfg.lstnr_port_number;
        if (!strcmp(cfg.lstnr_addr, hb_address)) strcpy(hb_address, cfg.lstnr_addr);
}
