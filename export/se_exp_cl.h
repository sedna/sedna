/*
 * File: se_exp_cl.h
 * Copyright (C) 2013 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */

#ifndef __SE_EXP_CL_H
#define __SE_EXP_CL_H

#include "common/argtable.h"

#define ARG_SIZE 1000
#define SOCKET_PORT_DIGITS_NUM 20
#define PARSE_ERR_MSG_SIZE 1000
#define SPROMT_LOGIN_SIZE 100

const int narg = 13;

int exp_s_help = 0;
int exp_l_help = 0;
int exp_version = 0;
int exp_verbose = 0;
int exp_log = 1;
int exp_ro = 0;
int exp_idx_skip = 0;
int socket_port = 5050;

char command[ARG_SIZE];
char path[ARG_SIZE];
char host[ARG_SIZE + SOCKET_PORT_DIGITS_NUM + 1];
char db_name[ARG_SIZE];
char login[ARG_SIZE];
char password[ARG_SIZE];

arg_rec exp_argtable[] =
{
        {"-help",            NULL,       arg_lit,   &exp_s_help,                 "0",         "\t\t\t  display this help and exit"},
        {"--help",           NULL,       arg_lit,   &exp_l_help,                 "0",         "\t\t  display this help and exit"},
        {"-version",         NULL,       arg_lit,   &exp_version,                "0",         "\t\t  display product version and exit"},
        {"-verbose",        " on/off",   arg_bool,  &exp_verbose,                "off",       "\t  verbose output (default off)"},
        {"-ro",             " on/off",   arg_bool,  &exp_ro,                     "off" ,      "\t\t  run export transaction in read-only mode (default off)"},
        {"-host",           " host",     arg_str,   host,                        "localhost", "\t\t  hostname of the machine with Sedna running\n\t\t\t  (default localhost)"},
        {"-port-number",    " port",     arg_int,   &socket_port,                "5050",      "\t  socket listening port  (default 5050)"},
        {"-name",           " name",     arg_str,   login,                       "SYSTEM",    "\t\t  user name "},
        {"-pswd",           " password", arg_str,   password,                    "MANAGER",   "\t  user password "},
        {"-idx-skip",       " on/off",   arg_bool,  &exp_idx_skip,               "off",       "\t  skip indices (default off)"},
        {NULL,              " command",  arg_str,   command,                     "-",         "\t\t  'export' - to export data\n\t\t\t  'restore' - to restore data\n\t\t\t  'import' - to import data"},
        {NULL,              " db-name",  arg_str,   db_name,                     "-",         "\t\t  database name"},
        {NULL,              " path",     arg_str,   path,                        "-",         "\t\t\t  path with exported/imported data"}

};

#endif /* __SE_EXP_CL_H */
