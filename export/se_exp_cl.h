#include "argtable.h"

#define ARG_SIZE 1000

const size_t narg = 11;

int exp_s_help = 0;
int exp_l_help = 0;
int exp_version = 0;
int exp_verbose = 0;
int exp_log = 1;
int socket_port = 5050;

char command[ARG_SIZE];
char path[ARG_SIZE];
char host[ARG_SIZE];
char db_name[ARG_SIZE];
char login[ARG_SIZE];
char password[ARG_SIZE];

arg_rec exp_argtable[] =
{
	{"-help",            NULL,       arg_lit,   &exp_s_help,                 "0",        "\t\t\t  display this help and exit"},
	{"--help",           NULL,       arg_lit,   &exp_l_help,                 "0",        "\t\t  display this help and exit"},
	{"-version",         NULL,       arg_lit,   &exp_version,                "0",        "\t\t  display product version and exit"},
	{"-verbose",        " on/off",   arg_bool,  &exp_verbose,                "off",      "\t  verbose output (default off)"},
	{"-log",            " on/off",   arg_bool,  &exp_log,                    "on",      "\t\t  write lof file (default off)"},
	{"-host",           " host",     arg_str,   &host,                      "localhost", "\t\t  hostname of the machine with Sedna running\n\t\t\t  (default localhost)"},
 /*   {"-port-number",    " port",     arg_int,   &socket_port,               "5050",      "\t  socket listening port  (default 5050)"},*/
	{"-name",           " name",     arg_str,   &login,                     "-",         "\t\t  user name "},
	{"-pswd",           " password", arg_str,   &password,                  "-",         "\t  user password "},
	{NULL,              " command",  arg_str,   &command,                   "-",         "\t\t  'export'-to export data/'restore'-to restore data/'import'-to import data"},
	{NULL,              " db-name",  arg_str,   &db_name,                   "-",         "\t\t  database name"},
	{NULL,              " path",     arg_str,   &path,                      "-",         "\t\t\t  path with exported/imported data"}

};
