/*
 * File:  startup.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */
    
#include <stdio.h>
#include <iostream>
#include <ostream>
#include <sys/types.h>

#include "libsedna.h"

#include "base.h"
#include "argtable.h"
#include "d_printf.h"
#include "version.h"
#include "exceptions.h"
#include "term_globals.h"
#include "term_funcs.h"
#include "mainloop.h"
#include "sprompt.h"

using namespace std;

void print_term_usage()
{
    printf("Usage: se_term [options] dbname\n\n");
    printf("options:\n%s\n", arg_glossary(term_argtable, narg, "  "));
    exit(0);
}

/*
 * main
 */
int main(int argc, char *argv[])
{
	int arg_scan_ret_val = 0; // 1 - parsed successful, 0 - there was errors
	char errmsg[1000];
	arg_scan_ret_val = arg_scanargv(argc, argv, term_argtable, narg, NULL, errmsg, NULL);
	
	int			ret_code;
	
	try{
		if (arg_scan_ret_val == 0)
			throw USER_ENV_EXCEPTION(errmsg, false);

        if (argc == 1) { print_term_usage(); return 0; }
		
        //d_printf2("params:help=%d, mode=%d", lstnr_help, background_mode);
        
        if ((term_s_help == 1)||(term_l_help == 1)){print_term_usage(); return 0; }
        
        if (term_version == 1) {print_version_and_copyright("Sedna Terminal");  return 0; }
        
        if (socket_port == 0) socket_port = 5050;
        if (strcmp(host, "???") == 0 ) strcpy(host, "localhost"); 
        
        if (strcmp(db_name, "???") == 0 )
            throw USER_EXCEPTION(SE4601);
/*
#ifdef AUTH_SWITCH
#  if (AUTH_SWITCH == 1)
        if (strcmp(login,"???") == 0)           //
        {                                       // security is ON!!!
        //	throw USER_EXCEPTION(SE3067);       //

        //
        //  ask password from keybord
        //
       		char* got_login = simple_prompt("Login: ", 100, true);
		    strcpy(login,got_login);
       		free(got_login);
        }                                       //
                                                // 
       if (strcmp(password,"???") == 0)
       {
        //
        //  ask password from keybord
        //
       		char* got_password = simple_prompt("Password: ", 100, false);
		    strcpy(password,got_password);
       		free(got_password);
       }
#  endif
#  endif
*/
        if (strcmp(login,"???") == 0)
            strcpy(login,"SYSTEM");

        if (strcmp(password,"???") == 0)
            strcpy(password,"MANAGER");
        
        if (strcmp(output_file, "STDOUT") == 0)
            res_os = stdout;
        else
            res_os = fopen(output_file, "w");
        
        if (strcmp(filename,"???") != 0)
            ret_code = process_file_commands();
        else if (strcmp(query, "???") != 0)
            ret_code = process_commandline_query();
        else
            ret_code = MainLoop(stdin);
        
        if (strcmp(output_file, "STDOUT") != 0)
            fclose(res_os);

        return ret_code;
        
    } catch (SednaUserException &e) {
        if (strcmp(output_file, "STDOUT") != 0) fclose(res_os);
    	fprintf(stderr, "%s\n", e.getMsg().c_str());
    } catch (SednaException &e) {
    	fprintf(stderr, "System error\n");
        sedna_soft_fault(e);
    } catch (...) {
       	fprintf(stderr, "System error\n");
       	sedna_soft_fault();
    }

}
