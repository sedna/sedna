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

    for(int i=0; i<argc; i++)
    {
        if(strlen(argv[i])>TERM_ARGSTRLEN)
        {
            fprintf(stderr, "One of the command line argumets is too long\n");
            return EXIT_TERM_FAILED;
        }
    }
    
	arg_scan_ret_val = arg_scanargv(argc, argv, term_argtable, narg, NULL, errmsg, NULL);
	
	int			ret_code;
	
	try{
		if (arg_scan_ret_val == 0)
			throw USER_ENV_EXCEPTION(errmsg, false);

        if (argc == 1) { print_term_usage(); return 0; }

        //d_printf2("params:help=%d, mode=%d", lstnr_help, background_mode);

        if ((term_s_help == 1)||(term_l_help == 1)){print_term_usage(); return 0; }

        if (term_version == 1) {print_version_and_copyright("Sedna Terminal"); throw USER_SOFT_EXCEPTION("");}

        if (socket_port == 0) socket_port = 5050;
        if (strcmp(host, "???") == 0 ) strcpy(host, "localhost"); 

        if (strcmp(db_name, "???") == 0 )
            throw USER_EXCEPTION(SE4601);

        if (strcmp(echo_str, "???") != 0)
        {
            if ((strcmp(echo_str, "on") != 0) && (strcmp(echo_str, "off") != 0)) throw USER_EXCEPTION(SE4601);
            echo = (strcmp(echo_str, "on") == 0) ? 1 : 0;
        }
        if (strcmp(login,"???") == 0)
            strcpy(login,"SYSTEM");

        if (strcmp(password,"???") == 0)
            strcpy(password,"MANAGER");

        if (strcmp(output_file, "STDOUT") == 0)
            res_os = stdout;
        else
            if((res_os = fopen(output_file, "wt")) == NULL)
            {
                fprintf(stderr, "Can't open file %s\n", output_file);
                return EXIT_TERM_FAILED;
            } 
        
        if (strcmp(filename,"???") != 0)
        {
            FILE* script_file;
			
	interactive_mode=0;

            if (strcmp(echo_str, "???") == 0)
                echo = 0;                          // echo is off when in batch mode
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
                echo = 0;                          // echo is off when execute command line query
            
			interactive_mode=0;
			ret_code = process_commandline_query();
        }
        else
        {
           if (strcmp(echo_str, "???") == 0)
                echo = 1;                          // echo is on when in interactive mode
		   if (interactive_mode) do
		   {
			   interactive_mode=0;
			   if (!isatty(fileno(stdin))) break;
			   if (!ile_init()) break;
			   interactive_mode=1;
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
    	fprintf(stderr, "%s\n", e.getMsg().c_str());
    } catch (SednaException &e) {
    	fprintf(stderr, "System error\n");
    	fprintf(stderr, e.getMsg().c_str());
    } catch (ANY_SE_EXCEPTION) {
       	fprintf(stderr, "System error\n");
    }
    
    return 1;
}
