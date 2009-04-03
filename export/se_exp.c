#include "se_exp.h"
#include "se_exp_common.h"
#include "se_exp_import.h"
#include "se_exp_export.h"
#include "se_exp_cl.h"
#include "common/u/uutils.h"
#include "common/version.h"



void print_usage() {
    printf("Usage: se_exp [options] command dbname path\n\n");
    printf("options:\n%s\n", arg_glossary(exp_argtable, narg, "  "));
    exit(0);
} 


int main(int argc, char* argv[]) {
  
  /*Under Solaris there is no SO_NOSIGPIPE/MSG_NOSIGNAL/SO_NOSIGNAL,
    so we must block SIGPIPE with sigignore.*/
#if defined(SunOS)
  sigignore(SIGPIPE);
#endif

  char parse_errmsg[PARSE_ERR_MSG_SIZE];
  char *input_login=NULL,*input_password=NULL;
  int tmp;
  
    /*if (argc < 4) {
		print_usage();
		exit(-1);
	} */
	if (arg_scanargv(argc, argv, exp_argtable, narg, NULL, parse_errmsg, NULL)==0) {
		printf("ERROR: %s\n\n", parse_errmsg);
		print_usage();
		exit(-1);
	}
	if (exp_s_help == 1 || exp_l_help == 1) { 
		print_usage(); 
		exit(0); 
	}
	if (exp_version == 1) { 
		print_version_and_copyright("Sedna Export Version");
		exit(0); 
	}
	if (strcmp(command,"-") == 0) {
		print_usage();
		exit(-1);		
	}
	if (strcmp(command,"export") && strcmp(command,"import") && strcmp(command,"restore") != 0) {
		printf("ERROR: unexpected command: %s\n",command);
		print_usage();
		exit(-1);
	}
	if (exp_verbose == 1) { 
		printf("Operating in verbose mode.\n"); 
	}
	if (exp_log == 0) { 
		printf("Logging is off.\n"); 
	}

	/* don't ask for user name and password: the default values are used instead
	if (strcmp(login,"-") == 0) {           
       	input_login = simple_prompt("Login: ", SPROMT_LOGIN_SIZE, 1);
		strcpy(login,input_login);
       	if (input_login!=NULL) free(input_login);
    }                                       
	if (strcmp(password,"-") == 0) {
       	input_password = simple_prompt("Password: ", SPROMT_LOGIN_SIZE, 0);
		strcpy(password,input_password);
       	if (input_password!=NULL) free(input_password);
    }
    */

	// Add the slash to the specified path 
	tmp=strlen(path);
	if (path[tmp-1]!='\\' && path[tmp-1]!='/') {
		path[tmp]=SE_EXP_PATH_SEP;
		path[tmp+1]='\0';
	}

    // Concat the host with socket_port
	if (socket_port!=5050) {
		char buf[SOCKET_PORT_DIGITS_NUM + 1];
		strcat(host,":");
		strcat(host,u_itoa(socket_port, buf, 10));
	}

	if (!strcmp(command,"export")) {
        int export_status;

		printf("\nEXPORTING DATA (path=%s host=%s database=%s)\n",path,host,db_name);
		// until the export transaction succed
		while ((export_status=export(path,host,db_name,login,password))==SE_EXP_RB_ERROR) {
			printf("\nEXPORT TRANSACTION WAS RALLED BACK\nTRYING AGAIN...\n");
		}
		//export_status=export(path,host,db_name,login,password);
		if (export_status == SE_EXP_FATAL_ERROR) {			
			printf("\nEXPORT FAILED\n");
			exit(-1);
		} else 
		if (export_status == SE_EXP_SUCCEED) {
			printf("EXPORT SUCCEDED\n");
		} else {
			printf("EXPORT FAILED WITH UNKNOWN ERROR: %d\n", export_status);
			exit(-1);
		}
	} else
	if (!strcmp(command,"restore")) {
		printf("\nRESTORING DATA (path=%s host=%s database=%s)\n",path,host,db_name);
		// restore means to restore all data + security information
		if (import(path,host,db_name,login,password,1)!=0) {
			printf("\nRESTORING FAILED\n");
			exit(-1);
		} else {
			printf("RESTORING SUCCEDED\n");
		}
	} else {
		printf("\nIMPORTING DATA (path=%s host=%s database=%s)\n",path,host,db_name);
		// import means to restore all data except security information
		if (import(path,host,db_name,login,password,0)!=0) {
			printf("\nIMPORT FAILED\n");
			exit(-1);
		} else {
			printf("IMPORT SUCCEDED\n");
		}
	}

    return 0;
} 	
