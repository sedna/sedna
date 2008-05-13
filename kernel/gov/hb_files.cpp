
#include "hb_files.h"

#include "common/u/u.h"
#include "common/u/uutils.h"
#include "common/u/uprocess.h"
#include "common/u/uhdd.h"
#include <stdio.h>
#include "common/sedna.h"

// make data file name and write it to buf
// return: length of str
int hbMakeDataFileName(char *buf, int maxlen, const char *dbname)
{
    char data_file_name[U_MAX_PATH + 1];

    // make data file name
    strcpy(data_file_name, SEDNA_DATA);
    strcat(data_file_name, "/data/");
    strcat(data_file_name, dbname);
    strcat(data_file_name, "_files/");
    strcat(data_file_name, dbname);
    strcat(data_file_name, ".sedata");

    if (strlen(data_file_name) > maxlen) return -1;

    strncpy(buf, data_file_name, maxlen);

    return strlen(data_file_name);
}

// make llog file name and write it to buf
// return: length of str
int hbMakeLogFileName(char *buf, int maxlen, const char *dbname, int lnum)
{
    char log_file_name[U_MAX_PATH + 1];

    char ui_buf[20]; // for u_uitoa

    // make log file name
    strcpy(log_file_name, SEDNA_DATA);
    strcat(log_file_name, "/data/");
    strcat(log_file_name, dbname);
    strcat(log_file_name, "_files/");
    strcat(log_file_name, dbname);
    strcat(log_file_name, ".");
    strcat(log_file_name, u_itoa(lnum, ui_buf, 10));
   	strcat(log_file_name, "llog");

    if (strlen(log_file_name) > maxlen) return -1;

    strncpy(buf, log_file_name, maxlen);

    return strlen(log_file_name);
}

// retrieves ph file name
int	hbMakePhFileName(char *buf, int maxlen, const char *dbname, __int64 ts)
{
    char ph_file_name[U_MAX_PATH + 1];

    char ui_buf[20]; // for u_ui64toa

    // make ph file name
    strcpy(ph_file_name, SEDNA_DATA);
    strcat(ph_file_name, "/data/");
    strcat(ph_file_name, dbname);
    strcat(ph_file_name, "_files/");
    strcat(ph_file_name, dbname);
    strcat(ph_file_name, ".");
    strcat(ph_file_name, u_i64toa(ts, ui_buf, 10));
   	strcat(ph_file_name, ".seph");

    if (strlen(ph_file_name) > maxlen) return -1;

    strncpy(buf, ph_file_name, maxlen);

    return strlen(ph_file_name);
}

// retrieves vmm.dat file name
int	hbMakeVmmFileName(char *buf, int maxlen)
{
    char vmm_file_name[U_MAX_PATH + 1];

    // make data file name
    strcpy(vmm_file_name, SEDNA_DATA);
    strcat(vmm_file_name, "/data/");
    strcat(vmm_file_name, "vmm.dat");

    if (strlen(vmm_file_name) > maxlen) return -1;

    strncpy(buf, vmm_file_name, maxlen);

    return strlen(vmm_file_name);
}

// retrieves sednaconf file name
int	hbMakeConfGlobalFileName(char *buf, int maxlen)
{
	//find sednaconf.xml
	char sedna_cfg_file[U_MAX_PATH + 1];

	// start from image path
	uGetImageProcPath(sedna_cfg_file, __sys_call_error);
	if (sedna_cfg_file[0] == '\0') 
    	throw USER_EXCEPTION(SE4081);

  	strcat(sedna_cfg_file, "/../etc/sednaconf.xml");

  	// cfg file not found
  	if (!uIsFileExist(sedna_cfg_file, __sys_call_error))
  	{
#ifndef _WIN32 //UNIX, trying to find in /etc
    	strcpy(sedna_cfg_file, "/etc/sednaconf.xml");

    	// still not found; it's ok, just won't archive it
    	if (!uIsFileExist(sedna_cfg_file, __sys_call_error))
    		return 0;
#else
		return 0;
#endif
  	}

    if (strlen(sedna_cfg_file) > maxlen) return -1;

    strncpy(buf, sedna_cfg_file, maxlen);

    return strlen(sedna_cfg_file);
}

// retrieves db config file name
int	hbMakeConfFileName(char *buf, int maxlen, const char *dbname)
{
    char cfg_file_name[U_MAX_PATH + 1];

    // make data file name
    strcpy(cfg_file_name, SEDNA_DATA);
    strcat(cfg_file_name, "/cfg/");
    strcat(cfg_file_name, dbname);
    strcat(cfg_file_name, "_cfg.xml");

    if (strlen(cfg_file_name) > maxlen) return -1;

    strncpy(buf, cfg_file_name, maxlen);

    return strlen(cfg_file_name);
}
