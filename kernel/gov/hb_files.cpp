
#include "hb_files.h"

#include "common/u/u.h"
#include "common/u/uutils.h"
#include "common/u/uprocess.h"
#include "common/u/uhdd.h"
#include <stdio.h>
#include "common/sedna.h"

// make data file name and write it to buf
// return: length of str
int hbMakeDataFileName(char *buf, unsigned int maxlen, const char *dbname)
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
int hbMakeLogFileName(char *buf, unsigned int maxlen, const char *dbname, uint64_t lnum)
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
    strcat(log_file_name, u_ui64toa(lnum, ui_buf, 10));
   	strcat(log_file_name, ".llog");

    if (strlen(log_file_name) > maxlen) return -1;

    strncpy(buf, log_file_name, maxlen);

    return strlen(log_file_name);
}

// retrieves vmm.dat file name
int	hbMakeVmmFileName(char *buf, unsigned int maxlen)
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

// retrieves db config file name
int	hbMakeConfFileName(char *buf, unsigned int maxlen, const char *dbname)
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
