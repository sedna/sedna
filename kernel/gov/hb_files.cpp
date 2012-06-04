
#include "hb_files.h"

#include <stdio.h>

#include "u/uutils.h"
#include "u/uprocess.h"
#include "u/uhdd.h"

#include "common/sedna.h"


// make data file name and write it to buf
// return: length of str
unsigned hbMakeDataFileName(char *buf, unsigned int maxlen, const char *dbname)
{
    char data_file_name[U_MAX_PATH + 1];

    // make data file name
    strcpy(data_file_name, SEDNA_DATA);
    strcat(data_file_name, "/data/");
    strcat(data_file_name, dbname);
    strcat(data_file_name, "_files/");
    strcat(data_file_name, dbname);
    strcat(data_file_name, ".sedata");

    if (strlen(data_file_name) > maxlen) return (unsigned)-1;

    strncpy(buf, data_file_name, maxlen);

    return (unsigned)strlen(data_file_name);
}

// make llog file name and write it to buf
// return: length of str
unsigned hbMakeLogFileName(char *buf, unsigned int maxlen, const char *dbname, uint64_t lnum)
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

    if (strlen(log_file_name) > maxlen) return (unsigned)-1;

    strncpy(buf, log_file_name, maxlen);

    return (unsigned)strlen(log_file_name);
}

// retrieves db config file name
unsigned hbMakeConfFileName(char *buf, unsigned int maxlen, const char *dbname)
{
    char cfg_file_name[U_MAX_PATH + 1];

    // make data file name
    strcpy(cfg_file_name, SEDNA_DATA);
    strcat(cfg_file_name, "/cfg/");
    strcat(cfg_file_name, dbname);
    strcat(cfg_file_name, "_cfg.xml");

    if (strlen(cfg_file_name) > maxlen) return (unsigned)-1;

    strncpy(buf, cfg_file_name, maxlen);

    return (unsigned)strlen(cfg_file_name);
}
