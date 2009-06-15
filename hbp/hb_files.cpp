/*
 * File:  hb_main.cpp - Procedures for file copying, making directory, etc.
 * Copyright (C) 2008 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

/*
 * Hot-backup directory structure:
 *
 * <dist-dir-from-cmd>/backup-<db-name>-<curr time>/cfg/<db-name>_cfg.xml
 * <dist-dir-from-cmd>/backup-<db-name>-<curr time>/cfg/sednaconf.xml
 * <dist-dir-from-cmd>/backup-<db-name>-<curr time>/data/<db-name>.sedata
 * <dist-dir-from-cmd>/backup-<db-name>-<curr time>/data/<db-name>.<number>llog
 * <dist-dir-from-cmd>/backup-<db-name>-<curr time>/data/vmm.dat
 *
 */

#include "hb_files.h"
#include "hb_aux.h"

#include <string>

#include "common/sedna.h"
#include "common/u/uhdd.h"

using namespace std;

static char hbDistance[U_MAX_PATH + 1];
static char hbDBName[SE_MAX_DB_NAME_LENGTH + 1];

typedef pair <string, bool> name_isdir_pair;    // name-bool pair, "true" for dir, "false" for file
static vector <name_isdir_pair> hbCreatedFiles; // created dirs and files register here, used for cleanup

// write distance directory name to hbDistance and create directory
static int hbMakeDirWithTimestamp(const char *dist_dir, const char *db_name)
{
        char dt_buf[32];
        struct tm *newtime;
        time_t aclock;

        // obtain timestamp for backup directory
        time(&aclock);
        newtime = localtime(&aclock);

        sprintf(dt_buf,"%04d-%02d-%02d-%02d-%02d-%02d/",
                    newtime->tm_year + 1900, newtime->tm_mon + 1, newtime->tm_mday,
                    newtime->tm_hour, newtime->tm_min, newtime->tm_sec);

        // make distance name
        strcpy(hbDistance, dist_dir);

        // create directory if user specified so
        if (hb_mkdir && !uIsFileExist(hbDistance, NULL))
        {
        	if (uMkDir(hbDistance, NULL, __sys_call_error) == 0)
    	       	return -1;

	        // register directory
    	    hbCreatedFiles.push_back(name_isdir_pair(string(hbDistance), true));
    	}

        if (hb_timestamp)
        {
	        strcat(hbDistance, "/backup-");
    	    strcat(hbDistance, db_name);
        	strcat(hbDistance, "-");
        	strcat(hbDistance, dt_buf);

	       	if (!uIsFileExist(hbDistance, NULL))
	       	{
	       		if (uMkDir(hbDistance, NULL, __sys_call_error) == 0)
   		       		return -1;

	        	// register directory
    	    	hbCreatedFiles.push_back(name_isdir_pair(string(hbDistance), true));
    	    }
        }
        else
	        strcat(hbDistance, "/");

	    // if directory doesn't exist - error
	    if (!uIsFileExist(hbDistance, NULL))
           	return -1;

        return 0;
}

// make data directory
static int hbMakeDataDirectory()
{
    char data_dir_name[U_MAX_PATH + 1];

    // make data dir name
    strcpy(data_dir_name, hbDistance);
    strcat(data_dir_name, "/data/");

    // create backup directory
   	if (!uIsFileExist(data_dir_name, NULL))
   	{
	   	if (uMkDir(data_dir_name, NULL, __sys_call_error) == 0)
    	   	return -1;

	    // register directory
    	hbCreatedFiles.push_back(name_isdir_pair(string(data_dir_name), true));
    }

    strcat(data_dir_name, hb_db_name);
    strcat(data_dir_name, "_files/");

    // create backup directory
   	if (!uIsFileExist(data_dir_name, NULL))
   	{
	   	if (uMkDir(data_dir_name, NULL, __sys_call_error) == 0)
    	   	return -1;

	    // register directory
    	hbCreatedFiles.push_back(name_isdir_pair(string(data_dir_name), true));
    }

    return 0;
}

// make cfg directory
static int hbMakeCfgDirectory()
{
    char cfg_dir_name[U_MAX_PATH + 1];

    // don't create cfg directory in add mode
//    if (!strncmp(hb_incr_mode, "add", 512))
//    	return 0;

    // make cfg dir name
    strcpy(cfg_dir_name, hbDistance);
    strcat(cfg_dir_name, "/cfg");

    // create backup directory
   	if (!uIsFileExist(cfg_dir_name, NULL))
   	{
	   	if (uMkDir(cfg_dir_name, NULL, __sys_call_error) == 0)
    	   	return -1;

	    // register directory
    	hbCreatedFiles.push_back(name_isdir_pair(string(cfg_dir_name), true));
    }

    return 0;
}

// prepare distance directory (make hot-backup directory with current timestamp and cfg directory)
int hbPrepareDistance(const char *hb_dir_name, const char *hb_db_name)
{
	// make main timestamp directory
	if (hbMakeDirWithTimestamp(hb_dir_name, hb_db_name) == -1) return -1;
	// make data directory
	if (hbMakeDataDirectory() == -1) return -1;
	// make cfg directory
	if (hbMakeCfgDirectory() == -1) return -1;
	// copy db name
	strcpy(hbDBName, hb_db_name);

	return 0;
}

// copy one of the additional files
int hbCopyFile(char *file_path)
{
	char file_name[U_MAX_PATH + 1];
    char backup_file_name[U_MAX_PATH + 1];

	if (uGetFileNameFromFilePath(file_path, file_name, U_MAX_PATH + 1, __sys_call_error) == NULL)
		return -1;

    // make backup file name
    strcpy(backup_file_name, hbDistance);
    int len = strlen(file_name);

    if (len >= 3 && !strcmp(&(file_name[len - 3]), "xml"))
   	    strcat(backup_file_name, "/cfg/");
    else if (!strcmp(file_name, "vmm.dat"))
	    strcat(backup_file_name, "/data/");
    else
    {
	    strcat(backup_file_name, "/data/");
	    strcat(backup_file_name, hb_db_name);
    	strcat(backup_file_name, "_files/");
    }

    strcat(backup_file_name, file_name);

    if (uCopyFile(file_path, backup_file_name, false, __sys_call_error) == 0)
        return -1;

    // register data file
   	hbCreatedFiles.push_back(name_isdir_pair(string(backup_file_name), false));

   	return 0;
}

// makes cleanup of hot-backup files in case of failure
void hbMakeCleanup()
{
	for (int i = hbCreatedFiles.size() - 1; i >= 0; i--)
	{
		if (hbCreatedFiles[i].second) // this is a directory
		{
	        if (uDelDir(hbCreatedFiles[i].first.c_str(), __sys_call_error) == 0)
    	   		throw USER_EXCEPTION2(SE4041, hbCreatedFiles[i].first.c_str());
    	}
    	else // this is a file
		{
	        if (uDeleteFile(hbCreatedFiles[i].first.c_str(), __sys_call_error) == 0)
    	   		throw USER_EXCEPTION2(SE4041, hbCreatedFiles[i].first.c_str());
    	}
	}
}
