/*********************************************************************************************************
 * 
 *  Ok, here is the main plan:
 *		1) Read cfg file in /data/rcvtest.cfg
 *		2) If we have some previous saved state use it instead (/data/rcvstate.tmp)
 *      3) When we step on macros RCV_TEST_CRASH rcvTestCrashPoint function is called
 *		4) Check if we need to crash here by: 
 *				a) check if function is in cfg table
 *				b) computing probability
 *		5) If check is ok then decrementing number of tries, save state to rcvstate.tmp file and exit.
 *		6) Else, continuing execution
 *
 *	Config note: you must define all crash points in rcvtest.cfg along with crash probability 
 *	(real from [0; 1]) and max number of tries (-1 for infinite number; 0 to disable crash).
 *
 ********************************************************************************************************/

#include "common/rcv_test.h"
#include "common/errdbg/d_printf.h"
#include "common/u/uhdd.h"

#include <map>
#include <string>

typedef struct
{
	int try_numbers;
	double prob;

} rcvPointInfo;

using namespace std;

static  map <string, rcvPointInfo *> fname_cfg_table;
typedef	map <string, rcvPointInfo *>::iterator table_it;

// store current crash info into file; file name is relative to SEDNA_DATA
static void rcvSerializeInfoToFile(const char *file_name)
{
	char tmp_file_name[U_MAX_PATH];
	UFile tfh;
	table_it it = fname_cfg_table.begin();
	unsigned int written, len;
    int err = 0;

	sprintf(tmp_file_name, "%s/%s", SEDNA_DATA, file_name);

	// delete old file (ignore error, if it doesn't exist)	
	uDeleteFile(tmp_file_name, __sys_call_error);

	// create new file
	tfh = uCreateFile(tmp_file_name, U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE, 0, NULL, NULL);
	if (tfh == U_INVALID_FD)
   		fprintf(stderr, "Cannot create temporary file to store rcv_crash info\n");

	while (it != fname_cfg_table.end())
	{
		// get function name length
		len = (it->first).size();

		// store string length
		if (uWriteFile(tfh, &len, sizeof len, &written, __sys_call_error) == 0 ||
				written != sizeof len)
		{
	   		fprintf(stderr, "Cannot serialize recovery crash info\n");
			err = 1;
			break;
		}

		// store current function name
		if (uWriteFile(tfh, (it->first).c_str(), len, &written, __sys_call_error) == 0 ||
				written != len)
		{
	   		fprintf(stderr, "Cannot serialize recovery crash info\n");
			err = 1;
			break;
		}

		// store the associated structure
		if (uWriteFile(tfh, (it->second), sizeof(rcvPointInfo), &written, __sys_call_error) == 0 ||
				written != sizeof(rcvPointInfo))
		{
	   		fprintf(stderr, "Cannot serialize recovery crash info\n");
			err = 1;
			break;
		}
		
		// next element
		it++;
	}

	uCloseFile(tfh, __sys_call_error);

	// delete tmp file in case of error	
	if (err == 1)
		uDeleteFile(tmp_file_name, __sys_call_error);
}

// load current crash info from file; file name is relative to SEDNA_DATA
// Returns: 0 - all ok; 1 - some problem (file not found, etc.)
static int rcvLoadInfoFromFile(const char *file_name)
{
	char tmp_file_name[U_MAX_PATH], func_buf[1024];
	UFile tfh;
	unsigned int readb, len;
	rcvPointInfo *rcvInfo;
	table_it it;

	sprintf(tmp_file_name, "%s/%s", SEDNA_DATA, file_name);

	// open temp file
	tfh = uOpenFile(tmp_file_name, U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE, 0, NULL);
	// if we cannot open tmp file then read default cfg instead
	if (tfh == U_INVALID_FD)
		return 1;  

	while (1)
	{
		// load string length
		if (uReadFile(tfh, &len, sizeof(len), &readb, __sys_call_error) == 0 ||
				readb != sizeof(len))
			break;

		// load current function name
		if (uReadFile(tfh, func_buf, len, &readb, __sys_call_error) == 0 ||
				readb != len)
			break;

		// add trailing zero
		func_buf[len] = '\0';

		// allocate new info structure
		if ((rcvInfo = (rcvPointInfo *)malloc(sizeof(rcvPointInfo))) == NULL)
			throw SYSTEM_EXCEPTION("Cannot allocate memory!");

		// load the associated structure
		if (uReadFile(tfh, rcvInfo, sizeof(rcvPointInfo), &readb, __sys_call_error) == 0 ||
				readb != sizeof(rcvPointInfo))
			break;

		// append info to the table
		fname_cfg_table[func_buf] = rcvInfo;
	}

	uCloseFile(tfh, __sys_call_error);

	// delete tmp file in case of error	and clear read config
	if (readb != 0)
	{
		uDeleteFile(tmp_file_name, __sys_call_error);
		for (it = fname_cfg_table.begin(); it != fname_cfg_table.end(); it++)
			free(it->second);

		fname_cfg_table.clear();

		return 1;
	}

	return 0;
}

void rcvTestCrashPoint(const char *func_name)
{
    UFile r_fh;
	rcvPointInfo *point_info;
	table_it it;
	char crash_info[256];
	double prob;
	unsigned int written;

	// if we don't have points to crash then return
	if (fname_cfg_table.empty()) return;

	// try to find info about the point	
	it = fname_cfg_table.find(string(func_name));

	// nope, so don't crash	
	if (it == fname_cfg_table.end()) return;

	// ok, we've found it	
	point_info = it->second;

    // check probability
	if ((prob = ((double)rand() / (double)RAND_MAX)) < point_info->prob)
    {
		// print to elog
		elog(EL_ERROR, ("\nTr is crashed in %s due to recovery testing\n", func_name));
		// and to the console
		d_printf2("\nTr is crashed in %s due to recovery testing\n", func_name);
       
		// correct number of tries
		if (point_info->try_numbers != -1)
			point_info->try_numbers--;

		// recovery file-flag
		string rcv_fname = string(SEDNA_DATA) + string("/data/") + string("rcv_test_crash");

		// create notification file, so that recovery driver could recognize recovery crash
		uDeleteFile(rcv_fname.c_str(), __sys_call_error);
		r_fh = uCreateFile(rcv_fname.c_str(), U_SHARE_READ | U_SHARE_WRITE, U_READ_WRITE, 0, NULL, NULL);
		if (r_fh == U_INVALID_FD)
       		fprintf(stderr, "Cannot create rcv_test_crash file\n");
		
		// write some debug info in this file
		sprintf(crash_info, "point=%s genp=%lf prob=%lf tryrem=%d", func_name, prob, point_info->prob,
			point_info->try_numbers);

		// write this info to the file (ignore errors)
		uWriteFile(r_fh, crash_info, strlen(crash_info), &written, __sys_call_error);

		uCloseFile(r_fh, NULL);

		// if it is the last crash then delete record from config table
		if (point_info->try_numbers == 0)
		{
			free(it->second);
			fname_cfg_table.erase(it);
		}

		// serialize current config info to a rcvstate.tmp file to load it after recovery
		rcvSerializeInfoToFile("/data/rcvstate.tmp");

		// do abnormal exit (recovery crash)
		_exit(1);
    }
}

// read recovery config in /data/rcvtest.cfg
void rcvReadTestCfg()
{
	char rcv_file_name[U_MAX_PATH], read_str[1024], func_name[256];
	int max_try;
	double crash_prob;
	FILE *fd;
	rcvPointInfo *rcvInfo;

	// init random seed
	srand((unsigned)time(NULL));

	// first, try to read saved state
	if (rcvLoadInfoFromFile("/data/rcvstate.tmp") == 0)
		return;
	
	// else, try to read global recovery testing config
	sprintf(rcv_file_name, "%s/data/rcvtest.cfg", SEDNA_DATA);

	d_printf2("\nrcv_test_cfg_file=%s\n", rcv_file_name);

	// open config file
	fd = fopen(rcv_file_name, "r");
	if (fd == NULL) return;

	// parse it until EOF	
	while (fgets(read_str, 1024, fd))     
		// ignore comments and not well-defined strings
		if (sscanf(read_str, "%s %lf %d\n", func_name, &crash_prob, &max_try) == 3 && 
				func_name[0] != '#' && (max_try > 0 || max_try == -1))
		{
			// allocate new info structure
			if ((rcvInfo = (rcvPointInfo *)malloc(sizeof(rcvPointInfo))) == NULL)
				throw SYSTEM_EXCEPTION("Cannot allocate memory!");

			rcvInfo->try_numbers = max_try;
			rcvInfo->prob = crash_prob;
			
			fname_cfg_table[func_name] = rcvInfo;
		}

	// if we've caught an error then complain
	if (!feof(fd))
		throw USER_EXCEPTION2(SE4044, "rcvtest.cfg");

	fclose(fd);
}
