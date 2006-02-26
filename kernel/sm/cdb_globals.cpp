/*
 * File:  cdb_globals.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <string>

#include "base.h"
#include "cdb_globals.h"
#include "sm_globals.h"
#include "utils.h"
#include "uprocess.h"
#include "exceptions.h"
#include "uhdd.h"
#include "usecurity.h"
#include "version.h"


using namespace std;

int _cdb_s_help_ = 0;
int _cdb_l_help_ = 0;
int _cdb_version_ = 0;

int _data_file_max_size_ = 2048;
int _tmp_file_max_size_ = 2048;
int _data_file_extending_portion_ = 100;
int _tmp_file_extending_portion_ = 100;
int _data_file_initial_size_ = 10;
int _tmp_file_initial_size_ = 10;
int _persistent_heap_size_ = 10;
int _phys_log_size_ = 100;
int _phys_log_ext_portion_ = 10;
int _phys_log_buf_size_ = 1;

int _bufs_num_ =1600;
int _max_trs_num_ =10;
char _db_name_[MAX_ARGS_LENGTH];


const size_t cdb_narg = 13;

arg_rec cdb_argtable[] =
{
{"--help",                       NULL,  arg_lit, &_cdb_l_help_,                 "0",   "\t\t\tdisplay this help and exit"},
{"-help",                        NULL,  arg_lit, &_cdb_s_help_,                 "0",   "\t\t\t\tdisplay this help and exit"},
{"-version",                     NULL,  arg_lit, &_cdb_version_,                  "0",   "\t\t\tdisplay product version and exit"},
{"-data-file-max-size",          " Mbs", arg_int, &_data_file_max_size_,         "2147483647", "\tthe max size of data file (in Mb), infinite size\t\t\t\tby default"},
{"-tmp-file-max-size",           " Mbs", arg_int, &_tmp_file_max_size_,          "2147483647","\tthe max size of tmp file (in Mb), infinite size\n\t\t\t\tby default"},
{"-data-file-ext-portion", " Mbs", arg_int, &_data_file_extending_portion_,"100", "\tthe data file extending portion size (in Mb), \n\t\t\t\tdefault 100Mb"},
{"-tmp-file-ext-portion",  " Mbs", arg_int, &_tmp_file_extending_portion_, "100", "\tthe tmp file extending portion size (in Mb),\n\t\t\t\tdefault 100Mb"},
{"-data-file-init-size",      " Mbs", arg_int, &_data_file_initial_size_,     "100",  "\tthe data file initial size (in Mb),\n\t\t\t\tdefault 100Mb"},
{"-tmp-file-init-size",       " Mbs", arg_int, &_tmp_file_initial_size_,       "100",  "\tthe tmp file initial size (in Mb),\n\t\t\t\tdefault 100Mb"},
{"-persistent-heap-size",       " Mbs", arg_int, &_persistent_heap_size_,        "10",  "\tthe persistent heap size (in Mb),\n\t\t\t\tdefault 10Mb"},
{"-bufs-num",                    " N",   arg_int, &_bufs_num_,                   "1600","\t\t\tthe number of buffers in main memory,\n\t\t\t\tdefault 1600"},
{"-max-trs-num",                 " N",   arg_int, &_max_trs_num_,                "10",  "\t\tthe number of concurrent micro transactions\n\t\t\t\tover database, default 10"},
/*
{"-phys-log-init-size",         " Mbs", arg_int, &_phys_log_size_,                  "10", "\tthe physical log file initial size (in Mb),\n\t\t\t\tdefault 10Mb" },
{"-phys-log-ext-portion",  " Mbs",  arg_int, &_phys_log_ext_portion_,                 "10", "\tthe physical log file extending portion size \n\t\t\t\t(in Mb), default 10Mb"},
*/
{NULL,                     "\ndb_name", arg_str, _db_name_,               "???", "\t\tthe name of the database to be created"}
};

void print_cdb_usage()
{
   throw USER_SOFT_EXCEPTION(string("Usage: se_cdb [options] dbname\n\n") +
                               string("options:\n") + string(arg_glossary(cdb_argtable, cdb_narg, "  ")) + string("\n"));

}

void setup_cdb_globals(int argc, 
                      char** argv,
                      __int64 &data_file_max_size,
                      __int64 &tmp_file_max_size,
                      int &data_file_extending_portion,
                      int &tmp_file_extending_portion,
                      int &data_file_initial_size,
                      int &tmp_file_initial_size,
                      int &persistent_heap_size,
                      int &phys_log_size,
                      int &phys_log_ext_portion
                     )
{
   int arg_scan_ret_val = 0; // 1 - parsed successful, 0 - there was errors
   char errmsg[1000];
   arg_scan_ret_val = arg_scanargv(argc, argv, cdb_argtable, cdb_narg, NULL, errmsg, NULL);

   if (_cdb_s_help_ == 1 || _cdb_l_help_ == 1)
      print_cdb_usage();

   if (_cdb_version_ == 1)
   {
      print_version_and_copyright("Sedna Create Data Base Utility");
      throw USER_SOFT_EXCEPTION("");
   }

   if (arg_scan_ret_val == 0)
      throw USER_EXCEPTION2(SE4601, errmsg);


   data_file_max_size = (__int64)_data_file_max_size_ * 0x100000;
   tmp_file_max_size = (__int64)_tmp_file_max_size_ * 0x100000;
   data_file_extending_portion = _data_file_extending_portion_ * 0x10;
   tmp_file_extending_portion = _tmp_file_extending_portion_ * 0x10;
   data_file_initial_size = _data_file_initial_size_ * 0x10;   
   tmp_file_initial_size = _tmp_file_initial_size_ * 0x10;
   persistent_heap_size = _persistent_heap_size_ * 0x100000;
   bufs_num = _bufs_num_;
   max_trs_num = _max_trs_num_;
   phys_log_size = _phys_log_size_ * 0x100000;
   phys_log_ext_portion = _phys_log_ext_portion_ * 0x100000;


   db_name = new char[strlen(_db_name_)+1];
   strcpy (db_name, _db_name_);

   if (strcmp(db_name, "???") == 0)
      throw USER_EXCEPTION2(SE4601, "The name of the database must be specified");

   //d_printf2("SEDNA_DATA=%s\n", SEDNA_DATA);
   string data_files_path = string(SEDNA_DATA) + "/data/" + _db_name_ + "_files/";
   db_files_path  =new char [data_files_path.length()+1];
   strcpy(db_files_path, data_files_path.c_str());

}


void create_cfg_file(char *db_name,
                     int max_trs_num,
                     int bufs_num,
                     int phys_log_size,
                     int phys_log_ext_portion
                    )
{
   UFile cfg_file_handle;
   USECURITY_ATTRIBUTES *sa;
   
   string cfg_files_path = string(SEDNA_DATA)+"/cfg";
   string cfg_file_name  = cfg_files_path + "/" + string(db_name) + "_cfg.xml";


   if (uMkDir(cfg_files_path.c_str()) == 0)
      throw USER_EXCEPTION2(SE4300, cfg_files_path.c_str());

   if(uCreateSA(U_ALL_ACCESS, &sa)!=0) throw USER_EXCEPTION(SE3060);
   cfg_file_handle = uCreateFile(cfg_file_name.c_str(),
  	                            0,
  	                            U_READ_WRITE,
  	                            U_WRITE_THROUGH,
  	                            sa);
   if (cfg_file_handle == U_INVALID_FD)
      throw USER_EXCEPTION2(SE4040, "database configuration file");

   uReleaseSA(sa);
	  
//   if ( (cfg_file=fopen(cfg_file_name.c_str(), "w")) == NULL)
//      throw CharismaException(string("???: Can't create file ") + cfg_file_name);

   string cfg_file_content;

   //cfg_file_content  = "<!-- Configuration file for sm -->\n";
   cfg_file_content =  "<?xml version=\"1.0\" standalone=\"yes\"?>\n";
   //cfg_file_content += "<dbs>\n";
   cfg_file_content += "<db>\n";
   cfg_file_content += "   <name>" + string(db_name) + string("</name>\n");
   cfg_file_content += "   <bufs_num>" + int2string(bufs_num) + string("</bufs_num>\n");
   cfg_file_content += "   <max_trs_num>" + int2string(max_trs_num) + string("</max_trs_num>\n");

   cfg_file_content += "   <init_phys_log_size>" + int2string(phys_log_size) + string("</init_phys_log_size>\n");
   cfg_file_content += "   <phys_log_ext_portion>" + int2string(phys_log_ext_portion) + string("</phys_log_ext_portion>\n");

   cfg_file_content += "</db>\n";
   //cfg_file_content += "</dbs>";

   int res;
   int nbytes_written;
   
   res = uWriteFile(cfg_file_handle,
                    cfg_file_content.c_str(),
                    cfg_file_content.size(),
                    &nbytes_written
                    );
   
  if ( res == 0 || nbytes_written != cfg_file_content.size())
      throw USER_EXCEPTION2(SE4045,  cfg_file_name.c_str());

   if (uCloseFile(cfg_file_handle) == 0)
      throw USER_EXCEPTION2(SE4043, "configuration file");
   return;

}

void create_data_directory()
{
   string data_files_path = string(SEDNA_DATA) + string("/data/");

   if (uMkDir(data_files_path.c_str()) == 0)
      throw USER_EXCEPTION2(SE4300, data_files_path.c_str());

   data_files_path += ( string(db_name) + "_files" );

   if (uMkDir(data_files_path.c_str()) == 0)
      throw USER_EXCEPTION2(SE4300, data_files_path.c_str());
}


