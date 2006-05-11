/*
 * File:  llmgr_helpers.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "sedna.h"
#include "base.h"
#include "llmgr_core.h"
#include "uhdd.h"
#include "cdb_globals.h"
#include "uutils.h"
#include "d_printf.h"

#include <string>
#include <map>

#ifdef _WIN32
#include <io.h>
#else
#include <sys/types.h>
#include <dirent.h>
#endif


using namespace std;


logical_log_file_head llmgr_core::read_log_file_header(UFile file_dsc)
{
  int res;

  res = uSetFilePointer(
                   file_dsc,
                   0,
                   NULL,
                   U_FILE_BEGIN,
                   __sys_call_error
                 );

  if (res == 0)
     throw USER_EXCEPTION2(SE4046, "logical log file");

  logical_log_file_head file_head;
  int already_read;

  res = uReadFile(
              file_dsc,
              &file_head,
              sizeof(logical_log_file_head),
              &already_read,
               __sys_call_error
             );

  if (res == 0 || sizeof(logical_log_file_head) != already_read)
     throw USER_EXCEPTION2(SE4044, "logical log file header");


  return file_head;
}

void llmgr_core::flush_last_commit_lsn(LONG_LSN &commit_lsn)
{
  //get tail log dsc;
  //set pointer to the begin of last file 
  LONG_LSN lsn = (commit_lsn/LOG_FILE_PORTION_SIZE)*LOG_FILE_PORTION_SIZE;
  set_file_pointer(lsn);

  int res;
  int written;
  int next_lsn = commit_lsn + COMMIT_LOG_RECORD_LEN; 

  char buf[sizeof(LONG_LSN) + sizeof(LONG_LSN)];
  memcpy(buf, &commit_lsn, sizeof(LONG_LSN));
  memcpy(buf+sizeof(LONG_LSN), &next_lsn, sizeof(LONG_LSN));

  //flush last commit lsn
  res = uWriteFile(ll_curr_file_dsc,
                   buf,
                   2*sizeof(LONG_LSN),
                   &written,
                   __sys_call_error
                    );
  if (res == 0 || written != 2*sizeof(LONG_LSN))
     throw SYSTEM_EXCEPTION("Can't write to logical log file last commit lsn");
}

/*
void llmgr_core::flush_last_checkpoint_lsn(LONG_LSN &checkpoint_lsn)
{
  set_file_pointer(2*sizeof(LONG_LSN));

  int res;
  int written;

  res = uWriteFile(ll_curr_file_dsc,
                   &checkpoint_lsn,
                   sizeof(LONG_LSN),
                   &written
                  );
  if (res == 0 || written != sizeof(LONG_LSN))
     throw SYSTEM_EXCEPTION("Can't write to logical log file last checkpoint lsn");
}
*/

void llmgr_core::set_file_pointer(LONG_LSN &lsn)
{

  //calculate the log file in which this recird stored

  __int64 addr;
  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;

  addr = lsn - mem_head->base_addr;

  int portions = (int)(addr / LOG_FILE_PORTION_SIZE);
  int rmndr = addr - portions*LOG_FILE_PORTION_SIZE;

  int log_file_number = mem_head->ll_files_arr[portions]; 
  
  ll_curr_file_dsc = get_log_file_descriptor(log_file_number);//this function open file if it was not opened before
  
  int res;

  res = uSetFilePointer(
                    ll_curr_file_dsc,
                    rmndr,
                    NULL,
                    U_FILE_BEGIN,
                    __sys_call_error
                  );

  if (res == 0)
  {
//     d_printf2("Error=%d\n", GetLastError());
     throw SYSTEM_EXCEPTION("Can't set file pointer for logical log file");
  }

}


const char* llmgr_core::get_record_from_disk(LONG_LSN& lsn)
{
  if (lsn == NULL_LSN) return NULL;

  set_file_pointer(lsn);
  
  int res;
  int bytes_read;

  res = uReadFile(
               ll_curr_file_dsc,
               read_buf,
               LOGICAL_LOG_UNDO_READ_PORTION,
               &bytes_read,
               __sys_call_error
             );

  if (res == 0)
     throw USER_EXCEPTION2(SE4044, "logical log file");

  logical_log_head* log_head;

  log_head = (logical_log_head*)read_buf;
  int rec_len = log_head->body_len + sizeof(logical_log_head);

  if (rec_len > bytes_read && bytes_read < LOGICAL_LOG_UNDO_READ_PORTION)
     throw USER_EXCEPTION(SE4154);

  if (rec_len > LOGICAL_LOG_UNDO_READ_PORTION)
  {//not full record in buffer
     if (read_buf_size < rec_len)
     {
       delete [] read_buf;
       read_buf = new char[rec_len];
       read_buf_size = rec_len;
     }

     set_file_pointer(lsn);
  
     res = uReadFile(
                 ll_curr_file_dsc,
                 read_buf,
                 rec_len,
                 &bytes_read,
                 __sys_call_error
             );  

     if( res == 0 || bytes_read != rec_len)
       throw USER_EXCEPTION2(SE4044, "logical log file");

     return read_buf;
  }
  else
     return read_buf;
}


int llmgr_core::get_record_length(const void* rec)
{
  if (rec == NULL) return 0;
  else return  sizeof(logical_log_head) + ((logical_log_head*)rec)->body_len;
}


const char* llmgr_core::get_record_from_shared_memory(int end_offs, int len)
{
  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;
  char* mem_beg = (char*)shared_mem;

  if ((end_offs - sizeof(logical_log_sh_mem_head)) >= len)
     return mem_beg + end_offs - len;
  else
  {
    int second_portion = end_offs - sizeof(logical_log_sh_mem_head);
    int first_portion = len - second_portion;
    int rec_beg_offs =  mem_head->size - first_portion;


    if (len > read_buf_size)
    {
       delete [] read_buf;
       read_buf = new char[len]; 
       read_buf_size = len;
    }

    memcpy(read_buf, mem_beg + rec_beg_offs, first_portion);
    memcpy(read_buf + first_portion, mem_beg + sizeof(logical_log_sh_mem_head), second_portion);
    return read_buf;
  }
    
     
}

void llmgr_core::extend_logical_log(bool sync)
{
  ll_log_lock(sync);

  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;
  UFile new_logical_log_dsc; 


  if (mem_head->ll_free_files_num <= 0)
     throw SYSTEM_EXCEPTION("Too long logical log");

  int new_file_number = mem_head->ll_free_files_arr[mem_head->ll_free_files_num - 1]; 
  char buf[20];
  string new_log_name = db_files_path + db_name +string(".") + 
                        string(u_itoa(new_file_number, buf, 10)) + "llog";

  //get header of previous file
  UFile dsc = get_log_file_descriptor(mem_head->ll_files_arr[mem_head->ll_files_num - 1]);
  logical_log_file_head prev_file_head = read_log_file_header(dsc);

  new_logical_log_dsc = create_logical_log(new_log_name.c_str(),
                                           prev_file_head.valid_number,
                                           prev_file_head.base_addr,
                                           mem_head->ll_files_arr[mem_head->ll_files_num - 1],//prev file number
                                           prev_file_head.last_commit_lsn,
                                           prev_file_head.next_lsn,
                                           false                                              
                                          );

  //modify shared memory
  mem_head->ll_free_files_num--;
  mem_head->ll_files_arr[mem_head->ll_files_num] = new_file_number;
  mem_head->ll_files_num++;

  mem_head->next_lsn = (mem_head->next_lsn/LOG_FILE_PORTION_SIZE + 1)*LOG_FILE_PORTION_SIZE + 
                       sizeof(logical_log_file_head); 

  mem_head->next_durable_lsn = mem_head->next_lsn;

  //insert new descriptor in vector
  log_file_dsc file_dsc;
  file_dsc.dsc = new_logical_log_dsc;
  file_dsc.name_number = new_file_number;
  ll_open_files.push_back(file_dsc);

  if (mem_head->ll_files_num > MAX_LOG_FILE_PORTIONS_NUMBER_WITHOUT_CHECKPOINTS)
     activate_checkpoint();//activate checkpoint to trancate uneccary log files
  
  ll_log_unlock(sync);
}

void llmgr_core::open_all_log_files()
{

  string log_number;  
  int number;

  
  log_file_dsc file_dsc;
  UFile descr;
  logical_log_file_head file_head;
  map<int, log_file_dsc> tmp_map;
  typedef pair <int, log_file_dsc> dsc_pair;
  map<int, log_file_dsc>::iterator it, it2;


#ifdef _WIN32

  char buf[4096];
  char buf2[4096];
  char *cur_dir;
  cur_dir  = uGetCurrentWorkingDirectory(buf, 4096, __sys_call_error);


  if (uChangeWorkingDirectory(db_files_path.c_str(), __sys_call_error) != 0 )
     throw USER_EXCEPTION(SE4604); 
  

  struct _finddata_t log_file;
  long dsc;

  //find first cfg file

  if ( (dsc = _findfirst("*llog", &log_file)) == -1L)
     throw USER_EXCEPTION2(SE4044, "logical log file");

  do 
  {
     log_number = log_file.name;
     log_number = log_number.erase(0, db_name.size() + 1);
     log_number.erase(log_number.end() - 4, log_number.end());
     number = atoi(log_number.c_str());
     d_printf3("log_number=%s, %d\n", log_number.c_str(), number);


     descr = uOpenFile(
                      log_file.name,
                      U_SHARE_READ | U_SHARE_WRITE,
                      U_READ_WRITE,
                      U_WRITE_THROUGH,
                      __sys_call_error 
                     );

     if ( descr == U_INVALID_FD )
         throw USER_EXCEPTION2(SE4042, log_file.name);

     file_dsc.dsc = descr;
     file_dsc.name_number = number;

     file_head = read_log_file_header(descr);

     tmp_map.insert(dsc_pair(file_head.prev_file_number, file_dsc));

//     ll_open_files.push_back(file_dsc);

  } while(_findnext(dsc, &log_file) == 0);

    
  _findclose(dsc);

  if (uChangeWorkingDirectory(cur_dir, __sys_call_error) != 0 )
     throw USER_EXCEPTION(SE4604); 
#else
  DIR *dir;
  struct dirent* dent;

  dir = opendir(db_files_path.c_str());

  if (dir == NULL)
     throw USER_EXCEPTION(SE4604); 

  string ext;
  string is_llog;
  while ( NULL != (dent = readdir (dir)) )
  {
     is_llog = (log_number = dent->d_name);
     if (is_llog.size() < 7) continue;
//d_printf2("IS_LLOG=%s\n", is_llog.c_str());

     if ( is_llog.substr(is_llog.size()-4, 4) != "llog") continue;

     log_number = log_number.erase(0, db_name.size() + 1);
//d_printf2("7log_number =%s\n", log_number.c_str());
     log_number.erase(log_number.end() - 4, log_number.end());
//d_printf2("8log_number=%s\n", log_number.c_str());
     number = atoi(log_number.c_str());
//     d_printf3("log_number=%s, %d\n", log_number.c_str(), number);


     descr = uOpenFile(
                      (db_files_path + dent->d_name).c_str(),
                      U_SHARE_READ | U_SHARE_WRITE,
                      U_READ_WRITE,
                      U_WRITE_THROUGH,
                      __sys_call_error 
                     );

     if ( descr == U_INVALID_FD )
         throw USER_EXCEPTION2(SE4042, dent->d_name);

//d_printf2("Opened log file=%s\n", dent->d_name);
     file_dsc.dsc = descr;
     file_dsc.name_number = number;

     file_head = read_log_file_header(descr);

     tmp_map.insert(dsc_pair(file_head.prev_file_number, file_dsc));
  }

  if (0 != closedir(dir))
     throw USER_EXCEPTION2(SE4054, db_files_path.c_str());
#endif  

  //sort tmp map and fill ll_open_files

  //find first log file
  bool exist_prev;
  for (it = tmp_map.begin(); it != tmp_map.end(); it++)
  {
    exist_prev = false;
    for (it2 = tmp_map.begin(); it2 != tmp_map.end(); it2++)
    {
       if (it->first == it2->second.name_number)
       {
         exist_prev = true;
         break;
       }
    }   

    if (!exist_prev) break;
  }

  if (it == tmp_map.end()) //firts element not found
     throw SYSTEM_EXCEPTION("Incorrect logic in logical log");


  while (it != tmp_map.end())
  {
    ll_open_files.push_back(it->second);
    it = tmp_map.find(it->second.name_number); 
  }
}


void llmgr_core::close_all_log_files()
{
  int i, res;
  for (i= 0; i < ll_open_files.size(); i++)
  {
      res = uCloseFile(ll_open_files[i].dsc, __sys_call_error);
  
      if (res == 0)
        throw USER_EXCEPTION2(SE4043, "logical log file");
  }

  ll_open_files.clear();
}

UFile llmgr_core::get_log_file_descriptor(int log_file_number)
{
  //firstly search on the top of vector 
  if (!ll_open_files.empty())
  {
     if (ll_open_files.back().name_number == log_file_number)
        return ll_open_files.back().dsc;

     //search is file opened
     int i,j;
     vector<log_file_dsc>::iterator it;
     for (i = ll_open_files.size() - 2; i>=0; i--)
     {
       if (ll_open_files[i].name_number == log_file_number)
       {//find case
          ll_open_files.push_back(ll_open_files[i]);//put on the top
          //eliminate duplicate
          it = ll_open_files.begin();
          for (j= 0; j<i; j++) it++;
          ll_open_files.erase(it);
          return ll_open_files.back().dsc;
       }
     }
  }

  //log file dsc not found
  UFile dsc;
  char buf[20];
  std::string log_file_name = db_files_path + db_name + "." + u_itoa(log_file_number, buf, 10) + "llog";

  dsc = uOpenFile(log_file_name.c_str(),
                  U_SHARE_READ | U_SHARE_WRITE,
                  U_READ_WRITE,
                  U_WRITE_THROUGH,
                  __sys_call_error 
                 );

  if ( dsc == U_INVALID_FD )
  {
//    d_printf2("Error = %d\n", GetLastError());
    throw USER_EXCEPTION2(SE4042, log_file_name.c_str());
  }

  log_file_dsc file_dsc;
  file_dsc.dsc = dsc;
  file_dsc.name_number = log_file_number;
  ll_open_files.push_back(file_dsc);


  return dsc;
}



//log_file_name is a full path to file to be created
//prev_log_file_name is a previous log file (only the name without path)
UFile create_logical_log(const char* log_file_name,
                         int valid_file_number,
                         LONG_LSN base_addr,
                         int _prev_file_number_,
                         LONG_LSN commit_lsn,
                         LONG_LSN next_after_commit_lsn,
						 bool is_close_file
                        )
{
#ifdef LOGICAL_LOG

  UFile logical_log_dsc;
  USECURITY_ATTRIBUTES *sa;

//  string logical_log_file_name = string(db_files_path) + string(db_name) + ".llog";
  if(uCreateSA(&sa, U_SEDNA_DEFAULT_ACCESS_PERMISSIONS_MASK, 0, __sys_call_error)!=0) throw USER_EXCEPTION(SE3060);

  //create phys log file
  logical_log_dsc = uCreateFile(
                            log_file_name,
                            U_SHARE_READ | U_SHARE_WRITE,
                            U_READ_WRITE,
                            U_WRITE_THROUGH,
                            sa,
                            __sys_call_error
                           );

  if (logical_log_dsc == U_INVALID_FD)
     throw USER_EXCEPTION2(SE4040, "logical log file");
  
  if(uReleaseSA(sa, __sys_call_error)!=0) throw USER_EXCEPTION(SE3063);

  logical_log_file_head ll_head;

  ll_head.last_commit_lsn = commit_lsn;
  ll_head.next_lsn = next_after_commit_lsn;
  ll_head.prev_file_number = _prev_file_number_; 
  ll_head.base_addr = base_addr;
  ll_head.valid_number = valid_file_number;

  
 

  int nbytes_written;

  int res;
  res = uWriteFile(
               logical_log_dsc,
               &ll_head,
               sizeof(logical_log_file_head),
               &nbytes_written,
               __sys_call_error 
              );


  if ( res == 0 || nbytes_written != sizeof(logical_log_file_head))
     throw USER_EXCEPTION2(SE4045, "logical log file");

  if (is_close_file)
     if (uCloseFile(logical_log_dsc, __sys_call_error) == 0)
        throw USER_EXCEPTION2(SE4043, "logical log file");

  return logical_log_dsc;
#endif
}
