/*
 * File:  llmgr_helpers.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "base.h"
#include "llmgr_core.h"
#include "uhdd.h"
#include "exceptions.h"

#include <stdio.h>
#include <stdlib.h>


logical_log_file_head llmgr_core::read_log_file_header(UFile file_dsc)
{
  int res;

  res = uSetFilePointer(
                   file_dsc,
                   0,
                   NULL,
                   U_FILE_BEGIN
                 );

  if (res == 0)
     throw USER_EXCEPTION2(SE4046, "logical log file");

  logical_log_file_head file_head;
  int already_read;

  res = uReadFile(
              ll_file_curr_dsc,
              &file_head,
              sizeof(logical_log_file_head),
              &already_read
             );

  if (res == 0 || sizeof(logical_log_file_head) != already_read)
     throw USER_EXCEPTION2(SE4044, "logical log file header");


  return file_head;
}

void llmgr_core::flush_last_commit_lsn(LONG_LSN &commit_lsn)
{
  //get tail log dsc;
  //set pointer to the begin of last file 
  set_file_pointer((commit_lsn/LOG_FILE_PORTION_SIZE)*LOG_FILE_PORTION_SIZE);

  int res;
  int written;
  int next_lsn = commit_lsn + COMMIT_LOG_RECORD_LEN; 

  char buf[sizeof(LONG_LSN) + sizeof(LONG_LSN)];
  memcpy(buf, &commit_lsn, sizeof(LONG_LSN));
  memcpy(buf+sizeof(LONG_LSN), &next_lsn, sizeof(LONG_LSN));

  //flush last commit lsn
  res = uWriteFile(ll_file_curr_dsc,
                   buf,
                   2*sizeof(LONG_LSN),
                   &written
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

  res = uWriteFile(ll_file_curr_dsc,
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

  addr = lsn - base_addr;

  int portions = (int)(addr / LOG_FILE_PORTION_SIZE);
  int rmndr = addr - portions*LOG_FILE_PORTION_SIZE;
  logical_log_sh_mem_head *mem_head = (logical_log_sh_mem_head*)shared_mem;

  int log_file_number = mem_head->ll_files_arr[portions]; 
  
  ll_file_curr_dsc = get_log_file_descriptor(log_file_number);//this function open file if it was not opened before
  
  int res;

  res = uSetFilePointer(
                    ll_file_curr_dsc,
                    rmndr,
                    NULL,
                    U_FILE_BEGIN
                  );

  if (res == 0)
     throw SYSTEM_EXCEPTION("Can't set file pointer for logical log file");

}


const char* llmgr_core::get_record_from_disk(LONG_LSN& lsn)
{
  if (lsn == NULL_LSN) return NULL;

  set_file_pointer(lsn);
  
  int res;
  int bytes_read;

  res = uReadFile(
               ll_file_curr_dsc,
               small_read_buf,
               sizeof(small_read_buf),
               &bytes_read
             );

  if (res == 0)
     throw USER_EXCEPTION2(SE4044, "logical log file");

  logical_log_head* log_head;

  log_head = (logical_log_head*)small_read_buf;
  int rec_len = log_head->body_len + sizeof(logical_log_head);

  if (rec_len > bytes_read && bytes_read < sizeof(small_read_buf))
     throw USER_EXCEPTION(SE4154);

  if (rec_len > sizeof(small_read_buf))
  {//not full record in buffer
     large_read_buf = new char[rec_len];
     set_file_pointer(lsn);
  
     res = uReadFile(
                 ll_file_curr_dsc,
                 large_read_buf,
                 rec_len,
                 &bytes_read
             );  

     if( res == 0 || bytes_read != rec_len)
       throw USER_EXCEPTION2(SE4044, "logical log file");

     return large_read_buf;
  }
  else
     return small_read_buf;
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


    if (len <= sizeof(small_read_buf) ) 
    {
       memcpy(small_read_buf, mem_beg + rec_beg_offs, first_portion);
       memcpy(small_read_buf + first_portion, mem_beg + sizeof(logical_log_sh_mem_head), second_portion);
       return small_read_buf;
    }
    else
    {
       large_read_buf = new char[len];
       memcpy(large_read_buf, mem_beg + rec_beg_offs, first_portion);
       memcpy(large_read_buf + first_portion, mem_beg + sizeof(logical_log_sh_mem_head), second_portion);
       return large_read_buf;
    }
  }
    
     
}

void llmgr_core::delete_large_read_buf()
{
   if (large_read_buf != NULL)
   {
      delete [] large_read_buf;
      large_read_buf = NULL;
   }
}

void llmgr_core::extend_logical_log(bool sync)
{
  ll_log_lock(sync);

  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;
  UFile new_logical_log_dsc; 


  if (mem_head->ll_free_files_num <= 0)
     throw SYSTEM_EXCEPTION("Too long logical log");

  int new_file_number = mem_head->ll_free_files_num - 1; 
  string new_log_name = db_files_path + db_name + "." + 
                        mem_head->ll_free_files_arr[new_file_number] + "llog";

  //get header of previous file
  UFile dsc = get_log_file_descriptor(mem_head->ll_files_arr[mem_head->ll_files_num - 1]);
  logical_log_file_head prev_file_head = read_log_file_header(dsc);

  new_logical_log_dsc = create_logical_log(new_log_name.c_str(),
                                           prev_file_head.valid_number,
                                           mem_head->ll_files_arr[mem_head->ll_files_num - 1],//prev file number
                                           prev_file_head.last_commit_lsn,
                                           prev_file_head.next_lsn   
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
  typedef map <int, log_file_dsc>::iterator it, it2;


#ifdef _WIN32

  char buf[4096];
  char buf2[4096];
  char *cur_dir;
  cur_dir  = uGetCurrentWorkingDirectory(buf, 4096);


  if (uChangeWorkingDirectory(db_files_path.c_str()) != 0 )
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
     log_number.erase(log_number.end() - 4, log_number.end() -1);
     number = atoi(log_number.c_str());
     d_printf3("log_number=%s, %d\n", log_number.c_str(), number);


     descr = uOpenFile(
                      log_file.name,
                      U_SHARE_READ | U_SHARE_WRITE,
                      U_READ_WRITE,
                      U_WRITE_THROUGH 
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

  if (uChangeWorkingDirectory(cur_dir) != 0 )
     throw USER_EXCEPTION(SE4604); 
#else
  DIR *dir;
  struct dirent* dent;

  dir = opendir(db_file_path.c_str());

  if (dir == NULL)
     throw USER_EXCEPTION(SE4604); 


  string ext;
  string is_llog;
  while ( NULL != (dent = readdir (dir)) )
  {

     is_llog = (log_number = dent->d_name);

     if ( is_llog.substr(is_llog.size()-3, 4) != "llog") continue;


     log_number = log_number.erase(0, db_name.size() + 1);
     log_number.erase(log_number.end() - 4, log_number.end() -1);
     number = atoi(log_number.c_str());
     d_printf3("log_number=%s, %d\n", log_number.c_str(), number);


     descr = uOpenFile(
                      dent->d_name,
                      U_SHARE_READ | U_SHARE_WRITE,
                      U_READ_WRITE,
                      U_WRITE_THROUGH 
                     );

     if ( descr == U_INVALID_FD )
         throw USER_EXCEPTION2(SE4042, log_file.name);

     file_dsc.dsc = descr;
     file_dsc.name_number = number;

     file_head = read_log_file_header(descr);

     tmp_map.insert(dsc_pair(file_head.prev_file_number, file_dsc));
  }

  if (0 != closedir(dir))
     throw USER_EXCEPTION2(SE4054, cfg_files_dir.c_str());
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
      res = uCloseFile(ll_open_files[i].dsc);
  
      if (res == 0)
        throw USER_EXCEPTION2(SE4043, "logical log file");
  }
}

UFile llmgr_core::get_log_file_descriptor(int log_file_number)
{
  //firstly search on the top of vector 
  if (ll_open_files.back().name_number == log_file_number)
     return ll_open_files.back().dsc;

  //search is file opened
  int i,j;
  vector<log_file_dsc>::iterator it;
  for (i = ll_open_files.size() - 2; i>=0; i++)
  {
    if (ll_open_files[i].name_number == log_file_number)
    {//find case
       ll_open_files.push_back(ll_open_files[i]);//put on the top
       //eliminate duplicate
       it = ll_open_files.begin()
       for (j= 0; j<i; j++) it++;
       ll_open_files.erase(it);
       return ll_open_files.back().dsc;
    }
  }

  //log file dsc not found
  UFile dsc;
  std::string log_file_name = db_files_path + db_name + "." + _itoa(log_file_number, buf, 10) + "llog";

  dsc = uOpenFile(log_file_name.c_str(),
                  U_SHARE_READ | U_SHARE_WRITE,
                  U_READ_WRITE,
                  U_WRITE_THROUGH 
                 );

  if ( dsr == U_INVALID_FD )
      throw USER_EXCEPTION2(SE4042, log_file_name.c_str());
}

