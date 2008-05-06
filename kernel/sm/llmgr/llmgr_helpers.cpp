/*
 * File:  llmgr_helpers.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */


#include "common/sedna.h"
#include "common/base.h"
#include "sm/llmgr/llmgr_core.h"
#include "common/u/uhdd.h"
#include "sm/cdb_globals.h"
#include "common/u/uutils.h"
#include "common/errdbg/d_printf.h"

#include "sm/bufmgr/bm_core.h"

#include <string>
#include <map>

#ifdef _WIN32
#include <io.h>
#else
#include <sys/types.h>
#include <dirent.h>
#endif


using namespace std;

__int64 get_last_archived_log_file_number()
{
	return hbLastFileNum;
}

__int64  ll_get_prev_archived_log_file_number(__int64 lnumber)
{
  	ll_log_lock(true);

  	logical_log_sh_mem_head *mem_head = (logical_log_sh_mem_head*)shared_mem;

	for (int i = 0; i < ll_files_num; i++)
		if (mem_head->ll_files_arr[i] == lnumber) break;

	if (i == ll_files_num || i == 0) 
	{
	  	ll_log_unlock(true);
		return -1;
	}
	
	__int64 res = mem_head->ll_files_arr[i - 1];

  	ll_log_unlock(true);

	return res;
}

void llmgr_core::logArchive()
{
     ll_log_flush(false);
     extend_logical_log(false);
}

void llmgr_core::log_hotbackup(hb_state state)
{
  	ll_log_lock(true);
    
    hbStatus = state;

    if (hbStatus == HB_ARCHIVELOG)
    	logArchive();

    if (hbStatus == HB_END)
	    hbLastFileNum = -1;

    hbStatus = HB_END;

  	ll_log_unlock(true);
}

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

// this function writes is_stopped_correctly field in the file_head
// we use this field to determine if the database was stopped correctly
void llmgr_core::writeIsStoppedCorrectly(bool is_stopped_correctly)
{
  logical_log_sh_mem_head *mem_head = (logical_log_sh_mem_head*)shared_mem;

  logical_log_file_head file_head =
              read_log_file_header(get_log_file_descriptor(mem_head->ll_files_arr[mem_head->ll_files_num - 1]));

  file_head.is_stopped_successfully = is_stopped_correctly;

  //get tail log dsc;
  //set pointer to the begin of last file 
  LONG_LSN lsn = ((mem_head->last_lsn)/LOG_FILE_PORTION_SIZE)*LOG_FILE_PORTION_SIZE;
  set_file_pointer(lsn);

  int res;
  int written;

  RECOVERY_CRASH;

  res = uWriteFile(ll_curr_file_dsc,
//                   buf,
                   &file_head,
                   sizeof(logical_log_file_head),
                   &written,
                   __sys_call_error
                    );

  if (res == 0 || written != sizeof(logical_log_file_head))
     throw SYSTEM_EXCEPTION("Can't write file_head to logical log file");
}

void llmgr_core::flush_file_head(bool sync)
{

  ll_log_lock(sync);

  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;

  logical_log_file_head file_head =
              read_log_file_header(get_log_file_descriptor(mem_head->ll_files_arr[mem_head->ll_files_num - 1]));

  //get tail log dsc;
  //set pointer to the begin of last file 
  LONG_LSN lsn = ((mem_head->last_lsn)/LOG_FILE_PORTION_SIZE)*LOG_FILE_PORTION_SIZE;
  set_file_pointer(lsn);

  int res;
  int written;

  file_head.last_lsn = mem_head->last_lsn;
  file_head.next_lsn = mem_head->next_lsn;

  file_head.last_checkpoint_lsn = mem_head->last_checkpoint_lsn;
  file_head.last_chain_lsn = mem_head->last_chain_lsn;
  file_head.ts = mem_head->ts;

  RECOVERY_CRASH;

  res = uWriteFile(ll_curr_file_dsc,
//                   buf,
                   &file_head,
                   sizeof(logical_log_file_head),
                   &written,
                   __sys_call_error
                    );
  if (res == 0 || written != sizeof(logical_log_file_head))
     throw SYSTEM_EXCEPTION("Can't write file_head to logical log file");

  ll_log_unlock(sync);
}


void llmgr_core::flush_last_commit_lsn(LONG_LSN &commit_lsn)
{
  //get tail log dsc;
  //set pointer to the begin of last file 
  LONG_LSN lsn = (commit_lsn/LOG_FILE_PORTION_SIZE)*LOG_FILE_PORTION_SIZE;
  set_file_pointer(lsn);

  int res;
  int written;
  LONG_LSN next_lsn = commit_lsn + COMMIT_LOG_RECORD_LEN; 

  char buf[sizeof(LONG_LSN) + sizeof(LONG_LSN)];
  memcpy(buf, &commit_lsn, sizeof(LONG_LSN));
  memcpy(buf+sizeof(LONG_LSN), &next_lsn, sizeof(LONG_LSN));

  //flush last commit lsn
  RECOVERY_CRASH;

  res = uWriteFile(ll_curr_file_dsc,
                   buf,
                   2*sizeof(LONG_LSN),
                   &written,
                   __sys_call_error
                    );
  if (res == 0 || written != 2*sizeof(LONG_LSN))
     throw SYSTEM_EXCEPTION("Can't write to logical log file last commit lsn");
}

void llmgr_core::flush_file_head_lsn(LONG_LSN llsn, LONG_LSN nlsn, LONG_LSN lclsn, bool sync)
{

  ll_log_lock(sync);

  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;

  logical_log_file_head file_head =
              read_log_file_header(get_log_file_descriptor(mem_head->ll_files_arr[mem_head->ll_files_num - 1]));

  //get tail log dsc;
  //set pointer to the begin of last file 
  LONG_LSN lsn = ((mem_head->last_lsn)/LOG_FILE_PORTION_SIZE)*LOG_FILE_PORTION_SIZE;
  set_file_pointer(lsn);

  int res;
  int written;
//  LONG_LSN next_lsn = commit_lsn + COMMIT_LOG_RECORD_LEN; 

  file_head.last_lsn = llsn;
  file_head.next_lsn = nlsn;

  file_head.last_checkpoint_lsn = mem_head->last_checkpoint_lsn;
  file_head.last_chain_lsn = lclsn;
  file_head.ts = mem_head->ts;

  RECOVERY_CRASH;

  res = uWriteFile(ll_curr_file_dsc,
//                   buf,
                   &file_head,
                   sizeof(logical_log_file_head),
                   &written,
                   __sys_call_error
                    );
  if (res == 0 || written != sizeof(logical_log_file_head))
     throw SYSTEM_EXCEPTION("Can't write file_head to logical log file");

  ll_log_unlock(sync);
}

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
       se_delete(read_buf);
       read_buf = se_new_cxt(TransactionContext) char[rec_len];
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
       se_delete(read_buf);
       read_buf = se_new_cxt(TransactionContext) char[len]; 
       read_buf_size = len;
    }

    memcpy(read_buf, mem_beg + rec_beg_offs, first_portion);
    memcpy(read_buf + first_portion, mem_beg + sizeof(logical_log_sh_mem_head), second_portion);
    return read_buf;
  }
    
     
}

// this function return the timestamp of the last persistent snapshot
TIMESTAMP llmgr_core::returnTimestampOfPersSnapshot(bool sync)
{
  ll_log_lock(sync);

  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;

  ll_log_unlock(sync);

  return mem_head->ts;
}

void llmgr_core::hbWriteFileHeader(bool hbFlag)
{
  logical_log_sh_mem_head *mem_head = (logical_log_sh_mem_head*)shared_mem;

  logical_log_file_head file_head =
              read_log_file_header(get_log_file_descriptor(mem_head->ll_files_arr[mem_head->ll_files_num - 1]));

  file_head.is_hot_backup = hbFlag;

  //get tail log dsc;
  //set pointer to the begin of last file 
  LONG_LSN lsn = ((mem_head->last_lsn)/LOG_FILE_PORTION_SIZE)*LOG_FILE_PORTION_SIZE;
  set_file_pointer(lsn);

  int res;
  int written;

  RECOVERY_CRASH;

  res = uWriteFile(ll_curr_file_dsc,
                   &file_head,
                   sizeof(logical_log_file_head),
                   &written,
                   __sys_call_error
                    );

  if (res == 0 || written != sizeof(logical_log_file_head))
     throw SYSTEM_EXCEPTION("Can't write file_head to logical log file");
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
                                           prev_file_head.last_lsn,
                                           prev_file_head.next_lsn,

  						                   prev_file_head.last_checkpoint_lsn, 
  						                   prev_file_head.last_chain_lsn, 
  						                   prev_file_head.ts,
//                                           prev_file_head.ph_cp_counter,
                                           
                                           false                                              
                                          );

  // modify previous header if archiving log
  if (hbStatus == HB_ARCHIVELOG)
  {
  		hbLastFileNum = mem_head->ll_files_arr[mem_head->ll_files_num - 1];
  		hbWriteFileHeader(true);
  }
  
  //modify shared memory
  mem_head->ll_free_files_num--;
  mem_head->ll_files_arr[mem_head->ll_files_num] = new_file_number;
  mem_head->ll_files_num++;

//This is not correct  mem_head->next_lsn = (mem_head->next_lsn/LOG_FILE_PORTION_SIZE + 1)*LOG_FILE_PORTION_SIZE + 
//                       sizeof(logical_log_file_head); 
  U_ASSERT(mem_head->base_addr + (mem_head->ll_files_num - 1)*LOG_FILE_PORTION_SIZE + sizeof(logical_log_file_head) >= 0);

  mem_head->next_lsn = mem_head->base_addr + (mem_head->ll_files_num - 1)*LOG_FILE_PORTION_SIZE +
                       sizeof(logical_log_file_head); 

  mem_head->next_durable_lsn = mem_head->next_lsn;

  //insert new descriptor in vector
  log_file_dsc file_dsc;
  file_dsc.dsc = new_logical_log_dsc;
  file_dsc.name_number = new_file_number;
  ll_open_files.push_back(file_dsc);

  if (mem_head->ll_files_num > MAX_LOG_FILE_PORTIONS_NUMBER_WITHOUT_CHECKPOINTS)
     activate_checkpoint(false);//activate checkpoint to truncate unnecessary log files

  ll_log_unlock(sync);
}

void llmgr_core::open_all_log_files(bool delete_old)
{

  string log_number;  
  int number;

  log_file_dsc file_dsc;
  UFile descr;
  logical_log_file_head file_head;
 
  typedef pair <int, log_file_dsc> dsc_pair;
  vector <dsc_pair> files_vec;
  
  vector <dsc_pair>::iterator it;

  vector <int> del_nums; // numbers of the corrupted files (zeroed file_head)
  int val_file_num = -1; // valid log file number

  LONG_LSN tlsn = NULL_LSN;
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

     // here we must delete all files that has been corrupted (create file without file_head) 
     // so we check it for zeroed header, which is invalid
     if (file_head.next_lsn == 0 || file_head.ts == 0 || file_head.sedna_db_version == 0)
     {
      	if (uCloseFile(descr, __sys_call_error) == 0)
        	throw USER_EXCEPTION2(SE4043, "logical log file");

     	del_nums.push_back(number);
     }
     else
     {
   	    if (file_head.next_lsn > tlsn)
     	{
     		tlsn = file_head.next_lsn;
     		val_file_num = file_head.valid_number;
     	}

	    files_vec.push_back(dsc_pair(file_head.prev_file_number, file_dsc));
	 }
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

     // here we must delete all files that has been corrupted (create file without file_head) 
     // so we check it for zeroed header, which is invalid
     if (file_head.next_lsn == 0 || file_head.ts == 0 || file_head.sedna_db_version == 0)
     {
      	if (uCloseFile(descr, __sys_call_error) == 0)
        	throw USER_EXCEPTION2(SE4043, "logical log file");

     	del_nums.push_back(number);
     }
     else
     {
   	    if (file_head.next_lsn > tlsn)
     	{
     		tlsn = file_head.next_lsn;
     		val_file_num = file_head.valid_number;
     	}

	    files_vec.push_back(dsc_pair(file_head.prev_file_number, file_dsc));
	 }
  }

  if (0 != closedir(dir))
     throw USER_EXCEPTION2(SE4054, db_files_path.c_str());
#endif  

  // delete corrupted files
  char buf3[20];
  string log_file_name;

  for (int i = 0; i < del_nums.size(); i++)
  {
  		log_file_name = db_files_path + db_name + "." + u_itoa(del_nums[i], buf3, 10) + "llog";
      	if (uDeleteFile(log_file_name.c_str(), __sys_call_error) == 0)
         	throw USER_EXCEPTION2(SE4041, log_file_name.c_str());
  }

/*
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
*/
  it = files_vec.begin();

  while (it != files_vec.end() && it->second.name_number != val_file_num) it++;

  if (it == files_vec.end()) //valid element not found
     throw SYSTEM_EXCEPTION("Incorrect logic in logical log: valid file not found");

  // go forward from valid file
  while (it != files_vec.end())
  {
    ll_open_files.push_back(it->second);

    int fnum = it->second.name_number;
    files_vec.erase(it); 
    
    it = files_vec.begin();
	while (it != files_vec.end() && it->first != fnum) it++;
  }

  // close and delete old files (this is possible, if old files weren't deleted at truncate due to failure)
  for (int i = 0; i < files_vec.size(); i++)
  {
      if (uCloseFile(files_vec[i].second.dsc, __sys_call_error) == 0)
        	throw USER_EXCEPTION2(SE4043, "logical log file");

	  if (delete_old)
	  {
  	  	log_file_name = db_files_path + db_name + "." + u_itoa(files_vec[i].second.name_number, buf3, 10) + "llog";
      	if (uDeleteFile(log_file_name.c_str(), __sys_call_error) == 0)
         	throw USER_EXCEPTION2(SE4041, log_file_name.c_str());
      }
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
  						 
  						 LONG_LSN last_checkpoint_lsn, 
  						 LONG_LSN last_chain_lsn, 
  						 TIMESTAMP ts,
//                         __int64 ph_cp_counter,

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

  ll_head.last_lsn = commit_lsn;
  ll_head.next_lsn = next_after_commit_lsn;
  ll_head.prev_file_number = _prev_file_number_; 
  ll_head.base_addr = base_addr;
  ll_head.valid_number = valid_file_number;

  ll_head.last_checkpoint_lsn = last_checkpoint_lsn;
  ll_head.last_chain_lsn = last_chain_lsn;
  ll_head.ts = ts;
  ll_head.is_stopped_successfully = false;
  ll_head.sedna_db_version = SEDNA_DATA_STRUCTURES_VER;
  ll_head.is_hot_backup = false;
  
  int nbytes_written;

  RECOVERY_CRASH;

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
#else
return 0;
#endif
}

LONG_LSN llmgr_core::ll_get_lsn_of_first_record_in_logical_log(transaction_id &trid, bool sync)
{
  ll_log_lock(sync);
   
  logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;
  LONG_LSN lsn = mem_head->t_tbl[trid].first_lsn;
  ll_log_unlock(sync);
  return lsn;	  
}

bool llmgr_core::find_redo_trn_cell(transaction_id trid,
                                    trns_redo_analysis_list& redo_list,
                                    LONG_LSN lsn,
                                    trn_cell_analysis_redo& redo_trn_cell/*out*/)
{
   trns_redo_analysis_list_iterator it;

   for (it = redo_list.begin(); it != redo_list.end(); it++)
   {
      if (it->trid == trid && it->trn_start_rcv_lsn <= lsn && it->trn_end_lsn >= lsn)
      {
         redo_trn_cell = *it;
         return true;
      }

      if (it->trn_start_rcv_lsn > lsn) return false;
        
   }
   return false;
}

bool llmgr_core::find_last_redo_trn_cell(transaction_id trid, trns_redo_analysis_list& redo_list, trn_cell_analysis_redo& redo_trn_cell/*out*/)
{
  trns_redo_analysis_list_reverse_iterator it;
  for (it = redo_list.rbegin(); it != redo_list.rend(); it++)
  {
     if (it->trid == trid)
     {
       redo_trn_cell=*it;
       return true;
     }
  }

  return false;

}

void llmgr_core::set_last_redo_trn_cell(transaction_id trid, trns_redo_analysis_list& redo_list,trn_cell_analysis_redo& redo_trn_cell/*in*/)
{
  trns_redo_analysis_list_reverse_iterator it;
  for (it = redo_list.rbegin(); it != redo_list.rend(); it++)
  {
     if (it->trid == trid)
     {
        *it =  redo_trn_cell;
        return;
     }
  }

  d_printf1("!!!TRID NOT FOUND\n");
  return;
}

// this function returns lsn of the first record in the bunch of chckpoint records
LONG_LSN llmgr_core::getFirstCheckpointLSN(LONG_LSN lastCheckpointLSN)
{
  LONG_LSN lsn = lastCheckpointLSN, ret_lsn;
  int state;
  char *block_ofs;
  __int64 file_size;
  size_t count;

  const char *rec;
  const char *body_beg;

  do
  {
    set_file_pointer(lsn);
    if( uGetFileSize(ll_curr_file_dsc, &file_size, __sys_call_error) == 0)
       throw SYSTEM_EXCEPTION("Can't get file size");

    if ((lsn%LOG_FILE_PORTION_SIZE) == file_size)//here we must reinit lsn
      lsn = (lsn/LOG_FILE_PORTION_SIZE + 1)*LOG_FILE_PORTION_SIZE + sizeof(logical_log_file_head);

    rec = get_record_from_disk(lsn);
    body_beg = rec + sizeof(logical_log_head);

    if (body_beg[0] != LL_CHECKPOINT)
       throw USER_EXCEPTION(SE4153);
 
    state = *((int *)(body_beg + sizeof(char)));
    block_ofs = const_cast<char *>(body_beg) + sizeof(char) + sizeof(int);

    if (state == 0)
    {
    	block_ofs += sizeof(bm_masterblock) + sizeof(LONG_LSN);
	}
  	 
    block_ofs += sizeof(int);

    count = *((size_t *)block_ofs);
    block_ofs += sizeof(size_t);

    ret_lsn = lsn;
    lsn = *((LONG_LSN *)(block_ofs + count * sizeof(WuVersionEntry)));
  } while (state != 0);

  return ret_lsn;
}

static const char *glogentrynam(int i)
{
	const char *res="UNK";

#define LOCAL_BRANCH(NAM) case NAM: res=#NAM; break;
	switch(i)
	{
		LOCAL_BRANCH(LL_INSERT_ATTR)
		LOCAL_BRANCH(LL_DELETE_ATTR)
		LOCAL_BRANCH(LL_INSERT_ELEM)
		LOCAL_BRANCH(LL_DELETE_ELEM)
		LOCAL_BRANCH(LL_INSERT_TEXT)
		LOCAL_BRANCH(LL_DELETE_TEXT)
		LOCAL_BRANCH(LL_INSERT_LEFT_TEXT)
		LOCAL_BRANCH(LL_DELETE_LEFT_TEXT)
		LOCAL_BRANCH(LL_INSERT_RIGHT_TEXT)
		LOCAL_BRANCH(LL_DELETE_RIGHT_TEXT)
		LOCAL_BRANCH(LL_INSERT_DOC)
		LOCAL_BRANCH(LL_DELETE_DOC)
		LOCAL_BRANCH(LL_INSERT_COMMENT)
		LOCAL_BRANCH(LL_DELETE_COMMENT)
		LOCAL_BRANCH(LL_INSERT_PI)
		LOCAL_BRANCH(LL_DELETE_PI)
		LOCAL_BRANCH(LL_INSERT_COLLECTION)
		LOCAL_BRANCH(LL_DELETE_COLLECTION)
		LOCAL_BRANCH(LL_INSERT_NS)
		LOCAL_BRANCH(LL_DELETE_NS)
		LOCAL_BRANCH(LL_INSERT_DOC_INDEX)
		LOCAL_BRANCH(LL_DELETE_DOC_INDEX)
		LOCAL_BRANCH(LL_INSERT_COL_INDEX)
		LOCAL_BRANCH(LL_DELETE_COL_INDEX)
		LOCAL_BRANCH(LL_COMMIT)
		LOCAL_BRANCH(LL_ROLLBACK)
		LOCAL_BRANCH(LL_CHECKPOINT)
		LOCAL_BRANCH(LL_INSERT_DOC_FTS_INDEX)
		LOCAL_BRANCH(LL_DELETE_DOC_FTS_INDEX)
		LOCAL_BRANCH(LL_INSERT_COL_FTS_INDEX)
		LOCAL_BRANCH(LL_DELETE_COL_FTS_INDEX)
		LOCAL_BRANCH(LL_FREE_BLOCKS)       
		LOCAL_BRANCH(LL_PERS_SNAPSHOT_ADD) 
		LOCAL_BRANCH(LL_DECREASE)
		LOCAL_BRANCH(LL_HBBLOCK)          
		LOCAL_BRANCH(LL_INSERT_DOC_TRG)
		LOCAL_BRANCH(LL_DELETE_DOC_TRG)
		LOCAL_BRANCH(LL_INSERT_COL_TRG)
		LOCAL_BRANCH(LL_DELETE_COL_TRG)
	}
#undef LOCAL_BRANCH

	return res;
}

void llmgr_core::print_llog()
{
#define XPTR_FMT(P) (P).layer, (int)(P).addr

	logical_log_sh_mem_head* mem_head = (logical_log_sh_mem_head*)shared_mem;

	__int64 file_size;
	
	const char *rec, *body_beg;
	
	LONG_LSN lsn = mem_head->base_addr + sizeof(logical_log_file_head), end_lsn = mem_head->last_lsn;

	printf("Start of logical log...\n");
    printf("Header info: last_lsn = %016llx, nextlsn = %016llx, last_phys_lsn = %016llx, last_chkp_lsn = %016llx, timestamp = %016llx\n",
             mem_head->last_lsn,
             mem_head->next_lsn,
             mem_head->last_chain_lsn,
             mem_head->last_checkpoint_lsn,
             mem_head->ts);

	while (lsn <= end_lsn)
	{
		set_file_pointer(lsn);
		
		if (uGetFileSize(ll_curr_file_dsc, &file_size, __sys_call_error) == 0)
			throw SYSTEM_EXCEPTION("Cannot get file size!");

		int rmndr = lsn % LOG_FILE_PORTION_SIZE;

		if (rmndr == file_size)
			lsn = (lsn / LOG_FILE_PORTION_SIZE + 1) * LOG_FILE_PORTION_SIZE + sizeof(logical_log_file_head);
		else if (rmndr == 0)
			lsn += sizeof(logical_log_file_head);

		rec = get_record_from_disk(lsn);
		body_beg = rec + sizeof(logical_log_head);

		char type = body_beg[0];
		int offs = sizeof(char);
		int trid;

		if (type == LL_INSERT_ATTR || type == LL_DELETE_ATTR)
		{

			memcpy(&trid, body_beg + offs, sizeof(int));
			offs += sizeof(int);

			printf("Transaction id = %d\n", trid);

		    const char *name, *uri, *prefix, *value;
		    int value_size;
		    xmlscm_type xtype;
		    xptr self, left, right, parent;
			
			name = body_beg + offs;
			offs += strlen(name) + 1;
			uri = body_beg + offs;
			offs += strlen(uri) + 1;
			prefix = body_beg + offs;
			offs += strlen(prefix) + 1;
			memcpy(&value_size, body_beg + offs, sizeof(int));
			offs += sizeof(int);
			value = body_beg + offs;
			offs += value_size;
		    memcpy(&xtype, body_beg + offs, sizeof(xmlscm_type));
		    offs += sizeof(xmlscm_type);
		    memcpy(&self, body_beg + offs, sizeof(xptr));
		    offs += sizeof(xptr);
		    memcpy(&left, body_beg + offs, sizeof(xptr));
		    offs += sizeof(xptr);
		    memcpy(&right, body_beg + offs, sizeof(xptr));
		    offs += sizeof(xptr);
		    memcpy(&parent, body_beg + offs, sizeof(xptr));

		    printf("%016llx %s self=%08x%08x left=%08x%08x right=%08x%08x parent=%08x%08x\n", 
		    	lsn,
		    	glogentrynam(type),
		    	XPTR_FMT(self),
		   		XPTR_FMT(left),
		    	XPTR_FMT(right),
		    	XPTR_FMT(parent)
		    );
		}
		else if (type == LL_INSERT_ELEM || type == LL_DELETE_ELEM)
		{

			memcpy(&trid, body_beg + offs, sizeof(int));
			offs += sizeof(int);

			printf("Transaction id = %d\n", trid);

		    const char *name, *uri, *prefix;
		    xmlscm_type xtype;
		    xptr self, left, right, parent;
			
			name = body_beg + offs;
			offs += strlen(name) + 1;
			uri = body_beg + offs;
			offs += strlen(uri) + 1;
			prefix = body_beg + offs;
			offs += strlen(prefix) + 1;
		    memcpy(&xtype, body_beg + offs, sizeof(xmlscm_type));
		    offs += sizeof(xmlscm_type);
		    memcpy(&self, body_beg + offs, sizeof(xptr));
		    offs += sizeof(xptr);
		    memcpy(&left, body_beg + offs, sizeof(xptr));
		    offs += sizeof(xptr);
		    memcpy(&right, body_beg + offs, sizeof(xptr));
		    offs += sizeof(xptr);
		    memcpy(&parent, body_beg + offs, sizeof(xptr));

		    printf("%016llx %s self=%08x%08x left=%08x%08x right=%08x%08x parent=%08x%08x\n", 
		    	lsn,
		    	glogentrynam(type),
		    	XPTR_FMT(self),
		   		XPTR_FMT(left),
		    	XPTR_FMT(right),
		    	XPTR_FMT(parent)
		    );
		}
		else if (type == LL_INSERT_TEXT || type == LL_DELETE_TEXT)
		{

			memcpy(&trid, body_beg + offs, sizeof(int));
			offs += sizeof(int);

			printf("Transaction id = %d\n", trid);

		    const char *value;
		    xptr self, left, right, parent;
			int value_size;

		    memcpy(&value_size, body_beg + offs, sizeof(int));
		    offs += sizeof(int);
		    value = body_beg + offs;
		    offs += value_size;
		    memcpy(&self, body_beg + offs, sizeof(xptr));
		    offs += sizeof(xptr);
		    memcpy(&left, body_beg + offs, sizeof(xptr));
		    offs += sizeof(xptr);
		    memcpy(&right, body_beg + offs, sizeof(xptr));
		    offs += sizeof(xptr);
		    memcpy(&parent, body_beg + offs, sizeof(xptr));

		    printf("%016llx %s self=%08x%08x left=%08x%08x right=%08x%08x parent=%08x%08x\n", 
		    	lsn,
		    	glogentrynam(type),
		    	XPTR_FMT(self),
		   		XPTR_FMT(left),
		    	XPTR_FMT(right),
		    	XPTR_FMT(parent)
		    );
		}
		else if (type == LL_INSERT_LEFT_TEXT || type == LL_DELETE_LEFT_TEXT || type == LL_INSERT_RIGHT_TEXT || type == LL_DELETE_RIGHT_TEXT)
		{

				memcpy(&trid, body_beg + offs, sizeof(int));
				offs += sizeof(int);

				printf("Transaction id = %d\n", trid);

			    const char *value;
			    xptr self;
				int value_size;

		 	    memcpy(&value_size, body_beg + offs, sizeof(int));
			    offs += sizeof(int);
			    value = body_beg + offs;
			    offs += value_size;
			    memcpy(&self, body_beg + offs, sizeof(xptr));
		
			    printf("%016llx %s self=%08x%08x\n", 
			    	lsn,
			    	glogentrynam(type),
		    		XPTR_FMT(self)
			    );
		}
		else if (type == LL_COMMIT || type == LL_ROLLBACK)
		{

			memcpy(&trid, body_beg + offs, sizeof(int));
			offs += sizeof(int);

			printf("Transaction id = %d\n", trid);

		    printf("%016llx %s\n", 
		    	lsn,
		    	glogentrynam(type)
		    );
		}
		else if (type == LL_INSERT_DOC || type == LL_DELETE_DOC)
		{

			memcpy(&trid, body_beg + offs, sizeof(int));
			offs += sizeof(int);

			printf("Transaction id = %d\n", trid);

		    const char *name, *coll;
		    xptr self;

		    name = body_beg + offs;
		    offs += strlen(name) + 1;
		    coll = body_beg + offs;
		    offs += strlen(coll) + 1;
		    memcpy(&self, body_beg + offs, sizeof(xptr));

		    printf("%016llx %s name=%s coll=%s self=%08x%08x\n", 
		    	lsn,
		    	glogentrynam(type),
		    	name,
		    	coll,
		    	XPTR_FMT(self)
		    );
		}
		else if (type == LL_INSERT_COMMENT || type == LL_DELETE_COMMENT)
		{

			memcpy(&trid, body_beg + offs, sizeof(int));
			offs += sizeof(int);

			printf("Transaction id = %d\n", trid);

		    xptr self, left, right, parent;
		    const char *value;
			int value_size;
     
		    memcpy(&value_size, body_beg + offs, sizeof(int));
		    offs += sizeof(int);
		    value = body_beg + offs;
		    offs += value_size;
		    memcpy(&self, body_beg + offs, sizeof(xptr));
		    offs += sizeof(xptr);
		    memcpy(&left, body_beg + offs, sizeof(xptr));
		    offs += sizeof(xptr);
		    memcpy(&right, body_beg + offs, sizeof(xptr));
		    offs += sizeof(xptr);
		    memcpy(&parent, body_beg + offs, sizeof(xptr));

		    printf("%016llx %s self=%08x%08x left=%08x%08x right=%08x%08x parent=%08x%08x\n", 
		    	lsn,
		    	glogentrynam(type),
		    	XPTR_FMT(self),
		   		XPTR_FMT(left),
		    	XPTR_FMT(right),
		    	XPTR_FMT(parent)
		    );
		}
		else if (type == LL_INSERT_PI || type == LL_DELETE_PI)
		{

			memcpy(&trid, body_beg + offs, sizeof(int));
			offs += sizeof(int);

			printf("Transaction id = %d\n", trid);

			int total_size;
			shft target_size;
			const char *value;
			xptr self, left, right, parent;

		    memcpy(&total_size, body_beg + offs, sizeof(int));
		    offs += sizeof(int);
		    memcpy(&target_size, body_beg + offs, sizeof(shft));
	  	    offs += sizeof(shft);
		    value = body_beg + offs;
		    offs += total_size;
	     	memcpy(&self, body_beg + offs, sizeof(xptr));
		    offs += sizeof(xptr);
		    memcpy(&left, body_beg + offs, sizeof(xptr));
		    offs += sizeof(xptr);
		    memcpy(&right, body_beg + offs, sizeof(xptr));
		    offs += sizeof(xptr);
		    memcpy(&parent, body_beg + offs, sizeof(xptr));

		    printf("%016llx %s self=%08x%08x left=%08x%08x right=%08x%08x parent=%08x%08x\n", 
		    	lsn,
		    	glogentrynam(type),
		    	XPTR_FMT(self),
		   		XPTR_FMT(left),
		    	XPTR_FMT(right),
		    	XPTR_FMT(parent)
		    );
		}
		else if (type == LL_INSERT_COLLECTION || type == LL_DELETE_COLLECTION)
		{

			memcpy(&trid, body_beg + offs, sizeof(int));
			offs += sizeof(int);

			printf("Transaction id = %d\n", trid);

			const char *name;

		    name = body_beg + offs;

		    printf("%016llx %s name=%s\n", 
		    	lsn,
		    	glogentrynam(type),
		    	name
		    );
		}
		else if (type == LL_INSERT_NS || type == LL_DELETE_NS)
		{

			memcpy(&trid, body_beg + offs, sizeof(int));
			offs += sizeof(int);

			printf("Transaction id = %d\n", trid);

		    const char *uri, *prefix;
		    xptr self, left, right, parent;
		    
		    uri = body_beg + offs;
		    offs += strlen(uri) + 1;
		    prefix = body_beg + offs;
		    offs += strlen(prefix) + 1;
		    memcpy(&self, body_beg + offs, sizeof(xptr));
		    offs += sizeof(xptr);
		    memcpy(&left, body_beg + offs, sizeof(xptr));
		    offs += sizeof(xptr);
		    memcpy(&right, body_beg + offs, sizeof(xptr));
		    offs += sizeof(xptr);
		    memcpy(&parent, body_beg + offs, sizeof(xptr));

		    printf("%016llx %s self=%08x%08x left=%08x%08x right=%08x%08x parent=%08x%08x\n", 
		    	lsn,
		    	glogentrynam(type),
		    	XPTR_FMT(self),
		   		XPTR_FMT(left),
		    	XPTR_FMT(right),
		    	XPTR_FMT(parent)
		    );
		}
		else if (type == LL_INSERT_DOC_INDEX || type == LL_DELETE_DOC_INDEX || type == LL_INSERT_COL_INDEX || type == LL_DELETE_COL_INDEX)
		{

			memcpy(&trid, body_beg + offs, sizeof(int));
			offs += sizeof(int);

			printf("Transaction id = %d\n", trid);

		     const char *obj_path, *key_path, *ind_name, *doc_name;
		     xmlscm_type key_type;

		     obj_path = body_beg + offs;
		     offs += strlen(obj_path) + 1;
		     key_path = body_beg + offs;
		     offs += strlen(key_path) + 1;
		     memcpy(&key_type, body_beg + offs, sizeof(xmlscm_type));
		     offs += sizeof(xmlscm_type);
		     ind_name = body_beg + offs;
		     offs += strlen(ind_name) + 1;
		     doc_name = body_beg + offs;

			 printf("%016llx %s obj_path=%s key_path=%s ind_name=%s name=%s\n", 
		    	lsn,
		    	glogentrynam(type),
		    	obj_path,
		    	key_path,
		    	ind_name,
		    	doc_name
		     );
        }
		else if(type == LL_INSERT_DOC_FTS_INDEX || type == LL_DELETE_DOC_FTS_INDEX || type == LL_INSERT_COL_FTS_INDEX || type == LL_DELETE_COL_FTS_INDEX)
		{

			memcpy(&trid, body_beg + offs, sizeof(int));
			offs += sizeof(int);

			printf("Transaction id = %d\n", trid);

		     const char *obj_path, *ind_name, *doc_name;
		     xmlscm_type key_type;
		     int itconst;

		     obj_path = body_beg + offs;
		     offs += strlen(obj_path) + 1;
		     memcpy(&itconst, body_beg + offs, sizeof(int));
		     offs += sizeof(int);
		     ind_name = body_beg + offs;
		     offs += strlen(ind_name) + 1;
		     doc_name = body_beg + offs;

			 printf("%016llx %s obj_path=%s ind_name=%s name=%s\n", 
		    	lsn,
		    	glogentrynam(type),
		    	obj_path,
		    	ind_name,
		    	doc_name
		     );
        }
		else if (type == LL_INSERT_DOC_TRG || type == LL_DELETE_DOC_TRG || type == LL_INSERT_COL_TRG || type == LL_DELETE_COL_TRG)
		{

			memcpy(&trid, body_beg + offs, sizeof(int));
			offs += sizeof(int);

			printf("Transaction id = %d\n", trid);

			 printf("%016llx %s", 
		    	lsn,
		    	glogentrynam(type)
		     );	
		}
	    else if (type == LL_CHECKPOINT)
	    {
	    	int state = *((int *)(body_beg + sizeof(char)));
    		int lsn_offs = sizeof(char) + sizeof(int);

    		if (state == 0)
    			lsn_offs += sizeof(bm_masterblock) + sizeof(LONG_LSN);
  	 
    		// isGarbage
    		lsn_offs += sizeof(int);

	    	// count
	    	size_t count = *(size_t *)(body_beg + lsn_offs);
	    	lsn_offs += sizeof(size_t);

    		lsn_offs += count * sizeof(WuVersionEntry);

    		LONG_LSN plsn = *(LONG_LSN *)(body_beg + lsn_offs);
    		printf("%016llx %s state=%d count=%d prev_lsn=%016llx\n",
    			lsn,
    			glogentrynam(type),
    			state,
    			count,
    			plsn
    		);
		}
        else if (type == LL_FREE_BLOCKS)
        {
	    	int free_blk_info_size = *((int *)(body_beg + sizeof(char)));
	    	xptr free_blk_info_xptr = *(xptr *)(body_beg + sizeof(char) + sizeof(int));
	    	LONG_LSN plsn = *(LONG_LSN *)(body_beg + sizeof(char) + sizeof(int) + sizeof(xptr) + free_blk_info_size);

            printf("%016llx %s block_size=%d fbxptr=%08x%08x prev_lsn=%016llx\n",
            	lsn,
            	glogentrynam(type),
            	free_blk_info_size,
		    	XPTR_FMT(free_blk_info_xptr),
		    	plsn
            );
        }
        else if (type == LL_PERS_SNAPSHOT_ADD)
        {
	    	TIMESTAMP ts = *((TIMESTAMP *)(body_beg + sizeof(char) + sizeof(WuVersionEntry)));
    		WuVersionEntry *blocks_info = (WuVersionEntry *)(body_beg + sizeof(char));
        	LONG_LSN plsn = *(LONG_LSN *)(body_beg + sizeof(char) + sizeof(WuVersionEntry) + sizeof(TIMESTAMP) + sizeof(int));  

            printf("%016llx %s ts=%016llx, lxptr=%016llx, xptr=%016llx, prev_lsn=%016llx\n",
            	lsn,
            	glogentrynam(type),
            	ts,
            	blocks_info->lxptr,
            	blocks_info->xptr,
            	plsn
            );
        }
        else if (type == LL_DECREASE)
        {
        	__int64 old_size = *(__int64 *)(body_beg + sizeof(char));
        	 LONG_LSN plsn = *(LONG_LSN *)(body_beg + sizeof(char) + sizeof(__int64));  

			 printf("%016llx %s old_size=%016llx prev_lsn=%016llx\n", 
		    	lsn,
		    	glogentrynam(type),
		    	old_size,
		    	plsn
		     );	
        }       
        else if (type == LL_HBBLOCK)
        {

        }
        else
			 printf("%016llx Unknown type of record\n");

		lsn += get_record_length(rec);		
	}

#undef XPTR_FMT
}
