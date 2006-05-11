/*
 * File:  sm_plmgr.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include <iostream>

#include "sedna.h"

#include "plmgr_core.h"
#include "sm_plmgr.h"
#include "bm_rcv.h"
#include "d_printf.h"

using namespace std;
/*****************************************************************************

        !!!!  Recover Data Base State by Physical Log  !!!!

******************************************************************************/

int bl_num = 0;//for debug 

LONG_LSN sm_plmgr::recoverDataBase()
{
  int version;
  LSN _durable_lsn_ = pl_head_for_rcv.prev_lsn;//last durable lsn before checkpoint

  version = pl_head_for_rcv.version;

  //get file size
  __int64 file_size;
  int res;
  res = uGetFileSize(pl_file_handler, &file_size, __sys_call_error);

  if ( res == 0)
     throw USER_EXCEPTION2(SE4050, "phys log file");

  //set file pointer to the beginning of log records 
  res = uSetFilePointer(
                     pl_file_handler,
                     sector_size,
                     NULL,
                     U_FILE_BEGIN,
                     __sys_call_error);

  if ( res == 0)
     throw USER_EXCEPTION2(SE4046, "phys log file");

  //find the last durable log record
  int file_rmndr = file_size - sector_size;


  while( file_rmndr >0 )
  {

     readSector(read_buf, sector_size);


     if (((sector_head*)read_buf)->version == version )
     {
         _durable_lsn_ = ((sector_head*)read_buf)->durable_lsn;
         file_rmndr -= sector_size;
     }
     else
     {
        if (((sector_head*)read_buf)->version > version )
        {
           d_printf2("sector version=%d\n", ((sector_head*)read_buf)->version);
           d_printf2("file header version=%d\n", version);
           throw SYSTEM_EXCEPTION("Unpredictable state of sector header");
        }
        else
           break;
     }
  }

  //Now _durable_lsn_ is the pointer to the last good log record

  #ifndef PHYS_LOG_TEST           
  // !!! init bm recovery facilities
  bm_rcv_init();
  
  // !!! recover persistent heap;
  bm_rcv_ph(pl_head_for_rcv.ph_bu_to_ph);


  #endif

  d_printf2("recovery durable lsn=%d\n", _durable_lsn_);

  if ( _durable_lsn_ < pl_head_for_rcv.next_lsn )
  {
     #ifndef PHYS_LOG_TEST
     bm_rcv_release();
     bm_rcv_tmp_file();
     #endif

     (pl_head_for_rcv.version)++;
     //flush phys log head
     _writeFile(&pl_head_for_rcv, sizeof(file_head), 0);

//    std::cout <<"Logical log checkpoint lsn=" << pl_head_for_rcv->last_checkpoint_lsn << endl;
     return pl_head_for_rcv.last_checkpoint_lsn;
  }    //OK, database is already in consistent state

  LSN _prev_durable_lsn_ = _durable_lsn_;
  phys_log_head* log_head;
  int debug = 1;

  while ( _durable_lsn_ != NULL_LSN )
  {
     readLogRecordFromDisk(read_buf, _durable_lsn_);
     //d_printf2("rcv log record num=%d\n", debug);
     debug++;

     log_head = (phys_log_head*)read_buf;

     #ifdef PHYS_LOG_TEST
     char *tmp_buf;
     tmp_buf = new char[(log_head->len)+1];
     memcpy(tmp_buf, read_buf+sizeof(phys_log_head), log_head->len);
     tmp_buf[log_head->len]='\0';
     //d_printf2("Phys log len=%d\n", log_head->len);
     //d_printf2("Phys log body=%s\n", tmp_buf);
     //d_printf2("debug =%d\n", debug);
     delete [] tmp_buf;
     #endif

     //d_printf2("rcv log record lsn=%d\n", log_head->lsn);
     //(log_head->p).print();
     //d_printf2("size=%d\n", log_head->len);



      _durable_lsn_ = log_head->prev_lsn;

     switch (log_head->op)
     {
        case PL_CHANGE:
        {
           #ifndef PHYS_LOG_TEST           
           bm_rcv_change(log_head->p,
                         read_buf + sizeof(phys_log_head),
                         log_head->len);
           #endif
           break;
        }

        case PL_CHANGE_MASTER:
        {
           #ifndef PHYS_LOG_TEST  
           d_printf1("recovery master block\n");         
           bm_rcv_master_block(read_buf + sizeof(phys_log_head));
           #endif
           break;
        }

        case PL_DECREASE:
        {
           #ifndef PHYS_LOG_TEST           
           bm_rcv_decrease(*((__int64*)(read_buf + sizeof(phys_log_head))));
           #endif
           break;
        }
/*
        case PL_CREATE_NODE_BLK:
        {
           #ifndef PHYS_LOG_TEST           
           bm_rcv_create_node_blk(log_head->p);
           #endif
           break;
        } 
*/
        default:
        {
            throw SYSTEM_EXCEPTION("Unknown operation in phys log record");
        }
     }
     
  }

end:

  (pl_head_for_rcv.version)++;

 // flush phys log head
  _writeFile(&pl_head_for_rcv, sizeof(file_head), 0);

  #ifndef PHYS_LOG_TEST           
   //release bm
  bm_rcv_release();
  #endif

  bm_rcv_tmp_file();

//  std::cout <<"Logical log checkpoint lsn=" << pl_head_for_rcv->last_checkpoint_lsn << endl;
  return pl_head_for_rcv.last_checkpoint_lsn;
}
