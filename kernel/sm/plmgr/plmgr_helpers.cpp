/*
 * File:  plmgr_helpers.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include "plmgr_core.h"
#include "xptr.h"
#include "base.h"
#include "exceptions.h"


void plmgr_core::writeSector(void *p, int size, int file_pos, LSN& drbl_lsn)
{
  //Note: file position is pointed to  the next durable lsn
  int sect_beg = file_pos - (file_pos % sector_size);
  sector_head* sect_head;
 
  shared_mem_head* mem_head = (shared_mem_head*)share_mem;


  if (sect_beg == file_pos)
  {

     sect_head = (sector_head*)p;
     sect_head->durable_lsn = drbl_lsn;
     sect_head->version = mem_head->pl_head.version;

     _writeFile(p, size, file_pos);

  }
  else
  {
     sect_head = new sector_head;
     sect_head->durable_lsn = drbl_lsn;
     sect_head->version = mem_head->pl_head.version;

      //write the header of sector
     _writeFile(sect_head, sizeof(sector_head), sect_beg);

     //write body of sector
     _writeFile(p, size, file_pos);

     delete sect_head;
  }
}


void plmgr_core::_writeFile(void* p, int size, int file_pos)
{
  int res;
  int nbytes_written;

  //d_printf3("Phys log file write, size=%d, file_pos=%d\n", size, file_pos);

  res = uSetFilePointer(
                    pl_file_handler,
                    (__int64)file_pos,
                    NULL,
                    U_FILE_BEGIN);

  if (res == 0)
      throw SYSTEM_EXCEPTION("Cant Set File Pointer in log file");

  res = uWriteFile(
               pl_file_handler,
               p,
               size,
               &nbytes_written
              );

  if ( res == 0 || nbytes_written != size )
      throw SYSTEM_EXCEPTION("Can't write sector head to log file");

}


//reads one sector from disk (file_pointer points to the sector begin)
void plmgr_core::readSector(void* p, int size)
{
  int res;
  int nbytes_read;

  res = uReadFile(
              pl_file_handler,
              p,
              size,
              &nbytes_read
             );

  if ( res == 0 ) // nbytes_read != size (this condition does not work on the last sector when sector is not filled completely)
     throw SYSTEM_EXCEPTION("Can't read one sector from phys log file");
}


//this function is used only during recovery
void plmgr_core::readLogRecordFromDisk(char* buf, LSN& lsn)
{
  int res;
  __int64 file_pos;
  char tmp_buf[PHYS_LOG_READ_BUF_LENGTH];

                                               //header
  file_pos = (lsn - pl_head_for_rcv->next_lsn) + sector_size;

  res = uSetFilePointer(
                    pl_file_handler,
                    file_pos,
                    NULL,
                    U_FILE_BEGIN                    
                   );

  if ( res == 0)
      throw SYSTEM_EXCEPTION("Cant Set File Pointer in log file");

  //read the header of log record
  int nbytes_read;
  int log_head_size;
  int first_portion;
  int second_portion;


  if ((sector_size - file_pos%sector_size) < sizeof(phys_log_head) )
  {//header is not contiguous
     first_portion = sector_size - file_pos%sector_size;
     second_portion = sizeof(phys_log_head) - first_portion;

     log_head_size = sizeof(phys_log_head) + sizeof(sector_head);
  }
  else
  {//header is contiguous
     first_portion = sizeof(phys_log_head);
     second_portion = 0;
	 if (file_pos%sector_size == 0)
        log_head_size = sizeof(phys_log_head) + sizeof(sector_head);
	 else
		log_head_size = sizeof(phys_log_head);
  }

  res = uReadFile(
               pl_file_handler,
               tmp_buf,
               log_head_size,
               &nbytes_read);

  if ( res == 0 || nbytes_read != log_head_size)  
     throw  SYSTEM_EXCEPTION("Can't read header of log record");

  phys_log_head log_head;
 
  if (second_portion != 0)
  {
     memcpy(&log_head, tmp_buf, first_portion);
     memcpy(((char*)&log_head)+first_portion, 
            tmp_buf + first_portion + sizeof(sector_head),
            second_portion);
  }
  else
  {
	 if (log_head_size > sizeof(phys_log_head))//the begin of sector
 		 memcpy(&log_head, tmp_buf + sizeof(sector_head), first_portion);
	 else//not the begin of sector
         memcpy(&log_head, tmp_buf, first_portion);
  }

  memcpy(buf, &log_head, sizeof(phys_log_head));

  int log_len_rmndr = log_head.drbl_len - log_head_size;
  
  // read the body of log record
  res = uReadFile(
              pl_file_handler,
              tmp_buf,
              log_len_rmndr,
              &nbytes_read
             );           

  if ( res == 0 || nbytes_read != log_len_rmndr)  
     throw SYSTEM_EXCEPTION("Can't read log record from phys log");


  //copy tmp_buf into buf excluding sector_head
  int portion = _min2(log_len_rmndr,
                    sector_size-(file_pos + log_head_size)%sector_size);
  int buf_offs = sizeof(phys_log_head);
  int tmp_buf_offs = 0;
  bool isSectorBegin = (((file_pos + log_head_size)%sector_size) !=0 ? false : true);

  while (log_len_rmndr >0 )
  {
     if (!isSectorBegin)
     {//case when is not need to exclude sector head    
        memcpy(buf+buf_offs, tmp_buf + tmp_buf_offs, portion);
        log_len_rmndr -=portion;
        buf_offs +=portion;
        tmp_buf_offs +=portion;
        portion = _min2(log_len_rmndr, sector_size);
        isSectorBegin = true;
     }
     else
     {//case when it is need to exclude sector head
        memcpy(buf+buf_offs,
               tmp_buf + tmp_buf_offs + sizeof(sector_head),
               portion - sizeof(sector_head));
        log_len_rmndr -= portion;
        buf_offs += portion - sizeof(sector_head);
        tmp_buf_offs +=portion;
        portion = _min2(log_len_rmndr, sector_size);
        isSectorBegin = true;
     }
  }

}


void plmgr_core::readPureDataFromSharedMem(int source_offs,
                                           void* dest,
                                           int source_len)
{
  // !!! Note source_offs MUST be LESS then mem_head->size
  shared_mem_head* mem_head = (shared_mem_head*)share_mem;
  char* mem_beg = (char*)share_mem;
  int rmndr_len = source_len;
  int dest_offs = 0;
  int src_offs  = source_offs;
  int portion;
  int tmp;

  if ((tmp=(source_offs - sizeof(shared_mem_head))%sector_size) == 0)
     portion = _min2(rmndr_len, sector_size - sizeof(sector_head));
  else
     portion = _min2(rmndr_len, sector_size - tmp);

  bool isBeginSector =
        (((source_offs - sizeof(shared_mem_head))%sector_size)== 0 ? true : false);


  while ( rmndr_len >0 )
  {
     if (isBeginSector)
     {
       memcpy((char*)dest + dest_offs,
              mem_beg + src_offs + sizeof(sector_head),
              portion
           );

       if ( (src_offs + sizeof(sector_head) +portion) >= mem_head->size )
          src_offs = sizeof(shared_mem_head);
       else
          src_offs += portion + sizeof(sector_head);

     }
     else
     {
       memcpy((char*)dest + dest_offs,
              mem_beg + src_offs,
              portion
          );

       if ( (src_offs + portion) >= mem_head->size )
          src_offs = sizeof(shared_mem_head);
       else
          src_offs +=portion;


      

     }

     rmndr_len -= portion;
     dest_offs += portion;
    
     portion = _min2(sector_size - sizeof(sector_head), rmndr_len);

     isBeginSector = true;
  }
   
  return;
  
}


//pred condition: shared memory contain enough free memory ( > size)
void plmgr_core::writeSharedMemory(const void* p, int size, sector_head& old_sect_h, sector_head& new_sect_h)
{
   shared_mem_head* mem_head = (shared_mem_head*)share_mem;
   char* mem_beg = (char*)share_mem;

   //write data to shared memory inserting sector_head
   int len_rmndr = size;
   int mem_offs = ((mem_head->end_offs < mem_head->size ) ? 
                            mem_head->end_offs : sizeof(shared_mem_head));
   int p_offs = 0;

   int portion = ((mem_head->sect_free_bytes >0) ?
                     _min2(mem_head->sect_free_bytes, len_rmndr) :
                     _min2(sector_size-sizeof(sector_head), len_rmndr));
   bool isBeginSector = ((mem_head->sect_free_bytes > 0) ? false : true);

   while (len_rmndr >0) 
   {

       if (!isBeginSector)
       { 
           memcpy(mem_beg + mem_offs, 
                  ((char*)p) + p_offs,
                  portion);

           len_rmndr -=portion;

           //fill the sector header
           if (len_rmndr == 0)
             memcpy(mem_beg + mem_offs - (mem_offs - sizeof(shared_mem_head))%sector_size,
                    &new_sect_h,
                    sizeof(sector_head));
           else
             memcpy(mem_beg + mem_offs - (mem_offs - sizeof(shared_mem_head))%sector_size,
                    &old_sect_h,
                    sizeof(sector_head));
             
           //reinit cycle variables
           if((mem_offs+portion) < mem_head->size)
              mem_offs += portion;
           else
              mem_offs = sizeof(shared_mem_head);

           p_offs +=portion;
           portion = _min2 (len_rmndr, sector_size-sizeof(sector_head));
           isBeginSector = true;
       }
       else
       {  
           memcpy(mem_beg + mem_offs + sizeof(sector_head),
                  ((char*)p) + p_offs,
                  portion);

           len_rmndr -=portion;

           // fill the sector head
           if ( len_rmndr == 0 )
              memcpy(mem_beg + mem_offs,
                     &new_sect_h,
                     sizeof(sector_head));
           else
              memcpy(mem_beg + mem_offs,
                     &old_sect_h,
                     sizeof(sector_head));

           //reint variables of cycle
           if ((mem_offs + portion + sizeof(sector_head)) < mem_head->size )
               mem_offs += portion + sizeof(sector_head);
           else
               mem_offs = sizeof(shared_mem_head);
           p_offs +=portion;
           portion = _min2( len_rmndr, sector_size-sizeof(sector_head));
           isBeginSector = true;
       }
   }
   

   //reinit shared memory header
   mem_head->end_offs = mem_offs;
   
   if (mem_head->begin_not_drbl_offs <= mem_head->end_offs)
   {
      mem_head->free_bytes =  mem_head->size - 
                              sizeof(shared_mem_head) -
                              (mem_head->end_offs - mem_head->begin_not_drbl_offs);
             
   }
   else
   {
      mem_head->free_bytes = mem_head->begin_not_drbl_offs - mem_head->end_offs;
   }     
   
   mem_head->keep_bytes = mem_head->size - mem_head->free_bytes - sizeof(shared_mem_head);

   mem_head->sect_free_bytes = 
            (((mem_head->end_offs - sizeof(shared_mem_head))%sector_size == 0) ?
            0 :
            sector_size - (mem_head->end_offs - sizeof(shared_mem_head))%sector_size);
 
  
}




int plmgr_core::getLogFileSize(bool sync)
{

  //get semaphore
  DownSemaphore(sync);

  int res;
  __int64 file_size;

  res = uGetFileSize(pl_file_handler, &file_size);  

  if ( res == 0)
     throw SYSTEM_EXCEPTION("Can't get the phys log size");

  //give semaphore
  UpSemaphore(sync);

  return (int)file_size;

}

//return the size of phys log (excluding log records which are not flushed)
int plmgr_core::getPhysLogSize(bool sync)
{

  DownSemaphore(sync);

  shared_mem_head* mem_head = (shared_mem_head*)share_mem;

  int phys_log_size = mem_head->next_durable_lsn - mem_head->pl_head.next_lsn + sector_size;

  UpSemaphore(sync);

  return phys_log_size;
}


void plmgr_core::DownSemaphore(bool sync)
{
  int r;

  if (sync)
  {
     r = USemaphoreDown(sem);

     if (r != 0)
        throw SYSTEM_EXCEPTION("Can't down semaphore protected shared memory");
  }
}

void plmgr_core::UpSemaphore(bool sync)
{
  int r;

  if ( sync )
  {
     r = USemaphoreUp(sem);

     if(r != 0)
       throw SYSTEM_EXCEPTION("Can't up semaphore protected shared memory");
  }
}

void plmgr_core::set_ph_bu_to_ph(bool ph_bu_to_ph, bool sync)
{
  DownSemaphore(sync);
  shared_mem_head* mem_head = (shared_mem_head*)share_mem;

  mem_head->pl_head.ph_bu_to_ph = ph_bu_to_ph;

  _writeFile(&(mem_head->pl_head), sizeof(file_head), 0);
  
  UpSemaphore(sync);
}

file_head plmgr_core::get_file_head(bool sync)
{
  
  DownSemaphore(sync);
  file_head head;
  shared_mem_head* mem_head = (shared_mem_head*)share_mem;
  head = mem_head->pl_head;
  UpSemaphore(sync);
  return head;
}
