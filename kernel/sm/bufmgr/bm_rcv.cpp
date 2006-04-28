/*
 * File:  bm_rcv.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "sedna.h"
#include <string>
#include <iostream>
#include "bm_rcv.h"
#include "bm_core.h"
#include "sm_globals.h"
#include "bm_functions.h"
#include "blk_mngmt.h"
#include "d_printf.h"

using namespace std;

void bm_rcv_init()
{
    // open data file
    string data_file_name = string(db_files_path) + string(db_name) + ".data";
    data_file_handler = uOpenFile(data_file_name.c_str(), 0, U_WRITE, U_WRITE_THROUGH);
    if (data_file_handler == U_INVALID_FD)
        throw USER_EXCEPTION2(SE4042, data_file_name.c_str());
}

void bm_rcv_release()
{
    // close data file
    if (uCloseFile(data_file_handler) == 0)
        throw USER_EXCEPTION2(SE4043, ".data file");
}

void bm_rcv_change(const xptr& xaddr, const void *p, shft size, __int64 file_size)
{
    __int64 _dsk_offs;

    if (IS_DATA_BLOCK(xaddr)) 
    {
        _dsk_offs = ABS_DATA_OFFSET(xaddr) + (__int64)PAGE_SIZE;
        
        if (file_size != 0)
            if (!((__int64)PAGE_SIZE <= _dsk_offs && _dsk_offs <= file_size - (__int64)PAGE_SIZE))
                throw SYSTEM_EXCEPTION("Offset is out of range");
    }
    else throw SYSTEM_EXCEPTION("Wrong physical log record (for tmp file)");

    // restore backup
    if (uSetFilePointer(data_file_handler, _dsk_offs, NULL, U_FILE_BEGIN) == 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot set file pointer");

    int number_of_bytes_written = 0;
    int res = uWriteFile(data_file_handler, p, size, &number_of_bytes_written);
    if (res == 0 || number_of_bytes_written != size)
        throw SYSTEM_ENV_EXCEPTION("Cannot write to file");
}

void bm_rcv_decrease(__int64 old_size)
{
    if (uSetEndOfFile(data_file_handler, old_size, U_FILE_BEGIN) == 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot decrease data file");
}

void bm_rcv_master_block(const void* p)
{
#ifdef _WIN32
    void *n_p = VirtualAlloc(
                       NULL,
                       MASTER_BLOCK_SIZE,
                       MEM_COMMIT,
                       PAGE_READWRITE
                );
#else
    void *n_p = new char[MASTER_BLOCK_SIZE];
#endif

    if (n_p == NULL) throw USER_ENV_EXCEPTION("Cannot allocate enough memory", false);

    //__int64 tmp_file_cur_size = ((bm_masterblock*)p)->tmp_file_cur_size;
    //((bm_masterblock*)p)->tmp_file_cur_size = (__int64)0;

    memset(n_p, '\0', MASTER_BLOCK_SIZE);
    memcpy(n_p, p, sizeof(bm_masterblock));

    if (uSetFilePointer(data_file_handler, (__int64)0, NULL, U_FILE_BEGIN) == 0)
        throw USER_ENV_EXCEPTION("Cannot write master block", false);

    int number_of_bytes_written = 0;
    int res = uWriteFile(data_file_handler, n_p, MASTER_BLOCK_SIZE, &number_of_bytes_written);
    if (res == 0 || number_of_bytes_written != MASTER_BLOCK_SIZE)
        throw USER_ENV_EXCEPTION("Cannot write master block", false);

#ifdef _WIN32
    BOOL res_m = VirtualFree(
                        (void*)n_p,
                        0,
                        MEM_RELEASE
               );
    if (res_m == 0) throw USER_ENV_EXCEPTION("Cannot free allocated memory", false);
#else
    delete [] (char*)n_p;
#endif
}

void bm_rcv_tmp_file()
{
    // truncate tmp file up to zero size
    // open tmp file
    string tmp_file_name = string(db_files_path) + string(db_name) + ".tmp";
    tmp_file_handler = uOpenFile(tmp_file_name.c_str(), 0, U_WRITE, U_WRITE_THROUGH);
    if (tmp_file_handler == U_INVALID_FD)
        throw USER_EXCEPTION2(SE4042, tmp_file_name.c_str());

    if (uSetEndOfFile(tmp_file_handler, (__int64)0, U_FILE_BEGIN) == 0)
        throw USER_ENV_EXCEPTION("Cannot truncate tmp file", false);

    if (uCloseFile(tmp_file_handler) == 0)
        throw USER_EXCEPTION2(SE4043, tmp_file_name.c_str());

    // reform tmp file
    bm_startup();
    d_printf1("sm_startup call successful\n");

    __int64 tmp_file_cur_size = mb->tmp_file_cur_size / (__int64)PAGE_SIZE;
    mb->tmp_file_cur_size = (__int64)0;
	mb->free_tmp_blocks = XNULL;

    extend_tmp_file (tmp_file_cur_size);
    d_printf1("extend_tmp_file call successful\n");

    bm_shutdown();
    d_printf1("sm_shutdown call successful\n");
}

void bm_rcv_ph(bool ph_bu_to_ph)
{
    string ph_file_name    = string(db_files_path) + string(db_name) + ".ph";
    string ph_bu_file_name = string(db_files_path) + string(db_name) + ".ph.bu";

    if (ph_bu_to_ph)
    {
        if (uCopyFile(ph_bu_file_name.c_str(), ph_file_name.c_str(), false) == 0)
            throw USER_EXCEPTION2(SE4049, (ph_bu_file_name + " to " + ph_file_name).c_str());
    }
    else
    {
        if (uCopyFile(ph_file_name.c_str(), ph_bu_file_name.c_str(), false) == 0)
            throw USER_EXCEPTION2(SE4049, (ph_file_name + " to " + ph_bu_file_name).c_str());
    }
}


/******************************************************************************
  Persistent Heap recovery plan:


        ph_bu_to_ph = true        ph_bu_to_ph = false    ph_bu_to_ph = true
 <----------------------------->|<------------------->|<------------------->
                                |                     |
                CHECKPOINT      |               ORDINARY WORK
 ...=====|======================|=====================|====================> time

          1. call flush_buffers() 1. call backup_ph()
          2. call flush_ph()      2. set ph_bu_to_ph
          3. clear phys log and      to true
             set ph_bu_to_ph to
             false

 In case of failure bm_rcv_ph() function should be called with the ph_bu_to_ph
 parameter read from disk.

******************************************************************************/


/*
void bm_rcv_create_node_blk(const xptr& blk)
{
    //!!! NOT IMPLEMENTED
    throw 1;
}
*/

