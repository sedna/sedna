/*
 * File:  bm_rcv.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <string>
#include <iostream>
#include "sm/bufmgr/bm_rcv.h"
#include "sm/bufmgr/bm_core.h"
#include "sm/bufmgr/bm_functions.h"
#include "sm/bufmgr/blk_mngmt.h"
#include "sm/sm_globals.h"
#include "common/errdbg/d_printf.h"

using namespace std;

void bm_rcv_init()
{
    // open data file
    string data_file_name = string(sm_globals::db_files_path) + string(sm_globals::db_name) + ".sedata";
    data_file_handler = uOpenFile(data_file_name.c_str(), U_SHARE_READ, U_WRITE, U_WRITE_THROUGH, __sys_call_error);
    if (data_file_handler == U_INVALID_FD)
        throw USER_EXCEPTION2(SE4042, data_file_name.c_str());
}

void bm_rcv_release()
{
    // close data file
    if (uCloseFile(data_file_handler, __sys_call_error) == 0)
        throw USER_EXCEPTION2(SE4043, ".sedata file");
}

void bm_rcv_change(const xptr& xaddr, const void *p, unsigned int size)
{
    uint64_t _dsk_offs;

    if (IS_DATA_BLOCK(xaddr))
    {
        _dsk_offs = ABS_DATA_OFFSET(xaddr) + (int64_t)PAGE_SIZE;
    }
    else throw SYSTEM_EXCEPTION("Wrong physical log record (for tmp file)");

    // restore backup
    if (uSetFilePointer(data_file_handler, _dsk_offs, NULL, U_FILE_BEGIN, __sys_call_error) == 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot set file pointer");

    unsigned int number_of_bytes_written = 0;
    int res = uWriteFile(data_file_handler, p, size, &number_of_bytes_written, __sys_call_error);
	if (res == 0 || number_of_bytes_written != size)
        throw SYSTEM_ENV_EXCEPTION("Cannot write to file");
}

void bm_rcv_read_block(const xptr &p, void *buf)
{
    uint64_t _dsk_offs;

    _dsk_offs = ABS_DATA_OFFSET(p) + (int64_t)PAGE_SIZE;

    // read block
    if (uSetFilePointer(data_file_handler, _dsk_offs, NULL, U_FILE_BEGIN, __sys_call_error) == 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot set file pointer");

    unsigned int number_of_bytes_read = 0;
    int res = uReadFile(data_file_handler, buf, PAGE_SIZE, &number_of_bytes_read, __sys_call_error);
    if (res == 0 || number_of_bytes_read != PAGE_SIZE)
        throw SYSTEM_ENV_EXCEPTION("Cannot read block");
}

void bm_rcv_decrease(int64_t old_size)
{
    if (uSetEndOfFile(data_file_handler, old_size, U_FILE_BEGIN, __sys_call_error) == 0)
        throw SYSTEM_ENV_EXCEPTION("Cannot decrease data file");
}

void bm_rcv_master_block(const void* p)
{
    memset(mb, '\0', MASTER_BLOCK_SIZE);
    memcpy(mb, p, sizeof(bm_masterblock));

    if (uSetFilePointer(data_file_handler, (int64_t)0, NULL, U_FILE_BEGIN, __sys_call_error) == 0)
        throw USER_ENV_EXCEPTION("Cannot write master block", false);

    unsigned int number_of_bytes_written = 0;
    int res = uWriteFile(data_file_handler, mb, MASTER_BLOCK_SIZE, &number_of_bytes_written, __sys_call_error);
    if (res == 0 || number_of_bytes_written != MASTER_BLOCK_SIZE)
        throw USER_ENV_EXCEPTION("Cannot write master block", false);
}


void bm_rcv_tmp_file()
{
    // truncate tmp file up to zero size
    if (uSetEndOfFile(tmp_file_handler, (uint64_t)0, U_FILE_BEGIN, __sys_call_error) == 0)
        throw USER_ENV_EXCEPTION("Cannot truncate tmp file", false);

    mb->tmp_file_cur_size = (uint64_t)0;
        mb->free_tmp_blocks = XNULL;

    extend_tmp_file((int)MBS2PAGES(sm_globals::tmp_file_initial_size));
    d_printf1("extend_tmp_file call successful\n");
}


/*
void bm_rcv_create_node_blk(const xptr& blk)
{
    //!!! NOT IMPLEMENTED
    throw 1;
}
*/

