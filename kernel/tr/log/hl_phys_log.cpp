/*
 * File:  hl_phys_log.cpp
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#include "common/sedna.h"
#include <string>
#include <iostream>
#include "tr/log/log.h"
#include "common/errdbg/d_printf.h"
#include "sm/plmgr/tr_plmgr.h"
#include "common/base.h"
#include "common/XptrHash.h"
#include "tr/tr_globals.h"
#include "common/tr_debug.h"
#include "common/sm_vmm_data.h"
#include "sm/trmgr.h"

using namespace std;

tr_plmgr* phys_log_mgr;
bool is_initialized_on_session = false;
bool is_initialized_on_transaction = false;
XptrHash<char, 16, 16> pl_hm;

//variables for debug
int log_change_times =0;
int log_change_size =0;

int log_change_blk_times =0;


int log_create_node_blk_times = 0;

time_t t_1, t_2;
double total_change_time = 0;
double total_change_blk_time = 0;
double total_create_node_blk_time = 0;


void hl_phys_log_on_session_begin(std::string phys_log_path)
{
#ifdef PHYS_LOG
     phys_log_mgr = se_new tr_plmgr();
     phys_log_mgr->open_phys_log(phys_log_path);
     phys_log_mgr->open_shared_mem();
     is_initialized_on_session = true;
#endif
}

void hl_phys_log_on_transaction_begin()
{
#ifdef PHYS_LOG
     is_initialized_on_transaction = true;
#endif
}

void hl_phys_log_on_session_end()
{
#ifdef PHYS_LOG
  if (is_initialized_on_session)
  {
//     d_printf1("\nPhys log Statistics:\n");
//     d_printf2("log_change_times=%d\n",log_change_times);
//     d_printf2("log_change_size=%d\n",log_change_size);
     //d_printf2("log_change_funcall_total_time=%f\n", total_change_time);
//     d_printf2("log_change_blk_times=%d\n",log_change_blk_times);
     //d_printf2("log_change_blk_funcall_total_time=%f\n", total_change_blk_time);
//     d_printf2("log_create_node_blk_times=%d\n",log_create_node_blk_times);
     //d_printf2("log_create_node_blk_funcall_total_time=%f\n",total_create_node_blk_time);
     //d_printf2("hash_map size=%d\n", pl_hm.size());
    
     phys_log_mgr->close_phys_log();
     phys_log_mgr->close_shared_mem();
  }

  is_initialized_on_session = false;

#endif
}

void hl_phys_log_on_transaction_end()
{
#ifdef PHYS_LOG
  is_initialized_on_transaction = false;
#endif
}

int bl_num = 0;

void hl_phys_log_change(const /*xptr &*/void *p, shft size)
{
#ifdef PHYS_LOG
     
   //  time(&t_1);

     //cout << "change xptr=";
     //GETBLOCKHDR_ADDR(p)->p.print();
     //cout << endl;
     //d_printf1("hl_phys_log_change begin\n");

     if (phys_log_mgr->get_bulk_load_blk(trid) == '1')
     {
        pl_hm.clear();
        //d_printf1("clear Xptr Hash\n");
        phys_log_mgr->set_empty_bulk_load_blk(trid, '0');
     }

     //d_printf1("after Xptr Hash blk\n");

     char data = '\0';
     vmm_sm_blk_hdr *hdr = (vmm_sm_blk_hdr*)(ALIGN_ADDR(p));
     shft offs = (shft)((int)p- (int)p & PAGE_BIT_MASK);

//d_printf2("before res offs=%d\n", ((int)p- ((int)p & PAGE_BIT_MASK)));

     int res = pl_hm.find(hdr->p, data);
     if (res != 0)
     {
        log_change_times++;
        //d_printf2("log_change_times=%d\n", log_change_times);
        log_change_size+=size;
        //d_printf1("before logInsert\n");
        int n = get_phys_record_block_parts(p, size);
        int first_part_num = n % BLOCK_PARTS;
        int parts_num = 1 + n / BLOCK_PARTS;
//        d_printf2("first part=%d\n", first_part_num);
//        d_printf2("parts num=%d\n", parts_num);
//        d_printf2("offs=%d\n", ((int)p- ((int)p & PAGE_BIT_MASK)));
//        d_printf2("size=%d\n", size);
//        (hdr->p).print();


        int i;
        char* addr = (char*)ALIGN_ADDR(p);
        if ((first_part_num + parts_num) > BLOCK_PARTS)
            throw SYSTEM_EXCEPTION("Incorrect determining of parts to write in phys log");

        for (i = first_part_num; i< (first_part_num + parts_num); i++)
        {
           if (hdr->cntrs[i] <= phys_log_mgr->get_cp_num())
           {
              phys_log_mgr->logInsert(addr+i*BLOCK_PART_SIZE, BLOCK_PART_SIZE, PL_CHANGE);
              hdr->cntrs[i] = phys_log_mgr->get_cp_num() + 1;
           }
        }
     }

//     d_printf2("log_change_times=%d\n",log_change_times);

     //d_printf2("hl_phys_log_change size=%d\n", size);

#endif
}

void hl_phys_log_change_blk(const /*xptr &*/void *p)
{
#ifdef PHYS_LOG

//     d_printf1("hl_phys_log_change_blk begin\n");

     if (phys_log_mgr->get_bulk_load_blk(trid) == '1')
     {
        pl_hm.clear();
        //d_printf1("clear Xptr Hash\n");
        phys_log_mgr->set_empty_bulk_load_blk(trid, '0');
     }

     char data ='\0';
     int res = pl_hm.find(((vmm_sm_blk_hdr*)(ALIGN_ADDR(p)))->p, data);
     if (res != 0)
     {
         log_change_blk_times++;

         int i;
         char* addr = (char*)ALIGN_ADDR(p);
         vmm_sm_blk_hdr *hdr = (vmm_sm_blk_hdr*)(ALIGN_ADDR(p));

         for (i = 0; i < BLOCK_PARTS; i++)
         {
           if (hdr->cntrs[i] <= phys_log_mgr->get_cp_num())
           {
//              d_printf1("Record inserted\n");
              phys_log_mgr->logInsert(addr + i*BLOCK_PART_SIZE, BLOCK_PART_SIZE, PL_CHANGE);
              hdr->cntrs[i] = phys_log_mgr->get_cp_num() + 1;
           }
         }
     }

//     d_printf1("hl_phys_log_change_blk end\n");
#endif
}

void hl_phys_log_create_node_blk(const void* p)
{
#ifdef PHYS_LOG
     //d_printf1("hl_phys_log_create_node_blk begin\n");

     if (phys_log_mgr->get_bulk_load_blk(trid) == '1')
     {
        pl_hm.clear();
        //d_printf1("clear Xptr Hash\n");
        phys_log_mgr->set_empty_bulk_load_blk(trid, '0');
     }

    log_create_node_blk_times++;

   // time(&t_1);
    pl_hm.insert(((vmm_sm_blk_hdr*)(ALIGN_ADDR(p)))->p, '\0');

    //d_printf1("hl_phys_log_create_node_blk end\n");
    //cout << "create block xptr=";
    //GETBLOCKHDR_ADDR(p)->p.print();
    //cout << endl; 
   // time(&t_2);
   // total_create_node_blk_time += difftime(t_2, t_1);
   //string str = string("hl_phys_log_create_node_blk finished\n");
   // WRITE_DEBUG_LOG(str.c_str());

#endif
}

void activate_and_wait_for_end_checkpoint()
{
#ifdef CHECKPOINT_ON
     phys_log_mgr->activate_checkpoint(true);
     uSleep(1, __sys_call_error);
     wait_for_checkpoint_finished();
#endif
}

int get_phys_record_block_parts(const void * p, int size)
{
	int i=(shft)((int)p- ((int)p & PAGE_BIT_MASK));
    int part= i / (PAGE_SIZE/BLOCK_PARTS);
//    d_printf2("XXX part=%d\n", part);
//	d_printf2("XXX i=%d\n", i);
//	d_printf2("XXX div=%d\n", PAGE_SIZE/BLOCK_PARTS);
//	d_printf2("XXX p=%d\n", p);
//	d_printf2("XXX p offs=%d\n", (int)p & PAGE_BIT_MASK);
    i+=(size-1);
    i= i / (PAGE_SIZE/BLOCK_PARTS);
//    d_printf2("ret value=%d\n", BLOCK_PARTS*(i-part)+part);
    return  BLOCK_PARTS*(i-part)+part;
}