/*
 * File:  lm_base.h
 * Copyright (C) 2004 The Institute for System Programming of the Russian Academy of Sciences (ISP RAS)
 */

#ifndef _LM_BASE_H
#define _LM_BASE_H

#include <string>
#include "common/sedna.h"
#include "common/base.h"

enum resource_kind {LM_DOCUMENT, LM_COLLECTION, LM_INDEX, LM_TRIGGER, LM_DATABASE};

enum lock_mode
{
    NULL_LOCK,
    lm_s,		// Shared
    lm_x,		// eXclusive
    lm_is,      //intention shared
    lm_ix,      //intention exclusive
    lm_six      //shared and intention exclusive
};

//enum lock_reply {LOCK_GRANTED, LOCK_WAIT, LOCK_ERROR}; 

enum lock_reply {LOCK_OK, LOCK_TIMEOUT, LOCK_DEADLOCK, LOCK_NOT_LOCKED };

enum lock_status {LOCK_GRANTED, LOCK_CONVERTING, LOCK_WAITING, LOCK_DENIED, NONE_STATUS};

enum lock_class {LOCK_INSTANT, LOCK_SHORT, LOCK_MEDIUM, LOCK_LONG, LOCK_VERY_LONG};

class resource_id
{
private:
   std::string res_name;
   resource_kind kind; //document or collection
public:
   friend void print_resource_id(std::string);
   resource_id(std::string r_n, resource_kind r_k): res_name(r_n), kind(r_k) {};
   resource_id(const resource_id& r_id): res_name(r_id.res_name), kind(r_id.kind) {};
   resource_id(){};
   std::string get_res_name() { return res_name;};
   resource_kind get_resource_kind() {return kind;};
   std::string get_str_res_id() 
   {
     std::string hash_r_id;

     switch (kind)
     {
        case LM_DOCUMENT: hash_r_id = std::string("_doc_") + res_name;
                          break;
      
        case LM_COLLECTION: hash_r_id = std::string("_col_") + res_name;
                            break;

        case LM_INDEX: hash_r_id = std::string("_ind_") + res_name;
                       break;

        case LM_TRIGGER: hash_r_id = std::string("_trg_") + res_name;
                       break;

        case LM_DATABASE: hash_r_id = std::string("_dtb_") + res_name;
                          break;
   
        default: throw USER_EXCEPTION(SE4700);
     }
    
     return hash_r_id;

   };
};



enum lm_commands {LM_LOCK, LM_RELEASE};

#define MAX_SEM_NAME_LENGTH 100

#endif
