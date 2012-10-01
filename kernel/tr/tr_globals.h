/*
 * File:  tr_globals.h 
 * Copyright (C) 2004 ISP RAS
 * The Institute for System Programming of the Russian Academy of Sciences
 */


#ifndef _TR_GLOBALS_H
#define _TR_GLOBALS_H

#include "tr/tr_base.h"
#include "tr/crmutils/serialization.h"
#include "common/sedna.h"
#include "common/base.h"
#include "common/structures/config_data.h"
#include "u/usem.h"
#include "u/u.h"

namespace tr_globals
{
    /*
     * Buffer for e_strs (used for various intermediate operations with e_strs
     * instead of allocating dynamic memory by new operator)
     */
    extern char e_string_buf[PAGE_SIZE];

    extern int run_rewriter;
    extern int show_time;
    extern int print_intermed;
    extern int debug_mode;
    extern int authentication;
    extern int authorization;
    
    extern DatabaseOptions databaseOptions; //SessionOptions are inside.
    extern SednaOptions sednaOptions;

    /* Special transactions */
    extern bool run_recovery;
    extern bool first_transaction;
    
    //received from sm
    extern transaction_id trid;
    //received from gov
    extern session_id     sid;

    extern char filename[];
    extern char password[];
    extern char login[];
    extern char output_file[];

    extern QueryType query_type;

    extern bool is_need_checkpoint_on_transaction_commit;
    extern bool is_ro_mode;       // may change during transaction execution!
    extern bool is_log_less_mode; // true, if we write only one record on every bulkload

    extern int internal_auth_switch;
    
    /*
     * Waiting semaphore for tarnsaction. In some cases transaction may be
     * asked to wait by sm after an SSMMSG call. This is the semaphore to do
     * the waiting. SM will up it when it's possible for  transaction to
     * continue.
     */
    extern USemaphore wait_sem;

    /*
     * Signals that there is timeout. Pointer to this flag is passed to the
     * pping client which counts time and sets this flag when counter greater
     * than tr_globals::query_timeout. Executor and some other tr operations
     * use this flag through CHECK_TIMER_FLAG macro to ROLLBACK current
     * transaction.
     */
     extern TLS_VAR_DECL
     volatile bool is_timer_fired;
 
     extern Serializer * serializer;
     void create_serializer(enum se_output_method method);
}

/* 
 * Check in time is out.
 * See tr_globals::is_timer_fired for the description.
 */
 #define CHECK_TIMER_FLAG         if (tr_globals::is_timer_fired) \
                                      throw USER_EXCEPTION(SE4620);


#endif /* _TR_GLOBALS_H */
